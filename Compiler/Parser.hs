{-# LANGUAGE FlexibleContexts #-}
module Compiler.Parser (Compiler.Parser.parse) where

import Compiler.Token
import Compiler.Ast.Ast hiding (declaration)

import Data.Either
import Data.Maybe
import Data.List.NonEmpty hiding (head, last)

import Control.Monad (liftM)
import Prelude hiding (EQ, GT, LT, exp)
import Text.Parsec hiding(satisfy, string)
import Control.Applicative ((<$>), (<*), (<*>))

import Control.Lens

import Compiler.Position

-- Parses 
parse :: [Token] -> Either ParseError Ast
parse tokens = fmap (\p -> Ast p) prog
    where
        prog = Text.Parsec.parse (Compiler.Parser.program <* eof) "program" tokens
        eof = satisfy' p <?> "end of program text"
        p :: Token -> Maybe Bool
        p (TEof) = Just True
        p _ = Nothing

satisfy :: (Stream [Token] m Token) => (Token -> Bool) -> ParsecT [Token] u m Token
satisfy f = tokenPrim show nextPos tokeq
    where
      tokeq :: Token -> Maybe Token
      tokeq t = if f t then Just t else Nothing

satisfy' :: (Stream [Token] m Token) => (Token -> Maybe a) -> ParsecT [Token] u m a
satisfy' = tokenPrim show nextPos

nextPos :: SourcePos -> Token -> [Token] -> SourcePos
nextPos pos _ (TEof : _) = pos
nextPos pos _ (t : _) = setSourceColumn (setSourceLine pos $ Compiler.Token.lineNumber t) (Compiler.Token.column t)
nextPos pos _ []      = pos

tok :: (Stream [Token] m Token) => TokenType -> ParsecT [Token] u m Token
tok t = satisfy p <?> show t
    where
        p :: Token -> Bool
        p (TEof) = False
        p (Token t' _ _ _ _ _) = t' == t

tok' :: (Stream [Token] m Token) => TokenType -> ParsecT [Token] u m ()
tok' p = tok p >> return ()

parens :: Monad m => ParsecT [Token] u m a -> ParsecT [Token] u m a
parens = between (tok TLeftParen) (tok TRightParen)

brackets :: Monad m => ParsecT [Token] u m a -> ParsecT [Token] u m a
brackets = between (tok TLeftBracket) (tok TRightBracket)

braces :: Monad m => ParsecT [Token] u m a -> ParsecT [Token] u m a
braces = between (tok TLeftBrace) (tok TRightBrace)

list :: Monad m => ParsecT [Token] u m a -> ParsecT [Token] u m [a]
list parser = rec <|> (return [])
    where
        rec = do
                first <- parser
                rest <- list parser
                return (first : rest)

list' :: Monad m => ParsecT [Token] u m a -> ParsecT [Token] u m (NonEmpty a)
list' parser = do
                 first <- parser
                 rest <- list parser
                 return (first :| rest)

listWith :: Monad m => ParsecT [Token] u m a -> ParsecT [Token] u m b -> ParsecT [Token] u m [b]
listWith separator parser = rec <|> (return [])
    where
        rec = do
                first <- parser
                rest <- (separator >> (listWith separator parser)) <|> (return [])
                return (first : rest)

listWith' :: Monad m => ParsecT [Token] u m a -> ParsecT [Token] u m b -> ParsecT [Token] u m (NonEmpty b)
listWith' separator parser = do
                               first <- parser
                               rest <- (separator >> (listWith separator parser)) <|> (return [])
                               return (first :| rest)

identifier :: Monad m => ParsecT [Token] u m String
identifier = satisfy' p <?> "identifier"
    where
        p :: Token -> Maybe String
        p t@(Token TIdentifier _ _ v _ _) = v
        p _ = Nothing

identifierWithTok :: Monad m => ParsecT [Token] u m (String, Token)
identifierWithTok = satisfy' p <?> "identifier"
    where
        p :: Token -> Maybe (String, Token)
        p t@(Token TIdentifier _ _ (Just v) _ _) = Just (v, t)
        p _ = Nothing

functionCall :: Monad m => ParsecT [Token] u m FunctionCall
functionCall = do
                 (id, ti) <- identifierWithTok
                 args <- parens expressionList
                 return $ FunctionCall id args Nothing $ tokenLocation ti

basicType :: Monad m => ParsecT [Token] u m BasicType
basicType = satisfy' p <?> "basic type"
    where
        p :: Token -> Maybe BasicType
        p t@(Token TFloat _ _ _ _ _) = Just $ Float $ tokenLocation t
        p t@(Token TInt _ _ _ _ _) = Just $ Int $ tokenLocation t
        p t@(Token TBool _ _ _ _ _) = Just $ Bool $ tokenLocation t
        p _ = Nothing

returnType :: Monad m => ParsecT [Token] u m ReturnType
returnType = satisfy' p <?> "return type"
    where
        p :: Token -> Maybe ReturnType
        p t@(Token TFloat _ _ _ _ _) = Just $ BasicType (Float $ tokenLocation t) $ tokenLocation t
        p t@(Token TInt _ _ _ _ _) = Just $ BasicType (Int $ tokenLocation t) $ tokenLocation t
        p t@(Token TBool _ _ _ _ _) = Just $ BasicType (Bool $ tokenLocation t) $ tokenLocation t
        p t@(Token TVoid _ _ _ _ _) = Just $ Void $ tokenLocation t
        p _ = Nothing          

program :: Monad m => ParsecT [Token] u m Program
program = do
            decls <- list' declaration
            return $ Program decls

declaration :: Monad m => ParsecT [Token] u m Declaration
declaration = funDec <|> varDec <|> arrDec <|> funDef <|> varDef <|> arrDef <?> "top level declaration"
    where
        funDec = do 
                try $ do
                    tokExt <- (tok TExtern)
                    header <- functionHeader Imported
                    tokS <- (tok TSemiColon)
                    return $ FunctionDeclaration header $ tokenLocationBetween tokExt tokS
        varDec = do 
                try $ do 
                   tokExt <- (tok TExtern) 
                   bt <- basicType
                   id <- identifier
                   tokS <- (tok TSemiColon)
                   let loc = tokenLocationBetween tokExt tokS
                   return $ GlobalDeclaration (VariableDefinition bt id Nothing Imported Nothing loc) $ loc
        arrDec = do
                   tokExt <- (tok TExtern) 
                   bt <- basicType
                   sizes <- brackets $ listWith' (tok TComma) identifier
                   id <- identifier
                   tokS <- (tok TSemiColon)
                   let loc = tokenLocationBetween tokExt tokS
                   return $ GlobalDeclaration (ArrayDeclaration bt id sizes Imported loc) $ loc
        funDef = do 
                ((exported, t), header) <- try $ do
                   (exported, t) <- export
                   let origin = case exported of True  -> Exported
                                                 False -> Neither
                   header <- functionHeader origin
                   return ((exported, t), header)

                body <- functionBody
                -- | TODO: between location
                return $ FunctionDefinition header exported body $ loc t (header ^. location)
                where
                    loc :: Maybe Token -> Location -> Location
                    loc (Just t) _ = tokenLocation t
                    loc Nothing l = l
        varDef = do 
                try $ do
                   (exported, t) <- export
                   bt <- basicType
                   (id, ti) <- identifierWithTok
                   init <- (optionMaybe (tok TEqualSign >> expression)) 
                   tokS <- (tok TSemiColon)

                   let origin = case exported of True  -> Exported
                                                 False -> Neither

                   let loc = tokenLocationBetween (t1 t ti) tokS
                   return $ GlobalDefinition (VariableDefinition bt id init origin Nothing loc) exported $ loc
                   where
                        t1 :: Maybe Token -> Token -> Token
                        t1 (Just tk) ti = tk
                        t1 _ ti = ti
        arrDef = do
                   (exported, t) <- export
                   bt <- basicType
                   sizes <- brackets expressionList'
                   id <- identifier
                   init <- (optionMaybe (tok TEqualSign >> Compiler.Parser.arrayInitializer))
                   tokS <- (tok TSemiColon)
                   let loc = locf t (bt ^. location) (tokenAt tokS)
                   let origin = case exported of True  -> Exported
                                                 False -> Neither
                   return $ GlobalDefinition (ArrayDefinition bt id sizes init origin Nothing loc) exported $ loc
                   where
                        locf :: Maybe Token -> Location -> Position -> Location
                        locf (Just (Token _ c1 l1 _ gl f)) _ (At l3 c3)
                            = Location (Between l1 c1 l3 c3) f gl
                        -- | Note that we are ensured here that 
                        -- bt ^.  location returns an At _ _
                        locf Nothing (Location (At l2 c2) f gl) (At l3 c3)
                            = Location (Between l2 c2 l3 c3) f gl
        export = satisfy' p <|> (return (False, Nothing))
            where
                p :: Token -> Maybe (Bool, Maybe Token)
                p t@(Token TExport _ _ _ _ _) = Just (True, Just t)
                p _ = Nothing

parameter :: Monad m => ParsecT [Token] u m Parameter
parameter = try var <|> array <?> "parameter"
    where
        var = do
                bt <- basicType
                (id, t) <- identifierWithTok
                return $ Parameter bt id $ loc (bt ^. location) (tokenAt t)
                where
                    loc :: Location -> Position -> Location
                    loc (Location (At l1 c1) _ gl) (At l2 c2) = Location (Between l1 c1 l2 c2) "" gl
        array = do
                  bt <- basicType
                  sizes <- brackets $ listWith' (tok TComma) identifier
                  (id, t) <- identifierWithTok
                  return $ ArrayParameter bt id sizes $ loc (bt ^. location) (tokenAt t)
                where
                    loc :: Location -> Position -> Location
                    loc (Location (At l1 c1) _ gl) (At l2 c2) = Location (Between l1 c1 l2 c2) "" gl

-- | TODO: position correctly
functionBody :: Monad m => ParsecT [Token] u m FunctionBody
functionBody = braces internal <?> "function body"
    where
        internal = do
            -- XXX: Added a try here since local functions
            -- would result in a parse error without it!
            -- TODO: Maybe this could be done a bit better?
                     vars <- list $ try localDeclaration
                     localFuns <- list localFunctionDeclaration
                     body <- list statement
                     return $ FunctionBody vars localFuns body Nothing $ defaultLocation

functionHeader :: Monad m => Origin -> ParsecT [Token] u m FunctionHeader
functionHeader o = h <?> "function header" 
            where h = do
                   rt <- Compiler.Parser.returnType
                   id <- identifier
                   lp <- tok TLeftParen
                   params <- listWith (tok TComma) parameter
                   rp <- tok TRightParen

                   
                   return $ FunctionHeader rt id params Nothing o Nothing $ locationBetween (rt ^. location) $ (tokenLocation rp)

-- | TODO: might need a better location
localFunctionDeclaration :: Monad m => ParsecT [Token] u m LocalFunctionDeclaration
localFunctionDeclaration = do
                             header <- functionHeader Neither
                             body <- functionBody
                             return $ LocalFunctionDeclaration header body $ header ^. location

localDeclaration :: Monad m => ParsecT [Token] u m LocalDeclaration
localDeclaration = (try var <|> try array) <* tok TSemiColon <?> "variable declaration"
    where
        var = do
                bt <- basicType
                (id, t) <- identifierWithTok
                init <- optionMaybe (tok TEqualSign >> expression)
                let loc = locf bt t init
                return $ LocalDeclaration (VariableDefinition bt id init Neither Nothing loc) $ loc
                where 
                      locf :: BasicType -> Token -> Maybe Expression -> Location
                      locf bt _ (Just expr) = locationBetween (bt ^. location) (expr ^. location)
                      locf bt t _ = locationBetween (bt ^. location) (tokenLocation t)
        array = do
                  bt <- basicType
                  size <- brackets expressionList'
                  (id, t) <- identifierWithTok
                  init <- optionMaybe (tok TEqualSign >> Compiler.Parser.arrayInitializer)
                  let loc = locf bt t init
                  return $ LocalDeclaration (ArrayDefinition bt id size init Neither Nothing loc) $ loc
                where 
                      locf :: BasicType -> Token -> Maybe ArrayInitializer -> Location
                      locf bt _ (Just expr) = locationBetween (bt ^. location) (expr ^. location)
                      locf bt t _ = locationBetween (bt ^. location) (tokenLocation t)

statementList :: Monad m => ParsecT [Token] u m [Statement]
statementList = list statement

statement :: Monad m => ParsecT [Token] u m Statement
statement = ((try assign <|> try elemAssign <|> call <|> ret <|> doWhile) <* tok TSemiColon) <|> while <|> ifStatement <|> for <?> "statement"
    where
        assign = do
                   (id, t) <- identifierWithTok
                   expr <- tok TEqualSign >> expression
                   return $ AssignmentStatement id expr Nothing $ locationBetween (expr ^. location) $ tokenLocation t
        elemAssign = do
                       (id, t) <- identifierWithTok
                       indices <- brackets expressionList'
                       expr <- tok TEqualSign >> expression
                       return $ ElemAssignmentStatement id indices expr Nothing $ locationBetween (expr ^. location) $ tokenLocation t
        call = do
                 fc <- functionCall
                 return $ FunctionCallStatement fc $ fc ^. location
        while = do
                  t <- tok TWhile
                  condition <- parens expression
                  body <- block
                  return $ WhileStatement condition body $ tokenLocation t
        ret = do
                t <- tok TReturn
                expr <- (optionMaybe expression)
                return $ ReturnStatement expr Nothing $ loc (tokenLocation t) expr
                where
                    loc :: Location -> Maybe Expression -> Location
                    loc l (Just expr) = locationBetween l $ expr ^. location
                    loc l _ = l
        doWhile = do
                    td <- tok TDo
                    body <- block
                    expr <- tok TWhile >> parens expression
                    return $ DoWhileStatement expr body $ locationBetween (tokenLocation td) (expr ^. location)
        ifStatement = do
                        t <- tok TIf
                        expr <- parens expression
                        thenBody <- block
                        elseBody <- optionMaybe (tok TElse >> block)
                        return $ IfStatement expr thenBody elseBody $ locationBetween (tokenLocation t) (loc thenBody elseBody)
                        where
                            loc :: [Statement] -> Maybe [Statement] -> Location
                            loc _ (Just (f:_)) = f ^. location
                            loc (f:_) _ = f ^. location
                            loc _ _ = defaultLocation
        for = do
                
                t <- tok TFor
                name <- tok TLeftParen >> tok TInt >> identifier
                init <- tok TEqualSign >> expression
                end <- tok TComma >> expression
                step <- optionMaybe (tok TComma >> expression) <* tok TRightParen
                body <- block
                return $ ForStatement name init end step body $ loc t body
                where
                    loc t [] = tokenLocation t
                    loc t body = locationBetween (tokenLocation t) ((last body) ^. location)

block :: Monad m => ParsecT [Token] u m [Statement]
block = braces statementList <|> single <?> "block statement"
    where
        single = do
                   st <- statement
                   return [st]

arrayInitializer :: Monad m => ParsecT [Token] u m ArrayInitializer
arrayInitializer = arr <|> expr <?> "array initializer"
    where
        arr = do
                exprs <- brackets (listWith (tok TComma) Compiler.Parser.arrayInitializer)
                return $ ArrayInitializer (fromList exprs) (locationList exprs)
        expr = do
                 e <- expression
                 return $ ArrayInitializerExpression e $ e ^. location

expressionList :: Monad m => ParsecT [Token] u m [Expression]
expressionList = listWith (tok TComma) expression

expressionList' :: Monad m => ParsecT [Token] u m (NonEmpty Expression)
expressionList' = listWith' (tok TComma) expression

expression :: Monad m => ParsecT [Token] u m Expression
expression = do
               binExpr <- orExpression
               return $ Expression binExpr $ binExpr ^. location

binExpr :: Monad m => ParsecT [Token] u m BinaryExpression -> ParsecT [Token] u m BinaryExpression -> ParsecT [Token] u m BinaryOperator -> ParsecT [Token] u m BinaryExpression
binExpr left right op = do
                          lhs <- left
                          rem <- binExpr' lhs
                          return rem
    where
        binExpr' lhs = binary <|> (return lhs) <?> "binary expression expected"
            where
                binary = do
                           operator <- op
                           rhs <- right
                           rem <- binExpr' $ BinaryExpression lhs rhs operator $ locationBetween (operator ^. location) (rhs ^. location)
                           return rem

binExpr' :: Monad m => ParsecT [Token] u m BinaryExpression -> ParsecT [Token] u m BinaryOperator -> ParsecT [Token] u m BinaryExpression
binExpr' left op = binExpr left left op

orExpression :: Monad m => ParsecT [Token] u m BinaryExpression
orExpression = binExpr' andExpression op <?> "logical or expression"
    where
        op = satisfy' p
        p :: Token -> Maybe BinaryOperator
        p t@(Token TDoublePipe _ _ _ _ _) = Just $ Or $ tokenLocation t
        p _ = Nothing

andExpression :: Monad m => ParsecT [Token] u m BinaryExpression
andExpression = binExpr' equalityExpression op <?> "logical and expression"
    where
        op = satisfy' p <?> "and operator"
        p :: Token -> Maybe BinaryOperator
        p t@(Token TDoubleAmpersand _ _ _ _ _) = Just $ And $ tokenLocation t
        p _ = Nothing

equalityExpression :: Monad m => ParsecT [Token] u m BinaryExpression
equalityExpression = binExpr' relationalExpression op
    where
        op = satisfy' p <?> "equality operator"
        p :: Token -> Maybe BinaryOperator
        p t@(Token TDoubleEqual _ _ _ _ _) = Just $ Equal $ tokenLocation t
        p t@(Token TNotEqual _ _ _ _ _) = Just $ NotEqual $ tokenLocation t
        p _ = Nothing

relationalExpression :: Monad m => ParsecT [Token] u m BinaryExpression
relationalExpression = binExpr' additiveExpression op
    where
        op = satisfy' p <?> "relational operator"
        p :: Token -> Maybe BinaryOperator
        p t@(Token TGreaterThan _ _ _ _ _) = Just $ GreaterThan $ tokenLocation t
        p t@(Token TGreaterEqual _ _ _ _ _) = Just $ GreaterEqual $ tokenLocation t
        p t@(Token TLessThan _ _ _ _ _) = Just $ LessThan $ tokenLocation t
        p t@(Token TLessEqual _ _ _ _ _) = Just $ LessEqual $ tokenLocation t
        p _ = Nothing
            

additiveExpression :: Monad m => ParsecT [Token] u m BinaryExpression
additiveExpression = binExpr' multiplicativeExpression op
    where
        op = satisfy' p <?> "additive operator"
        p :: Token -> Maybe BinaryOperator
        p t@(Token TPlusSign _ _ _ _ _) = Just $ Add $ tokenLocation t
        p t@(Token TMinusSign _ _ _ _ _) = Just $ Subtract $ tokenLocation t
        p _ = Nothing

multiplicativeExpression :: Monad m => ParsecT [Token] u m BinaryExpression
multiplicativeExpression = do
                             left <- unaryExpression
                             rem <- multiplicativeExpression' $ PassThrough left $ left ^. location
                             return rem

multiplicativeExpression' :: Monad m => BinaryExpression -> ParsecT [Token] u m BinaryExpression
multiplicativeExpression' left = binary <|> (return left) <?> "multiplicative expression"
    where
        binary = do
                   op <- multiplicativeOperator
                   right <- unaryExpression
                   rem <- multiplicativeExpression' $ BinaryExpression left (PassThrough right $ right ^. location) op $ locationBetween (left ^. location) (right ^. location)
                   return rem
        multiplicativeOperator :: Monad m => ParsecT [Token] u m BinaryOperator
        multiplicativeOperator = satisfy' p <?> "multiplicative operator"
            where
                p :: Token -> Maybe BinaryOperator
                p t@(Token TSolidus _ _ _ _ _) = Just $ Divide $ tokenLocation t
                p t@(Token TAsterisk _ _ _ _ _) = Just $ Multiply $ tokenLocation t
                p t@(Token TPercentSign _ _ _ _ _) = Just $ Modulus $ tokenLocation t
                p _ = Nothing

unaryExpression :: Monad m => ParsecT [Token] u m UnaryExpression
unaryExpression = (try castExpression) <|> unaryExpr <|> passThrough <?> "unary expression"
    where
        passThrough = do
                        expr <- basicExpression
                        return $ UnaryPassThrough expr $ expr ^. location 
        castExpression = do
                           lb <- tok TLeftParen
                           bt <- basicType
                           rb <- tok TRightParen
                           expr <- unaryExpression
                           return $ CastExpression bt expr $ locationBetween (tokenLocation lb) (expr ^. location)
        unaryExpr = do
                      op <- unaryOperator
                      expr <- unaryExpression
                      return $ UnaryExpression op expr $ locationBetween (op ^. location) (expr ^. location)
        unaryOperator :: Monad m => ParsecT [Token] u m UnaryOperator
        unaryOperator = satisfy' p <?> "unary operator"
            where
                p :: Token -> Maybe UnaryOperator
                p t@(Token TMinusSign _ _ _ _ _) = Just $ Negate $ tokenLocation t
                p t@(Token TNotSign _ _ _ _ _) = Just $ Not $ tokenLocation t
                p _ = Nothing              

basicExpression :: Monad m => ParsecT [Token] u m BasicExpression
basicExpression = literal <|> parenExpr <|> try callExpr <|> try arrExpr <|> identifierExpression <?> "basic expression"
    where
        parenExpr = do
                      expr <- parens expression
                      return $ ParenthesizedExpression expr $ expr ^. location
        callExpr = do
                     fc <- functionCall
                     return $ FunctionCallExpression fc $ fc ^. location
        arrExpr = do
                    (id, t) <- identifierWithTok
                    expr <- brackets expressionList'
                    return $ ArrayExpression id expr Nothing $ locationBetween (tokenLocation t) ((last $ toList expr) ^. location)

literal :: Monad m => ParsecT [Token] u m BasicExpression
literal = satisfy' p <?> "literal"
    where
        p :: Token -> Maybe BasicExpression
        p t@(Token TIntegerLiteral _ _ (Just v) _ _) = Just $ Literal (IntegerValue $ read v) $ tokenLocation t
        p t@(Token TFloatLiteral _ _ (Just v) _ _) = Just $ Literal (FloatValue $ read v) $ tokenLocation t
        p t@(Token TTrue _ _ _ _ _) = Just $ Literal (BooleanValue True) $ tokenLocation t
        p t@(Token TFalse _ _ _ _ _) = Just $ Literal (BooleanValue False) $ tokenLocation t
        p _ = Nothing

identifierExpression :: Monad m => ParsecT [Token] u m BasicExpression
identifierExpression = do
                         (id, t) <- identifierWithTok
                         return $ IdentifierExpression id Nothing $ tokenLocation t

tokenLocation :: Token -> Location
tokenLocation (Token _ c l _ getl f) = Location (At l c) f getl

tokenAt (Token _ c l _ _ _) = At l c

-- TODO: should we assume the same getLine here?
tokenLocationBetween :: Token -> Token -> Location
tokenLocationBetween (Token _ c l _ getl f) (Token _ c2 l2 _ _ _) = Location (Between l c l2 c2) f getl

-- TODO: should we assume the same getLine here?
locationBetween :: Location -> Location -> Location
locationBetween (Location (At l1 c1) f gl) (Location (At l2 c2) _ _) = Location (Between l1 c1 l2 c2) f gl
locationBetween (Location (Between l1 c1 _ _) f gl) (Location (At l2 c2) _ _) = Location (Between l1 c1 l2 c2) f gl
locationBetween (Location (At l1 c1) f gl) (Location (Between l2 c2 _ _) _ _) = Location (Between l1 c1 l2 c2) f gl
locationBetween (Location (Between l1 c1 _ _) f gl) (Location (Between _ _ l2 c2) _ _) = Location (Between l1 c1 l2 c2) f gl

locationList :: HasLocation a Location => [a] -> Location
locationList [] = defaultLocation
locationList l1 = locationBetween ((head l1) ^. location) ((last l1) ^. location)

locationListBetween :: (HasLocation a Location, HasLocation b Location) => [a] -> [b] -> Location
locationListBetween [] _ = defaultLocation
locationListBetween _ [] = defaultLocation
locationListBetween l1 l2 = locationBetween ((head l1) ^. location) ((last l2) ^. location)
