{
module Compiler.Scanner (scan) where

import Compiler.Token
import Text.Printf (printf)
import Compiler.Error
import Compiler.Position
}

%wrapper "monadUserState"

$stringTerm = [\" \n]
$stringChar = . # $stringTerm
$newline = \n
$white' = $white # $newline

tokens                                              :-
<0> $white+                                         ;
<0> "//".*                                          ;

-- | Enter a comment block. The reason for the explicit comment block state is
-- To report unclosed comment block errors correctly.
<0> "/*"                                             {enterComment `andBegin` block_comment}

<0> "#"                                              {returnTok TPreprocessorStart `andBegin` preprocessor}

<0> "while"                                          { tok TWhile}
<0> "do"                                             { tok TDo}
<0> "if"                                             { tok TIf}
<0> "else"                                           { tok TElse}
<0> "for"                                            { tok TFor}
<0> "return"                                         { tok TReturn}
<0> "extern"                                         { tok TExtern}
<0> "export"                                         { tok TExport}
<0> "void"                                           { tok TVoid}
<0> "bool"                                           { tok TBool}
<0> "int"                                            { tok TInt}
<0> "float"                                          { tok TFloat}
<0> "true"                                           { tok TTrue}
<0> "false"                                          { tok TFalse}
<0> "{"                                              { tok TLeftBrace}
<0> "}"                                              { tok TRightBrace}
<0> "["                                              { tok TLeftBracket}
<0> "]"                                              { tok TRightBracket}
<0> ";"                                              { tok TSemiColon}
<0> ","                                              { tok TComma}
<0> "("                                              { tok TLeftParen}
<0> ")"                                              { tok TRightParen}
<0> "="                                              { tok TEqualSign}
<0> "+"                                              { tok TPlusSign}
<0> "-"                                              { tok TMinusSign}
<0> "*"                                              { tok TAsterisk}
<0> "/"                                              { tok TSolidus}
<0> "%"                                              { tok TPercentSign}
<0> "!"                                              { tok TNotSign}
<0> "!="                                             { tok TNotEqual}
<0> "=="                                             { tok TDoubleEqual}
<0> "<"                                              { tok TLessThan}
<0> "<="                                             { tok TLessEqual}
<0> ">"                                              { tok TGreaterThan}
<0> ">="                                             { tok TGreaterEqual}
<0> "&&"                                             { tok TDoubleAmpersand}
<0> "||"                                             { tok TDoublePipe}
<0> 0|([1-9][0-9]*)                                  { tokVal TIntegerLiteral}
<0> (0|([1-9][0-9]*))(\.[0-9]+)?([eE][\+\-]?[0-9]+)? { tokVal TFloatLiteral}
<0> [a-zA-Z][a-zA-Z0-9_]*                            { tokVal TIdentifier}


-- While in a comment block, skip everything
<block_comment> .                                  ;
<block_comment> $newline                           ;
-- Unless "/*", which closes the comment block
<block_comment> \*\/                                { exitComment `andBegin` 0 }

<preprocessor> "include"                            { tok TInclude }
<preprocessor> \"                                   { enterStringLiteral `andBegin` pstring}
<preprocessor> $newline                             { returnTok TPreprocessorEnd `andBegin` 0 }
<preprocessor> $white'+                             ;

<pstring> [$stringChar]+                         { tokVal TStringLiteral }
<pstring> \"                                     { exitStringLiteral `andBegin` preprocessor }
<pstring> $newline                               { begin preprocessor }



{
-- data AlexState = AlexState
--                  { alex_pos :: !AlexPosn  -- position at current input location
--                  , alex_inp :: String     -- the current input
--                  , alex_chr :: !Char      -- the character before the input
--                  , alex_bytes :: [Byte]   -- rest of the bytes for the current char
--                  , alex_scd :: !Int       -- the current startcode
--                  , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
--                  }
-- type AlexInput = (AlexPosn,     -- current position,
--                   Char,         -- previous char
--                   [Byte],       -- pending bytes on current char
--                   String)       -- current input string


data AlexUserState = AlexUserState
                        { inComment  :: Bool
                        , commentLoc :: AlexPosn
                        , inString   :: Bool
                        , stringLoc  :: AlexPosn
                        , fGetLine   :: (Int -> String)
                        }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                        { inComment = False
                        , commentLoc = AlexPn (-1) (-1) (-1)
                        , inString = False
                        , stringLoc = AlexPn (-1) (-1) (-1)
                        , fGetLine = (\_ -> "")
                        }

alexInitUserStateGetLine :: (Int -> String) -> AlexUserState
alexInitUserStateGetLine getl = AlexUserState
                        { inComment = False
                        , commentLoc = AlexPn (-1) (-1) (-1)
                        , inString = False
                        , stringLoc = AlexPn (-1) (-1) (-1)
                        , fGetLine = getl
                        }


alexEOF :: Alex Token
alexEOF = return TEof

-- Sets the comment state to value
setComment :: Bool -> Alex ()
setComment v = Alex $ \s -> Right (s {alex_ust = (alex_ust s) {inComment = v}}, ())

setCommentLoc :: Alex ()
setCommentLoc = Alex $ \s -> Right (s {alex_ust = (alex_ust s) {commentLoc = alexAdd (alex_pos s) (AlexPn 0 0 (-2))}}, ())
        where
            alexAdd :: AlexPosn -> AlexPosn -> AlexPosn
            alexAdd (AlexPn a b c) (AlexPn a1 b1 c1) = AlexPn (a + a1) (b + b1) (c + c1)

-- Sets the comment state to value
setString :: Bool -> Alex ()
setString v = Alex $ \s -> Right (s {alex_ust = (alex_ust s) {inString = v}}, ())

setStringLoc :: Alex ()
setStringLoc = Alex $ \s -> Right (s {alex_ust = (alex_ust s) {stringLoc = alexAdd (alex_pos s) (AlexPn 0 0 (-2))}}, ())
        where
            alexAdd :: AlexPosn -> AlexPosn -> AlexPosn
            alexAdd (AlexPn a b c) (AlexPn a1 b1 c1) = AlexPn (a + a1) (b + b1) (c + c1)

enterComment :: AlexAction Token
enterComment _ _ = do
                    setComment True
                    setCommentLoc
                    alexMonadScan'


exitComment _ _ = do
                    setComment False
                    alexMonadScan'

enterStringLiteral :: AlexAction Token
enterStringLiteral _ _ = do
                    setString True
                    setStringLoc
                    alexMonadScan'
exitStringLiteral _ _ = do
                    setString False
                    alexMonadScan'

-- | scanNext scans the next token, adding it to the head of the token list
scanNext :: Alex [Token]
scanNext = do
              t <- alexMonadScan'
              if t == TEof
                 then do
                        cs <- getState inComment
                        ss <- getState inString

                        case (cs, ss) of
                            (True, _) -> commentNotClosedError
                            (_, True) -> stringNotClosedError
                            (_, _)    -> return [t]
                 else do
                     ts <- scanNext
                     return (t : ts)

-- | Our own implementation for alexMonadScan.
-- Gives more verbose error messages.
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),char,_,_) -> do
        getl <- getGetLine
        alexError $ show (LexError char (loc line column getl))
        where
           loc line column getl = Location (At line column) "" getl
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len


commentNotClosedError :: Alex [Token]
commentNotClosedError
    = do
        getl <- getGetLine
        AlexPn _ l c <- getLoc commentLoc
        alexError $ show (CommentNotClosedError $ (loc l c getl))
        where
           loc line column getl = Location (At line column) "" getl

stringNotClosedError :: Alex [Token]
stringNotClosedError
    = do
        getl <- getGetLine
        AlexPn _ l c <- getLoc stringLoc
        alexError $ show (StringNotClosedError $ (loc l c getl))
        where
           loc line column getl = Location (At line column) "" getl

scan :: (Int -> String) -> String -> Either String [Token]
scan getLine str = runAlex' getLine str scanNext

-- | Our own version of runAlex.
-- Does the same thing as runAlex except that it injects getl into the Alex
-- monad.
-- XXX: this is a bit of a hack, could be done neater
runAlex' :: (Int -> String) -> String -> Alex a -> Either String a
runAlex' getl input (Alex f)
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,
                        alex_chr = '\n',
                        alex_bytes = [],

                        alex_ust = alexInitUserStateGetLine getl,

                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a


getLoc :: (AlexUserState -> AlexPosn) -> Alex AlexPosn
getLoc f = Alex $ \s@AlexState{alex_ust=u} -> Right (s, f u)

getGetLine:: Alex (Int -> String)
getGetLine = Alex $ \s@AlexState{alex_ust=u} -> Right (s, fGetLine u)


getState :: (AlexUserState -> Bool) -> Alex Bool
getState f = Alex $ \s@AlexState{alex_ust=u} -> Right (s, f u)

getCommentState :: Alex Bool
getCommentState = Alex $ \s@AlexState{alex_ust=u} -> Right (s, inComment u)

getStringState :: Alex Bool
getStringState = Alex $ \s@AlexState{alex_ust=u} -> Right (s, inComment u)

tok :: TokenType -> AlexInput -> Int -> Alex Token
tok t ((AlexPn _ l c), _, _, _) _ = return (Token t c l Nothing (\_ -> "") "")

returnTok :: Token -> AlexInput -> Int -> Alex Token
returnTok t ((AlexPn _ l c), _, _, _) _ = return t

tokVal :: TokenType -> AlexInput -> Int -> Alex Token
tokVal t ((AlexPn _ l c), _, _, s) len = return (Token t c l (Just (take len s)) (\_ -> "") "")
}
