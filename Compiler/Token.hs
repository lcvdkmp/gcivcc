module Compiler.Token (
    Token(..),
    TokenType(..),
    ) where
import Compiler.Ast.Ast

data TokenType = TWhile
               | TDo
               | TIf
               | TElse
               | TExport
               | TExtern
               | TReturn
               | TFor
               | TVoid
               | TBool
               | TInt
               | TFloat
               | TTrue
               | TFalse
               | TLeftBrace
               | TRightBrace
               | TLeftBracket
               | TRightBracket
               | TLeftParen
               | TRightParen
               | TSemiColon
               | TComma
               | TEqualSign
               | TPlusSign
               | TMinusSign
               | TAsterisk
               | TSolidus
               | TPercentSign
               | TNotSign
               | TNotEqual
               | TDoubleEqual
               | TLessThan
               | TLessEqual
               | TGreaterThan
               | TGreaterEqual
               | TDoubleAmpersand
               | TDoublePipe
               | TIntegerLiteral
               | TFloatLiteral
               | TIdentifier
               | TInclude
               | TStringLiteral
               deriving (Eq, Enum)

qoute :: String -> String
qoute s = "'" ++ s ++ "'"

instance Show TokenType where
        show TWhile           = qoute "while"
        show TDo              = qoute "do"
        show TIf              = qoute "if"
        show TElse            = qoute "else"
        show TExport          = qoute "export"
        show TExtern          = qoute "extern"
        show TReturn          = qoute "return"
        show TFor             = qoute "for"
        show TVoid            = qoute "void"
        show TBool            = qoute "bool"
        show TInt             = qoute "int"
        show TFloat           = qoute "float"
        show TTrue            = qoute "true"
        show TFalse           = qoute "false"
        show TLeftBrace       = qoute "{"
        show TRightBrace      = qoute "}"
        show TLeftBracket     = qoute "["
        show TRightBracket    = qoute "]"
        show TLeftParen       = qoute "("
        show TRightParen      = qoute ")"
        show TSemiColon       = qoute ";"
        show TComma           = qoute ","
        show TEqualSign       = qoute "="
        show TPlusSign        = qoute "+"
        show TMinusSign       = qoute "-"
        show TAsterisk        = qoute "*"
        show TSolidus         = qoute "/"
        show TPercentSign     = qoute "%"
        show TNotSign         = qoute "!"
        show TNotEqual        = qoute "!="
        show TDoubleEqual     = qoute "=="
        show TLessThan        = qoute "<"
        show TLessEqual       = qoute "<="
        show TGreaterThan     = qoute ">"
        show TGreaterEqual    = qoute ">="
        show TDoubleAmpersand = qoute "&&"
        show TDoublePipe      = qoute "||"
        show TIntegerLiteral  = "integer literal"
        show TFloatLiteral    = "float literal"
        show TIdentifier      = "identifier"
        show TInclude         = qoute "include"
        show TStringLiteral   = "string literal"

data Token = Token
                { tokenType  :: TokenType
                , column     :: Int
                , lineNumber :: Int
                , value      :: Maybe String
                , getLine    :: (Int -> String)
                , file       :: String
                }
            | TEof
            | TPreprocessorStart
            | TPreprocessorEnd

instance Eq Token where
        (Token t c l v _ _) == (Token t1 c1 l1 v1 _ _) = t == t1 
                                                      && c == c1
                                                      && l == l1
                                                      && v == v1

        TEof               == TEof                     = True
        TPreprocessorStart == TPreprocessorStart       = True
        TPreprocessorEnd   == TPreprocessorEnd         = True
        _                  == _                        = False


instance Show Token where
        show (Token t c l Nothing _ _)  = "token " ++ (show t)
        show (Token t c l (Just v) _ _) = "token " ++ (show t) ++ "(" ++ v ++ ")"
        show TEof                       = "end of file token"
        show TPreprocessorStart         = "Preprocessor start"
        show TPreprocessorEnd           = "Preprocessor end"
