module Compiler.Preprocessor (
    preprocess,
    scanAndPreprocess,
) where

import Data.List.Split (splitOn)
import Compiler.Error hiding (value)
import Compiler.Token hiding (getLine)
import qualified Compiler.Token as Token (getLine)
import System.IO
import System.Exit
import Compiler.Scanner
import Data.Set hiding (filter)

-- A very simple preprocessor that injects filenames and getLines in tokens. 
-- Also supports the #include preprocessor directive.
-- TODO: the preprocessor error messages are very bold

-- | Scan a file given the filename and its contents. The tokens scanned
-- are injected with the filename and the appropriate getLine functions
scanAndPreprocess :: String -> String -> IO [Token]
scanAndPreprocess fn is = do
        t <- case scan (gl is) is of
                Left err -> 
                    do 
                       hPutStrLn stderr $ "Lexical error: " ++ err
                       exitFailure
                Right t ->
                    do return t

        let t' = fmap ((injectGetLine $ gl is) . injectFileName fn) t

        t'' <- preprocess (insert fn empty) t'
        return t''
        where
            gl :: String -> Int -> String
            gl s i = (splitOn "\n" s) !! (i - 1)


-- | Given a context and a list of tokens, preprocess these tokens.
-- The context is a set that contains the files we shouldn't preprocess
-- when encountering an #include of this file. Instead a loop include error
-- is printed.
preprocess :: (Set String) -> [Token] -> IO ([Token])
preprocess c t = do
                let s = splitOn [TPreprocessorStart] t
                (_, t') <- specialMapM preprocessDirectives c (tail s)
                return $ concat [head s, concat t']

-- | A mapM that also folds a context with the map
specialMapM :: (Monad m) => (b -> a -> m (b, c)) -> b -> [a] -> m (b, [c])
specialMapM _ v [] = return (v, [])
specialMapM f v (x : xs) = do
                             (v', x') <- f v x
                             (v'', xs') <- specialMapM f v' xs
                             return (v'', x' : xs')
        

preprocessDirectives :: (Set String) -> [Token] -> IO (Set String, [Token])
preprocessDirectives c l = do 
                           let t = filter (/= []) $ splitOn [TPreprocessorEnd] l
                           if length t /= 2
                               then do 
                                       hPutStrLn stderr "Unknown preprocessor directive!"
                                       exitFailure
                               else do 
                                       (c', p) <- preprocessDirective c $ (head $ head t, head t !! 1)
                                       return $ (c', concat [p, t !! 1])

preprocessDirective :: (Set String) -> (Token, Token) -> IO (Set String, [Token])
preprocessDirective c (Token { tokenType = TInclude } , Token { tokenType = TStringLiteral, value = Just s } ) = do
        t <- injectIf c (\a -> notMember a c) s
        let c' = insert s c
        return (c', t)
preprocessDirective _ (_, _) = do 
                                  hPutStrLn stderr "Unknown preprocessor directive!"
                                  exitFailure
                                -- TODO: better error
                                --
-- | Inject only when a predicate holds. Print "Include loop" and exit
-- otherwise.
injectIf :: Set String -> (String -> Bool) -> String -> IO [Token]
injectIf c e s | e s = injectTokensFrom c s
               | otherwise = do 
                              hPutStrLn stderr "Include loop"
                              exitFailure
-- | Get tokens from a given file and return them
-- TODO: could most likely be merged with normal scanAndPreprocess
injectTokensFrom :: Set String -> String -> IO [Token]
injectTokensFrom c s = do
        f <- readFile s
        t <- case scan (gl f) f of
                Left err -> 
                    do 
                       hPutStrLn stderr $ "Lexical error: " ++ err
                       exitFailure
                Right t ->
                    do return t
        let t' = fmap ((injectGetLine $ gl f) . injectFileName s) t

        t'' <- preprocess (insert s c) t'

        
        -- Here we remove the last token from the token list since this
        -- token is an EOF token
        return $ init t''
        where
            gl :: String -> Int -> String
            gl s i = (splitOn "\n" s) !! (i - 1)

-- | Inject a getLine inside a token
injectGetLine :: (Int -> String) -> Token -> Token
injectGetLine gl t@(Token {}) = t {Token.getLine = gl}
injectGetLine gl t = t

-- | inject a filename inside a token
injectFileName :: String -> Token -> Token
injectFileName f t@(Token {}) = t {file = f}
injectFileName f t = t
