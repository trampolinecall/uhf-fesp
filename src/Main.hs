module Main (main) where

import qualified AST
import qualified SIR
import qualified ToSIR
import qualified Lexer
import qualified Parser
import qualified IRPP

import Control.Monad.Writer (Writer, WriterT)
import qualified Control.Monad.Writer as Writer

main :: IO ()
main = do
    contents <- getContents
    let tokens = Lexer.lex contents
    let ast = Parser.parse tokens
    case ast of
        Right ast -> do
            let (sir, errs) = Writer.runWriter $ ToSIR.convert ast
            case errs of
                [] ->
                    putStrLn $ IRPP.dump_main_module sir

                errs ->
                    putStrLn $ "errors in converting to sir: " ++ show errs
        Left err ->
            putStrLn $ "parse error: " ++ show err
