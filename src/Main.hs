module Main where

import Codec.Binary.UTF8.String (encode)
import Control.Monad.State
import Control.Monad.Except (ExceptT, runExceptT, liftEither)
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import qualified Codegen.Codegen as Codegen
import Codegen.Compiler (Compiler)
import qualified Codegen.Compiler as Compiler
import Frontend
import Parser
import Span
import qualified Grammar
import SymbolTable(SymbolTable)
import qualified SymbolTable
import Error (Error)

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

frontend :: Frontend (Grammar.Program, SymbolTable ())
frontend = do
  program <- Parser.parse
  symtab  <- gets Frontend.symbolTable
  return (program, symtab)

backend :: Grammar.Program -> Compiler String
backend program = do
  Codegen.parseProgram program
  Compiler.getTranslatedCode

main :: IO ()
main = do
  args                    <- getArgs
  (inputFile, outputFile) <- case args of
    [inputFile] -> return (inputFile, replaceExtension inputFile ".xsm")
    [inputFile, outputFile] -> return (inputFile, outputFile)
    _ -> do
      putStrLn "Syntax: expl input [output]"
      exitFailure
  input <- readFile inputFile
  handleError input inputFile $ do
    (program, symtab) <- liftEither
      $ Frontend.runFrontend (Frontend.initData input) frontend
    output <- liftEither $ Compiler.runCompiler (backend program) symtab
    liftIO $ writeFile outputFile output
 where
  handleError :: String -> String -> ExceptT Error IO a -> IO ()
  handleError input inputFile ex = do
    run <- runExceptT ex
    case run of
      Left error ->
        mapM_ (\(str, span) -> printError str span input inputFile) error
      Right _ -> return ()


printError :: String -> Span -> String -> String -> IO ()
printError errorName errorSpan input inputFile = do
  let errorFullSpan              = getFullSpan errorSpan input
  let (errorLine, squigglesLine) = getErrorLineAndSquiggles errorFullSpan
  putStrLn
    $  inputFile
    ++ ":"
    ++ show (fspanLineNo errorFullSpan)
    ++ ":"
    ++ show (fspanColNo errorFullSpan)
    ++ " - "
    ++ errorName
  putStrLn errorLine
  putStrLn squigglesLine

getErrorLineAndSquiggles :: FullSpan -> (String, String)
getErrorLineAndSquiggles errorFullSpan =
  let errorLine     = fspanLine errorFullSpan
      squigglesLine = replicate (fspanColNo errorFullSpan - 1) ' '
        ++ replicate (fspanLength errorFullSpan) '^'
  in  (errorLine, squigglesLine)
