module Main where

import Control.Monad.State
import Control.Monad.Except (ExceptT, runExceptT, liftEither)
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Backend.Codegen as Codegen
import Backend.Instructions
import Error (Error, printError)
import Frontend
import qualified Grammar
import Parser

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

frontend :: Frontend Grammar.Program
frontend = Parser.parse


backend :: CodeOutputMode -> Grammar.Program -> String
backend mode program = case mode of
  CodeOutputTranslated ->
    unlines $ xexeHeader ++ (map toString (compileXEXE program))
  CodeOutputUntranslated -> unlines $ map
    (\(i, c) -> i ++ ":\t\t" ++ (toString c))
    (compileXEXEUntranslated program)
  CodeOutputLibrary -> unlines $ map toString (compileLibrary program)

data CodeOutputMode
  = CodeOutputTranslated
  | CodeOutputUntranslated
  | CodeOutputLibrary
  deriving (Eq)

main :: IO ()
main = do
  args_ <- getArgs
  let
    (codeOutputMode, args) = case args_ of
      ("-u" : args') -> (CodeOutputUntranslated, args')
      ("-l" : args') -> (CodeOutputLibrary, args')
      _              -> (CodeOutputTranslated, args_)

  (inputFile, outputFile) <- case args of
    [inputFile] -> return (inputFile, replaceExtension inputFile ".xsm")
    [inputFile, outputFile] -> return (inputFile, outputFile)
    _ -> do
      putStrLn "Syntax: expl [-u | -l] input [output]"
      putStrLn "    -u Output code untranslated labels"
      putStrLn "    -l Compile library code"
      exitFailure
  input <- readFile inputFile
  handleError input inputFile $ do
    program <- liftEither $ Frontend.runFrontend
      (Frontend.initData input Grammar.gsInit)
      frontend
    let output = backend codeOutputMode program
    liftIO $ writeFile outputFile output
 where
  handleError :: String -> String -> ExceptT Error IO a -> IO ()
  handleError input inputFile ex = do
    run <- runExceptT ex
    case run of
      Left e ->
        mapM_ (\(str, span) -> printError str span input inputFile) e
      Right _ -> return ()

