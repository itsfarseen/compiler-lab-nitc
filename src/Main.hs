module Main where

import Control.Monad.State
import Control.Monad.Except (ExceptT, runExceptT, liftEither)
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import qualified Backend.Main as Backend
import Error (Error)
import Frontend
import qualified Grammar
import Parser
import Span

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

frontend :: Frontend (Grammar.Program, [Grammar.Symbol])
frontend = do
  program <- Parser.parse
  symbols <- gets Frontend.symbols
  return (program, symbols)

main :: IO ()
main = do
  args_ <- getArgs
  let (codeOutputMode, args) = case args_ of
        ("-u" : args') -> (Backend.CodeOutputUntranslated, args')
        ("-n" : args') -> (Backend.CodeOutputTranslatedWithAddress, args')
        _              -> (Backend.CodeOutputTranslated, args_)

  (inputFile, outputFile) <- case args of
    [inputFile] -> return (inputFile, replaceExtension inputFile ".xsm")
    [inputFile, outputFile] -> return (inputFile, outputFile)
    _ -> do
      putStrLn "Syntax: expl [-u] input [output]"
      exitFailure
  input <- readFile inputFile
  handleError input inputFile $ do
    (program, symbols) <- liftEither
      $ Frontend.runFrontend (Frontend.initData input) frontend
    output <- liftEither $ Backend.main program codeOutputMode symbols
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
