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

frontend :: Frontend ([Grammar.Func], [Grammar.Symbol])
frontend = do
  Parser.parse
  symbols <- gets Frontend.gSymbols
  funcs <- gets Frontend.funcs
  return (funcs, symbols)

backend mode = do
  execSetupGlobalSymtab
  execCallMainFunc
  execFuncDefs
  code <- case mode of
    CodeOutputTranslated -> do
      code <- getCodeTranslated
      return $ map toString code
    CodeOutputTranslatedWithAddress -> do
      code <- getCodeTranslated
      let codeNumbered = zip [codeStartAddr,codeStartAddr+2..] code
      return $ map (\(i, c) -> (show i) ++ ":\t" ++ (show c)) codeNumbered
    CodeOutputUntranslated -> do
      codeLabelled <- getCodeLabelled
      return $ map (\(i, c) -> (show i) ++ ":\t" ++ (show c)) codeLabelled
  return $ unlines (xexeHeader ++ code)

data CodeOutputMode
  = CodeOutputTranslated
  | CodeOutputTranslatedWithAddress
  | CodeOutputUntranslated
  deriving (Eq)

main :: IO ()
main = do
  args_ <- getArgs
  let (codeOutputMode, args) = case args_ of
        ("-u" : args') -> (CodeOutputUntranslated, args')
        ("-n" : args') -> (CodeOutputTranslatedWithAddress, args')
        _              -> (CodeOutputTranslated, args_)

  (inputFile, outputFile) <- case args of
    [inputFile] -> return (inputFile, replaceExtension inputFile ".xsm")
    [inputFile, outputFile] -> return (inputFile, outputFile)
    _ -> do
      putStrLn "Syntax: expl [-u] input [output]"
      exitFailure
  input <- readFile inputFile
  handleError input inputFile $ do
    (funcs, symbols) <- liftEither
      $ Frontend.runFrontend (Frontend.initData input) frontend
    output <- liftEither $ Codegen.runCodegen (backend codeOutputMode) (initCodegenState symbols funcs)
    liftIO $ writeFile outputFile output
 where
  handleError :: String -> String -> ExceptT Error IO a -> IO ()
  handleError input inputFile ex = do
    run <- runExceptT ex
    case run of
      Left e ->
        mapM_ (\(str, span) -> printError str span input inputFile) e
      Right _ -> return ()

