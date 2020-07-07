{-# LANGUAGE BangPatterns #-}
module Test.LibraryUtils where

import System.Directory
import System.FilePath
import qualified Parser
import qualified Frontend
import qualified Backend.Codegen as Codegen
import Backend.Instructions (XSMInstr)
import Control.Monad.Except
import Error (Error, panicError)

loadLibrary :: IO [XSMInstr]
loadLibrary = do
  dir <- getCurrentDirectory
  let library = dir </> "extra" </> "library.expl"
  libraryStr  <- readFile library
  program <- handleError library libraryStr $ do
    res <- liftEither $ Frontend.runFrontend
      (Frontend.initData libraryStr)
      Parser.parse
    res' <- liftEither $ res
    return res'
  return $ Codegen.compileLibrary program

handleError :: String -> String -> ExceptT (Error) IO a -> IO a
handleError input inputFile ex = do
  !run <- runExceptT ex
  case run of
    Left e ->
      print e
        >> mapM_ (\(str, span) -> panicError str span input inputFile) e
        >> return (error "UNREACH")
    Right r -> return r
  
  
