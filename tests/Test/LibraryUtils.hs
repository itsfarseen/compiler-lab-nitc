{-# LANGUAGE BangPatterns #-}
module Test.LibraryUtils where

import System.Directory
import System.FilePath
import qualified Parser
import qualified Frontend
import qualified Grammar as G
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
    liftEither $ Frontend.runFrontend
      (Frontend.initData libraryStr G.gsInit)
      Parser.parse
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
  
  