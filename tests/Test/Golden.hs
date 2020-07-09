{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Test.Golden where

import Test.Tasty
import Test.Tasty.Golden
import System.Directory
import System.FilePath
import qualified Parser
import qualified Frontend
import qualified Backend.Simulator as Simulator
import qualified Backend.Codegen as Codegen
import Control.Monad.Except
import Error (Error, panicError)
import Data.ByteString.Lazy.UTF8 (fromString)
import Test.LibraryUtils (loadLibrary)

import Debug.Trace
-- dbg v = trace (toString v) v
dbgs :: Show a => [Char] -> a -> a
dbgs s v = trace (s ++ show v) v
-- dbgst s v = trace (s ++ v) v

main :: IO TestTree
main = do
  dir <- getCurrentDirectory
  putStrLn dir
  let goldenDir = dir </> "tests" </> "Test" </> "golden"
  explFiles <- map (goldenDir </>) <$> listDirectory goldenDir
  return
    $ testGroup "Golden tests group"
    $ map
        (\explFile ->
          let
            stdinFile  = explFile -<.> "stdin"
            goldenFile = explFile -<.> "stdout"
          in goldenVsStringDiff
            (takeFileName explFile)
            (\ref new -> ["diff", "--side-by-side", ref, new])
            goldenFile
            (fromString <$> runGoldenTest explFile stdinFile)
        )
    $ (filter (\x -> takeExtension x == ".expl") explFiles)

runGoldenTest :: FilePath -> FilePath -> IO String
runGoldenTest explFile stdinFile = do
  library <- loadLibrary
  expl    <- readFile explFile
  stdin   <- lines <$> readFile stdinFile
  program <- handleError expl explFile $ do
    liftEither $ Frontend.runFrontend
      (Frontend.initData expl)
      do
        res <- Parser.parse
        res' <- liftEither res
        return res'
  let code      = Codegen.compileXEXE program
  let simulator = Simulator.run (Simulator.initWithStdin stdin code library)
  let stdout    = unlines $ Simulator.getStdout simulator
  return stdout


handleError :: String -> String -> ExceptT (Error) IO a -> IO a
handleError input inputFile ex = do
  !run <- runExceptT ex
  case run of
    Left e ->
      print e
        >> mapM_ (\(str, span) -> panicError str span input inputFile) e
        >> return (error "UNREACH")
    Right r -> return r
