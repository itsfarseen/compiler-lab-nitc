{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
module Test.Golden where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import System.Directory
import System.FilePath
import qualified Parser
import qualified Frontend
import qualified Backend.Simulator as Simulator
import qualified Backend.Codegen as Codegen
import Control.Monad.Except
import Control.Monad.State.Strict
import Error (Error, panicError)
import Data.ByteString.Lazy.UTF8 (fromString)

main :: IO TestTree
main = do
  dir <- getCurrentDirectory
  putStrLn dir
  let goldenDir = dir </> "tests" </> "Test" </> "golden"
  explFiles <- map (goldenDir </>) <$> listDirectory goldenDir
  return $ testGroup "Golden tests group"
    $ map
        (\explFile ->
          let
            stdinFile  = explFile -<.> "stdin"
            goldenFile = explFile -<.> "stdout"
            -- golden <- readFile goldenFile
            -- stdout <- runGoldenTest explFile stdinFile
            -- golden @=? stdout
          in goldenVsStringDiff
            (takeFileName explFile)
            (\ref new -> ["diff", "--side-by-side", ref, new])
            goldenFile
            (fromString <$> runGoldenTest explFile stdinFile)
        )
    $ (filter (\x -> takeExtension x == ".expl") explFiles)

runGoldenTest :: FilePath -> FilePath -> IO String
runGoldenTest explFile stdinFile = do
  expl  <- readFile explFile
  stdin <- lines <$> readFile stdinFile
  (funcs, symbols) <- handleError expl explFile $ do
    liftEither $ Frontend.runFrontend
      (Frontend.initData expl)
      do
        Parser.parse
        symbols <- gets Frontend.gSymbols
        funcs   <- gets Frontend.funcs
        return (funcs, symbols)
  code <- handleError expl explFile $ do
    liftEither $ Codegen.runCodegen
      (do
        Codegen.execSetupGlobalSymtab
        Codegen.execCallMainFunc
        Codegen.execFuncDefs
        Codegen.getCodeTranslated
      )
      (Codegen.initCodegenState symbols funcs)
  let simulator = Simulator.run (Simulator.initWithStdin stdin code)
  let stdout = unlines $ Simulator.getStdout simulator
  return stdout


handleError :: String -> String -> ExceptT (Error) IO a -> IO a
handleError input inputFile ex = do
  !run <- runExceptT ex
  case run of
    Left  e -> print e >> mapM_ (\(str, span) -> panicError str span input inputFile) e >> return (error "UNREACH")
    Right r -> return r