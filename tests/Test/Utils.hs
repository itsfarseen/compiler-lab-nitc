{-# LANGUAGE LambdaCase #-}

module Test.Utils where

import Span
import Test.Tasty.HUnit


getRight :: Show a => HasCallStack => Either a b -> IO b
getRight = \case
  Left e -> assertFailure $ "Error: " ++ (show e)
  Right x     -> return x

assertRight :: Show a => Either a b -> IO ()
assertRight x = getRight x >> return ()

assertError :: HasCallStack => Either a b -> IO ()
assertError = \case
  Left _ -> return ()
  _      -> assertFailure "Error was not raised"

spanW :: a -> SpanW a
spanW a = SpanW a (Span 0 0)

span0 :: Span
span0 = Span 0 0

