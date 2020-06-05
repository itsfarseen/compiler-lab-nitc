{-# LANGUAGE LambdaCase #-}

module Test.Utils where

import Span
import Test.Tasty.HUnit

assertError :: HasCallStack => Either a b -> IO ()
assertError = \case
  Left _ -> return ()
  _      -> assertFailure "Error was not raised"

assertRight :: Show a => HasCallStack => Either a b -> IO b
assertRight = \case
  Left  error -> assertFailure $ "Error: " ++ (show error)
  Right x     -> return x

insertList :: Eq k => (a -> k) -> a -> [a] -> [a]
insertList key a list = case list of
  [] -> [a]
  (a' : as') ->
    (if key a == key a'
      then a : as'
      else a' : insertList key a as'
    )

spanW :: a -> SpanW a
spanW a = SpanW a (Span 0 0)
