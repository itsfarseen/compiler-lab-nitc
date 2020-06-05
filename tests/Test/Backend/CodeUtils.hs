module Test.Backend.CodeUtils where

import Test.Tasty
import Test.Tasty.HUnit

test_getCode :: TestTree
test_getCode = testCaseSteps "getCode" $ \step -> do
{-
    Three variants:
    1. With labels
    2. Label translated                        <-- This is the important one
    3. Label translated, location numbered

    Might need the other three sometimes for debugging.

    With labels:
      don't need header, cos ain't gonna execute any way
      setup code
      code untranslated

    Label translated:
      header
      setup code
      label translate

    Label translated, location numbered
      number header 2048..2055
      code' <- setup code ++ label translate
      number code' 2056,2058..

    Where should setup code go:
      setup global symbol table
-}

  undefined
