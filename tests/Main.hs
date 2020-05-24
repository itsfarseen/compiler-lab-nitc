{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Debug.Trace
import Test.Tasty.TH
import Symbol
import SymbolTable (SymbolTable)
import qualified Backend.Compiler as Compiler
import qualified SymbolTable
import qualified Backend.Codegen as Codegen
import Data.Either.Extra
import Grammar

main :: IO ()
main = $(defaultMainGenerator)

{-
    int foo[10][7]
    foo[3][4]   - type = int            - size = 1
    foo[3]      - type = int[7]         - size = 7
    foo         - type = int[10][7]     - size = 10*7

-}

makeSymTab :: [Symbol ()] -> SymbolTable ()
makeSymTab symbols =
  let symtab = SymbolTable.initSymbolTable
  in  foldl (\symtab sym -> fromRight' $ SymbolTable.insert sym symtab)
            symtab
            symbols

case_foo :: Assertion
case_foo =
  fromRight'
    $ flip
        Compiler.runCompiler
        (makeSymTab
          [ Symbol
              { name     = "foo"
              , dataType = DataTypeArray 5  -- int foo[5][4][10]
                           $ DataTypeArray 4
                           $ DataTypeArray 10 DataTypeInt
              , declSpan = undefined
              , ext      = ()
              }
          ]
        )
    $ do
        let ident = LValueIdent "foo" undefined
        -- foo[2][3][7]
        let arrayIdx = LValueArrayIndex
              (Exp $ ExpNum 2)
              (LValueArrayIndex
                (Exp $ ExpNum 3)
                (LValueArrayIndex (Exp $ ExpNum 7) identLValue)
              )
        undefined

