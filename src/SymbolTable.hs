{-# LANGUAGE DuplicateRecordFields #-}

module SymbolTable where

import qualified Data.HashMap.Strict as HM

import Symbol (Symbol)
import qualified Symbol

newtype SymbolTable =
  SymbolTable (HM.HashMap String Symbol)

initSymbolTable :: SymbolTable
initSymbolTable = SymbolTable HM.empty

data SymbolExists =
  SymbolExists Symbol

insert :: Symbol -> SymbolTable -> Either SymbolExists SymbolTable
insert symbol symtab =
  let SymbolTable hm = symtab
      name           = Symbol.name symbol
  in  case HM.lookup name hm of
        Just symbol' -> Left $ SymbolExists symbol'
        Nothing      -> Right $ SymbolTable (HM.insert name symbol hm)

lookup :: String -> SymbolTable -> Maybe Symbol
lookup name symtab = let SymbolTable hm = symtab in HM.lookup name hm
