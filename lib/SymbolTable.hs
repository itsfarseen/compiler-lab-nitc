{-# LANGUAGE NamedFieldPuns #-}

module SymbolTable where

import qualified Data.HashMap.Strict as HM

import Symbol (Symbol(..))
import qualified Symbol

newtype SymbolTable a =
  SymbolTable (HM.HashMap String (Symbol a))

initSymbolTable :: SymbolTable a
initSymbolTable = SymbolTable HM.empty

data SymbolExists a =
  SymbolExists (Symbol a)

insert :: Symbol a -> SymbolTable a -> Either (SymbolExists a) (SymbolTable a)
insert symbol symtab =
  let SymbolTable hm = symtab
      name           = Symbol.name symbol
  in  case HM.lookup name hm of
        Just symbol' -> Left $ SymbolExists symbol'
        Nothing      -> Right $ SymbolTable (HM.insert name symbol hm)

lookup :: String -> SymbolTable a -> Maybe (Symbol a)
lookup name symtab = let SymbolTable hm = symtab in HM.lookup name hm

map :: (a -> Symbol v1 -> (a, v2)) -> a -> SymbolTable v1 -> (a, SymbolTable v2)
map f z symtab =
  let SymbolTable hm = symtab
      syms           = HM.elems hm
      (z', hm')      = loop f z syms HM.empty
  in  (z', SymbolTable hm')
 where
  loop f a syms hm = case syms of
    [] -> (a, hm)
    (sym : syms') ->
      let (a', ext) = f a sym
          hm'       = HM.insert (Symbol.name sym) (sym { ext }) hm
      in  loop f a' syms' hm'
