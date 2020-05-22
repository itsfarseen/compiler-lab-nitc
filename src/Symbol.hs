module Symbol where

import Span (Span)

data Symbol a =
  Symbol
    { name :: String
    , dataType :: DataType
    , declSpan :: Span
    , ext :: a
    }
    deriving Show

data DataType
  = DataTypeInt
  | DataTypeBool
  | DataTypeArray Int DataType
  deriving Eq

instance Show DataType where
  show DataTypeInt                = "Int"
  show DataTypeBool               = "Bool"
  show (DataTypeArray size inner) = show inner ++ "[" ++ show size ++ "]"


getSize :: DataType -> Int
getSize DataTypeInt                = 1
getSize DataTypeBool               = 1
getSize (DataTypeArray size inner) = size * getSize inner


-- TODO: Move Symbol to SymbolTable.hs and DataType to TypeTable.hs
