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
  deriving Eq

instance Show DataType where
  show DataTypeInt  = "Int"
  show DataTypeBool = "Bool"
