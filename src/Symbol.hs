module Symbol where

import Span (Span)

data Symbol =
  Symbol
    { name :: String
    , dataType :: DataType
    , declSpan :: Span
    }

data DataType
  = DataTypeInt
  | DataTypeBool
  deriving (Eq)

instance Show DataType where
  show DataTypeInt  = "Int"
  show DataTypeBool = "Bool"
