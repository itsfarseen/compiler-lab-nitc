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
  show dataType =
    let (s, dims) = dataTypeDims dataType
    in  s ++ concatMap (\n -> "[" ++ show n ++ "]") dims

dataTypeDims dataType = case dataType of
  DataTypeInt  -> ("int", [])
  DataTypeBool -> ("bool", [])
  DataTypeArray dim inner ->
    let (s, dims) = dataTypeDims inner in (s, dim : dims)

dataTypeReverseAddDim :: DataType -> Int -> DataType
dataTypeReverseAddDim dataType size = case dataType of
  DataTypeInt  -> DataTypeArray size dataType
  DataTypeBool -> DataTypeArray size dataType
  DataTypeArray size' inner ->
    DataTypeArray size' (dataTypeReverseAddDim inner size)


getSize :: DataType -> Int
getSize DataTypeInt                = 1
getSize DataTypeBool               = 1
getSize (DataTypeArray size inner) = size * getSize inner


-- TODO: Move Symbol to SymbolTable.hs and DataType to TypeTable.hs
