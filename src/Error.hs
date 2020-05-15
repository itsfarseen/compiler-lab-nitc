{-# LANGUAGE DuplicateRecordFields #-}
module Error where

import Span
import Symbol (DataType)

type Error = [(String, Span)]

syntaxError :: Span -> Error
syntaxError span = [("Syntax Error", span)]

identifierNotDeclared :: String -> Span -> Error
identifierNotDeclared identName span =
  [("Identifier not declared: " ++ identName, span)]

identifierRedeclared :: String -> Span -> Span -> Error
identifierRedeclared identName declSpan span =
  [ ("Identifier redeclared: " ++ identName, span)
  , ("Was already declared here"           , declSpan)
  ]

identifierNotInitialized :: String -> Span -> Span -> Error
identifierNotInitialized identName declSpan span =
  [ ("Identifier not initialized: " ++ identName, span)
  , ("Was declared here"                        , declSpan)
  ]


assignmentTypeMismatch :: DataType -> Span -> DataType -> Span -> Error
assignmentTypeMismatch identDataType declSpan rhsDataType span =
  [ ( "Assignment type mismatch - "
      ++ show identDataType
      ++ " = "
      ++ show rhsDataType
    , span
    )
  , ("Was declared here", declSpan)
  ]

typeNotAllowed :: [DataType] -> DataType -> Span -> Error
typeNotAllowed allowedTypes gotType span =
  [ ( "Type not allowed: "
      ++ show gotType
      ++ ". Allowed types: "
      ++ show allowedTypes
    , span
    )
  ]


