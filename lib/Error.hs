module Error where

import Span

type Error = [(String, Span)]

customError :: String -> Span -> Error
customError string span = [(string, span)]

syntaxError :: Span -> Error
syntaxError span = [("Syntax Error", span)]

compilerError :: String -> Span -> Error
compilerError s span = [("Compiler Error: " ++ s, span)]
