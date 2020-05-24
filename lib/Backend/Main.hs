module Backend.Main (main, CodeOutputMode(..)) where

import qualified Backend.CodeUtils as CodeUtils
import Backend.CodeUtils (CodeOutputMode)
import qualified Backend.Codegen as Codegen
import qualified Backend.Compiler as Compiler
import Grammar (Program)
import Error (Error)
import SymbolTable (SymbolTable)


main :: Program -> CodeOutputMode -> SymbolTable () -> Either Error String
main program mode symtab = flip Compiler.runCompiler symtab $ do
  Codegen.parseProgram program
  CodeUtils.getCode mode
