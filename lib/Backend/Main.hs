module Backend.Main (main, CodeOutputMode(..)) where

import qualified Backend.CodeUtils as CodeUtils
import Backend.CodeUtils (CodeOutputMode)
import qualified Backend.Codegen as Codegen
import qualified Backend.Compiler as Compiler
import Grammar (Program, Symbol)
import Error (Error)

main :: Program -> CodeOutputMode -> [Symbol] -> Either Error String
main program mode symbols = flip Compiler.runCompiler symbols $ do
  Codegen.parseProgram program
  CodeUtils.getCode mode

--- Steps
--   * Generate code for main function
--   * Generate code for other functions
--
