{-# LANGUAGE TypeApplications #-}
module Test.Library where

import Test.Tasty
import Test.Tasty.HUnit
import Backend.Codegen
import qualified Grammar as G
import Backend.Instructions
import Backend.Simulator as Simulator
import Test.LibraryUtils

compileGetCode :: Codegen a -> CodegenState -> [XSMInstr]
compileGetCode codegenM codegenState =
  fst $ compileGetCodeAndValue codegenM codegenState

compileGetCodeAndValue :: Codegen a -> CodegenState -> ([XSMInstr], a)
compileGetCodeAndValue codegenM codegenState = runCodegen
  (do
    execSetupGlobalSymtab
    ret <- codegenM
    appendCode [XSM_INT 10]
    execFuncDefs
    code <- getCodeTranslated codeStartAddrXEXE
    return (code, ret)
  )
  codegenState

runSimulator :: [XSMInstr] -> [String] -> IO Simulator.Machine
runSimulator code stdin =
  Simulator.run . Simulator.initWithStdin stdin code <$> loadLibrary


tests :: TestTree
tests = testGroup "Library" [test_Alloc]

test_Alloc :: TestTree
test_Alloc = testCaseSteps "alloc()" $ \step -> do
  step "Single alloc"
  let
    (code, r) = compileGetCodeAndValue
      (do
        execStmtInitialize (G.MkStmtInitialize)
        execStmtAlloc
          (G.MkStmtAlloc (G.LValueSymbol "foo" []))
        getRValueInReg (G.RLValue (G.LValueSymbol "foo" []))
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] (G.TypeUser "Foo")) 0]
        , gSymbolsSize = 1
        , userTypes    = [UserType {utName = "Foo", utFields = [], utSize = 0}]
        }
  machine <- runSimulator code []
  read @Int (Simulator.getRegVal r machine) @?= (1024 + 16*8)
  read @Int (Simulator.getMemory (1024 + 16) machine) @?= 1
  read @Int (Simulator.getMemory (1024 + 15) machine) @?= 1
  read @Int (Simulator.getMemory (1024 + 17) machine) @?= 0

  step "Two alloc"
  let
    (code, r) = compileGetCodeAndValue
      (do
        execStmtInitialize (G.MkStmtInitialize)
        execStmtAlloc
          (G.MkStmtAlloc (G.LValueSymbol "foo" []))
        execStmtAlloc
          (G.MkStmtAlloc (G.LValueSymbol "foo" []))
        getRValueInReg (G.RLValue (G.LValueSymbol "foo" []))
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] (G.TypeUser "Foo")) 0]
        , gSymbolsSize = 1
        , userTypes    = [UserType {utName = "Foo", utFields = [], utSize = 0}]
        }
  machine <- runSimulator code []
  read @Int (Simulator.getRegVal r machine) @?= (1024 + 16*8 + 8)
  read @Int (Simulator.getMemory (1024 + 16) machine) @?= 1
  read @Int (Simulator.getMemory (1024 + 15) machine) @?= 1
  read @Int (Simulator.getMemory (1024 + 17) machine) @?= 1
  read @Int (Simulator.getMemory (1024 + 18) machine) @?= 0
