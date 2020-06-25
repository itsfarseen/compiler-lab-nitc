{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}



module Test.Backend.Codegen where

import Backend.Codegen as Codegen
import Backend.Instructions
import Backend.Reg
import Control.Monad.State.Strict
import Span
import Test.Tasty
import Test.Tasty.HUnit
import qualified Backend.Simulator as Simulator
import qualified Grammar as G
import Test.LibraryUtils (loadLibrary)
import Safe

span0 :: Span
span0 = Span 0 0

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
    code <- getCodeTranslated
    return (code, ret)
  )
  codegenState

runSimulator :: [XSMInstr] -> [String] -> IO Simulator.Machine
runSimulator code stdin =
  Simulator.run . Simulator.initWithStdin stdin code <$> loadLibrary

tests :: TestTree
tests = testGroup
  "Codegen"
  [ test_simpleMain
  , test_execStmtAssign
  , test_execStmtRead
  , test_execStmtWrite
  , test_execStmtIf
  , test_execStmtIfElse
  , test_execStmtWhile
  , test_execStmtBreak
  , test_execStmtContinue
  , test_execFunctionCall
  , test_getLValueLocInReg
  , test_buildFuncArgsTable
  , test_Syscall
  ]

test_simpleMain :: TestTree
test_simpleMain = testCaseSteps "Simple main function" $ \step -> do
  step "Simple 1"
  let
    code = compileGetCode
      execCallMainFunc
      codegenStateDefault
        { funcs =
          [ Func
              { funcName          = "main"
              , funcRetType       = G.TypeInt
              , funcBody          =
                [ G.StmtWrite
                  $ G.MkStmtWrite (G.RExp $ G.ExpStr "Hello World!")
                , G.StmtReturn $ G.MkStmtReturn (G.RExp $ G.ExpNum 0)
                ]
              , funcSymbols       = []
              , funcLocalVarsSize = 0
              , funcLabel         = "main"
              }
          ]
        }

  machine <- runSimulator code []
  Simulator.getStdout machine @?= ["Hello World!"]
  step "Simple 2"
  let
    code = compileGetCode
      execCallMainFunc
      codegenStateDefault
        { gSymbols     =
          [ Symbol
              { symName   = "foo"
              , symType   = G.Type2 [] G.TypeInt
              , symRelLoc = 0
              }
          ]
        , gSymbolsSize = 1
        , funcs        =
          [ Func
              { funcName          = "main"
              , funcRetType       = G.TypeInt
              , funcBody          =
                [ G.StmtAssign $ G.MkStmtAssign
                  (G.LValueSymbol "foo" [])
                  (G.RExp $ G.ExpNum 121)
                , G.StmtReturn $ G.MkStmtReturn (G.RExp $ G.ExpNum 0)
                ]
              , funcSymbols       = []
              , funcLocalVarsSize = 0
              , funcLabel         = "main"
              }
          ]
        }
  machine <- runSimulator code []
  Simulator.getMemory 4096 machine @?= "121"

test_execStmtAssign :: TestTree
test_execStmtAssign = testCaseSteps "execStmtAssign" $ \step -> do
  step "Simple assign"
  let
    code = compileGetCode
      (execStmtAssign
        (G.MkStmtAssign (G.LValueSymbol "foo" []) (G.RExp $ G.ExpNum 10))
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
        , gSymbolsSize = 1
        }
  machine <- runSimulator code []
  Simulator.getMemory 4096 machine @?= "10"

  step "Assign self"
  let
    code = compileGetCode
      (do
        execStmtAssign
          (G.MkStmtAssign (G.LValueSymbol "foo" []) (G.RExp $ G.ExpNum 10))
        execStmtAssign
          (G.MkStmtAssign
            (G.LValueSymbol "foo" [])
            (G.RLValue $ G.LValueSymbol "foo" [])
          )
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
        , gSymbolsSize = 1
        }
  machine <- runSimulator code []
  Simulator.getMemory 4096 machine @?= "10"

test_execStmtRead :: TestTree
test_execStmtRead = testCaseSteps "execStmtRead" $ \step -> do
  step "Single read"
  let
    code = compileGetCode
      (execStmtRead $ G.MkStmtRead $ G.LValueSymbol "foo" [])
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
        , gSymbolsSize = 1
        }
  machine <- runSimulator code ["10"]
  Simulator.getMemory 4096 machine @?= "10"

  step "Double read"
  let
    code = compileGetCode
      (do
        execStmtRead $ G.MkStmtRead $ G.LValueSymbol "foo" []
        execStmtRead $ G.MkStmtRead $ G.LValueSymbol "foo" []
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
        , gSymbolsSize = 1
        }
  machine <- runSimulator code ["10", "555"]
  Simulator.getMemory 4096 machine @?= "555"

test_execStmtWrite :: TestTree
test_execStmtWrite = testCaseSteps "execStmtWrite" $ \step -> do
  step "Simple write"
  let
    code = compileGetCode
      (do
        execStmtWrite (G.MkStmtWrite (G.RExp $ G.ExpNum 100))
        execStmtWrite (G.MkStmtWrite (G.RExp $ G.ExpStr "ASD"))
      )
      codegenStateDefault
  machine <- runSimulator code ["10", "555"]
  Simulator.getStdout machine @?= ["100", "ASD"]

test_execStmtIf :: TestTree
test_execStmtIf = testCaseSteps "execStmtIf" $ \step -> do
  step "If stmt true"
  let
    code = compileGetCode
      (do
        execStmtIf $ G.MkStmtIf
          (G.RExp $ G.MkExpRelational
            (G.RExp $ G.ExpNum 1)
            G.OpEQ
            (G.RExp $ G.ExpNum 1)
          )
          (   G.StmtWrite
          <$> [ G.MkStmtWrite (G.RExp $ G.ExpNum 100)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 200)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 300)
              ]
          )
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpNum 555)
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpNum 666)
      )
      codegenStateDefault

  machine <- runSimulator code []
  Simulator.getStdout machine @?= ["100", "200", "300", "555", "666"]
  step "If stmt false"
  let
    code = compileGetCode
      (do
        execStmtIf $ G.MkStmtIf
          (G.RExp $ G.MkExpRelational
            (G.RExp $ G.ExpNum 1)
            G.OpEQ
            (G.RExp $ G.ExpNum 2)
          )
          (   G.StmtWrite
          <$> [ G.MkStmtWrite (G.RExp $ G.ExpNum 100)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 200)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 300)
              ]
          )
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpNum 555)
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpNum 666)
      )
      codegenStateDefault

  machine <- runSimulator code []
  Simulator.getStdout machine @?= ["555", "666"]

test_execStmtIfElse :: TestTree
test_execStmtIfElse = testCaseSteps "execStmtIfElse" $ \step -> do
  step "cond true"
  let
    code = compileGetCode
      (do
        execStmtIfElse $ G.MkStmtIfElse
          (G.RExp $ G.MkExpRelational
            (G.RExp $ G.ExpNum 1)
            G.OpEQ
            (G.RExp $ G.ExpNum 1)
          )
          (   G.StmtWrite
          <$> [ G.MkStmtWrite (G.RExp $ G.ExpNum 100)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 200)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 300)
              ]
          )
          (   G.StmtWrite
          <$> [ G.MkStmtWrite (G.RExp $ G.ExpNum 500)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 600)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 700)
              ]
          )
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpNum 555)
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpNum 666)
      )
      codegenStateDefault
  machine <- runSimulator code []
  Simulator.getStdout machine @?= ["100", "200", "300", "555", "666"]
  step "cond false"
  let
    code = compileGetCode
      (do
        execStmtIfElse $ G.MkStmtIfElse
          (G.RExp $ G.MkExpRelational
            (G.RExp $ G.ExpNum 1)
            G.OpNE
            (G.RExp $ G.ExpNum 1)
          )
          (   G.StmtWrite
          <$> [ G.MkStmtWrite (G.RExp $ G.ExpNum 100)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 200)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 300)
              ]
          )
          (   G.StmtWrite
          <$> [ G.MkStmtWrite (G.RExp $ G.ExpNum 500)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 600)
              , G.MkStmtWrite (G.RExp $ G.ExpNum 700)
              ]
          )
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpNum 555)
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpNum 666)
      )
      codegenStateDefault
  machine <- runSimulator code []
  Simulator.getStdout machine @?= ["500", "600", "700", "555", "666"]

test_execStmtWhile :: TestTree
test_execStmtWhile = testCaseSteps "execStmtWhile" $ \step -> do
  step "While stmt true"
  let
    code = compileGetCode
      (do
        execStmtAssign $ G.MkStmtAssign
          (G.LValueSymbol "foo" [])
          (G.RExp $ G.ExpNum 0)
        execStmtWhile $ G.MkStmtWhile
          (G.RExp $ G.MkExpRelational
            (G.RLValue $ G.LValueSymbol "foo" [])
            G.OpLT
            (G.RExp $ G.ExpNum 10)
          )
          [ G.StmtAssign $ G.MkStmtAssign
            (G.LValueSymbol "foo" [])
            (G.RExp $ G.MkExpArithmetic
              (G.RLValue $ G.LValueSymbol "foo" [])
              G.OpAdd
              (G.RExp $ G.ExpNum 1)
            )
          , G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "loop")
          ]
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "foo")
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "bar")
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
        , gSymbolsSize = 1
        }
  machine <- runSimulator code []
  Simulator.getMemory 4096 machine @?= "10"
  Simulator.getStdout machine @?= replicate 10 "loop" ++ ["foo", "bar"]
  step "While stmt false"
  let
    code = compileGetCode
      (do
        execStmtAssign $ G.MkStmtAssign
          (G.LValueSymbol "foo" [])
          (G.RExp $ G.ExpNum 15)
        execStmtWhile $ G.MkStmtWhile
          (G.RExp $ G.MkExpRelational
            (G.RLValue $ G.LValueSymbol "foo" [])
            G.OpLT
            (G.RExp $ G.ExpNum 10)
          )
          [ G.StmtAssign $ G.MkStmtAssign
            (G.LValueSymbol "foo" [])
            (G.RExp $ G.MkExpArithmetic
              (G.RLValue $ G.LValueSymbol "foo" [])
              G.OpAdd
              (G.RExp $ G.ExpNum 1)
            )
          , G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "loop")
          ]
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "foo")
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "bar")
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
        , gSymbolsSize = 0
        }
  machine <- runSimulator code []
  Simulator.getMemory 4096 machine @?= "15"
  Simulator.getStdout machine @?= ["foo", "bar"]

test_execStmtBreak :: TestTree
test_execStmtBreak = testCaseSteps "execStmtBreak" $ \_ -> do
  let
    code = compileGetCode
      (do
        execStmtAssign $ G.MkStmtAssign
          (G.LValueSymbol "foo" [])
          (G.RExp $ G.ExpNum 0)
        execStmtWhile $ G.MkStmtWhile
          (G.RExp $ G.MkExpRelational
            (G.RLValue $ G.LValueSymbol "foo" [])
            G.OpLT
            (G.RExp $ G.ExpNum 10)
          )
          [ G.StmtBreak G.MkStmtBreak
          , G.StmtAssign $ G.MkStmtAssign
            (G.LValueSymbol "foo" [])
            (G.RExp $ G.MkExpArithmetic
              (G.RLValue $ G.LValueSymbol "foo" [])
              G.OpAdd
              (G.RExp $ G.ExpNum 1)
            )
          , G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "loop")
          ]
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "foo")
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "bar")
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
        , gSymbolsSize = 0
        }
  machine <- runSimulator code []
  Simulator.getMemory 4096 machine @?= "0"
  Simulator.getStdout machine @?= ["foo", "bar"]

test_execStmtContinue :: TestTree
test_execStmtContinue = testCaseSteps "execStmtContinue" $ \_ -> do
  let
    code = compileGetCode
      (do
        execStmtAssign $ G.MkStmtAssign
          (G.LValueSymbol "foo" [])
          (G.RExp $ G.ExpNum 0)
        execStmtWhile $ G.MkStmtWhile
          (G.RExp $ G.MkExpRelational
            (G.RLValue $ G.LValueSymbol "foo" [])
            G.OpLT
            (G.RExp $ G.ExpNum 10)
          )
          [ G.StmtAssign $ G.MkStmtAssign
            (G.LValueSymbol "foo" [])
            (G.RExp $ G.MkExpArithmetic
              (G.RLValue $ G.LValueSymbol "foo" [])
              G.OpAdd
              (G.RExp $ G.ExpNum 1)
            )
          , G.StmtContinue G.MkStmtContinue
          , G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "loop")
          ]
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "foo")
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "bar")
      )
      codegenStateDefault
        { gSymbols     = [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
        , gSymbolsSize = 0
        }
  machine <- runSimulator code []
  Simulator.getMemory 4096 machine @?= "10"
  Simulator.getStdout machine @?= ["foo", "bar"]

test_execFunctionCall :: TestTree
test_execFunctionCall = testCaseSteps "execFunctionCall" $ \step -> do
  step "Without arguments - Write"
  let
    code = compileGetCode
      (execStmtRValue $ G.MkStmtRValue $ G.RFuncCall "foo" [])
      codegenStateDefault
        { funcs =
          [ Func
              { funcName          = "foo"
              , funcRetType       = G.TypeInt
              , funcSymbols       = []
              , funcBody          =
                [ G.StmtWrite
                    $ G.MkStmtWrite (G.RExp $ G.ExpStr "Hello World")
                ]
              , funcLocalVarsSize = 0
              , funcLabel         = "F1"
              }
          ]
        }

  -- mapM print code
  machine <- runSimulator code []
  Simulator.getStdout machine @?= ["Hello World"]

  step "Without arguments - Return"
  let
    (code, r) = compileGetCodeAndValue
      (getRValueInReg (G.RFuncCall "foo" []))
      codegenStateDefault
        { funcs =
          [ Func
              { funcName          = "foo"
              , funcRetType       = G.TypeInt
              , funcSymbols       = []
              , funcBody          =
                [G.StmtReturn $ G.MkStmtReturn $ G.RExp $ G.ExpNum 123]
              , funcLocalVarsSize = 0
              , funcLabel         = "F1"
              }
          ]
        }

  machine <- runSimulator code []
  let rVal = read @Int $ Simulator.getRegVal r machine
  rVal @?= 123

  step "With arguments"
  let
    (code, r) = compileGetCodeAndValue
      (getRValueInReg
        (G.RFuncCall "foo" [G.RExp $ G.ExpNum 123, G.RExp $ G.ExpNum 444])
      )
      codegenStateDefault
        { funcs =
          [ Func
              { funcName          = "foo"
              , funcRetType       = G.TypeInt
              , funcSymbols       =
                [ Symbol "a" (G.Type2 [] G.TypeInt) (-4)
                , Symbol "b" (G.Type2 [] G.TypeInt) (-3)
                ]
              , funcBody          =
                [ G.StmtReturn
                  $ G.MkStmtReturn
                  $ G.RExp
                  $ G.MkExpArithmetic
                      (G.RLValue $ G.LValueSymbol "a" [])
                      G.OpSub
                      (G.RLValue $ G.LValueSymbol "b" [])
                ]
              , funcLocalVarsSize = 0
              , funcLabel         = "F1"
              }
          ]
        }
  machine <- runSimulator code []
  let rVal = read @Int $ Simulator.getRegVal r machine
  rVal @?= 123 - 444

----------------------------------------------------------------------------------------

test_getLValueLocInReg :: TestTree
test_getLValueLocInReg = testCaseSteps "getLValueLocInReg" $ \step -> do
  step "Simple"
  let
    (code, (r1, r2)) = compileGetCodeAndValue
      (do
        r1 <- getLValueLocInReg (G.LValueSymbol "foo" [])
        r2 <- getLValueLocInReg (G.LValueSymbol "bar" [])
        return (r1, r2)
      )
      codegenStateDefault
        { gSymbols     =
          [ Symbol "foo" (G.Type2 [] G.TypeInt) 0
          , Symbol "bar" (G.Type2 [] G.TypeInt) 1
          ]
        , gSymbolsSize = 2
        }

  machine <- runSimulator code []
  let loc1 = read @Int $ Simulator.getRegVal r1 machine
  let loc2 = read @Int $ Simulator.getRegVal r2 machine
  loc1 @?= 4096
  loc2 @?= 4097

  step "2D Array index"
  let
    (code, (basereg, reg)) = compileGetCodeAndValue
      (do
        r1 <- getLValueLocInReg (G.LValueSymbol "bar" [])
        r2 <- getLValueLocInReg
          (G.LValueSymbol "bar" (G.RExp . G.ExpNum <$> [2, 3]))
        return (r1, r2)
      )
      codegenStateDefault
        { gSymbols     =
          [ Symbol "foo" (G.Type2 [3, 2] G.TypeInt) 0
          , Symbol "bar" (G.Type2 [4, 5] G.TypeInt) 6
          ]
        , gSymbolsSize = 26
        }

  machine <- runSimulator code []
  let loc  = read @Int (Simulator.getRegVal reg machine)
  let base = read @Int (Simulator.getRegVal basereg machine)
  base @?= 4096 + 6
  loc @?= base + 2 * 5 + 3


  step "Function Args"
  let
    (code, (r1, r2)) = compileGetCodeAndValue
      (do
        appendCode [XSM_MOV_Int BP 4900]
        modify
          (\s -> s
            { lSymbols = Just
              [ Symbol "foo" (G.Type2 [] G.TypeInt) (-3)
              , Symbol "bar" (G.Type2 [] G.TypeInt) (-2)
              ]
            }
          )
        r1 <- getLValueLocInReg (G.LValueSymbol "foo" [])
        r2 <- getLValueLocInReg (G.LValueSymbol "bar" [])
        return (r1, r2)
      )
      codegenStateDefault

  machine <- runSimulator code []
  let r1Loc = read @Int (Simulator.getRegVal r1 machine)
  let r2Loc = read @Int (Simulator.getRegVal r2 machine)
  r1Loc @?= 4900 - 3
  r2Loc @?= 4900 - 2

  step "User Type fields"
  let
    (code, (reg_foo_val, reg_foo_bar_loc, reg_foo_baz_loc)) =
      compileGetCodeAndValue
        (do
          execStmtInitialize (G.MkStmtInitialize)
          let foo     = G.LValueSymbol "foo" []
          let foo_bar = G.LValueField foo "bar" []
          let foo_baz = G.LValueField foo "baz" []
          execStmtAlloc (G.MkStmtAlloc foo)
          foo_val     <- getRValueInReg (G.RLValue foo)
          foo_bar_loc <- getLValueLocInReg foo_bar
          foo_baz_loc <- getLValueLocInReg foo_baz
          return (foo_val, foo_bar_loc, foo_baz_loc)
        )
        codegenStateDefault
          { userTypes    =
            [ UserType
                { utName   = "Foo"
                , utSize   = 2
                , utFields =
                  [ Symbol
                    { symName   = "bar"
                    , symType   = G.Type2 [] (G.TypeUser "Foo")
                    , symRelLoc = 0
                    }
                  , Symbol
                    { symName   = "baz"
                    , symType   = G.Type2 [] G.TypeInt
                    , symRelLoc = 1
                    }
                  ]
                }
            ]
          , gSymbols     =
            [ Symbol
                { symName   = "foo"
                , symType   = G.Type2 [] (G.TypeUser "Foo")
                , symRelLoc = 0
                }
            ]
          , gSymbolsSize = 1
          }

  machine <- runSimulator code []
  let foo_val = readNote @Int "foo_val" $ Simulator.getRegVal reg_foo_val machine
  let
    foo_bar_loc = readNote @Int "foo_bar_loc" $ Simulator.getRegVal reg_foo_bar_loc machine
  let
    foo_baz_loc = readNote @Int "foo_baz_loc" $ Simulator.getRegVal reg_foo_baz_loc machine
  foo_val @?= 1024 + 16*8
  foo_bar_loc @?= foo_val
  foo_baz_loc @?= foo_val + 1

test_buildFuncArgsTable :: TestTree
test_buildFuncArgsTable = testCaseSteps "buildFuncArgsTable" $ \_steps ->
  do
    let
      symbols =
        [ G.Symbol
          { G.symName     = "foo"
          , G.symType     = G.Type2 [] G.TypeInt
          , G.symDeclSpan = undefined
          }
        , G.Symbol
          { G.symName     = "bar"
          , G.symType     = G.Type2 [] G.TypeInt
          , G.symDeclSpan = undefined
          }
        ]
    let [s1, s2] = buildFuncArgsTable symbols (-3)
    s1 @?= Symbol
      { symName   = "foo"
      , symType   = G.Type2 [] G.TypeInt
      , symRelLoc = -4
      }
    s2 @?= Symbol
      { symName   = "bar"
      , symType   = G.Type2 [] G.TypeInt
      , symRelLoc = -3
      }


test_Syscall :: TestTree
test_Syscall = testCaseSteps "Syscall" $ \step -> do
  step "Syscall Write"
  let
    code = compileGetCode
      (execStmtRValue $ G.MkStmtRValue $ G.RSyscall
        7
        5
        (G.RExp $ G.ExpNum (-2))
        (G.RExp $ G.ExpStr "Hello")
        (G.RExp $ G.ExpNum 0)
      )
      codegenStateDefault
  machine <- runSimulator code []
  Simulator.getStdout machine @?= ["Hello"]

