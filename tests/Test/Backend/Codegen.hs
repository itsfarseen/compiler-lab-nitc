{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

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

span0 :: Span
span0 = Span 0 0

span0A :: Applicative a => a Span
span0A = pure span0



cRun2 :: Codegen a -> [Symbol] -> Int -> [Func] -> [XSMInstr]
cRun2 codegenM syms symsLen funcs = runCodegen
  (execSetupGlobalSymtab >> codegenM >> getCodeTranslated)
  (initCodegenStateInternal syms symsLen funcs [])

cRun3 :: Codegen t -> [Symbol] -> Int -> [Func] -> ([XSMInstr], t)
cRun3 codegenM syms symsLen funcs = runCodegen
  (execSetupGlobalSymtab >> codegenM >>= \a -> (, a) <$> getCodeTranslated)
  (initCodegenStateInternal syms symsLen funcs [])



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
    code =
      Codegen.runCodegen
          (do
            execSetupGlobalSymtab
            execCallMainFunc
            execFuncDefs
            getCodeTranslated
          )
        $ initCodegenStateInternal
            []
            0
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
            []

  let simulator = Simulator.run (Simulator.init code)
  Simulator.getStdout simulator @?= ["Hello World!"]
  step "Simple 2"
  let
    code =
      Codegen.runCodegen
          (do
            execSetupGlobalSymtab
            execCallMainFunc
            execFuncDefs
            getCodeTranslated
          )
        $ initCodegenStateInternal
            [ Symbol
                { symName   = "foo"
                , symType   = G.Type2 [] G.TypeInt
                , symRelLoc = 0
                }
            ]
            1
            [ Func
                { funcName          = "main"
                , funcRetType       = G.TypeInt
                , funcBody          =
                  [ G.StmtAssign $ G.MkStmtAssign
                    (G.LValue [] "foo")
                    (G.RExp $ G.ExpNum 121)
                  , G.StmtReturn $ G.MkStmtReturn (G.RExp $ G.ExpNum 0)
                  ]
                , funcSymbols       = []
                , funcLocalVarsSize = 0
                , funcLabel         = "main"
                }
            ]
            []
  let simulator = Simulator.run (Simulator.init code)

  Simulator.getMemory 4096 simulator @?= "121"

test_execStmtAssign :: TestTree
test_execStmtAssign = testCaseSteps "execStmtAssign" $ \step -> do
  step "Simple assign"
  let
    code = cRun2
      (do
        execStmtAssign
          (G.MkStmtAssign (G.LValue [] "foo") (G.RExp $ G.ExpNum 10))
        getCodeTranslated
      )
      [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
      1
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"
  step "Assign self"
  let
    code = cRun2
      (do
        execStmtAssign
          (G.MkStmtAssign (G.LValue [] "foo") (G.RExp $ G.ExpNum 10))
        execStmtAssign
          (G.MkStmtAssign
            (G.LValue [] "foo")
            (G.RLValue $ G.LValue [] "foo")
          )
      )
      [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
      1
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"

test_execStmtRead :: TestTree
test_execStmtRead = testCaseSteps "execStmtRead" $ \step -> do
  step "Single read"
  let
    code = cRun2
      (execStmtRead $ G.MkStmtRead $ G.LValue [] "foo")
      [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
      1
      []
  let simulator = Simulator.run (Simulator.initWithStdin ["10"] code)
  Simulator.getMemory 4096 simulator @?= "10"
  step "Double read"
  let
    code = cRun2
      (do
        execStmtRead $ G.MkStmtRead $ G.LValue [] "foo"
        execStmtRead $ G.MkStmtRead $ G.LValue [] "foo"
      )
      [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
      1
      []
  let
    simulator = Simulator.run $ Simulator.initWithStdin ["10", "555"] code
  Simulator.getMemory 4096 simulator @?= "555"

test_execStmtWrite :: TestTree
test_execStmtWrite = testCaseSteps "execStmtWrite" $ \step -> do
  step "Simple write"
  let
    code = cRun2
      (do
        execStmtWrite (G.MkStmtWrite (G.RExp $ G.ExpNum 100))
        execStmtWrite (G.MkStmtWrite (G.RExp $ G.ExpStr "ASD"))
      )
      []
      0
      []
  let
    simulator = Simulator.run (Simulator.initWithStdin ["10", "555"] code)
  Simulator.getStdout simulator @?= ["100", "ASD"]

test_execStmtIf :: TestTree
test_execStmtIf = testCaseSteps "execStmtIf" $ \step -> do
  step "If stmt true"
  let
    code = cRun2
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
      []
      0
      []
  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["100", "200", "300", "555", "666"]
  step "If stmt false"
  let
    code = cRun2
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
      []
      0
      []
  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["555", "666"]

test_execStmtIfElse :: TestTree
test_execStmtIfElse = testCaseSteps "execStmtIfElse" $ \step -> do
  step "cond true"
  let
    code = cRun2
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
      []
      0
      []
  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["100", "200", "300", "555", "666"]
  step "cond false"
  let
    code = cRun2
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
      []
      0
      []
  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["500", "600", "700", "555", "666"]

test_execStmtWhile :: TestTree
test_execStmtWhile = testCaseSteps "execStmtWhile" $ \step -> do
  step "While stmt true"
  let
    code = cRun2
      (do
        execStmtAssign
          $ G.MkStmtAssign (G.LValue [] "foo") (G.RExp $ G.ExpNum 0)
        execStmtWhile $ G.MkStmtWhile
          (G.RExp $ G.MkExpRelational
            (G.RLValue $ G.LValue [] "foo")
            G.OpLT
            (G.RExp $ G.ExpNum 10)
          )
          [ G.StmtAssign $ G.MkStmtAssign
            (G.LValue [] "foo")
            (G.RExp $ G.MkExpArithmetic
              (G.RLValue $ G.LValue [] "foo")
              G.OpAdd
              (G.RExp $ G.ExpNum 1)
            )
          , G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "loop")
          ]
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "foo")
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "bar")
      )
      [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
      0
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"
  Simulator.getStdout simulator
    @?= (take 10 $ repeat "loop")
    ++  ["foo", "bar"]
  step "While stmt false"
  let
    code = cRun2
      (do
        execStmtAssign
          $ G.MkStmtAssign (G.LValue [] "foo") (G.RExp $ G.ExpNum 15)
        execStmtWhile $ G.MkStmtWhile
          (G.RExp $ G.MkExpRelational
            (G.RLValue $ G.LValue [] "foo")
            G.OpLT
            (G.RExp $ G.ExpNum 10)
          )
          [ G.StmtAssign $ G.MkStmtAssign
            (G.LValue [] "foo")
            (G.RExp $ G.MkExpArithmetic
              (G.RLValue $ G.LValue [] "foo")
              G.OpAdd
              (G.RExp $ G.ExpNum 1)
            )
          , G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "loop")
          ]
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "foo")
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "bar")
      )
      [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
      0
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "15"
  Simulator.getStdout simulator @?= ["foo", "bar"]

test_execStmtBreak :: TestTree
test_execStmtBreak = testCaseSteps "execStmtBreak" $ \_ -> do
  let
    code = cRun2
      (do
        execStmtAssign
          $ G.MkStmtAssign (G.LValue [] "foo") (G.RExp $ G.ExpNum 0)
        execStmtWhile $ G.MkStmtWhile
          (G.RExp $ G.MkExpRelational
            (G.RLValue $ G.LValue [] "foo")
            G.OpLT
            (G.RExp $ G.ExpNum 10)
          )
          [ G.StmtBreak $ G.MkStmtBreak
          , G.StmtAssign $ G.MkStmtAssign
            (G.LValue [] "foo")
            (G.RExp $ G.MkExpArithmetic
              (G.RLValue $ G.LValue [] "foo")
              G.OpAdd
              (G.RExp $ G.ExpNum 1)
            )
          , G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "loop")
          ]
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "foo")
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "bar")
      )
      [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
      0
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "0"
  Simulator.getStdout simulator @?= ["foo", "bar"]

test_execStmtContinue :: TestTree
test_execStmtContinue = testCaseSteps "execStmtContinue" $ \_ -> do
  let
    code = cRun2
      (do
        execStmtAssign
          $ G.MkStmtAssign (G.LValue [] "foo") (G.RExp $ G.ExpNum 0)
        execStmtWhile $ G.MkStmtWhile
          (G.RExp $ G.MkExpRelational
            (G.RLValue $ G.LValue [] "foo")
            G.OpLT
            (G.RExp $ G.ExpNum 10)
          )
          [ G.StmtAssign $ G.MkStmtAssign
            (G.LValue [] "foo")
            (G.RExp $ G.MkExpArithmetic
              (G.RLValue $ G.LValue [] "foo")
              G.OpAdd
              (G.RExp $ G.ExpNum 1)
            )
          , G.StmtContinue $ G.MkStmtContinue
          , G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "loop")
          ]
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "foo")
        execStmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "bar")
      )
      [Symbol "foo" (G.Type2 [] G.TypeInt) 0]
      0
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"
  Simulator.getStdout simulator @?= ["foo", "bar"]

test_execFunctionCall :: TestTree
test_execFunctionCall = testCaseSteps "execFunctionCall" $ \step -> do
  step "Without arguments - Write"
  let
    code = cRun2
      (do
        r <- execStmtRValue $ G.MkStmtRValue $ G.RFuncCall "foo" []
        appendCode [XSM_INT 10]
        execFuncDefs
        return r
      )
      []
      0
      [ Func
          { funcName          = "foo"
          , funcRetType       = G.TypeInt
          , funcSymbols       = []
          , funcBody          =
            [G.StmtWrite $ G.MkStmtWrite (G.RExp $ G.ExpStr "Hello World")]
          , funcLocalVarsSize = 0
          , funcLabel         = "F1"
          }
      ]

  -- mapM print code
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getStdout simulator @?= ["Hello World"]

  step "Without arguments - Return"
  let
    (code, r) = cRun3
      (do
        r <- getRValueInReg (G.RFuncCall "foo" [])
        appendCode [XSM_INT 10]
        execFuncDefs
        return r
      )
      []
      0
      [ Func
          { funcName          = "foo"
          , funcRetType       = G.TypeInt
          , funcSymbols       = []
          , funcBody          =
            [G.StmtReturn $ G.MkStmtReturn $ G.RExp $ G.ExpNum (123)]
          , funcLocalVarsSize = 0
          , funcLabel         = "F1"
          }
      ]

  let simulator = Simulator.run (Simulator.init code)
  let rVal      = read @Int $ Simulator.getRegVal r simulator
  rVal @?= 123

  step "With arguments"
  let
    (code, r) = cRun3
      (do
        r <- getRValueInReg
          (G.RFuncCall "foo" [G.RExp $ G.ExpNum 123, G.RExp $ G.ExpNum 444]
          )
        appendCode [XSM_INT 10]
        execFuncDefs
        return r
      )
      []
      0
      [ Func
          { funcName          = "foo"
          , funcRetType       = G.TypeInt
          , funcSymbols       =
            [ Symbol "a" (G.Type2 [] G.TypeInt) (-4)
            , Symbol "b" (G.Type2 [] G.TypeInt) (-3)
            ]
          , funcBody          =
            [ G.StmtReturn $ G.MkStmtReturn $ G.RExp $ G.MkExpArithmetic
                (G.RLValue $ G.LValue [] "a")
                G.OpSub
                (G.RLValue $ G.LValue [] "b")
            ]
          , funcLocalVarsSize = 0
          , funcLabel         = "F1"
          }
      ]
  let simulator = Simulator.run (Simulator.init code)
  let rVal      = read @Int $ Simulator.getRegVal r simulator
  rVal @?= 123 - 444

----------------------------------------------------------------------------------------

test_getLValueLocInReg :: TestTree
test_getLValueLocInReg = testCaseSteps "getLValueLocInReg" $ \step -> do
  step "Simple"
  let
    (code, (r1, r2)) = cRun3
      (do
        r1 <- getLValueLocInReg (G.LValue [] "foo")
        r2 <- getLValueLocInReg (G.LValue [] "bar")
        return (r1, r2)
      )
      [ Symbol "foo" (G.Type2 [] G.TypeInt) 0
      , Symbol "bar" (G.Type2 [] G.TypeInt) 1
      ]
      2
      []

  let simulator = Simulator.run (Simulator.init code)
  let loc1      = read @Int $ Simulator.getRegVal r1 simulator
  let loc2      = read @Int $ Simulator.getRegVal r2 simulator
  loc1 @?= 4096
  loc2 @?= 4097

  step "2D Array index"
  let
    (code, (basereg, reg)) = cRun3
      (do
        r1 <- getLValueLocInReg (G.LValue [] "bar")
        r2 <- getLValueLocInReg
          (G.LValue (G.RExp . G.ExpNum <$> [2, 3]) "bar")
        return (r1, r2)
      )
      [ Symbol "foo" (G.Type2 [3, 2] G.TypeInt) 0
      , Symbol "bar" (G.Type2 [4, 5] G.TypeInt) 6
      ]
      26
      []

  let simulator = Simulator.run (Simulator.init code)
  let loc       = read @Int $ (Simulator.getRegVal reg simulator)
  let base      = read @Int $ (Simulator.getRegVal basereg simulator)
  base @?= 4096 + 6
  loc @?= base + 2 * 5 + 3


  step "Function Args"
  let
    (code, (r1, r2)) = cRun3
      (do
        appendCode [XSM_MOV_Int BP 4900]
        modify
          (\s -> s
            { lSymbols =
              Just
                $ [ Symbol "foo" (G.Type2 [] G.TypeInt) (-3)
                  , Symbol "bar" (G.Type2 [] G.TypeInt) (-2)
                  ]
            }
          )
        r1 <- getLValueLocInReg (G.LValue [] "foo")
        r2 <- getLValueLocInReg (G.LValue [] "bar")
        return (r1, r2)
      )
      []
      0
      []

  let simulator = Simulator.run (Simulator.init code)
  let r1Loc     = read @Int $ (Simulator.getRegVal r1 simulator)
  let r2Loc     = read @Int $ (Simulator.getRegVal r2 simulator)
  r1Loc @?= 4900 - 3
  r2Loc @?= 4900 - 2

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
    code = cRun2
      (execStmtRValue $ G.MkStmtRValue $ G.RSyscall
        7
        5
        (G.RExp $ G.ExpNum (-2))
        (G.RExp $ G.ExpStr "Hello")
        (G.RExp $ G.ExpNum 0)
      )
      []
      0
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getStdout simulator @?= ["Hello"]

