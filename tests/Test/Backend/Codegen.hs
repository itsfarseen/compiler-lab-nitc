{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Test.Backend.Codegen where

import Backend.Codegen as Codegen
import Backend.Instructions
import Backend.Instructions (XSMInstr)
import Backend.Reg
import Control.Monad.State.Strict
import Grammar hiding (Func(..), FuncDecl(..), FuncDef(..), Symbol(..))
import Span
import Test.Tasty
import Test.Tasty.HUnit
import qualified Backend.Simulator as Simulator
import qualified Grammar
import qualified Grammar as G

span0 :: Span
span0 = Span 0 0

span0A :: Applicative a => a Span
span0A = pure span0

fromRight'' :: Show a => Either a p -> p
fromRight'' x = case x of
  Left  l -> error $ show l
  Right r -> r


cRun2 :: Codegen a -> [Symbol] -> Int -> [Func] -> [XSMInstr]
cRun2 codegenM syms symsLen funcs = fromRight'' $ runCodegen
  (execSetupGlobalSymtab >> codegenM >> getCodeTranslated)
  (initCodegenStateInternal syms symsLen funcs)

cRun3 :: Codegen t -> [Symbol] -> Int -> [Func] -> ([XSMInstr], t)
cRun3 codegenM syms symsLen funcs = fromRight'' $ runCodegen
  (execSetupGlobalSymtab >> codegenM >>= \a -> (, a) <$> getCodeTranslated)
  (initCodegenStateInternal syms symsLen funcs)

execStmtsGetCode :: Codegen [XSMInstr]
execStmtsGetCode = do
  execSetupGlobalSymtab
  execCallMainFunc
  execFuncDefs
  getCodeTranslated

-- Update list of tests - Run the following line as vim macro - 0w"qy$@q -
-- mq:let @b="" | g/\<test_\i\+\> ::/exe 'norm "ByE'|let @B=", "'q4j$di["bphxx
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
  ]

test_simpleMain :: TestTree
test_simpleMain = testCaseSteps "Simple main function" $ \step -> do
  step "Simple 1"
  let
    code =
      fromRight''
        $ Codegen.runCodegen do
            execSetupGlobalSymtab
            execCallMainFunc
            execFuncDefs
            getCodeTranslated
        $ initCodegenState
            []
            [ Grammar.FuncDefined
                (Grammar.FuncDecl
                  { Grammar.funcName     = "main"
                  , Grammar.funcRetType  = TypeInt
                  , Grammar.funcArgTypes = []
                  , Grammar.funcDeclSpan = span0
                  }
                )
                (Grammar.FuncDef
                  { Grammar.funcArgsLen = 0
                  , Grammar.funcSyms    = []
                  , Grammar.funcBody    =
                    [ StmtWrite
                      $ MkStmtWrite (RExp $ ExpStr "Hello World!")
                    , StmtReturn $ MkStmtReturn (RExp $ ExpNum 0)
                    ]
                  , Grammar.funcDefSpan = span0
                  }
                )
            ]
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getStdout simulator @?= ["Hello World!"]
  step "Simple 2"
  let
    code =
      fromRight''
        $ Codegen.runCodegen do
            execSetupGlobalSymtab
            execCallMainFunc
            execFuncDefs
            getCodeTranslated
        $ initCodegenState
            [ Grammar.Symbol
                { Grammar.symName     = "foo"
                , Grammar.symDataType = DataType [] TypeInt
                , Grammar.symDeclSpan = span0
                }
            ]
            [ Grammar.FuncDefined
                (Grammar.FuncDecl
                  { Grammar.funcName     = "main"
                  , Grammar.funcRetType  = TypeInt
                  , Grammar.funcArgTypes = []
                  , Grammar.funcDeclSpan = span0
                  }
                )
                (Grammar.FuncDef
                  { Grammar.funcArgsLen = 0
                  , Grammar.funcSyms    = []
                  , Grammar.funcBody    =
                    [ StmtAssign
                      $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 121)
                    , StmtReturn $ MkStmtReturn (RExp $ ExpNum 0)
                    ]
                  , Grammar.funcDefSpan = span0
                  }
                )
            ]
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "121"

test_execStmtAssign :: TestTree
test_execStmtAssign = testCaseSteps "execStmtAssign" $ \step -> do
  step "Simple assign"
  let
    code = cRun2
      do
        execStmtAssign (MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 10))
        getCodeTranslated
      [Symbol "foo" (DataType [] TypeInt) 0]
      1
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"
  step "Assign self"
  let
    code = cRun2
      do
        execStmtAssign (MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 10))
        execStmtAssign
          (MkStmtAssign (LValue [] "foo") (RLValue $ LValue [] "foo"))
      [Symbol "foo" (DataType [] TypeInt) 0]
      1
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"

test_execStmtRead :: TestTree
test_execStmtRead = testCaseSteps "execStmtRead" $ \step -> do
  step "Single read"
  let
    code = cRun2
      (execStmtRead $ MkStmtRead $ LValue [] "foo")
      [Symbol "foo" (DataType [] TypeInt) 0]
      1
      []
  let simulator = Simulator.run (Simulator.initWithStdin ["10"] code)
  Simulator.getMemory 4096 simulator @?= "10"
  step "Double read"
  let
    code = cRun2
      do
        execStmtRead $ MkStmtRead $ LValue [] "foo"
        execStmtRead $ MkStmtRead $ LValue [] "foo"
      [Symbol "foo" (DataType [] TypeInt) 0]
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
      do
        execStmtWrite (MkStmtWrite (RExp $ ExpNum 100))
        execStmtWrite (MkStmtWrite (RExp $ ExpStr "ASD"))
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
      do
        execStmtIf $ MkStmtIf
          (RExp $ MkExpRelational (RExp $ ExpNum 1) OpEQ (RExp $ ExpNum 1))
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 100)
              , MkStmtWrite (RExp $ ExpNum 200)
              , MkStmtWrite (RExp $ ExpNum 300)
              ]
          )
        execStmtWrite $ MkStmtWrite (RExp $ ExpNum 555)
        execStmtWrite $ MkStmtWrite (RExp $ ExpNum 666)
      []
      0
      []
  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["100", "200", "300", "555", "666"]
  step "If stmt false"
  let
    code = cRun2
      do
        execStmtIf $ MkStmtIf
          (RExp $ MkExpRelational (RExp $ ExpNum 1) OpEQ (RExp $ ExpNum 2))
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 100)
              , MkStmtWrite (RExp $ ExpNum 200)
              , MkStmtWrite (RExp $ ExpNum 300)
              ]
          )
        execStmtWrite $ MkStmtWrite (RExp $ ExpNum 555)
        execStmtWrite $ MkStmtWrite (RExp $ ExpNum 666)
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
      do
        execStmtIfElse $ MkStmtIfElse
          (RExp $ MkExpRelational (RExp $ ExpNum 1) OpEQ (RExp $ ExpNum 1))
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 100)
              , MkStmtWrite (RExp $ ExpNum 200)
              , MkStmtWrite (RExp $ ExpNum 300)
              ]
          )
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 500)
              , MkStmtWrite (RExp $ ExpNum 600)
              , MkStmtWrite (RExp $ ExpNum 700)
              ]
          )
        execStmtWrite $ MkStmtWrite (RExp $ ExpNum 555)
        execStmtWrite $ MkStmtWrite (RExp $ ExpNum 666)
      []
      0
      []
  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["100", "200", "300", "555", "666"]
  step "cond false"
  let
    code = cRun2
      do
        execStmtIfElse $ MkStmtIfElse
          (RExp $ MkExpRelational (RExp $ ExpNum 1) OpNE (RExp $ ExpNum 1))
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 100)
              , MkStmtWrite (RExp $ ExpNum 200)
              , MkStmtWrite (RExp $ ExpNum 300)
              ]
          )
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 500)
              , MkStmtWrite (RExp $ ExpNum 600)
              , MkStmtWrite (RExp $ ExpNum 700)
              ]
          )
        execStmtWrite $ MkStmtWrite (RExp $ ExpNum 555)
        execStmtWrite $ MkStmtWrite (RExp $ ExpNum 666)
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
      do
        execStmtAssign $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 0)
        execStmtWhile $ MkStmtWhile
          (RExp $ MkExpRelational
            (RLValue $ LValue [] "foo")
            OpLT
            (RExp $ ExpNum 10)
          )
          [ StmtAssign $ MkStmtAssign
            (LValue [] "foo")
            (RExp $ MkExpArithmetic
              (RLValue $ LValue [] "foo")
              OpAdd
              (RExp $ ExpNum 1)
            )
          , StmtWrite $ MkStmtWrite (RExp $ ExpStr "loop")
          ]
        execStmtWrite $ MkStmtWrite (RExp $ ExpStr "foo")
        execStmtWrite $ MkStmtWrite (RExp $ ExpStr "bar")
      [Symbol "foo" (DataType [] TypeInt) 0]
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
      do
        execStmtAssign $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 15)
        execStmtWhile $ MkStmtWhile
          (RExp $ MkExpRelational
            (RLValue $ LValue [] "foo")
            OpLT
            (RExp $ ExpNum 10)
          )
          [ StmtAssign $ MkStmtAssign
            (LValue [] "foo")
            (RExp $ MkExpArithmetic
              (RLValue $ LValue [] "foo")
              OpAdd
              (RExp $ ExpNum 1)
            )
          , StmtWrite $ MkStmtWrite (RExp $ ExpStr "loop")
          ]
        execStmtWrite $ MkStmtWrite (RExp $ ExpStr "foo")
        execStmtWrite $ MkStmtWrite (RExp $ ExpStr "bar")
      [Symbol "foo" (DataType [] TypeInt) 0]
      0
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "15"
  Simulator.getStdout simulator @?= ["foo", "bar"]

test_execStmtBreak :: TestTree
test_execStmtBreak = testCaseSteps "execStmtBreak" $ \_ -> do
  let
    code = cRun2
      do
        execStmtAssign $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 0)
        execStmtWhile $ MkStmtWhile
          (RExp $ MkExpRelational
            (RLValue $ LValue [] "foo")
            OpLT
            (RExp $ ExpNum 10)
          )
          [ StmtBreak $ MkStmtBreak
          , StmtAssign $ MkStmtAssign
            (LValue [] "foo")
            (RExp $ MkExpArithmetic
              (RLValue $ LValue [] "foo")
              OpAdd
              (RExp $ ExpNum 1)
            )
          , StmtWrite $ MkStmtWrite (RExp $ ExpStr "loop")
          ]
        execStmtWrite $ MkStmtWrite (RExp $ ExpStr "foo")
        execStmtWrite $ MkStmtWrite (RExp $ ExpStr "bar")
      [Symbol "foo" (DataType [] TypeInt) 0]
      0
      []
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "0"
  Simulator.getStdout simulator @?= ["foo", "bar"]

test_execStmtContinue :: TestTree
test_execStmtContinue = testCaseSteps "execStmtContinue" $ \_ -> do
  let
    code = cRun2
      do
        execStmtAssign $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 0)
        execStmtWhile $ MkStmtWhile
          (RExp $ MkExpRelational
            (RLValue $ LValue [] "foo")
            OpLT
            (RExp $ ExpNum 10)
          )
          [ StmtAssign $ MkStmtAssign
            (LValue [] "foo")
            (RExp $ MkExpArithmetic
              (RLValue $ LValue [] "foo")
              OpAdd
              (RExp $ ExpNum 1)
            )
          , StmtContinue $ MkStmtContinue
          , StmtWrite $ MkStmtWrite (RExp $ ExpStr "loop")
          ]
        execStmtWrite $ MkStmtWrite (RExp $ ExpStr "foo")
        execStmtWrite $ MkStmtWrite (RExp $ ExpStr "bar")
      [Symbol "foo" (DataType [] TypeInt) 0]
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
      do
        r <- execStmtRValue $ MkStmtRValue $ RFuncCall "foo" []
        appendCode [XSM_INT 10]
        execFuncDefs
        return r
      []
      0
      [ Func
          { funcName          = "foo"
          , funcRetType       = TypeInt
          , funcSymbols       = []
          , funcBody          =
            [StmtWrite $ MkStmtWrite (RExp $ ExpStr "Hello World")]
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
      do
        r <- getRValueInReg (RFuncCall "foo" [])
        appendCode [XSM_INT 10]
        execFuncDefs
        return r
      []
      0
      [ Func
          { funcName          = "foo"
          , funcRetType       = TypeInt
          , funcSymbols       = []
          , funcBody = [StmtReturn $ MkStmtReturn $ RExp $ ExpNum (123)]
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
      do
        r <- getRValueInReg
          (RFuncCall "foo" [RExp $ ExpNum 123, RExp $ ExpNum 444])
        appendCode [XSM_INT 10]
        execFuncDefs
        return r
      []
      0
      [ Func
          { funcName          = "foo"
          , funcRetType       = TypeInt
          , funcSymbols       =
            [ Symbol "a" (DataType [] TypeInt) (-4)
            , Symbol "b" (DataType [] TypeInt) (-3)
            ]
          , funcBody          =
            [ StmtReturn $ MkStmtReturn $ RExp $ MkExpArithmetic
                (RLValue $ LValue [] "a")
                OpSub
                (RLValue $ LValue [] "b")
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
    (code :: [XSMInstr], (r1, r2)) = cRun3
      do
        r1 <- getLValueLocInReg (LValue [] "foo")
        r2 <- getLValueLocInReg (LValue [] "bar")
        return (r1, r2)
      [ Symbol "foo" (DataType [] TypeInt) 0
      , Symbol "bar" (DataType [] TypeInt) 1
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
      do
        r1 <- getLValueLocInReg (LValue [] "bar")
        r2 <- getLValueLocInReg (LValue (RExp . ExpNum <$> [2, 3]) "bar")
        return (r1, r2)
      [ Symbol "foo" (DataType [3, 2] TypeInt) 0
      , Symbol "bar" (DataType [4, 5] TypeInt) 6
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
      do
        appendCode [XSM_MOV_Int BP 4900]
        modify
          (\s -> s
            { lSymbols =
              Just
                $ [ Symbol "foo" (DataType [] TypeInt) (-3)
                  , Symbol "bar" (DataType [] TypeInt) (-2)
                  ]
            }
          )
        r1 <- getLValueLocInReg (LValue [] "foo")
        r2 <- getLValueLocInReg (LValue [] "bar")
        return (r1, r2)
      []
      0
      []

  let simulator = Simulator.run (Simulator.init code)
  let r1Loc     = read @Int $ (Simulator.getRegVal r1 simulator)
  let r2Loc     = read @Int $ (Simulator.getRegVal r2 simulator)
  r1Loc @?= 4900 - 3
  r2Loc @?= 4900 - 2

test_buildFuncArgsTable :: TestTree
test_buildFuncArgsTable = testCaseSteps "buildFuncArgsTable" $ \steps ->
  do
    let
      symbols =
        [ G.Symbol
            { G.symName     = "foo"
            , G.symDataType = DataType [] TypeInt
            , G.symDeclSpan = undefined
            }
        , G.Symbol
            { G.symName     = "bar"
            , G.symDataType = DataType [] TypeInt
            , G.symDeclSpan = undefined
            }
        ]
    let [s1, s2] = buildFuncArgsTable symbols (-3)
    s1 @?= Symbol {symName = "foo", symDataType = DataType [] TypeInt, symRelLoc = -4}
    s2 @?= Symbol {symName = "bar", symDataType = DataType [] TypeInt, symRelLoc = -3}





