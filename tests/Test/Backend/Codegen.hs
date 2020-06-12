{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Backend.Codegen where

import Backend.Codegen as Codegen
import Backend.Instructions (XSMInstr)
import qualified Backend.Simulator as Simulator
import Control.Monad.State.Strict
import Data.Either.Extra
import Debug.Trace
import qualified Grammar
import Grammar hiding (Func (..), FuncDecl (..), FuncDef (..), Symbol (..))
import Span
import Test.GrammarUtils
import Test.Tasty
import Test.Tasty.HUnit
import Test.Utils

span0 :: Span
span0 = Span 0 0

span0A :: Applicative a => a Span
span0A = pure span0

fromRight'' x = case x of
  Left  l -> error $ show l
  Right r -> r

cRun' :: HasCallStack => GrammarM a -> (a -> Codegen b) -> b
cRun' g compfn =
  let
    (a, gstate) = fromRight'' $ gRun g
    symbols     = head $ gsSymbols gstate
    funcs       = gsFuncs gstate
  in fromRight' $ runCodegen (compfn a) (initCodegenState symbols funcs)

cRun2 codegenM syms symsLen funcs = fromRight'' $ runCodegen
  (execSetupGlobalSymtab >> codegenM >> getCodeTranslated)
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
                    [ StmtWrite $ MkStmtWrite (RExp $ ExpStr "Hello World!")
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
  let simulator = Simulator.run $ Simulator.initWithStdin ["10", "555"] code
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
  let simulator = Simulator.run (Simulator.initWithStdin ["10", "555"] code)
  Simulator.getStdout simulator @?= ["100", "ASD"]

test_execStmtIf :: TestTree
test_execStmtIf = testCaseSteps "execStmtIf" $ \step -> do
  step "If stmt true"
  let
    code = cRun2
      do
        execStmtIf $ MkStmtIf
          (RExp $ MkExpLogical (RExp $ ExpNum 1) OpEQ (RExp $ ExpNum 1))
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
          (RExp $ MkExpLogical (RExp $ ExpNum 1) OpEQ (RExp $ ExpNum 2))
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
          (RExp $ MkExpLogical (RExp $ ExpNum 1) OpEQ (RExp $ ExpNum 1))
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
          (RExp $ MkExpLogical (RExp $ ExpNum 1) OpNE (RExp $ ExpNum 1))
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
          ( RExp
          $ MkExpLogical (RLValue $ LValue [] "foo") OpLT (RExp $ ExpNum 10)
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
  Simulator.getStdout simulator @?= (take 10 $ repeat "loop") ++ ["foo", "bar"]
  step "While stmt false"
  let
    code = cRun2
      do
        execStmtAssign $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 15)
        execStmtWhile $ MkStmtWhile
          ( RExp
          $ MkExpLogical (RLValue $ LValue [] "foo") OpLT (RExp $ ExpNum 10)
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
          ( RExp
          $ MkExpLogical (RLValue $ LValue [] "foo") OpLT (RExp $ ExpNum 10)
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
          ( RExp
          $ MkExpLogical (RLValue $ LValue [] "foo") OpLT (RExp $ ExpNum 10)
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
test_execFunctionCall = testCaseSteps "execFunctionCallRValue" $ \step -> do
  step "No args"

-- do
--   execStmtRValue (StmtRValue (RFuncCall "foo" []))
--   execFuncDef

-- let
--   code = cRun
--     do
--       doVarDeclare "foo" TypeInt [] span0
--       doVarDeclare "bar" TypeInt [] span0
--       define <- doFuncDefine
--         TypeInt
--         "inc"
--         [spanW ("i", TypeInt)]
--         span0
--       join $ define <$> sequence
--         [ do
--           lhs <- mkLValue (spanW "foo") []
--           rhs <- RLValue <$> mkLValue (spanW "i") []
--           StmtAssign <$> mkStmtAssign lhs rhs span0
--         , do
--           StmtReturn <$> join
--             (   mkStmtReturn
--             <$> (RExp <$> join
--                   (   mkExpArithmetic
--                   <$> (   spanW
--                       .   RLValue
--                       <$> mkLValue (spanW "i") []
--                       )
--                   <*> (pure OpAdd)
--                   <*> (pure . spanW $ RExp $ ExpNum 1)
--                   )
--                 )
--             <*> (pure span0)
--             )
--         ]
--       sequence
--         [ StmtAssign <$> join
--             (   mkStmtAssign
--             <$> (mkLValue (spanW "bar") [])
--             <*> (mkExpFuncCall
--                   "inc"
--                   [spanW $ RExp $ ExpNum 100]
--                   span0
--                 )
--             <*> span0A
--             )
--         ]
--     execStmtsGetCode
-- let simulator = Simulator.run (Simulator.init code)
-- Simulator.getMemory 4096 simulator @?= "100"
-- Simulator.getMemory 4097 simulator @?= "101"

----------------------------------------------------------------------------------------

test_getLValueLocInReg :: TestTree
test_getLValueLocInReg = testCaseSteps "getLValueLocInReg" $ \step -> do
  step "Simple"
  let
    (code :: [XSMInstr], r1, r2) = cRun'
      do
        doVarDeclare "foo" TypeInt [] span0
        doVarDeclare "bar" TypeInt [] span0
        lValue1 <- mkLValue (spanW "foo") []
        lValue2 <- mkLValue (spanW "bar") []
        return (lValue1, lValue2)
      (\(lValue1, lValue2) -> do
        r1   <- getLValueLocInReg (lValue1)
        r2   <- getLValueLocInReg (lValue2)
        code <- gets Codegen.code
        return (code, r1, r2)
      )
  let simulator = Simulator.run (Simulator.init code)
  let loc1      = Simulator.getRegVal r1 simulator
  let loc2      = Simulator.getRegVal r2 simulator
  (read loc1 >= (4096 :: Int)) @? "Variables must be in stack"
  (read loc2 >= (4096 :: Int)) @? "Variables must be in stack"
  loc1 /= loc2 @? "Two variables must have different location"
  step "2D Array index"
  let
    (code, basereg, reg) = cRun'
      do
        doVarDeclare "foo" TypeInt [3, 2] span0
        doVarDeclare "bar" TypeInt [4, 5] span0
        l1 <- mkLValue (spanW "bar") []
        l2 <- mkLValue (spanW "bar") (spanW . RExp . ExpNum <$> [2, 3])
        return (l1, l2)
      (\(l1, l2) -> do
        base <- getLValueLocInReg l1
        r    <- getLValueLocInReg l2
        code <- gets Codegen.code
        return (code, base, r)
      )
  let simulator = Simulator.run (Simulator.init code)
  let loc       = read (Simulator.getRegVal reg simulator) :: Int
  let base      = read (Simulator.getRegVal basereg simulator) :: Int
  loc @?= base + 2 * 5 + 3
