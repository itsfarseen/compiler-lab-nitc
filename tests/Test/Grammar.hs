{-# LANGUAGE LambdaCase #-}
module Test.Grammar where

import Grammar
import Span
import Test.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Frontend (Frontend)
import qualified Frontend
import Error (Error)
import Control.Monad.State.Strict (gets)

gRun :: Frontend a -> Either Error a
gRun = Frontend.runFrontend (Frontend.initData "" Grammar.gsInit)

gAssertError :: Frontend a -> IO ()
gAssertError = assertError . gRun

gAssertRight :: Frontend a -> IO ()
gAssertRight x = gExtractRight x >> return ()

gExtractRight :: Frontend a -> IO a
gExtractRight x = assertRight $ gRun x

span0 :: Span
span0 = Span 0 0

-- Update list of tests - Run the following line as vim macro - 0w"qy$@q -
-- mq:let @b="" | g/\<test_\i\+\> ::/exe 'norm "ByE'|let @B=", "'q4j$di["bphxx
tests :: TestTree
tests = testGroup
  "Codegen"
  [ test_varDeclare
  , test_mkLValue
  , test_stmtAssign
  , test_stmtRead
  , test_stmtWrite
  , test_stmtIf
  , test_stmtIfElse
  , test_stmtWhile
  , test_stmtBreak
  , test_stmtContinue
  , test_funcDeclare
  , test_funcDefine
  , test_mkExpArithmetic
  , test_mkExpRelational
  , test_mkExpFuncCall
  ]

test_varDeclare :: TestTree
test_varDeclare = testCaseSteps "Variable Declaration" $ \step -> do
  step "Declare var"
  syms <- gExtractRight $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    mkLValue (spanW "foo") (spanW . RExp . ExpNum <$> [1, 2])
    Grammar.gsGets Grammar.gsGSymbols
  syms @?= [Symbol {symName = "foo", symDataType = DataType [5, 10] TypeInt, symDeclSpan = span0}]

  step "Declare var inside function"
  syms <- gExtractRight $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    define <- doFuncDefine TypeInt "fn" [] span0
    mkLValue (spanW "foo") (spanW . RExp . ExpNum <$> [1, 2])
    Grammar.gsGets Grammar.gsGSymbols
  syms @?= [Symbol {symName = "foo", symDataType = DataType [5, 10] TypeInt, symDeclSpan = span0}]


  step "Redeclare var"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [1, 2] (Span 0 0)
    doVarDeclare "foo" TypeInt [2, 2] (Span 0 0)

test_mkLValue :: TestTree
test_mkLValue = testCaseSteps "mkLValue" $ \step -> do
  step "Undeclared LValue"
  gAssertError $ mkLValue (spanW "foo") []

  step "Declared LValue"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkLValue (spanW "foo") []

  step "Too many index"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [1, 2, 3] (Span 0 0)
    mkLValue (spanW "foo") (spanW . RExp . ExpNum <$> [0, 1, 0, 0])

  step "Correct index"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [5, 5] (Span 0 0)
    mkLValue (spanW "foo") (spanW . RExp . ExpNum <$> [2, 2])
  return ()

test_stmtAssign :: TestTree
test_stmtAssign = testCaseSteps "StmtAssign" $ \step -> do
  step "Assign constant"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    mkStmtAssign
      (LValue (RExp . ExpNum <$> [1, 2]) "foo")
      (RExp $ ExpNum 10)
      (Span 0 0)

  step "Assign variable"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    doVarDeclare "bar" TypeInt []      (Span 0 0)
    mkStmtAssign
      (LValue (RExp . ExpNum <$> [1, 2]) "foo")
      (RLValue $ LValue [] "bar")
      (Span 0 0)

  step "Assign self"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkStmtAssign (LValue [] "foo") (RLValue $ LValue [] "foo") (Span 0 0)

  step "Type mismatch"
  gAssertError $ do
    doVarDeclare "foo" TypeString [5, 10] (Span 0 0)
    mkStmtAssign
      (LValue (RExp . ExpNum <$> [1, 2]) "foo")
      (RExp $ ExpNum 10)
      (Span 0 0)

  step "Assign to array"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    doVarDeclare "bar" TypeInt [5, 10] (Span 0 0)
    mkStmtAssign (LValue [] "foo") (RLValue $ LValue [] "bar") (Span 0 0)

test_stmtRead :: TestTree
test_stmtRead = testCaseSteps "StmtRead" $ \step -> do
  step "Read Int"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    mkStmtRead $ SpanW (LValue (RExp . ExpNum <$> [0, 1]) "foo") (Span 0 0)

  step "Read String"
  gAssertRight $ do
    doVarDeclare "foo" TypeString [5, 10] (Span 0 0)
    mkStmtRead $ SpanW (LValue (RExp . ExpNum <$> [0, 1]) "foo") (Span 0 0)

  step "Read bool"
  gAssertError $ do
    doVarDeclare "foo" TypeBool [5, 10] (Span 0 0)
    mkStmtRead $ SpanW (LValue [] "foo") (Span 0 0)

  step "Read array"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [1] (Span 0 0)
    mkStmtRead $ SpanW (LValue [] "foo") (Span 0 0)

test_stmtWrite :: TestTree
test_stmtWrite = testCaseSteps "StmtWrite" $ \step -> do
  step "Write Int"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkStmtWrite $ SpanW (RLValue $ LValue [] "foo") (Span 0 0)

  step "Write String"
  gAssertRight $ do
    doVarDeclare "foo" TypeString [] (Span 0 0)
    mkStmtWrite $ SpanW (RLValue $ LValue [] "foo") (Span 0 0)

  step "Write Bool"
  gAssertError $ do
    doVarDeclare "foo" TypeBool [] (Span 0 0)
    mkStmtWrite $ SpanW (RLValue $ LValue [] "foo") (Span 0 0)

  step "Write Array"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [1] (Span 0 0)
    mkStmtWrite $ SpanW (RLValue $ LValue [] "foo") (Span 0 0)

test_stmtIf :: TestTree
test_stmtIf = testCaseSteps "StmtIf" $ \step -> do
  step "If Bool"
  gAssertRight $ do
    doVarDeclare "foo" TypeBool [] (Span 0 0)
    mkStmtIf (SpanW (RLValue $ LValue [] "foo") (Span 0 0)) []

  step "If non Bool"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkStmtIf (SpanW (RLValue $ LValue [] "foo") (Span 0 0)) []

test_stmtIfElse :: TestTree
test_stmtIfElse = testCaseSteps "StmtIfElse" $ \step -> do
  step "IfElse Bool"
  gAssertRight $ do
    doVarDeclare "foo" TypeBool [] (Span 0 0)
    mkStmtIfElse (SpanW (RLValue $ LValue [] "foo") (Span 0 0)) [] []

  step "IfElse non Bool"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkStmtIfElse (SpanW (RLValue $ LValue [] "foo") (Span 0 0)) [] []

test_stmtWhile :: TestTree
test_stmtWhile = testCaseSteps "StmtWhile" $ \step -> do
  step "While Bool"
  gAssertRight $ do
    doVarDeclare "foo" TypeBool [] (Span 0 0)
    mkStmtWhile (SpanW (RLValue $ LValue [] "foo") (Span 0 0)) []

  step "While non Bool"
  gAssertError $ do
    doVarDeclare "foo" TypeString [] (Span 0 0)
    mkStmtWhile (SpanW (RLValue $ LValue [] "foo") (Span 0 0)) []

test_stmtBreak :: TestTree
test_stmtBreak = testCaseSteps "StmtBreak" $ \step -> do
  step "Break Inside Loop"
  gAssertRight $ do
    pushLoop
    mkStmtBreak (Span 0 0)

  step "Break Outside Loop"
  gAssertError $ mkStmtBreak (Span 0 0)

test_stmtContinue :: TestTree
test_stmtContinue = testCaseSteps "StmtContinue" $ \step -> do
  step "Continue Inside Loop"
  gAssertRight $ do
    pushLoop
    mkStmtContinue (Span 0 0)

  step "Continue Outside Loop"
  gAssertError $ mkStmtContinue (Span 0 0)

test_funcDeclare :: TestTree
test_funcDeclare = testCaseSteps "Func Declare" $ \step -> do
  step "Declare function"
  gAssertRight $ do
    doFuncDeclare
      TypeInt
      "foo"
      (flip SpanW (Span 0 0) <$> [TypeInt, TypeInt, TypeInt])
      (Span 0 0)

  -- TODO: Check function is actually declared using RValue

  step "Redeclare function"
  gAssertError $ do
    doFuncDeclare TypeString "foo" [] (Span 0 0)
    doFuncDeclare TypeString "foo" [] (Span 0 0)

test_funcDefine :: TestTree
test_funcDefine = testCaseSteps "Func Define" $ \step -> do

  step "Declare and define function"
  gAssertRight $ do
    doFuncDeclare TypeInt "foo" (spanW <$> [TypeInt, TypeInt]) (Span 0 0)
    define <- doFuncDefine
      TypeInt
      "foo"
      (spanW <$> [("fff", TypeInt), ("bar", TypeInt)])
      (Span 0 0)
    define []

  step "Define without declare"
  gAssertRight $ do
    define <- doFuncDefine
      TypeInt
      "foo"
      (flip SpanW (Span 0 0) <$> [("fff", TypeInt), ("bar", TypeInt)])
      (Span 0 0)
    define []

  step "Redeclare function"
  gAssertError $ do
    define <- doFuncDefine
      TypeInt
      "foo"
      (flip SpanW (Span 0 0) <$> [("fff", TypeInt), ("bar", TypeInt)])
      (Span 0 0)
    define []
    doFuncDeclare TypeString "foo" [] (Span 0 0)

  step "Function declaration mismatch - return type"
  gAssertError $ do
    doFuncDeclare TypeString "foo" [] (Span 0 0)
    _ <- doFuncDefine TypeInt "foo" [] (Span 0 0)
    return ()

  step "Function declaration mismatch - args"
  gAssertError $ do
    doFuncDeclare TypeString "foo" [] (Span 0 0)
    _ <- doFuncDefine
      TypeString
      "foo"
      [SpanW ("ff", TypeInt) (Span 0 0)]
      (Span 0 0)
    return ()

test_mkExpArithmetic :: TestTree
test_mkExpArithmetic = testCaseSteps "Exp Arithmetic" $ \step -> do
  step "Int Int"
  gAssertRight $ mkExpArithmetic
    (spanW (RExp $ ExpNum 1))
    OpAdd
    (spanW (RExp $ ExpNum 1))

  step "Str Int"
  gAssertError $ mkExpArithmetic
    (spanW (RExp $ ExpStr "Foo"))
    OpAdd
    (spanW (RExp $ ExpNum 1))

  step "Int Str"
  gAssertError $ mkExpArithmetic
    (spanW (RExp $ ExpNum 1))
    OpAdd
    (spanW (RExp $ ExpStr "Foo"))

  return ()

test_mkExpRelational :: TestTree
test_mkExpRelational = testCaseSteps "Exp Relational" $ \step -> do
  step "Int Int"
  gAssertRight
    $ mkExpRelational (spanW (RExp $ ExpNum 1)) OpLT (spanW (RExp $ ExpNum 1))

  step "Str Str"
  gAssertRight $ mkExpRelational
    (spanW (RExp $ ExpStr "A"))
    OpLT
    (spanW (RExp $ ExpStr "B"))

  step "Int Str"
  gAssertError
    $ mkExpRelational (spanW (RExp $ ExpNum 1)) OpLT (spanW (RExp $ ExpStr "B"))

  return ()

test_mkExpFuncCall :: TestTree
test_mkExpFuncCall = testCaseSteps "mkExpFuncCall" $ \step -> do
  step "Undeclared function"
  gAssertError $ do
    mkExpFuncCall "foo" [] (Span 0 0)

  step "Declared function"
  gAssertRight $ do
    doFuncDeclare TypeInt "foo" [] (Span 0 0)
    mkExpFuncCall "foo" [] (Span 0 0)

  step "arg type mismatch"
  gAssertError $ do
    doFuncDeclare TypeInt "foo" (spanW <$> [TypeInt]) (Span 0 0)
    mkExpFuncCall "foo" (spanW . RExp . ExpNum <$> [1, 2]) (Span 0 0)

  step "assign to var"
  gAssertRight $ do
    doVarDeclare "bar" TypeInt [] (Span 0 0)
    doFuncDeclare TypeInt "foo" [] (Span 0 0)
    exp    <- mkExpFuncCall "foo" [] (Span 0 0)
    lValue <- mkLValue (spanW "bar") []
    mkStmtAssign lValue exp (Span 0 0)

  return ()
