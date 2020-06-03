{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Test.Grammar where

import Control.Monad.Except (MonadError)
import Control.Monad.State.Strict
import Data.List (find)
import Error
import Grammar
import Span
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

data TestMState = TestMState
  { tsSymbols :: [[Symbol]],
    tsFuncs :: [Func],
    tsLoopStack :: Int
  }

initState :: TestMState
initState = TestMState [[]] [] 0

newtype TestM a
  = TestM (StateT TestMState (Either Error) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError Error,
      MonadState TestMState
    )

insertList :: Eq k => (a -> k) -> a -> [a] -> [a]
insertList key a list = case list of
  [] -> [a]
  (a' : as') ->
    (if key a == key a'
      then a : as'
      else a' : insertList key a as'
    )

spanW :: a -> SpanW a
spanW a = SpanW a (Span 0 0)

instance ReadSymbols TestM where
  symLookup name = gets
    (find (\x -> symName x == name) . concat . tsSymbols)

instance WriteSymbols TestM where
  symInsert sym = modify
    (\state ->
      let (symtab : ss) = tsSymbols state
          symtab'       = symtab ++ [sym]
          tsSymbols'    = (symtab' : ss)
      in  state { tsSymbols = tsSymbols' }
        -- Note: multiple insert will cause duplicates
    )

instance ReadFuncs TestM where
  funcLookup name = gets
    (find (\x -> (funcName . funcDecl) x == name) . tsFuncs)

instance WriteFuncs TestM where
  funcInsert func = modify
    (\state -> state
      { tsFuncs = insertList (funcName . funcDecl)
                             func
                             (tsFuncs state)
      }
        -- Note: multiple insert will cause duplicates
    )

instance ReadLoopStack TestM where
  hasLoop = gets (\x -> tsLoopStack x > 0)

instance LoopStackWriter TestM where
  pushLoop =
    modify (\x -> x { tsLoopStack = tsLoopStack x + 1 })
  popLoop =
    modify
      (\x -> x { tsLoopStack = max 0 (tsLoopStack x - 1) })

instance SymbolTableStack TestM where
  symStackPush =
    modify (\x -> x { tsSymbols = [] : (tsSymbols x) })
  symStackPop = do
    (symtab, ss) <- gets tsSymbols >>= \case
      [] -> error "tsSymbols can't be empty"
      [_] -> error "can't pop symStack, only one symtab"
      (symtab : ss) -> return (symtab, ss)
    modify (\x -> x { tsSymbols = ss })
    return symtab

runTestM
  :: TestMState -> TestM a -> Either Error (a, TestMState)
runTestM state testM =
  let (TestM stateT) = testM in runStateT stateT state

assertError :: HasCallStack => Either Error b -> IO ()
assertError = \case
  Left _ -> return ()
  _      -> assertFailure "Error was not raised"

assertRight :: HasCallStack => Either Error a -> IO a
assertRight = \case
  Left  error -> assertFailure $ "Error: " ++ (show error)
  Right x     -> return x

test_varDeclare :: TestTree
test_varDeclare =
  testCaseSteps "Variable Declaration" $ \step -> do
    step "Declare var"
    _ <- assertRight $ runTestM initState $ do
      doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
      mkLValue (spanW "foo")
               (spanW . RExp . ExpNum <$> [1, 2])

    step "Redeclare var"
    assertError $ runTestM initState $ do
      doVarDeclare "foo" TypeInt  [1, 2] (Span 0 0)
      doVarDeclare "foo" TypeBool [2, 2] (Span 0 0)


test_mkLValue :: TestTree
test_mkLValue = testCaseSteps "mkLValue" $ \step -> do
  step "Undeclared LValue"
  assertError $ runTestM initState $ mkLValue
    (spanW "foo")
    []

  step "Declared LValue"
  _ <- assertRight $ runTestM initState $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkLValue (spanW "foo") []

  step "Too many index"
  assertError $ runTestM initState $ do
    doVarDeclare "foo" TypeInt [1, 2, 3] (Span 0 0)
    mkLValue (spanW "foo")
             (spanW . RExp . ExpNum <$> [0, 1, 0, 0])

  step "Correct index"
  _ <- assertRight $ runTestM initState $ do
    doVarDeclare "foo" TypeInt [5, 5] (Span 0 0)
    mkLValue (spanW "foo")
             (spanW . RExp . ExpNum <$> [2, 2])
  return ()

test_stmtAssign :: TestTree
test_stmtAssign = testCaseSteps "StmtAssign" $ \step -> do
  step "Assign constant"
  _ <- assertRight $ runTestM initState $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    mkStmtAssign (LValue (RExp . ExpNum <$> [1, 2]) "foo")
                 (RExp $ ExpNum 10)
                 (Span 0 0)

  step "Assign variable"
  (stmt, _) <- assertRight $ runTestM initState $ do
    symInsert
      $ Symbol "foo" (DataType [5, 10] TypeInt) (Span 0 0)
    symInsert
      $ Symbol "bar" (DataType [] TypeInt) (Span 0 0)
    mkStmtAssign (LValue (RExp . ExpNum <$> [1, 2]) "foo")
                 (RLValue $ LValue [] "bar")
                 (Span 0 0)
  let (MkStmtAssign lhs rhs) = stmt
  lhs @?= LValue (RExp . ExpNum <$> [1, 2]) "foo"
  rhs @?= (RLValue $ LValue [] "bar")
  step "Assign self"
  (stmt, _) <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeInt) (Span 0 0))
    mkStmtAssign (LValue [] "foo")
                 (RLValue $ LValue [] "foo")
                 (Span 0 0)
  let (MkStmtAssign lhs rhs) = stmt
  lhs @?= LValue [] "foo"
  rhs @?= (RLValue $ LValue [] "foo")
  step "Type mismatch"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [5, 10] TypeString) (Span 0 0)
      )
    mkStmtAssign (LValue (RExp . ExpNum <$> [1, 2]) "foo")
                 (RExp $ ExpNum 10)
                 (Span 0 0)
  step "Assign to array"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [5, 10] TypeString) (Span 0 0)
      )
    symInsert
      (Symbol "bar" (DataType [5, 10] TypeString) (Span 0 0)
      )
    mkStmtAssign (LValue [] "foo")
                 (RLValue $ LValue [] "bar")
                 (Span 0 0)

test_stmtRead :: TestTree
test_stmtRead = testCaseSteps "StmtRead" $ \step -> do
  step "Read Int"
  _ <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [5, 10] TypeString) (Span 0 0)
      )
    mkStmtRead $ SpanW
      (LValue (RExp . ExpNum <$> [0, 1]) "foo")
      (Span 0 0)

  step "Read String"
  _ <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [5, 10] TypeString) (Span 0 0)
      )
    mkStmtRead $ SpanW
      (LValue (RExp . ExpNum <$> [0, 1]) "foo")
      (Span 0 0)

  step "Read bool"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeBool) (Span 0 0))
    mkStmtRead $ SpanW (LValue [] "foo") (Span 0 0)
  step "Read array"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [1] TypeBool) (Span 0 0))
    mkStmtRead $ SpanW (LValue [] "foo") (Span 0 0)
  step "Read array"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [5, 10] TypeBool) (Span 0 0))
    mkStmtRead
      $ SpanW (LValue [RExp $ ExpNum 1] "foo") (Span 0 0)

test_stmtWrite :: TestTree
test_stmtWrite = testCaseSteps "StmtWrite" $ \step -> do
  step "Write Int"
  _ <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeInt) (Span 0 0))
    mkStmtWrite
      $ SpanW (RLValue $ LValue [] "foo") (Span 0 0)

  step "Write String"
  _ <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeString) (Span 0 0))
    mkStmtWrite
      $ SpanW (RLValue $ LValue [] "foo") (Span 0 0)

  step "Write Bool"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeBool) (Span 0 0))
    mkStmtWrite
      $ SpanW (RLValue $ LValue [] "foo") (Span 0 0)

  step "Write Array Element"
  _ <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [5, 10] TypeInt) (Span 0 0))
    mkStmtWrite $ SpanW
      (RLValue $ LValue (RExp . ExpNum <$> [0, 1]) "foo")
      (Span 0 0)

  step "Write Array"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [1] TypeInt) (Span 0 0))
    mkStmtWrite
      $ SpanW (RLValue $ LValue [] "foo") (Span 0 0)

test_stmtIf :: TestTree
test_stmtIf = testCaseSteps "StmtIf" $ \step -> do
  step "If Bool"
  _ <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeBool) (Span 0 0))
    mkStmtIf
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []

  step "If String"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeString) (Span 0 0))
    mkStmtIf
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []

  step "If Int"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeInt) (Span 0 0))
    mkStmtIf
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []

  step "If Bool Array"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [1] TypeBool) (Span 0 0))
    mkStmtIf
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []

test_stmtIfElse :: TestTree
test_stmtIfElse = testCaseSteps "StmtIfElse" $ \step -> do
  step "IfElse Bool"
  _ <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeBool) (Span 0 0))
    mkStmtIfElse
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []
      []

  step "IfElse String"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeString) (Span 0 0))
    mkStmtIfElse
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []
      []
  step "IfElse Int"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeInt) (Span 0 0))
    mkStmtIfElse
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []
      []
  step "IfElse Array"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [1] TypeBool) (Span 0 0))
    mkStmtIfElse
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []
      []

test_stmtWhile :: TestTree
test_stmtWhile = testCaseSteps "StmtWhile" $ \step -> do
  step "While Bool"
  _ <- assertRight $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeBool) (Span 0 0))
    mkStmtWhile
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []

  step "While String"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeString) (Span 0 0))
    mkStmtWhile
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []
  step "While Int"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [] TypeInt) (Span 0 0))
    mkStmtWhile
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []
  step "While Bool Array"
  assertError $ runTestM initState $ do
    symInsert
      (Symbol "foo" (DataType [1] TypeBool) (Span 0 0))
    mkStmtWhile
      (SpanW (RLValue $ LValue [] "foo") (Span 0 0))
      []

test_stmtBreak :: TestTree
test_stmtBreak = testCaseSteps "StmtBreak" $ \step -> do
  step "Break Inside Loop"
  _ <- assertRight $ runTestM initState $ do
    pushLoop
    mkStmtBreak (Span 0 0)
  step "Break Outside Loop"
  assertError $ runTestM initState $ mkStmtBreak (Span 0 0)

test_stmtContinue :: TestTree
test_stmtContinue =
  testCaseSteps "StmtContinue" $ \step -> do
    step "Continue Inside Loop"
    _ <- assertRight $ runTestM initState $ do
      pushLoop
      mkStmtContinue (Span 0 0)
    step "Continue Outside Loop"
    assertError $ runTestM initState $ mkStmtContinue
      (Span 0 0)

test_funcDeclare :: TestTree
test_funcDeclare = testCaseSteps "Func Declare" $ \step ->
  do
    step "Declare function"
    _ <- assertRight $ runTestM initState $ do
      doFuncDeclare
        TypeInt
        "foo"
        (   flip SpanW (Span 0 0)
        <$> [TypeInt, TypeInt, TypeInt]
        )
        (Span 0 0)

    -- TODO: Check function is actually declared using RValue

    step "Redeclare function"
    assertError $ runTestM initState $ do
      doFuncDeclare TypeString "foo" [] (Span 0 0)
      doFuncDeclare TypeString "foo" [] (Span 0 0)

test_funcDefine :: TestTree
test_funcDefine = testCaseSteps "Func Define" $ \step -> do

  step "Declare and define function"
  _ <- assertRight $ runTestM initState $ do
    doFuncDeclare TypeInt
                  "foo"
                  (spanW <$> [TypeInt, TypeInt])
                  (Span 0 0)
    define <- doFuncDefine
      TypeInt
      "foo"
      (spanW <$> [("fff", TypeInt), ("bar", TypeInt)])
      (Span 0 0)
    define []

  step "Define without declare"
  _ <- assertRight $ runTestM initState $ do
    define <- doFuncDefine
      TypeInt
      "foo"
      (   flip SpanW (Span 0 0)
      <$> [("fff", TypeInt), ("bar", TypeInt)]
      )
      (Span 0 0)
    define []

  step "Redeclare function"
  assertError $ runTestM initState $ do
    define <- doFuncDefine
      TypeInt
      "foo"
      (   flip SpanW (Span 0 0)
      <$> [("fff", TypeInt), ("bar", TypeInt)]
      )
      (Span 0 0)
    define []
    doFuncDeclare TypeString "foo" [] (Span 0 0)

  step "Function declaration mismatch"
  assertError $ runTestM initState $ do
    doFuncDeclare TypeString "foo" [] (Span 0 0)
    _ <- doFuncDefine TypeInt "foo" [] (Span 0 0)
    return ()

  step "Function declaration mismatch"
  assertError $ runTestM initState $ do
    doFuncDeclare TypeString "foo" [] (Span 0 0)
    _ <- doFuncDefine TypeString
                      "foo"
                      [SpanW ("ff", TypeInt) (Span 0 0)]
                      (Span 0 0)
    return ()

test_mkExpArithmetic :: TestTree
test_mkExpArithmetic =
  testCaseSteps "Exp Arithmetic" $ \step -> do
    step "Int Int"
    _ <- assertRight $ runTestM initState $ mkExpArithmetic
      (spanW (RExp $ ExpNum 1))
      OpAdd
      (spanW (RExp $ ExpNum 1))

    step "Str Int"
    assertError $ runTestM initState $ mkExpArithmetic
      (spanW (RExp $ ExpStr "Foo"))
      OpAdd
      (spanW (RExp $ ExpNum 1))

    step "Int Str"
    assertError $ runTestM initState $ mkExpArithmetic
      (spanW (RExp $ ExpNum 1))
      OpAdd
      (spanW (RExp $ ExpStr "Foo"))

    return ()

test_mkExpLogical :: TestTree
test_mkExpLogical = testCaseSteps "Exp Logical" $ \step ->
  do
    step "Int Int"
    _ <- assertRight $ runTestM initState $ mkExpLogical
      (spanW (RExp $ ExpNum 1))
      OpLT
      (spanW (RExp $ ExpNum 1))

    step "Str Str"

    _ <- assertRight $ runTestM initState $ mkExpLogical
      (spanW (RExp $ ExpStr "A"))
      OpLT
      (spanW (RExp $ ExpStr "B"))

    step "Int Str"
    assertError $ runTestM initState $ mkExpLogical
      (spanW (RExp $ ExpNum 1))
      OpLT
      (spanW (RExp $ ExpStr "B"))

    return ()

test_mkExpFuncCall = testCaseSteps "mkExpFuncCall" $ \step -> do
  step "Undeclared function"
  assertError $ runTestM initState $ do
    mkExpFuncCall "foo" [] (Span 0 0)

  step "Declared function"
  _ <- assertRight $ runTestM initState $ do
    doFuncDeclare TypeInt "foo" [] (Span 0 0)
    mkExpFuncCall "foo" [] (Span 0 0)

  step "arg type mismatch"
  assertError $ runTestM initState $ do
    doFuncDeclare TypeInt "foo" ( spanW <$> [TypeInt] ) (Span 0 0)
    mkExpFuncCall "foo" ( spanW . RExp . ExpNum <$> [1, 2] ) (Span 0 0)

  step "assign to var"
  _ <- assertRight $ runTestM initState $ do
    doVarDeclare "bar" TypeInt [] (Span 0 0)
    doFuncDeclare TypeInt "foo" [] (Span 0 0)
    exp <- mkExpFuncCall "foo" [] (Span 0 0)
    lValue <- mkLValue (spanW "bar") []
    mkStmtAssign lValue exp (Span 0 0)

  return ()



