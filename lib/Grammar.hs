{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TupleSections    #-}

module Grammar where

import Control.Monad (unless)
import Control.Applicative
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import Error (Error)
import qualified Error
import Span

import Debug.Trace
dbgs :: Show a => String -> a -> a
dbgs s v = trace (s ++ ": " ++ show v) v

-- AST
data Program =
  Program SymTabGlobal [FuncDef] [UserType]
  deriving (Eq, Show)

data Stmt
  = StmtAssign StmtAssign
  | StmtRead StmtRead
  | StmtWrite StmtWrite
  | StmtIf StmtIf
  | StmtIfElse StmtIfElse
  | StmtWhile StmtWhile
  | StmtBreak StmtBreak
  | StmtContinue StmtContinue
  | StmtRValue StmtRValue -- For function calls, syscall
                          -- they are both RValue and Statement
  | StmtReturn StmtReturn
  | StmtInitialize StmtInitialize
  | StmtAlloc StmtAlloc
  | StmtFree StmtFree
  | StmtPoke StmtPoke
  | StmtNew StmtNew
  deriving (Show, Eq)

data StmtAssign =
  MkStmtAssign LValue RValue
  deriving (Show, Eq)

data StmtRead =
  MkStmtRead LValue
  deriving (Show, Eq)

data StmtWrite =
  MkStmtWrite RValue
  deriving (Show, Eq)

data StmtIf =
  MkStmtIf RValue [Stmt]
  deriving (Show, Eq)

data StmtIfElse =
  MkStmtIfElse RValue [Stmt] [Stmt]
  deriving (Show, Eq)

data StmtWhile =
  MkStmtWhile RValue [Stmt]
  deriving (Show, Eq)

data StmtBreak =
  MkStmtBreak
  deriving (Show, Eq)

data StmtContinue =
  MkStmtContinue
  deriving (Show, Eq)

data StmtRValue =
  MkStmtRValue RValue
  deriving (Show, Eq)

data StmtReturn =
  MkStmtReturn RValue
  deriving (Show, Eq)

data StmtInitialize =
  MkStmtInitialize
  deriving (Show, Eq)

data StmtAlloc =
  MkStmtAlloc LValue
  deriving (Show, Eq)

data StmtFree =
  MkStmtFree LValue
  deriving (Show, Eq)

data StmtPoke =
  MkStmtPoke RValue RValue
  deriving (Show, Eq)

data StmtNew = 
  MkStmtNew LValue Type1
  deriving (Show, Eq)

data LValue
  = LValueGlobal { lvName     :: String
                 , lvIndices  :: [RValue]
                 , lvType     :: Type2
                 , lvDeclSpan :: Span
                 }
  | LValueLocal { lvName     :: String
                , lvIndices  :: [RValue]
                , lvType     :: Type2
                , lvDeclSpan :: Span
                }
  | LValueField { lvName       :: String
                , lvIndices    :: [RValue]
                , lvType       :: Type2
                , lvDeclSpan   :: Span
                , lvParent     :: LValue
                , lvParentTypeName :: String
                }
  | LValueSelf  { lvType :: Type2 }
  deriving (Show, Eq)

newtype SymTabLocal = SymTabLocal [Symbol] deriving (Show, Eq, SymTab)
newtype SymTabGlobal = SymTabGlobal [Symbol] deriving (Show, Eq, SymTab)
newtype SymTabFields = SymTabFields [Symbol] deriving (Show, Eq, SymTab)

class SymTab a where
  symLookup :: String -> a -> Maybe Symbol
  symInsert :: Symbol -> a -> a

instance SymTab [Symbol] where
  symLookup name symbols = (find $ \s -> symName s == name) symbols
  symInsert symbol symbols = insertList symName symbol symbols

newtype LoopStack = LoopStack Int deriving (Show, Eq)

loopStackPush :: LoopStack -> LoopStack
loopStackPush (LoopStack i) = (LoopStack $ i + 1)

hasLoop :: LoopStack -> Bool
hasLoop (LoopStack i) = i > 0

loopStackInit :: LoopStack
loopStackInit = LoopStack 0

newtype FuncRetType = FuncRetType Type2

data RValue
  = RLValue LValue
  | RExp Exp
  | RFuncCall String [RValue] Type1
  | RMethodCall LValue String [RValue] Type1
  | RSyscall Int Int RValue RValue RValue
  | RPeek RValue
  | RNull
  deriving (Show, Eq)

data Exp
  = ExpNum Int
  | ExpStr String
  | MkExpArithmetic RValue OpArithmetic RValue
  | MkExpRelational RValue OpRelational RValue
  | MkExpLogical RValue OpLogical RValue
  deriving (Show, Eq)

data OpArithmetic
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  deriving (Show, Eq)

data OpRelational
  = OpLT
  | OpGT
  | OpLE
  | OpGE
  | OpEQ
  | OpNE
  deriving (Show, Eq)

data OpLogical
  = OpLAnd
  | OpLOr
  deriving (Show, Eq)

data Symbol
  = Symbol { symName       :: String
           , symType       :: Type2
           , symDeclSpan   :: Span
           , symVisibility :: SymbolVisibility
           }
  deriving (Show, Eq)

data SymbolVisibility
  = SVPublic
  | SVPrivate
  deriving (Show, Eq)

data Func
  = FuncDeclared FuncDecl
  | FuncDefined FuncDef
  deriving (Show, Eq)

data FuncDecl
  = FuncDecl { fDeclName     :: String
             , fDeclRetType  :: Type1
             , fDeclArgTypes :: [SpanW Type1]
             , fDeclSpan     :: Span
             }
  deriving (Show, Eq)

data FuncDef
  = FuncDef { fDefName     :: String
            , fDefRetType  :: Type1
            , fDefArgTypes :: [SpanW Type1]
            , fDefDeclSpan :: Span
            , fDefBody     :: [Stmt]
            , fDefArgsLen  :: Int
            , fDefSyms     :: SymTabLocal
            , fDefSpan     :: Span
            }
  deriving (Show, Eq)

-- Types that take single word
data Type1
  = TypeInt
  | TypeBool
  | TypeString
  | TypeUser [String] -- Will be implemented as pointer, so single word
  | TypeAny
  deriving (Eq)

data Type2 =
  Type2
    [Int] -- Dims
    Type1
  deriving (Eq)

data UserType
  = UserType { utName     :: String
             , utFields   :: [Symbol]
             , utFuncs    :: [Func]
             , utDeclSpan :: Span
             , utFieldsVisibility :: SymbolVisibility
             , utParentName :: Maybe String
             }
  deriving (Eq, Show)

mkProgram :: SymTabGlobal -> [Func] -> [UserType] -> Either Error Program
mkProgram syms funcs userTypes = do
  funcs <- mapM getFuncDef funcs
  return $ Program syms funcs userTypes
 where
  getFuncDef func = case func of
    FuncDeclared f -> Left $ errFuncNotDefined (fDeclName f) (fDeclSpan f)
    FuncDefined  f -> return f

doVarDeclare
  :: SymTab symTab
  => String
  -> Type1
  -> [Int]
  -> Span
  -> symTab
  -> Either Error symTab
doVarDeclare identName primType dims span symTab = do
  checkSymbolExists
  return $ symInsert symbol symTab
 where
  dataType = Type2 dims primType
  symbol   = Symbol
    { symName       = identName
    , symDeclSpan   = span
    , symType       = dataType
    , symVisibility = SVPublic
    }
  checkSymbolExists = case symLookup identName symTab of
    Nothing -> return ()
    Just sym ->
      Left $ errIdentifierRedeclared identName (symDeclSpan sym) span

mkStmtAssign :: LValue -> RValue -> Span -> Either Error StmtAssign
mkStmtAssign lhs rhs span = do
  let lhsType        = lvType lhs
  let lhsDeclSpan    = lvDeclSpan lhs
  let rhsType        = rValueType rhs
  let (Type2 dims _) = lhsType
  unless (null dims)
    $ Left --
    $ errArrayNotAllowed span lhsDeclSpan
  typeCheck lhsType rhsType span
  return $ MkStmtAssign lhs rhs

mkStmtRead :: SpanW LValue -> Either Error StmtRead
mkStmtRead (SpanW lValue lValueSpan) = do
  typeCheck1
    (lvType lValue)
    ([TypeInt, TypeString])
    lValueSpan
  return $ MkStmtRead lValue

mkStmtWrite :: SpanW RValue -> Either Error StmtWrite
mkStmtWrite (SpanW rValue rValueSpan) = do
  typeCheck1 (rValueType rValue) ( [TypeInt, TypeString]) rValueSpan
  return $ MkStmtWrite rValue

mkStmtIf :: SpanW RValue -> [Stmt] -> Either Error StmtIf
mkStmtIf (SpanW cond span) body = do
  typeCheck1 (rValueType cond) ( [TypeBool]) span
  return $ MkStmtIf cond body

mkStmtIfElse :: SpanW RValue -> [Stmt] -> [Stmt] -> Either Error StmtIfElse
mkStmtIfElse (SpanW cond span) thenBody elseBody = do
  typeCheck1 (rValueType cond) ( [TypeBool]) span
  return $ MkStmtIfElse cond thenBody elseBody

mkStmtWhile
  :: SpanW RValue
  -> LoopStack
  -> Either Error (LoopStack, [Stmt] -> StmtWhile)
mkStmtWhile (SpanW cond span) loopStack = do
  typeCheck1 (rValueType cond) ( [TypeBool]) span
  return $ (loopStackPush loopStack, ) $ \body -> MkStmtWhile cond body

mkStmtBreak :: Span -> LoopStack -> Either Error StmtBreak
mkStmtBreak span loopStack = do
  unless (hasLoop loopStack) throwOutOfLoop
  return MkStmtBreak
  where throwOutOfLoop = Left (Error.syntaxError span)

mkStmtContinue :: Span -> LoopStack -> Either Error StmtContinue
mkStmtContinue span loopStack = do
  unless (hasLoop loopStack) throwOutOfLoop
  return MkStmtContinue
  where throwOutOfLoop = Left (Error.syntaxError span)

mkStmtRValue :: RValue -> StmtRValue
mkStmtRValue rValue = MkStmtRValue rValue

mkStmtReturn :: RValue -> Span -> FuncDecl -> Either Error StmtReturn
mkStmtReturn rValue span funcDecl = do
  let retType     = Type2 [] (fDeclRetType funcDecl)
  let rValueType_ = rValueType rValue
  typeCheck retType rValueType_ span
  return $ MkStmtReturn rValue

mkStmtInitialize :: Span -> Either Error StmtInitialize
mkStmtInitialize _span = return MkStmtInitialize

mkStmtAlloc :: LValue -> Span -> Either Error StmtAlloc
mkStmtAlloc lValue span = do
  let (Type2 dims type1) = lvType lValue
  let declSpan           = lvDeclSpan lValue
  unless (null dims)
    $ Left --
    $ errArrayNotAllowed span declSpan
  unless
      (case type1 of
        TypeUser{} -> True
        _          -> False
      )
    $ Left --
    $ errExpectedUserType type1 span declSpan
  return $ MkStmtAlloc lValue

mkStmtNew :: LValue -> Type1 -> Span -> Either Error StmtNew
mkStmtNew lValue targetType span = do
  let (Type2 dims type1) = lvType lValue
  let declSpan           = lvDeclSpan lValue
  unless (null dims)
    $ Left --
    $ errArrayNotAllowed span declSpan
  targetTypeInhChain <- case targetType of
                          TypeUser inhChain -> return inhChain
                          _ -> Left $ Error.customError ("Expected class. Got: " ++ (show type1)) span
  unless 
      (case type1 of
        TypeUser inhChain -> (head inhChain) `elem` targetTypeInhChain
        _          -> False
      )
    $ Left $ Error.customError ("Type mismatch2. Expected: " ++ (show type1) ++ "Got: " ++ (show targetType)) span
  return $ MkStmtNew lValue targetType

mkStmtFree :: LValue -> Span -> Either Error StmtFree
mkStmtFree lValue span = do
  let (Type2 dims type1) = lvType lValue
  let declSpan           = lvDeclSpan lValue
  unless (null dims)
    $ Left --
    $ errArrayNotAllowed span declSpan
  unless
      (case type1 of
        TypeUser{} -> True
        _          -> False
      )
    $ Left --
    $ errExpectedUserType type1 span declSpan
  return $ MkStmtFree lValue

mkStmtPoke :: SpanW RValue -> SpanW RValue -> Span -> Either Error StmtPoke
mkStmtPoke rValue1SW rValue2SW span = do
  let rValue1 = spanWVal rValue1SW
  let rValue2 = spanWVal rValue2SW
  typeCheck1 (rValueType rValue1) [TypeInt] (getSpan rValue1SW)
  let (Type2 dims2 _) = rValueType rValue2
  unless (null dims2)
    $ Left --
    $ errArrayNotAllowed span span
  return $ MkStmtPoke rValue1 rValue2

mkLValueSymbol
  :: SpanW String
  -> [SpanW RValue]
  -> SymTabLocal
  -> SymTabGlobal
  -> Either Error LValue
mkLValueSymbol (SpanW identName identSpan) indices lSyms gSyms = do
  case (symLookup identName lSyms, symLookup identName gSyms) of
    (Just sym, _) -> do
      indices <- checkIndices sym indices
      let Type2 dims type1 = symType sym
      let lvType           = Type2 (drop (length indices) dims) type1
      return LValueLocal
        { lvName     = identName
        , lvIndices  = indices
        , lvType
        , lvDeclSpan = symDeclSpan sym
        }
    (Nothing, Just sym) -> do
      indices <- checkIndices sym indices
      let Type2 dims type1 = symType sym
      let lvType           = Type2 (drop (length indices) dims) type1
      return LValueGlobal
        { lvName     = identName
        , lvIndices  = indices
        , lvType
        , lvDeclSpan = symDeclSpan sym
        }
    (Nothing, Nothing) -> do
      Left $ errIdentifierNotDeclared identName identSpan
 where
  checkIndices :: Symbol -> [SpanW RValue] -> Either Error [RValue]
  checkIndices sym indices = do
    let Type2 dims _ = symType sym
    unless (length indices <= length dims)
      $ Left (Error.customError "Too much indices" identSpan) -- TODO Better error
    mapM
      (\(SpanW index indexSpan) -> do
        typeCheck1 (rValueType index) [TypeInt] indexSpan
        return index
      )
      indices

mkLValueField
  :: LValue
  -> SpanW String
  -> [SpanW RValue]
  -> [UserType]
  -> Either Error LValue
mkLValueField lValue (SpanW fieldName fieldNameSpan) indices userTypes =
  do
    utName <- getUserTypeName $ lvType lValue
    let
      userType = fromJust $ userTypeLookup utName userTypes
      isSelf   = case lValue of
        LValueSelf{} -> True
        _            -> False
    unless
      (isSelf || utFieldsVisibility userType == SVPublic)
      (Left $ Error.customError
        ("Fields are private for: " ++ utName)
        fieldNameSpan
      )
    sym <- symLookup fieldName (utFields userType) & \case
      -- TODO Better error
      Nothing  -> Left $ errIdentifierNotDeclared fieldName fieldNameSpan
      Just sym -> return sym
    let Type2 dims _ = symType sym
    unless (length indices <= length dims)
      $ Left (Error.customError "Too much indices" fieldNameSpan) -- TODO Better error
    indices' <- mapM
      (\(SpanW index indexSpan) -> do
        typeCheck1 (rValueType index) [TypeInt] indexSpan
        return index
      )
      indices
    let Type2 dims type1 = symType sym
    let lvType           = Type2 (drop (length indices) dims) type1
    return $ LValueField
      { lvName           = fieldName
      , lvIndices        = indices'
      , lvType
      , lvDeclSpan       = symDeclSpan sym
      , lvParent         = lValue
      , lvParentTypeName = utName
      }
 where
  getUserTypeName typ = case typ of
    Type2 [] (TypeUser (utName:_)) -> return utName
    Type2 _  _                 -> Left
      (Error.customError
        ("Expected UserType, Got: " ++ show typ)
        fieldNameSpan -- TODO Better error
      )

mkExpArithmetic
  :: SpanW RValue -> OpArithmetic -> SpanW RValue -> Either Error Exp
mkExpArithmetic (SpanW r1 span1) op (SpanW r2 span2) = do
  typeCheck1 (rValueType r1) ( [TypeInt]) span1
  typeCheck1 (rValueType r2) ( [TypeInt]) span2
  return $ MkExpArithmetic r1 op r2

mkExpRelational
  :: SpanW RValue -> OpRelational -> SpanW RValue -> Either Error Exp
mkExpRelational (SpanW r1 span1) op (SpanW r2 span2) = do
  let dataType1       = rValueType r1
  let dataType2       = rValueType r2
  let (Type2 dims1 _) = dataType1
  let (Type2 dims2 _) = dataType2
  unless (null dims1)
    $ Left --
    $ errArrayNotAllowed span1 span1 -- TODO DeclSpan
  unless (null dims2)
    $ Left --
    $ errArrayNotAllowed span2 span2 -- TODO DeclSpan
  typeCheck dataType1 dataType2 span2
  return $ MkExpRelational r1 op r2

mkExpLogical
  :: SpanW RValue -> OpLogical -> SpanW RValue -> Either Error Exp
mkExpLogical (SpanW r1 span1) op (SpanW r2 span2) = do
  typeCheck1 (rValueType r1) ( [TypeBool]) span1
  typeCheck1 (rValueType r2) ( [TypeBool]) span2
  return $ MkExpLogical r1 op r2

doFuncDeclare
  :: Type1
  -> String
  -> [SpanW Type1]
  -> Span
  -> [Func]
  -> Either Error [Func]
doFuncDeclare retType funcName argTypes span funcs = do
  funcLookup funcName funcs & throwFuncRedeclared
  let
    func = FuncDecl
      { fDeclName     = funcName
      , fDeclRetType  = retType
      , fDeclArgTypes = argTypes
      , fDeclSpan     = span
      }
  return $ funcInsert (FuncDeclared func) funcs
 where
  throwFuncRedeclared func = case func of
    Nothing -> return ()
    Just (FuncDeclared funcDec) ->
      Left $ errFuncRedeclared span (fDeclSpan funcDec)
    Just (FuncDefined funcDef) ->
      Left $ errFuncRedeclared span (fDefDeclSpan funcDef)

doFuncDefine
  :: Type1
  -> String
  -> [SpanW (String, Type1)]
  -> Span
  -> [Func]
  -> Either
       Error
       ( [Func]
       , FuncDecl
       , SymTabLocal
       , SymTabLocal -> [Stmt] -> [Func]
       )
doFuncDefine retType name args span funcs = do
  let fDecl0 = funcLookup name funcs
  funcs <- if isNothing fDecl0
    then doFuncDeclare retType name (fmap (fmap snd) args) span funcs
    else return funcs
  fDecl <-
    throwRedefined (fromJust (funcLookup name funcs))
      >>= throwMismatch retType name args
  syms <- foldlM
    (\syms (SpanW (name, primType) span) ->
      doVarDeclare name primType [] span syms
    )
    (SymTabLocal [])
    args
  return $ (funcs, fDecl, syms, ) $ \syms stmts -> do
    let
      fDef = FuncDef
        { fDefName     = fDeclName fDecl
        , fDefRetType  = fDeclRetType fDecl
        , fDefArgTypes = fDeclArgTypes fDecl
        , fDefDeclSpan = fDeclSpan fDecl
        , fDefArgsLen  = length args
        , fDefSyms     = syms
        , fDefBody     = stmts
        , fDefSpan     = span
        }
    funcInsert (FuncDefined fDef) funcs
 where
  throwRedefined :: Func -> Either Error FuncDecl
  throwRedefined = \case
    (FuncDefined  fDef ) -> Left $ errFuncRedefined span (fDefSpan fDef)
    (FuncDeclared fDecl) -> return fDecl
  throwMismatch
    :: Type1
    -> String
    -> [SpanW (String, Type1)]
    -> FuncDecl
    -> Either Error FuncDecl
  throwMismatch retType name args fDecl = do
    unless
        (  fDeclName fDecl
        == name
        && fDeclRetType fDecl
        == retType
        && (spanWVal <$> fDeclArgTypes fDecl)
        == (snd . spanWVal <$> args)
        )
      $ Left
      $ errFuncDefMismatch span (fDeclSpan fDecl)
    return fDecl

mkExpFuncCall
  :: String -> [SpanW RValue] -> Span -> [Func] -> Either Error RValue
mkExpFuncCall funcName args span funcs = do
  fDecl <- throwFuncNotDeclared $ fDeclLookup funcName funcs
  unless (length args == length (fDeclArgTypes fDecl))
    $ Left
    $ errFuncArgLengthMismatch
        (length $ fDeclArgTypes fDecl)
        (length args)
        (fDeclSpan fDecl)
        span
  args' <-
    mapM
        (\(SpanW arg argSpan, SpanW expectedType _expectedTypeSpan) -> do
          let argType                  = rValueType arg
          typeCheck (Type2 [] expectedType) argType argSpan
          return arg
        )
      $ zip args (fDeclArgTypes fDecl)
  return $ RFuncCall funcName args' (fDeclRetType fDecl)
 where
  throwFuncNotDeclared sym = case sym of
    Nothing  -> Left $ errFuncNotDeclared funcName span
    Just sym -> return sym


mkExpMethodCall
  :: SpanW LValue
  -> String
  -> [SpanW RValue]
  -> Span
  -> [UserType]
  -> Either Error RValue
mkExpMethodCall (SpanW lValue lValueSpan) funcName args span userTypes =
  do
    utName <- getUserTypeName $ lvType lValue
    fDecl  <- throwFuncNotDeclared $ getFDecl funcName utName userTypes
    unless (length args == length (fDeclArgTypes fDecl))
      $ Left
      $ errFuncArgLengthMismatch
          (length $ fDeclArgTypes fDecl)
          (length args)
          (fDeclSpan fDecl)
          span
    args' <-
      mapM
          (\(SpanW arg argSpan, SpanW expectedType _expectedTypeSpan) -> do
            let argType                  = rValueType arg
            typeCheck (Type2 [] expectedType) argType argSpan
            return arg
          )
        $ zip args (fDeclArgTypes fDecl)
    return $ RMethodCall lValue funcName args' (fDeclRetType fDecl)
 where
  throwFuncNotDeclared sym = case sym of
    Nothing  -> Left $ errFuncNotDeclared funcName span
    Just sym -> return sym
  getFDecl funcName utName userTypes =
    let userType = fromJust $ userTypeLookup utName userTypes
    in
      fDeclLookup funcName (utFuncs userType)
        <|> (do
              utName <- utParentName userType
              getFDecl funcName utName userTypes
            )
  getUserTypeName typ = case typ of
    Type2 [] (TypeUser (utName:_)) -> return utName
    Type2 _  _                 -> Left
      (Error.customError
        ("Expected UserType, Got: " ++ show typ)
        lValueSpan -- TODO Better error
      )



{-
doTypeDefine
  :: SpanW String
  -> [UserType]
  -> Either
       Error
       (  [UserType]
       ,  [Symbol]
       -> Span
       -> [UserType]
       -> Either Error [UserType]
       )
doTypeDefine (SpanW name nameSpan) userTypes = do
  throwExistingType $ userTypeLookup name userTypes
  let
    userType =
      UserType { utName = name, utFields = [], utDeclSpan = nameSpan }
  let userTypes' = userTypeInsert userType userTypes
  return $ (userTypes', ) $ \fields span userTypes -> do
    let
      userType =
        UserType { utName = name, utFields = fields, utDeclSpan = span }
    return $ userTypeInsert userType userTypes
 where
  throwExistingType maybeType = case maybeType of
    Just userType ->
      Left $ errTypeRedeclared name (utDeclSpan userType) nameSpan
    Nothing -> return ()
-}

mkType1 :: SpanW String -> [UserType] -> Either Error Type1
mkType1 name' userTypes =
  let
    name = spanWVal name'
    span = getSpan name'
  in case name of
    "int"  -> return TypeInt
    "str"  -> return TypeString
    "bool" -> return TypeBool
    "any"  -> return TypeAny
    _      -> do
      userType <- userTypeLookup name userTypes & throwTypeDoesnotExist name span
      let inhChain = getInhChain userType userTypes
      return $ TypeUser inhChain
 where
  throwTypeDoesnotExist name span maybeType = case maybeType of
    Just x  -> return x
    Nothing -> Left $ errTypeDoesnotExist name span
  getInhChain userType userTypes =
    case (utParentName userType) of
      Nothing ->
        [utName userType]
      Just parentName ->
        let parentType = fromJust $ userTypeLookup parentName userTypes
         in (utName userType) : (getInhChain parentType userTypes)



mkExpSyscall
  :: SpanW Int
  -> SpanW Int
  -> SpanW RValue
  -> SpanW RValue
  -> SpanW RValue
  -> Span
  -> RValue
mkExpSyscall intNum callNum arg1 arg2 arg3 _span = RSyscall
  (spanWVal intNum)
  (spanWVal callNum)
  (spanWVal arg1)
  (spanWVal arg2)
  (spanWVal arg3)

mkExpPeek :: SpanW RValue -> Either Error RValue
mkExpPeek rValueSW = do
  let
    rValue = spanWVal rValueSW
    span   = getSpan rValueSW
  typeCheck1 (rValueType rValue) [TypeInt] span
  return $ RPeek rValue

-- Helper Functions



rValueType :: RValue -> Type2
rValueType (RLValue v  )             = lvType v
rValueType (RExp    exp)             = expType exp
rValueType RSyscall{}                = (Type2 [] TypeAny)
rValueType RPeek{}                   = (Type2 [] TypeAny)
rValueType (RFuncCall _ _ type1    ) = Type2 [] type1
rValueType (RMethodCall _ _ _ type1) = Type2 [] type1
rValueType (RNull) = Type2 [] TypeAny

expType :: Exp -> Type2
expType exp = case exp of
  ExpNum{}          -> Type2 [] TypeInt
  ExpStr{}          -> Type2 [] TypeString
  MkExpArithmetic{} -> Type2 [] TypeInt
  MkExpRelational{} -> Type2 [] TypeBool
  MkExpLogical{}    -> Type2 [] TypeBool

-- Types

instance Show Type1 where
  show TypeInt             = "int"
  show TypeString          = "str"
  show TypeBool            = "bool"
  show (TypeUser inhChain) = show inhChain
  show TypeAny             = "any"

instance Show Type2 where
  show (Type2 dims primType) =
    let s = show primType
    in s ++ concatMap (\n -> "[" ++ show n ++ "]") dims

-- Utilities

insertList :: Eq a => (t -> a) -> t -> [t] -> [t]
insertList key a list = case list of
  [] -> [a]
  (a' : as') ->
    if key a == key a' then a : as' else a' : insertList key a as'



funcLookup :: String -> [Func] -> Maybe Func
funcLookup name = find $ \f -> funcName f == name

userTypeLookup :: String -> [UserType] -> Maybe UserType
userTypeLookup name = find (\t -> utName t == name)

userTypeInsert :: UserType -> [UserType] -> [UserType]
userTypeInsert = insertList utName

funcName :: Func -> String
funcName (FuncDeclared FuncDecl { fDeclName }) = fDeclName
funcName (FuncDefined  FuncDef { fDefName }  ) = fDefName

funcInsert :: Func -> [Func] -> [Func]
funcInsert = insertList funcName

fDeclLookup :: String -> [Func] -> Maybe FuncDecl
fDeclLookup name funcs = do
  func <- funcLookup name funcs
  case func of
    FuncDeclared fDecl -> return $ fDecl
    FuncDefined FuncDef { fDefName, fDefRetType, fDefArgTypes, fDefDeclSpan }
      -> return $ FuncDecl
        { fDeclName     = fDefName
        , fDeclRetType  = fDefRetType
        , fDeclArgTypes = fDefArgTypes
        , fDeclSpan     = fDefDeclSpan
        }


typeCheck :: Type2 -> Type2 -> Span -> Either Error ()
typeCheck lhsType rhsType span = do 
  unless
    (case (lhsType, rhsType) of
      (Type2 _ TypeAny, _      ) -> True
      (_      , Type2 _ TypeAny) -> True
      (Type2 [] (TypeUser inhChainL), Type2 [] (TypeUser inhChainR)) ->
        (head inhChainL) `elem` inhChainR
      (x      , y      ) -> x == y
    )
    (Left $ Error.customError
      (  "Type mismatch: Expected "
      ++ (show lhsType)
      ++ ". Got: "
      ++ (show rhsType)
      )
      span
    )

typeCheck1 :: Type2 -> [Type1] -> Span -> Either Error ()
typeCheck1 type2@(Type2 dims type1) allowedTypes span = do
  unless (dims == [] && (type1 == TypeAny || type1 `elem` allowedTypes))
    (Left $ Error.customError
      (  "Type mismatch1: Expected "
      ++ (show allowedTypes)
      ++ ". Got: "
      ++ (show type2)
      )
      span
    )

-- Errors
errIdentifierNotDeclared :: String -> Span -> Error
errIdentifierNotDeclared identName span =
  [("Identifier not declared: " ++ identName, span)]

errIdentifierRedeclared :: String -> Span -> Span -> Error
errIdentifierRedeclared identName declSpan span =
  [ ("Identifier redeclared: " ++ identName, span)
  , ("Was already declared here"           , declSpan)
  ]

errTypeMismatch :: Type2 -> Span -> Type2 -> Span -> Error
errTypeMismatch identType declSpan rhsType span =
  [ ( "Type mismatch: Expected "
      ++ show identType
      ++ ". Got "
      ++ show rhsType
    , span
    )
  , ("Was declared here", declSpan)
  ]

errTypeNotAllowed :: [Type2] -> Type2 -> Span -> Error
errTypeNotAllowed allowedTypes gotType span =
  [ ( "Type not allowed: "
      ++ show gotType
      ++ ". Allowed types: "
      ++ show allowedTypes
    , span
    )
  ]

errFuncNotDeclared :: String -> Span -> Error
errFuncNotDeclared funcName span =
  [("Function not declared: " ++ funcName, span)]

errFuncNotDefined :: String -> Span -> Error
errFuncNotDefined funcName span =
  [("Function declared here, but not defined: " ++ funcName, span)]

errFuncRedeclared :: Span -> Span -> Error
errFuncRedeclared span declSpan =
  [("Function redeclared", span), ("Was already declared here", declSpan)]

errFuncRedefined :: Span -> Span -> Error
errFuncRedefined span defSpan =
  [("Function redefined", span), ("Was already defined here", defSpan)]

errFuncArgLengthMismatch :: Int -> Int -> Span -> Span -> Error
errFuncArgLengthMismatch expLength gotLength declSpan span =
  [ ( "Mismatch in function argument length. Expected: "
      ++ show expLength
      ++ ". Got: "
      ++ show gotLength
    , span
    )
  , ("Was declared here", declSpan)
  ]

errFuncDefMismatch :: Span -> Span -> Error
errFuncDefMismatch span declSpan =
  [ ("Function definition and declarations are different", span)
  , ("Was declared here", declSpan)
  ]

errArrayNotAllowed :: Span -> Span -> Error
errArrayNotAllowed span declSpan =
  [ ("An array is not allowed in this position", span)
  , ("Was declared here"                       , declSpan)
  ]

errTypeRedeclared :: String -> Span -> Span -> Error
errTypeRedeclared typeName declSpan span =
  [ ("Type redeclared: " ++ typeName, span)
  , ("Was already declared here"    , declSpan)
  ]

errTypeDoesnotExist :: String -> Span -> Error
errTypeDoesnotExist typeName span =
  [("Type doesnot exist: " ++ typeName, span)]

errExpectedUserType :: Type1 -> Span -> Span -> Error
errExpectedUserType t span declSpan =
  [ ("Expected user type. Actual type: " ++ show t, span)
  , ("Was declared here"                          , declSpan)
  ]
