{
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import qualified Lexer
import Frontend
import Grammar
import Token
import Span
import Error (Error)
import qualified Error

import Text.Printf
import Control.Monad.Error
import Data.Functor
import Data.Tuple.Extra
import Debug.Trace

}

%name parse
%monad {Frontend}
%lexer {Lexer.lexer} {TokenEOF _}
%tokentype {Token}
%error {parseError}

%token
    number   {TokenNumber    $$}
    strlit   {TokenStrLit    $$}
    ident    {TokenIdent     $$}

    '+'      {TokenPlus      _ }
    '-'      {TokenMinus     _ }
    '*'      {TokenStar      _ }
    '/'      {TokenFwdSlash  _ }
    '%'      {TokenPercent   _ }

    '('      {TokenParOpen   _ }
    ')'      {TokenParClose  _ }

    '{'      {TokenBraceOpen _ }
    '}'      {TokenBraceClose _}

    '['      {TokenSqBracketOpen _ }
    ']'      {TokenSqBracketClose _}

    ';'      {TokenSemiColon _ }
    ','      {TokenComma     _ }
    '='      {TokenEquals    _ }

    '<'      {TokenLT        _ }
    '=='     {TokenEQ        _ }
    '!='     {TokenNE        _ }
    '>'      {TokenGT        _ }
    '<='     {TokenLE        _ }
    '>='     {TokenGE        _ }

    '&&'     {TokenLAnd      _ }
    '||'     {TokenLOr       _ }

    read     {TokenRead      _ }
    write    {TokenWrite     _ }

    if       {TokenIf        _ }
    then     {TokenThen      _ }
    else     {TokenElse      _ }
    endif    {TokenEndIf     _ }
    while    {TokenWhile     _ }
    endwhile {TokenEndWhile  _ }
    do       {TokenDo        _ }
    break    {TokenBreak     _ }
    continue {TokenContinue  _ }
    return   {TokenReturn    _ }

    type     {TokenType     _ }
    syscall  {TokenSyscall  _ }

%nonassoc '='
%left '&&' '||'
%nonassoc '==' '<' '>' '<=' '>=' '!='
%left '+' '-'
%left '*' '/' '%'

%%

Program :: {Program}
Program:
      TSlist            {% mkProgram }

TopLevelStmt :: {()}
TopLevelStmt:
      DoVarDeclare      {()}
    | DoFuncDeclare     {()}
    | DoFuncDefine      {()}
    | DoTypeDefine      {()}
    | TopLevelStmt ';'  {$1}

TSlist :: {()}
TSlist:
      TopLevelStmt      {()}
    | TSlist TopLevelStmt
                        {()}

Slist :: {[Stmt]}
Slist:
      Slist Stmt        { $1 ++ $2 }
    | Stmt              { $1 }

Stmt :: {[Stmt]}
Stmt:
      DoVarDeclare      { [] }
    | StmtAssign        { [StmtAssign $1] }
    | StmtRead          { [StmtRead $1] }
    | StmtWrite         { [StmtWrite $1] }
    | StmtIf            { [StmtIf $1] }
    | StmtIfElse        { [StmtIfElse $1] }
    | StmtWhile         { [StmtWhile $1] }
    | StmtBreak         { [StmtBreak $1] }
    | StmtContinue      { [StmtContinue $1] }
    | StmtReturn        { [StmtReturn $1] }
    | StmtRValue        { [StmtRValue $1] }
    | StmtSyscall       { [StmtSyscall $1] }
    | Stmt ';'          {$1}

DoVarDeclare :: {()}
DoVarDeclare:
      Type1 IdentDims ';'
                        {% mapM_ (\x ->
                            doVarDeclare
                              (fst (spanWVal x))
                              (spanWVal $1)
                              (snd (spanWVal  x))
                              (getSpanBwn $1 $3))
                            $2 }

IdentDims :: {[SpanW (String, [Int])]}
IdentDims:
      ident Dims        { [SpanW (spanWVal $1, spanWVal $2) (getSpanBwn $1 $2)] }
    | IdentDims ',' ident Dims
                        { $1 ++ [SpanW (spanWVal $3, spanWVal $4) (getSpanBwn $3 $4)] }

Dims :: {SpanW [Int]}
Dims:
    {- empty -}         { SpanW ([]) (Span 0 0) }
  | Dims '[' number ']' { SpanW (spanWVal $1 ++ [spanWVal $3]) (getSpanBwn $1 $4) }

StmtAssign :: {StmtAssign}
StmtAssign:
    LValue '=' RValue ';'
                        {% mkStmtAssign
                            (spanWVal $1)
                            (spanWVal $3)
                            (getSpanBwn $1 $3)}

StmtRead :: {StmtRead}
StmtRead:
    read '(' LValue ')' ';'
                        {% mkStmtRead $3 }

StmtWrite :: {StmtWrite}
StmtWrite:
    write '(' RValue ')' ';'
                        {% mkStmtWrite $3 }

StmtIf :: {StmtIf}
StmtIf:
      if '(' RValue ')'
      then
        Slist
      endif             {% mkStmtIf $3 $6 }

StmtIfElse :: {StmtIfElse}
StmtIfElse:
      if '(' RValue ')' then
        Slist
      else
        Slist
      endif             {% mkStmtIfElse $3 $6 $8 }

StmtWhileEnter :: {SpanW RValue}
StmtWhileEnter:
      while '(' RValue ')' do
                        {% pushLoop >> return $3 }

StmtWhile :: {StmtWhile}
StmtWhile:
      StmtWhileEnter
        Slist
      endwhile          {% popLoop >> mkStmtWhile $1 $2 }

StmtBreak :: {StmtBreak}
StmtBreak:
      break ';'         {% mkStmtBreak (getSpan $1)}

StmtContinue :: {StmtContinue}
StmtContinue:
      continue ';'      {% mkStmtContinue (getSpan $1)}

StmtRValue :: {StmtRValue}
StmtRValue:
      RValue ';'      {% mkStmtRValue (spanWVal $1) }

StmtReturn :: {StmtReturn}
StmtReturn:
      return RValue ';'      {% mkStmtReturn (spanWVal $2) (getSpan $2) }

StmtSyscall :: {StmtSyscall}
StmtSyscall:
      syscall '(' number ',' number ',' RValue ',' RValue ',' RValue ')' ';'
                             {% mkStmtSyscall $3 $5 $7 $9 $11 (getSpanBwn $1 $13) }

FunctionArg :: {SpanW (String, Type1)}
FunctionArg:
    Type1 ident
                        { SpanW (spanWVal $2, spanWVal $1) (getSpanBwn $1 $2) }

FunctionArgList :: {[SpanW (String, Type1)]}
FunctionArgList:
      FunctionArg
                        { [$1] }
    | FunctionArgList ',' FunctionArg
                        { $1 ++ [$3] }

DoFuncDeclare :: {()}
DoFuncDeclare:
      Type1 ident '(' ')' ';'
                        {% doFuncDeclare
                            (spanWVal $1)
                            (spanWVal $2)
                            []
                            (getSpanBwn $1 $5) }
    | Type1 ident '(' FunctionArgList ')' ';'
                        {% doFuncDeclare
                            (spanWVal $1)
                            (spanWVal $2)
                            (fmap (fmap snd) $4)
                            (getSpanBwn $1 $6) }

FuncDefineEnter:
      Type1 ident '(' ')'
                        {% doFuncDefine (spanWVal $1) (spanWVal $2) [] (getSpanBwn $1 $4) }
    | Type1 ident '(' FunctionArgList ')'
                        {% doFuncDefine (spanWVal $1) (spanWVal $2) $4 (getSpanBwn $1 $5) }

DoFuncDefine :: {()}
DoFuncDefine:
      FuncDefineEnter '{' Slist '}'
                        {% $1 $3 }

LValue :: {SpanW LValue}
LValue:
      ident             {% mkLValue $1 [] <&> flip SpanW (getSpan $1)}
    | ident Indices     {% mkLValue $1 (spanWVal $2) <&> flip SpanW (getSpanBwn $1 $2)}

Indices :: {SpanW [SpanW RValue]}
Indices:
      '[' RValue ']'    { SpanW ([$2]) (getSpanBwn $1 $3) }
    | Indices '[' RValue ']'
                        { SpanW (spanWVal $1 ++ [$3]) (getSpanBwn $1 $4) }

RValue :: {SpanW RValue}
RValue:
      LValue            { fmap RLValue $1 }
    | Exp               { fmap RExp $1 }
    | ident '(' ')'     {% mkExpFuncCall (spanWVal $1) [] (getSpanBwn $1 $3)
                                            <&> flip SpanW (getSpanBwn $1 $3) }
    | ident '(' RValues ')'
                        {% mkExpFuncCall (spanWVal $1) $3 (getSpanBwn $1 $4)
                                            <&> flip SpanW (getSpanBwn $1 $4) }

RValues :: {[SpanW RValue]}
RValues:
      RValue                {[$1]}
    | RValue ',' RValues    {$1:$3}

Exp :: {SpanW Exp}
Exp:
     number             { fmap ExpNum $1 }
   | strlit             { fmap ExpStr $1 }
   | '(' Exp ')'        { $2 }
   | RValue '+' RValue  {% (mkExpArithmetic $1 OpAdd $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '-' RValue  {% (mkExpArithmetic $1 OpSub $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '*' RValue  {% (mkExpArithmetic $1 OpMul $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '/' RValue  {% (mkExpArithmetic $1 OpDiv $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '%' RValue  {% (mkExpArithmetic $1 OpMod $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '<' RValue  {% (mkExpRelational $1 OpLT  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '>' RValue  {% (mkExpRelational $1 OpGT  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '<=' RValue {% (mkExpRelational $1 OpLE  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '>=' RValue {% (mkExpRelational $1 OpGE  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '==' RValue {% (mkExpRelational $1 OpEQ  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '!=' RValue {% (mkExpRelational $1 OpNE  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '&&' RValue {% (mkExpLogical $1 OpLAnd  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '||' RValue {% (mkExpLogical $1 OpLOr  $3) <&> flip SpanW (getSpanBwn $1 $3)}

Type1 :: {SpanW Type1}
Type1:
    ident               {% mkType1 $1 <&> flip SpanW (getSpan $1) }

DoTypeDefine :: {()}
DoTypeDefine:
    type ident '{' FieldList '}'
                        {% doTypeDefine (spanWVal $2) $4 (getSpanBwn $1 $2) }

Field :: {[SpanW (String, Type2)]}
Field:
    Type1 IdentDims ';' {   
                            map (fmap (second (\d -> Type2 d (spanWVal $1)))) $2
                        }

FieldList :: {[SpanW (String, Type2)]}
FieldList:
    Field               {$1}
  | Field FieldList     {$1 ++ $2}



{
dbgs s v = trace (s ++ ": " ++ show v) v

parseError :: Token -> Frontend a
parseError token = throwError $ Error.customError ("Parsing error 2: " ++ (show token)) $ getSpan token

fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
thd4 (a,b,c,d) = c
frth4 (a,b,c,d) = d

}
