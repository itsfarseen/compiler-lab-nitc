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

}

%name parse
%monad {Frontend}
%lexer {Lexer.lexer} {TokenEOF _}
%tokentype {Token}
%error {parseError}

%token
    number   {TokenNumber    $$}
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
    '='      {TokenEquals    _ }

    '<'      {TokenLT        _ }
    '=='     {TokenEQ        _ }
    '!='     {TokenNE        _ }
    '>'      {TokenGT        _ }
    '<='     {TokenLE        _ }
    '>='     {TokenGE        _ }

    begin    {TokenBegin     _ }
    end      {TokenEnd       _ }

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
    int      {TokenInt       _ }
    bool     {TokenBool      _ }
    string   {TokenString    _ }

%nonassoc '=' '==' '<' '>' '<=' '>=' '!='
%left '+' '-'
%left '*' '/' '%'

-- Intermixing of `if (..) then ..` and `if ( .. ) { .. }` are disallowed.
-- This can lead to ambiguities: `if (..) then if ( .. ) { .. } else .. endif`
%nonassoc '{' '}'
%nonassoc else

%%

Program:
      begin Slist end       {Program {stmts=$2}}
    | begin end             {Program {stmts=[]}}

Slist:
      Slist Stmt            {$1 ++ [$2]}
    | Stmt                  {[$1]}

Stmt:
      StmtDeclare           {StmtDeclare $1}
    | StmtAssign            {StmtAssign $1}
    | StmtRead              {StmtRead $1}
    | StmtWrite             {StmtWrite $1}
    | StmtIf                {StmtIf $1}
    | StmtIfElse            {StmtIfElse $1}
    | StmtWhile             {StmtWhile $1}
    | StmtDoWhile           {StmtDoWhile $1}
    | StmtBreak             {StmtBreak $1}
    | StmtContinue          {StmtContinue $1}
    | Stmt ';'              {$1}

StmtDeclare:
    DataType ident ';'       {% mkStmtDeclare (spanWVal $2) (spanWVal $1) (getSpanBwn $1 $3)}

StmtAssign:
    LValue '=' RValue ';'    {% mkStmtAssign (spanWVal $1) (spanWVal $3) (getSpanBwn $1 $3)}

StmtRead:
    read '(' LValue ')' ';'  {% mkStmtRead $3 }

StmtWrite:
    write '(' RValue ')' ';' {% mkStmtWrite $3 }

StmtIf:
      if '(' RValue ')' 
      then 
        Slist 
      endif                {% mkStmtIf $3 $6 }

    | if '(' RValue ')' '{' 
        Slist 
     '}'                   {% mkStmtIf $3 $6 }
                        
StmtIfElse:
      if '(' RValue ')' then 
        Slist 
      else 
        Slist 
      endif                {% mkStmtIfElse $3 $6 $8 }

    | if '(' RValue ')' '{' 
        Slist 
      '}' else '{' 
        Slist 
      '}'                  {% mkStmtIfElse $3 $6 $10 }

StmtWhileEnter1:
      while '(' RValue ')' do {% pushLoopStack >> return $3 }

StmtWhileExit1:
      endwhile             {% popLoopStack >> return $1 }

StmtWhileEnter2:
      while '(' RValue ')' '{' {% pushLoopStack >> return $3 }

StmtWhileExit2:
      '}'                   {% popLoopStack >> return $1 }

StmtWhile:
      StmtWhileEnter1
        Slist 
      StmtWhileExit1      {% mkStmtWhile $1 $2 }

    | StmtWhileEnter2
        Slist 
      StmtWhileExit2      {% mkStmtWhile $1 $2 }

StmtDoWhileEnter:
      do '{'              {% pushLoopStack >> return $1 }

StmtDoWhileExit:
      '}' while '(' RValue ')' {% popLoopStack >> return $4 }

StmtDoWhile:
      StmtDoWhileEnter
        Slist 
      StmtDoWhileExit     {% mkStmtDoWhile $3 $2 }

StmtBreak:
      break ';'           {% mkStmtBreak (getSpan $1)}

StmtContinue:
      continue ';'        {% mkStmtContinue (getSpan $1)}

LValue:
      Ident                 {fmap LValueIdent $1}
    | LValue '[' RValue ']' {% (mkArrayIndex $1 $3) <&> flip SpanW (getSpanBwn $1 $4)}

RValue:
      LValue              {fmap LValue $1}
    | Exp                 {fmap Exp $1}

Ident:
     ident                {% (mkIdent $1) <&> flip SpanW (getSpan $1)}

Exp: 
     number               {fmap ExpNum $1}
   | '(' Exp ')'          {$2}
   | RValue '+' RValue    {% (mkExpArithmetic $1 OpAdd $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '-' RValue    {% (mkExpArithmetic $1 OpSub $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '*' RValue    {% (mkExpArithmetic $1 OpMul $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '/' RValue    {% (mkExpArithmetic $1 OpDiv $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '%' RValue    {% (mkExpArithmetic $1 OpMod $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '<' RValue    {% (mkExpLogical $1 OpLT  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '>' RValue    {% (mkExpLogical $1 OpGT  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '<=' RValue    {% (mkExpLogical $1 OpLE  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '>=' RValue    {% (mkExpLogical $1 OpGE  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '==' RValue    {% (mkExpLogical $1 OpEQ  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '!=' RValue    {% (mkExpLogical $1 OpNE  $3) <&> flip SpanW (getSpanBwn $1 $3)}

-- ExpArrayIndex:

DataType:
    int                     {SpanW DataTypeInt (getSpan $1)}
  | bool                    {SpanW DataTypeBool (getSpan $1)}
  | DataType '[' number ']' {SpanW (dataTypeReverseAddDim (spanWVal $1) (spanWVal $3)) (getSpanBwn $1 $4)}

{

dataTypeReverseAddDim :: DataType -> Int -> DataType
dataTypeReverseAddDim dataType size = case dataType of
  DataTypeInt  -> DataTypeArray size dataType
  DataTypeBool -> DataTypeArray size dataType
  DataTypeArray size' inner ->
    DataTypeArray size' (dataTypeReverseAddDim inner size)

parseError :: Token -> Frontend a
parseError token = throwError $ Error.customError ("Parsing error 2: " ++ (show token)) $ getSpan token

}