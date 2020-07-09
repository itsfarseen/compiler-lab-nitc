{
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lexer where

import Control.Monad.Except
import Codec.Binary.UTF8.String (decode)
import Data.Word (Word8)

import Token
import Frontend (AlexInput(..), AlexInputState(..))
import Span
import Error (Error)
import qualified Error

import Debug.Trace
}

$plus        = \+
$minus       = \-
$star        = \*
$fwd_slash   = \/
$percent     = \%

$lt          = \<
@eq          = "=="
@ne          = "!="
$gt          = \>
@lte         = "<="
@gte         = ">="
@l_and       = "&&"
@l_or        = "||"

$equals      = \=

$semi_colon  = \;
$comma       = \,
$dot         = \.
$par_open    = \(
$par_close   = \)
$brace_open  = \{
$brace_close = \}
$sq_bracket_open  = \[
$sq_bracket_close = \]

@read        = "read"
@write       = "write"
@if          = "if"
@then        = "then"
@else        = "else"
@endif       = "endif"
@while       = "while"
@endwhile    = "endwhile"
@do          = "do"
@break       = "break"
@continue    = "continue"
@return      = "return"
@type        = "type"
@syscall     = "syscall"
@initialize  = "initialize"
@alloc       = "alloc"
@free        = "free"
@peek        = "peek"
@poke        = "poke"
@class       = "class"
@extends     = "extends"
@null        = "null"
@new         = "new"
@self        = "self"
@breakpoint  = "breakpoint"

@number      = [0-9]+
@strlit      = \"[^\"]*\"
@ident       = [a-zA-Z_][a-zA-Z0-9_]*
@skip        = decl|enddecl|begin|end|endtype
@comment     = \/\/[^\n]*
@comment2    = \/\*[^\*]*\*\/

tokens :-
    $white+      ;
    @skip        ;
    @comment     ;
    @comment2    ;

    $plus        {token TokenPlus}
    $minus       {token TokenMinus}
    $star        {token TokenStar}
    $fwd_slash   {token TokenFwdSlash}
    $percent     {token TokenPercent}

    $lt          {token TokenLT}
    @eq          {token TokenEQ}
    @ne          {token TokenNE}
    $gt          {token TokenGT}
    @lte         {token TokenLE}
    @gte         {token TokenGE}

    @l_and       {token TokenLAnd}
    @l_or        {token TokenLOr}

    $equals      {token TokenEquals}

    $semi_colon  {token TokenSemiColon}
    $comma       {token TokenComma}
    $dot         {token TokenDot}
    $par_open    {token TokenParOpen}
    $par_close   {token TokenParClose}
    $brace_open  {token TokenBraceOpen}
    $brace_close {token TokenBraceClose}
    $sq_bracket_open  {token TokenSqBracketOpen}
    $sq_bracket_close {token TokenSqBracketClose}

    @read        {token TokenRead}
    @write       {token TokenWrite}
    @if          {token TokenIf}
    @then        {token TokenThen}
    @else        {token TokenElse}
    @endif       {token TokenEndIf}
    @while       {token TokenWhile}
    @endwhile    {token TokenEndWhile}
    @do          {token TokenDo}
    @break       {token TokenBreak}
    @continue    {token TokenContinue}
    @return      {token TokenReturn}
    @type        {token TokenType}
    @syscall     {token TokenSyscall}
    @initialize  {token TokenInitialize}
    @alloc       {token TokenAlloc}
    @free        {token TokenFree}
    @peek        {token TokenPeek}
    @poke        {token TokenPoke}
    @class       {token TokenClass}
    @extends     {token TokenExtends}
    @null        {token TokenNull}
    @new         {token TokenNew}
    @self        {token TokenSelf}
    @breakpoint  {token TokenBreakpoint}

    @number      {tokenInt   TokenNumber}
    @strlit      {tokenStr   TokenStrLit}
    @ident       {tokenIdent TokenIdent}

{
token action = \_ span -> action span
tokenStr action = \s span -> action $ SpanW (trimQuotes s) span
tokenIdent action = \s span -> action $ SpanW s span
tokenInt action = \s span -> action $ SpanW (read s) span

trimQuotes s = init $ tail $ s

-- Lowest level API, because we don't specify %wrapper

-- We need to define these three
-- type AlexInput = Frontend.AlexInput

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte alexInput@AlexInput {alexInputStr, alexTokenOffset} =
  case alexInputStr of
    (b:bs) ->
      Just
        ( b
        , alexInput {alexInputStr = bs, alexTokenOffset = alexTokenOffset + 1})
    [] -> Nothing

alexInputPrevChar = undefined -- our tokens' regexes don't need prev char

-- Alex will provide
--
-- alexScan :: AlexInput             -- The current input
--          -> Int                   -- The "start code"
--          -> AlexReturn action     -- The return value

-- data AlexReturn action
--   = AlexEOF

--   | AlexError
--       !AlexInput     -- Remaining input

--   | AlexSkip
--       !AlexInput     -- Remaining input
--       !Int           -- Token length

--   | AlexToken
--       !AlexInput     -- Remaining input
--       !Int           -- Token length
--       action         -- action value

readToken :: (MonadError Error m, AlexInputState m) => m Token
readToken = do
  alexInput@AlexInput { alexInputStr, alexTokenOffset } <- getAlexInput
  case alexScan alexInput 0 of
    AlexEOF     -> return $ TokenEOF $ Span alexTokenOffset 0
    AlexError _ -> throwError $ Error.customError "Parsing Error" $ Span alexTokenOffset 1
    AlexSkip alexInput' _ ->
      do
        putAlexInput $ alexInput'
        readToken
    AlexToken alexInput' len tk
      -> do
        putAlexInput $ alexInput'
        return (tk (decode $ take len alexInputStr) (Span alexTokenOffset len))

lexer :: (MonadError Error m, AlexInputState m) => (Token -> m a) -> m a
lexer cont = do
  tok <- readToken
  cont tok

dbgs s v = trace (s ++ ": " ++ show v) v
}
