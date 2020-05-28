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

$equals      = \=

$semi_colon  = \;
$par_open    = \(
$par_close   = \)
$brace_open  = \{
$brace_close = \}
$sq_bracket_open  = \[
$sq_bracket_close = \]

@begin       = "begin"
@end         = "end"
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
@int         = "int"
@bool        = "bool"

@number      = [0-9]+
@ident       = [a-z]

tokens :-
    $white+      ;
    
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

    $equals      {token TokenEquals}

    $semi_colon  {token TokenSemiColon}
    $par_open    {token TokenParOpen}
    $par_close   {token TokenParClose}
    $brace_open  {token TokenBraceOpen}
    $brace_close {token TokenBraceClose}
    $sq_bracket_open  {token TokenSqBracketOpen}
    $sq_bracket_close {token TokenSqBracketClose}

    @begin       {token TokenBegin}
    @end         {token TokenEnd}
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
    @int         {token TokenInt}
    @bool        {token TokenBool}

    @number      {tokenInt TokenNumber}
    @ident       {tokenStr TokenIdent}

{
token action = \_ span -> action span
tokenStr action = \s span -> action $ SpanW s span
tokenInt action = \s span -> action $ SpanW (read s) span

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


}