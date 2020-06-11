{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Backend.Simulator where

import Backend.Instructions
import Backend.Reg
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Flow
import Text.Read (readMaybe)

-- import Debug.Trace
-- dbg v = trace (toString v) v
-- dbgs s v = trace (s ++ show v) v
-- dbgst s v = trace (s ++ v) v

data Machine =
  Machine
    { memory :: HM.HashMap Int String
    , registers :: HM.HashMap Int String
    , code :: [XSMInstr]
    , ip :: Int
    , stdin :: [String]
    , stdout :: [String]
    }

type Memory = HM.HashMap Int Int

class XSMStr a where
  toXSMStr :: a -> String
  -- fromXSMStr :: a -> String

instance XSMStr String where
  toXSMStr s = s
  -- fromXSMStr s = s

instance XSMStr Int where
  toXSMStr i = show i
  -- fromXSMStr s = s

init :: [XSMInstr] -> Machine
init code = initWithStdin [] code

initWithStdin :: [String] -> [XSMInstr] -> Machine
initWithStdin stdin code = Machine
  { memory    = HM.empty
  , registers = HM.empty
  , code      = code
  , ip        = 2056
  , stdin     = stdin
  , stdout    = []
  }

run :: Machine -> Machine
run machine = case fetchInstr machine of
  Nothing    -> machine
  Just instr ->
    let (!machine', halt) = machine |> execute instr
    in
      if not halt then
        machine' |> incrIP |> run
      else
        machine'

getStdout :: Machine -> [String]
getStdout = reverse . stdout

fetchInstr :: Machine -> Maybe XSMInstr
fetchInstr machine =
  let
    ipVal    = ip machine
    instrIdx = (ipVal - 2056) `div` 2
  in if instrIdx < (length $ code machine)
    then Just $ (code machine) !! instrIdx
    else Nothing

incrIP :: Machine -> Machine
incrIP machine = machine { ip = ip machine + 2 }

getRegVal :: Reg -> Machine -> String
getRegVal reg = fromMaybe "" . HM.lookup (fromEnum reg) . registers

setRegVal :: (XSMStr a) => Reg -> a -> Machine -> Machine
setRegVal reg !val machine = machine
  { registers = HM.insert
    (fromEnum reg)
    (toXSMStr val)
    (registers machine)
  }

getMemory :: Int -> Machine -> String
getMemory loc = fromMaybe "" . HM.lookup loc . memory

setMemory :: (XSMStr a) => Int -> a -> Machine -> Machine
setMemory !loc !val machine =
  machine { memory = HM.insert loc (toXSMStr val) (memory machine) }

execute :: XSMInstr -> Machine -> (Machine, Bool)
execute instr !machine =
  case instr of
    XSM_INT 10 -> (machine, True)
    _ ->
      (, False) $
        case instr of
          XSM_MOV_R r1 r2 ->
            let r2Val = getRegVal r2 machine in setRegVal r1 r2Val machine
          XSM_MOV_Int r1 val -> setRegVal r1 val machine
          XSM_MOV_Str r1 val -> setRegVal r1 val machine
          XSM_MOV_IndSrc r1 r2 ->
            let
              r2Val = read (getRegVal r2 machine) :: Int
              r2Mem = getMemory r2Val machine
            in setRegVal r1 r2Mem machine
          XSM_MOV_IndDst r1 r2 ->
            let
              r2Val = getRegVal r2 machine
              r1Val = read (getRegVal r1 machine) :: Int
            in setMemory r1Val r2Val machine
          XSM_MOV_DirSrc r1 loc ->
            let mem = getMemory loc machine in setRegVal r1 mem machine
          XSM_MOV_DirDst loc r2 ->
            let r2Val = getRegVal r2 machine in setMemory loc r2Val machine
          XSM_PUSH r1 ->
            let
              spVal = read @Int (getRegVal SP machine)
              r1Val = getRegVal r1 machine
            in machine
              |> setRegVal SP (spVal + 1)
              |> setMemory (spVal + 1) r1Val
          XSM_POP r1 ->
            let
              spVal  = read @Int (getRegVal SP machine)
              memVal = getMemory spVal machine
            in
              machine
              |> setRegVal r1 memVal --
              |> setRegVal SP (show (spVal - 1))

          XSM_ADD r1 r2 ->
            let
              r1Val = read (getRegVal r1 machine) :: Int
              r2Val = read (getRegVal r2 machine) :: Int
            in setRegVal r1 (r1Val + r2Val) machine
          XSM_ADD_I r1 i ->
            let r1Val = read (getRegVal r1 machine)
            in setRegVal r1 (r1Val + i) machine
          XSM_SUB r1 r2 ->
            let
              r1Val = read (getRegVal r1 machine) :: Int
              r2Val = read (getRegVal r2 machine) :: Int
            in setRegVal r1 (r1Val - r2Val) machine
          XSM_SUB_I r1 i ->
            let r1Val = read (getRegVal r1 machine)
            in setRegVal r1 (r1Val - i) machine
          XSM_MUL r1 r2 ->
            let
              r1Val = read (getRegVal r1 machine) :: Int
              r2Val = read (getRegVal r2 machine) :: Int
            in setRegVal r1 (r1Val * r2Val) machine
          XSM_MUL_I r1 i ->
            let r1Val = read (getRegVal r1 machine)
            in setRegVal r1 (r1Val * i) machine
          XSM_DIV r1 r2 ->
            let
              r1Val = read (getRegVal r1 machine) :: Int
              r2Val = read (getRegVal r2 machine) :: Int
            in setRegVal r1 (r1Val `div` r2Val) machine
          XSM_DIV_I r1 i ->
            let r1Val = read (getRegVal r1 machine)
            in setRegVal r1 (r1Val `div` i) machine
          XSM_MOD r1 r2 ->
            let
              r1Val = read (getRegVal r1 machine) :: Int
              r2Val = read (getRegVal r2 machine) :: Int
            in setRegVal r1 (r1Val `mod` r2Val) machine
          XSM_MOD_I r1 i ->
            let r1Val = read (getRegVal r1 machine) :: Int
            in setRegVal r1 (r1Val `mod` i) machine
          XSM_LT r1 r2 -> cmpSet r1 (<) (<) r2 machine
          XSM_EQ r1 r2 -> cmpSet r1 (==) (==) r2 machine
          XSM_NE r1 r2 -> cmpSet r1 (/=) (/=) r2 machine
          XSM_GT r1 r2 -> cmpSet r1 (>) (>) r2 machine
          XSM_LE r1 r2 -> cmpSet r1 (<=) (<=) r2 machine
          XSM_GE r1 r2 -> cmpSet r1 (>=) (>=) r2 machine
          XSM_JZ r1 loc ->
            let r1Val = getRegVal r1 machine
            in if r1Val == "0" then machine { ip = loc - 2 } else machine
          XSM_JNZ r1 loc ->
            let r1Val = getRegVal r1 machine
            in if r1Val /= "0" then machine { ip = loc - 2 } else machine
          XSM_JMP loc -> machine { ip = loc - 2 }
          XSM_INT int -> syscall int machine
          XSM_CALL loc ->
            let spVal = read @Int (getRegVal SP machine)
            in
              machine {ip = loc - 2}
              |> setRegVal SP (spVal + 1)
              |> setMemory (spVal + 1) (ip machine + 2)
          XSM_RET ->
            let
              spVal = read @Int (getRegVal SP machine)
              ipSav = read @Int (getMemory spVal machine)
            in
              machine {ip = ipSav - 2}
              |> setRegVal SP (show (spVal - 1))
          XSM_UTJ{} ->
            error $ "Untranslated jump encountered" ++ toString instr

cmpSet
  :: Reg
  -> (Int -> Int -> Bool)
  -> (String -> String -> Bool)
  -> Reg
  -> Machine
  -> Machine
cmpSet r1 opI opS r2 machine =
  -- We need both opI and opS separately because haskell needs to pass
  -- two typeclass dictionaries for two types
  let
    r1Val = getRegVal r1 machine
    r2Val = getRegVal r2 machine
  in
    case
      (readMaybe r1Val, readMaybe r2Val) :: (Maybe Int, Maybe Int)
    of
      (Just r1Val', Just r2Val') ->
        let cmp = if r1Val' `opI` r2Val' then 1 else 0 :: Int
        in setRegVal r1 cmp machine
      _ ->
        let cmp = if r1Val `opS` r2Val then 1 else 0 :: Int
        in setRegVal r1 cmp machine

syscall :: Int -> Machine -> Machine
syscall num machine =
  let
    spVal = read (getRegVal SP machine) :: Int
    args  = [ getMemory (spVal - i) machine | i <- [4, 3 .. 0] ]
  in case num of
    6 -> if (funcNum, fPtr) /= ("7", "-1")
      then
        (  error
        $  "Read sycall invalid args: (funcnum, ptr) = "
        ++ (show (funcNum, fPtr))
        )
      else setMemory bufLoc s (machine { stdin = stdin' })
     where
      [funcNum, fPtr, bufLoc_, _, _] = args
      bufLoc                         = read bufLoc_ :: Int
      (s : stdin')                   = stdin machine

    7 -> if (funcNum, fPtr) /= ("5", "-2")
      then
        (  error
        $  "Write sycall invalid args: (funcnum, ptr) = "
        ++ (show (funcNum, fPtr))
        )
      else machine { stdout = out : (stdout machine) }
      where [funcNum, fPtr, out, _, _] = args
    _ -> error $ "Syscall not implemented: " ++ (show num)
