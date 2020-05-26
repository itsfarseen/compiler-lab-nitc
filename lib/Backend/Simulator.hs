module Backend.Simulator where

import Backend.Instructions
import Backend.Reg
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Flow

-- import Debug.Trace
-- debug s v = trace (s ++ show v) v

data Machine =
    Machine
            {
                memory :: HM.HashMap Int Int,
                registers :: HM.HashMap Int Int,
                code :: [XSMInstr],
                ip :: Int
            }

type Memory = HM.HashMap Int Int

init :: [XSMInstr] -> Machine
init code = Machine HM.empty HM.empty code 2056

run :: Machine -> Machine
run machine = case fetchInstr machine of
  Nothing    -> machine
  Just instr -> machine |> execute instr |> incrIP |> run

fetchInstr :: Machine -> Maybe XSMInstr
fetchInstr machine =
  let ipVal    = ip machine
      instrIdx = (ipVal - 2056) `div` 2
  in  if instrIdx < (length $ code machine)
        then Just $ (code machine) !! instrIdx
        else Nothing

incrIP :: Machine -> Machine
incrIP machine = machine { ip = ip machine + 2 }

getRegVal :: Reg -> Machine -> Int
getRegVal reg = fromMaybe 0 . HM.lookup (fromEnum reg) . registers

setRegVal :: Reg -> Int -> Machine -> Machine
setRegVal reg val machine =
  machine { registers = HM.insert (fromEnum reg) val (registers machine) }

getMemory :: Int -> Machine -> Int
getMemory loc = fromMaybe 0 . HM.lookup loc . memory

setMemory :: Int -> Int -> Machine -> Machine
setMemory loc val machine =
  machine { memory = HM.insert loc val (memory machine) }

execute :: XSMInstr -> Machine -> Machine
execute instr machine = case instr of
  XSM_MOV_R r1 r2 ->
    let r2Val = getRegVal r2 machine in setRegVal r1 r2Val machine
  XSM_MOV_Int r1 val -> setRegVal r1 val machine
  XSM_MOV_Str{}      -> error $ "Not implemented: " ++ toString instr
  XSM_MOV_IndSrc r1 r2 ->
    let r2Val = getRegVal r2 machine
        r2Mem = getMemory r2Val machine
    in  setRegVal r1 r2Mem machine
  XSM_MOV_IndDst r1 r2 ->
    let r2Val = getRegVal r2 machine
        r1Val = getRegVal r1 machine
    in  setMemory r1Val r2Val machine
  XSM_MOV_DirSrc r1 loc ->
    let mem = getMemory loc machine in setRegVal r1 mem machine
  XSM_MOV_DirDst loc r2 ->
    let r2Val = getRegVal r2 machine in setMemory loc r2Val machine
  XSM_PUSH r1 ->
    let spVal = getRegVal SP machine
        r1Val = getRegVal r1 machine
    in  machine |> setRegVal SP (spVal + 1) |> setMemory (spVal + 1) r1Val
  XSM_POP r1 ->
    let spVal  = getRegVal SP machine
        memVal = getMemory spVal machine
    in  machine |> setRegVal r1 memVal |> setRegVal SP (spVal - 1)

  XSM_ADD r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
    in  setRegVal r1 (r1Val + r2Val) machine
  XSM_ADD_I r1 i ->
    let r1Val = getRegVal r1 machine in setRegVal r1 (r1Val + i) machine
  XSM_SUB r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
    in  setRegVal r1 (r1Val - r2Val) machine
  XSM_SUB_I r1 i ->
    let r1Val = getRegVal r1 machine in setRegVal r1 (r1Val - i) machine
  XSM_MUL r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
    in  setRegVal r1 (r1Val * r2Val) machine
  XSM_MUL_I r1 i ->
    let r1Val = getRegVal r1 machine in setRegVal r1 (r1Val * i) machine
  XSM_DIV r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
    in  setRegVal r1 (r1Val `div` r2Val) machine
  XSM_DIV_I r1 i ->
    let r1Val = getRegVal r1 machine in setRegVal r1 (r1Val `div` i) machine
  XSM_MOD r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
    in  setRegVal r1 (r1Val `mod` r2Val) machine
  XSM_MOD_I r1 i ->
    let r1Val = getRegVal r1 machine in setRegVal r1 (r1Val `mod` i) machine
  XSM_LT r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
        cmp   = if r1Val < r2Val then 1 else 0
    in  setRegVal r1 cmp machine
  XSM_EQ r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
        cmp   = if r1Val == r2Val then 1 else 0
    in  setRegVal r1 cmp machine
  XSM_NE r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
        cmp   = if r1Val /= r2Val then 1 else 0
    in  setRegVal r1 cmp machine
  XSM_GT r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
        cmp   = if r1Val > r2Val then 1 else 0
    in  setRegVal r1 cmp machine
  XSM_LE r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
        cmp   = if r1Val <= r2Val then 1 else 0
    in  setRegVal r1 cmp machine
  XSM_GE r1 r2 ->
    let r1Val = getRegVal r1 machine
        r2Val = getRegVal r2 machine
        cmp   = if r1Val >= r2Val then 1 else 0
    in  setRegVal r1 cmp machine
  XSM_JZ r1 loc ->
    let r1Val = getRegVal r1 machine
    in  if r1Val == 0 then machine { ip = loc - 2 } else machine
  XSM_JNZ r1 loc ->
    let r1Val = getRegVal r1 machine
    in  if r1Val /= 0 then machine { ip = loc - 2 } else machine

  XSM_JMP loc -> machine { ip = loc - 2 }
  XSM_INT int -> error $ "Interrupt not implemented: " ++ show int
  XSM_UTJ{}   -> error $ "Untranslated jump encountered" ++ toString instr


