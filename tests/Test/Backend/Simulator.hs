module Test.Backend.Simulator where

import Flow
import Test.Tasty
import Test.Tasty.HUnit


import qualified Backend.Simulator as Simulator
import Backend.Instructions
import Backend.Reg

tests :: TestTree
tests = testGroup
  "Simulator"
  [testCase "Simulator" unit_simulator, testCase "Loop" unit_simulator_loop]

unit_simulator :: Assertion
unit_simulator =
  let
    machine =
      Simulator.init code []
        |> Simulator.setMemory 4096 "123"
        |> Simulator.setMemory 4097 "4098"
        |> Simulator.setMemory 4098 "234"
        |> Simulator.run
    code =
      [ XSM_MOV_Int SP 4095
      , XSM_ADD_I SP 3
      , XSM_MOV_Int R1 345
      , XSM_PUSH R1          -- setMemory 4099 345
      , XSM_MOV_DirSrc R1 4096
      , XSM_MOV_DirSrc R2 4097
      , XSM_MOV_IndSrc R2 R2
      , XSM_POP R3
      ]
  in do
    Simulator.getRegVal SP machine @?= "4098"
    Simulator.getMemory 4096 machine @?= "123"
    Simulator.getMemory 4097 machine @?= "4098"
    Simulator.getMemory 4098 machine @?= "234"
    Simulator.getRegVal R1 machine @?= "123"
    Simulator.getRegVal R2 machine @?= "234"
    Simulator.getRegVal R3 machine @?= "345"

-- brittany-disable-next-binding
unit_simulator_loop :: Assertion
unit_simulator_loop =
  let machine = Simulator.init code [] |> Simulator.run
      code =
          [ {-2056-} XSM_MOV_Int R1 10 -- n
          , {-2058-} XSM_MOV_Int R2 1  -- i
          , {-2060-} XSM_MOV_Int R3 0  -- s
          , {-2062-} XSM_MOV_R R4 R2
          , {-2064-} XSM_LE R4 R1
          , {-2066-} XSM_JZ R4 2078
          , {-2068-} XSM_MOV_R R4 R2
          , {-2070-} XSM_MUL R4 R4 -- i*i
          , {-2072-} XSM_ADD R3 R4 -- s += i*i
          , {-2074-} XSM_ADD_I R2 1 -- i += 1
          , {-2076-} XSM_JMP 2062
          ]
  in  do
        Simulator.getRegVal R1 machine @?= "10"
        Simulator.getRegVal R2 machine @?= "11"
        Simulator.getRegVal R3 machine @?= show (sum $ map (\x -> x * x) [1 .. 10 :: Int])
