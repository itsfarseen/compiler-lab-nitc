module Backend.CodeUtils where

import Backend.Compiler
import Backend.Instructions as Instructions
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Control.Monad.State.Strict
import Backend.Reg

data CodeOutputMode
  = CodeOutputTranslated
  | CodeOutputTranslatedWithAddress
  | CodeOutputUntranslated
  deriving (Eq)


-- TODO: This function does too much.
-- Break it down and
--
getCode :: CodeOutputMode -> Compiler String
getCode mode = do
  let header = ["0", "2056", "0", "0", "0", "0", "0", "0"]
  let loadLoc = 2048
  symbolTableLastLoc <- gets symbolTableLastLoc
  let setupCode =
        [ XSM_MOV_Int SP 4096
        , XSM_ADD_I SP symbolTableLastLoc
        ]
  labels  <- gets labels
  code    <- gets code
  codeStr <- if mode == CodeOutputUntranslated
    then do
      let codeStr = map Instructions.toString code
      return $ prependLabels codeStr 0 (HM.toList labels)
    else
      (do
        let code' = labelTranslate
              ( loadLoc
              + length header
              + (length setupCode * 2)
              )
              code
              labels
        return $ map Instructions.toString code'
      )
  let setupCodeStr = map Instructions.toString setupCode
  if mode == CodeOutputTranslatedWithAddress
    then
      let (headerNumbered, codeNumbered) = prependAddress
            loadLoc
            header
            (setupCodeStr ++ codeStr)
      in  return $ unlines $ headerNumbered ++ codeNumbered
    else
      return
      $  unlines
      $  header
      ++ setupCodeStr
      ++ codeStr
      ++ ["HALT"]

getTranslatedInstrs :: Compiler [XSMInstr]
getTranslatedInstrs = do
  code   <- gets code
  labels <- gets labels
  return $ labelTranslate 2056 code labels

prependAddress
  :: Int -> [String] -> [String] -> ([String], [String])
prependAddress loadLoc header code =
  let
    headerNumbered =
      zipWith insertNumber [loadLoc ..] header
    codeStart    = loadLoc + length header
    codeNumbered = zipWith insertNumber
                           [codeStart, codeStart + 2 ..]
                           code
  in
    (headerNumbered, codeNumbered)
  where insertNumber i s = show i ++ ":\t" ++ s

labelTranslate
  :: Int
  -> [XSMInstr]
  -> HM.HashMap String Int
  -> [XSMInstr]
labelTranslate offset instrs labels = map
  (\instr -> case instr of
    XSM_UTJ jmp ->
      let label = utjGetLabel jmp
          loc   = labels HM.! label
          loc'  = (loc * 2) + offset
      in  utjTranslate jmp loc'
    _ -> instr
  )
  instrs


prependLabels
  :: [String] -> Int -> [(String, Int)] -> [String]
prependLabels code i labels =
  let labelsSorted = sortOn snd labels
  in
    case labelsSorted of
      []                     -> code
      ((label, j) : labels') -> case code of
        [] ->
          (label ++ ":\t")
            : prependLabels [] (i + 1) labels'
        (c : code') -> if i == j
          then
            let c' = label ++ ":\t" ++ c
            in  c' : prependLabels code' (i + 1) labels'
          else
            ("\t" ++ c) : prependLabels code' (i + 1) labels

