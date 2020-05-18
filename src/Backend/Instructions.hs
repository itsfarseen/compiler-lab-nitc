module Backend.Instructions where

import Backend.Reg

data XSMInstr
  = XSM_MOV_R Reg Reg
  | XSM_MOV_Int Reg Int
  | XSM_MOV_Str Reg String
  | XSM_MOV_IndSrc Reg Reg
  | XSM_MOV_IndDst Reg Reg
  | XSM_MOV_DirSrc Reg Int
  | XSM_MOV_DirDst Int Reg
  | XSM_PUSH Reg
  | XSM_POP Reg
  | XSM_ADD Reg Reg
  | XSM_ADD_I Reg Int
  | XSM_SUB Reg Reg
  | XSM_SUB_I Reg Int
  | XSM_MUL Reg Reg
  | XSM_MUL_I Reg Int
  | XSM_DIV Reg Reg
  | XSM_DIV_I Reg Int
  | XSM_MOD Reg Reg
  | XSM_MOD_I Reg Int
  | XSM_LT Reg Reg
  | XSM_EQ Reg Reg
  | XSM_NE Reg Reg
  | XSM_GT Reg Reg
  | XSM_LE Reg Reg
  | XSM_GE Reg Reg
  | XSM_JZ Reg Int
  | XSM_JNZ Reg Int
  | XSM_JMP Int
  | XSM_INT Int
  | XSM_UTJ UntranslatedJump

type Label = String

data UntranslatedJump
  = XSM_UTJ_JZ Reg Label
  | XSM_UTJ_JNZ Reg Label
  | XSM_UTJ_JMP Label
  deriving (Show)

utjGetLabel :: UntranslatedJump -> String
utjGetLabel jmp = case jmp of
  XSM_UTJ_JZ  _ label -> label
  XSM_UTJ_JNZ _ label -> label
  XSM_UTJ_JMP label   -> label

utjTranslate :: UntranslatedJump -> Int -> XSMInstr
utjTranslate jmp loc = case jmp of
  XSM_UTJ_JZ  r _ -> XSM_JZ r loc
  XSM_UTJ_JNZ r _ -> XSM_JNZ r loc
  XSM_UTJ_JMP _   -> XSM_JMP loc

toCode :: XSMInstr -> String
toCode instr = case instr of
  XSM_MOV_R      rd rs -> "MOV " ++ show rd ++ ", " ++ show rs
  XSM_MOV_Int    r  i  -> "MOV " ++ show r ++ ", " ++ show i
  XSM_MOV_Str    r  s  -> "MOV " ++ show r ++ ", " ++ s
  XSM_MOV_IndSrc r  s  -> "MOV " ++ show r ++ ", [" ++ show s ++ "]"
  XSM_MOV_IndDst r  s  -> "MOV [" ++ show r ++ "], " ++ show s
  XSM_MOV_DirSrc r  s  -> "MOV " ++ show r ++ ", [" ++ show s ++ "]"
  XSM_MOV_DirDst r  s  -> "MOV [" ++ show r ++ "], " ++ show s
  XSM_PUSH r           -> "PUSH " ++ show r
  XSM_POP  r           -> "POP " ++ show r
  XSM_ADD   r  ri      -> "ADD " ++ show r ++ ", " ++ show ri
  XSM_ADD_I r  ri      -> "ADD " ++ show r ++ ", " ++ show ri
  XSM_SUB   r  ri      -> "SUB " ++ show r ++ ", " ++ show ri
  XSM_SUB_I r  ri      -> "SUB " ++ show r ++ ", " ++ show ri
  XSM_MUL   r  ri      -> "MUL " ++ show r ++ ", " ++ show ri
  XSM_MUL_I r  ri      -> "MUL " ++ show r ++ ", " ++ show ri
  XSM_DIV   r  ri      -> "DIV " ++ show r ++ ", " ++ show ri
  XSM_DIV_I r  ri      -> "DIV " ++ show r ++ ", " ++ show ri
  XSM_MOD   r  ri      -> "MOD " ++ show r ++ ", " ++ show ri
  XSM_MOD_I r  ri      -> "MOD " ++ show r ++ ", " ++ show ri
  XSM_LT    r1 r2      -> "LT " ++ show r1 ++ ", " ++ show r2
  XSM_EQ    r1 r2      -> "EQ " ++ show r1 ++ ", " ++ show r2
  XSM_NE    r1 r2      -> "NE " ++ show r1 ++ ", " ++ show r2
  XSM_GT    r1 r2      -> "GT " ++ show r1 ++ ", " ++ show r2
  XSM_LE    r1 r2      -> "LE " ++ show r1 ++ ", " ++ show r2
  XSM_GE    r1 r2      -> "GE " ++ show r1 ++ ", " ++ show r2
  XSM_JZ    r1 i       -> "JZ " ++ show r1 ++ ", " ++ show i
  XSM_JNZ   r1 i       -> "JNZ " ++ show r1 ++ ", " ++ show i
  XSM_JMP i            -> "JMP " ++ show i
  XSM_INT i            -> "INT " ++ show i
  XSM_UTJ utj          -> error "Untranslated jump encountered: " ++ show utj

toCodeUntranslated :: XSMInstr -> String
toCodeUntranslated instr = case instr of
  XSM_UTJ (XSM_UTJ_JMP label  ) -> "JMP " ++ label
  XSM_UTJ (XSM_UTJ_JNZ r label) -> "JNZ " ++ show r ++ ", " ++ label
  XSM_UTJ (XSM_UTJ_JZ  r label) -> "JZ " ++ show r ++ ", " ++ label
  _                             -> toCode instr
