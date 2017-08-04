module Hardware

public export
Word : Type
Word = Int

public export
data Register = R1 | R2 | R3 | R4 | R5 | PC

-- Memory layout
public export
record Registers where
  constructor MkRegisters
  r1, r2, r3, r4, r5, pc : Int

public export
data Instruction : Type where
  ||| Rz = Rx + yR
  ADD     : Register -> Register -> Register -> Instruction
  ||| Rz = sha3(Rx)
  SHA3    : Register -> Register -> Instruction
  ||| Rz = current contract address
  ADDRESS : Register -> Instruction
  ||| Rz = data of location Ry
  LOAD    : Register -> Register -> Instruction
  ||| If Rx, execute at (current location + int)
  JUMPI   : Register -> Int -> Instruction
  ||| Rz = Rx == Ry
  EQUAL   : Register -> Register -> Register -> Instruction
  ||| Rz = !Rx
  NOT     : Register -> Register -> Instruction

  --
  ||| Not an opcode - This is used to load data
  DATA    : Word -> Instruction

public export
record Memory where
  constructor MkMemory
  regs : Registers
  asm  : List Instruction

--------------------------------------------------
--              util functions

export
toRD : Register -> Memory -> Int
toRD R1 m = r1 $ regs m
toRD R2 m = r2 $ regs m
toRD R3 m = r3 $ regs m
toRD R4 m = r4 $ regs m
toRD R5 m = r5 $ regs m
toRD PC m = pc $ regs m

export
updtRD : Register -> Int -> Memory -> Memory
updtRD R1 i m = record { regs->r1 = i } m
updtRD R2 i m = record { regs->r2 = i } m
updtRD R3 i m = record { regs->r3 = i } m
updtRD R4 i m = record { regs->r4 = i } m
updtRD R5 i m = record { regs->r5 = i } m
updtRD PC i m = record { regs->pc = i } m

--------------------------------------------------
--                  print

export
printRegisters : Memory -> IO ()
printRegisters m = let r1' = show $ r1 $ regs m
                       r2' = show $ r2 $ regs m
                       r3' = show $ r3 $ regs m
                       r4' = show $ r4 $ regs m
                       r5' = show $ r5 $ regs m
                       pc' = show $ pc $ regs m
                   in printLn $   "R1-" <+> r1' <+> " "
                              <+> "R2-" <+> r2' <+> " "
                              <+> "R3-" <+> r3' <+> " "
                              <+> "R4-" <+> r4' <+> " "
                              <+> "R5-" <+> r5' <+> " "
                              <+> "PC-" <+> pc' <+> " "
