module Main

--------------------------------------------------
--                  Types

Word : Type
Word = Int

data Register = R1 | R2 | R3 | R4 | R5 | PC

data ISA : Type where
  ||| Rz = Rx + yR
  ADD     : Register -> Register -> Register -> ISA
  ||| Rz = sha3(Rx)
  SHA3    : Register -> Register -> ISA
  ||| Rz = current contract address
  ADDRESS : Register -> ISA
  ||| Rz = data of location Ry
  LOAD    : Register -> Register -> ISA
  ||| If Rx, execute at (current location + int)
  JUMPI   : Register -> Int -> ISA
  ||| Rz = Rx == Ry
  EQUAL   : Register -> Register -> Register -> ISA
  ||| Rz = !Rx
  NOT     : Register -> Register -> ISA
  ||| ??
  STOP    : ISA
  ||| Not an opcode - This is used to load data
  DATA    : Word -> ISA

data Assembly : Type where
  ASM  : ISA -> Assembly -> Assembly
  HALT : Assembly

-- Memory layout
record Registers where
  constructor MkRegisters
  r1, r2, r3, r4, r5, pc : Int

record Memory where
  constructor MkMemory
  regs : Registers
  asm  : Assembly

--------------------------------------------------
--              util functions

toRD : Register -> Memory -> Int
toRD R1 m = r1 $ regs m
toRD R2 m = r2 $ regs m
toRD R3 m = r3 $ regs m
toRD R4 m = r4 $ regs m
toRD R5 m = r5 $ regs m
toRD PC m = pc $ regs m

updtRD : Register -> Int -> Memory -> Memory
updtRD R1 i m = record { regs->r1 = i } m
updtRD R2 i m = record { regs->r2 = i } m
updtRD R3 i m = record { regs->r3 = i } m
updtRD R4 i m = record { regs->r4 = i } m
updtRD R5 i m = record { regs->r5 = i } m
updtRD PC i m = record { regs->pc = i } m

incPC : Memory -> Memory
incPC m = updtRD PC (1 + toRD PC m) m

getInstruction : Int -> Memory -> ISA
getInstruction 0 (MkMemory regs' (ASM instr asm')) = instr
getInstruction i (MkMemory regs' (ASM instr asm')) 
  = getInstruction (i - 1) (MkMemory regs' asm')
getInstruction _ _ = STOP  

evaluator : Memory -> Memory
evaluator mem = case getInstruction (pc $ regs mem) mem of
  ADD r r' r'' => evaluator $ incPC $ (updtRD r ((toRD r' mem) + (toRD r'' mem)) mem)
  -- SHA3    : Register -> Register -> ISA
  ADDRESS r => evaluator $ incPC mem -- todo
  LOAD r r' => evaluator $ incPC mem -- todo
  JUMPI r i => evaluator $ updtRD PC ((if (toRD r mem) == 0 then 1 else i) + toRD PC mem) mem
  EQUAL r r' r'' => evaluator $ incPC $ updtRD r (if (toRD r' mem) == (toRD r'' mem) then 1 else 0) mem
  NOT r r' => evaluator $ incPC $ updtRD r (if (toRD r' mem) == 0 then 1 else 0) mem
  STOP => mem -- todo
  DATA i => mem -- todo

--------------------------------------------------
--               helper functions
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

nil : ISA
nil = DATA 0

accumR : Register 
stopR : Register
incR : Register
boolR : Register
accumR = R1
incR = R2
stopR = R3
boolR = R4

dat : Int -> ISA
dat x = DATA x

--------------------------------------------------
--                Memory

simpleAsm : Assembly
simpleAsm
    = ASM (ADD accumR accumR incR)
    $ ASM (EQUAL boolR accumR stopR)
    $ ASM (NOT boolR boolR)
    $ ASM (JUMPI boolR (-3))
    $ HALT

simpleLoop : Memory
simpleLoop = MkMemory (MkRegisters 0 1 6 0 0 0) simpleAsm

--------------------------------------------------
--                  main

main : IO ()
main = do printLn "Registers before: "
          printRegisters simpleLoop
          printLn "Registers after: "
          printRegisters $ evaluator simpleLoop
          printLn ""

-- GOALS
--  1 Better for functional (formal verification built-in)
--  2 Better computation verification (chunkable for verification)
--  3 Establish consensus (may use ethereum for main chain)
--- 4 Mining is computation verification
