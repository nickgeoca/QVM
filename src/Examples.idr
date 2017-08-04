module Examples

import Hardware
import Processor
--------------------------------------------------
--               helper functions

nil : Instruction
nil = DATA 0

accumR : Register 
stopR : Register
incR : Register
boolR : Register
accumR = R1
incR = R2
stopR = R3
boolR = R4

dat : Int -> Instruction
dat x = DATA x

--------------------------------------------------
--               ex simple loop

simpleAsm : List Instruction
simpleAsm =
  [ ADD accumR accumR incR
  , EQUAL boolR accumR stopR
  , NOT boolR boolR
  , JUMPI boolR (-3)
  ]

export
simpleLoop : Memory
simpleLoop = MkMemory (MkRegisters 0 1 6 0 0 0) simpleAsm

