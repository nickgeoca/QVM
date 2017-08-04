module Processor

import Hardware

incPC : Memory -> Memory
incPC m = updtRD PC (1 + toRD PC m) m

getInstruction : Int -> Memory -> Instruction
getInstruction 0 (MkMemory regs' (instr :: asm')) = instr
getInstruction i (MkMemory regs' (instr :: asm')) 
  = getInstruction (i - 1) (MkMemory regs' asm')

outOfBounds : Int -> Memory -> Bool
outOfBounds i (MkMemory regs' instrs) = cast i >= length instrs

export
evaluator : Memory -> Memory
evaluator mem = if outOfBounds (pc $ regs mem) mem then mem else
                  case getInstruction (pc $ regs mem) mem of
  ADD r r' r'' => evaluator $ incPC $ (updtRD r ((toRD r' mem) + (toRD r'' mem)) mem)
  -- SHA3    : Register -> Register -> ISA
  ADDRESS r => evaluator $ incPC mem -- todo
  LOAD r r' => evaluator $ incPC mem -- todo
  JUMPI r i => evaluator $ updtRD PC ((if (toRD r mem) == 0 then 1 else i) + toRD PC mem) mem
  EQUAL r r' r'' => evaluator $ incPC $ updtRD r (if (toRD r' mem) == (toRD r'' mem) then 1 else 0) mem
  NOT r r' => evaluator $ incPC $ updtRD r (if (toRD r' mem) == 0 then 1 else 0) mem
  STOP => mem -- todo
  DATA i => mem -- todo



