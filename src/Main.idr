module Main

import Hardware
import Processor
import Examples

--------------------------------------------------
--                  main

main : IO ()
main = do printLn "Registers before: "
          printRegisters simpleLoop
          printLn "Registers after: "
          printRegisters $ evaluator simpleLoop
          printLn "Expected result is 6 in R1"
          printLn ""
