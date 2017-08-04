module MiddleLang

--------------------------------------------------
--               verifyable chunks
data Chunk : Type where
  ||| loop count, start location
  FORLOOP : Int -> Int -> Chunk
