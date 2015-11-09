module Ninja where
import Data.Bits

xorPowset [] = [0]
xorPowset (x:xs) = let sub = xorPowset xs
                   in sub ++ map (x `xor`) sub
