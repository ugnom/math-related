-- Finding primitive root modulo of N for a prime number
-- It can be proved a primitive root modulo is existing for a prime
-- m as Primitive root modulo of N is :
--   m^0, m^1 .. m^(N-2) contains all numbers from 1 to N-1
--   It constructs a Cyclic group of (Z/pZ)* 

import Data.Set hiding (foldl, take, map)

-- Primitive Root Modulo
findPRM :: Integer -> Integer
findPRM p = if p == 2 then 1 else calc 2
  where
    calc :: Integer -> Integer
    calc cand =
      let s = setOrderOf p cand
          candOrder = fromIntegral $ size s
      in if candOrder == (p-1) then
        cand
      else
        let nonDup = (head $ rmvDup (toList s) [1..p-1])
        in calc $ findNextCand p cand nonDup candOrder (fromIntegral $ size $ setOrderOf p nonDup)


prime = 2:q
q = 3:5:7:(siege 3 q)
siege m (x1:x2:xs) = [n | n <- [x1^2,x1^2+2..x2^2-2], gcd n m < 2]
                     ++ (siege (m*x2) (x2:xs))

primeFactors :: Integer -> [(Integer, Integer)]
primeFactors n = inPF n prime
  where
    inPF x (p:ps)
      | x == 1 = []
      | otherwise =
          let (k, divided) = divPrime x p
          in (p, k) : (inPF divided ps)

divPrime x p =
  if x `mod` p == 0 then
    let (k, divided) = divPrime (x `div` p) p
    in (1+k, divided)
  else
    (0, x)

calcOrderOf :: Integer -> Integer -> [Integer]
calcOrderOf p a = 1:(inCalc 1)
  where
    inCalc b =
      let m = (b * a) `mod` p
      in case m of
        1         -> []
        otherwise -> m : (inCalc m)

setOrderOf :: Integer -> Integer -> Set Integer
setOrderOf p a = insert 1 (inCalc 1)
  where
    inCalc b =
      let m = (b * a) `mod` p
      in case m of
        1         -> empty
        otherwise -> insert m (inCalc m)

rmvDup :: [Integer] -> [Integer] -> [Integer]
rmvDup [] ys = ys
rmvDup xs [] = []
rmvDup ax@(x:xs) ay@(y:ys)
  | x > y = y : (rmvDup ax ys)
  | x < y = rmvDup xs ay
  | otherwise = rmvDup xs ys


comparePF [] [] = (1,1)
comparePF az [] = (1, 1)
comparePF [] bz = (1, 1)
comparePF ((primea,powa):az) ((primeb,powb):bz) =
  let (mA, mB)  = comparePF az bz
  in if powa >= powb then
    (mA, mB * primeb^powb)
  else
    (mA * primea^powa, mB)

findNextCand p a b orderA orderB =
  let pfA = primeFactors orderA
      pfB = primeFactors orderB
      (mA, mB) = comparePF pfA pfB
  in (a^mA * b^mB) `mod` p
