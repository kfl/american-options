{-# LANGUAGE FlexibleContexts #-}
module AmrPutAcc 
       (binom, main)
where
import qualified Data.Array.Accelerate as A
import qualified Data.Vector.Storable as V

import Data.Array.Accelerate (Z(..), (:.)(..),(!))

import qualified Data.Array.Accelerate.Interpreter as AI
import qualified Data.Array.Accelerate.CUDA as ACUDA

import Data.List(foldl1')
import System.Environment(getArgs)

--import qualified Criterion.Main as C


-- Pointwise manipulation of vectors an scalars
v1 ^*^ v2 = A.zipWith (*) v1 v2
v1 ^+^ v2 = A.zipWith (+) v1 v2
c -^ v = A.map (c -) v
c *^ v = A.map (c *) v

pmax v c = A.map (max c) v
ppmax = A.zipWith max

vtail = A.tail
vinit = A.init
vtake i v = A.take (A.constant i) v
vdrop i v = A.drop (A.constant i) v

vreverse v = 
  let len = A.unindex1 (A.shape v) in
  A.backpermute (A.shape v) (\ix -> A.index1 $ len - (A.unindex1 ix) - 1) v

type FloatRep = Float
--type FloatRep = Double  
-- I would like to use Double, but my NVIDA card does not support double


binom :: Int -> A.Acc(A.Vector FloatRep)
binom expiry = first
  where     
    i2f = A.fromIntegral . A.unindex1
    
    uPow, dPow :: Int -> A.Acc(A.Vector FloatRep)
    dPow i = vdrop (n+1-i)
           $ vreverse
           $ A.generate (A.constant (Z:.n+1)) (\ix -> d' ** i2f ix)

    uPow i = vtake i
           $ A.generate (A.constant (Z:.n+1)) (\ix -> u' ** i2f ix)

    finalPut = pmax (strike -^ st) 0
        where st = s0 *^ (uPow(n+1)^*^ dPow(n+1))

-- for (i in n:1) {
--   St<-S0*u.pow[1:i]*d.pow[i:1]
--   put[1:i]<-pmax(strike-St,(qUR*put[2:(i+1)]+qDR*put[1:i]))
-- }
    first = foldl1' (A.>->) (map prevPut [n, n-1 .. 1]) finalPut
    prevPut :: Int -> A.Acc(A.Vector FloatRep) -> A.Acc(A.Vector FloatRep)
    prevPut i put = 
      ppmax(strike -^ st) ((qUR *^ vtail put) ^+^ (qDR *^ vinit put))
        where st = s0 *^ (uPow i ^*^ dPow i)

    -- standard econ parameters
    strike = 100
    bankDays = 252
    s0 = 100 
    r = 0.03; alpha = 0.07; sigma = 0.20

    n :: Int
    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = A.constant$ q/stepR; qDR = A.constant$ (1-q)/stepR
    u' = A.constant u
    d' = A.constant d




arun run x = head $ A.toList $ run x

main = do
  args <- getArgs
  case args of
    ["-c", n] -> print $ arun ACUDA.run $ binom (read n)
    ["-i", n] -> print $ arun AI.run $ binom (read n)
    _         -> print $ arun ACUDA.run $ binom 8
