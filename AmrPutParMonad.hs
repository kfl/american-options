{-# LANGUAGE BangPatterns #-}
module AmrPutParMonad
       (binom, main)
where
import qualified Data.Vector.Unboxed as V

import qualified Control.Monad.Par as P
import Control.DeepSeq

import Data.List(foldl')
import System.Environment(getArgs)

import qualified Criterion.Main as C

--instance V.Unbox a => NFData (V.Vector a)


-- Pointwise manipulation of vectors and scalars
v1 ^*^ v2 = V.zipWith (*) v1 v2
v1 ^+^ v2 = V.zipWith (+) v1 v2
c -^ v = V.map (c -) v
c *^ v = V.map (c *) v

pmax v c = V.map (max c) v
ppmax = V.zipWith max 



binom :: Int -> Double
binom expiry = V.head first
  where 
    !uPow = V.generate (n+1) (u^)
    !dPow = V.reverse $ V.generate (n+1) (d^)
    
    !st = s0 *^ (uPow ^*^ dPow)
    !finalPut = pmax (strike -^ st) 0

    first = foldl' prevPut finalPut [n, n-1 .. 1]
    prevPut put i = P.runPar $ do
      stx <- P.spawn.return $ strike -^ (s0 *^ ((V.take i uPow) ^*^ (V.drop (n+1-i) dPow)))
      px  <- P.spawn.return $ ((qUR *^ V.tail put) ^+^ (qDR *^ V.init put))
      st  <- P.get stx
      p   <- P.get px
      
      return $ ppmax st p

    -- standard econ parameters
    strike = 100
    bankDays = 252
    s0 = 100 
    r = 0.03; alpha = 0.07; sigma = 0.20

    n = expiry*bankDays
    dt = fromIntegral expiry/fromIntegral n
    u = exp(alpha*dt+sigma*sqrt dt)
    d = exp(alpha*dt-sigma*sqrt dt)
    stepR = exp(r*dt)
    q = (stepR-d)/(u-d)
    qUR = q/stepR; qDR = (1-q)/stepR


main = do
  args <- getArgs
  case args of
    ["-f"] -> print $ binom 128
    _ -> do
      let benchmarks = [ C.bench (show years) $ C.nf binom years
                       | years <- [1, 16, 30, 32]]
      C.defaultMain benchmarks
