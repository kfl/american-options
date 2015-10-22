American Options
================

Emperiments with different libraries and (eventually) algorithms for
pricing American Options.  Currently only using a standard binomial
model.

The various implementation in `AmrPut`* is based on the R-code from
Rolf Poulsen's FAMØS paper
[Amerikanske optioner og finansielle beregninger](http://www.math.ku.dk/~rolf/FAMOES/Famoes_Follow-up.pdf).


Compiling Haskell Versions
--------------------------

The Haskell implementations do not define a Main module, so in order
to compile them to standalone executables you have to pass an
appropriate `-main-is` option to GHC.  For example:

    ghc -O3 --make AmrPut.hs -main-is AmrPut -o AmrPut

Check the source file for the module name to use.


Benmark Results from Haskell Version(s)
---------------------------------------

Experiments done using Haskell library `criterion`, on a MacBook Pro
(early-2010 model).

   Data.Vector.Unboxed, optimised with GHC 7.4.2 -O3 (AmrPut.hs)
   
        Fuction call            Time (ms)
        ------------------------------------
        AmrPut.binom 1          0.8 ±   0.02
        AmrPut.binom 16       207   ±   6
        AmrPut.binom 30       941   ±  36
        AmrPut.binom 64      5576   ±  58 
        AmrPut.binom 128    27750   ± 468 



Fusable Functional Vectors in Standard ML
-----------------------------------------

The experiments below demonstrate that the use of fusable functional
vectors (FFVs) can give a runtime improvement of more than 20 percent
compared to a Vector/VectorSlice implementation. The experiments were
done running with Mlton on a MacBook Pro (mid-2012 model):

   Vector/VectorSlice (AmrPut.sml):

	     bash-3.2$ time ./AmrPutVec 
	     AmrPut.binom(1) = 6.74543295136
	     AmrPut.binom(8) = 13.9456888384
	     AmrPut.binom(16) = 16.2225913859
	     AmrPut.binom(30) = 17.6537059071
	     AmrPut.binom(64) = 18.4299315651
	     AmrPut.binom(128) = 18.5737326153

	     real    0m17.551s
	     user    0m17.499s
	     sys     0m0.046s

   Functional Vectors (AmrPutVec.sml):

	     bash-3.2$ time ./AmrPutVec 
	     AmrPut.binom(1) = 6.74543295136
	     AmrPut.binom(8) = 13.9456888384
	     AmrPut.binom(16) = 16.2225913859
	     AmrPut.binom(30) = 17.6537059071
	     AmrPut.binom(64) = 18.4299315651
	     AmrPut.binom(128) = 18.5737326153

	     real    0m13.288s
	     user    0m13.272s
	     sys     0m0.015s

With the use of Unsafe.Vector in the implementation of the
Fvec.memoize function, real time drops to 12.57s.

To redo the experiments, for each case, alter the file
AmrPutVecTest.sml appropriately and run 

    $ make AmrPutVec; time ./AmrPutVec

To get the desired speedups, the programmer is required to insert
memoize operations at subtle places in the source code; see the file
AmrPutVec.sml for details.
