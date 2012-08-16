american-options
================

Emperiments with algorithms for pricing American Options

Fusable Functional Vectors in Standard ML
-----------------------------------------

The experiments below demonstrate that the use of fusable functional
vectors (FFVs) can give a runtime improvement of more than 20 percent
compared to a Vector/VectorSlice implementation. The experiments were
done running with Mlton on a MacBook Pro:

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

To redo the experiments, for each case, alter the file
AmrPutVecTest.sml appropriately and run 

   $ make AmrPutVec; time ./AmrPutVec

To get the desired speedups, the programmer is required to insert
memoize operations at subtle places in the source code; see the file
AmrPutVec.sml for details.