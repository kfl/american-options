
#MLKITLIB=$(HOME)/mlkit-4.3.6
#MLCOMP=SML_LIB=$(MLKITLIB)/lib/mlkit $(MLKITLIB)/bin/mlkit
MLCOMP=mlton

UTEST_FILES=$(shell ls utest/*.{sig,sml,mlb})
VEC_FILES=$(shell ls vec/*.{sig,sml,mlb})
.PHONY: all
all: runvec

runvec: $(VEC_FILES) $(UTEST_FILES)
	$(MLCOMP) -output $@ vec/vec.mlb

AmrPutVec: AmrPutVec.sml AmrPutVecTest.sml $(VEC_FILES) $(UTEST_FILES)
	$(MLCOMP) -output $@ AmrPutVec.mlb

clean:
	find . -name MLB | xargs rm -rf
	rm -f runvec apl AmrPutVec *~ vec/*~ utest/*~
