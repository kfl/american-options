
#MLKITLIB=$(HOME)/mlkit-4.3.6
#MLCOMP=SML_LIB=$(MLKITLIB)/lib/mlkit $(MLKITLIB)/bin/mlkit
MLCOMP=mlton

UTEST_FILES=$(shell ls utest/*.sig utest/*.sml utest/*.mlb)
VEC_FILES=$(shell ls vec/*.sig vec/*.sml vec/*.mlb)
# Set FVEC to fvec for using Standard ML
FVEC=fvec

.PHONY: all
all: runvec

runvec: $(VEC_FILES) $(UTEST_FILES)
	$(MLCOMP) -mlb-path-var 'FVEC $(FVEC)' -output $@ vec/vec.mlb

AmrPutVec: AmrPutVec.sml AmrPutVecTest.sml $(VEC_FILES) $(UTEST_FILES)
	$(MLCOMP) -mlb-path-var 'FVEC $(FVEC)' -output $@ AmrPutVec.mlb

clean:
	find . -name MLB | xargs rm -rf
	rm -f runvec apl AmrPutVec *~ vec/*~ utest/*~
