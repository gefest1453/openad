CC=gcc
FC=gfortran 
F90C=gfortran 
CXX=g++
LINK=$(F90C)
CPP_FLAGS       =
FFLAGS      	= -O0 -g -ffree-line-length-132 -fimplicit-none $(CPP_FLAGS) 
F90FLAGS      	= -O0 -g -ffree-line-length-132   -fimplicit-none $(CPP_FLAGS)
.SUFFIXES: .o .f .p .F .c .d .ff90 .f90 .FF90 .F90 .F95 .flowdir .flow .f95 .f03 
all: tabulate adjoint

FILES= w2f__types.f90  OAD_active.f90 OAD_cp.f90 OAD_rev.f90 OAD_tape.f90 log_eq.pre.xb.x2w.w2f.post.f90  norma.f90 line_search.f90 Main1.f90 revStats.f90 revolve.f90

adjoint: log_eq.f90 Main1.f90  
		mkdir -p ./work ; cd ./work ; cp -uf ../*.F90 . && cp -uf ../ad_*.f . ; cp -uf ../*.sh . && cp -uf ../*.F .  && cp -uf ../Makefile . && make adjoint1 ; cd ..
#log_eq.pre.xb.x2w.w2f.post.f90:  log_eq.f90
#		openad --copy -m rj -d 2 ./log_eq.f90 
numCore.pre.f: log_eq.f90
	${OPENADFORTTK_BASE}/tools/SourceProcessing/preProcess.py -m r --inputFormat=free -o $@ $<
numCore.pre.B: numCore.pre.f 
	${OPEN64ROOT}/crayf90/sgi/mfef90 -z -F -N132 $<
# whirl -> xaif
numCore.pre.xaif : numCore.pre.B 
	${OPENADFORTTKROOT}/bin/whirl2xaif -n -o $@ $<
# xaif -> xaif'
numCore.pre.xb.xaif : numCore.pre.xaif
	${XAIFBOOSTERROOT}/xaifBooster/algorithms/BasicBlockPreaccumulationReverse/driver/oadDriver \
	-c ${XAIFSCHEMAROOT}/schema/examples/inlinable_intrinsics.xaif \
	-s ${XAIFSCHEMAROOT}/schema -f -i $< -o $@ 
numCore.pre.xb.x2w.B : numCore.pre.xb.xaif  
	${OPENADFORTTKROOT}/bin/xaif2whirl numCore.pre.B $<
# whirl' -> fortran'
numCore.pre.xb.x2w.w2f.f: numCore.pre.xb.x2w.B
	${OPEN64ROOT}/whirl2f/whirl2f -openad $<
# inject template directives
numCore.pre.xb.x2w.w2f.td.f: numCore.pre.xb.x2w.w2f.f injectTemplateDirects.sh
	./injectTemplateDirects.sh $< $@
# postprocess 
log_eq.pre.xb.x2w.w2f.post.f90: numCore.pre.xb.x2w.w2f.td.f ad_template.f revStats.o revolve.o
	${OPENADFORTTK_BASE}/tools/SourceProcessing/postProcess.py -m r \
	--infoUnitFile=w2f__types.f90 \
	-i ${OPENADROOT}/runTimeSupport/cpToFile/ad_inline.f		\
	--outputFormat=free -o $@ $<
		
w2f__types.f90: ../w2f__types.F90
	cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
OAD_active.f90: ../OAD_active.F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
OAD_rev.f90: ../OAD_rev.F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
revolve.f90: ../revolve.F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
%.o : %.f
	${F90C} -o $@ -c $< 
	mkdir -p ./source
	cp -f $< ./source 

adjoint1:ad_template.split.f $(FILES:.f90=.o )  OAD_tape.f90 line_search.f90 OAD_cp.f90 revolve.f90
		 $(LINK) $(FILES:.f90=.o) -o adjoint1
		

tabulate: log_eq.o Main.o ad_template.split.f ad_template.revolve.f
				$(F90C) log_eq.o Main.o -o tabulate


%.o:%.f90 
	$(F90C) $(F90FLAGS) $(FFLAGS) -M -cpp $<  > $(basename $<).d
	$(F90C) $(F90FLAGS) $(FFLAGS) $(MODPATH) -c -o $@ $< 
	mkdir -p ./source
	cp -f $< ./source 

%.d:%.F90
	$(F90C) $(F90FLAGS) $(FFLAGS) -M -cpp $<  > $(basename $<).d


	
%.o:%.F90 
	$(F90C) $(F90FLAGS) $(FFLAGS) -M -cpp $<  > $(basename $<).d
	$(F90C) $(F90FLAGS) $(FFLAGS) $(MODPATH) -c -o $@ $< 
	mkdir -p ./source
	cp -f $< ./source 

ad_template.split.f:ad_template_split.F
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
ad_template.revolve.f:ad_template_revolve.F
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
		
# C compile rule
Main1.f90:Main1.F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
log_eq.f90:log_eq.F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
%.f90:%.F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
%.f:%.F
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@

%.o:%.c 
	$(CC) -c $(CFLAGS) -o $@ $< 
	mkdir -p source1
	cp $< ./source1
.PHONY. clean:
		 rm -rf ./work ./*.o ./*.f  ./*.d ./*.mod ./*.exe ./*.f90  *.f95 ./oad_output ./oad_work *whirl adal* *.xaif *.B numericalCore*  ./tabulate
