CC=gcc
FC=gfortran 
F90C=gfortran 
CXX=g++
LINK=$(F90C)
CPP_FLAGS       =
FFLAGS      	= -O2 -ffree-line-length-512  $(CPP_FLAGS)
F90FLAGS      	= -O2 -ffree-line-length-512  $(CPP_FLAGS)
.SUFFIXES: .o .f .p .F .c .d .ff90 .f90 .FF90 .F90 .F95 .flowdir .flow .f95 .f03 
all: tabulate adjoint

FILES= w2f__types.f90  OAD_active.f90 OAD_cp.f90 OAD_rev.f90 OAD_tape.f90 log_eq.pre.xb.x2w.w2f.post.f90  norma.f90 line_search.f90 Main1.f90

adjoint: log_eq.f90 Main1.f90
		mkdir -p ./work && 	cd ./work && cp -uf ../*.F90 . && cp -uf ../Makefile . && make adjoint1 ; cd ..
log_eq.pre.xb.x2w.w2f.post.f90:  log_eq.f90
		openad --copy -m rj -d 2 ./log_eq.f90 
		
 OAD_active.f90 OAD_cp.f90 OAD_rev.f90 OAD_tape.f90 w2f__types.f90:log_eq.pre.xb.x2w.w2f.post.f90

adjoint1: $(FILES:.f90=.o ) 
		 $(LINK) $(FILES:.f90=.o) -o adjoint1
		

tabulate: log_eq.o Main.o
				$(F90C) log_eq.o Main.o -o tabulate


%.o:%.f90 
	$(F90C) $(F90FLAGS) $(FFLAGS) -M -cpp $<  > $(basename $<).d
	$(F90C) $(F90FLAGS) $(FFLAGS) $(MODPATH) -c -o $@ $< 

%.d:%.F90
	$(F90C) $(F90FLAGS) $(FFLAGS) -M -cpp $<  > $(basename $<).d


	
%.o:%.F90 
	$(F90C) $(F90FLAGS) $(FFLAGS) -M -cpp $<  > $(basename $<).d
	$(F90C) $(F90FLAGS) $(FFLAGS) $(MODPATH) -c -o $@ $< 

# C compile rule
Main1.f90:Main1.F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
log_eq.f90:log_eq.F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
%.f90:%F90
		cat $< |cpp -traditional -P $(CPP_FLAGS) -D OPENAD > $@
%.o:%.c 
	$(CC) -c $(CFLAGS) -o $@ $< 
	mkdir -p source1
	cp $< ./source1
.PHONY. clean:
		 rm -rf ./work ./*.o  ./*.d ./*.mod ./*.exe ./*.f90  *.f95 ./oad_output ./oad_work *whirl adal* *.xaif *.B numericalCore* 
