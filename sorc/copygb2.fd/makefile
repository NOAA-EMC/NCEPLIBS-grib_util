################################################################################
# This makefile was produced by fmjif at 17:21:11 on 97/02/24.
SHELL=	/bin/sh
FC =ifort
CMD=	copygb2
FOPTS=  -O3 -g -r8 -convert big_endian -auto -mkl
LOPTS=  -static-intel -mkl -openmp
INCS= -I${G2_INCd} -I${IP_INCd}
OBJS  = copygb2.o \
         polates0.o polatev0.o \
         polates1.o polatev1.o \
         polates2.o polatev2.o \
         polates3.o polatev3.o \
         polates6.o polatev6.o

LIBS  = ${G2_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${IP_LIBd} ${SP_LIBd}\
         ${PNG_LIB} ${JASPER_LIB} ${Z_LIB}

################################################################################
$(phony all):	$(CMD)
$(CMD):	$(OBJS)
	$(FC) $(LOPTS) $(OBJS) $(LIBS) -o $(CMD)

copygb2.o:      copygb2.f
	$(FC) -c $(INCS) $(FOPTS) $*.f

polates0.o:     polates0.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polatev0.o:     polatev0.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polates1.o:     polates1.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polatev1.o:     polatev1.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polates2.o:     polates2.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polatev2.o:     polatev2.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polates3.o:     polates3.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polatev3.o:     polatev3.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polates6.o:     polates6.f90
	$(FC) -c $(INCS) $(FOPTS) $<

polatev6.o:     polatev6.f90
	$(FC) -c $(INCS) $(FOPTS) $<

install:
	mkdir -p ../../exec
	cp -p $(CMD) ../../exec
clean:	
	-rm -f $(OBJS)
