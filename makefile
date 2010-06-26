#
# Makefile for huckel_energy
#

FC=gfortran
CFLAGS= -Wall -g -c
LFLAGS= -Wall -g
LIBS= -llapack -lblas
EXE=huckel_energy
#
# Target specification
#


all: huckel_energy

huckel_energy: huckel_energy.o parse.o printing.o matrixOp.o doublePrecision.o
	$(FC) $(LFLAGS) $(LIBS) -o $(EXE)  huckel_energy.o parse.o printing.o matrixOp.o doublePrecision.o 
	
huckel_energy.o: doublePrecision.o parse.o printing.o matrixOp.o huckel_energy.f90
	$(FC) $(CFLAGS) huckel_energy.f90
doublePrecision.o: doublePrecision.f90
	$(FC) $(CFLAGS) doublePrecision.f90

parse.o: doublePrecision.o parse.f90
	$(FC) $(CFLAGS) parse.f90

printing.o: doublePrecision.o printing.f90
	$(FC) $(CFLAGS) printing.f90

matrixOp.o: doublePrecision.o matrixOp.f90
	$(FC) $(CFLAGS) matrixOp.f90

clean:
	rm -f *.mod *.o huckel_energy
