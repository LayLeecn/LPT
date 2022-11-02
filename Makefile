# target file
TAR := LPT 

# source files
SRC := main.f90 welcome.f90 namelist.f90 readinputs.f90 setinput.f90 \
	calsecs.f90 isleapyear.f90 printinputs.f90 readdatefile.f90 \
	allocateparticles.f90 core.f90 readmeteovars.f90 readfield.f90 \
	handle_err.f90 calga.f90

# object files
OBJ := main.o welcome.o namelist.o readinputs.o setinput.o \
	calsecs.o isleapyear.o printinputs.o readdatefile.o \
	allocateparticles.o core.o readmeteovars.o readfield.o \
	handle_err.o calga.o

# external including files and library
INC := /home/laylee/Library/include
LIB := /home/laylee/Library/lib

FC := gfortran -I$(INC) -L$(LIB)   

$(TAR):$(OBJ)
	$(FC) -o $(TAR) $(OBJ) -lnetcdf -lnetcdff

# list all the dependency
main.o:main.f90 $(MOD)
	$(FC) -c main.f90
welcome.o:welcome.f90 $(MOD)
	$(FC) -c welcome.f90
readinputs.o:readinputs.f90 setinput.f90 $(MOD)
	$(FC) -c readinputs.f90
setinput.o:setinput.f90 $(MOD)
	$(FC) -c setinput.f90
calsecs.o:calsecs.f90 isleapyear.f90 $(MOD)
	$(FC) -c calsecs.f90
isleapyear.o:isleapyear.f90 $(MOD)
	$(FC) -c isleapyear.f90
printinputs.o:printinputs.f90 readdatefile.f90 $(MOD)
	$(FC) -c printinputs.f90
readdatefile.o:readdatefile.f90 $(MOD)
	$(FC) -c readdatefile.f90
allocateparticles.o:allocateparticles.f90 $(MOD)
	$(FC) -c allocateparticles.f90
core.o:core.f90 $(MOD)
	$(FC) -c core.f90
readmeteovars.o:readmeteovars.f90 readfield.f90 $(MOD)
	$(FC) -c readmeteovars.f90
readfield.o:readfield.f90 handle_err.f90 $(MOD)
	$(FC) -c readfield.f90
calga.o:calga.f90 $(MOD)
	$(FC) -c calga.f90
handle_err.o:handle_err.f90 $(MOD)
	$(FC) -c handle_err.f90
namelist.o $(MOD):namelist.f90
	$(FC) -c namelist.f90
# clean
clean:
	rm -rf *.o *.mod