help:
	@echo "Available targets: llvm, gnu, intel, pgi, xl, nag, cray"
	exit 1

llvm:
	( $(MAKE) all \
	 "CC = mpicc" \
	 "CFLAGS = -g -Weverything" \
	 "FC = mpifort" \
	 "FFLAGS = -g -Mbounds -Mchkptr -Mstandard" )

gnu:
	( $(MAKE) all \
	 "CC = mpicc" \
	 "CFLAGS = -g -Wall -pedantic" \
	 "FC = mpif90"\
	 "FFLAGS = -g -Wall -fcheck=all -pedantic -std=f2003 -fbacktrace" )

intel:
	( $(MAKE) all \
	 "CC = mpicc" \
	 "CFLAGS = -g -Wall -traceback" \
	 "FC = mpif90 " \
	 "FFLAGS = -g -warn all -check all -traceback" )

pgi:
	( $(MAKE) all \
	 "CC = mpicc" \
	 "CFLAGS = -g -traceback" \
	 "FC = mpif90" \
	 "FFLAGS = -g -Mbounds -Mchkptr -traceback" )

xl:
	( $(MAKE) all \
	 "CC = mpicc \
	 "CFLAGS = -g" \
	 "FC = mpifort" \
	 "FFLAGS = -g -C" )

nag:
	( $(MAKE) all \
	 "CC = mpicc" \
	 "CFLAGS = -g -Wall -pedantic" \
	 "FC = mpifort" \
	 "FFLAGS = -f2003 -g -C=all" )

cray:
	( $(MAKE) all \
	 "CC = cc" \
	 "CFLAGS = -G 0 -h conform -h nomessage=193 -h msglevel_0 -h bounds -h dir_check" \
	 "FC = ftn" \
	 "FFLAGS = -G 0 -m 0 -h dir_check -R bps -O 0 -e nI -N 132" )


ifneq "$(PNETCDF)" ""
export CPPFLAGS = -I${PNETCDF}/include
export LIBS = -L${PNETCDF}/lib -lpnetcdf
endif

all:
	$(MAKE) -C src
	\ln -sf src/mrtb .

clean:
	$(RM) -f mrtb
	$(MAKE) -C src clean 
