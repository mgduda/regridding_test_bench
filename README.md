# MPAS-Atmosphere parallel regridding test bench

## Prerequisites
In addition to a C compiler and a Fortran 2003 compiler, the following
libraries are needed in order to build this test bench:
* parallel-netCDF: The environment variable PNETCDF should be set to
  the installation path of the parallel-netCDF library
* MPI: The C and Fortran MPI compiler wrappers should be in your PATH

## Compilation
This test bench is written in standard C89 and Fortran 2003, and it
should be possible to compile it with a wide range of compilers.
Compilation targets for the following compilers have been defined in
the top-level Makefile:

* GNU (`make gnu`)
* LLVM (`make llvm`)
* Intel (`make intel`)
* PGI (`make pgi`)
* NAG (`make nag`)
* XL (`make xl`)
* Cray (`make cray`)

All that is needed to build the main program (`mrtb` -- the MPAS
Regridding Test Bench) is to invoke GNU Make with one of the above
targets.
