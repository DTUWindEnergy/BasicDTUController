# GNU FORTRAN COMPILER
FC         =gfortran
FC_NAME    =gfortran
FC_DEF     =-D__GFORTRAN__
FOUT_EXE   = -o
FOUT_OBJ   = -o
FOUT_DLL   = -o
# Usual flags
FF_FREE     = -free 
FF_OPT0     = -O0
FF_OPT      = -O3
FF_OPTO3    = -O3
FF_OPTO5    = -O5
FF_DLL      = -fPIC
FF_FPP      = -cpp
FF_BYTERECL = 
FF_MODINC   = -J
FF_OPENMP   = -fopenmp
# Warnings and debug options
FF_DEBUGINFO= -g
FF_TRACE    = -fbacktrace -fdump-core 
FF_WARN     = -Wall -Wno-intrinsic-shadow  -Wtabs -Wuninitialized -O -Wunused
FF_WARNERROR= -warn error
#     -Wno-c-binding-type -Wno-unused-function 
FF_WARNEXTRA= -Wcharacter-truncation -Wextra -Wno-implicit-interface -Wno-implicit-procedure -Wunderflow -Wunused-dummy-argument -Wunused-parameter -Wmaybe-uninitialized 
FF_DEBUG    = -fbounds-check -finit-real=nan 
# Advanced flags
FF_PE       = -ffpe-trap=invalid,zero,overflow 
FF_AUTOPAR  = 
FF_F95      = -std=f95 -fno-realloc-lhs
FF_F03      = -ffree-line-length-none
FF_NOLOGO   = 
# Windows Specific flags
ifeq ($(OS_NAME),windows)
FF_DLL      = 
# -mrtd # Causes segfault!
endif
# Checking that compiler is present
ifeq ($(GFORTRAN_STATUS),)
    include MakefileFortran_gfortran_Checks.mk
endif
ifeq ($(GFORTRAN_STATUS),0)
    $(error " $(FC) not in current shell. Is it installed on your machine?")
endif
# Variable defined in _Checks
FC_ARCHI=$(GFORTRAN_ARCHI)

# Overriding variables defined in MakefileOS... NASTY
LD=ld
LD_OUT=-o
LD_DLL=
AR=ar
AR_OUT=
