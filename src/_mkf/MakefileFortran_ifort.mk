# INTEL FORTRAN COMPILER
FC          =ifort
FC_NAME     =intel
FC_DEF      =-D__INTEL_COMPILER
# Output flags
FOUT_EXE    = -o
FOUT_OBJ    = -o
FOUT_DLL    = -o
# Usual flags
FF_FREE     = -free
FF_OPT0     = -O0
FF_OPT      = -O3
FF_OPTO3    = -O3
FF_OPTO5    = -O5
FF_DLL      = -fPIC
FF_FPP      = -fpp
FF_BYTERECL = -assume byterecl
FF_MODINC   = -module 
FF_OPENMP   = -openmp
# Warnings and debug options
FF_TRACE    = -traceback
FF_DEBUGINFO= -g
FF_WARN     = -warn all
FF_WARNERROR= -warn error
FF_WARNEXTRA= 
FF_DEBUG    = -check bounds -check format -check output_conversion -check pointers -check uninit -debug full -gen-interface
FF_DEBUGARG = -check arg_temp_created
# Advanced flags
FF_PE       = -fpe0 
FF_AUTOPAR  = -parallel -par-report1
FF_ACC      = #-offload-build #-no-offload
FF_F90      = -stand f90
FF_F95      = -stand f95
FF_F03      = -assume realloc_lhs -stand f03
FF_SAVE     = -save
FF_NOLOGO   = -nologo
# Windows Specific flags
ifeq ($(OS_NAME),windows)
FOUT_EXE   = /exe:
FOUT_OBJ   = /obj:
FOUT_DLL   = /out:
FF_OPTO5    = -O3
FF_OPENMP   = -Qopenmp
FF_MODINC   = -module=
FF_WARN     = -warn:all
FF_WARNERROR= -warn:error
FF_DEBUGINFO= 
#     FF_DEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -fpe0 -gen-interface -traceback
FF_DEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -gen-interface
FF_F95      = -assume:norealloc_lhs
FF_DLL      = /libs:dll 
FF_SAVE     = /Qsave
endif
# Checking that compiler is present
ifeq ($(IFORT_STATUS),)
    include MakefileFortran_ifort_Checks.mk
endif
ifeq ($(IFORT_STATUS),0)
    $(error " $(FC) not in current shell, load ifort using ifortvars <arch> <vs>")
endif
# Variable defined in _Checks
FC_ARCHI=$(IFORT_ARCHI)
