# Compaq Compiler
FC          =f90
FC_NAME     =compaq
FC_DEF      =-D_DF_VERSION_ -DCOMPAQ
# Output flags
FOUT_EXE    = /exe:
FOUT_OBJ    = /obj:
FOUT_DLL    = /out:
# Usual flags
FF_FREE     = -free
FF_OPT0     = /Optimize=0
FF_OPT      = /Optimize=3
FF_OPTO3    = /Optimize=3
FF_OPTO5    = /Optimize=3
FF_DLL      = /libs:dll 
FF_FPP      = -fpp
FF_BYTERECL = -assume byterecl
FF_MODINC   = /module=
FF_OPENMP   = -openmp
# Warnings and debug options
FF_TRACE    = -traceback
FF_DEBUGINFO=
FF_WARN     = -warn:all
FF_WARNERROR=
FF_WARNEXTRA= 
FF_DEBUG    = -check:bounds -check:format -check:output_conversion -check:pointers -check:uninit -debug:full -gen-interface
FF_DEBUGARG = -check arg_temp_created
# Advanced flags
FF_PE       = -fpe0 
FF_AUTOPAR  = -parallel -par-report1
FF_ACC      = #-offload-build #-no-offload
FF_SAVE     = /Qsave
FF_NOLOGO   = -nologo
# Checking that compiler is present
#ifeq ($(COMPAQ_STATUS),)
#	include MakefileFortran_compaq_Checks.mk
#endif
#ifeq ($(COMPAQ_STATUS),0)
#	$(error " $(FC) not in current shell, load ifort using ifortvars <arch> <vs>")
#endif
# Variable defined in _Checks
FC_ARCHI=ia32