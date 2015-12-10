# SUN COMPILER
FC          =f95
FC_NAME     =sun
FC_DEF      =-DSUN_NOT_DEF
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
FF_FPP      = -xpp
FF_BYTERECL = 
FF_MODINC   = -M
FF_OPENMP   = fopenmp
# Warnings and debug options
FF_DEBUGINFO= -g
FF_TRACE    =
FF_WARN     =
FF_WARNERROR=
FF_WARNEXTRA=
FF_DEBUG    = -C
# Advanced flags
FF_AUTOPAR  = -xloopinfo -xautopar
FF_NOLOGO   = 
# Windows Specific flags
ifeq ($(OS_NAME),windows)
endif
# Checking that compiler is present
#ifeq (, $(shell $(FC)))
#	$(error " $(FC) not in current shell. Is it installed on your machine")
#endif
# Variable defined in _Checks
FC_ARCHI=ARCH_NOT_SET_SUN