# --------------------------------------------------------------------------------
# --- Defining OS and Archi
# --------------------------------------------------------------------------------
include ../_mkf/MakefileOS.mk

# ---- Detection of Compilers
include ../_mkf/MakefileFortran_ifort_Checks.mk
include ../_mkf/MakefileFortran_gfortran_Checks.mk
# --------------------------------------------------------------------------------
# ---  Main Makefile variables
# --------------------------------------------------------------------------------
# Release or debug
ifeq ($(RELEASE),)
    RELEASE=1
endif
# FORTRAN COMPILER. If not defined, we look at the available ones
ifeq ($(FCOMPILER),)
    ifeq ($(GFORTRAN_STATUS),1)
        FCOMPILER=0
    endif
    ifeq ($(IFORT_STATUS),1)
        FCOMPILER=1
    endif
endif
OBJ_DIR_BASE=_build
LIB_DIR_BASE=..$(SLASH)_lib
INC_DIR_BASE=_inc

MAKE_DLL=1
MAKE_STATIC=1
