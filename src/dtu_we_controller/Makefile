# --------------------------------------------------------------------------------
# --- Init makefile 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileDllInit.mk
# --------------------------------------------------------------------------------
# --- Makefile flags
# --------------------------------------------------------------------------------
LIB_NAME_BASE=dtu_we_controller
SRC= misc_mod.f90 dtu_we_controller_fcns.f90 turbine_controller.f90 safety_system.f90 dtu_we_controller.f90

ifeq ($(FCOMPILER),1)
FFLAGS_EXTRA= -libs:static -threads
AFLAGS_EXTRA=
endif

# --------------------------------------------------------------------------------
# --- Default makefile to compile dll 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileDll.mk
