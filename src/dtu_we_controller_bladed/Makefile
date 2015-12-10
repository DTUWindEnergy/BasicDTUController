# --------------------------------------------------------------------------------
# --- Init makefile 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileDllInit.mk
# --------------------------------------------------------------------------------
# --- Makefile flags
# --------------------------------------------------------------------------------
LIB_NAME_BASE=dtu_we_controller_bladed

SRC= dtu_we_controller_bladed.f90

ifeq ($(FCOMPILER),1)
FFLAGS_EXTRA=-libs:dll -threads
ARFLAGS_EXTRA=
endif
LDFLAGS_EXTRA=

# Adding static lib (note headers/modules are already in INCS)
LIBS_EXTRA=-L $(LIB_DIR) -Wl,-static -ldtu_we_controller -Wl,-Bdynamic
ifeq ($(FCOMPILER),1)
    ifeq ($(OS_NAME),windows)
         LIBS_EXTRA=/LIBPATH:$(LIB_DIR) dtu_we_controller.lib
    endif
endif

# --------------------------------------------------------------------------------
# --- Default makefile to compile dll 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileDll.mk
