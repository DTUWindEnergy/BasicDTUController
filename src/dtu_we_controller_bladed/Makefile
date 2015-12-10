# --------------------------------------------------------------------------------
# --- Init makefile 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileDllInit.mk
# --------------------------------------------------------------------------------
# --- Makefile flags
# --------------------------------------------------------------------------------
LIB_NAME_BASE=dtu_we_controller_bladed

SRC= dtu_we_controller_bladed.f90

# Adding dtu_we_controller
LIBS_EXTRA=-L $(LIB_DIR) -Wl,-static -ldtu_we_controller -Wl,-Bdynamic

ifeq ($(FCOMPILER),1)
    FFLAGS_EXTRA= -libs:static -threads
    ARFLAGS_EXTRA=
    ifeq ($(OS_NAME),windows)
         LDFLAGS_EXTRA+=/INCREMENTAL:NO
         LIBS_EXTRA=/LIBPATH:$(LIB_DIR) dtu_we_controller.lib
#          LIBS_EXTRA+=libifcoremt.lib
    endif
    ifeq ($(OS_NAME),linux)
		 # USING Static Intel libs to avoid dependencies
         LIBS_EXTRA+=-lifcore_pic
         LDFLAGS_EXTRA+=-static-intel
    endif
endif
ifeq ($(FCOMPILER),0)
    ifeq ($(OS_NAME),windows)
        LDFLAGS_EXTRA+= -static-libgfortran -static-libgcc 
    endif
endif

# --------------------------------------------------------------------------------
# --- Default makefile to compile dll 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileDll.mk
