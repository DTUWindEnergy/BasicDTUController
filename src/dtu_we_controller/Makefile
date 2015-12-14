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
    ARFLAGS_EXTRA=
    ifeq ($(OS_NAME),windows)
         LDFLAGS_EXTRA+=/INCREMENTAL:NO
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
