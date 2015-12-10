# --------------------------------------------------------------------------------
# --- Description 
# --------------------------------------------------------------------------------
# This Makefile perform checks for intel fortran compiler :
#  - If the compiler is available, the variable STATUS is set to 1
#  - The architecture the compiler will use to compile is set in ARCHI
#  - The variable FC_AVAILABLE is incremented
#
# This makefile requires MakefileOS

# --------------------------------------------------------------------------------
# ---  Requirements
# --------------------------------------------------------------------------------
ifeq ($(ERR_TO_STD),)
    $(warning 'MakefileFortran_gfortran_Checks needs MakefileOS' )
    ERR_TO_STD=2>&1
    GREP=grep
endif

# --------------------------------------------------------------------------------
# --- Gfortran 
# --------------------------------------------------------------------------------
GFORTRAN_STATUS =X$(shell gfortran --version $(ERR_TO_NULL))
ifeq ($(GFORTRAN_STATUS),X)
    GFORTRAN_STATUS=0
else
    GFORTRAN_STATUS=1
	# --- Detecting architecture for compilation
    GFORTRAN_ARCHI=X$(shell gfortran -v $(ERR_TO_STD) | $(GREP) "mingw32")
    ifneq ($(GFORTRAN_ARCHI),X)
        GFORTRAN_ARCHI=ia32
    else
        GFORTRAN_ARCHI=X$(shell gfortran -v $(ERR_TO_STD) | $(GREP) "x86_64")
        ifneq ($(GFORTRAN_ARCHI),X)
            GFORTRAN_ARCHI=amd64
        else
            GFORTRAN_ARCHI=X$(shell gfortran -v $(ERR_TO_STD) | $(GREP) "i586")
            ifneq ($(GFORTRAN_ARCHI),X)
                GFORTRAN_ARCHI=ia32
            else
                $(error 'Cannot detect gfortran architecture')
            endif
        endif
    endif
    FC_AVAILABLE+=gfortran-$(GFORTRAN_ARCHI)
endif
