# --------------------------------------------------------------------------------
# --- Description 
# --------------------------------------------------------------------------------
# This Makefile perform checks for the compaq compiler :
#  - If the compiler is available, the variable STATUS is set to 1
#  - The architecture the compiler will use to compile is set in ARCHI
#  - The variable FC_AVAILABLE is incremented
#
# This makefile requires MakefileOS

# --------------------------------------------------------------------------------
# ---  Requirements
# --------------------------------------------------------------------------------
ifeq ($(ERR_TO_STD),)
    $(warning 'MakefileFortran_compaq_Checks needs MakefileOS' )
    ERR_TO_STD=2>&1
    GREP=grep
endif

# --------------------------------------------------------------------------------
# --- Compaq Fortran  
# --------------------------------------------------------------------------------
COMPAQ_STATUS=X$(shell f90 $(ERR_TO_STD) | $(GREP) "Compaq")
ifeq ($(COMPAQ_STATUS),X)
    COMPAQ_STATUS=0
else
    COMPAQ_STATUS=1
	# --- Detecting architecture for compilation
    COMPAQ_ARCHI=ia32
    FC_AVAILABLE+=compaq-$(COMPAQ_ARCHI)
endif
