# --------------------------------------------------------------------------------
# --- NOTE 
# --------------------------------------------------------------------------------
# This dll tester should work on linux with ifort and gfortran
# --------------------------------------------------------------------------------
# --- Detecting OS and compilers 
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileOS.mk
include ../../_mkf/MakefileFortran_gfortran_Checks.mk
include ../../_mkf/MakefileFortran_ifort_Checks.mk
# --------------------------------------------------------------------------------
# ---   Dll and routines we will test
# --------------------------------------------------------------------------------
PROC_NAME=init_regulation
ifeq ($(OS_NAME),windows)
    LIB_NAME=dtu_we_controller.dll
else
    LIB_NAME=libdtu_we_controller.so
endif
# --------------------------------------------------------------------------------
# --- Defining rules depending on which compiler is available 
# --------------------------------------------------------------------------------
RULES=
ifeq ($(IFORT_STATUS),1)
    ifeq ($(OS_NAME),windows)
        $(warning "This implementation does not work on windows with ifort for now")
    else
        RULES+=ifort-all
    endif
endif
ifeq ($(GFORTRAN_STATUS),1)
    RULES+=gfortran-all
endif


SRC=SupportDll_posix.f90 main.f90
ifeq ($(OS_NAME),windows)
	# Compiles but test is crashing
# SRC=SupportDll_windowsAPI_ptr_handle.f90 main.f90 
endif

# --------------------------------------------------------------------------------
# ---  
# --------------------------------------------------------------------------------

all: $(RULES)

gfortran-all: FC=gfortran
gfortran-all: FC_NAME=gfortran
gfortran-all: FC_ARCHI=$(GFORTRAN_ARCHI)
gfortran-all: CONFIG=$(OS_NAME)-$(FC_ARCHI)-$(FC_NAME)
gfortran-all: EXE_NAME=dll_tester-$(CONFIG)
gfortran-all: gfortran-compile test-gfortran clean-gfortran

ifort-all: FC=ifort
ifort-all: FC_NAME=intel
ifort-all: FC_ARCHI=$(IFORT_ARCHI)
ifort-all: CONFIG=$(OS_NAME)-$(FC_ARCHI)-$(FC_NAME)
ifort-all: EXE_NAME=dll_tester-$(CONFIG)
ifort-all: FFLAGS=-nologo
ifort-all: ifort-compile test-ifort clean-ifort

gfortran-compile:
	@echo "------------------------------------------------------------------"
	@echo "- $(FC) compilation"
	@echo "------------------------------------------------------------------"
ifeq ($(OS_NAME),windows)
	$(FC) $(FFLAGS) $(SRC) -o $(EXE_NAME)
else
	$(FC) $(FFLAGS) $(SRC) -ldl  -o $(EXE_NAME)
endif
	@echo ""

ifort-compile:
	@echo "------------------------------------------------------------------"
	@echo "- $(FC) compilation"
	@echo "------------------------------------------------------------------"
ifeq ($(OS_NAME),windows)
	$(FC) $(FFLAGS) $(SRC3) -o $(EXE_NAME)
else
	$(FC) $(FFLAGS) $(SRC) -o $(EXE_NAME)
endif
	@echo ""


# Finding all possible dlls we can load
test-%:LIB_AVAIL=$(wildcard ../../_lib-$(OS_NAME)-$(FC_ARCHI)*/$(LIB_NAME))
test-%:
	@echo "------------------------------------------------------------------"
	@echo "- Using $(EXE_NAME) for $(LIB_NAME) and $(PROC_NAME)"
	@echo "------------------------------------------------------------------"
	@$(foreach LIB,$(LIB_AVAIL), \
		echo "" $(RUN_NL) \
		echo ">>> Testing:"$(LIB) $(RUN_NL)  \
		echo "$(EXEC)$(EXE_NAME) $(LIB) $(PROC_NAME)" $(RUN_NL)  \
		$(EXEC)$(EXE_NAME) $(LIB) $(PROC_NAME)  $(RUN_NL) )

	
clean-%:	
	@$(RM) a.out *.mod *.obj *.o 
clean:	
	@$(RM) a.out *.mod *.obj *.o 
purge:	
	@$(RM) dll_tester*


include ../../_mkf/MakefileSimpleRules.mk
