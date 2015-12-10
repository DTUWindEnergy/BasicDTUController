# --------------------------------------------------------------------------------
# --- NOTE 
# --------------------------------------------------------------------------------
# This dll tester should work on windows and linux with ifort, gfortran and compaq

# --------------------------------------------------------------------------------
# --- Detecting OS and compilers 
# --------------------------------------------------------------------------------
include ../../_mkf/MakefileOS.mk
include ../../_mkf/MakefileFortran_gfortran_Checks.mk
include ../../_mkf/MakefileFortran_ifort_Checks.mk
include ../../_mkf/MakefileFortran_compaq_Checks.mk

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
ifeq ($(COMPAQ_STATUS),1)
    RULES+=compaq-all
endif
ifeq ($(IFORT_STATUS),1)
    RULES+=ifort-all
endif
ifeq ($(GFORTRAN_STATUS),1)
    RULES+=gfortran-all
endif



# For linux
SRC                  = SupportCommandArgs_empty.f90  SupportDll_posix_int_handle.f90 main.f90
# For Windows - intel
SRC_WINAPI_IFWIN     = SupportCommandArgs_empty.f90  SupportDll_windowsAPI_ifwin.f90 main.f90
# For Windows - Gfortran
SRC_WINAPI_INT       = SupportCommandArgs_empty.f90  SupportDll_windowsAPI_int_handle.f90 main.f90
# For Windows - Compaq
SRC_WINAPI_KERNEL32  = SupportCommandArgs_compaq.f90 Support_iso_c_binding_compaq.f90 SupportDll_windowsAPI_kernel32.f90 main.f90

# --------------------------------------------------------------------------------
# ---  
# --------------------------------------------------------------------------------

all: $(RULES)

# IMPORTANT CRAY POINTER!!!:
gfortran-all: FFLAGS=-fcray-pointer    
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

compaq-all: FC=f90
compaq-all: FC_NAME=compaq
compaq-all: FC_ARCHI=$(COMPAQ_ARCHI)
compaq-all: CONFIG=$(OS_NAME)-$(FC_ARCHI)-$(FC_NAME)
compaq-all: EXE_NAME=dll_tester-$(CONFIG)
compaq-all: FFLAGS=-nologo
compaq-all: compaq-compile test-compaq clean-compaq

gfortran-compile:
	@echo "------------------------------------------------------------------"
	@echo "- $(FC) compilation"
	@echo "------------------------------------------------------------------"
ifeq ($(OS_NAME),windows)
	$(FC) $(FFLAGS) $(SRC_WINAPI_INT) -o $(EXE_NAME)
else
	$(FC) $(FFLAGS) $(SRC) -ldl  -o $(EXE_NAME)
endif
	@echo ""

ifort-compile:
	@echo "------------------------------------------------------------------"
	@echo "- $(FC) compilation"
	@echo "------------------------------------------------------------------"
ifeq ($(OS_NAME),windows)
	$(FC) $(FFLAGS) $(SRC_WINAPI_IFWIN) -o $(EXE_NAME)
else
	$(FC) $(FFLAGS) $(SRC) -o $(EXE_NAME)
endif
	@echo ""

compaq-compile:
	@echo "------------------------------------------------------------------"
	@echo "- $(FC) compilation"
	@echo "------------------------------------------------------------------"
	$(FC) $(FFLAGS) $(SRC_WINAPI_KERNEL32) /exe:$(EXE_NAME)
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
purge:	clean
	@$(RM) dll_tester*


include ../../_mkf/MakefileSimpleRules.mk
