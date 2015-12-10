# --------------------------------------------------------------------------------
# --- INFO 
# --------------------------------------------------------------------------------
# This makefile can perform the following:
#  - loops over the directories and execute the common rules: all, clean, purge
#  - perform a batch compilation for different CONFIG (e.g. compilers, DEBUG)

# --------------------------------------------------------------------------------
# ---  Detection of possible configs
# --------------------------------------------------------------------------------
# ---  Detection of OS
include _mkf/MakefileOS.mk
# ---- Detection of Compilers
include _mkf/MakefileFortran_ifort_Checks.mk
include _mkf/MakefileFortran_gfortran_Checks.mk
include _mkf/MakefileFortran_compaq_Checks.mk
# --------------------------------------------------------------------------------
# --- Subdirectories 
# --------------------------------------------------------------------------------
# Defining dependencies between sub-directories
SUBDIRS  = dtu_we_controller dtu_we_controller_bladed

# Creating rules by adding suffixes to directories
SUBCLEAN = $(addsuffix .clean,$(SUBDIRS))
SUBALL   = $(addsuffix .all,$(SUBDIRS))
SUBPURGE = $(addsuffix .purge,$(SUBDIRS))

# --------------------------------------------------------------------------------
# ---  General rules
# --------------------------------------------------------------------------------
.PHONY: clean all

all: $(SUBALL)
	@echo "[ OK ] make all in subfolders"
	@echo ""

clean: $(SUBCLEAN)
	@echo "[ OK ] make clean in subfolders"
	@echo ""

purge: $(SUBPURGE) 
	@$(MKDIR) DUMMY
	@$(RMDIR) DUMMY $(wildcard _lib*) $(ERR_TO_NULL)
	@echo "[ OK ] make purge in subfolders"
	@echo ""

# --------------------------------------------------------------------------------
# ---  Subdirectories rules
# --------------------------------------------------------------------------------
$(SUBALL): %.all:
	@$(MAKE) -C $* all
$(SUBCLEAN):%.clean:
	@$(MAKE) -C $* clean
$(SUBPURGE):%.purge:
	@$(MAKE) -C $* purge



# --------------------------------------------------------------------------------
# --- Batch compilation
# --------------------------------------------------------------------------------
batch:
ifeq ($(IFORT_STATUS),1)
	make FCOMPILER=1 RELEASE=1
endif
ifeq ($(GFORTRAN_STATUS),1)
	make FCOMPILER=0 RELEASE=1
endif
	@echo "[ OK ] Batch compilation"



# --------------------------------------------------------------------------------
# --- Automatic generation of def files based on bind(c) 
# --------------------------------------------------------------------------------
def:
	python ./simple-fortran-parser/fortran_signature_def_gen.py dtu_we_controller$(SLASH)dtu_we_controller.f90
	python ./simple-fortran-parser/fortran_signature_def_gen.py dtu_we_controller_bladed$(SLASH)dtu_we_controller_bladed.f90
# --------------------------------------------------------------------------------
# --- Automatic generation of header files 
# --------------------------------------------------------------------------------
headers:
	python ./simple-fortran-parser/fortran_signature_gen.py dtu_we_controller$(SLASH)dtu_we_controller.f90
	python ./simple-fortran-parser/fortran_signature_gen.py dtu_we_controller_bladed$(SLASH)dtu_we_controller_bladed.f90


# --------------------------------------------------------------------------------
# --- SIMPLE RULES 
# --------------------------------------------------------------------------------
include _mkf/MakefileSimpleRules.mk

