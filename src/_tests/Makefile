# --------------------------------------------------------------------------------
# --- INFO 
# --------------------------------------------------------------------------------
# This makefile 
#  - loops over the directories and execute the common rules: all, clean, purge

# --------------------------------------------------------------------------------
# --- Subdirectories 
# --------------------------------------------------------------------------------
# Defining dependencies between sub-directories
SUBDIRS  = test-int_handle test-cray-pointer
#test-ptr_handle is postponed for now because it failed on some windows
# it works on linux though

# Creating rules by adding suffixes to directories
SUBCLEAN = $(addsuffix .clean,$(SUBDIRS))
SUBALL   = $(addsuffix .all,$(SUBDIRS))
SUBPURGE = $(addsuffix .purge,$(SUBDIRS))

# --------------------------------------------------------------------------------
# ---  General rules
# --------------------------------------------------------------------------------
.PHONY: clean all

all: $(SUBALL)
	@echo "[ OK ] make all in test subfolders"
	@echo ""

clean: $(SUBCLEAN)
	@echo "[ OK ] make clean in test subfolders"
	@echo ""

purge: $(SUBPURGE)
	@echo "[ OK ] make purge in test subfolders"
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
