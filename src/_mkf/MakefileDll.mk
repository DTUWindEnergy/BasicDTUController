# --------------------------------------------------------------------------------
# --- Setup Fortran Compiler
# --------------------------------------------------------------------------------
include ../_mkf/MakefileFortran.mk

# Note: This is a default makefile to compile a library

# --------------------------------------------------------------------------------
# --- Defining a configuration string (e.g. windows-amd64-ifort-debug)
# --------------------------------------------------------------------------------
CONFIG=$(strip $(OS_NAME))-$(strip $(FC_ARCHI))-$(strip $(FC_NAME))
ifeq ($(RELEASE),0)
    CONFIG:=$(CONFIG)-debug
endif


# --------------------------------------------------------------------------------
# --- Defining variables based on OS and fortran
# --------------------------------------------------------------------------------
LIB_DIR=$(LIB_DIR_BASE)-$(CONFIG)
INC_DIR=$(LIB_DIR)$(SLASH)$(INC_DIR_BASE)
OBJ_DIR=$(OBJ_DIR_BASE)-$(CONFIG)
LIB_NAME= $(LIB_NAME_BASE)
ifeq ($(OS_NAME),linux)
    LIB_NAME=lib$(LIB_NAME_BASE)
endif

ifeq ($(MAKE_STATIC),1)
    RULES+= $(LIB_DIR)$(SLASH)$(LIB_NAME).$(lib)
endif
ifeq ($(MAKE_DLL),1)
    RULES+= $(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll)
endif
# --------------------------------------------------------------------------------
# --- INCLUDES 
# --------------------------------------------------------------------------------
INCS=-I$(INC_DIR)
INCS+=$(INC_EXTRA)
# --------------------------------------------------------------------------------
# --- DEFINITIONS
# --------------------------------------------------------------------------------
DEFS=$(OS_DEF) -D__MAKEFILE__
DEFS+=$(DEFS_EXTRA)
# --------------------------------------------------------------------------------
# --- LIBS
# --------------------------------------------------------------------------------
LIBS=
LIBS+=$(LIBS_EXTRA)
# --------------------------------------------------------------------------------
# --- Compiler Flags 
# --------------------------------------------------------------------------------
FFLAGS    = $(FF_NOLOGO) $(FF_MODINC)$(OBJ_DIR)
FFLAGS   += $(FF_DLL)
ifeq ($(RELEASE),0)
    FFLAGS   += $(FF_DEBUGINFO) $(FF_DEBUG) $(FF_PE) $(FF_WARN) $(FF_WARNEXTRA) $(FF_OPT0)
    #FFLAGS   += $(FF_WARNERROR) 
    FFLAGS   += $(FF_TRACE)
    BUILD=debug
else
    FFLAGS   += $(FF_OPTO5)
    BUILD=release
endif
FFLAGS   += $(FFLAGS_EXTRA)
#
# --------------------------------------------------------------------------------
# ---  ARCHIVER flags
# --------------------------------------------------------------------------------
ifeq ($(AR),Lib)
    ARFLAGS=$(FFNOLOGO)
else
	# v: verbose
	# r: insert with replacement
	# c: create
	# q: quickly append without checking for replacements
    #ARFLAGS=-cq 
    ARFLAGS=-r
endif
ARFLAGS+= $(ARFLAGS_EXTRA)

# --------------------------------------------------------------------------------
# --- Linker flags 
# --------------------------------------------------------------------------------
ifeq ($(OS_NAME),windows)
    ifeq ($(LD),ld)
        # We erase LD
        LD=$(FC) -shared $(LIB_NAME).def 
#         LDFLAGS=-Wl,--enable-runtime-pseudo-reloc,-no-undefined
    else
	    # WINDOWS - IFORT
        LDFLAGS=$(LD_DLL) /def:$(LIB_NAME).def  
    endif
endif
LDFLAGS+= $(LDFLAGS_EXTRA)


# --------------------------------------------------------------------------------
# --- Defining Objects based on SRC
# --------------------------------------------------------------------------------
# Setting up objects
OBJ:= $(patsubst %.f90,%.$(o),$(SRC)) 
OBJ:= $(patsubst %.F90,%.$(o),$(OBJ))
OBJ:= $(patsubst %.for,%.$(o),$(OBJ))
OBJ:= $(patsubst %,$(OBJ_DIR)/%,$(OBJ))


vpath %.f90 
vpath %.F90
vpath %.for

# --------------------------------------------------------------------------------
# --- Main rules  
# --------------------------------------------------------------------------------
.PHONY: lib all clean flags

all: $(RULES)

clean:OBJ_DIRS:=$(wildcard $(OBJ_DIR_BASE)*)
clean:
	@mkdir DUMMY
	@$(RMDIR) DUMMY $(OBJ_DIRS) $(ERR_TO_NULL)
	@echo "[ OK ] $(LIB_NAME_BASE) lib cleaned"
	@echo ""

purge: clean
	@$(RM) $(LIB_DIR)$(SLASH)$(LIB_NAME)* $(ERR_TO_NULL)
	@echo "[ OK ] $(LIB_NAME_BASE) lib purged"
	@echo ""


# --------------------------------------------------------------------------------
# ---  Static library
# --------------------------------------------------------------------------------
$(LIB_DIR)$(SLASH)$(LIB_NAME).$(lib): $(LIB_DIR) $(INC_DIR) $(OBJ_DIR) $(OBJ)
	@echo "----------------------------------------------------------------------"
	@echo "- Compiling static library:  " $(LIB_DIR)$(SLASH)$(LIB_NAME).$(lib)
	@echo "----------------------------------------------------------------------"
	$(AR) $(ARFLAGS) $(AR_OUT)$(LIB_DIR)$(SLASH)$(LIB_NAME).$(lib) $(OBJ)
	@$(TOUCH) $(OBJ_DIR)$(SLASH)dummy.mod
	@$(CP) $(OBJ_DIR)$(SLASH)*.mod $(INC_DIR)
	@$(RM) $(OBJ_DIR)$(SLASH)dummy.mod
	@$(RM) $(INC_DIR)$(SLASH)dummy.mod
	@echo "[ OK ] Compilation of static library $(LIB_NAME)"
	@echo ""

# --------------------------------------------------------------------------------
# ---  DLL library
# --------------------------------------------------------------------------------
$(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll): $(LIB_DIR) $(INC_DIR) $(OBJ_DIR) $(OBJ)
	@echo "----------------------------------------------------------------------"
	@echo "- Compiling dynamic library: " $(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll)
	@echo "----------------------------------------------------------------------"
ifeq ($(OS_NAME),windows)
	$(LD) $(LDFLAGS)  $(LD_OUT)"$(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll)" $(OBJ_DIR)$(SLASH)*.$(o)  $(LIBS) 
# 	dlltool -z $(LIB_NAME).def --export-all-symbols $(OBJ_DIR)$(SLASH)\*.$(o) -e exports.o
#     gcc dll.o exports.o -o dll.dll
else
# 	$(FC) $(DEFS) $(INCS) -shared $(LDFLAGS) -Wl,-soname,$(LIB_NAME).$(dll).1  $(OBJ_DIR)$(SLASH)*.$(o) $(LIBS) $(LD_OUT)$(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll) 
	$(FC) $(DEFS) $(INCS) -shared  $(LD_OUT)$(LIB_DIR)$(SLASH)$(LIB_NAME).$(dll) $(LDFLAGS) -Wl,-soname,$(LIB_NAME).$(dll).1 $ $(OBJ_DIR)$(SLASH)*.$(o) $(LIBS)
endif
	@echo "[ OK ] Compilation of dynamic library $(LIB_NAME)"
	@echo ""


# --------------------------------------------------------------------------------
# --- Low-level Compilation rules 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileDefaultCompile.mk


# --------------------------------------------------------------------------------
# --- DEPENDENCIES 
# --------------------------------------------------------------------------------
# Creating build directory
$(OBJ_DIR):
	@make --no-print-directory flags
	@$(MKDIR) $(OBJ_DIR)

$(LIB_DIR):
	@$(MKDIR) $(LIB_DIR)

$(INC_DIR):
	@$(MKDIR) $(INC_DIR)

# --------------------------------------------------------------------------------
# --- SIMPLE RULES 
# --------------------------------------------------------------------------------
include ../_mkf/MakefileSimpleRules.mk
