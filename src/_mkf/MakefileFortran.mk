f=f90 
FC      =NOT_SET
FC_NAME =NOT_SET
FC_DEF  =NOT_SET
FC_ARCHI=NOT_SET

# mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
# current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
SELF_DIR := $(dir $(lastword $(MAKEFILE_LIST)))
# --------------------------------------------------------------------------------
# --- Setup of compiler variables   FC_* and flags FF_*
# --------------------------------------------------------------------------------
# GFORTRAN COMPILER
ifeq ($(FCOMPILER),0)
include $(SELF_DIR)MakefileFortran_gfortran.mk
endif

# INTEL FORTRAN COMPILER
ifeq ($(FCOMPILER),1)
include $(SELF_DIR)MakefileFortran_ifort.mk
endif

# SUN COMPILER
ifeq ($(FCOMPILER),2)
    include $(SELF_DIR)MakefileFortran_sun.mk
endif

# COMPAQ COMPILER
ifeq ($(FCOMPILER),3)
    include $(SELF_DIR)MakefileFortran_compaq.mk
endif

# --------------------------------------------------------------------------------
# ---  USER OVERRIDE
# --------------------------------------------------------------------------------
ifneq ($(CUSTOM_FC),)
    FC=$(CUSTOM_FC)
endif

# --------------------------------------------------------------------------------
# --- MKL LIBRARY 
# --------------------------------------------------------------------------------
# include MakefileMKL.mk




# --------------------------------------------------------------------------------
# ---  MPI (NASTY FOR NOW)
# --------------------------------------------------------------------------------
MPIFC  =   mpif90
MPIRUN =   mpirun -n $(PPN)
RUN =   mpirun 
ifeq ($(strip $(HOSTNAME)),jess.dtu.dk)
    MPIFC  =   mpiifort
    MPIRUN =   mpirun -n $(PPN)
    RUN    =   mpirun 
endif
ifeq ($(strip $(PBS_O_HOST)),jess.dtu.dk)
    MPIFC  =   mpiifort
    MPIRUN =   mpirun -n $(PPN)
    RUN    =   mpirun 
endif
ifeq ($(strip $(HOSTNAME)),g-000.risoe.dk)
    MPIFC  =   mpiifort
    MPIRUN =   mpirun -n $(PPN)
    RUN    =   mpirun 
endif
ifeq ($(strip $(HOSTNAME)),work)
ifeq ($(FCOMPILER),0)
MPIFC  =   mpif90.openmpi
MPIRUN =   mpirun.openmpi -n $(PPN)
    RUN    =   mpirun.openmpi
endif
endif
ifeq ($(strip $(HOSTNAME)),olympe)
ifeq ($(FCOMPILER),0)
MPIFC  =   mpif90.openmpi
MPIRUN =   mpirun.openmpi -n $(PPN)
RUN    =   mpirun.openmpi
endif
endif

