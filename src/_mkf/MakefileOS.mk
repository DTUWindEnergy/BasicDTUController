# This makefile was originally inspired from: http://mad-scientist.net/make/multi-arch.html
#--------------------------------------------------------------------------------
# ---  Architecture, system name, objects
# --------------------------------------------------------------------------------
ifeq ($(OS),Windows_NT)
    OS_NAME=windows
    #REG=$(shell reg query "HKLM\System\CurrentControlSet\Control\Session Manager\Environment" /v PROCESSOR_ARCHITECTURE)

    ifeq ($(PROCESSOR_ARCHITEW6432),AMD64)
        OS_ARCHI = amd64
    endif
    
    ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
        OS_ARCHI ?= amd64
    else
        OS_ARCHI ?= ia32
    endif

    OS_DEF=-DWINDOWS -D_WIN32
	# Forcing the usual preprocessor flags
    ifeq ($(ARCHI),amd64)
        OS_DEF := $(OS_DEF) -D_WIN64
    endif


    # File Extensions
    o=obj
    lib=lib
    dll=dll
    EXE=.exe

else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
         OS_NAME=linux
    else ifeq ($(UNAME_S),Darwin)
        OS_NAME=mac
    endif
    UNAME_P := $(shell uname -p)
    UNAME_M := $(shell uname -m)
    ifeq ($(UNAME_M),x86_64)
        OS_ARCHI=amd64
    # STUFF BELOW NEED TO BE re-tested..
    else ifneq ($(filter %86,$(UNAME_P)),)
        OS_ARCHI=ia32
    else ifneq ($(filter arm%,$(UNAME_P)),)
        OS_ARCHI=arm
    else ifneq ($(filter unknown%,$(UNAME_P)),)
        OS_ARCHI=ia32
    endif

    OS_DEF=-D__linux__ -D__unix__ -D__LINUX__ -D__UNIX__
	# Forcing the usual preprocessor flags
    ifeq ($(OS_ARCHI),amd64)
        OS_DEF := $(OS_DEF)
    endif
 
    # File Extensions
    o=o
    lib=a
    dll=so
    EXE=

endif

#--------------------------------------------------------------------------------
# ---  System Commands
# --------------------------------------------------------------------------------
ifeq ($(OS),Windows_NT)
    # System
    RM=del /q
    RMDIR=rmdir /q /s 
    LN=copy /y
    CP=copy /y
    MKDIR=mkdir 
    SLASH=/
    SLASH := $(subst /,\,$(SLASH))
    TOUCH=echo.>
    MKDEPF=makedepf90.exe
    SHELL=cmd.exe
    LD=link.exe
    LD_OUT=/out:
    LD_DLL=/nologo /dll
    AR=Lib
    AR_OUT=/out:
    CAT=type
    GREP=find /n 
    ECHOSAFE=echo(
    ERR_TO_NULL= 2>nul
    ERR_TO_STD= 2>&1
    RUN_NL=&
    EXEC=
    GREP_STATUS =X$(shell grep --version $(ERR_TO_NULL))
    ifneq ($(GREP_STATUS),X)
        GREP=grep
    endif

else
    # System
    RM=rm -rf
    RMDIR=rm -rf
    LN=ln -sf
    CP=cp
    MKDIR=mkdir -p
    SLASH=/
    TOUCH=touch
    MKDEPF=makedepf90
    SHELL=/bin/bash
    LD=ld
    LD_OUT=-o
    LD_DLL=
    AR=ar
    AR_OUT=
    CAT=cat
    GREP=grep
    ECHOSAFE=echo 
    RUN_NL=;
    EXEC=./
    ERR_TO_NULL= 2>/dev/null
    ERR_TO_STD= 2>&1
endif



HOSTNAME=$(shell hostname)


# --------------------------------------------------------------------------------
# --- USER Overrides 
# --------------------------------------------------------------------------------
ifneq ($(CUSTOM_LD),)
    LD=$(CUSTOM_LD)
endif
ifneq ($(CUSTOM_AR),)
    AR=$(CUSTOM_AR)
endif

