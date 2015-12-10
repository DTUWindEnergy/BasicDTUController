@echo OFF
:: --------------------------------------------
:: --- Setting up Visual Studio
:: --------------------------------------------
:: (you need to have the proper folder of Visual Studio in your environment variable PATH)
:: For Example: C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC
::call vcvarsall.bat x86
call vcvarsall.bat amd64

:: --------------------------------------------
:: --- Setting up ifortran 
:: --------------------------------------------
:: (you need to have the folder bin of intel in your environment variable PATH)
:: For example:  C:\Program Files (x86)\Intel\ComposerXE-2011\bin
::call ifortvars.bat ia32 vs2010
call ifortvars.bat intel64 vs2010

cmd

