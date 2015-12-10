!> This modules is intended to be used for linux implementations with gfortran or intel fortran compiler
module SupportDll
    implicit none

    ! --------------------------------------------------------------------------------
    ! ---  Interface to linux API
    ! --------------------------------------------------------------------------------
    interface
        function dlopen(filename,mode) bind(c,name="dlopen")
            ! void *dlopen(const char *filename, int mode);
            use iso_c_binding, only: C_INTPTR_T
            use iso_c_binding, only: C_INT, C_CHAR
            integer(C_INTPTR_T) :: dlopen
            character(C_CHAR),  dimension(*),intent(in) :: filename
            integer(C_INT), value :: mode
        end function

        function dlsym(handle,name) bind(c,name="dlsym")
            ! void *dlsym(void *handle, const char *name);
            use iso_c_binding, only: C_INTPTR_T
            use iso_c_binding, only: C_CHAR
            integer(C_INTPTR_T)        :: dlsym
            integer(C_INTPTR_T), value :: handle
            character(C_CHAR), dimension(*), intent(in) :: name
        end function

        function dlclose(handle) bind(c,name="dlclose")
            ! int dlclose(void *handle);
            use iso_c_binding, only: C_INTPTR_T
            integer(C_INTPTR_T)        :: dlclose
            integer(C_INTPTR_T), value :: handle
        end function
    end interface

    private
    public :: loadlibrary, getprocaddress, loadsymbol, freelibrary


contains
    ! --------------------------------------------------------------------------------
    ! --- Fundamentals 
    ! --------------------------------------------------------------------------------
    function loadlibrary( libfile ) result(file_address)
        !!gcc$ attributes stdcall :: loadlibrary 
        use iso_c_binding, only: C_INTPTR_T
        use iso_c_binding, only: C_INT
        ! Function result
        integer(C_INTPTR_T) :: file_address
        ! Arguments
        character(*) :: libfile
        ! Variables
        integer(C_INT), parameter :: rtld_lazy=1
        !
        file_address = dlopen( libfile, rtld_lazy )
    end function loadlibrary

    function getprocaddress( libhandle, procedure_name )
        !        !gcc$ attributes stdcall :: getprocaddress
        use iso_c_binding, only: C_INTPTR_T
        integer(C_INTPTR_T) :: getprocaddress
        ! Arguments
        integer(C_INTPTR_T) :: libhandle
        character(*) :: procedure_name
        getprocaddress = dlsym( libhandle, procedure_name )
    end function getprocaddress

    ! --------------------------------------------------------------------------------
    ! --- Wrap functions 
    ! --------------------------------------------------------------------------------
    function loadsymbol(libhandle,s)
        use iso_c_binding, only: C_INTPTR_T
        integer(C_INTPTR_T)  :: loadsymbol
        ! Arguments
        integer(C_INTPTR_T) :: libhandle
        character*(*) :: s
        !
        s(LEN_trim(s)+1:LEN_trim(s)+1) = CHAR(0)
        loadsymbol = getprocaddress(libhandle,s)
    end function

    function freelibrary( libhandle )
        use iso_c_binding, only: C_INTPTR_T
        logical :: freelibrary
        ! Arguments
        integer(C_INTPTR_T) :: libhandle
        ! Variables
        integer :: err

        err = dlclose( libhandle )
        freelibrary = ( err .eq. 0 )
        return
    end function freelibrary
end module SupportDll
