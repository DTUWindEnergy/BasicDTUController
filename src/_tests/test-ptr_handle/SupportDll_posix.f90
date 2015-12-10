module SupportDll
    implicit none

    ! --------------------------------------------------------------------------------
    ! ---  Interface to linux API
    ! --------------------------------------------------------------------------------
    interface
        function dlopen(filename,mode) bind(c,name="dlopen")
            ! void *dlopen(const char *filename, int mode);
            use iso_c_binding, only: C_PTR
            use iso_c_binding, only: C_INT, C_CHAR
            type(C_PTR) :: dlopen
            character(C_CHAR),  dimension(*),intent(in) :: filename
            integer(C_INT), value :: mode
        end function

        function dlsym(handle,name) bind(c,name="dlsym")
            ! void *dlsym(void *handle, const char *name);
            use iso_c_binding, only: C_FUNPTR, C_PTR
            use iso_c_binding, only: C_CHAR
            type(C_FUNPTR)     :: dlsym
            type(C_PTR), value :: handle
            character(C_CHAR), dimension(*), intent(in) :: name
        end function

        function dlclose(handle) bind(c,name="dlclose")
            ! int dlclose(void *handle);
            use iso_c_binding, only: C_PTR
            use iso_c_binding, only: C_INT
            integer(C_INT) :: dlclose
            type(C_PTR), value :: handle
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
        use iso_c_binding, only: C_PTR
        use iso_c_binding, only: C_INT
        ! Function result
        type(C_PTR) :: file_address
        ! Arguments
        character(*) :: libfile
        ! Variables
        integer(C_INT), parameter :: rtld_lazy=1
        !
        file_address = dlopen( libfile, rtld_lazy )
    end function loadlibrary

    function getprocaddress( libhandle, procedure_name )
        !!gcc$ attributes stdcall :: getprocaddress
        use iso_c_binding, only: C_FUNPTR, C_PTR
        type(C_FUNPTR) :: getprocaddress
        ! Arguments
        type(C_PTR) :: libhandle
        character(*) :: procedure_name
        getprocaddress = dlsym( libhandle, procedure_name )
    end function getprocaddress

    ! --------------------------------------------------------------------------------
    ! --- Wrap functions 
    ! --------------------------------------------------------------------------------
    function loadsymbol(libhandle,s)
        use iso_c_binding, only: C_FUNPTR,C_PTR
        type(C_FUNPTR) :: loadsymbol
        ! Arguments
        type(C_PTR) :: libhandle
        character*(*) :: s
        !
        s(LEN_trim(s)+1:LEN_trim(s)+1) = CHAR(0)
        loadsymbol = getprocaddress(libhandle,s)
    end function

    function freelibrary( libhandle )
        use iso_c_binding, only: C_PTR
        logical :: freelibrary
        ! Arguments
        type(C_PTR) :: libhandle
        ! Variables
        integer :: err

        err = dlclose( libhandle )
        freelibrary = ( err .eq. 0 )
        return
    end function freelibrary
end module SupportDll
