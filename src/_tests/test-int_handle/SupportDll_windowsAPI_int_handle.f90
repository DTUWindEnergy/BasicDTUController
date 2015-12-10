!>
! To be used for compilation on windows platform with gnu fortran
module SupportDll
    use iso_c_binding
    interface 
        function LoadLibrary(libfile) bind(c,name='LoadLibraryA')
            use, intrinsic :: iso_c_binding, only: C_INTPTR_T, C_CHAR
            implicit none 
            !GCC$ ATTRIBUTES STDCALL :: LoadLibrary 
            ! Arguments
            character(KIND=C_CHAR), dimension(*) :: libfile
            ! Return
            integer(C_INTPTR_T) :: LoadLibrary 
        end function LoadLibrary 

        function GetProcAddress(libhandle, procedure_name)  &
                bind(c, name='GetProcAddress')
            use, intrinsic :: iso_c_binding, only:  &
                C_FUNPTR, C_INTPTR_T, C_CHAR
            implicit none
            !GCC$ ATTRIBUTES STDCALL :: GetProcAddress
            ! Arguments
            integer(C_INTPTR_T), VALUE :: libhandle
            character(kind=C_CHAR), dimension(*) :: procedure_name
            ! Return
            integer(C_INTPTR_T) :: GetProcAddress
            !type(c_funptr) :: getprocaddress
        end function GetProcAddress      
    end interface

    public :: loadlibrary, getprocaddress, loadsymbol, freelibrary

contains
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

        err = 0 ! TODO
        freelibrary = ( err .eq. 0 )
        return
    end function freelibrary
end module
