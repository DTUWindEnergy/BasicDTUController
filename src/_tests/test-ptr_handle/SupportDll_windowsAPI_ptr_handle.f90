module SupportDll
    implicit none

    interface
    ! --------------------------------------------------------------------------------
    ! --- Fundamentals 
    ! --------------------------------------------------------------------------------
    function loadlibrary( libfile ) result(file_address) &
	bind(C,NAME='LoadLibraryA')
        use iso_c_binding, only: C_PTR
        use iso_c_binding, only: C_INT
        use iso_c_binding, only: C_CHAR
        !gcc$ attributes stdcall :: loadlibrary 
        ! Function result
        type(C_PTR) :: file_address
        ! Arguments
        character(kind=C_CHAR) :: libfile(*)
        ! Variables
        integer(C_INT), parameter :: rtld_lazy=1
        !
    end function loadlibrary

    function getprocaddress( libhandle, procedure_name )&
	bind(C,NAME='GetProcAddress')
        use iso_c_binding, only: C_FUNPTR, C_PTR, C_CHAR
        !gcc$ attributes stdcall :: getprocaddress
        type(C_FUNPTR) :: getprocaddress
        ! Arguments
        type(C_PTR) :: libhandle
        character(kind=C_CHAR) :: procedure_name(*)
    end function getprocaddress
	end interface

	public :: loadlibrary, getprocaddress, freelibrary
contains
    function freelibrary( libhandle )
        use iso_c_binding, only: C_PTR
        logical :: freelibrary
        ! Arguments
        type(C_PTR) :: libhandle
        ! Variables
        integer :: err

        err = 0
        freelibrary = ( err .eq. 0 )
        return
    end function freelibrary
end module

