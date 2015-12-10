module iso_c_binding
    implicit none
    ! --------------------------------------------------------------------------------
    ! --- ISO_C_BINDING 
    ! --------------------------------------------------------------------------------
    ! Builtin types kind
    integer, parameter :: C_INT    = 4 
    integer, parameter :: C_FLOAT  = 4
    integer, parameter :: C_DOUBLE = 8
    integer, parameter :: C_CHAR   = 1
    integer, parameter :: C_BOOL   = 1 
    
    !
    integer, parameter :: C_INTPTR_T = int_ptr_kind() ! 32/64-bits: 4/8
    integer(C_INTPTR_T), parameter :: C_NULL_FUNPTR = 0
    character(kind=C_CHAR, len=1), parameter :: C_NULL_CHAR=CHAR(0)   

    ! type(TODO):
    type C_FUNPTR
        PRIVATE
        integer :: i
    end type
    type C_PTR
        PRIVATE
        integer :: i
    end type

    ! --------------------------------------------------------------------------------
    ! --- ISO_FORTRAN_ENV
    ! --------------------------------------------------------------------------------
    integer, parameter :: INT32  = 4
    integer, parameter :: INT64  = 8
    integer, parameter :: REAL32 = 4
    integer, parameter :: REAL64 = 8


contains

    subroutine print_iso()
        print*,'C_INTPTR_T', C_INTPTR_T
        print*,'C_INT', C_INT
    end subroutine

end module iso_c_binding
