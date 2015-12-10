program main
    use iso_c_binding, only: c_intptr_t, c_funptr, C_NULL_CHAR, C_CHAR
    use iso_c_binding, only: C_NULL_FUNPTR
    use iso_c_binding, only: c_f_procpointer
    use iso_c_binding, only: C_DOUBLE
    use SupportDll

    implicit none
    character(kind=C_CHAR,len=256) :: libfile
    character(kind=C_CHAR,len=256) :: procedure_name

    integer(C_INTPTR_T) :: library_handle ! ifwin defines the type handle
    integer(C_INTPTR_T) :: proc_address
    procedure (f_initialize_interf), pointer :: f_initialize ! if bind(c)


    ! Variables
    character(len=256) :: lib_file
    character(len=256) :: proc_name
    integer :: n
    real(C_DOUBLE), allocatable,dimension(:) :: v1
    real(C_DOUBLE), allocatable,dimension(:) :: v2
    logical :: bError
    logical :: bDebug

    abstract interface
        subroutine f_initialize_interf(a,b)
            use iso_c_binding, only: C_DOUBLE
            real(C_DOUBLE) :: a(*),b(*)
        end subroutine
    end interface

    bDebug=.true.
    ! --------------------------------------------------------------------------------
    ! --- Command line arguments 
    ! --------------------------------------------------------------------------------
    n=command_argument_count()
    if (n/=2) then 
        STOP 'Error: Not enough input argument'
    endif

    ! --------------------------------------------------------------------------------
    ! --- Loading library
    ! --------------------------------------------------------------------------------
    call get_command_argument(1, lib_file)
    libfile=trim(lib_file)//C_NULL_CHAR
    library_handle = loadlibrary(libfile)

    
    bError=(library_handle==0)
    call test(bError,'Loading dll: '//trim(lib_file),bDebug)

    ! --------------------------------------------------------------------------------
    ! --- Getting procedure address 
    ! --------------------------------------------------------------------------------
    call get_command_argument(2, proc_name)
    procedure_name=trim(proc_name)//C_NULL_CHAR
    ! ---
    proc_address = getprocaddress( library_handle, procedure_name)
    bError= (proc_address==0)
    call test(bError,'Getting procedure address for: '//trim(proc_name),bDebug)
    ! --------------------------------------------------------------------------------
    ! ---  Associating procedure address with interface_pointer
    ! --------------------------------------------------------------------------------
    call c_f_procpointer(transfer(proc_address,C_NULL_FUNPTR), f_initialize)

    allocate(v1(1:100)); v1(1:100)=10.0_C_DOUBLE  !
    allocate(v2(1));     v2(1)    = 0.0_C_DOUBLE  !
    call f_initialize(v1,v2)
    !
    bError=abs(v2(1))>1e-10
    call test(bError,'Result from dll subroutine',bDebug)
    ! 

    ! --------------------------------------------------------------------------------
    ! --- Closing dll
    ! --------------------------------------------------------------------------------
    bError= .not. freelibrary(library_handle)
    call test(bError,'Closing dll',bDebug)
    ! --------------------------------------------------------------------------------
    ! ---  
    ! --------------------------------------------------------------------------------
    call test(bError,'All tests passed for dll: '//trim(lib_file),.true.)
contains

    subroutine test(bError,msg,bPrintOK)
        logical, intent(in) :: bError
        character(len=*), intent(in) :: msg
        logical, optional, intent(in) :: bPrintOK
        if (bError) then
            print'(A)','[FAIL] '//trim(msg)
            STOP "error"
        else
            if (present(bPrintOK)) then
                if (bPrintOK) then
            print'(A)','[ OK ] '//trim(msg)
                endif 
            endif
        endif
    end subroutine
end program main
