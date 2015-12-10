module SupportCommandArgs
    implicit none
    contains
        !
       integer function command_argument_count() result(n)
           use DFLIB
           n=NARGS()-1
           print*,'NARGS',NARGS(),n
       end function

       subroutine get_command_argument(i,arg) 
           use DFLIB
           integer, intent(in) :: i
           character(len=*), intent(inout) :: arg
           call getarg(i,arg)
       end subroutine
end module
