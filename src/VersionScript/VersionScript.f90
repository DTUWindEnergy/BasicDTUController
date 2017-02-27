program VersionScript
! The program is called by VS in a pre-built step and it creates two fortran INCLUDE blocks 
! for the revision number (called 'svn_revision.f90') and About box 
character text35*35
integer*4 i
logical::notfound=.true.
open(222,file='git_version.txt')
read(222,*) text35
close(222)
open(223,file='dtu_we_controller/git_version.inc')
write(223,'(a)')  'text35='''//trim(adjustl(text35))//''''
close(223)
stop
end program VersionScript
