module turbine_controller_derate_mod
!**************************************************************************************************
! Power Reference definition based on de-rate strategies 
!**************************************************************************************************
use dtu_we_controller_fcns
implicit none

!type(PRefdata), save   :: PdDatavar    

contains

! ** INIT_POWER_DERATE
subroutine init_power_derate 
! Read ppdata file
! Read Regulation parameters
! OPdatavar = OPdatavardr in derating strategy
!real(mk),dimension(3)	:: array1	! Input array, from hawc2 to dll
!real(mk),dimension(1)	:: array2	! Array .dll -> hawc2
!character :: Ref_der%str 
integer i, ifejl
write(6,*) 'Power reference subroutine init accessed'    ! Init message
! ************
! Read pddata , derating archive and store it in OPdatavardr%pdata file 
open(88,file=Ref_der%str)                                                
read(88,*,iostat=ifejl) ! BLANK LINE 
read(88,*,iostat=ifejl) Ref_der%Control_method
read(88,*,iostat=ifejl) Ref_der%Strategy
read(88,*,iostat=ifejl) Ref_der%TSR
read(88,*,iostat=ifejl) Ref_Der%CPVal
read(88,*,iostat=ifejl) Ref_der%MaxRate
read(88,*,iostat=ifejl) Ref_der%DRmin
read(88,*,iostat=ifejl) Ref_der%Wrho
read(88,*,iostat=ifejl) PratedF%tau
read(88,*,iostat=ifejl) Koptfirstordervar%tau 
read(88,*,iostat=ifejl) Ref_der%lines


write(0,*) ' nlnes ' , Ref_der%lines
if(PratedF%tau.ne.-1)  PratedF%tau = PratedF%tau*2.0_mk*pi
if( Koptfirstordervar%tau.ne.-1)   Koptfirstordervar%tau  =  Koptfirstordervar%tau *2.0_mk*pi

if (ifejl.eq.0) then
  do i=1,Ref_der%lines
    read(88,*,iostat=ifejl) Ref_der%pdata(i,1), Ref_der%pdata(i,2)
    if (ifejl.ne.0) then
      write(6,*) ' *** ERROR *** Could not read lines in power reference '&
               //'table in file pdata.'
      stop
    endif
   enddo
else
  write(6,*) ' *** ERROR *** Could not read number of lines in pddata file!'
  stop
endif
   close(88)
! End read pddata file 
! ************
end subroutine init_power_derate

subroutine set_power_derate(TimeSim,deltat,stepno,WSP,PeRated0,Radius)
 
  !real(mk), dimension(10), intent(inout) :: array1
  !real(mk), dimension(10), intent(inout) :: array2
     
      ! - Local variables - !
  real(mk)             :: TimeSim, TimeSim_l, WSP, Pe_av, PeRated, RefValue, PeRated0, DeltaPrated,rho_value  ! 
  real(mk)             :: Radius, Cp_value
  real(mk)             :: PeRated_1,PeRated_2,PeRated_3,PeRated_4
  real(mk)             ::  Pmin,Perated_old,MaxPrate  
  real(mk),intent(in)  :: deltat
  integer :: stepno, Control_method
  integer              :: Ind0 = 0
  integer              :: Ind1 , Dr_Ft 

  
  !DR_V = DP_u%DR_Va
  Control_method =  Ref_der%array_ref(3)
  MaxPRate = Ref_der%array_ref(7)*1000
  Cp_value = Ref_der%array_ref(8) 
  rho_value = Ref_der%array_ref(9) 
  Pmin = Ref_der%array_ref(10)/100
  
  if (stepno.eq.1) then
  Ind0 = 2 
  else
  
  TimeSim_l = TimeSim
  
  do while(TimeSim_l.gt.Ref_der%pdata(Ind0,1))
          Ind0 = Ind0 +1
  if (Ind0.eq.Ref_der%lines) then
        TimeSim_l = TimeSim-50000
  endif
  enddo 
    endif
  
   Ind0 = min(Ind0,Ref_der%lines)
   Ind0 = Ind0 -1 
   Ind1 = min(Ind0+1,Ref_der%lines) 
    
  RefValue = interpolate2(TimeSim,Ref_der%pdata(Ind0,1),Ref_der%pdata(Ind1,1),Ref_der%pdata(Ind0,2),Ref_der%pdata(Ind1,2))
  
     if (Ind1.eq.Ref_der%lines.and.TimeSim.gt.Ref_der%pdata(Ind1,1)) then
        RefValue =  Ref_der%pdata(Ind1,2)
     endif    
   
  Pe_av = min(PeRated0, 0.5*rho_value*pi*Radius**2*WSP**3*Cp_value)
   !
   

   select case (Control_method) 
        case (1) 
            PeRated     = Pe_av*(1-RefValue*0.01_mk)
        case (2)
            PeRated     = Pe_av-PeRated0*RefValue*0.01_mk
        case (3)
            PeRated     = PeRated0*(1-RefValue*0.01_mk)
        case default
            write(0,*) 'Wrong value for Control Method.' 
            stop
        end select
 
  PeRated_1 = PeRated
  PeRated     = max(PeRated,Pmin*PeRated0)     
  PeRated_2 = PeRated     
  
    if (stepno.eq.1) then
        DP_u%PeRated_old  = PeRated
    endif
 
   DeltaPrated = PeRated-DP_u%PeRated_old                                                       
   PeRated     = max(-MaxPRate,min(MaxPRate,DeltaPrated/deltat))*deltat+DP_u%PeRated_old         
   PeRated_3 = PeRated

   if (PratedF%tau.ne.-1) then
       PeRated   = lowpass1orderfilt(deltat, stepno, PratedF, PeRated)       
   endif    
  
 
   DP_u%PeRated_old = PeRated

   Dr_Ft = 0 ;
   if (RefValue.gt.0) Dr_ft = 1 
   
   
   ! Outputs 
   
   DP_u%array_u(1) = Dr_ft                            ! Flag Derate 
   DP_u%array_u(2) = RefValue                         ! Reference value De-rate
   DP_u%array_u(3) = PeRated_1                        ! Power before limits and filters
   DP_u%array_u(4) = PeRated_2                        ! Power after limits (min derate) 
   DP_u%array_u(5) = PeRated_3                        ! Power after limit  (kw/s)
   DP_u%array_u(6) = PeRated                          ! Power after filtering
   DP_u%array_u(7) = Pe_av                            ! Available Power 
   DP_u%array_u(8) = WSP                              ! Used filter wind speed 
   DP_u%array_u(9) = DP_u%PeRated_old                 ! Power previous step
 
   
   
end subroutine set_power_derate

  function interpolate2(x, x1, x2, y1, y2)
       
       real(mk) interpolate2, x, x1, x2, y1, y2
       if (x1 .eq. x2) then
          interpolate2 = y1
       else
          interpolate2 = ((y2-y1)/(x2-x1))*(x-x1)+y1
       endif
       
       return
    end function interpolate2 

end module turbine_controller_derate_mod

  