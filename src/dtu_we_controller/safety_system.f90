module safety_system_mod
   use dtu_we_controller_fcns
   implicit none
   type(TSafetySystem) SafetySystemVar
contains
subroutine safety_system(stepno, deltat, omega, TTAccVect, EmergPitchStop, ActiveMechBrake, dump_array)
   !
   ! Safety system. Higher level of system monitoring. It activates the emergency pitch stop and 
   ! the mechanical brake if the rotor speed or the tower top acceleration exceed certain levels.
   !
   integer, intent(in) :: stepno ! Time step number.
   real(mk), intent(in) :: omega ! Measured rotor speed.
   real(mk), intent(in) :: TTAccVect(2)    ! Measured tower top acceleration. Longitudinal and &
                                           ! lateral components [m/s**2].
   real(mk), intent(in) :: deltat          ! Time step size.
   integer, intent(out) :: EmergPitchStop  ! Flag for emergency pitch stop.
   integer, intent(out) :: ActiveMechBrake ! Flag for emergency brake activation.
   real(mk), intent(inout) :: dump_array(50)
   real(mk) omegafilt, y(2), TTAccFilt, TTAcc
   ! Low-pass filtering of the rotor speed
   y = lowpass2orderfilt(deltat, stepno, SafetySystemVar%omega2ordervar, omega)
   omegafilt = y(1)
   ! Low-pass filtering of the nacelle wind speed
   TTAcc = dsqrt(TTAccVect(1)**2 + TTAccVect(2)**2)
   TTAccFilt = lowpass1orderfilt(deltat, stepno, SafetySystemVar%rystevagtfirstordervar, TTAcc)
   !***********************************************************************************************
   ! Overspeed
   !***********************************************************************************************
   if (omegafilt .gt. SafetySystemVar%overspeed) then
      EmergPitchStop = 1
      ActiveMechBrake = 1
      write(6,'(a,f6.2,a,f6.2,a)') 'Safety system alarm: Filtered generator speed = ',omegafilt,' rad/s. Threshold = ',SafetySystemVar%overspeed,' rad/s'
   endif
   !***********************************************************************************************
   ! Tower top acceleration monitoring
   !***********************************************************************************************
   if (TTAccFilt .gt. SafetySystemVar%RysteVagtLevel) then
      EmergPitchStop = 1
      ActiveMechBrake = 1
      write(6,'(a,f6.2,a,f6.2,a)') 'Safety system alarm: Filtered tower acceleration = ',TTAccFilt,' m/s^2. Threshold = ',SafetySystemVar%RysteVagtLevel,' m/s^2'
   endif
end subroutine
end module safety_system_mod
