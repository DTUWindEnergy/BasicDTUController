module safety_system_mod
   use dtu_we_controller_fcns
   implicit none
   type(TSafetySystem) SafetySystemVar
contains
subroutine safety_system(stepno, deltat, omega, TTAccVect, EmergPitchStop, ActiveMechBrake, dump_array)
   !
   ! Safety system. Higher level of system monitoring.
   ! 
   ! Input/Output
   ! ------------
   ! dump_array:
   !    Array for outputs printing.
   ! Inputs
   ! ------
   ! stepno:
   !    Time step number.
   ! deltat:
   !    Time step size.
   ! omega:
   !    Measured rotor speed.
   ! TTAccVect:
   !    Measured tower top acceleration.
   ! Outputs
   ! -------
   ! EmergPitchStop
   !    Flag for emergency pitch stop.
   ! ActiveMechBrake
   !    Flag for emergency brake activation.
   !
   integer, intent(in) :: stepno
   real(mk), intent(in) :: omega, TTAccVect(2), deltat
   integer, intent(out) :: EmergPitchStop, ActiveMechBrake
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
   endif
   !***********************************************************************************************
   ! Tower top acceleration monitoring
   !***********************************************************************************************
   if (TTAccFilt .gt. SafetySystemVar%RysteVagtLevel) then
      EmergPitchStop = 1
      ActiveMechBrake = 1
   endif
end subroutine
end module safety_system_mod