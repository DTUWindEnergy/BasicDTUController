module turbine_controller_mod
   !
   ! Module containing the main subroutines for the wind turbine regulation.
   !
   use dtu_we_controller_fcns
   implicit none
   ! Parameters
   logical generator_cutin
   integer PartialLoadControlMode, stoptype
   real(mk) deltat
   real(mk) GenSpeedRefMax, GenSpeedRefMin, PeRated, GenTorqueRated, PitchStopAng, GenTorqueMax
   real(mk) TTfa_PWR_lower, TTfa_PWR_upper, TorqueCtrlRatio
   real(mk) Kopt, Kopt_dot, TSR_opt, R, GearRatio
   real(mk) Vcutout, Vstorm
   real(mk) Err0, ErrDot0, PitNonLin1, rel_limit
   ! Dynamic variables
   integer :: stepno = 0, w_region = 0
   real(mk) AddedPitchRate, PitchColRef0, GenTorqueRef0, PitchColRefOld, GenTorqueRefOld
   real(mk) TimerGenCutin, TimerStartup, TimerExcl, TimerShutdown, TimerShutdown2
   real(mk) GenSpeed_at_stop, GenTorque_at_stop
   real(mk) excl_flag
   real(mk)::outmax_old=0.0_mk,outmin_old=0.0_mk
   logical::fullload=.false.
   ! Types
   type(Tlowpass2order), save :: omega2ordervar
   type(Tlowpass2order), save :: power2ordervar
   type(Tfirstordervar), save :: pitchfirstordervar
   type(Tfirstordervar), save :: wspfirstordervar
   type(Tfirstordervar), save :: switchfirstordervar
   type(Tpidvar), save        :: PID_gen_var
   type(Tnotch2order), save   :: DT_mode_filt
   type(Tnotch2order), save   :: pwr_DT_mode_filt
   type(Tpid2var), save       :: PID_pit_var
   type(Tdamper), save        :: DT_damper
   type(Tdamper), save        :: TTfa_damper
   type(Tfirstordervar), save :: TTfa_PWRfirstordervar
   type(Tcutin), save         :: CutinVar
   type(Tcutout), save        :: CutoutVar
   type(Tswitch), save        :: SwitchVar
   type(TSafetySystem), save  :: MoniVar
   type(TPitchGSvar), save    :: PitchGSVar
!**************************************************************************************************
contains
!**************************************************************************************************
subroutine turbine_controller(CtrlStatus, GridFlag, GenSpeed, PitchVect, wsp, Pe, TTAccVect, &
                              GenTorqueRef, PitchColRef, dump_array)
   !
   ! Main subroutine of the wind turbine controller.
   ! It calls the system monitoring and selects the controller status (normal operation, start-up,
   ! and shut-down).
   !
   integer , intent(inout) :: CtrlStatus ! Integer indicating the status of the controller.&
                                         !  (0: Normal operation, <0: Start-up., >0: Shutdown)
   integer , intent(inout) :: GridFlag   ! Integer indicating the status of the grid.
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk), intent(in)    :: GenSpeed     ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(in)    :: Pe           ! Measured electrical power [W].
   real(mk), intent(in)    :: TTAccVect(2) ! Measured tower top acceleration. Longitudinal and &
                                           !  lateral components [m/s**2].
   real(mk), intent(out)   :: GenTorqueRef ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef  ! Reference collective pitch [rad].
   real(mk) TTAcc
   !***********************************************************************************************
   ! Wind turbine monitoring
   !***********************************************************************************************
   TTAcc = dsqrt(TTAccVect(1)**2 + TTAccVect(2)**2)
   call monitoring(CtrlStatus, GridFlag, GenSpeed, TTAcc, dump_array)
   !***********************************************************************************************
   ! Normal operation for control status CtrlStatus = 0
   !***********************************************************************************************
   if (CtrlStatus .eq. 0) then
      call normal_operation(GenSpeed, PitchVect, wsp, Pe, TTAccVect(2), GenTorqueRef, PitchColRef,&
                            dump_array)
   endif
   !***********************************************************************************************
   ! Cut-in procedure for control status CtrlStatus = -1
   !***********************************************************************************************
   if (CtrlStatus .lt. 0) then
      if (CtrlStatus .eq. -2) then ! First pitch out
         call shut_down(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, &
                        dump_array)
      else
         call start_up(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, dump_array)
      endif
   endif
   !***********************************************************************************************
   ! Cut-out procedure for control status CtrlStatus > 0
   !***********************************************************************************************
   if (CtrlStatus .gt. 0) then
      call shut_down(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, dump_array)
   endif
   !***********************************************************************************************
   ! Save reference signals
   !***********************************************************************************************
   PitchColRefOld = PitchColRef
   GenTorqueRefOld = GenTorqueRef
end subroutine
!**************************************************************************************************
subroutine normal_operation(GenSpeed, PitchVect, wsp, Pe, TTfa_acc, GenTorqueRef, PitchColRef, dump_array)
   !
   ! Controller for normal operation.
   !
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: GenSpeed     ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(in)    :: Pe           ! Measured electrical power [W].
   real(mk), intent(in)    :: TTfa_acc     ! Measured tower top longitudinal acceleration.
   real(mk), intent(out)   :: GenTorqueRef   ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef    ! Reference collective pitch [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) WSPfilt
   real(mk) GenSpeedFilt, dGenSpeed_dtFilt,PeFilt
   real(mk) PitchMean, PitchMeanFilt, PitchMin
   real(mk) GenSpeedRef_full
   real(mk) Qdamp_ref, theta_dam_ref, P_filt
   real(mk) x, y(2)
   !***********************************************************************************************
   ! Inputs and their filtering
   !***********************************************************************************************
   ! Mean pitch angle
   PitchMean = (PitchVect(1) + PitchVect(2) + PitchVect(3)) / 3.0_mk
   ! Low-pass filtering of the rotor speed
   y = lowpass2orderfilt(deltat, stepno, omega2ordervar, GenSpeed)
   GenSpeedFilt = y(1)
   dGenSpeed_dtFilt = y(2)
   ! Low pass filtered power
   y = lowpass2orderfilt(deltat, stepno, power2ordervar, Pe)
   PeFilt=y(1)
   ! Low-pass filtering of the mean pitch angle for gain scheduling
   PitchMeanFilt = lowpass1orderfilt(deltat, stepno, pitchfirstordervar, PitchMean)
   PitchMeanFilt = min(PitchMeanFilt, 30.0_mk*degrad)
   ! Low-pass filtering of the nacelle wind speed
   WSPfilt = lowpass1orderfilt(deltat, stepno, wspfirstordervar, wsp)
   ! Minimum pitch angle may vary with filtered wind speed
   PitchMin = GetOptiPitch(WSPfilt)
   !***********************************************************************************************
   ! Limit reference speed for storm control
   !***********************************************************************************************
   if (Vcutout .gt. Vstorm) then
      GenSpeedRef_full = GenSpeedRefMax - max(0.0_mk, &
                         (WSPfilt - Vstorm)/(Vcutout - Vstorm)*(GenSpeedRefMax - GenSpeedRefMin))
   else
      GenSpeedRef_full = GenSpeedRefMax
   endif
   GenSpeedRef_full = max(min(GenSpeedRef_full, GenSpeedRefMax), GenSpeedRefMin)
   !***********************************************************************************************
   ! PID regulation of generator torque
   !***********************************************************************************************
   call torquecontroller(GenSpeed, GenSpeedFilt, dGenSpeed_dtFilt, PitchMean, WSPfilt, PitchMin, &
                         GenSpeedRef_full, Pe, GenTorqueRef, dump_array)
   !***********************************************************************************************
   ! Active DT damping based on filtered rotor speed
   !***********************************************************************************************
   call drivetraindamper(GenSpeed, Qdamp_ref, dump_array)
   TimerGenCutin = TimerGenCutin + deltat
   x = switch_spline(TimerGenCutin, CutinVar%delay, 2.0_mk*CutinVar%delay)
   GenTorqueRef = min(max(GenTorqueRef + Qdamp_ref*x, 0.0_mk), GenTorqueMax)
   !***********************************************************************************************
   ! PID regulation of collective pitch angle
   !***********************************************************************************************
   call pitchcontroller(GenSpeedFilt, dGenSpeed_dtFilt, PitchMeanFilt, PeFilt, PitchMin, &
                        GenSpeedRef_full, PitchColRef, dump_array)
   !***********************************************************************************************
   ! Active Tower damping based on filtered tower top aceleration
   !***********************************************************************************************
   P_filt = lowpass1orderfilt(deltat, stepno, TTfa_PWRfirstordervar, GenTorqueRef*GenSpeedFilt)
   call towerdamper(TTfa_acc, theta_dam_ref, dump_array)
   x = switch_spline(P_filt, TTfa_PWR_lower*PeRated, TTfa_PWR_upper*PeRated)
   PitchColRef = min(max(PitchColRef + theta_dam_ref*x, PID_pit_var%outmin), PID_pit_var%outmax)
   ! Write into dump array
   dump_array(1) = GenTorqueRef*GenSpeed
   dump_array(2) = WSPfilt
   dump_array(3) = GenSpeedFilt
   dump_array(20) = PitchMeanFilt
end subroutine
!**************************************************************************************************
subroutine start_up(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, dump_array)
   !
   ! Start-up procedures.
   !
   integer,  intent(inout) :: CtrlStatus ! Integer indicating the status of the controller.&
                                         !  (0: Normal operation, <0: Start-up., >0: Shutdown)
   real(mk), intent(in)    :: GenSpeed     ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(out)   :: GenTorqueRef ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef  ! Reference collective pitch [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) WSPfilt, GenSpeedFilt, dGenSpeed_dtFilt, PitchMin
   real(mk) GenSpeedFiltErr, err_pitch(2)
   real(mk) kgain_torque(3), kgain_pitch(3, 2)
   real(mk) dummy, y(2), PitchMean, PitchMeanFilt, GenSpeedRef
   ! Low-pass filtering of the rotor speed
   y = lowpass2orderfilt(deltat, stepno, omega2ordervar, GenSpeed)
   GenSpeedFilt = y(1)
   dGenSpeed_dtFilt = y(2)
   ! Mean pitch angle
   PitchMean = (PitchVect(1) + PitchVect(2) + PitchVect(3)) / 3.0_mk
   PitchMeanFilt = lowpass1orderfilt(deltat, stepno, pitchfirstordervar, PitchMean)
   PitchMeanFilt = min(PitchMeanFilt, 30.0_mk*degrad)
   ! Low-pass filtering of the nacelle wind speed
   WSPfilt = lowpass1orderfilt(deltat, stepno, wspfirstordervar, wsp)
   ! Minimum pitch angle may vary with filtered wind speed
   PitchMin = GetOptiPitch(WSPfilt)
   select case (PartialLoadControlMode)
   case (1)
      if (GenSpeedFilt .gt. 0.5_mk*(GenSpeedRefMax + GenSpeedRefMin)) then
         GenSpeedRef = GenSpeedRefMax
      else
         GenSpeedRef = GenSpeedRefMin
       endif
    case (2)
      GenSpeedRef = min(max(WSPfilt*TSR_opt/R,GenSpeedRefMin),GenSpeedRefMax)
   end select
   GenSpeedFiltErr = GenSpeedFilt - GenSpeedRef
   ! Set point for the pitch feedback is 10% above the minimum speed
   err_pitch(1) = GenSpeedFilt - GenSpeedRefMax
   err_pitch(2) = 0.0_mk
   if ((GenSpeed + dGenSpeed_dtFilt*CutinVar%delay .lt. GenSpeedRefMin) .and. &
       (.not. generator_cutin)) then
      ! Track AOA with pitch reference
      PitchColRef = min(PitchColRefOld, atan2(WSPfilt, 0.75_mk*R*GenSpeedFilt) - 6.0_mk*degrad)
      ! Remember pitch reference for transition
      PitchColRef0 = PitchColRef
      ! Wind-up integral term of PID2 controller
      PID_pit_var%outmin = PitchColRef
      kgain_pitch = 1.0_mk
      dummy = PID2(stepno, deltat, kgain_pitch, PID_pit_var, err_pitch, AddedPitchRate)
      ! Generator is still cut-out
      GenTorqueRef = 0.0_mk
      ! Timer generator cut-in
      TimerGenCutin = 0.0_mk
      ! Set the initial generator torque to K
      GenTorqueRef0 = min(PitchColRef*raddeg/10.0_mk*Kopt*GenSpeedFilt**2, GenTorqueRated)
      ! Windup the integral term of PID controller
      PID_gen_var%outmin = GenTorqueRef
      kgain_torque = 1.0_mk
      dummy = PID(stepno, deltat, kgain_torque, PID_gen_var, GenSpeedFiltErr)
   elseif (TimerStartup .lt. CutinVar%delay) then
      ! Start increasing the timer for the delay
      TimerStartup = TimerStartup + deltat
      ! Generator is cut-in
      generator_cutin = .true.
      ! Gradually set the minimum pitch angle to optimal pitch
      PID_pit_var%outmin = PitchColRef0 + &
                           (PitchMin - 0.5_mk*PitchColRef0)*TimerStartup/CutinVar%delay
      ! Let pitch PID2 control the speed while the torque is increased
      kgain_pitch = 1.0_mk
      PitchColRef = PID2(stepno, deltat, kgain_pitch, PID_pit_var, err_pitch, AddedPitchRate)
      ! Linearly increase the torque reference
      GenTorqueRef = min(GenTorqueRef0, GenTorqueRef0*TimerStartup/CutinVar%delay)
      ! Start up the switch filter
      dummy = lowpass1orderfilt(deltat, stepno, switchfirstordervar, 0.0_mk)
      ! Windup the integral term of PID controller
      PID_gen_var%outmin = GenTorqueRef
      kgain_torque = 1.0_mk
      dummy = PID(stepno, deltat, kgain_torque, PID_gen_var, GenSpeedFiltErr)
   else
      GenTorqueRef = GenTorqueRef0
      ! Done with start-up
      CtrlStatus = 0
      TimerStartup = 0.0_mk
   endif
   ! Write into dump array
   dump_array(1) = GenTorqueRef*GenSpeed
   dump_array(2) = WSPfilt
   dump_array(3) = GenSpeedFilt
   dump_array(4) = GenSpeedFiltErr
   dump_array(6) = PID_gen_var%outpro
   dump_array(7) = PID_gen_var%outset
   dump_array(8) = PID_gen_var%outmin
   dump_array(9) = PID_gen_var%outmax
   dump_array(12) = err_pitch(2)
   dump_array(13) = PID_pit_var%outpro
   dump_array(14) = PID_pit_var%outset
   dump_array(15) = PID_pit_var%outmin
   dump_array(16) = PID_pit_var%outmax
   dump_array(18) = CtrlStatus
   dump_array(19) = AddedPitchRate
   dump_array(20) = PitchMeanFilt
end subroutine
!**************************************************************************************************
subroutine shut_down(CtrlStatus, GenSpeed, PitchVect, wsp, GenTorqueRef, PitchColRef, dump_array)
   !
   ! Shut-down procedures.
   !
   integer,  intent(in)    :: CtrlStatus ! Integer indicating the status of the controller. &
                                         !  (0: Normal operation, <0: Start-up., >0: Shutdown)
   real(mk), intent(in)    :: GenSpeed   ! Measured generator speed [rad/s].
   real(mk), intent(in)    :: PitchVect(3) ! Measured pitch angles [rad].
   real(mk), intent(in)    :: wsp          ! Measured wind speed [m/s].
   real(mk), intent(out)   :: GenTorqueRef ! Generator torque reference [Nm].
   real(mk), intent(out)   :: PitchColRef  ! Reference collective pitch [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) y(2), WSPfilt, PitchMean, PitchMeanFilt
   ! Filtering
   WSPfilt = lowpass1orderfilt(deltat, stepno, wspfirstordervar, wsp)
   y = lowpass2orderfilt(deltat, stepno, omega2ordervar, GenSpeed)
   PitchMean = (PitchVect(1) + PitchVect(2) + PitchVect(3)) / 3.0_mk
   PitchMeanFilt = lowpass1orderfilt(deltat, stepno, pitchfirstordervar, PitchMean)
   PitchMeanFilt = min(PitchMeanFilt, 30.0_mk*degrad)
   ! Start increasing the timer for the delay
   TimerShutdown2 = TimerShutdown2 + deltat
   ! Generator settings
   select case(CtrlStatus)
      case(1, 3, 4)
         if (GenSpeed .gt. GenSpeed_at_stop*0.8_mk) then
            GenTorqueRef = GenTorque_at_stop
         else
            TimerShutdown = TimerShutdown + deltat
            GenTorqueRef = max(0.0_mk, GenTorque_at_stop*(1.0_mk - TimerShutdown/CutoutVar%torquedelay))
         endif
      case(2, 5, 6)
         GenTorqueRef = 0.0_mk
      case(-2) ! Pitch-out before cut-in
         GenTorqueRef = 0.0_mk
   end select
   ! Pitch seetings
   select case(CtrlStatus)
     case(1, 3, 4) ! Two pitch speed stop
       if (TimerShutdown2 .gt. CutoutVar%pitchdelay + CutoutVar%pitchdelay2) then
         PitchColRef = min(PitchStopAng, PitchColRefOld + deltat*CutoutVar%pitchvelmax2)
       elseif (TimerShutdown2 .gt. CutoutVar%pitchdelay) then
         PitchColRef = min(PitchStopAng, PitchColRefOld + deltat*CutoutVar%pitchvelmax)
       elseif (TimerShutdown2 .gt. 0.0_mk) then
         PitchColRef = PitchColRefOld
       endif
     case(2, 5, 6) ! Pitch out at maximum speed
       PitchColRef = min(PitchStopAng, PitchColRefOld + &
                         deltat*max(CutoutVar%pitchvelmax, CutoutVar%pitchvelmax2))
     case(-2) ! Pitch-out before cut-in
        PitchColRef = min(PitchStopAng, PitchColRefOld + deltat*CutoutVar%pitchvelmax)
   end select
   ! Write into dump array
   dump_array(1) = GenTorqueRef*GenSpeed
   dump_array(3) = y(1)
   dump_array(5) = y(2)
   dump_array(18) = CtrlStatus
   dump_array(20) = PitchMeanFilt
end subroutine
!**************************************************************************************************
subroutine monitoring(CtrlStatus, GridFlag, GenSpeed, TTAcc, dump_array)
   !
   ! Lower level system monitoring. It changes the controller status to:
   ! - (1) if filtered GenSpeed is higher than the overspeed limit.
   ! - (2) if GridFlag is not 0.
   ! - (3) if filtered TTAcc is higher than the safety limit.
   ! - (6) if GenSpeed is negative.
   !
   integer, intent(inout) :: CtrlStatus ! Integer indicating the status of the controller.&
                                        !  (0: Normal operation, <0: Start-up., >0: Shutdown)
   integer, intent(inout)  :: GridFlag  ! Integer indicating the status of the grid.
   real(mk), intent(in)    :: TTAcc     ! Tower top acceleration [m/s**2].
   real(mk), intent(in)    :: GenSpeed  ! Measured generator speed [rad/s].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) GenSpeedFilt, dGenSpeed_dtFilt, TTAccFilt
   real(mk) y(2)
   ! Low-pass filtering of the rotor speed
   y = lowpass2orderfilt(deltat, stepno, MoniVar%omega2ordervar, GenSpeed)
   GenSpeedFilt = y(1)
   dGenSpeed_dtFilt = y(2)
   ! Low-pass filtering of the nacelle wind speed
   TTAccFilt = lowpass1orderfilt(deltat, stepno, MoniVar%rystevagtfirstordervar, TTAcc)
   !***********************************************************************************************
   ! Grid monitoring
   !***********************************************************************************************
   if (GridFlag .gt. 0) then
      CtrlStatus = 2
      stoptype = 1
      GenSpeed_at_stop = GenSpeed
      GenTorque_at_stop = GenTorqueRefOld
   endif
   !***********************************************************************************************
   ! Overspeed monitoring
   !***********************************************************************************************
   if ((GenSpeedFilt .gt. MoniVar%overspeed) .and. (CtrlStatus.eq.0)) then
      CtrlStatus = 1
      stoptype = 1
      GenSpeed_at_stop = GenSpeed
      GenTorque_at_stop = GenTorqueRefOld
   endif
   !***********************************************************************************************
   ! Acceleration monitoring
   !***********************************************************************************************
   if ((TTAccFilt .gt. MoniVar%RysteVagtLevel) .and. (CtrlStatus .eq. 0)) then
      CtrlStatus = 3
      stoptype = 1
      GenSpeed_at_stop = GenSpeed
      GenTorque_at_stop = GenTorqueRefOld
   endif
   !***********************************************************************************************
   ! Reverse speed monitoring
   !***********************************************************************************************
   if ((GenSpeedFilt .lt. 0.0_mk) .and. (CtrlStatus .eq. 0)) then
      CtrlStatus = 6
      stoptype = 1
      GenSpeed_at_stop = GenSpeed
      GenTorque_at_stop = GenTorqueRefOld
   endif
   ! Write into dump array
   dump_array(18) = CtrlStatus
   dump_array(23) = TTAccFilt
end subroutine
!**************************************************************************************************
subroutine torquecontroller(GenSpeed, GenSpeedFilt, dGenSpeed_dtFilt, PitchMean, WSPfilt, &
                            PitchMin, GenSpeedRef_full, Pe, GenTorqueRef, dump_array)
   !
   ! Generator torque controller. Controller that computes the generator torque reference.
   !
   real(mk), intent(in) :: GenSpeed          ! Measured generator speed [rad/s].
   real(mk), intent(in) :: GenSpeedFilt      ! Filtered generator speed [rad/s].
   real(mk), intent(in) :: dGenSpeed_dtFilt  ! Filtered generator acceleration [rad/s**2].
   real(mk), intent(in) :: PitchMean         ! Mean pitch angle [rad].
   real(mk), intent(in) :: PitchMin          ! Minimum pitch angle [rad].
   real(mk), intent(in) :: WSPfilt           ! Filtered wind speed [m/s].
   real(mk), intent(in) :: GenSpeedRef_full  ! Reference generator speed [rad/s].
   real(mk), intent(in) :: Pe                ! Measured electrical power [W].
   real(mk), intent(out) :: GenTorqueRef     ! Generator torque reference [Nm].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) GenTorqueMin_full, GenTorqueMax_full, GenTorqueMin_partial, GenTorqueMax_partial
   real(mk) GenSpeed_min1, GenSpeed_min2, GenSpeed_max1, GenSpeed_max2, GenSpeedRef
   real(mk) x, switch, switch_pitang_lower, switch_pitang_upper
   real(mk) kgain(3), GenSpeedFiltErr, GenSpeedErr, outmin, outmax
   !***********************************************************************************************
   ! Speed ref. changes max. <-> min. for torque contr. and remains at rated for pitch contr.
   !***********************************************************************************************
   select case (PartialLoadControlMode)
   case (1)
      if (GenSpeedFilt .gt. 0.5_mk*(GenSpeedRefMax + GenSpeedRefMin)) then
         GenSpeedRef = GenSpeedRefMax
      else
         GenSpeedRef = GenSpeedRefMin
      endif
   case (2)
      GenSpeedRef = WSPfilt*TSR_opt/R
      GenSpeedRef = min(max(GenSpeedRef, GenSpeedRefMin), GenSpeedRefMax)
   end select
   ! Rotor speed error
   GenSpeedErr = GenSpeed - GenSpeedRef
   GenSpeedFiltErr = GenSpeedFilt - GenSpeedRef
   !-----------------------------------------------------------------------------------------------
   ! Limits for full load
   !-----------------------------------------------------------------------------------------------
   GenTorqueMin_full = GenTorqueRated*(1.0_mk-TorqueCtrlRatio) &
                     + min((GenTorqueRated*GenSpeedRef_full)/max(GenSpeed, GenSpeedRefMin),GenTorqueMax)*TorqueCtrlRatio
   GenTorqueMax_full = GenTorqueMin_full
   !-----------------------------------------------------------------------------------------------
   ! Limits for partial load that opens in both ends
   !-----------------------------------------------------------------------------------------------
   select case (PartialLoadControlMode)
     ! Torque limits for K Omega^2 control of torque
     case (1)
       ! Calculate the constant limits for opening and closing of torque limits
       GenSpeed_min1 = GenSpeedRefMin
       GenSpeed_min2 = GenSpeedRefMin/SwitchVar%rel_sp_open_Qg
       GenSpeed_max1 = (2.0_mk*SwitchVar%rel_sp_open_Qg - 1.0_mk)*GenSpeedRefMax
       GenSpeed_max2 = SwitchVar%rel_sp_open_Qg*GenSpeedRefMax
       ! Compute lower torque limits
       x = switch_spline(GenSpeedFilt, GenSpeed_min1, GenSpeed_min2)
       GenTorqueMin_partial = (Kopt*GenSpeedFilt**2 - Kopt_dot*dGenSpeed_dtFilt)*x
       GenTorqueMin_partial = min(GenTorqueMin_partial, Kopt*GenSpeed_max1**2)
       x = switch_spline(GenSpeedFilt, GenSpeed_max1, GenSpeed_max2)
       ! Compute upper torque limits
       GenTorqueMax_partial = (Kopt*GenSpeedFilt**2 - Kopt_dot*dGenSpeed_dtFilt)*(1.0_mk - x) + &
                               GenTorqueMax_full*x
       GenTorqueMax_partial = max(GenTorqueMax_partial, Kopt*GenSpeed_min2**2)
     ! Torque limits for PID control of torque
     case (2)
       GenTorqueMin_partial = 0.0_mk
       GenTorqueMax_partial = GenTorqueMax_full
   end select
   ! Interpolation between partial and full load torque limits based on pitch
   switch_pitang_lower = 0.01_mk + PitchMin
   switch_pitang_upper = SwitchVar%pitang_upper + PitchMin
   if (PitchMean.le.switch_pitang_lower) then
     fullload=.false.
   endif
   if (PitchMean.ge.switch_pitang_upper) then
     fullload=.true.
   endif
   if (.not.fullload) then
     switch = switch_spline(PitchMean, PitchMin, switch_pitang_upper)
   else
     switch=1.0_mk
   endif
   outmin = (1.0_mk - switch)*GenTorqueMin_partial + switch*GenTorqueMin_full
   outmax = (1.0_mk - switch)*GenTorqueMax_partial + switch*GenTorqueMax_full
   !***********************************************************************************************
   ! Rotor speed exclusion zone
   !***********************************************************************************************
   call rotorspeedexcl(GenSpeedFilt, Pe/GenSpeed, GenTorqueMin_partial, GenTorqueMax_partial, GenSpeedFiltErr, &
                       outmax, outmin, dump_array)
   !-----------------------------------------------------------------------------------------------
   ! Check the generator torque limits
   !-----------------------------------------------------------------------------------------------
   if ((abs(outmax-outmax_old)/deltat) .gt. PID_gen_var%velmax) then
     outmax = outmax_old + dsign(PID_gen_var%velmax*deltat, outmax-outmax_old)
   endif
   if ((abs(outmin-outmin_old)/deltat) .gt. PID_gen_var%velmax) then
     outmin = outmin_old + dsign(PID_gen_var%velmax*deltat, outmin-outmin_old)
   endif
   outmax_old=outmax
   outmin_old=outmin
   PID_gen_var%outmin = outmin
   PID_gen_var%outmax = outmax
   if (PID_gen_var%outmin .gt. PID_gen_var%outmax) PID_gen_var%outmin = PID_gen_var%outmax
   !-----------------------------------------------------------------------------------------------
   ! Compute PID feedback to generator torque demand
   !-----------------------------------------------------------------------------------------------
   kgain = 1.0_mk
   GenTorqueRef = PID(stepno, deltat, kgain, PID_gen_var, GenSpeedErr)
   ! Write into dump array
   dump_array(4) = GenSpeedFiltErr
   dump_array(6) = PID_gen_var%outpro
   dump_array(7) = PID_gen_var%outset
   dump_array(8) = PID_gen_var%outmin
   dump_array(9) = PID_gen_var%outmax
   dump_array(10) = switch
end subroutine
!**************************************************************************************************
subroutine pitchcontroller(GenSpeedFilt, dGenSpeed_dtFilt, PitchMeanFilt, PeFilt, PitchMin, &
                           GenSpeedRef_full, PitchColRef, dump_array)
   !
   ! Pitch controller. Controller that computes the reference collective pitch angle.
   !
   real(mk), intent(in) :: GenSpeedFilt     ! Filtered generator speed [rad/s].
   real(mk), intent(in) :: dGenSpeed_dtFilt ! Filtered generator acceleration [rad/s**2].
   real(mk), intent(in) :: PitchMeanFilt    ! Filtered mean pitch angle [rad].
   real(mk), intent(in) :: PitchMin         ! Minimum pitch angle [rad].
   real(mk), intent(in) :: GenSpeedRef_full ! Reference generator speed [rad/s].
   real(mk), intent(in) :: PeFilt               ! Measured electrical power [W].
   real(mk), intent(out) :: PitchColRef     ! Reference collective pitch angle [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) GenSpeedFiltErr, added_term, aero_gain, aero_damp, kgain(3, 2), err_pitch(2)
   ! Rotor speed error
   GenSpeedFiltErr = GenSpeedFilt - GenSpeedRef_full
   ! Additional nonlinear pitch control term
   if ((PitNonLin1 .gt. 0.0_mk).and.(Err0 .gt. 0.0_mk).and.(ErrDot0.gt.0.0_mk)) then
     added_term = GenSpeedFiltErr/Err0 + dGenSpeed_dtFilt/ErrDot0
     if (added_term .gt. 1.0_mk) then
       AddedPitchRate = PitNonLin1*added_term + AddedPitchRate
     endif
   endif
   ! Limits
   PID_pit_var%outmin = PitchMin
   PID_pit_var%outmax = PitchStopAng
   ! Aerodynamic gain scheduling dQ/dtheta
   aero_gain = 1.0_mk + PitchGSVar%invkk1*PitchMeanFilt + PitchGSVar%invkk2*PitchMeanFilt**2
   kgain = 1.0_mk/aero_gain
   ! Nonlinear gain to avoid large rotor speed excursion
   if ((rel_limit .ne. 0.0_mk).and.(GenSpeedFiltErr.gt.0.0_mk)) then
     kgain = kgain*(GenSpeedFiltErr**2 / (GenSpeedRef_full*(rel_limit - 1.0_mk))**2 + 1.0_mk)
   endif
   ! Gainscheduling according to dQaero/dOmega
   aero_damp = 1.0_mk + PitchGSVar%invkk1_speed*PitchMeanFilt + &
               PitchGSVar%invkk2_speed*PitchMeanFilt**2
   PID_pit_var%kpro(1) = PID_pit_var%kpro(1) + PitchGSVar%kp_speed*aero_damp
   !-----------------------------------------------------------------------------------------------
   ! Compute PID feedback to pitch demand
   !-----------------------------------------------------------------------------------------------
   if (DT_mode_filt%f0 .gt. 0.0_mk) then
     err_pitch(1) = notch2orderfilt(deltat, stepno, DT_mode_filt, GenSpeedFiltErr)
     err_pitch(2) = notch2orderfilt(deltat, stepno, pwr_DT_mode_filt, PeFilt - PeRated)
   else
     err_pitch(1) = GenSpeedFiltErr
     err_pitch(2) = PeFilt - PeRated
   endif
   PitchColRef = PID2(stepno, deltat, kgain, PID_pit_var, err_pitch, AddedPitchRate)
   ! Write into dump array
   dump_array(11) = GenSpeedFiltErr
   dump_array(12) = err_pitch(2)
   dump_array(13) = PID_pit_var%outpro
   dump_array(14) = PID_pit_var%outset
   dump_array(15) = PID_pit_var%outmin
   dump_array(16) = PID_pit_var%outmax
   dump_array(19) = AddedPitchRate
end subroutine
!**************************************************************************************************
subroutine rotorspeedexcl(GenSpeedFilt, GenTorque, Qg_min_partial, GenTorqueMax_partial, GenSpeedFiltErr, &
                          outmax, outmin, dump_array)
   !
   ! Rotor speed exclusion zone. Subroutine that changes the generator torque limits and the
   ! generator speed error for the generator PID controller to avoid a rotor speed band.
   !
   real(mk), intent(in) :: GenSpeedFilt         ! Filtered measured generator speed [rad/s].
   real(mk), intent(in) :: Qg_min_partial       ! Generator torque lower limit [Nm].
   real(mk), intent(in) :: GenTorqueMax_partial ! Generator torque upper limit [Nm].
   real(mk), intent(in) :: GenTorque          ! Measured generator torque [Nm].
   real(mk), intent(inout) :: outmax          ! Generator torque maximum value [Nm].
   real(mk), intent(inout) :: outmin          ! Generator torque minimum value [Nm].
   real(mk), intent(inout) :: GenSpeedFiltErr ! Filtered generator speed error [rad/s].
   real(mk), intent(inout) :: dump_array(50)  ! Array for output.
   real(mk) y(2), GenSpeedFiltNotch, x1, x2
   real(mk) :: x
   real(mk) Lwr, Lwr_Tg, Hwr, Hwr_Tg, time_excl_delay
   x=0.0_mk
   Lwr = ExcluZone%Lwr
   Lwr_Tg = ExcluZone%Lwr_Tg
   Hwr = ExcluZone%Hwr
   Hwr_Tg = ExcluZone%Hwr_Tg
   time_excl_delay = ExcluZone%time_excl_delay
   ! Band stop filtering of the rotor speed
   y = notch2orderfilt(deltat, stepno, ExcluZone%notch, GenSpeedFilt)
   GenSpeedFiltNotch = y(1)
   TimerExcl = TimerExcl + deltat
   if ((Lwr .gt. 0.0_mk) .and. (Hwr .gt. 0.0_mk)) then
      select case (w_region)
         case (0)
            if ((GenSpeedFilt .gt. Lwr*0.99_mk) .and. (TimerGenCutin .gt. CutinVar%delay))then
               ! rotor reference angular speed
               GenSpeedFiltErr = GenSpeedFiltNotch - Lwr
               excl_flag = 0.0_mk
               w_region = 1
            elseif (GenSpeedFilt .gt. Hwr*(2.0_mk-0.99_mk)) then
               w_region = 3
            else
               w_region = 0
            endif
         case (1)
            if (GenTorque .gt. Lwr_Tg) then
               ! rotor reference angular speed
               TimerExcl = 0.0_mk
               excl_flag = 1.0_mk
               GenSpeedFiltErr = GenSpeedFiltNotch - Lwr
               w_region = 2
            elseif (GenSpeedFilt .lt. Lwr*0.99_mk) then
               w_region = 0
            else
               ! rotor reference angular speed
               if (excl_flag .eq. 0.0_mk) then
                  x = 1.0_mk
               else
                  x = switch_spline(TimerExcl, 0.0_mk, time_excl_delay)
               endif
               GenSpeedFiltErr = GenSpeedFiltNotch - (Lwr*x + Hwr*(1.0_mk - x))
               w_region = 1
            endif
         case (2)
            if (GenTorque .lt. Hwr_Tg) then
               TimerExcl = 0.0_mk
               excl_flag = 1.0_mk
               ! rotor reference angular speed
               GenSpeedFiltErr = GenSpeedFiltNotch - Hwr
               w_region = 1
            elseif (GenSpeedFilt .gt. Hwr*(2.0_mk-0.95_mk)) then
               w_region = 3
            else
               ! rotor reference angular speed
               if (excl_flag .eq. 0.0_mk) then
                  x = 1.0_mk
               else
                  x = switch_spline(TimerExcl, 0.0_mk, time_excl_delay)
               endif
               GenSpeedFiltErr = GenSpeedFiltNotch - (Hwr*x + Lwr*(1.0_mk - x))
               w_region = 2
            endif
         case default
            if (GenSpeedFilt .gt. Hwr*(2.0_mk - 0.99_mk)) then
               w_region = 3
            elseif (GenSpeedFilt .lt. Lwr*0.99_mk) then
               w_region = 0
            else
               ! rotor reference angular speed
               excl_flag = 0.0_mk
               GenSpeedFiltErr = GenSpeedFiltNotch - Hwr
               w_region = 2
            endif
      end select
      if ((w_region .eq. 1) .or. (w_region .eq. 2)) then
         x1 = switch_spline(GenSpeedFilt, Lwr*0.99_mk, Lwr)
         x2 = switch_spline(GenSpeedFilt, Hwr, Hwr*(2.0_mk-0.99_mk))
         !min/max generator torque @ exclution zone
         outmax  = GenTorqueMax_partial*(1.0_mk-x1 +x2) + Lwr_Tg * 1.05_mk*(x1-x1*x2)
         outmin  = Qg_min_partial*(1.0_mk-x1 +x2) + Hwr_Tg * 0.95_mk*(x1-x1*x2)
      endif
   dump_array(24) = w_region
   endif
end subroutine
!**************************************************************************************************
subroutine drivetraindamper(GenSpeed, Qdamp_ref, dump_array)
   !
   ! Drivetrain damper.
   !
   real(mk), intent(in)  :: GenSpeed  ! Measured generator speed [rad/s].
   real(mk), intent(out) :: Qdamp_ref ! Generator torque reference component from the drivetrain &
                                      ! damper [Nm].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) omega_dtfilt
   if ((DT_damper%gain .ne. 0.0_mk) .and. (DT_damper%bandpass%f0 .gt. 0.0_mk)) then
      call damper(stepno, deltat, GenSpeed, DT_damper, Qdamp_ref, omega_dtfilt)
   else
      omega_dtfilt = 0.0_mk
      Qdamp_ref = 0.0_mk
   endif
   dump_array(5) = omega_dtfilt
   dump_array(17) = Qdamp_ref
end subroutine
!**************************************************************************************************
subroutine towerdamper(TTfa_acc, theta_dam_ref, dump_array)
   !
   ! Longitudinal tower damper.
   !
   real(mk), intent(in) :: TTfa_acc ! Measured tower top longitudinal acceleration [m/s**2].
   real(mk), intent(out) :: theta_dam_ref ! Reference pitch angle component from longitudinal tower&
                                          ! damper [rad].
   real(mk), intent(inout) :: dump_array(50) ! Array for output.
   real(mk) TTfa_acc_filt
   if ((TTfa_damper%gain .ne. 0.0_mk) .and. (TTfa_damper%bandpass%f0 .gt. 0.0_mk)) then
      call damper(stepno, deltat, TTfa_acc, TTfa_damper, theta_dam_ref, TTfa_acc_filt)
   else
      TTfa_acc_filt = 0.0_mk
      theta_dam_ref = 0.0_mk
   endif
   dump_array(25) = TTfa_acc_filt
   dump_array(26) = theta_dam_ref
end subroutine
!**************************************************************************************************
end module turbine_controller_mod
