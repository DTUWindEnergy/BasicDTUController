module dtu_we_controller_mod
   use dtu_we_controller_fcns
   use turbine_controller_mod
   use safety_system_mod
   implicit none
   integer  CtrlStatus
   real(mk) dump_array(50)
   real(mk) time_old
contains
!**************************************************************************************************
subroutine init_regulation(array1, array2)
   !
   ! Controller parameters initialization.
   !
   !DEC$ IF .NOT. DEFINED(__LINUX__)
   !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_regulation'::init_regulation
   !DEC$ END IF
   real(mk) array1(100), array2(1)
   ! Local vars
   integer i, ifejl
   character(len=32) text32
   real(mk) minimum_pitch_angle
   logical findes
   write(6, *) 'Basic DTU Wind Energy Controller ' //trim(adjustl(vertext32))// ' loaded...'
   ! Input array1 must contain
   ! Overall parameters
   !  constant   1 ; Rated power [kW]
   !  constant   2 ; Minimum rotor speed [rad/s]
   !  constant   3 ; Rated rotor speed [rad/s]
   !  constant   4 ; Maximum allowable generator torque [Nm]
   !  constant   5 ; Minimum pitch angle, PitchMin [deg], 
   !               ; if |PitchMin|>90, then a table of <wsp,PitchMin> is read ;
   !               ; from a file named 'wptable.n', where n=int(PitchMin)
   !  constant   6 ; Maximum pitch angle [deg]
   !  constant   7 ; Maximum pitch velocity operation [deg/s]
   !  constant   8 ; Frequency of generator speed filter [Hz]
   !  constant   9 ; Damping ratio of speed filter [-]
   !  constant  10 ; Frequency of free-free DT torsion mode [Hz], if zero no notch filter used
   ! Partial load control parameters
   !  constant  11 ; Optimal Cp tracking K factor [Nm/(rad/s)^2], ;
   !               ; Qg=K*Omega^2, K=eta*0.5*rho*A*Cp_opt*R^3/lambda_opt^3
   !  constant  12 ; Proportional gain of torque controller [Nm/(rad/s)]
   !  constant  13 ; Integral gain of torque controller [Nm/rad]
   !  constant  14 ; Differential gain of torque controller [Nm/(rad/s^2)]
   ! Full load control parameters
   !  constant  15 ; Generator control switch [1=constant power, 2=constant torque]
   !  constant  16 ; Proportional gain of pitch controller [rad/(rad/s)]
   !  constant  17 ; Integral gain of pitch controller [rad/rad]
   !  constant  18 ; Differential gain of pitch controller [rad/(rad/s^2)]
   !  constant  19 ; Proportional power error gain [rad/W]
   !  constant  20 ; Integral power error gain [rad/(Ws)]
   !  constant  21 ; Coefficient of linear term in aerodynamic gain scheduling, KK1 [deg]
   !  constant  22 ; Coefficient of quadratic term in aerodynamic gain scheduling, KK2 [deg^2] &
   !               ; (if zero, KK1 = pitch angle at double gain)
   !  constant  23 ; Relative speed for double nonlinear gain [-]
   ! Cut-in simulation parameters
   !  constant  24 ; Cut-in time [s]
   !  constant  25 ; Time delay for soft start of torque [1/1P]
   ! Cut-out simulation parameters
   !  constant  26 ; Cut-out time [s]
   !  constant  27 ; Time constant for linear torque cut-out [s]
   !  constant  28 ; Stop type [1=normal, 2=emergency]
   !  constant  29 ; Time delay for pitch stop after shut-down signal [s]
   !  constant  30 ; Maximum pitch velocity during initial period of stop [deg/s]
   !  constant  31 ; Time period of initial pitch stop phase [s] (maintains pitch speed specified in constant 30)
   !  constant  32 ; Maximum pitch velocity during final phase of stop [deg/s]
   ! Expert parameters (keep default values unless otherwise given)
   !  constant  33 ; Lower angle above lowest minimum pitch angle for switch [deg]
   !  constant  34 ; Upper angle above lowest minimum pitch angle for switch [deg], if equal then hard switch
   !  constant  35 ; Ratio between filtered speed and reference speed for fully open torque limits [%]
   !  constant  36 ; Time constant of 1st order filter on wind speed used for minimum pitch [1/1P]
   !  constant  37 ; Time constant of 1st order filter on pitch angle used for gain scheduling [1/1P]
   ! Drivetrain damper
   !  constant  38 ; Proportional gain of active DT damper [Nm/(rad/s)], requires frequency in input 10
   ! Overspeed
   !  constant  39 ; Overspeed percentage before initiating turbine controller alarm (shut-down) [%]
   ! Additional non-linear pitch control term (not used when all zero)
   !  constant  40 ; Err0 [rad/s] 
   !  constant  41 ; ErrDot0 [rad/s^2]
   !  constant  42 ; PitNonLin1 [rad/s]
   ! Storm control command
   !  constant  43 ; Wind speed 'Vstorm' above which derating of rotor speed is used [m/s]
   !  constant  44 ; Cut-out wind speed (only used for derating of rotor speed in storm) [m/s]	  
   ! Safety system parameters
   !  constant  45 ; Overspeed percentage before initiating safety system alarm (shut-down) [%]
   !  constant  46 ; Max low-pass filtered tower top acceleration level before initiating turbine controller alarm (shut-down) [m/s^2]
   ! Turbine parameter
   !  constant  47 ; Nominal rotor diameter [m]
   ! Parameters for rotor inertia reduction in variable speed region
   !  constant  48 ; Proportional gain on rotor acceleration in variable speed region [Nm/(rad/s^2)] (not used when zero)
   ! Parameters for alternative partial load controller with PI regulated TSR tracking
   !  constant  49 ; Optimal tip speed ratio [-] (only used when K=constant 11 = 0 otherwise  Qg=K*Omega^2 is used)
   ! Parameters for adding aerodynamic drivetrain damping on gain scheduling
   !  constant  50 ; Proportional gain of aerodynamic DT damping [Nm/(rad/s)]
   !  constant  51 ; Coefficient of linear term in aerodynamic DT damping scheduling, KK1 [deg]
   !  constant  52 ; Coefficient of quadratic term in aerodynamic DT damping scheduling, KK2 [deg^2]
   !
   ! Output array2 contains nothing for init
   !
   ! Overall parameters
   PeRated             = array1( 1)*1.d3
   GenSpeedRefMin      = array1( 2)
   GenSpeedRefMax      = array1( 3)
   GenTorqueMax        = array1( 4)
   minimum_pitch_angle = array1( 5)*degrad
   PitchStopAng        = array1( 6)*degrad
   PID_pit_var%velmax  = array1( 7)*degrad
   ! Generator speed second order low pass filter
   omega2ordervar%f0   = array1( 8)
   omega2ordervar%zeta = array1( 9)
   MoniVar%omega2ordervar%f0           = omega2ordervar%f0
   MoniVar%omega2ordervar%zeta         = omega2ordervar%zeta
   SafetySystemVar%omega2ordervar%f0   = omega2ordervar%f0
   SafetySystemVar%omega2ordervar%zeta = omega2ordervar%zeta
   ! Drivetrain mode notch filters for pitch controller
   DT_mode_filt%f0     = array1(10)
   pwr_DT_mode_filt%f0 = DT_mode_filt%f0
   ! Partial load control parameters
   Kopt             = array1(11)
   PID_gen_var%Kpro = array1(12)
   PID_gen_var%Kint = array1(13)
   PID_gen_var%Kdif = array1(14)
   ! Full load control parameters
   const_power         = (int(array1(15)).eq.1)
   ! - Gains
   PID_pit_var%kpro(1) = array1(16)
   PID_pit_var%kint(1) = array1(17)
   PID_pit_var%kdif(1) = array1(18)
   PID_pit_var%kpro(2) = array1(19)
   PID_pit_var%kint(2) = array1(20)
   PID_pit_var%kdif(2) = 0.0_mk
   ! - Gain-scheduling
   PitchGSVar%invkk1 = 1.0_mk/(array1(21)*degrad)
   if (array1(22).eq.0.0_mk) then
     PitchGSVar%invkk2 = 0.0_mk
   else
     PitchGSVar%invkk2 = 1.0_mk/(array1(22)*degrad*degrad)
   endif
   rel_limit = array1(23)
   ! Cut-in simulation parameters
   CutinVar%time  = array1(24)
   CutinVar%delay = array1(25)*2.0_mk*pi/GenSpeedRefMax
   ! Cut-out simulation parameters
   CutoutVar%time         = array1(26)
   CutoutVar%torquedelay  = array1(27)
   CutoutVar%stoptype = int(array1(28))
   CutoutVar%pitchdelay   = array1(29)
   CutoutVar%pitchvelmax  = array1(30)*degrad
   CutoutVar%pitchdelay2  = array1(31)
   CutoutVar%pitchvelmax2 = array1(32)*degrad
   if (CutinVar%time .gt. 0.0_mk)then
     CtrlStatus = -2
     generator_cutin=.false.
   endif
   ! Expert parameters (keep default values unless otherwise given)
   SwitchVar%pitang_lower   = array1(33)*degrad
   SwitchVar%pitang_upper   = array1(34)*degrad
   SwitchVar%rel_sp_open_Qg = array1(35)*1.d-2
   wspfirstordervar%tau     = array1(36)*2.0_mk*pi/GenSpeedRefMax
   pitchfirstordervar%tau   = array1(37)*2.0_mk*pi/GenSpeedRefMax
   ! Drivetrain damper
   DT_damper%gain      = array1(38)
   DT_damper%bandpass%f0  = DT_mode_filt%f0
   ! Overspeed
   MoniVar%overspeed = (1.0_mk + array1(39)*0.01_mk)*GenSpeedRefMax
   ! Additional non-linear pitch control term
   Err0       = array1(40)
   ErrDot0    = array1(41)
   PitNonLin1 = array1(42)
   ! Default and derived parameters
   PID_gen_var%velmax = 0.0_mk !No limit to generator torque change rate
   GenTorqueRated = PeRated/GenSpeedRefMax
   switchfirstordervar%tau = 2.0_mk*pi/GenSpeedRefMax
   MoniVar%rystevagtfirstordervar%tau = 2.0_mk*pi/GenSpeedRefMax
   SafetySystemVar%rystevagtfirstordervar%tau = 2.0_mk*pi/GenSpeedRefMax
   ! Wind speed table
   if (dabs(minimum_pitch_angle).lt.90.0_mk*degrad) then 
     OPdatavar%lines=2
     OPdatavar%wpdata(1,1) = 0.0_mk
     OPdatavar%wpdata(2,1) = 99.0_mk
     OPdatavar%wpdata(1,2) = minimum_pitch_angle
     OPdatavar%wpdata(2,2) = minimum_pitch_angle
   else
     write(text32,'(i3)') int(minimum_pitch_angle*raddeg)
     inquire(file='.\control\wpdata.'//trim(adjustl(text32)),exist=findes)
     if (findes) then
       open(88,file='.\control\wpdata.'//trim(adjustl(text32)))
       read(88,*,iostat=ifejl) OPdatavar%lines
       if (ifejl.eq.0) then
         do i=1,OPdatavar%lines
           read(88,*,iostat=ifejl) OPdatavar%wpdata(i,1),OPdatavar%wpdata(i,2) 
           if (ifejl.ne.0) then
             write(6,*) ' *** ERROR *** Could not read lines in minimum '&
                      //'pitch table in file wpdata.'//trim(adjustl(text32))
             stop
           endif
           OPdatavar%wpdata(i,2)=OPdatavar%wpdata(i,2)*degrad
         enddo
       else
         write(6,*) ' *** ERROR *** Could not read number of lines '&
                  //'in minimum pitch table in file wpdata.'//trim(adjustl(text32))
         stop
       endif
       close(88)
     else
       write(6,*) ' *** ERROR *** File ''wpdata.'//trim(adjustl(text32))&
                //''' does not exist in the .\control\ folder'
       stop
     endif
   endif
   ! Storm controller input
   Vstorm  = array1(43) ! [m/s] Vstorm (e.g. 25)
   Vcutout = array1(44) ! [m/s] Vcut-out (e.g. 45)
   if (Vcutout.gt.Vstorm) then
     write (6,'(a,f4.1,a,f4.1,a)') ' Storm control is active above ', Vstorm, &
                                   'm/s until cut-out at ', Vcutout, 'm/s'
   endif
   ! Overspeed monitor
   SafetySystemVar%overspeed = (1.0_mk + array1(45)*0.01_mk)*GenSpeedRefMax
   ! "Rystevagt" monitor
   MoniVar%RysteVagtLevel = array1(46)
   R = 0.5_mk*array1(47)
   ! Alternative partial load controller
   Kopt_dot= array1(48)
   TSR_opt = array1(49)
   if (array1(11) .le. 0.0_mk) then
     PartialLoadControlMode = 2
   else
     PartialLoadControlMode = 1
   endif
   ! Gain scheduling dQdomega
   PitchGSVar%kp_speed = array1(50)
   if (array1(51) .gt. 0.0_mk) then
     PitchGSVar%invkk1_speed = 1.0_mk/(array1(51)*degrad)
   else
     PitchGSVar%invkk1_speed = 0.0_mk
   endif
   if (array1(52) .gt. 0.0_mk) then
     PitchGSVar%invkk2_speed = 1.0_mk/(array1(52)*degrad*degrad)
   else
     PitchGSVar%invkk2_speed = 0.0_mk
   endif
   ! Set parameters that can be modified with advanced options
   ! -Generator torque exclusion zone
   ExcluZone%Lwr             = 0.0_mk
   ExcluZone%Lwr_Tg          = 0.0_mk
   ExcluZone%Hwr             = 0.0_mk
   ExcluZone%Hwr_Tg          = 0.0_mk
   ExcluZone%time_excl_delay = 0.0_mk
   ! -Drive train mode damper
   DT_damper%notch%f0   = 10.0_mk*DT_damper%notch%f0
   DT_damper%bandpass%zeta = 0.7_mk
   DT_damper%notch%zeta2   = 0.01
   DT_damper%Td            = 0.0_mk
   ! -Tower top fore-aft mode damper
   TTfa_damper%bandpass%f0   = 1.0_mk
   TTfa_damper%notch%f0      = 1.0_mk
   TTfa_damper%bandpass%zeta = 0.7_mk
   TTfa_damper%notch%zeta2   = 0.01
   TTfa_damper%gain          = 0.0_mk
   TTfa_damper%Td            = 1.0_mk
   TTfa_PWRfirstordervar%tau = 1.0_mk
   TTfa_PWR_lower = array1(69)
   TTfa_PWR_upper = array1(70)
   ! -Tower top side-to- mode filter
   ExcluZone%notch%f0    = 100.0_mk
   ExcluZone%notch%zeta2 = 0.01_mk
   ! -"Rystevagt" monitor for Safety System
   SafetySystemVar%RysteVagtLevel = MoniVar%RysteVagtLevel*1.1_mk
   ! Gear Ratio
   GearRatio = 1
   ! Initiate the dynamic variables
   stepno = 0
   time_old = 0.0_mk
   AddedPitchRate = 0.0_mk
   ! No output
   array2 = 0.0_mk
   return
end subroutine init_regulation
!**************************************************************************************************
subroutine init_regulation_advanced(array1, array2)
   !
   ! Controller parameters initialization with additional inputs.
   !
   !DEC$ IF .NOT. DEFINED(__LINUX__)
   !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_regulation_advanced'::init_regulation_advanced
   !DEC$ END IF
   real(mk) array1(100),array2(1)
   ! Torque exclusion zone
   !  constant  53 ; Torque exclusion zone: Low speed [rad/s]
   !  constant  54 ; Torque exclusion zone: Low speed generator toque [Nm]
   !  constant  55 ; Torque exclusion zone: High speed [rad/s]
   !  constant  56 ; Torque exclusion zone: High speed generator toque [Nm]
   !  constant  57 ; Time constant of reference switching at exclusion zone [s]
   ! DT torsion mode damper
   !  constant  58 ; Frequency of notch filter [Hz]
   !  constant  59 ; Damping of BP filter [-]
   !  constant  60 ; Damping of notch filter [-]
   !  constant  61 ; Phase lag of damper [s] =>  max 40*dt 
   ! Fore-aft Tower mode damper
   !  constant  62 ; Frequency of BP filter [Hz]
   !  constant  63 ; Frequency of notch fiter [Hz]
   !  constant  64 ; Damping of BP filter [-]
   !  constant  65 ; Damping of notch filter [-]
   !  constant  66 ; Gain of damper [-]
   !  constant  67 ; Phase lag of damper [s] =>  max 40*dt
   !  constant  68 ; Time constant of 1st order filter on PWR used for fore-aft Tower mode damper GS [Hz]
   !  constant  69 ; Lower PWR limit used for fore-aft Tower mode damper GS [-]
   !  constant  70 ; Upper PWR limit used for fore-aft Tower mode damper GS [-]
   ! Side-to-side Tower mode filter
   !  constant  71 ; Frequency of Tower side-to-sede notch filter [Hz]
   !  constant  72 ; Damping of notch filter [-]
   !  constant  73 ; Max low-pass filtered tower top acceleration level before initiating safety system alarm (shut-down) [m/s^2]
   ! Additional filters
   !  constant  74 ; Time constant of 1st order filter on pitch angle used for switch [1/1P]
   !  constant  75 ; Time constant of 1st order filter on tower top acceleration used for ShakeGuard [1/1P]
   ! Gear ratio
   !  constant  76 ; Gear ratio (To be used only if input #2 refers to HSS)
   !
   call init_regulation(array1, array2)
   ! Generator torque exclusion zone
   ExcluZone%Lwr             = array1(53)
   ExcluZone%Lwr_Tg          = array1(54)
   ExcluZone%Hwr             = array1(55)
   ExcluZone%Hwr_Tg          = array1(56)
   ExcluZone%time_excl_delay = array1(57)
   ! Drive train mode damper
   DT_damper%notch%f0      = array1(58)
   DT_damper%bandpass%zeta = array1(59)
   DT_damper%notch%zeta2   = array1(60)
   DT_damper%Td            = array1(61)
   ! Tower top fore-aft mode damper
   TTfa_damper%bandpass%f0   = array1(62)
   TTfa_damper%notch%f0      = array1(63)
   TTfa_damper%bandpass%zeta = array1(64)
   TTfa_damper%notch%zeta2   = array1(65)
   TTfa_damper%gain          = array1(66)
   TTfa_damper%Td            = array1(67)
   TTfa_PWRfirstordervar%tau = 1.0_mk/(2.0_mk*pi*array1(68)) 
   TTfa_PWR_lower = array1(69)
   TTfa_PWR_upper = array1(70)
   !Tower top side-to- mode filter
   ExcluZone%notch%f0    = array1(71)
   ExcluZone%notch%zeta2 = array1(72)
   ! "Rystevagt" monitor for Safety System
   SafetySystemVar%RysteVagtLevel = array1(73)
   ! Additional filters
   switchfirstordervar%tau                    = array1(74)*2.0_mk*pi/GenSpeedRefMax
   MoniVar%rystevagtfirstordervar%tau         = array1(75)*2.0_mk*pi/GenSpeedRefMax
   SafetySystemVar%rystevagtfirstordervar%tau = MoniVar%rystevagtfirstordervar%tau
   ! Gear ratio
   GearRatio = array1(76)
   ! Initialization
   TimerExcl = -0.02_mk
end subroutine init_regulation_advanced
!**************************************************************************************************
subroutine update_regulation(array1, array2)
   !
   ! Controller interface. 
   !  - sets DLL inputs/outputs.
   !  - sets controller timers.
   !  - calls the safety system monitor (higher level).
   !
   !DEC$ IF .NOT. DEFINED(__LINUX__)
   !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_regulation'::update_regulation
   !DEC$ END IF
   real(mk) array1(100),array2(100)
   ! Input array1 must contain
   !
   !    1: general time                            [s]
   !    2: constraint bearing1 shaft_rot 1 only 2  [rad/s] Generator speed (Default LSS, if HSS insert gear ratio in input #76)
   !    3: constraint bearing2 pitch1 1 only 1     [rad]
   !    4: constraint bearing2 pitch2 1 only 1     [rad]
   !    5: constraint bearing2 pitch3 1 only 1     [rad]
   !  6-8: wind free_wind 1 0.0 0.0 hub height     [m/s] global coords at hub height
   !    9: elec. power  ; [W]
   !   10: grid flag  ; [1=no grid,0=grid]
   !   11: Tower top x-acceleration  ; [m/s^2]
   !   12: Tower top y-acceleration  ; [m/s^2]
   !
   ! Output array2 contains
   !
   !    1: Generator torque reference               [Nm]
   !    2: Pitch angle reference of blade 1         [rad]
   !    3: Pitch angle reference of blade 2         [rad]
   !    4: Pitch angle reference of blade 3         [rad]
   !    5: Power reference                          [W]
   !    6: Filtered wind speed                      [m/s]
   !    7: Filtered rotor speed                     [rad/s]
   !    8: Filtered rotor speed error for torque    [rad/s]
   !    9: Bandpass filtered rotor speed            [rad/s]
   !   10: Proportional term of torque contr.       [Nm]
   !   11: Integral term of torque controller       [Nm]
   !   12: Minimum limit of torque                  [Nm]
   !   13: Maximum limit of torque                  [Nm]
   !   14: Torque limit switch based on pitch       [-]
   !   15: Filtered rotor speed error for pitch     [rad/s]
   !   16: Power error for pitch                    [W]
   !   17: Proportional term of pitch controller    [rad]
   !   18: Integral term of pitch controller        [rad]
   !   19: Minimum limit of pitch                   [rad]
   !   20: Maximum limit of pitch                   [rad]
   !   21: Torque reference from DT damper          [Nm]
   !   22: Status signal                            [-]
   !   23: Total added pitch rate                   [rad/s]
   !   24: Filtered pitch angle                     [rad]
   !   25: Flag for mechnical brake                 [0=off/1=on]
   !   26: Flag for emergency pitch stop            [0=off/1=on]
   !   27: LP filtered acceleration level           [m/s^2]
   !   28: Rotor speed exlusion zone region         [-]
   !   29: Filtered tower top acc. for tower damper [m/s^2]
   !   30: Reference pitch from tower damper        [rad]
   !
   ! Local variables
   integer GridFlag, EmergPitchStop, ActiveMechBrake
   real(mk) GenSpeed, wsp, PitchVect(3), Pe, TT_acc(2), time
   real(mk) GenTorqueRef, PitchColRef
   EmergPitchStop = 0
   ActiveMechBrake = 0
   ! Time
   time = array1(1)
   !***********************************************************************************************
   ! Increment time step (may actually not be necessary in type2 DLLs)
   !***********************************************************************************************
   if (time .gt. time_old) then
     deltat = time - time_old
     if (deltat .eq. 0.0_mk) deltat = 0.01_mk
     time_old = time
     stepno = stepno + 1
   endif
   ! Rotor speed
   GenSpeed = array1(2)/GearRatio
   ! Pitch angle
   PitchVect(1) = array1(3)
   PitchVect(2) = array1(4)
   PitchVect(3) = array1(5)
   ! Wind speed as horizontal vector sum
   wsp = dsqrt(array1(6)**2 + array1(7)**2)
   if (stepno.eq.1) then 
      Pe = 0.0_mk ! Elec. power
      GridFlag = 0 ! Grid flag
   else
      Pe=array1(9) ! Elec. power
      GridFlag = int(array1(10)) ! Grid flag
   endif
   ! Tower top acceleration
   TT_acc(1) = array1(11)
   TT_acc(2) = array1(12)
   !***********************************************************************************************
   ! Safety system
   !***********************************************************************************************
   if (time .gt. 5.0_mk) then
      call safety_system(stepno, deltat, GenSpeed, TT_acc, EmergPitchStop, ActiveMechBrake, &
                         dump_array)
   endif
   !***********************************************************************************************
   ! Start-up timer timer monitoring
   !***********************************************************************************************
   if ((CutinVar%time .gt. 0.0_mk).and.(time .gt. CutinVar%time) .and. (CtrlStatus .eq. -2)) then
     CtrlStatus = -1
     TimerStartup = deltat
   endif
   !***********************************************************************************************
   ! Shut-down timer monitoring
   !***********************************************************************************************
   if ((CutoutVar%time .gt. 0.0_mk) .and. (time .gt. CutoutVar%time) .and. (CtrlStatus .eq. 0)) then
     if (stoptype.eq.1) CtrlStatus = 4
     if (stoptype.eq.2) CtrlStatus = 5
     GenSpeed_at_stop = GenSpeed
     GenTorque_at_stop = GenTorqueRefOld
     stoptype = CutoutVar%stoptype
     TimerShutdown = 0.0_mk
     TimerShutdown2 = 0.0_mk
   endif
   !***********************************************************************************************
   ! Wind turbine controller
   !***********************************************************************************************
   call turbine_controller(CtrlStatus, GridFlag, GenSpeed, PitchVect, wsp, Pe, TT_acc, &
                           GenTorqueRef, PitchColRef, dump_array)
   !***********************************************************************************************
   ! Output
   !***********************************************************************************************
   array2( 1) = GenTorqueRef*GearRatio !    1: Generator torque reference               [Nm]
   array2( 2) = PitchColRef            !    2: Pitch angle reference of blade 1         [rad]
   array2( 3) = PitchColRef            !    3: Pitch angle reference of blade 2         [rad]
   array2( 4) = PitchColRef            !    4: Pitch angle reference of blade 3         [rad]
   array2( 5) = dump_array(1)          !    5: Power reference                          [W]
   array2( 6) = dump_array(2)          !    6: Filtered wind speed                      [m/s]
   array2( 7) = dump_array(3)          !    7: Filtered rotor speed                     [rad/s]
   array2( 8) = dump_array(4)          !    8: Filtered rotor speed error for torque    [rad/s]
   array2( 9) = dump_array(5)          !    9: Bandpass filtered rotor speed            [rad/s]
   array2(10) = dump_array(6)          !   10: Proportional term of torque contr.       [Nm]
   array2(11) = dump_array(7)          !   11: Integral term of torque controller       [Nm]
   array2(12) = dump_array(8)          !   12: Minimum limit of torque                  [Nm]
   array2(13) = dump_array(9)          !   13: Maximum limit of torque                  [Nm]
   array2(14) = dump_array(10)         !   14: Torque limit switch based on pitch       [-]
   array2(15) = dump_array(11)         !   15: Filtered rotor speed error for pitch     [rad/s]
   array2(16) = dump_array(12)         !   16: Power error for pitch                    [W]
   array2(17) = dump_array(13)         !   17: Proportional term of pitch controller    [rad]
   array2(18) = dump_array(14)         !   18: Integral term of pitch controller        [rad]
   array2(19) = dump_array(15)         !   19: Minimum limit of pitch                   [rad]
   array2(20) = dump_array(16)         !   20: Maximum limit of pitch                   [rad]
   array2(21) = dump_array(17)         !   21: Torque reference from DT damper          [Nm]
   array2(22) = int(dump_array(18))    !   22: Status signal                            [-]
   array2(23) = dump_array(19)         !   23: Total added pitch rate                   [rad/s]
   array2(24) = dump_array(20)         !   24: Filtered pitch angle                     [rad]
   array2(25) = ActiveMechBrake      !   25: Flag for mechnical brake                 [0=off/1=on]
   array2(26) = EmergPitchStop         !   26: Flag for emergency pitch stop            [0=off/1=on]
   array2(27) = dump_array(23)         !   27: LP filtered acceleration level           [m/s^2]
   array2(28) = int(dump_array(24))    !   28: Rotor speed exlusion zone region         [-]
   array2(29) = dump_array(25)         !   29: Filtered tower top acc. for tower damper [m/s^2]
   array2(30) = dump_array(26)         !   30: Reference pitch from tower damper        [rad]
   return
end subroutine update_regulation
!**************************************************************************************************
end module dtu_we_controller_mod
