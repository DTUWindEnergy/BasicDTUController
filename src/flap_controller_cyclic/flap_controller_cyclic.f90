module cyclic_flap_controller_mod
!
! Control Dll of type 2:
! Dll for applying cyclic flap control actions, based on cyclic pitch formulation by mhha in cyclic_pitch_controller
! Bossanyi cyclic pitch with Colemann transformed blade root bending moments.
!
! v.1.0, 26/04/2018, TKBA
!
! ******************************* example htc input *******************************
!begin type2_dll; 
!name cyclic_flap_controller ; 
!filename  ./control/cyclic_flap_controller.dll ; 
!dll_subroutine_init init_cyclic_flap_controller ; 
!dll_subroutine_update update_cyclic_flap_controller ; 
!arraysizes_init    19  1 ; 
!arraysizes_update   8 14 ; 
!begin init ; 
!; .Reference Gains: 
!constant  1  2.3679E+01 	 	; Reference Lead Angle [deg]        
!constant  2  1.4469E-03 	 	; Reference kp         [deg/kNm]    
!constant  3  1.4120E-03 	 	; Reference ki         [deg/(kNm s)]
!constant  4    0.0  	 ; Differential gain at zero pitch [deg*s/kNm]
!constant  5   10.0  	 ; Max amplitude of cyclic flap action [deg]
!; Filter characterisitcs
!constant  6   10.0  	 ; Low-Pass Filter Frequency [Hz]
!constant  7    0.7  	 ; Low-Pass damping ratio [-]
!; Operational Parameter
!constant  8   20.0		 ; Time at which to start the flap ctrl [s]
!constant  9    0.0		 ; Collective flap Angle for shut-down opt [deg]
!constant 10    0.0		 ; Colelctive flap angle for start up opt  [deg]
!; Threshold on rated power
!constant 11    0.1      ; Threshold of "rated power indicator" above which flaps are fully on [-] 
!;  .Gain Scheduling: (if scheduling parameters are set to 0.0, than reference one is kept) 
!constant 12  +7.9936E-02 	 	; Reference Theta0 [rad]    
!constant 13  +0.0000E+00 	 	; Min Theta0 Sched [rad] - gains are not modified below 
!constant 14  +4.5379E-01 	 	; Max Theta0 Sched [rad] - gains are not modified above 
!constant 15  +3.0562E+01 	 	; Lin.Sched. Psi   [deg/rad]
!constant 16  -6.1023E+01 	 	; Quad.Sched. Psi  [deg/rad2]
!constant 17  +7.0023E-04 	 	; Lin.Sched. Kp    [./rad]  
!constant 18  +2.9845E-05 	 	; Lin.Sched. Ki    [./rad]  
!constant 19  +0.0000E+00 	 	; Lin.Sched. Kd    [./rad]  
!end init ; 
!; 
!begin output; 
!general time                            ;   1: general time [s]       
!constraint bearing1 shaft_rot 1 only 1  ;   2: Azimuth angle of blade 1  (zero = blade up) [rad]  
!mbdy momentvec blade1 2 2 blade1 only 1 ;   3: Flap BRM of blade 1 (pos. bend. forward) [kNm]      
!mbdy momentvec blade2 2 2 blade2 only 1 ;   4: Flap BRM of blade 2 (pos. bend. forward) [kNm]      
!mbdy momentvec blade3 2 2 blade3 only 1 ;   5: Flap BRM of blade 3 (pos. bend. forward) [kNm]      
!dll inpvec 1 22                         ;   6: Status flag from collective pitch controller [0=normal operation][1,2,3,4,5,-2:shut down][-1:start up][other:to 0.0]  
!dll inpvec 1 14                         ;   7: Torque limit switch based on pitch    [-]  
!dll inpvec 1 24                         ;   7: Torque limit switch based on pitch    [-]  
!end output; 
!end type2_dll; 
! *********************************************************************************
!
use cyclic_flap_controller_fcns_mod 
implicit none
contains
!**************************************************************************************************
subroutine init_cyclic_flap_controller(array1,array2)
! Initialize Cyclic Flap Controller
implicit none
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_cyclic_flap_controller'::init_cyclic_flap_controller
real*8 array1(19),array2(1)
! Input parameters
!   . Gains and Lead Angle
!   1: constant   1  ; Lead angle               [deg]
!   2: constant   2  ; Ref. Proportional gain   [deg/kNm]
!   3: constant   3  ; Ref. Integral gain       [deg/(kNm*s)]  
!   4: constant   4  ; Ref. Differential gain   [deg*s/kNm]  
!   5: constant   5  ; Maximum amplitude on cyclic flap action [deg]
!   . Filter Characteristics:
!   6: constant   6  ; Low-pass filter frequency        [Hz]
!   7: constant   7  ; Low-pass filter damping ratio    [-]
!   . Operational Parameters
!   8: constant   8  ; Time at which to start with flap action [s]. Set to -1 to start from beginning
!   9: constant   9  ; Collective Flap angle for shut down operations and idle (status 1,2,3,4,5,-2) [deg]
!  10: constant  10  ; Collective Flap angle for start up (status -1) [deg]
!   . Gain Scheduling:
!  11: constant  11  ; Threshold for full power switch [-]
!  12: constant 12   ; Reference Theta0 [rad] 
!  13: constant 13   ; Min Theta0 Sched [rad] 
!  14: constant 14   ; Max Theta0 Sched [rad] 
!  15: constant 15   ; Lin.Sched. Psi   [deg/rad]
!  16: constant 16   ; Quad.Sched. Psi  [deg/rad2]
!  17: constant 17   ; Lin.Sched. Kp    [./rad]  
!  18: constant 18   ; Lin.Sched. Ki    [./rad]
!  19: constant 19   ; Lin.Sched. Kd    [./rad]

! -- Read in parameters -- !
    ! . Gains and Lead Angle
psi_ref          = array1(1)
PID_cos_var.Kpro = array1(2)*1.d0
PID_cos_var.Kint = array1(3)*1.d0
PID_cos_var.Kdif = array1(4)*1.d0
PID_cos_var.outmin =-array1(5)*1.d0
PID_cos_var.outmax = array1(5)*1.d0
fllim_mbc          = array1(5)*1.d0     ! Repeat after merging of cos and sin parts (avoids saturation of signals)
PID_cos_var.velmax = 0.d0               ! Limit on sign change rate in reference signal. Set to 0.0 for no limit (applied later by servo)
    ! . Filter Characterisitcs
LP2_cos_var.f0   = array1(6) 
LP2_cos_var.zeta = array1(7)
    ! . Operational parameters
ctrl_tstart      = array1(8)
fldefl_shutd     = array1(9)
fldefl_strup     = array1(10)
    ! . Power Threshold    
thr_ratcyclic = array1(11)
    ! . Gain Scheduling
theta_gsref = array1(12)          ! [rad] Reference pitch angle for scheduling (at which given values are applied) 
theta_gsmin = array1(13)          ! [rad] Pitch angle, below which gains and psi are kept constant
theta_gsmax = array1(14)          ! [rad] Pitch angle, above which gains and psi are kept constant
psi_lin     = array1(15)          ! [deg/rad]  Linear term for scheduling of psi lead angle
psi_quad    = array1(16)          ! [deg/rad2] Quadratic term for scheduling of psi
kp_lin      = array1(17)          ! [./rad]    Linear term for scheduling of Prop gain
ki_lin      = array1(18)          ! [./rad]    Linear term for scheduling of Int gain
kd_lin      = array1(19)          ! [./rad]    Linear term for scheduling og Der gain
! The two PID controllers are the same
LP2_sin_var = LP2_cos_var
PID_sin_var = PID_cos_var
! Initialize the transformation matrices
call setup_mbc_matrices
! -- Dummy Output -- !
array2(1) = 1.d0
return
end subroutine init_cyclic_flap_controller
!**************************************************************************************************
subroutine update_cyclic_flap_controller(array1,array2)
implicit none
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_cyclic_flap_controller'::update_cyclic_flap_controller
real*8 array1(8),array2(14)
! Input array1 must contains
!
!    1: general time [s]     
!    2: Azimuth angle of blade 1 (zero = blade up) [rad]
!    3: Flap BRM of blade 1 (pos. bend. forward) [kNm]    
!    4: Flap BRM of blade 2 (pos. bend. forward) [kNm]    
!    5: Flap BRM of blade 3 (pos. bend. forward) [kNm]    
!    6: Status flag from main controller: dll inpvec 1 22 [0=normal operation]: set it to general constant -3 or any to have it off
!                    0. Normal Operation
!                    1. Overspeed 25%
!                    2. Grid loss flag from generator
!                    3. Shake-guard activated
!                    4. Normal Shut down
!                    5. Emergency shut down
!                   -1. Start Up
!                   -2. Idling
!       . Gain scheduling parameters:
!    7: Pullpower switch from main ctrl: dll inpvec 1 14 [1.0 for above rated]
!    8: Filtered Mean Pitch for gain scheduling from main ctrl: dll inpvec 1 24  [rad]
!
! Output array2 contains
!
!    1: Flap angle reference for Flap 1       [deg]    
!    2: Flap angle reference for Flap 2       [deg]    
!    3: Flap angle reference for Flap 3       [deg]    
!    4: Cosine moment [kNm]    
!    5: Sine moment [kNm]    
!    6: Filtered cosine moment [kNm]    
!    7: Filtered sine moment [kNm]    
!    8: Cosine flap [deg]    
!    9: Sine flap   [deg]    
!   10: Actual Psi Lead Angle [deg]
!   11: Actual Overall Scaling gain [-]
!   12: Actual Kp gain [deg/kNm]
!   13: Actual Ki gain [deg/kNms]
!   14: Actual Kd gain [deg s/kNm]
!
! Local variables
integer*4 i,CtrlStatus
real*8 time,AziAng,momvec_rot(3),momvec_mbc(3), momvec_mbc_filt(3)        
real*8 gscale_rat, meanpitref               ! Gain Scaling from rated power indicator, reference mean pitch angle 
real*8 psi_now, xx_gs                       ! Scheduled lead angle psi [deg], gain schdeuling helping variable
real*8 kgain(3),yfilt(2)                    ! Gain scheduling factor: Proportional, Integral, Derivative ; Filter Output [1. Filter, 2. average]
real*8 uref_mbc(3),uref_rot(3)              ! Reference control signal in fixed MBC frame 
! --------------------------------------------------------------- !
! Main Body
! --------------------------------------------------------------- !
! --- Read Inputs ---- !
time   = array1(1)
AziAng = array1(2)
momvec_rot = array1(2+rev_blade_no)     ! Blade Root BM in rotating coordinates (change order)
CtrlStatus = int(array1(6))             ! Read status from main ctrl
    ! . Increment time step (may actually not be necessary in type2 DLLs)
if (time-time_old.gt.1.d-6) then
  deltat=time-time_old
  time_old=time
  stepno=stepno+1
endif
    ! . Inputs for Gain Scheduling:
gscale_rat = MAX(MIN(array1(7)/thr_ratcyclic,1.d0),0.d0)     ! . Rated power indicator (0.0 to 1.0). Linear variation from 0.0 to threshold value
meanpitref = MAX(MIN(array1(8),theta_gsmax),theta_gsmin)     ! Pitch used for scheduling, bounded
    ! -- Schedule Lead Angle -- !
IF(ABS(psi_quad).lt.1.d-6)  THEN ! Null quadratic term, linear scheduling
    psi_now = psi_ref + (meanpitref-theta_gsref)*psi_lin
ELSE ! Quadratic scheduling
    xx_gs = meanpitref-theta_gsref    
    IF (psi_quad.lt.0) THEN ! limit scheduling to the ascending part of the curve
        xx_gs = MAX(MIN( MIN(xx_gs,-0.5*psi_lin/psi_quad), theta_gsmax-theta_gsref),theta_gsmin-theta_gsref)
    ENDIF
    psi_now = psi_ref + psi_lin*xx_gs + psi_quad*xx_gs**2
ENDIF
    ! -- Schedule Gains -- ! ! OBS: check if they are zero!
xx_gs    = meanpitref-theta_gsref    
kgain(1) = 1 + kp_lin *xx_gs/(DSIGN(1.d0,PID_cos_var.Kpro)*MAX(ABS(PID_cos_var.Kpro),1.d-8))
kgain(2) = 1 + ki_lin *xx_gs/(DSIGN(1.d0,PID_cos_var.Kint)*MAX(ABS(PID_cos_var.Kint),1.d-8))
kgain(3) = 1 + kd_lin *xx_gs/(DSIGN(1.d0,PID_cos_var.Kdif)*MAX(ABS(PID_cos_var.Kdif),1.d-8))
kgain    = kgain*gscale_rat
    ! -- Inverse Coleman transform to get non-rotating moments -- !
momvec_mbc=matmul(InvBmat(AziAng),momvec_rot)
    ! -- Low Pass filter on mbc moments -- !
momvec_mbc_filt(1) = 0.d0   ! don't care about collective bending moment
yfilt = lowpass2orderfilt(deltat,stepno,LP2_cos_var,momvec_mbc(2))
momvec_mbc_filt(2) = yfilt(1)
yfilt = lowpass2orderfilt(deltat,stepno,LP2_sin_var,momvec_mbc(3))
momvec_mbc_filt(3) = yfilt(1)
    ! -- Switch depending on time -- !
IF (time.lt.ctrl_tstart) THEN    ! all off before given time
    uref_rot(1:3) = 0.d0    
ELSE                             ! turn ctrl on 
    ! -- Switch depending on operation -- !
    SELECT CASE (CtrlStatus)
    CASE(0)     ! Turbine in normal production, ctrl is on.
            ! -- Control Actions from PID in MBC frame -- !
        uref_mbc(1) = 0.d0      ! Collective flap deflection is zero
        uref_mbc(2) = PID(stepno,deltat,kgain,PID_cos_var,momvec_mbc_filt(2))
        uref_mbc(3) = PID(stepno,deltat,kgain,PID_sin_var,momvec_mbc_filt(3))
            ! -- Anticipate by lead angle -- !
        uref_mbc = matmul(Blead(psi_now*degrad),uref_mbc)
            ! . Repeat limits application, after distributing sin and cos components
        uref_mbc(1) = 0.d0      ! Collective flap deflection is still zero
        uref_mbc(2) = MAX(MIN(uref_mbc(2),fllim_mbc),-fllim_mbc)
        uref_mbc(3) = MAX(MIN(uref_mbc(3),fllim_mbc),-fllim_mbc)
            ! -- Colemann Transform to rotating frame w. Lead angle effects -- !
        uref_rot(rev_blade_no) = matmul(Bmat(AziAng),uref_mbc)    
    !    uref_rot(rev_blade_no) = matmul(Bmat(AziAng+psi_now*degrad),uref_mbc)   ! Include lead angle here 
    CASE(1,2,3,4,5,-2)  ! Shut down states
        uref_rot(1:3) = fldefl_shutd
    CASE(-1)            ! Start up case
        uref_rot(1:3) = fldefl_strup
    CASE DEFAULT ! Default flap to zero (use it to turn ctrl off)
        uref_rot(1:3) = 0.d0    
    END SELECT
ENDIF
! --------------------------------------------------------------- !
! Output
! --------------------------------------------------------------- !
array2(1:3) = uref_rot(1:3)             ! Output for reference Flap 1,2,3    
array2(4:5) = momvec_mbc(2:3)           ! Mbc moments: cos and sin components
array2(6:7) = momvec_mbc_filt(2:3)      ! Mbc cos moment, filtered    
array2(8:9) = uref_mbc(2:3)             ! Mbc flap signal: cos and sin
    ! . Lead angle
array2(10)  = psi_now                   ! Lead angle for current pitch angle
    ! . Gains
array2(11) = gscale_rat                  ! Gain scaling on rated power
array2(12) = kgain(1)*PID_cos_var.Kpro   ! Gain scheduling factor: prop, int, deriv.
array2(13) = kgain(2)*PID_cos_var.Kint   ! Gain scheduling factor: prop, int, deriv.
array2(14) = kgain(3)*PID_cos_var.Kdif   ! Gain scheduling factor: prop, int, deriv.
return
end subroutine update_cyclic_flap_controller
!**************************************************************************************************
end module cyclic_flap_controller_mod
