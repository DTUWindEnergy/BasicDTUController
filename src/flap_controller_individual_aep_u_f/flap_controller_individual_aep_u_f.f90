module aep_u_f_flap_ctrl_mod
!
! Control Dll of type 2:
! Dll for applying flap control actions for increasing power production in partial load based on optimal static beta tracking based on wind speed,
! decrease extreme design loads with a cut-off switch based on blade root bending moments and limit fatigue in the full load region with a PD control
! (tuned base on linearised model's state space matrices from hawcstab2 with the Ziegler Nichiol's method) 
! Used based on flapwise bending root moment signal.  
! 
!    
! v.1.0 26/4/2018, TKBA
!    
   ! ******************************* example htc input *******************************
!    begin type2_dll;
!	name aep_u_flap_ctrl ;
!   filename  ./control/aep_u_f_flap_controller.dll ;
!	dll_subroutine_init init_aep_u_f_flap_controller ;
!	dll_subroutine_update update_aep_u_f_flap_controller ;	
!   arraysizes_init   22 1 ;
!	arraysizes_update  6 4 ;
!	begin init ;
!       ; - Gain scheduling and parameters for fatigue reduction part
!		constant 1  0 ;                                      [1]  1st order term for linear interp of proportional term gain scheduling based on V_filt
!		constant 2  0 ;                                      [2]  offset for linear interp of proportional term gain scheduling based on V_filt
!		constant 3  0 ;                                      [3]  1st order term for linear interp of differential term gain scheduling based on V_filt
!		constant 4  0 ;                                      [4]  offset for linear interp of differential term gain scheduling based on V_filt	
!		constant 5 12;                                       [5]  Min. filtered V, below min gain is kept [m/s]
!		constant 6 28 ;                                      [6]  Max. filtered V, above max gain is kept [m/s]
!		constant 7 1 ;								         [7]  Threshold of "rated power indicator" above which flaps are fully on [-]
!       ; - HP Filter setup  - ;
!        constant 8  0.05 ;	                                 [8]  corner frequency for blade flapwise moments high-pass filter [rad/s]
!	    ; - Time - ;
!        constant 9  0.02 ;                                  [9]  simulation time step [s]
!		; - Flap angle when the turbine is not in production mode
!		constant  10 -15 ;                         	         [10] Flap angle to take when over-speed is detected [deg]	
!       ;- Limit for operation status from main ctrl
!       constant 11  0     ;                                 [11] Value of status signal from main controller for operation with the turbine connected [-]    		  
!       constant 12  90    ;                                 [12] global wind speed upper limit after which beta is fixed to target [m/s]
!       ; - Gain scheduling for optimal beta tracking in partial load
!       constant 13  0  ;                                    [13] gain 1st order for AEP opti beta tracking (flap 1)
!       constant 14  0  ;                                    [14] gain 2nd order for AEP opti beta tracking (flap 1)	
!       constant 15  0  ;                                    [15] gain 3rd order for AEP opti beta tracking (flap 1)	
!       constant 16  0  ;                                    [16] gain 4th order for AEP opti beta tracking (flap 1)	 
!       constant 17  0  ;                                    [17] offset  for AEP opti beta tracking based on filtered wind speed (flap 1)
!       ; - Threshold and limits for switching
!       constant 18  13  ;                                   [18] speed limit to stop using AEP controller based on V LP filtered [m/s] 	 
!       constant 19  100000  ;                               [19] MxBR load threshold for cut off switch part for extreme load alleviation [kNm]
!       constant 20  100000  ;                               [20] Blade root bending moment threshold after which the fatigue load reduction mode will be deactivated [kNm]
!       constant 21  18      ;                               [21] Speed Limit to switch to second threshold in order to decouple extreme and fatigue [m/s]
!       constant 22  100000  ;                               [22] Blade root bending moment second (for V higher than the 2nd lim threshold) after which the fatigue load reduction mode will be deactivated [kNm]
!
!	end init ;
!	begin output;
!		; - Blade root moments - ;
!		mbdy momentvec  blade1 3 1 blade1 only 1 ;            [1]   Blade 1 flapwise root bending moment [kNm]
!		mbdy momentvec  blade2 3 1 blade2 only 1 ;            [2]   Blade 2 flapwise root bending moment [kNm]
!		mbdy momentvec  blade3 3 1 blade3 only 1 ;            [3]   Blade 3 flapwise root bending moment [kNm]
!		; - Input from main controller ;
!       dll inpvec 1 6 ;                                      [4]  filtered V [m/s]
!		dll inpvec 1 22 ;								 	  [5]  Main power control status: 0. normal operation [-]
!		dll inpvec 1 14 ;                               	  [6]  Power rating status: 0. below rated power operation 1. above rated power operation[-]		
!end output;
!end type2_dll;
! *********************************************************************************
!
implicit none
!   Declare all variables   !
! . Gain scheduling parameters
real*8 gs_v_min, gs_v_max                   ! Wind speeds for gain scheduling [m/s]: reference point, min. pitch (below min gain is kept), max. pitch (above max gain is kept)
real*8 thr_ratflaps                         ! Threshold of controller rated operation output to consider for fully operational flaps 
real*8 flap_target                          ! Flap angle constant in grid loss or idling from controller status
! - Internal Variables for update routine - !
real*8 dtsim                                ! Simulation time step dt (used in filters and for flap rate)
real(8),dimension(3) ::  mx_old,mx_filt_old ! Blade root bending moment of previous step stored (filtered and raw)   [kNm]
real*8 gain1_p, gain1_d                     ! 1st order terms for gain scheduling of Kd and Kp with V_filt
real*8 offset_p,offset_d                    ! offset terms for gain scheduling of Kd and Kp with V_filt
real*8 fc_mxfilt                            ! Corner frequency of blade root mom. high pass filter [Hz]
real*8 V_filt                               ! Input: LP filtered wind speed for gain scheduling
real*8 r_switch_filt                        ! Power rating status: 0. below rated power, 1. above rated power, 0<r_switch_filt<1. smooth transition
real*8 status_sig                           ! Op status of main controller: -1 cut in, 0. Operation, +1 stopping, etc...(first approach use output 22 [status signal from main controller])  
real*8 Alpha1	                            ! constant for blade root flapwise moments high-pass filter
real*8 flap_moment_1_filter_new	            ! high-pass filtered blade 1 root flapwise moment at current time step
real*8 flap_moment_2_filter_new	            ! high-pass filtered blade 2 root flapwise moment at current time step
real*8 flap_moment_3_filter_new	            ! high-pass filtered blade 3 root flapwise moment at current time step
real*8 flap_moment_1_D                      ! derivated blade 1 root flapwise moment at current time step
real*8 flap_moment_2_D                      ! derivated blade 2 root flapwise moment at current time step
real*8 flap_moment_3_D                      ! derivated blade 3 root flapwise moment at current time step
real*8 beta_ref_1_1, beta_ref_2_1,beta_ref_3_1    ! beta_ref_i output for i each blade to be passed into the flap servo
real*8 gfl_p,gfl_d                          ! Actual gains
real*8 full_load_ind                        ! full load indicator (0<full_load_ind<1) coming from main ctrl (output 14)
real*8 Mx_1, Mx_2,Mx_3                      ! Actual blade root bending moments   [kNm]
real*8 status_lim                           ! value of status signal (status_sig) from main controller that means the turbine is connected and operating
real*8 ctrl_mode_out                        ! output of controller's operating mode 1:AEP increase, 2:Extreme load reduction,  3:Fatigue Loads reduction
real*8 V_limit_global                       ! upper filtered wind speed limit after which the flaps will move to a fixed angle
real*8 gs_PD_V_filt                         ! filtered wind speed limited within the PD scheduled limits, used only in the fatigue part
real*8 load_thres_extr                      ! MxBR threshold for cut-off controller to be activated [kNm]
real*8 gain_1_stat_1                        ! 1st order gain for opti beta tracking below rated (flap 1) 
real*8 gain_2_stat_1                        ! 2nd order gain for opti beta tracking below rated (flap 1) 
real*8 gain_3_stat_1                        ! 3rd order gain for opti beta tracking below rated (flap 1) 
real*8 gain_4_stat_1                        ! 4th order gain for opti beta tracking below rated (flap 1) 
real*8 offset_stat_1                        ! offset for opti beta tracking below rated (flap 1)
real*8 fatigue_load_thres                   ! threshold of blade root bending moment above which the fatigue controller will stop
real*8 V_limit_aep                          ! filtered wind speed threshold until which opti beta tracking is active
real*8 V_limit_fatigue_thres_2              ! filtered wind speed threshold after which fatigue MxBR threshold should be changed to 2
real*8 fatigue_load_thres_2                 ! second threshold of blade root bending moment in wind speeds higher than the previous limit above which the fatigue controller will stop
!
contains
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! ----------------------------- Initialization SubRoutine ---------------------------------
subroutine init_aep_u_f_flap_ctrl(array1,array2)
implicit none
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_aep_u_f_flap_ctrl' :: init_aep_u_f_flap_ctrl
real*8 array1(22) ! Input array, from hawc2 to dll
real*8 array2(1)  ! Array dll -> hawc2 In this case dummy 0
!
!  Input parameters
!   . Scheduling Parameters:
!   1: constant 1   ; 1st order term for linear interp of proportional term gain scheduling based on V_filt
!   2: constant 2   ; offset for linear interp of proportional term gain scheduling based on V_filt	
!   3: constant 3   ; 1st order term for linear interp of differential term gain scheduling based on V_filt
!   4: constant 4   ; offset for linear interp of differential term gain scheduling based on V_filt	
!   5: constant 5   ; Min. wind speed, below min gain is kept [m/s]
!   6: constant 6   ; Max.  wind speed, above max gain is kept [m/s]
!   7: constant 7   ; Threshold of "rated power indicator" above which flaps are fully on [-] should be between 0 and 1
!   8: constant 8   ; corner frequency for blade flapwise moments high-pass filter [rad/s]
!   9: constant 9   ; simulation time step [s]
!  10: constant 10  ; Fixed beta angle when the turbine is idling or parked with rotor locked or off-grid or.... not in normal op
!  11: constant 11  ; Value of the status signal that indicates normal operation with grid connection
!  12: constant 12  ; wind speed limit at which the flaps should be set to beta fixed n [m/s] 
!  13: constant 13   ; gain 1st order for opti beta tracking below rated (flap 1) [-]
!  14: constant 14   ; gain 1st order for opti beta tracking below rated (flap 1) [-]
!  15: constant 15   ; gain 3rd order for opti beta tracking below rated (flap 1) [-]
!  16: constant 16   ; gain 4th order for opti beta tracking below rated (flap 1) [-]
!  17: constant 17   ; offset beta tracking below rated (flap 1) [-]
!  18: constant 18   ; V_filt upper limit until which opti static beta tracking is active [m/s]
!  19: constant 19   ; MxBR threshold for cut-off controller to be activated [kNm]
!  20: constant 20   ; Blade root bending moment threshold after which the fatigue load reduction mode will be deactivated [kNm]
!  21: constant 21   ; Speed Limit to switch to second threshold in order to decouple extreme and fatigue [m/s]
!  22: constant 22   ; Blade root bending moment second (for V higher than the 2nd lim threshold) after which the fatigue load reduction mode will be deactivated [kNm]
!
! - Read in scheduling parameters for PD fatigue part - 
gain1_p      = array1(1)
offset_p     = array1(2)
gain1_d      = array1(3)
offset_d     = array1(4)
gs_v_min     = array1(5)
gs_v_max     = array1(6)
thr_ratflaps = array1(7)   ! Read in at which value of "rated conditions indicator" have flap fully on 
! - Read in filter setup and PID gains - 
fc_mxfilt    = array1(8)
dtsim        = array1(9)
! - Read in flap angle at no connection - !
flap_target  = array1(10)
status_lim   = array1(11)
! - Read in global V limit at which all flaps will be turned to target value - !
V_limit_global  = array1(12)  ! [m/s]  V upper limit to apply fixed angle 
! - Read in scheduling parameters for opti static beta tracking below rated - 
gain_1_stat_1  = array1(13)   ! [-] gain 1st order (flap 1)
gain_2_stat_1  = array1(14)   ! [-] gain 2nd order (flap 1)
gain_3_stat_1  = array1(15)   ! [-] gain 3nd order (flap 1)
gain_4_stat_1  = array1(16)   ! [-] gain 4nd order (flap 1)
offset_stat_1  = array1(17)   ! [-] offset (flap 1)
V_limit_aep  = array1(18)   ! [m/s] V_filt upper limit until which opti static beta tracking is active
!- Threshold for extreme load reduction switch to be activated
load_thres_extr = array1(19)! 
!- Threshold after which the fatigue load reduction mode will be deactivated
fatigue_load_thres = array1(20)!   
V_limit_fatigue_thres_2 = array1(21)!             
fatigue_load_thres_2    = array1(22)!               
!
! - Initialize blade root bennding moment values to 0.0 - !
mx_old = (/ 0.d0, 0.d0, 0.d0 /)
mx_filt_old = mx_old

! dummy output to HAWC2
array2(1) = 1.d0;
end subroutine init_aep_u_f_flap_ctrl
! ------------------------------------------------------------------------------ !
! ----------------------------- Update SubRoutine -------------------------------!
! ------------------------------------------------------------------------------ !
subroutine update_aep_u_f_flap_ctrl(array1,array2)
implicit none
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_aep_u_f_flap_ctrl' :: update_aep_u_f_flap_ctrl
real(8) array1(6)	! Input array, from hawc2 to dll
real(8) array2(4)	! Array dll -> hawc2
! Input array1 must contain
!   
!    1: Blade 1 flapwise root bending moment Mx [kNm]
!    2: Blade 2 flapwise root bending moment Mx [kNm]
!    3: Blade 3 flapwise root bending moment Mx [kNm]
!    4: LP Filtered wind speed [m/s]   (output 6 of main ctrl)
!    5: Status signal given by the main controller for normal operation (0 normal op, 1 overspeding, 2 not connected to grid) usually output 22 from main ctrl
!    6: Power rating status: 0. below rated power, 1. above rated power, 0<r_switch_filt<1. smooth transition ( usually output 14 from main ctrl
!
! Output array2 contains
!
!    1: Flap angle reference for Flap 1 on blade 1 [deg]
!    2: Flap angle reference for Flap 1 on blade 2 [deg]
!    3: Flap angle reference for Flap 1 on blade 3 [deg]    
!    4: output of flap controller's operating mode 1:AEP increase, 2:Extreme load reduction,  3:Fatigue Loads reduction, 4: fixed to target turbine not in production mode or over the global V_lim, 5: none of the cntrl modes switched to 0 beta
!
! --------------------------------------------------------------- !
! Main Body
! --------------------------------------------------------------- !
!--- Read Inputs ---- !
! . Bending moments
Mx_1 = array1(1)
Mx_2 = array1(2)
Mx_3 = array1(3)
! . Signals from main controller 
V_filt           = array1(4)                ! Filtered wind speed from main controller
status_sig       = array1(5)                ! Main Ctrl status flag
full_load_ind    = array1(6)                ! indicator from main ctrl of full load(1) or partial load (0)

IF (full_load_ind/thr_ratflaps>0.8) THEN
    r_switch_filt        = MAX(MIN(full_load_ind/thr_ratflaps,1.d0),0.d0)     !  Rated power indicator (0.0 to 1.0). Smooth transition from below, above rated based on power rating status
ELSE
    r_switch_filt        = 0.d0
ENDIF

! --------- First calculate and store all the values and inputs independently of the operating conditions   --------------- !
! -- 1. High pass filter on blade root flapwise moments -- !
Alpha1=((1/fc_mxfilt)/((1/fc_mxfilt)+dtsim));
flap_moment_1_filter_new=(Alpha1*mx_filt_old(1)) + (Alpha1*(Mx_1-mx_old(1)))
flap_moment_2_filter_new=(Alpha1*mx_filt_old(2)) + (Alpha1*(Mx_2-mx_old(2)))
flap_moment_3_filter_new=(Alpha1*mx_filt_old(3)) + (Alpha1*(Mx_3-mx_old(3)))
    ! . Store for next step:
mx_filt_old(1) = flap_moment_1_filter_new
mx_filt_old(2) = flap_moment_2_filter_new
mx_filt_old(3) = flap_moment_3_filter_new
mx_old(1) = Mx_1
mx_old(2) = Mx_2
mx_old(3) = Mx_3
! -- 2. Derivate filtered root flapw. moment -- 
! derivated flapwise blade root moments
flap_moment_1_D = ((flap_moment_1_filter_new-mx_filt_old(1))/dtsim)
flap_moment_2_D = ((flap_moment_2_filter_new-mx_filt_old(2))/dtsim)
flap_moment_3_D = ((flap_moment_3_filter_new-mx_filt_old(3))/dtsim)
! -- 3. Compute Actual PID gains -- !
gs_PD_V_filt= MAX(MIN(V_filt,gs_v_max),gs_v_min) ! set V filtered among limits
gfl_p = offset_p+ gain1_p *gs_PD_V_filt! can also be made to polynomial regresion instead of this linear tapproximation
gfl_d = offset_d+ gain1_d *gs_PD_V_filt
!
! --Take action according to the different operating conditions -- !
    IF (status_sig /= status_lim) THEN         ! signal status showing that the turbine is not producing (shut down, parking grid loss etc) does not indicate errors like DLC2.2b
		beta_ref_1_1    = flap_target
		beta_ref_2_1    = flap_target
		beta_ref_3_1    = flap_target
		ctrl_mode_out = 4.d0
    ELSE IF (V_filt> V_limit_global) THEN      ! if wind speed is over a threshold  then switch flaps collectively to beta target
		beta_ref_1_1    = flap_target
		beta_ref_2_1    = flap_target
		beta_ref_3_1    = flap_target
		ctrl_mode_out = 4.d0
    ELSE IF (ABS(Mx_1) >= load_thres_extr .OR. ABS(Mx_2) >= load_thres_extr .OR. ABS(Mx_3) >= load_thres_extr ) THEN ! MxBR threshold reached switch flap collectively to target beta
        beta_ref_1_1    = flap_target
		beta_ref_2_1    = flap_target
		beta_ref_3_1    = flap_target
		ctrl_mode_out = 2.d0
	ELSE IF (V_filt <= V_limit_aep)	THEN       ! if filtered wind speed below the threshold and normal status signal is 0 (production) then track optimal static beta for power increase
		IF (V_filt <= 4.d0) THEN               ! when speed below cut in keep the fit to cut in
			beta_ref_1_1 = offset_stat_1+(gain_1_stat_1*4.d0)+(gain_2_stat_1*(4.d0)**2)+(gain_3_stat_1*(4.d0)**3)+(gain_4_stat_1*(4.d0)**4)
			beta_ref_2_1 = beta_ref_1_1
			beta_ref_3_1 = beta_ref_1_1
		ELSE                                   ! normal operation in partial load controller tracking optimal collective beta for AEP 
			beta_ref_1_1 = offset_stat_1+(gain_1_stat_1*V_filt)+(gain_2_stat_1*V_filt**2)+(gain_3_stat_1*V_filt**3)+(gain_4_stat_1*V_filt**4)
			beta_ref_2_1 = beta_ref_1_1
			beta_ref_3_1 = beta_ref_1_1
	    ENDIF
	    ctrl_mode_out = 1.d0	
    ELSE IF (ABS(Mx_1) <= fatigue_load_thres .OR. ABS(Mx_2) <= fatigue_load_thres .OR. ABS(Mx_3) <= fatigue_load_thres )  THEN    ! Turbine is in normal operation and MxBR is within the normal production operational envelope: PD on blade root bending moment
        IF      (ABS(Mx_1) <= fatigue_load_thres_2 .AND. V_filt >= V_limit_fatigue_thres_2 )  THEN   ! check blade 1
			beta_ref_1_1 = r_switch_filt *(gfl_p *flap_moment_1_filter_new + gfl_d *flap_moment_1_D)
        ELSE IF (ABS(Mx_1) <= fatigue_load_thres .AND. V_filt <= V_limit_fatigue_thres_2 )    THEN    
            beta_ref_1_1 = r_switch_filt *(gfl_p *flap_moment_1_filter_new + gfl_d *flap_moment_1_D)
        ELSE 
			beta_ref_1_1 = 0.d0
        ENDIF	   
		IF     (ABS(Mx_2) <= fatigue_load_thres_2 .AND. V_filt >= V_limit_fatigue_thres_2 )   THEN   ! check blade 2
			beta_ref_2_1 = r_switch_filt *(gfl_p *flap_moment_2_filter_new + gfl_d *flap_moment_2_D)
        ELSE IF (ABS(Mx_2) <= fatigue_load_thres .AND. V_filt <= V_limit_fatigue_thres_2 )    THEN  
            beta_ref_2_1 = r_switch_filt *(gfl_p *flap_moment_2_filter_new + gfl_d *flap_moment_2_D)
		ELSE 
			beta_ref_2_1 = 0.d0
		ENDIF
		IF     (ABS(Mx_3) <= fatigue_load_thres_2 .AND. V_filt >= V_limit_fatigue_thres_2 )   THEN   ! check blade 3
			beta_ref_3_1 = r_switch_filt *(gfl_p *flap_moment_3_filter_new + gfl_d *flap_moment_3_D)
        ELSE IF (ABS(Mx_3) <= fatigue_load_thres .AND. V_filt <= V_limit_fatigue_thres_2 )    THEN  
            beta_ref_3_1 = r_switch_filt *(gfl_p *flap_moment_3_filter_new + gfl_d *flap_moment_3_D)
        ELSE
			beta_ref_3_1 = 0.d0
        ENDIF
		ctrl_mode_out = 3.d0
    ELSE   ! Any other case, set the flaps to 0.0 
        beta_ref_1_1    = 0.d0
        beta_ref_2_1    = 0.d0
        beta_ref_3_1    = 0.d0
		ctrl_mode_out = 5.d0
    ENDIF 
! --------------------------------------------------------------- !
! Output to servo
! --------------------------------------------------------------- !
! flap control actions
 array2(1)  = beta_ref_1_1
 array2(2)  = beta_ref_2_1
 array2(3)  = beta_ref_3_1

 array2(4)  = ctrl_mode_out

end subroutine update_aep_u_f_flap_ctrl
!**************************************************************************************************
end module aep_u_f_flap_ctrl_mod