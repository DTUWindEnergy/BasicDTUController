Inputs/Outputs
==============

init_regulation
---------------

The following parameters need to be specified as parameters in ``init_regulation()``

-  1:  Rated power [kW]
-  2:  Minimum rotor speed [rad/s]
-  3:  Rated rotor speed [rad/s]
-  4:  Maximum allowable generator torque [Nm]
-  5:  Minimum pitch angle, PitchMin [deg], if |PitchMin|>90, then a table of <wsp,PitchMin> is read from a file named 'wptable.n', where n=int(PitchMin)
-  6:  Maximum pitch angle [deg]
-  7:  Maximum pitch velocity operation [deg/s]
-  8:  Frequency of generator speed filter [Hz]
-  9:  Damping ratio of speed filter [-]
- 10:  Frequency of free-free DT torsion mode [Hz], if zero no notch filter used 

Partial load control parameters

-  11:  Optimal Cp tracking K factor [Nm/(rad/s)^2], Qg=K*Omega^2, K=eta*0.5*rho*A*Cp_opt*R^3/lambda_opt^3
-  12:  Proportional gain of torque controller [Nm/(rad/s)]
-  13:  Integral gain of torque controller [Nm/rad]
-  14:  Differential gain of torque controller [Nm/(rad/s^2)]

Full load control parameters

-  15:  Generator control switch [1=constant power, 2=constant torque]
-  16:  Proportional gain of pitch controller [rad/(rad/s)]
-  17:  Integral gain of pitch controller [rad/rad]
-  18:  Differential gain of pitch controller [rad/(rad/s^2)]
-  19:  Proportional power error gain [rad/W]
-  20:  Integral power error gain [rad/(Ws)]
-  21:  Coefficient of linear term in aerodynamic gain scheduling, KK1 [deg]
-  22:  Coefficient of quadratic term in aerodynamic gain scheduling, KK2 [deg^2] (if zero, KK1 = pitch angle at double gain)
-  23:  Relative speed for double nonlinear gain [-]

Cut-in simulation parameters

-  24:  Cut-in time [s]
-  25:  Time delay for soft start of torque [1/1P]

Cut-out simulation parameters

-  26:  Cut-out time [s]
-  27:  Time constant for linear torque cut-out [s]
-  28:  Stop type [1=normal, 2=emergency]
-  29:  Time delay for pitch stop after shut-down signal [s]
-  30:  Maximum pitch velocity during initial period of stop [deg/s]
-  31:  Time period of initial pitch stop phase [s] (maintains pitch speed specified in constant 30)
-  32:  Maximum pitch velocity during final phase of stop [deg/s]

Expert parameters (keep default values unless otherwise given)

-  33:  Lower angle above lowest minimum pitch angle for switch [deg]
-  34:  Upper angle above lowest minimum pitch angle for switch [deg], if equal then hard switch
-  35:  Ratio between filtered speed and reference speed for fully open torque limits [%]
-  36:  Time constant of 1st order filter on wind speed used for minimum pitch [1/1P]
-  37:  Time constant of 1st order filter on pitch angle used for gain scheduling [1/1P]

Drivetrain damper

-  38:  Proportional gain of active DT damper [Nm/(rad/s)], requires frequency in input 10

Overspeed

-  39:  Overspeed percentage before initiating turbine controller alarm (shut-down) [%]

Additional non-linear pitch control term (not used when all zero)

-  40:  Err0 [rad/s] 
-  41:  ErrDot0 [rad/s^2]
-  42:  PitNonLin1 [rad/s]

Storm control command

-  43:  Wind speed 'Vstorm' above which derating of rotor speed is used [m/s]
-  44:  Cut-out wind speed (only used for derating of rotor speed in storm) [m/s]

Safety system parameters

-  45:  Overspeed percentage before initiating safety system alarm (shut-down) [%]
-  46:  Max low-pass filtered tower top acceleration level before initiating turbine controller alarm (shut-down) [m/s^2]

Turbine parameter

-  47:  Nominal rotor diameter [m]

Parameters for rotor inertia reduction in variable speed region

-  48:  Proportional gain on rotor acceleration in variable speed region [Nm/(rad/s^2)] (not used when zero)

Parameters for alternative partial load controller with PI regulated TSR tracking

-  49:  Optimal tip speed ratio [-] (only used when K=constant 11 = 0 otherwise  Qg=K*Omega^2 is used)

Parameters for adding aerodynamic drivetrain damping on gain scheduling

-  50:  Proportional gain of aerodynamic DT damping [Nm/(rad/s)]
-  51:  Coefficient of linear term in aerodynamic DT damping scheduling, KK1 [deg]
-  52:  Coefficient of quadratic term in aerodynamic DT damping scheduling, KK2 [deg^2]


init_regulation_advanced
------------------------
When using the ``init_regulation_advanced()`` the following inputs need to be added 

Torque exclusion zone

- 53:  Torque exclusion zone: Low speed [rad/s]
- 54:  Torque exclusion zone: Low speed generator toque [Nm]
- 55:  Torque exclusion zone: High speed [rad/s]
- 56:  Torque exclusion zone: High speed generator toque [Nm]
- 57:  Time constant of reference switching at exclusion zone [s]
 
DT torsion mode damper

- 58:  Frequency of notch filter [Hz]
- 59:  Damping of BP filter [-]
- 60:  Damping of notch filter [-]
- 61:  Phase lag of damper [s] =>  max 40*dt 
 
Fore-aft Tower mode damper

- 62:  Frequency of BP filter [Hz]
- 63:  Frequency of notch fiter [Hz]
- 64:  Damping of BP filter [-]
- 65:  Damping of notch filter [-]
- 66:  Gain of damper [-]
- 67:  Phase lag of damper [s] =>  max 40*dt
- 68:  Time constant of 1st order filter on PWR used for fore-aft Tower mode damper GS [Hz]
- 69:  Lower PWR limit used for fore-aft Tower mode damper GS [-]
- 70:  Upper PWR limit used for fore-aft Tower mode damper GS [-]
 
Side-to-side Tower mode filter

- 71:  Frequency of Tower side-to-sede notch filter [Hz]
- 72:  Damping of notch filter [-]
- 73:  Max low-pass filtered tower top acceleration level before initiating safety system alarm (shut-down) [m/s^2]
 
Additional filters

- 74:  Time constant of 1st order filter on pitch angle used for switch [1/1P]
- 75:  Time constant of 1st order filter on tower top acceleration used for ShakeGuard [1/1P]

 Gear ratio

- 76:  Gear ratio (To be used only if input #2 refers to HSS)
   
update_regulation
-----------------

Inputs:

-   1: general time                            [s]
-   2: constraint bearing1 shaft_rot 1 only 2  [rad/s] Generator speed (Default LSS, if HSS insert gear ratio in input #76)
-   3: constraint bearing2 pitch1 1 only 1     [rad]
-   4: constraint bearing2 pitch2 1 only 1     [rad]
-   5: constraint bearing2 pitch3 1 only 1     [rad]
- 6-8: wind free_wind 1 0.0 0.0 hub height     [m/s] global coords at hub height
-   9: elec. power   [W]
-  10: grid flag   [1=no grid,0=grid]
-  11: Tower top x-acceleration   [m/s^2]
-  12: Tower top y-acceleration   [m/s^2]

Outputs:

-  1: Generator torque reference               [Nm]
-  2: Pitch angle reference of blade 1         [rad]
-  3: Pitch angle reference of blade 2         [rad]
-  4: Pitch angle reference of blade 3         [rad]
-  5: Power reference                          [W]
-  6: Filtered wind speed                      [m/s]
-  7: Filtered rotor speed                     [rad/s]
-  8: Filtered rotor speed error for torque    [rad/s]
-  9: Bandpass filtered rotor speed            [rad/s]
- 10: Proportional term of torque contr.       [Nm]
- 11: Integral term of torque controller       [Nm]
- 12: Minimum limit of torque                  [Nm]
- 13: Maximum limit of torque                  [Nm]
- 14: Torque limit switch based on pitch       [-]
- 15: Filtered rotor speed error for pitch     [rad/s]
- 16: Power error for pitch                    [W]
- 17: Proportional term of pitch controller    [rad]
- 18: Integral term of pitch controller        [rad]
- 19: Minimum limit of pitch                   [rad]
- 20: Maximum limit of pitch                   [rad]
- 21: Torque reference from DT damper          [Nm]
- 22: Status signal                            [-]
- 23: Total added pitch rate                   [rad/s]
- 24: Filtered pitch angle                     [rad]
- 25: Flag for mechnical brake                 [0=off/1=on]
- 26: Flag for emergency pitch stop            [0=off/1=on]
- 27: LP filtered acceleration level           [m/s^2]
- 28: Rotor speed exlusion zone region         [-]
- 29: Filtered tower top acc. for tower damper [m/s^2]
- 30: Reference pitch from tower damper        [rad]