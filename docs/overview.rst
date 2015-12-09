========
Overview
========

The basic DTU Wind Energy controller is designed for pitch-regulated, variable speed wind turbines. The controller features both partial and full load operation capabilities as well as switching mechanisms ensuring smooth transition between the two modes of operation.

The partial and full load controllers are both based on classical proportional-integral control theory as well as on additional features such as a drivetrain and tower damper, a rotor speed exclusion zone, and a notch filter mitigating the influence of rotor speed dependent variations in the feedback. The controller relies on generator speed as the primary feedback sensor. Additionally, the reference generator power is used as a feedback term to smoothen the switching between partial and full load operation. Optionally, a low-pass filtered wind speed measurement can be used for wind speed dependent minimum blade pitch in partial load operation. The controller uses the collective blade pitch angle and electromagnetic generator torque to control the wind turbine. In full load operation a feedback term from the collective blade pitch angle is used to schedule the gains of the proportional-integral controller to counter the effects of changing dynamics of the wind turbine for different wind speeds.

Blade pitch servo and generator models are not included in this controller and should be modeled separately, if they are to be included in the simulations.
