# Basic DTU Wind Energy controller
## Introduction
The scope of this project is to provide a basic open-source open-access controller that can be used by the community as a reference.  

The basic DTU Wind Energy controller is designed for pitch-regulated variable speed wind turbines.
The controller features both partial and full load operation capabilities as well as switching mechanisms ensuring smooth transition between the two modes of operation. The partial load controller is based on a classical K Omega**2 strategy or on a proportional-integral controller to track constant tip speed ratio. The full load controllers is also based on classical proportional-integral control theory. The controller also includes drivetrain and tower dampers, a rotor speed exclusion zone, and filters on the feedback signal.

Blade pitch servo and generator models are not included in this controller.

## Compatibility
The repository includes project and make files to create DLLs to interface the controller to HAWC2 and BLADED.

The controller is written in Fortran and it is compilable with Intel and GFortran compilers. It can be compiled both on Windows and Linux. 

## References
When using the Basic DTU Wind Energy controller, please refer to the following publications:

* Hansen, MH & Henriksen, LC 2013, Basic DTU Wind Energy controller. DTU Wind Energy. DTU Wind Energy E, no. 0028
