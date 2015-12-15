# Basic DTU Wind Energy controller
## Introduction
The scope of this project is to provide a basic open-source open-access controller that can be used by the community as a reference.  

The basic DTU Wind Energy controller is designed for pitch-regulated variable speed wind turbines.
The controller features both partial and full load operation capabilities as well as switching mechanisms ensuring smooth transition between the two modes of operation. The partial load controller is based on a classical K Omega**2 strategy or on a proportional-integral controller to track constant tip speed ratio. The full load controllers is also based on classical proportional-integral control theory. The controller also includes drivetrain and tower dampers, a rotor speed exclusion zone, and filters on the feedback signal.

Blade pitch servo and generator models are not included in this controller.

## Compatibility
The repository includes Visual Studio project (for Windows) and Makefiles (for Windows and Linux) to create DLLs to interface the controller to HAWC2 and Bladed.

The controller is written in Fortran and it is compatible with Intel and GFortran compilers. It can be compiled both on Windows and Linux. 

## Compilation

- For compilation using Visual Studio open the Visual Studio Solution or Projec files (*.sln, *.vfproj)

- For compilation using Makefile, read the instructions in src/Makefile.README.txt


## Documentation

The documentation can be found [here](http://dtuwindenergy.github.io/BasicDTUController/index.html).

The documentation is written in [Sphinx](http://sphinx-doc.org/) and can be built by typing:

    $ cd docs
    $ make html
  
The documentations requires the [Fortran Sphynx](https://github.com/VACUMM/sphinx-fortran) extension that can be pip installed by typing:

    $ pip install sphinx-fortran

## Tests

For users that compiled using the shipped makefiles a set of tests are present in the folder "src/_tests". 
- Rudimentary matlab and fortran tests are present in this folder to dynamically load the compiled dlls and perform a simple call.
- Running `make` within this folder will perform the Fortran tests using different "Fortran" method to load a dll (i.e. cray-pointer or C-integer handles).



## License

The Basic DTU Wind Energy controller is distributed under the [GNU General Public License v3.0](http://en.wikipedia.org/wiki/GNU_General_Public_License).

## References
When using the Basic DTU Wind Energy controller, please refer to the following publications:

* Hansen, MH & Henriksen, LC 2013, Basic DTU Wind Energy controller. DTU Wind Energy. DTU Wind Energy E, no. 0028 [link](http://orbit.dtu.dk/en/publications/basic-dtu-wind-energy-controller(ff8123f8-55d2-4907-af7f-2fa139987c33).html)
