
include ../config.mk

.PHONY: all

all: dtu_we_controller

.PHONY: clean

clean:
	rm -f dtu_we_controller.so *.mod *.o

.PHONY: dtu_we_controller

dtu_we_controller: dtu_we_controller.so

dtu_we_controller.o: dtu_we_controller_fcns.o turbine_controller.o safety_system.o write_version.o

dtu_we_controller_fcns.o: misc_mod.o

dtu_we_controller.so: dtu_we_controller.o dtu_we_controller_fcns.o turbine_controller.o safety_system.o write_version.o misc_mod.o
	$(FC) $(LFLAGS) $^ -o $@


