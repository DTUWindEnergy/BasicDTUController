
include ../config.mk

.PHONY: all

all: flap_controller_individual_aep_u_f

.PHONY: clean

clean:
	rm -f flap_controller_individual_aep_u_f.so *.mod *.o

.PHONY: flap_controller_individual_aep_u_f

flap_controller_individual_aep_u_f: flap_controller_individual_aep_u_f.so

flap_controller_individual_aep_u_f.o: write_version.o

flap_controller_individual_aep_u_f.so: flap_controller_individual_aep_u_f.o write_version.o
	$(FC) $(LFLAGS) $^ -o $@
	


