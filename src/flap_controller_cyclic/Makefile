
include ../config.mk

.PHONY: all

all: flap_controller_cyclic

.PHONY: clean

clean:
	rm -f flap_controller_cyclic.so *.mod *.o

.PHONY: flap_controller_cyclic

flap_controller_cyclic: flap_controller_cyclic.so

flap_controller_cyclic.o: flap_controller_cyclic_fcns.o write_version.o

flap_controller_cyclic.so: flap_controller_cyclic.o flap_controller_cyclic.o write_version.o
	$(FC) $(LFLAGS)  -I../dtu_we_controller $^ -o $@


