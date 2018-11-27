
include ../config.mk

.PHONY: all

all: dtu_we_controller_bladed

.PHONY: clean

clean:
	rm -f dtu_we_controller_bladed.so *.mod *.o

.PHONY: dtu_we_controller_bladed

dtu_we_controller_bladed: dtu_we_controller_bladed.so

dtu_we_controller_bladed.so: dtu_we_controller_bladed.o
	$(FC) $(LFLAGS) $^ -o $@


