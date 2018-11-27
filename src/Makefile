
.PHONY: all

all: dtu_we_controller dtu_we_controller_bladed flap_controller_cyclic flap_controller_individual_aep_u_f

.PHONY: dtu_we_controller

dtu_we_controller:
	make -C dtu_we_controller

.PHONY: dtu_we_controller_bladed

dtu_we_controller_bladed:
	make -C dtu_we_controller_bladed

.PHONY: flap_controller_cyclic

flap_controller_cyclic:
	make -C flap_controller_cyclic
	
.PHONY: flap_controller_individual_aep_u_f

flap_controller_individual_aep_u_f:
	make -C flap_controller_individual_aep_u_f
	
.PHONY: clean

clean:
	make -C dtu_we_controller clean
	make -C dtu_we_controller_bladed clean
	make -C flap_controller_cyclic clean
	make -C flap_controller_individual_aep_u_f clean