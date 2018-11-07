FFLAGS=-fPIC -c -O3
LFLAGS=-shared -fPIC

%.o: %.f90
	$(FC) $(FFLAGS) -I../dtu_we_controller $< -o $@

%.so: %.o
	$(FC) $(LFLAGS) -I../dtu_we_controller $^ -o $@

