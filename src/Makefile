OBJS = main.o read_decomp.o

mrtb: $(OBJS)
	$(FC) $(FFLAGS) $(CPPFLAGS) $(OBJS) $(LIBS) -o mrtb

clean:
	$(RM) -f mrtb $(OBJS)

%.o: %.F90
	$(FC) -c $(FFLAGS) $(CPPFLAGS) $< -o $@
