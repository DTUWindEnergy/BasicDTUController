$(OBJ_DIR)/%.$(o): %.for
	@echo "($(CONFIG)):" $< 
	@$(FC) $(DEFS) $(INCS) $(FFLAGS) -c $<  $(LIBS) $(FOUT_OBJ)$(OBJ_DIR)$(SLASH)$*.$(o)

$(OBJ_DIR)/%.$(o): %.F90
	@echo "($(CONFIG)):" $< 
	@$(FC) $(DEFS) $(INCS) $(FFFREE) $(FFLAGS) -c $< $(LIBS) $(FOUT_OBJ)$(OBJ_DIR)$(SLASH)$*.$(o)

$(OBJ_DIR)/%.$(o): %.$(f)
	@echo "($(CONFIG)):" $< 
	@$(FC) $(DEFS) $(INCS) $(FFFREE) $(FFLAGS) -c $< $(LIBS) $(FOUT_OBJ)$(OBJ_DIR)$(SLASH)$*.$(o) 
