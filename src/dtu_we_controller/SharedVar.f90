MODULE SharedVar
	implicit none
	
	integer, parameter :: mk = kind(1.0d0)
    integer          :: IndTimePt          ! Index time point along the input file
    real(mk)         :: DeltaT             ! Time spacing of time vector (assumed uniform)
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    integer maxlines
    parameter(maxlines=1000)    
    !
    type pddata
      real(mk) pdata(maxlines,2)
      integer lines
    end type pddata
    !
    type(pddata), save   :: OPdatavar    
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
END MODULE SharedVar