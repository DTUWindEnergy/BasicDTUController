module dtu_we_controller_fcns
   !
   ! Module with general function that are specific of the DTU Wind Energy Controller. Types are
   ! also defined in this module.
   !
   use misc_mod
   implicit none
   ! Constants
   integer maxwplines
   parameter(maxwplines=100)
   logical :: newtimestep=.TRUE.
   ! Types
   type Tpidvar
      real(mk) Kpro,Kdif,Kint,outmin,outmax,velmax,error1,outset1,outres1
      integer :: stepno1 = 0
      real(mk) outset,outpro,outdif,error1_old,outset1_old,outres1_old,outres
   end type Tpidvar
   type Tpid2var
      real(mk) Kpro(2),Kdif(2),Kint(2),outmin,outmax,velmax,error1(2),outset1,outres1
      integer :: stepno1 = 0
      real(mk) outset,outpro,outdif,error1_old(2),outset1_old,outres1_old,outres
   end type Tpid2var
   type Twpdata
      real(mk) wpdata(maxwplines,2)
      integer lines
   end type Twpdata
   type Tdamper
      type(Tbandpassfilt) bandpass
      type(Tnotch2order) notch
      type(Ttdelay) delay
      real(mk) gain, Td
   end type
   type Texclzone
      type(Tnotch2order) notch
      real(mk) Lwr, Lwr_Tg, Hwr, Hwr_Tg, time_excl_delay
   end type
   type Tcutin
      real(mk) time, delay
   end type
   type Tcutout
      integer stoptype
      real(mk) time, pitchdelay, pitchdelay2, torquedelay, pitchvelmax, pitchvelmax2
   end type
   type Tswitch
      real(mk) pitang_lower, pitang_upper, rel_sp_open_Qg
   end type
   type TSafetySystem
      real(mk) overspeed, RysteVagtLevel
      type(Tlowpass2order) omega2ordervar
      type(Tfirstordervar) rystevagtfirstordervar
   end type
   type TPitchGSvar
      real(mk) invkk1, invkk2
      real(mk) kp_speed, invkk1_speed, invkk2_speed
   end type
   ! Custom Types
   type(Twpdata), save   :: OPdatavar
   type(Texclzone), save :: ExcluZone
!**************************************************************************************************
contains
!**************************************************************************************************
function switch_spline(x, x0, x1)
   ! A function that goes from 0 at x0 to 1 at x1
   real(mk) switch_spline, x, x0, x1
   if (x0 .ge. x1) then
      if (x .lt. x0) then
         switch_spline = 0.0_mk
      else
         switch_spline = 1.0_mk
      endif
   else
      if (x .lt. x0) then
         switch_spline = 0.0_mk
      elseif (x .gt. x1) then
         switch_spline = 1.0_mk
      else
         switch_spline = (x - x0)/(x1 - x0)
      endif
   endif
   return
end function switch_spline
!**************************************************************************************************
function interpolate(x, x0, x1, f0, f1)
   ! Linear interpolation of x through the points (x0, f0) and (x1, f1)
   real(mk) interpolate, x, x0, x1, f0, f1
   if (x0 .eq. x1) then
      interpolate = f0
   else
      interpolate = (x - x1)/(x0 - x1)*f0 + (x - x0)/(x1 - x0)*f1
   endif
   return
end function interpolate
!**************************************************************************************************
function GetOptiPitch(wsp)
   ! Computes pitch angle from look-up table based on wind speed input
   real(mk) GetOptiPitch,wsp
   ! local vars
   real(mk) x, x0, x1, f0, f1, pitch
   integer i
   i=1
   do while((OPdatavar%wpdata(i, 1) .le. wsp) .and. (i .le. OPdatavar%lines))
      i=i+1
   enddo
   if (i.eq.1) then
      GetOptiPitch = OPdatavar%wpdata(1, 2)
   elseif (i .gt. OPdatavar%lines) then
      GetOptiPitch = OPdatavar%wpdata(OPdatavar%lines, 2)
   else
      x = wsp
      x0 = OPdatavar%wpdata(i-1, 1)
      x1 = OPdatavar%wpdata(i, 1)
      f0 = OPdatavar%wpdata(i-1, 2)
      f1 = OPdatavar%wpdata(i, 2)
      Pitch = interpolate(x, x0, x1, f0, f1)
      GetOptiPitch = Pitch
   endif
   return
end function GetOptiPitch
!**************************************************************************************************
function PID(stepno, dt, kgain, PIDvar, error)
   ! PID controller with one input.
   integer stepno
   real(mk) PID, dt, kgain(3), error
   type(Tpidvar) PIDvar
   ! Local vars
   real(mk) eps
   parameter(eps = 0.000001_mk)
   ! Initiate
   if (stepno.eq.1) then
      PIDvar%outset1 = 0.0_mk
      PIDvar%outres1 = 0.0_mk
      PIDvar%error1 = 0.0_mk
      PIDvar%error1_old = 0.0_mk
      PIDvar%outset1_old = 0.0_mk
      PIDvar%outres1_old = 0.0_mk
   endif
   ! Save previous values
   if (stepno.gt.PIDvar%stepno1) then
      PIDvar%outset1_old = PIDvar%outset1
      PIDvar%outres1_old = PIDvar%outres1
      PIDvar%error1_old = PIDvar%error1
   endif
   ! Update the integral term
   PIDvar%outset = PIDvar%outset1_old + 0.5_mk*(error + PIDvar%error1_old)*Kgain(2)*PIDvar%Kint*dt
   ! Update proportional term
   PIDvar%outpro = Kgain(1)*PIDvar%Kpro*0.5_mk*(error + PIDvar%error1_old)
   ! Update differential term
   PIDvar%outdif = Kgain(3)*PIDvar%Kdif*(error - PIDvar%error1_old)/dt
   ! Sum to up
   PIDvar%outres = PIDvar%outset+PIDvar%outpro + PIDvar%outdif
   ! Satisfy hard limits
   if (PIDvar%outres .lt. PIDvar%outmin) then
      PIDvar%outres = PIDvar%outmin
   elseif (PIDvar%outres .gt. PIDvar%outmax) then
      PIDvar%outres = PIDvar%outmax
   endif
   ! Satisfy max velocity
   if (PIDvar%velmax .gt. eps) then
      if ((abs(PIDvar%outres-PIDvar%outres1_old)/dt) .gt. PIDvar%velmax) then
         PIDvar%outres = PIDvar%outres1_old + dsign(PIDvar%velmax*dt, PIDvar%outres-PIDvar%outres1_old)
      endif
   endif
   ! Anti-windup on integral term and save results
   PIDvar%outset1 = PIDvar%outres - PIDvar%outpro - PIDvar%outdif
   PIDvar%outres1 = PIDvar%outres
   PIDvar%error1 = error
   PIDvar%stepno1 = stepno
   ! Set output
   if (stepno .eq. 0) then
      PID = 0.0_mk
   else
      PID = PIDvar%outres
   endif
   return
end function PID
!**************************************************************************************************
function PID2(stepno,dt,kgain,PIDvar,error,added_term)
   ! PID controller with two inputs. Used for the pitch angle with feebacks from generator speed
   ! and power errors.
   integer stepno
   real(mk) PID2, dt, kgain(3, 2), error(2), added_term
   type(Tpid2var) PIDvar
   ! Local vars
   real(mk) eps
   parameter(eps=0.000001_mk)
   ! Initiate
   if (stepno .eq. 1) then
      PIDvar%outset1 = 0.0_mk
      PIDvar%outres1 = 0.0_mk
      PIDvar%error1 = 0.0_mk
      PIDvar%error1_old = 0.0_mk
      PIDvar%outset1_old = 0.0_mk
      PIDvar%outres1_old = 0.0_mk
   endif
   ! Save previous values
   if (stepno .gt. PIDvar%stepno1) then
      PIDvar%outset1_old = PIDvar%outset1
      PIDvar%outres1_old = PIDvar%outres1
      PIDvar%error1_old = PIDvar%error1
   endif
   ! Update the integral term
   PIDvar%outset = PIDvar%outset1_old + 0.5_mk*dt*(Kgain(2, 1)*PIDvar%Kint(1)*(error(1) + PIDvar%error1_old(1))&
                                                  +Kgain(2, 2)*PIDvar%Kint(2)*(error(2) + PIDvar%error1_old(2)))
   ! Update proportional term
   PIDvar%outpro = 0.5_mk*(Kgain(1, 1)*PIDvar%Kpro(1)*(error(1) + PIDvar%error1_old(1))&
                          +Kgain(1, 2)*PIDvar%Kpro(2)*(error(2) + PIDvar%error1_old(2)))
   ! Update differential term
   PIDvar%outdif = (Kgain(3, 1)*PIDvar%Kdif(1)*(error(1) - PIDvar%error1_old(1)))/dt + added_term*dt
   ! Sum up
   PIDvar%outres = PIDvar%outset + PIDvar%outpro + PIDvar%outdif
   ! Satisfy hard limits
   if (PIDvar%outres .lt. PIDvar%outmin) then
      PIDvar%outres = PIDvar%outmin
   elseif (PIDvar%outres .gt. PIDvar%outmax) then
      PIDvar%outres = PIDvar%outmax
   endif
   ! Write out values if the output is not-a-number
   ! NOTE: ebra: isnan is not standard
   if (isnan(PIDvar%outres)) then
      write(*, *)  'NaN issue. Stop in DTU controller'
      write(*, *)  'PIDvar%outres=', PIDvar%outres
      write(*, *)  'PIDvar%outpro=', PIDvar%outpro
      write(*, *)  'PIDvar%outdif=', PIDvar%outdif
      write(*, *)  'dt=', dt
      write(*, *)  'PIDvar%error1_old(1)=', PIDvar%error1_old(1)
      write(*, *)  'error(1)=', error(1)
      write(*, *)  'PIDvar%Kdif(1)=', PIDvar%Kdif(1)
      write(*, *)  'Padded_term=', added_term
      write(*, *)  'PIDvar%outset=', PIDvar%outset
      stop
   endif
   ! Satisfy max velocity
   if (PIDvar%velmax .gt. eps) then
      if ((abs(PIDvar%outres - PIDvar%outres1_old)/dt) .gt. PIDvar%velmax) then
         PIDvar%outres = PIDvar%outres1_old + dsign(PIDvar%velmax*dt, PIDvar%outres-PIDvar%outres1_old)
      endif
   endif
   ! Anti-windup on integral term and save results
   PIDvar%outset1 = PIDvar%outres - PIDvar%outpro - PIDvar%outdif
   PIDvar%outres1 = PIDvar%outres
   PIDvar%error1 = error
   PIDvar%stepno1 = stepno
   ! Set output
   if (stepno .eq. 0) then
      PID2 = 0.0_mk
   else
      PID2 = PIDvar%outres
   endif
   return
end function PID2
!**************************************************************************************************
subroutine damper(stepno, dt, x, filters, y, x_filt)
   ! General damper based on bandpass filter, notch filter, and a time delay.
   type(Tdamper), intent(in) :: filters
   integer stepno
   real(mk), intent(in) :: x
   real(mk), intent(out) :: y, x_filt
   real(mk) dt
   x_filt = bandpassfilt(dt, stepno, filters%bandpass, x)
   x_filt = notch2orderfilt(dt, stepno, filters%notch, x_filt)
   x_filt = timedelay(dt, stepno, filters%delay, filters%Td, x_filt)
   y = filters%gain*x_filt
end subroutine damper
!**************************************************************************************************
end module dtu_we_controller_fcns
