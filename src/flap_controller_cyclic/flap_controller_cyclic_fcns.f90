module flap_controller_cyclic_fcns_mod
!
! Types and functions used in cyclic flap control dll
! Copied from cyclic_pitch_controller_fcns
use misc_mod
! Types
type Tpidvar
    ! . From Input
  real*8 Kpro,Kdif,Kint,outmin,outmax,error1,outset1,outres1
  real*8  :: velmax=0.d0
    ! . Internal variables
  integer*4 stepno1
  real*8 outset,outpro,outdif,error1_old,outset1_old,outres1_old,outres
end type Tpidvar
! Constant matrices
integer*4 rev_blade_no(3)
data rev_blade_no /1,3,2/               ! Bl.1: points to top, bl.2 is at 4/3 pi, bl.3 at 2/3 pi - swap the channels so to have 1,2,3 from azimuth 
real*8 B0mat(3,3),Bcmat(3,3),Bsmat(3,3)
real*8 B0inv(3,3),Bcinv(3,3),Bsinv(3,3)
! Parameters and variables:
type(Tlowpass2order) LP2_cos_var        ! 2nd order LP filter variables
type(Tlowpass2order) LP2_sin_var
type(Tpidvar) PID_cos_var               ! PID variables
type(Tpidvar) PID_sin_var
real*8    :: psi_ref  = 0.d0            ! Lead angle for cos-sin coupling  [deg]
real*8    :: thr_ratcyclic = 0.99d0     ! Threshold value above which full gain on cyclic control is applied [-]
real*8    :: ctrl_tstart = 0.d0         ! At what time to start with cyclic action
real*8    :: fldefl_shutd = 0.d0        ! Flap deflection angle in shut dwn status [deg]
real*8    :: fldefl_strup = 0.d0        ! Flap deflection angle in start up status [deg]
real*8    :: fllim_mbc = 0.d0           ! Deflection limits in MBC coordinates
    ! Gain scheduling:
real*8     theta_gsref,theta_gsmin,theta_gsmax,psi_lin,psi_quad,kp_lin,ki_lin,kd_lin
! Simulations variables
integer*4:: stepno=0
real*8::    time_old=0.d0
real*8::    deltat=0.02d0
contains
!**************************************************************************************************
subroutine setup_mbc_matrices()
implicit none
! Initialize the transformation matrices
B0mat=0.d0
B0mat(:,1)=1.d0
Bcmat=0.d0
Bcmat(1,2)=1.d0
Bcmat(2,2)=-0.5d0
Bcmat(2,3)= sqrt(3.d0)/2.d0
Bcmat(3,2)=-0.5d0
Bcmat(3,3)=-sqrt(3.d0)/2.d0
Bsmat=0.d0
Bsmat(1,3)=1.d0
Bsmat(2,2)=-sqrt(3.d0)/2.d0
Bsmat(2,3)=-0.5d0
Bsmat(3,2)= sqrt(3.d0)/2.d0
Bsmat(3,3)=-0.5d0
B0inv=0.d0
B0inv(1,:)=1.d0/3.d0
Bcinv=0.d0
Bcinv(2,1)= 2.d0/3.d0
Bcinv(2,2)=-1.d0/3.d0
Bcinv(2,3)=-1.d0/3.d0
Bcinv(3,2)= sqrt(3.d0)/3.d0
Bcinv(3,3)=-sqrt(3.d0)/3.d0
Bsinv=0.d0
Bsinv(2,2)=-sqrt(3.d0)/3.d0
Bsinv(2,3)= sqrt(3.d0)/3.d0
Bsinv(3,1)= 2.d0/3.d0
Bsinv(3,2)=-1.d0/3.d0
Bsinv(3,3)=-1.d0/3.d0
return
end subroutine setup_mbc_matrices
!**************************************************************************************************
function Bmat(AzimuthAngle)
! Transforms from fix, non rotating coordinate system to rotating multiple blade one.
implicit none
real*8 Bmat(3,3)
real*8 AzimuthAngle
Bmat=B0mat+Bcmat*dcos(AzimuthAngle)+Bsmat*dsin(AzimuthAngle)
return
end function Bmat
!**************************************************************************************************
function InvBmat(AzimuthAngle)
! Transform from rotating coordinates to "quadrature" (non-rotating) axis, i.e. MBC transform
implicit none
real*8 InvBmat(3,3)
real*8 AzimuthAngle
InvBmat=B0inv+Bcinv*dcos(AzimuthAngle)+Bsinv*dsin(AzimuthAngle)
return
end function InvBmat
!**************************************************************************************************
function Blead(psi0)
! Accounts for coupling between cos and sin components given by lead angle psi0
implicit none
real*8 Blead(3,3)
real*8 psi0
Blead=0.d0
Blead(1,1) = 1.d0
Blead(2,2) =  dcos(psi0)
Blead(2,3) =  dsin(psi0)
Blead(3,2) = -Blead(2,3)
Blead(3,3) =  Blead(2,2)
return
end function Blead
!**************************************************************************************************
function PID(stepno,dt,kgain,PIDvar,error)
implicit none
integer*4 stepno              
real*8 PID,dt,kgain(3),error
type(Tpidvar) PIDvar
real*8 eps
parameter(eps=1.d-6)
! Initiate
if (stepno.eq.1) then
  PIDvar.outset1=0
  PIDvar.outres1=0
  PIDvar.error1=0
  PIDvar.error1_old=0.0
  PIDvar.outset1_old=0.0
  PIDvar.outres1_old=0.0
endif
! Save previous values
if (stepno.gt.PIDvar.stepno1) then
  PIDvar.outset1_old=PIDvar.outset1
  PIDvar.outres1_old=PIDvar.outres1
  PIDvar.error1_old=PIDvar.error1
endif
! Update the integral term
PIDvar.outset=PIDvar.outset1_old+0.5d0*(error+PIDvar.error1)*kgain(2)*PIDvar.Kint*dt
! Update proportional term
PIDvar.outpro=kgain(1)*PIDvar.Kpro*0.5d0*(error+PIDvar.error1)
! Update differential term
PIDvar.outdif=kgain(3)*PIDvar.Kdif*(error-PIDvar.error1_old)/dt
! Sum to up
PIDvar.outres=PIDvar.outset+PIDvar.outpro+PIDvar.outdif
! Satisfy hard limits
if (PIDvar.outres.lt.PIDvar.outmin) then 
  PIDvar.outres=PIDvar.outmin
elseif (PIDvar.outres.gt.PIDvar.outmax) then 
  PIDvar.outres=PIDvar.outmax
endif
! Satisfy max velocity
if (PIDvar.velmax.gt.eps) then
    if ((abs(PIDvar.outres-PIDvar.outres1_old)/dt).gt.PIDvar.velmax) &
      PIDvar.outres=PIDvar.outres1_old+dsign(PIDvar.velmax*dt,PIDvar.outres-PIDvar.outres1_old)
endif
! Anti-windup on integral term and save results
PIDvar.outset1 = PIDvar.outres-PIDvar.outpro-PIDvar.outdif  ! . makes the integral term react faster
PIDvar.outres1 = PIDvar.outres
PIDvar.error1  = error
PIDvar.stepno1 = stepno
! Set output
if (stepno.eq.0) then 
  PID=0
else 
  PID=PIDvar.outres
endif
return
end function PID
!**************************************************************************************************
end module flap_controller_cyclic_fcns_mod
