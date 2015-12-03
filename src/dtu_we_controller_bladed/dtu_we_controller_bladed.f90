!**************************************************************************************************
subroutine DISCON (avrSWAP, aviFAIL, accINFILE, avcOUTNAME, avcMSG)
   !DEC$ IF .NOT. DEFINED(__LINUX__)
   !DEC$ ATTRIBUTES DLLEXPORT, ALIAS:'DISCON' :: DISCON
   !DEC$ END IF
   use dtu_we_controller_mod
   implicit none
   ! Passed Variables:
   real(4),    intent(inout) :: avrSWAP   (*)     ! The swap array, used to pass data to, and receive data from, the DLL controller.
   integer(4), intent(out)   :: aviFAIL           ! A flag used to indicate the success of this DLL call set as follows: 0 if the DLL call was successful, >0 if the DLL call was successful but cMessage should be issued as a warning messsage, <0 if the DLL call was unsuccessful or for any other reason the simulation is to be stopped at this point with cMessage as the error message.
   integer(1), intent(in)    :: accINFILE (*)     ! The address of the first record of an array of 1-byte CHARACTERs giving the name of the parameter input file, 'basic_dtu_we_controller.IN'.
   integer(1), intent(out)   :: avcMSG    (*)     ! The address of the first record of an array of 1-byte CHARACTERS giving the message contained in cMessage, which will be displayed by the calling program if aviFAIL <> 0.
   integer(1), intent(in)    :: avcOUTNAME(*)     ! The address of the first record of an array of 1-byte CHARACTERS giving the simulation run name without extension.
   real(8) array1(100), array2(100)
   integer(4) :: i, iostat, callno = 0
   
   if (callno .eq. 0) then
      open(88, file='.\control\controller_input.dat')
      do i = 1, 76
         read(88, *, iostat=iostat) array1(i)
         if (iostat .ne. 0) then
            write(6,*) ' *** ERROR *** Could not read line ', i, &
                       ' in controller input file'
            stop
         endif
      enddo
      call init_regulation_advanced(array1, array2)
      callno = 1
      array1 = 0.0_mk
   endif

      array1( 1) = dble(nint(avrSWAP( 2)*1000.0_mk)/1000.0_mk)   !    1: general time
      array1( 2) = dble(avrSWAP(20))                         !    2: constraint bearing1 shaft_rot 1 only 2 
      array1( 3) = dble(avrSWAP( 4))                         !    3: constraint bearing2 pitch1 1 only 1
      array1( 4) = dble(avrSWAP(33))                         !    4: constraint bearing2 pitch2 1 only 1
      array1( 5) = dble(avrSWAP(34))                         !    5: constraint bearing2 pitch3 1 only 1
      array1( 6) = 0.0_mk                                      !  6-8: wind free_wind 1 0.0 0.0 hub height
      array1( 7) = dble(avrSWAP(27))
      array1( 8) = 0.0_mk
      array1( 9) = dble(avrSWAP(15))                         !    9: elec. power
      array1(10) = int(0)                                    !   10: grid flag  ; [1=no grid,0=grid]
      array1(11) = dble(avrSWAP(53))                         !   11: Tower top x-acceleration
      array1(12) = dble(avrSWAP(54))                         !   12: Tower top y-acceleration
      
      call update_regulation(array1, array2)
      
      avrSWAP(35) = 1.0e0          ! Generator contactor status: 1=main (high speed) variable-speed generator
      avrSWAP(36) = array2(25)   ! Shaft brake status: 0=off
      avrSWAP(41) = 0.0e0          ! Demanded yaw actuator torque
      avrSWAP(42) = array2( 2)   ! Use the command angles of all blades if using individual pitch
      avrSWAP(43) = array2( 3)   ! "
      avrSWAP(44) = array2( 4)   ! "
      avrSWAP(45) = array2( 2)   ! Use the command angle of blade 1 if using collective pitch
      avrSWAP(46) = 0.0e0          ! Demanded pitch rate (Collective pitch)
      avrSWAP(47) = array2( 1)   ! Demanded generator torque
      avrSWAP(48) = 0.0e0          ! Demanded nacelle yaw rate
      avrSWAP(55) = 0.0e0          ! Pitch override: 0=yes
      avrSWAP(56) = 0.0e0          ! Torque override: 0=yes
      avrSWAP(65) = 0.0e0          ! Number of variables returned for logging
      avrSWAP(72) = 0.0e0          ! Generator start-up resistance
      avrSWAP(79) = 0.0e0          ! Request for loads: 0=none
      avrSWAP(80) = 0.0e0          ! Variable slip current status
      avrSWAP(81) = 0.0e0          ! Variable slip current demand

   return
end subroutine DISCON
