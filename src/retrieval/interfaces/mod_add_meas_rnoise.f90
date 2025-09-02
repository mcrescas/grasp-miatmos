! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

#include "../constants_set/mod_globals.inc"
module mod_add_meas_rnoise

      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none

contains

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Add random noise to segment measurements
! INOISE  - the number of different noise sources              
! SGMS(I) - std of noise in i -th source                      
! INN(I)  - EQ.1.THEN error is absolute
!         - EQ.0 THEN error assumed relative
! DNN(I)  - variation of the noise of the I-th source

      subroutine add_meas_rnoise_segment ( RIN, deep_random_switch,  &
                                          MNOISEI, segment_meas    &
                                        )
	  
      use mod_retr_settings_derived_type
      
      implicit none
!	----------------------------------------------------------------------------
! IN :	
      type(retr_input_settings), intent(in) :: RIN
      logical, intent(in) :: deep_random_switch
      integer,dimension(KIP,KWM,KIMAGE), intent(in) :: MNOISEI
!	----------------------------------------------------------------------------
! INOUT :	
      type(segment_data), intent(inout) :: segment_meas
!	----------------------------------------------------------------------------
! LOCAL :
      integer :: ipix
      integer :: npixels
!	----------------------------------------------------------------------------
      npixels = segment_meas%npixels

! Disturb measurements with random noise
      do ipix=1,npixels
        call add_meas_rnoise_pixel ( deep_random_switch, &
                                    RIN, MNOISEI(:,:,ipix), &
                                    segment_meas%pixels(ipix) &
                                  )
      enddo ! ipix

      return
      end subroutine add_meas_rnoise_segment

!	ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Add random noise to measurements (simulated measurements)
      subroutine add_meas_rnoise_pixel ( deep_random_switch, &
                                       RIN, MNOISEI,       &
                                       pixel_meas          &
                                      )

      use mod_par_inv, only : KIP, KNBVM, KWM
      use mod_retr_settings_derived_type
      use mod_sdata, only : set_pixel_meas, rnoise

      implicit none
! -----------------------------------------------------------------
! IN :
      logical, intent(in) :: deep_random_switch
      type(retr_input_settings), intent(in) :: RIN

      integer,dimension(KIP,KWM),intent(in) :: MNOISEI

! -----------------------------------------------------------------
! INOUT :	  	  
      type(pixel), intent(inout) :: pixel_meas
! -----------------------------------------------------------------
! LOCAL :
      integer :: iw, ip, iv, iMN
      integer :: nwl, nip, meas_type, nvalid_meas
      integer :: ind
      real,dimension(KNBVM,KIP,KWM)  ::  meas
! ----------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------
! Modeling RANDOM NOISE                      
! INOISE  - the number of different noise sources           
! SGMS(I) - std of noise in i -th source                    

      nwl = pixel_meas%nwl
LOOP_WL : do iw=1,nwl
      NIP = pixel_meas%meas(iw)%NIP
LOOP_meas_type : do ip=1,nip
        meas_type = pixel_meas%meas(iw)%meas_type(ip)
        nvalid_meas = pixel_meas%meas(iw)%NBVM(ip)
        ind = 2 ! pixel => meas
        call set_pixel_meas (                          & ! IN
                           iw,                        &
                           nvalid_meas,               &
                           meas_type,                 &
                           ind,                       &
                           meas(1:nvalid_meas,ip,iw), & ! INOUT
                           pixel_meas                 &
                            )
         iMN = MNOISEI(IP,iw)
         !if(iMN .lt. 1 .or. iMN .gt. RIN%NOISE%INOISE) then
            !write(tmp_message,'(2(a,i0),2a)') 'iMN = ',iMN,'  INOISE = ',RIN%NOISE%INOISE, &
            !NEW_LINE('A'), &
            !'Noise index is not in valid range 1<iMN<INOISE'
            !G_ERROR(trim(tmp_message))
         !endif !

         if(RIN%NOISE%SGMS(iMN) .gt. 0.0) then
           do iv=1,nvalid_meas
               call rnoise (deep_random_switch, &
               RIN%NOISE%SGMS(iMN),RIN%NOISE%INN(iMN), &
               meas(iv,ip,iw))
           enddo ! iv
         endif ! RIN%NOISE%SGMS(iMN) .gt. 0.0
         ind = 1 ! meas => pixel
         call set_pixel_meas (                         & ! IN
                           iw,                        &
                           nvalid_meas,               &
                           meas_type,                 &
                           ind,                       &
                           meas(1:nvalid_meas,ip,iw), & ! INOUT
                           pixel_meas                 &
                            )
enddo LOOP_meas_type
enddo LOOP_WL

      return
      end subroutine add_meas_rnoise_pixel

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

end module mod_add_meas_rnoise
