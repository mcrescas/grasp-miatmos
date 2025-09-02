! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! file contains :

! subroutine iguess_apriori
! subroutine MIN_AP_MAX
#include "../constants_set/mod_globals.inc"
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

! Definition of Vector of a priori estimates
	 
   subroutine iguess_apriori (inversion_run_count, &
                              iu_main_output,      & ! IN
                              RIN,                 &
                              npixels,iguess,      &
                              AP,AP0,APMIN,APMAX   & ! OUT
                             )
	 
      use mod_par_inv, only : KIMAGE,KPARS
      use mod_retr_settings_derived_type
      use mod_c_utils
      use mod_stop_report

      implicit none

! ---------------------------------------------------	  
! IN :
      integer,      intent(in)   :: inversion_run_count ! can be deleted after edges testing
      type(retr_input_settings),      intent(in)  :: RIN
      integer,                        intent(in)  :: iu_main_output
      integer,                        intent(in)  :: npixels      
      real,dimension(KPARS,KIMAGE),   intent(in)  :: iguess
! ---------------------------------------------------
! OUT :
      real,dimension(KPARS,KIMAGE),   intent(out) :: AP,AP0
      real,dimension(KPARS),          intent(out) :: APMIN,APMAX 
! ---------------------------------------------------
! LOCAL :
      integer :: image_input
      integer :: I,KK,ipix,IDIM1,IDIM2,IDIM3
      real    :: iguess_temp
      character (len=GBL_FILE_PATH_LEN)   ::  internal_file_path      
! ---------------------------------------------------	  
      call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)
! min and max values validation

      if(RIN%KL .eq. 1) then
      if( ANY(RIN%APSMIN(1:RIN%KNSING) .le. 0.0) .or.   &
          ANY(RIN%APSMAX(1:RIN%KNSING) .le. 0.0) ) then
          write(tmp_message,'(3a)') 'Minimum or maximum parameter values can not be zero or negative.', &
          NEW_LINE('A'), &
          'Check input settings file.'
          G_ERROR(trim(tmp_message))
      endif
      endif
      
! initial guess
      AP(:,:)   = 0.0
      if (RIN%INPUT) then
        open (newunit=image_input, FILE=trim(internal_file_path)//"input_iguess.dat",status='old')
! can be deleted after testing edges
        if(inversion_run_count .eq. -1) &
        open (newunit=image_input, FILE=trim(internal_file_path)//"input_iguess_1.dat",status='old')
! can be deleted after testing edges
        if(inversion_run_count .eq. -2) &
        open (newunit=image_input, FILE=trim(internal_file_path)//"input_iguess_2.dat",status='old')
          do I=1,RIN%KNSING
            read(image_input,*) KK,(AP(I,ipix),ipix=1,npixels)
            write(*,*) KK,(AP(I,ipix),ipix=1,npixels)
          enddo
        close (image_input)
      else
        do ipix=1,npixels
          AP(1:RIN%KNSING,ipix) = RIN%APSING(1:RIN%KNSING)
        enddo ! ipix
      endif ! (INPUT)   

! If there is an additional a priori information it is used as an initial guess
      do ipix=1,npixels
        where(iguess(1:RIN%KNSING,ipix) .ne. -999.0) & 
        AP(1:RIN%KNSING,ipix) = iguess(1:RIN%KNSING,ipix)
      enddo ! ipix
! -------------------------------------------------------------------
! initial guess validation
      do ipix=1,npixels
      do i=1,RIN%KNSING
        if(AP(i,ipix) .le. 0.0) then
          write(tmp_message,'(2(a,i3,3x),a,es12.4,3x,a)') 'ipix=',ipix,'i=',i, &
          'AP =',AP(i,ipix),'initial guess can not be zero or negative.'
          G_ERROR(trim(tmp_message))
        endif
      enddo ! i
      enddo ! ipix
      do ipix=1,npixels
        do i=1,RIN%KNSING
          if(AP(i,ipix) .lt. RIN%APSMIN(i) .or. AP(i,ipix) .gt. RIN%APSMAX(i) ) then
            if(RIN%IPRI_verbose) then 
              write(iu_main_output,'(2(a,i0,3x),3(a,e12.5,3x),a)')   &
              'WARNING:  ipix = ',ipix,'i = ',i,             &
              'MIN =',RIN%APSMIN(i),'AP =',AP(i,ipix),'MAX =',RIN%APSMAX(i),  &
              '- initial guess and given range MIN < AP < MAX, in iguess_apriori'
              !stop 'stop in iguess_apriori'
            endif ! RIN%IPRI2
          endif
        enddo ! i
      enddo ! ipix
! -------------------------------------------------------------------
      APMIN(:) = 0.0
      APMAX(:) = 0.0
      select case(RIN%KL)
      case(0)
        APMIN(1:RIN%KNSING) = RIN%APSMIN(1:RIN%KNSING)
        APMAX(1:RIN%KNSING) = RIN%APSMAX(1:RIN%KNSING)
      case(1)
        APMIN(1:RIN%KNSING) = LOG(RIN%APSMIN(1:RIN%KNSING))
        APMAX(1:RIN%KNSING) = LOG(RIN%APSMAX(1:RIN%KNSING))
        do ipix=1,npixels
        AP(1:RIN%KNSING,ipix) = LOG(AP(1:RIN%KNSING,ipix))
        enddo ! ipix
      end select

! If initial guess is out of the given in settings range of characteristics 
! (min <= AP <= max), change initial guess
      do ipix=1,npixels
        call MIN_AP_MAX(1,RIN%KNSING,APMIN,APMAX,AP(:,ipix))
      enddo 

! a priori information
      AP0(:,:)  = 0.0
      do ipix=1,npixels
        do IDIM1=1,RIN%NDIM%n1
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
              if(RIN%SPCA%GSM(IDIM3,IDIM2,IDIM1) .gt. 0.0) then
                AP0(RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1,ipix) =   &
                AP (RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1,ipix)        
              endif
            enddo ! IDIM3
          enddo ! IDIM2  
        enddo ! IDIM1
      enddo ! ipix
      
!      write(iu_main_output,*) 'in iguess_apriori, AP,AP0:'
!      do I=1,RIN%KNSING
!        write(iu_main_output,'(i5,1000e14.5)')     &
!             I,(AP(I,ipix),AP0(I,ipix),ipix=1,npixels)
!      enddo 
      
   return
   end subroutine iguess_apriori 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine MIN_AP_MAX( from_par,to_par,APMIN,APMAX,AP )

      use mod_par_inv, only : KPARS

      implicit none
! ---------------------------------------------------	  
! IN 
      integer,              intent(in)    :: from_par,to_par
      real,dimension(KPARS),intent(in)    :: APMIN,APMAX  
      real,dimension(KPARS),intent(inout) :: AP  
! ---------------------------------------------------	  
      where(AP(from_par:to_par) .lt. APMIN(from_par:to_par))  & 
      AP(from_par:to_par) = APMIN(from_par:to_par)
      where(AP(from_par:to_par) .gt. APMAX(from_par:to_par))  & 
      AP(from_par:to_par) = APMAX(from_par:to_par)
      
      return
      end subroutine MIN_AP_MAX

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss 

