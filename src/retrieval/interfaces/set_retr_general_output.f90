! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

#include "../constants_set/mod_globals.inc"
!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine output_aerosol_initialization ( GOUT_aerosol )

      use mod_retr_general_output_derived_type
      
      implicit none
! -----------------------------------------------------------------------------------------	  
      type(output_segment_particles), intent(out) :: GOUT_aerosol
      integer   :: ipix,iw	  
! -----------------------------------------------------------------------------------------
      GOUT_aerosol%phmx%nangle = 0
      GOUT_aerosol%phmx%angle(:) = 0.0

      ipix=1
      do while(ipix .le. KIMAGE)
        GOUT_aerosol%opt%pixel(ipix)%Aexp = 0.0
        GOUT_aerosol%pm%pixel(ipix)%PM(:) = 0.0
        GOUT_aerosol%types%pixel(ipix)%index = 0 ! by default complex aerosol micture

        iw=1
        do while(iw .le. KW)
          GOUT_aerosol%opt%pixel(ipix)%wl(iw)%extt   = 0.0
          GOUT_aerosol%opt%pixel(ipix)%wl(iw)%ssat   = 0.0
          GOUT_aerosol%opt%pixel(ipix)%wl(iw)%ext(:) = 0.0
          GOUT_aerosol%opt%pixel(ipix)%wl(iw)%ssa(:) = 0.0

          GOUT_aerosol%opt%pixel(ipix)%wl(iw)%ext_cut_off(:,:) = 0.0
          GOUT_aerosol%opt%pixel(ipix)%wl(iw)%ssa_cut_off(:,:) = 0.0

          GOUT_aerosol%rind%pixel(ipix)%wl(iw)%mreal(:) = 0.0
          GOUT_aerosol%rind%pixel(ipix)%wl(iw)%mimag(:) = 0.0

          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph11(:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph12(:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph22(:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph33(:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph34(:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph44(:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%pht11(:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%pht12(:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%pht22(:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%pht33(:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%pht34(:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%pht44(:) = 0.0

          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph11_cut_off(:,:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph12_cut_off(:,:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph22_cut_off(:,:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph33_cut_off(:,:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph34_cut_off(:,:,:) = 0.0
          GOUT_aerosol%phmx%pixel(ipix)%wl(iw)%ph44_cut_off(:,:,:) = 0.0

          GOUT_aerosol%lidar%pixel(ipix)%wl(iw)%lrt     = 0.0
          GOUT_aerosol%lidar%pixel(ipix)%wl(iw)%ldprt   = 0.0
          GOUT_aerosol%lidar%pixel(ipix)%wl(iw)%lr(:)   = 0.0
          GOUT_aerosol%lidar%pixel(ipix)%wl(iw)%ldpar(:) = 0.0
          GOUT_aerosol%lidar%pixel(ipix)%wl(iw)%ldper(:) = 0.0
        iw=iw+1
        enddo ! while(iw .le. KW) 
        
        GOUT_aerosol%sd2m%mph%pixel(ipix)%reff(:)  = 0.0       
        GOUT_aerosol%sd2m%mph%pixel(ipix)%rm(:)    = 0.0
        GOUT_aerosol%sd2m%mph%pixel(ipix)%std(:)   = 0.0
        GOUT_aerosol%sd2m%mph%pixel(ipix)%cv(:)    = 0.0
        iw=1
        do while(iw .le. KW)
        GOUT_aerosol%sd2m%opt%pixel(ipix)%wl(iw)%ext(:) = 0.0
        iw=iw+1
        enddo ! while(iw .le. KW) 

        GOUT_aerosol%chem%pixel(ipix)%fwtr(:)   = 0.0
        GOUT_aerosol%chem%pixel(ipix)%fslbl(:)  = 0.0
        GOUT_aerosol%chem%pixel(ipix)%vfract(:,:) = 0.0
      ipix=ipix+1
      enddo ! while(ipix .le. KIMAGE)
	   	  
      return
      end subroutine output_aerosol_initialization

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


      subroutine output_gases_initialization ( GOUT_gases )

      use mod_retr_general_output_derived_type

      implicit none
! -----------------------------------------------------------------------------------------	  
      type(output_segment_gases), intent(out) :: GOUT_gases
      integer   :: ipix,iw	  
! -----------------------------------------------------------------------------------------
      ipix=1  
      do while(ipix .le. KIMAGE)
        iw=1
        do while(iw .le. KW)
          GOUT_gases%pixel(ipix)%wl(iw)%abs(:) = 0.0
        iw=iw+1
        enddo ! while(iw .le. KW) 
      ipix=ipix+1
      enddo ! while(ipix .le. KIMAGE)
	   	  
      return
      end subroutine output_gases_initialization

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


      subroutine output_surface_initialization ( GOUT_surf )

      use mod_retr_general_output_derived_type

      implicit none
! -----------------------------------------------------------------------------------------	  
      type(output_segment_surface), intent(out) :: GOUT_surf
      integer   :: ipix,iw	  
! -----------------------------------------------------------------------------------------
      ipix=1  
      do while(ipix .le. KIMAGE)
        iw=1
        do while(iw .le. KW)
          GOUT_surf%pixel(ipix)%wl(iw)%dhr = 0.0
          GOUT_surf%pixel(ipix)%wl(iw)%bhr_iso = 0.0
        iw=iw+1
        enddo ! while(iw .le. KW) 
      ipix=ipix+1
      enddo ! while(ipix .le. KIMAGE)
	   	  
      return
      end subroutine output_surface_initialization

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine output_err_estim_initialization ( GOUT_errest )

      use mod_retr_general_output_derived_type
      
      implicit none
! -----------------------------------------------------------------------------------------	  
      type(output_segment_err_estim), intent(out)   ::  GOUT_errest
      integer   :: ipix, iw
! -----------------------------------------------------------------------------------------
      ipix=1
      do while(ipix .le. KIMAGE)

      GOUT_errest%par%pixel(ipix)%ERRP(:)     = 0.0
      GOUT_errest%par%pixel(ipix)%BIASP(:)    = 0.0
      GOUT_errest%par%pixel(ipix)%TSTDP(:)    = 0.0
      GOUT_errest%par%pixel(ipix)%sd_err(:,:) = 0.0

      iw=1
      do while(iw .le. KW)
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%ERR_ext(:)  = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%BIAS_ext(:) = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%TSTD_ext(:) = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%ERR_ssa(:)  = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%BIAS_ssa(:) = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%TSTD_ssa(:) = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%ERR_aext(:)  = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%BIAS_aext(:) = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%TSTD_aext(:) = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%ERR_extt  = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%BIAS_extt = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%TSTD_extt = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%ERR_ssat  = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%BIAS_ssat = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%TSTD_ssat = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%ERR_aextt  = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%BIAS_aextt = 0.0
      GOUT_errest%aerosol%opt%pixel(ipix)%wl(iw)%TSTD_aextt = 0.0

      GOUT_errest%aerosol%lidar%pixel(ipix)%wl(iw)%ERR_lr(:)  = 0.0
      GOUT_errest%aerosol%lidar%pixel(ipix)%wl(iw)%BIAS_lr(:) = 0.0
      GOUT_errest%aerosol%lidar%pixel(ipix)%wl(iw)%TSTD_lr(:) = 0.0
      GOUT_errest%aerosol%lidar%pixel(ipix)%wl(iw)%ERR_lrt  = 0.0
      GOUT_errest%aerosol%lidar%pixel(ipix)%wl(iw)%BIAS_lrt = 0.0
      GOUT_errest%aerosol%lidar%pixel(ipix)%wl(iw)%TSTD_lrt = 0.0
      iw=iw+1
      enddo ! while(iw .le. KW)

      ipix=ipix+1
      enddo ! while(ipix .le. KIMAGE)
	   	  
      return
      end subroutine output_err_estim_initialization
!    sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! MEH: subroutine for initialization for error of SD_LN (when it is not part of the retrieval)
      subroutine output_err_estim_initialization_mic ( GOUT_errest )

      use mod_retr_general_output_derived_type
      
      implicit none
! -----------------------------------------------------------------------------------------
      type(output_segment_err_estim), intent(out)   ::  GOUT_errest
      integer   :: ipix, irad
! -----------------------------------------------------------------------------------------
      ipix=1
      do while(ipix .le. KIMAGE)

      irad=1
      do while(irad .le. 22) ! quitar el 22 y definir antes el nrad a partir de GOUT%information......
      GOUT_errest%aerosol%mic%pixel(ipix)%ngrid(irad)%ERR_sdt  = 0.0
      GOUT_errest%aerosol%mic%pixel(ipix)%ngrid(irad)%BIAS_sdt = 0.0
      GOUT_errest%aerosol%mic%pixel(ipix)%ngrid(irad)%TSTD_sdt = 0.0
      irad=irad+1
      enddo ! while(irad .le. ngrid(0))

      ipix=ipix+1
      enddo ! while(ipix .le. KIMAGE)
             
      return
      end subroutine output_err_estim_initialization_mic

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine output_forcing_initialization ( GOUT_forcing )

      use mod_retr_general_output_derived_type
      
      implicit none
! -----------------------------------------------------------------------------------------	  
      type(output_segment_rad_forcing), intent(out)   ::  GOUT_forcing
      integer   :: ipix	  
! -----------------------------------------------------------------------------------------

      ipix=1
      do while(ipix .le. KIMAGE)
        GOUT_forcing%bbflux%pixel(ipix)%NHLV  = 0
        GOUT_forcing%forcing%pixel(ipix)%NHLV = 0
        
        GOUT_forcing%bbflux%pixel(ipix)%HLV(:)    = 0.0
        GOUT_forcing%bbflux%pixel(ipix)%BBDFX0(:) = 0.0
        GOUT_forcing%bbflux%pixel(ipix)%BBUFX0(:) = 0.0
        GOUT_forcing%bbflux%pixel(ipix)%BBDFXA(:) = 0.0
        GOUT_forcing%bbflux%pixel(ipix)%BBUFXA(:) = 0.0

        GOUT_forcing%forcing%pixel(ipix)%HLV(:)     = 0.0
        GOUT_forcing%forcing%pixel(ipix)%NETFORC(:) = 0.0
      ipix=ipix+1
      enddo ! while(ipix .le. KIMAGE)
	   	  
      return
      end subroutine output_forcing_initialization

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine output_retrieval_initialization ( GOUT_retrieval )

      use mod_retr_general_output_derived_type
      
      implicit none
! -----------------------------------------------------------------------------------------
	  
      type(output_segment_retrieval), intent(out) :: GOUT_retrieval
      integer :: ipix, iw
! -----------------------------------------------------------------------------------------  
      GOUT_retrieval%res%rest  = 0.0
      GOUT_retrieval%res%resat = 0.0
      GOUT_retrieval%res%resrt = 0.0
        ipix=1
        do while(ipix .le. KIMAGE)
        GOUT_retrieval%res%pixel(ipix)%res     = 0.0
        GOUT_retrieval%res%pixel(ipix)%resa(:) = 0.0
        GOUT_retrieval%res%pixel(ipix)%resr(:) = 0.0
        GOUT_retrieval%par%pixel(ipix)%par(:)  = 0.0
          GOUT_retrieval%par%pixel(ipix)%sd(:,:) = 0.0
          GOUT_retrieval%par%pixel(ipix)%Cv(:,:) = 0.0
          GOUT_retrieval%par%pixel(ipix)%ShD(:,:) = 0.0
          GOUT_retrieval%par%pixel(ipix)%VD(:,:) = 0.0
          GOUT_retrieval%par%pixel(ipix)%BRF(:,:) = 0.0
          GOUT_retrieval%par%pixel(ipix)%BRP(:,:) = 0.0
          GOUT_retrieval%par%pixel(ipix)%BRM(:,:) = 0.0
        ipix=ipix+1
        enddo ! while(ipix .le. KIMAGE)
      GOUT_retrieval%information%npixels = 0
      GOUT_retrieval%information%nwl     = 0
      GOUT_retrieval%information%nsd     = 0
      GOUT_retrieval%information%nbins   = 0
      GOUT_retrieval%information%ngas    = 0
      GOUT_retrieval%information%nnoises = 0
      GOUT_retrieval%information%wl(:)   = 0.0

      GOUT_retrieval%information%ndim%n1             = 0
      GOUT_retrieval%information%ndim%n2(:)          = 0
      GOUT_retrieval%information%ndim%n3(:,:)        = 0
      GOUT_retrieval%information%ndim%ISTARSING(:,:) = 0
      GOUT_retrieval%information%ndim%par_type(:)    = 0
      GOUT_retrieval%information%ndim%par_retr(:)    = .false.

      GOUT_retrieval%information%ngrid(:)    = 0
      GOUT_retrieval%information%radius(:,:) = 0.0

      GOUT_retrieval%information%sd_lb(:,:)  = 0.0

      GOUT_retrieval%information%flag_plus = .false.
      GOUT_retrieval%information%ndim_plus%n1             = 0
      GOUT_retrieval%information%ndim_plus%n2(:)          = 0
      GOUT_retrieval%information%ndim_plus%n3(:,:)        = 0
      GOUT_retrieval%information%ndim_plus%ISTARSING(:,:) = 0
      GOUT_retrieval%information%ndim_plus%par_type(:)    = 0
      GOUT_retrieval%information%ndim_plus%par_retr(:)    = .false.

      GOUT_retrieval%information%nbrf = 0
      GOUT_retrieval%information%nbrp = 0
      GOUT_retrieval%information%nbrm = 0

      return
      end subroutine output_retrieval_initialization

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine output_coordinates_initialization ( GOUT_coord )

      use mod_retr_general_output_derived_type
      
      implicit none
! -----------------------------------------------------------------------------------------
	  
      type(output_segment_coordinates), intent(out) :: GOUT_coord
! -----------------------------------------------------------------------------------------  
      GOUT_coord%pixel(1:KIMAGE)%x_lon = 0.0
      GOUT_coord%pixel(1:KIMAGE)%y_lat = 0.0
      GOUT_coord%pixel(1:KIMAGE)%t_masl = 0

      return
      end subroutine output_coordinates_initialization

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_pixel_retr_output_residual (  INVSING,niter,ipix, & ! IN
                                                   ALS,                &
                                                   INOISE,resa,resr,   &
                                                   GOUT_retrieval_res  & ! INOUT
                                                )
      use mod_retr_general_output_derived_type
      	  
      implicit none

! -----------------------------------------------------------------------------------------
! IN :
      integer,                intent(in)  ::  INVSING,ipix,niter,INOISE
      real,                   intent(in)  ::  ALS
      real,dimension(KKNOISE),intent(in)  ::  resa,resr
! -----------------------------------------------------------------------------------------
! INOUT :
      type(output_segment_residual),intent(inout)  ::  GOUT_retrieval_res
! -----------------------------------------------------------------------------------------
! LOCAL :
      integer  ::  i
! -----------------------------------------------------------------------------------------
      GOUT_retrieval_res%pixel(ipix)%niter = niter
      if(INVSING .gt. 0) &
      GOUT_retrieval_res%niter = niter

      GOUT_retrieval_res%pixel(ipix)%res = SQRT(ALS)
      do i=1,INOISE
         GOUT_retrieval_res%pixel(ipix)%resa(i) = resa(i)  
         GOUT_retrieval_res%pixel(ipix)%resr(i) = resr(i)
      enddo  ! i

      return
      end subroutine set_pixel_retr_output_residual

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_total_retr_output_residual (  niter,                & ! IN
                                                   AGENP,                &
                                                   INOISE,resat,resrt,   &
                                                   GOUT_retrieval_res    & ! INOUT
                                                )
      use mod_retr_general_output_derived_type
      	  
      implicit none

! -----------------------------------------------------------------------------------------
! IN :
      integer,                intent(in)  ::  niter,INOISE
      real,                   intent(in)  ::  AGENP
      real,dimension(KKNOISE),intent(in)  ::  resat,resrt
! -----------------------------------------------------------------------------------------
! INOUT :
      type(output_segment_residual),intent(inout)  ::  GOUT_retrieval_res
! -----------------------------------------------------------------------------------------
! LOCAL :
      integer  ::  i
! -----------------------------------------------------------------------------------------

      !GOUT_retrieval_res%niter = niter
      GOUT_retrieval_res%rest  = SQRT(AGENP)
      do i=1,INOISE
         GOUT_retrieval_res%resat(i) = resat(i)  
         GOUT_retrieval_res%resrt(i) = resrt(i)         
      enddo  ! i

      return
      end subroutine set_total_retr_output_residual

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Retrieved Size distribution for precalculated lognormal bins

      subroutine SD_precalcul_ln_bins (  RIN,GOUT_retrieval_info,npixels,IDIM1,SD,SDout )

      use mod_retr_general_output_derived_type
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------------------------------
! IN :
      integer,                             intent(in)  ::  npixels,IDIM1
      type(retr_input_settings),           intent(in)  ::  RIN
      type(output_info),                   intent(in)  ::  GOUT_retrieval_info
      real,dimension(KIDIM3,KIDIM2,KIMAGE),intent(in)  ::  SD
      real,dimension(NRR,KIDIM2,KIMAGE),   intent(out) ::  SDout
! -----------------------------------------------------------------------------------------
! LOCAL :
      real     ::  temp(KIDIM3), dlnr
      integer  ::  i, IDIM2, IDIM3, NDIM3, ipix, ibins, ibeg, iend
! -----------------------------------------------------------------------------------------
! Array GOUT_retrieval_info%sd_lb(nradii,nbins) contains size distribustions for
! all aerosol components
! ibins - bin index
! ibeg, iend - bin indices for each aerosol component
      dlnr = log(GOUT_retrieval_info%radius(2,1)) - &
             log(GOUT_retrieval_info%radius(1,1))
      SDout(:,:,:) = 0.0
      ibins = 0
      ibeg = 1
      do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
        if(KCpar .lt. NDIM3) then
          write(tmp_message,'(a,3i4,4a)') &
          'ISD,KCpar,NBIN(ISD): ',IDIM2,KCpar,NDIM3, &
          NEW_LINE('A'), &
          'KCpar - parameter defined in mod_DLS_bin.f', &
          NEW_LINE('A'), &
          'KCpar .lt. NBIN(ISD)'
          G_ERROR(trim(tmp_message))
        endif ! KCpar .lt. NDIM3

        do IDIM3=1,NDIM3
          ibins = ibins + 1
          temp(IDIM3) = SUM(GOUT_retrieval_info%sd_lb(1:GOUT_retrieval_info%ngrid(IDIM2),ibins))*dlnr
!write(*,'(2i4,es12.4,a)') IDIM3,ibins,temp(IDIM3),'  - IDIM3, ibins, temp(IDIM3)'
        enddo ! IDIM3
        iend =ibins

        do ipix=1,npixels
!write(*,'(6(a,i0,2x))') 'ipix = ',ipix,'IDIM2 = ',IDIM2,'NDIM3 = ',NDIM3,'ibeg = ',ibeg,'iend = ',iend
          do i=1,GOUT_retrieval_info%ngrid(IDIM2)
            SDout(i,IDIM2,ipix) = SUM( GOUT_retrieval_info%sd_lb(i,ibeg:iend) / &
                                       temp(1:NDIM3)*SD(1:NDIM3,IDIM2,ipix) )
!write(*,'(2i4,15es12.4)') ipix,i,GOUT_retrieval_info%sd_lb(i,ibeg:iend),SD(1:NDIM3,IDIM2,ipix),SDout(i,IDIM2,ipix)
          enddo ! i
        enddo ! ipix	
      ibeg = iend+1
      enddo  ! IDIM2

      return
      end subroutine SD_precalcul_ln_bins

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!  Size Distribution calculated for retrieved parameters of lognormal size distribution

      subroutine SD_lognormal (  RIN,GOUT_retrieval_info,ipix,IDIM1,SD,CV,SDout )

      use mod_retr_general_output_derived_type
      use mod_par_DLS, only : KNpar
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------------------------------
! IN :
      integer,                             intent(in)   ::  ipix,IDIM1
      type(retr_input_settings),           intent(in)   ::  RIN
      type(output_info),                   intent(in)   ::  GOUT_retrieval_info
      real,dimension(KIDIM3,KIDIM2,KIMAGE),intent(in)   ::  SD, CV
      real,dimension(KNpar,KIDIM2,KIMAGE),intent(out)   ::  SDout
! -----------------------------------------------------------------------------------------
! LOCAL :
      integer  ::  ISD, i, ngrid
      real  ::  RSD
      real,dimension(KNpar)  ::  radius_tmp
! -----------------------------------------------------------------------------------------
      
      SDout(:,:,:) = 0.0

      do ISD=1,RIN%NDIM%n2(IDIM1)    ! particle component loop
        ngrid = GOUT_retrieval_info%ngrid(ISD)
        call SIZEDISDN (  0,                       & ! IPRI
                          -ngrid,1,3,1,1,          & ! KN,IA,ID,NSD,NMD
                          CV(1,ISD,ipix),          &
                          SD(2,ISD,ipix),          &
                          SD(1,ISD,ipix),          &
                          GOUT_retrieval_info%radius(1,ISD),      &
                          GOUT_retrieval_info%radius(ngrid,ISD),  &
                          radius_tmp(1:ngrid),     &
                          SDout(1:ngrid,ISD,ipix), &
                          RSD                      &
                       )
      enddo  ! ISD

      return
      end subroutine SD_lognormal

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! ****************************************************************
! ********************  OUTPUT sd2m   ****************************
! ****************************************************************

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

! *************************************************************************
! **************    Selection*of*the*minimum*value*of*SD (inflection point)  


!      subroutine sd2m_min_index(iu,IPRI,NBINT,RADIUST,SDT,IMIN)

!      use mod_par_DLS, only : KNpar
      
!      implicit none
!!	-------------------------------------------------------------------------------------
!      integer,               intent(in)   :: NBINT,iu
!      logical,               intent(in)   :: IPRI
!      real,dimension(NBINT), intent(in)   :: RADIUST,SDT
!      integer,               intent(out)  :: IMIN
!!	-------------------------------------------------------------------------------------
!      integer  ::  i,ib,ie
!      real     ::  rmin,SDTMIN
!!	-------------------------------------------------------------------------------------

!      if(NBINT .eq. KNpar) then
!        ib = 9
!        ie = 15
!        SDTMIN = 1.0e+30
!        do i=ib,ie
!          if(SDTMIN .gt. SDT(i) .and. SDT(i) .gt. 0.0) then
!            SDTMIN = SDT(i)
!            IMIN  = i
!          endif
!        enddo
!        !if(IPRI) then
!        !write(iu,'(a,i3,2(a,e12.3))') 'in sd2m_min_index:  IMIN=',IMIN,'  SDMIN=',SDTMIN,  &
!        !'  inflection_point=',RADIUST(IMIN)
!        !endif
!      else
!        rmin  = 0.6 ! um
!        do i=1,NBINT
!          if(RADIUST(i) .le. rmin) cycle
!            IMIN = i-1
!          exit
!        enddo ! i
!        !if(IPRI) then
!        !write(iu,'(a,i3,a,e12.3)') 'in sd2m_min_index:  IMIN=',IMIN,  &
!        !'  inflection_point=',RADIUST(IMIN)
!        !endif
!      endif !
      
!      return
!      end subroutine sd2m_min_index

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! *************************************************************************
! **************    Selection*of*the*minimum*value*of*SD (inflection point)  


      subroutine sd2m_min_index_LB(iu,IPRI,NBINT,RADIUST,IMIN)
      
      implicit none
!	-------------------------------------------------------------------------------------
      integer,               intent(in)   :: NBINT,iu
      logical,               intent(in)   :: IPRI
      real,dimension(NBINT), intent(in)   :: RADIUST
      integer,               intent(out)  :: IMIN
!	-------------------------------------------------------------------------------------
      integer  ::  i
      real     ::  rmin
!	-------------------------------------------------------------------------------------

      rmin  = 0.6 ! um
      i = 1
      do while(i .le. NBINT .and. RADIUST(i) .le. rmin)
        IMIN = i
        i = i + 1
      enddo ! i
      !if(IPRI) then
      !write(iu,'(a)') 'RADIUST:'
      !write(iu,'(10f12.4)') RADIUST(1:NBINT)
      !write(iu,'(a,i0,a,f12.4)') 'in sd2m_min_index_LB:  IMIN = ',IMIN,  &
      !'  rmin =',RADIUST(IMIN)
      !endif

      return
      end subroutine sd2m_min_index_LB

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! *************************************************************************
! **************    Selection*of*the*minimum*value*of*SD (inflection point)  

      subroutine sd2m_min_index_TB(iu,IPRI,NBINT,RADIUST,SDT,IMIN)
      
      implicit none
!	-------------------------------------------------------------------------------------
      integer,               intent(in)   :: NBINT, iu
      logical,               intent(in)   :: IPRI
      real,dimension(NBINT), intent(in)   :: RADIUST, SDT
      integer,               intent(out)  :: IMIN
!	-------------------------------------------------------------------------------------
      integer  ::  i, ib, ie, iminloc(1)
      real     ::  rmin, SDTMIN, tiny
!	-------------------------------------------------------------------------------------
      SDTMIN = 1.0e+30
      tiny = 1e-3
      if(NBINT .eq. 22 .and. abs(1.0-RADIUST(1)/0.05) .lt. tiny &
                       .and. abs(1.0-RADIUST(NBINT)/15.0) .lt. tiny) then
          ! Aeronet case for 22 radius triangle bins
          ib = 9
          ie = 12
          SDTMIN = MINVAL(SDT(ib:ie))
          iminloc = MINLOC(SDT(ib:ie))
          IMIN = ib + iminloc(1) - 1
      else
          rmin  = 0.6 ! um
          i = 1
          do while(i .le. NBINT .and. RADIUST(i) .le. rmin)
            IMIN = i
            i = i + 1
          enddo ! i
          ib = IMIN
          ie = IMIN
          SDTMIN = SDT(IMIN)
! do not delete
!          ! Generalized case for triangle bins; hardcoded radii are used.
!          ! 0.439 and 0.992 um correspond to ib=9 and ie=12 in above case
!          ib = 1
!          i = 1
!          do while(i .le. NBINT .and. RADIUST(i) .le. 0.439)
!              ib  = i
!              i = i + 1
!          enddo
!          ie = NBINT
!          i = NBINT
!          do while(i .ge. 1 .and. RADIUST(i) .gt. 0.992)
!              ie  = i - 1
!              i = i - 1
!          enddo
!          SDTMIN = MINVAL(SDT(ib:ie))
!          iminloc = MINLOC(SDT(ib:ie))
!          IMIN = ib + iminloc(1) - 1
      endif !

      !if(IPRI) then
      !write(iu,'(a)') 'RADIUST:'
      !write(iu,'(10f12.4)') RADIUST(1:NBINT)
      !write(iu,'(a)') 'SDT:'
      !write(iu,'(10e12.4)') SDT(1:NBINT)
      !write(iu,'(2(a,i0,2x))') 'ib = ',ib,'ie = ',ie
      !write(iu,'(a,i0,a,e12.4,a,f12.4)') 'in sd2m_min_index_TB:  IMIN = ',IMIN, &
      !'  SDMIN = ',SDTMIN,'  inflection_point=',RADIUST(IMIN)
      !endif

      return
      end subroutine sd2m_min_index_TB

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

!************************************************************************
!*******************   Size*distributions*of*fine*and*coarse*modes   ****

      subroutine sd2m_fine_coarse ( IMIN,par_type,     &
                                    NBIN,RADIUS,SD,    &
                                    NBINL,RADIUSL,SDL, &
                                    SDFC,              &
                                    cv,std,rm,reff     &             
                                  )

      use mod_par_OS,      only : KSD
      use mod_par_inv,     only : KIDIM3
      use mod_par_DLS_bin, only : NRR,KCpar
      use mod_par_type_aerosol
      use mod_sdata_meas_type
      use mod_stop_report
                  
      implicit none
!	-------------------------------------------------------------------------------------
      integer,                    intent(in)   :: IMIN,par_type
      integer,                    intent(in)   :: NBIN,NBINL
      real,dimension(KIDIM3),     intent(in)   :: RADIUS,SD
      real,dimension(NRR),        intent(in)   :: RADIUSL
      real,dimension(NRR,KCpar),  intent(in)   :: SDL
      real,dimension(KIDIM3,2),   intent(out)  :: SDFC
      real,dimension(0:2),        intent(out)  :: cv,std,rm,reff

!	-------------------------------------------------------------------------------------
      real,dimension(:),allocatable  ::  SDT,SDF,SDC,RADIUST      
      real,dimension(KCpar)          ::  temp
      integer  ::  i,i1,NBINT,ierr
      real     ::  pi = 3.141592653589793
      real     ::  dlnr
!	-------------------------------------------------------------------------------------

! ******************************************************************
! *************   Size*distributions*of*fine*and*coarse*modes   ****

      if(par_type .eq. par_type_SD_TB) then
        NBINT = NBIN        
      elseif(par_type .eq. par_type_SD_LB) then
        NBINT = NBINL      
      endif
      
      allocate(RADIUST(1:NBINT),stat=ierr)
      if (ierr /= 0) then
        write(tmp_message,'(a)') 'error while trying to allocate RADIUST(1:NBINT)'
        G_ERROR(trim(tmp_message))
      endif
      allocate(SDT(1:NBINT),stat=ierr)
      if (ierr /= 0) then
        write(tmp_message,'(a)') 'error while trying to allocate SDT(1:NBINT)'
        G_ERROR(trim(tmp_message))
      endif
      allocate(SDF(1:NBINT),stat=ierr)
      if (ierr /= 0) then
        write(tmp_message,'(a)') 'error while trying to allocate SDF(1:NBINT)'
        G_ERROR(trim(tmp_message))
      endif
      allocate(SDC(1:NBINT),stat=ierr)
      if (ierr /= 0) then
        write(tmp_message,'(a)') 'error while trying to allocate SDC(1:NBINT)'
        G_ERROR(trim(tmp_message))
      endif

      SDT(:)    = 0.0
      SDF(:)    = 0.0
      SDC(:)    = 0.0
      SDFC(:,:) = 0.0

      if(par_type .eq. par_type_SD_TB) then
        RADIUST(1:NBINT) = RADIUS(1:NBINT)
        SDT(1:NBINT) = SD(1:NBINT)            
        do i=1,NBINT
          if(i .le. IMIN) then
            SDF(i) = SDT(i)
            if(i .eq. IMIN) SDF(i) = SDF(i)*0.5
          endif
          if(i .ge. IMIN) then
            SDC(i) = SDT(i)
            if(i .eq. IMIN) SDC(i) = SDC(i)*0.5
          endif
        enddo ! i
        SDFC(1:NBINT,1) = SDF(1:NBINT)
        SDFC(1:NBINT,2) = SDC(1:NBINT)
      elseif(par_type .eq. par_type_SD_LB) then
        RADIUST(1:NBINT) = RADIUSL(1:NBINT)            
        dlnr = log(RADIUST(2))-log(RADIUST(1))
        do i1=1,NBIN
          temp(i1) = sum(SDL(1:NBINT,i1))*dlnr
        enddo ! i1
        do i=1,NBINT
          do i1=1,NBIN
            SDT(i) = SDT(i)+SDL(i,i1)/temp(i1)*SD(i1)
            if(i1 .le. IMIN) SDF(i) = SDF(i)+SDL(i,i1)/temp(i1)*SD(i1)
            if(i1 .gt. IMIN) SDC(i) = SDC(i)+SDL(i,i1)/temp(i1)*SD(i1)
          enddo ! i1
        enddo ! i
        SDFC(1:IMIN,1)      = SD(1:IMIN)
        SDFC(IMIN+1:NBIN,2) = SD(IMIN+1:NBIN)
      endif ! par_type .eq.

!      do i=1,NBINT
!         write(*,*) 'SD_total : ',RADIUST(i),SDT(i)
!      enddo
!      do i=1,NBINT
!         write(*,*) 'SD_fine  : ',RADIUST(i),SDF(i)
!      enddo
!      do i=1,NBINT
!         write(*,*) 'SD_coarse: ',RADIUST(i),SDC(i)
!      enddo

! R32(effective radius)
      call sd2m_reff(NBINT,RADIUST,SDT,reff(0))
      call sd2m_reff(NBINT,RADIUST,SDF,reff(1))
      call sd2m_reff(NBINT,RADIUST,SDC,reff(2))
!      write(*,*)'in sd2m_fine_coarse:  reff_total  =',rm(0)
!      write(*,*)'in sd2m_fine_coarse:  reff_fine   =',rm(1)
!      write(*,*)'in sd2m_fine_coarse:  reff_coarse =',rm(2)
! Volume median radius  
      call sd2m_rm(NBINT,RADIUST,SDT,rm(0))
      call sd2m_rm(NBINT,RADIUST,SDF,rm(1))
      call sd2m_rm(NBINT,RADIUST,SDC,rm(2))
!      write(*,*)'in sd2m_fine_coarse:  rm_total  =',rm(0)
!      write(*,*)'in sd2m_fine_coarse:  rm_fine   =',rm(1)
!      write(*,*)'in sd2m_fine_coarse:  rm_coarse =',rm(2)

! Volume concentration 
      call sd2m_cv(NBINT,RADIUST,SDT,cv(0))
      call sd2m_cv(NBINT,RADIUST,SDF,cv(1))
      call sd2m_cv(NBINT,RADIUST,SDC,cv(2))
!      write(*,*)'in sd2m_fine_coarse:  cv_total  =',cv(0)
!      write(*,*)'in sd2m_fine_coarse:  cv_fine   =',cv(1)
!      write(*,*)'in sd2m_fine_coarse:  cv_coarse =',cv(2)

! Volume median radius and standard deviation 
      call sd2m_std(NBINT,RADIUST,SDT,rm(0),cv(0),std(0))
      call sd2m_std(NBINT,RADIUST,SDF,rm(1),cv(1),std(1))
      call sd2m_std(NBINT,RADIUST,SDC,rm(2),cv(2),std(2))
!      write(*,*)'in sd2m_fine_coarse:  std_total  =',std(0)
!      write(*,*)'in sd2m_fine_coarse:  std_fine   =',std(1)
!      write(*,*)'in sd2m_fine_coarse:  std_coarse =',std(2)

      deallocate(RADIUST,stat=ierr)
      if (ierr /= 0) then
        write(tmp_message,'(a)') 'error while trying to deallocate RADIUST'
        G_ERROR(trim(tmp_message))
      endif
      deallocate(SDT,stat=ierr)
      if (ierr /= 0) then
        write(tmp_message,'(a)') 'error while trying to deallocate SDT'
        G_ERROR(trim(tmp_message))
      endif
      deallocate(SDF,stat=ierr)
      if (ierr /= 0) then
        write(tmp_message,'(a)') 'error while trying to deallocate SDF'
        G_ERROR(trim(tmp_message))
      endif
      deallocate(SDC,stat=ierr)
      if (ierr /= 0) then
        write(tmp_message,'(a)') 'error while trying to deallocate SDC'
        G_ERROR(trim(tmp_message))
      endif

      return
      end subroutine sd2m_fine_coarse

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!*******************   Size*distributions*effective*radius   ****

      subroutine sd2m_reff ( NBIN,RADIUS,SD,reff )
      
      implicit none
!	-------------------------------------------------------------------------------------
      integer,             intent(in)   :: NBIN
      real,dimension(NBIN),intent(in)   :: RADIUS,SD
      real,                intent(out)  :: reff
!	-------------------------------------------------------------------------------------
      real                  ::  pi = 3.141592653589793
      real,dimension(NBIN)  ::  SDN
!	-------------------------------------------------------------------------------------
!  R32(effective*radius) 
      SDN(:) = 0.0
      reff   = 0.0
      
      SDN(1:NBIN) = SD(1:NBIN)*3./(4.*pi*RADIUS(1:NBIN)**3)
      reff = SUM(SDN(1:NBIN)*RADIUS(1:NBIN)**3) / SUM(SDN(1:NBIN)*RADIUS(1:NBIN)**2)

!      write(*,*) 'in sd2m_reff: reff =',reff

      return
      end subroutine sd2m_reff
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!*******************   Size*distributions*effective*radius   ****

      subroutine sd2m_reff_2modes_retrieval ( ipix, GOUT_retrieval, reff )

      use mod_retr_general_output_derived_type
      use mod_par_inv, only : KIDIM3

      implicit none
!	-------------------------------------------------------------------------------------
      integer, intent(in) :: ipix
      type(output_segment_retrieval), intent(in) :: GOUT_retrieval
      real, dimension(0:2), intent(out) ::  reff
!	-------------------------------------------------------------------------------------
      integer                 ::  NBINT, ISD, i
      real,dimension(4*KIDIM3) ::  RADIUST,SDT
!	-------------------------------------------------------------------------------------
!  R32(effective*radius) for 2 modes aerosol retrieval (RIN%NSD=2)

      NBINT      = 0
      SDT(:)     = 0.0
      RADIUST(:) = 0.0
!write(*,'(a,i0)') 'ipix = ',ipix
      do ISD=0,GOUT_retrieval%information%nsd
        NBINT = GOUT_retrieval%information%ngrid(ISD)
        RADIUST(1:NBINT) = GOUT_retrieval%information%radius(1:NBINT,ISD)
        SDT(1:NBINT) = GOUT_retrieval%par%pixel(ipix)%sd(1:NBINT,ISD)
        call sd2m_reff ( NBINT, RADIUST(1:NBINT), SDT(1:NBINT), reff(ISD) )
!write(*,'(i4,es12.4,a)') isd,reff(ISD),'  - isd,reff(ISD)'
!write(*,'(a4,2a12,a)') 'i','RADIUST','SDT'
!do i=1,NBINT
!write(*,'(i4,2es12.4)') i,RADIUST(i),SDT(i)
!enddo

      enddo ! ISD

      return
      end subroutine sd2m_reff_2modes_retrieval

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!        !> @brief Routine calculates total sd for given lognormal bins.
!        !>
!        !> @author Tatsiana Lapionak
!        !> @date 24 MAR 2016
!        !>
!        !> @param[in]   nsd - number of particle components
!        !> @param[in]   NBIN - given number of bins in each component
!        !> @param[in]   RADIUS  - bin radius provided in settings
!        !> @param[in]   SD - bin concentrations
!        !> @param[in]   ngrid - number of grid points for total SD
!        !> @param[in]   RADIUSL - values of grid radii
!        !> @param[in]   SDL - values of SDs for given bins
!        !> @param[out]  SDT - total SD at grid points

!      subroutine total_sd_lognormal_bins( NSD, NBIN, RADIUS, SD, &
!                                          ngrid, RADIUSL, SDL,   &
!                                          SDT )
!      use mod_par_inv,     only : KIDIM3
!      use mod_par_OS,      only : KSD
!      use mod_par_DLS_bin, only : NRR, KCpar
      
!      implicit none
!!	-------------------------------------------------------------------------------------
!      integer,                   intent(in)  ::  NSD, ngrid
!      integer,dimension(KSD),    intent(in)  ::  NBIN
!      real,dimension(KIDIM3,KSD),intent(in)  ::  RADIUS, SD
!      real,dimension(NRR),       intent(in)  ::  RADIUSL
!      real,dimension(NRR,KCpar), intent(in)  ::  SDL
!      real,dimension(NRR,KSD),   intent(out) ::  SDT
!!	-------------------------------------------------------------------------------------
!      integer  ::  i, isd, ibins
!      real     ::  norm, dlnr
!      real     ::  sdl_temp(NRR,KCpar,KSD)
!!	----------------------------------------------------------------------
!      SDT(:,:) = 0.0
!      dlnr = ( log(RADIUSL(ngrid))-log(RADIUSL(1)) ) / (ngrid-1)

!      do isd=1, nsd
!        do i=1,NBIN(isd)
!          norm = sum( sdl(1:ngrid, i))*dlnr
!          sdl_temp(1:ngrid, i, isd) = sdl(1:ngrid, i) / norm
!        enddo
!        do i=1,ngrid
!          SDT(i,isd) = sum( SD(1:NBIN(isd),isd) * sdl_temp(i, 1:NBIN(isd), isd) )
!        enddo
!      enddo

!      return
!      end subroutine total_sd_lognormal_bins

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine calculates effective radius for lognormal size distribution.
        !>
        !> @author Tatsiana Lapionak
        !> @date 5 MAY 2015
        !>
        !> @param[in]   id_sd  - 0 - number, 1 - radius, 2 - cross section, 3 - volume sd
        !> @param[in]   rm     - mean radius in micromiters
        !> @param[in]   sigma  - standard deviation      
        ! @param[out]  reff   - effective radius in micromiters
        
! rn - mean radius of number SD
! df/dlnr = 1./(ln(sigma)*sqrt(2.*PI)) *  &
!           exp(-(ln(r)-ln(rn))**2/(2.*ln(sigma)*ln(sigma)))
! reff = rn*exp(5.*ln(sigma)*ln(sigma)/2.)

      function reff_lognormal_sd( id_sd, sigma, rm )
      
      use mod_stop_report
!      implicit none
!	----------------------------------------------------------------------
      integer, intent(in) :: id_sd
      real(4), intent(in) :: sigma, rm
      real(4) :: reff_lognormal_sd
!	----------------------------------------------------------------------
      real(4) :: lnsigma, rn
!	----------------------------------------------------------------------
!  R32(effective*radius) 
      reff_lognormal_sd = -999.0
      lnsigma = log(sigma)
      
      select case(id_sd)
      case(0) ! number      
        rn   = rm
      case(1) ! radius   
        rn   = exp( log(rm) - lnsigma*lnsigma)
      case(2) ! cross section     
        rn   = exp( log(rm) - 2.0*lnsigma*lnsigma )
      case(3) ! volume     
        rn   = exp( log(rm) - 3.0*lnsigma*lnsigma )
      case default      
        write(tmp_message,'(a,i0,a)') 'id_sd = ',id_sd,  &
        ' is not valid id_sd of aerosol particle size distribution'
        G_ERROR(trim(tmp_message))
      end select
      reff_lognormal_sd = rn * exp( 5.0*lnsigma*lnsigma*0.5 )

!      write(*,*) 'in reff_lognormal_sd: reff =',reff_lognormal_sd

      end function reff_lognormal_sd

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!*******************   Size*distributions*volume*median*radius   ****

      subroutine sd2m_rm ( NBIN,RADIUS,SD,    &
                           rm )
      
      implicit none
!	-------------------------------------------------------------------------------------
      integer,             intent(in)   :: NBIN
      real,dimension(NBIN),intent(in)   :: RADIUS,SD
      real,                intent(out)  :: rm
!	-------------------------------------------------------------------------------------
!  Volume median radius
      
      rm = sum(SD(1:NBIN)*LOG(RADIUS(1:NBIN)))/sum(SD(1:NBIN))
      rm = exp(rm)

!      write(*,*)'in sd2m_rm:  rm =',rm
      
      return
      end subroutine sd2m_rm

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!*******************   Size*distributions*volume*concentration   ****

      subroutine sd2m_cv ( NBIN,RADIUS,SD,    &
                           cv )
      
      implicit none
!	-------------------------------------------------------------------------------------
      integer,             intent(in)   :: NBIN
      real,dimension(NBIN),intent(in)   :: RADIUS,SD
      real,                intent(out)  :: cv
!	-------------------------------------------------------------------------------------
      real                  ::  dlnr
!	-------------------------------------------------------------------------------------
!  Volume median radius 
      
      dlnr = log(RADIUS(2))-log(RADIUS(1))

      cv = sum(SD(1:NBIN))*dlnr

!      write(*,*)'in sd2m_cv:  cv =',cv
      
      return
      end subroutine sd2m_cv
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!*******************   Size*distributions*standard*deviation   ****

      subroutine sd2m_std ( NBIN,RADIUS,SD,rm,cv,    &
                            std )
      
      implicit none
!	-------------------------------------------------------------------------------------
      integer,             intent(in)   :: NBIN
      real,                intent(in)   :: rm,cv
      real,dimension(NBIN),intent(in)   :: RADIUS,SD
      real,                intent(out)  :: std
!	-------------------------------------------------------------------------------------
      real                  ::  dlnr
!	-------------------------------------------------------------------------------------
!  Volume median radius 
      
      dlnr = log(RADIUS(2))-log(RADIUS(1))

      std = sqrt( sum( (log(RADIUS(1:NBIN))-log(rm))**2*SD(1:NBIN) )*dlnr/cv )

!      write(*,*)'in sd2m_std:  std =',std
      
      return
      end subroutine sd2m_std
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_GOUT_product_flags (RIN, GOUT_products)

      use mod_retr_settings_derived_type
      
      implicit none
!	-------------------------------------------------------------------------------------
      type(retr_input_settings),    intent(in)     ::  RIN
      type(output_segment_products),intent(inout)  ::  GOUT_products
!	-------------------------------------------------------------------------------------
      integer  ::  i, idim1, par_type
!	-------------------------------------------------------------------------------------
! retrieval
      GOUT_products%retrieval%res = .true.
      GOUT_products%retrieval%par = .true.
      GOUT_products%retrieval%fit = .true.

! aerosol
      GOUT_products%aerosol%opt   = .true.
      GOUT_products%aerosol%rind  = .true.
      GOUT_products%aerosol%chem  = .false.
      do idim1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(idim1)        
        if(par_type .gt. par_type_aerosol_beg .and. par_type .lt. par_type_aerosol_end) then      
          if(par_type .eq. par_type_CXRI_chem .or. par_type .eq. par_type_CXRI_nmix) then
          GOUT_products%aerosol%chem = .true.          
          exit
          endif
        endif        
      enddo ! IDIM1

! gases (hyperspectral)
      GOUT_products%gases%concentration = .false.
      GOUT_products%gases%absorption = .false.
      do idim1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(idim1)
        if(par_type .gt. par_type_gases_concentration_beg .and. par_type .lt. par_type_gases_concentration_end) then
          GOUT_products%gases%concentration = .true.
          GOUT_products%gases%absorption = .true.
          exit
        endif
      enddo ! IDIM1

      GOUT_products%aerosol%phmx  = .false.
      if(RIN%DLSF%keyEL .gt. 0) then
        GOUT_products%aerosol%phmx  = .true.
      endif
      GOUT_products%aerosol%lidar = .false.
      if(RIN%DLSF%keyEL .gt. 0) then
        GOUT_products%aerosol%lidar = .true.
      endif

      GOUT_products%aerosol%sd2m_mph = .false.
      GOUT_products%aerosol%sd2m_ext = .false.
      if(RIN%NSD .eq. 1) then
        do idim1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(idim1)
          if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) exit
        enddo
        if(par_type .ne. par_type_SD_LN) then
        if(RIN%products%aerosol%sd2m_mph .or. RIN%products%aerosol%sd2m_ext) then
          GOUT_products%aerosol%sd2m_mph = .true.
          GOUT_products%aerosol%sd2m_ext = .true.
        endif
        endif
      elseif(RIN%NSD .eq. 2) then
        !do idim1=1,RIN%NDIM%n1
          !par_type = RIN%NDIM%par_type(idim1)
          !if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) exit
        !enddo
        !if(par_type .eq. par_type_SD_LB) then
          !if(RIN%products%aerosol%sd2m_mph .or. RIN%products%aerosol%sd2m_ext) then
            !GOUT_products%aerosol%sd2m_mph = .false.
            !GOUT_products%aerosol%sd2m_ext = .false.
          !endif
        !else
          if(RIN%products%aerosol%sd2m_mph) then
            GOUT_products%aerosol%sd2m_mph = .true.
          endif
        !endif
      endif

      GOUT_products%aerosol%pm = .false.
      !if(RIN%products%aerosol%pm) then
      !if(RIN%products%aerosol%sd2m_mph .or. RIN%products%aerosol%sd2m_ext) then
        !GOUT_products%aerosol%pm = .true.
      !endif
      !endif
      GOUT_products%aerosol%types = .false.
      !if(RIN%products%aerosol%types) then
      !if(RIN%products%aerosol%sd2m_mph .or. RIN%products%aerosol%sd2m_ext) then
        !GOUT_products%aerosol%types = .true.
      !endif
      !endif

! surface
      GOUT_products%surface%surf = .false.
      GOUT_products%surface%bhr_iso = .false.
      do idim1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(idim1)        
        if(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF_water_end) then      
          GOUT_products%surface%surf = .true.          
        exit
        endif        
      enddo ! IDIM1 

! errest
      GOUT_products%errest%par = .false.
      !if( RIN%products%errest%par ) &
        !GOUT_products%errest%par = .true.
      ! aerosol  
      GOUT_products%errest%aerosol%opt   = .false.
      GOUT_products%errest%aerosol%lidar = .false.
      !if( RIN%products%errest%aerosol%opt ) &
        !GOUT_products%errest%aerosol%opt = .true.
      !if( RIN%products%errest%aerosol%lidar ) &
        !GOUT_products%errest%aerosol%lidar = .true.

! forcing
      GOUT_products%forcing%bbflux  = .false.
      GOUT_products%forcing%forcing = .false.
      !if( RIN%products%forcing%bbflux .or. RIN%products%forcing%forcing) then
        !GOUT_products%forcing%bbflux = .true.
        !GOUT_products%forcing%forcing = .true.
      !endif

      return
      end subroutine set_GOUT_product_flags

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_segm_retr_output ( RIN, segment, GOUT )
      use mod_sdata_derived_type
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_par_DLS,     only : KNpar
      use mod_par_DLS_bin, only : NRR,KCpar
      use mod_par_inv, only : KIDIM3
      use m_inssor
      use mod_stop_report
      use mod_c_utils
      use mod_globals, only : GBL_FILE_PATH_LEN
      
      implicit none
! -----------------------------------------------------------------------------------------
! IN :
      type(retr_input_settings),     intent(in) :: RIN
      type(segment_data),            intent(in) :: segment
! -----------------------------------------------------------------------------------------
! OUT :
      type(output_segment_general),intent(out) :: GOUT
! -----------------------------------------------------------------------------------------
! LOCAL :
      integer :: i, i1, ii, ibeg, iend
      integer :: npixels
      character(LEN=20) :: NBINC
      character(LEN=GBL_FILE_PATH_LEN) :: file_name
      integer :: iu, NRC1
      real,dimension(NRR,KCpar) :: SDL
      real,dimension(NRR) :: gridL
      integer :: IDIM1, IDIM2, IDIM3
      integer :: NDIM2, NDIM3
      integer :: par_type, ngrid
      real :: dlnr, rmax0
      real,dimension(4*KIDIM3) :: radius, radius_tmp
      integer :: KN
! -----------------------------------------------------------------------------------------
      character (len=GBL_FILE_PATH_LEN)   ::  internal_file_path
      character (len=GBL_FILE_PATH_LEN)   ::  distname_N
      !logical :: icv
! -----------------------------------------------------------------------------------------
      call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)
      call cstring2fstring(RIN%DLSF%distname_N, distname_N)
      
!! ***  Initialize Retrieval OUTput structure       

      call output_coordinates_initialization(GOUT%coord)
      call output_retrieval_initialization(GOUT%retrieval)
      call output_aerosol_initialization(GOUT%aerosol)
      call output_surface_initialization(GOUT%surface)
      call output_forcing_initialization(GOUT%forcing)

      npixels = segment%npixels

! Set coordinates of pixels for the segment
      GOUT%coord%pixel(1:npixels)%x_lon = segment%pixels(1:npixels)%x
      GOUT%coord%pixel(1:npixels)%y_lat = segment%pixels(1:npixels)%y
      GOUT%coord%pixel(1:npixels)%t_masl = segment%pixels(1:npixels)%t

! Presence of volume concentration characteristic in settings
!      icv = .false.
!      do i=1,RIN%NDIM%n1
!        par_type = RIN%NDIM%par_type(i)
!        if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
!          icv = .true.
!        exit
!        endif ! RIN%NDIM%par_type(i) .gt. par_type_SD_beg .and.
!      enddo ! i

! ***  Define Retrieval OUTput structure with variables general for segment
      NRC1 = 0
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then
          select case(par_type)
          case(par_type_SD_LB)
            NRC1 = sum(RIN%NDIM%n3(1:RIN%NSD,IDIM1))
            if ( RIN%indep_par ) then
            if ( RIN%flag_plus ) then
            NRC1 = NRC1 + 1
            endif
            endif
            GOUT%retrieval%information%nbins = NRC1
            ! Read SD for lognormal bins
            if(KCpar .lt. NRC1) then
                write(tmp_message,'(2(a,i0),2a)') &
                'KCpar = ',KCpar,' .lt. NRC1 = ',NRC1, &
                NEW_LINE('A'), &
                'KCpar - parameter defined in mod_DLS_bin.f'
                G_ERROR(trim(tmp_message))
            endif ! KCpar .lt. NRC1
            write(NBINC,*) NRC1
            file_name = &
            trim(internal_file_path)//trim(distname_N)//'SD_bin'//trim(adjustl(NBINC))//'.dat'
            open(newunit=iu,file=trim(file_name),status='old')
                read(iu,*)
                do i=1,NRC1
                  read(iu,*)
                enddo ! i
                read(iu,*)
                read(iu,*)
                do i=1,NRR
                  read(iu,*) ii,gridL(i),(SDL(i,i1),i1=1,NRC1)
                enddo	! i
            close(iu)
            do IDIM2=0,RIN%NDIM%n2(IDIM1)
            GOUT%retrieval%information%ngrid(IDIM2) = NRR
            GOUT%retrieval%information%radius(1:NRR,IDIM2) = gridL(1:NRR)
            enddo
            GOUT%retrieval%information%sd_lb(1:NRR,1:NRC1) = SDL(1:NRR,1:NRC1)
          case(par_type_SD_LN)
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
              KN = RIN%KNLN(IDIM2)
              GOUT%retrieval%information%ngrid(IDIM2) = KN
              dlnr = (log(RIN%RADIUS1(2,IDIM2))-log(RIN%RADIUS1(1,IDIM2)))/real(KN-1)
              rmax0 = 0.0
              do i=1,KN
                GOUT%retrieval%information%radius(i,IDIM2) = &
                                exp(log(RIN%RADIUS1(1,IDIM2))+dlnr*(i-1))
              enddo ! i
              if ( GOUT%retrieval%information%radius(KN,IDIM2) .gt. rmax0 ) then
                rmax0 = GOUT%retrieval%information%radius(KN,IDIM2)
              endif
            enddo ! IDIM2
            KN = MAXVAL( GOUT%retrieval%information%ngrid(1:RIN%NDIM%n2(IDIM1)) )
            GOUT%retrieval%information%ngrid(0) = KN
            GOUT%retrieval%information%radius(1,0) = &
            MINVAL( GOUT%retrieval%information%radius(1,1:RIN%NDIM%n2(IDIM1)) )
            GOUT%retrieval%information%radius(KN,0) = rmax0
            !
            dlnr = (log(rmax0)-log(GOUT%retrieval%information%radius(1,0)))/real(KN-1)
            do i=1,KN
              GOUT%retrieval%information%radius(i,0) = &
                    exp(log(GOUT%retrieval%information%radius(1,0))+dlnr*(i-1))
            enddo ! i
            !write(*,*) GOUT%retrieval%information%ngrid(0:RIN%NSD)
            !do i=0,RIN%NSD
            !write(*,'(a,i0)') 'tracer # ',i
            !write(*,'(10es12.4)') GOUT%retrieval%information%radius(1:GOUT%retrieval%information%ngrid(i),i)
            !enddo
            !stop 'test radius in set_segm_retr_output'
          case(par_type_SD_TB)
            radius(:) = 0.0
            iend = 0
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            if ( RIN%indep_par ) then
            if ( RIN%flag_plus ) then
            NDIM3 = NDIM3 + 1
            endif
            endif
            GOUT%retrieval%information%ngrid(IDIM2) = NDIM3
            GOUT%retrieval%information%radius(1:NDIM3,IDIM2) = &
                                              RIN%RADIUS1(1:NDIM3,IDIM2)
            ! Set radius grid points for total SD
              ibeg = iend + 1
              iend = iend + NDIM3
              radius(ibeg:iend) = &
                      GOUT%retrieval%information%radius(1:NDIM3,IDIM2)
            enddo ! IDIM2
            ngrid = iend
            ! Sort radius array in ascending order
            call inssor(radius(1:ngrid))
            ! Check and remove repeated radius values
            radius_tmp(:) = 0.0
            iend = 1
            radius_tmp(iend) = radius(iend)
            do i=2,ngrid
              if(abs(radius(i)-radius(i-1))/radius(i) .gt. 1e-4) then
              iend = iend+1
              radius_tmp(iend) = radius(i)
              endif
            enddo
            ngrid = iend
            radius(:) = 0.0
            radius(1:ngrid) = radius_tmp(1:ngrid)
            GOUT%retrieval%information%ngrid(0) = ngrid
            GOUT%retrieval%information%radius(1:ngrid,0) = radius(1:ngrid)
          end select
        exit
        endif ! par_type .gt. par_type_SD_beg .and.
      enddo ! IDIM1

      GOUT%retrieval%information%npixels = npixels
      GOUT%retrieval%information%nwl = RIN%nw
      GOUT%retrieval%information%nsd = RIN%nsd
      GOUT%retrieval%information%nnoises = RIN%NOISE%INOISE
      GOUT%retrieval%information%wl(1:RIN%nw) = RIN%wave(1:RIN%nw)

      GOUT%retrieval%information%ndim%n1 = RIN%ndim%n1
      GOUT%retrieval%information%ndim%n2(:) = RIN%ndim%n2(:)
      GOUT%retrieval%information%ndim%n3(:,:) = RIN%ndim%n3(:,:)
      GOUT%retrieval%information%ndim%ISTARSING(:,:) = RIN%ndim%ISTARSING(:,:)
      GOUT%retrieval%information%ndim%par_type(:) = RIN%ndim%par_type(:)
      GOUT%retrieval%information%ndim%par_retr(:) = RIN%ndim%par_retr(:)

      GOUT%retrieval%information%ngas = 0
      do i=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(i)
        if(par_type .gt. par_type_gases_concentration_beg .and. &
           par_type .lt. par_type_gases_concentration_end) then
           GOUT%retrieval%information%ngas = GOUT%retrieval%information%ngas + 1
        endif
      enddo ! i

      GOUT%retrieval%information%flag_plus = RIN%flag_plus
      GOUT%retrieval%information%ndim_plus%n1 = RIN%ndim_plus%n1
      GOUT%retrieval%information%ndim_plus%n2(:) = RIN%ndim_plus%n2(:)
      GOUT%retrieval%information%ndim_plus%n3(:,:) = RIN%ndim_plus%n3(:,:)
      GOUT%retrieval%information%ndim_plus%ISTARSING(:,:) = RIN%ndim_plus%ISTARSING(:,:)
      GOUT%retrieval%information%ndim_plus%par_type(:) = RIN%ndim_plus%par_type(:)
      GOUT%retrieval%information%ndim_plus%par_retr(:) = RIN%ndim_plus%par_retr(:)


      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .eq. par_type_CXRI_nmix ) then
          if(.not. RIN%indep_par) then
            NDIM2 = RIN%NDIM%n2(IDIM1)
          else
            NDIM2 = RIN%NDIM_plus%n2(IDIM1)
          endif
          do IDIM2=1,NDIM2
          GOUT%retrieval%information%nchem(IDIM2) = RIN%ndim%n3(IDIM2,IDIM1)
          enddo
        exit
        elseif(par_type .eq. par_type_CXRI_chem ) then
          NDIM2 = RIN%NDIM%n2(IDIM1)
          do IDIM2=1,NDIM2
          GOUT%retrieval%information%nchem(IDIM2) = RIN%ndim%n3(IDIM2,IDIM1)
          enddo
        exit
        endif
      enddo ! IDIM1

      return
      end subroutine set_segm_retr_output

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine get_PM(D_MAX,RIN,GOUT,segment_meas,PM)
!AL function calculates atmospheric particulate matter (PM) on the ground level (MASL) 
!AL with the maximum diameter D_MAX, using retrieved aerosol optical and microphysical data stored in GOUT structure
!AL so return provides PM{D_MAX}
      use mod_retr_general_output_derived_type
      use mod_retr_settings_derived_type
      use mod_sdata_derived_type
      use mod_par_DLS_bin, only : NRR ! number of bins in lognormal functions
      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE,KVERTM
      use mod_par_OS,      only : HMAX_atm, KVERT_WD
      USE MOD_RT_SOS
      use mod_sdata, only : get_hvp_lidar
      use mod_intrpl_linear
      use mod_stop_report
      
      implicit none
! -----------------------------------------------------------------------------------------
! IN :
      real,                          intent(in)  :: D_MAX ! maximum diameter of the particles in micrometers
      type(retr_input_settings),     intent(in)  :: RIN   ! structure containing retrieval input settings
      type(output_segment_general),  intent(in)  :: GOUT  ! GOUT structure containing all the retrieved values
      type(segment_data),            intent(in)  :: segment_meas  ! structure containing measurements information
! -----------------------------------------------------------------------------------------
! OUT:
      real,dimension(KIMAGE),       intent(out)  :: PM
! -----------------------------------------------------------------------------------------
! LOCAL:
      integer                              :: IDIM1,IDIM2, i, ipix, ISD ! indice
      integer                              :: par_type !stores value corresponding to parameter type
      integer                              :: npixels, NDIM3
 !     logical                              :: icv ! flag if concentration is retrieved as a standalone value
      real,dimension(NRR,KIDIM2,KIMAGE)    :: SDout  ! used to store columnar volume concentration and Size distribution parameters
      real,dimension(KIDIM2,KIMAGE)        :: CV_PM,VC_ground ! accumulator of volume concentration for the specified size, aerosol vertical concentration at ground level
      real,dimension(NRR,KIDIM2)           :: R ! radii corresponding to the size distributions
      real,dimension(KIDIM2)               :: DLNR ! log difference for radii of different modes
      integer,dimension(KIDIM2)            :: NR ! number of radii in size distribution
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: RERI,sigma,h01,SD,CV ! for real part of refractive index and std of aerosol vertical distribut
      real,dimension(KIDIM2,KIMAGE)        :: RERI_mean !spectrally mean real refractive index
      real,dimension(KIDIM2)               :: WF ! water fraction and
      real                                 :: HGR_km, RSD ! height of the ground in km(above sea level)
      integer                              :: NHVP_meas,NHVP_retr
      real,dimension(KVERTM,KIMAGE)        :: HVP_meas
      real,dimension(KVERTM)               :: HVP_retr_km
      integer                              :: NH
      real,dimension(KVERT_WD)             :: H_km
      real, dimension(KSD)                 :: xnorm
      character(len=20) :: distr_type
      real, dimension(KVERT_WD) :: prof_temp
! -----------------------------------------------------------------------------------------
 !     icv = .false.
      CV(:,:,:)=1.0
      SDout(:,:,:)=0.0
      npixels = segment_meas%npixels
! check if concentration is in list of retrieved parameters if yes get particle volume concentration
      do IDIM1=1,RIN%NDIM%n1
         par_type = RIN%NDIM%par_type(IDIM1)
!get and rewrite CV if it is retrieved
         if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
!           icv = .true.
           call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,CV )
!           exit
         endif ! par_type .gt. par_type_Cv_beg .and.
!get the standard deviation of the aerosol vertical distribution
         if(par_type .eq. par_type_AVP_par_std) then
            call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,sigma )
         endif
!get spectrally mean aerosol refractive index
         if(par_type .gt. par_type_RERI_beg .and. par_type .lt. par_type_RERI_end) then
!          if(par_type .ne. par_type_CXRI_chem .and. par_type .ne. par_type_CXRI_nmix) then
!             write(*,*) 'before call unpack_retr_param_to_print(RERI)'
!             call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,RERI )
!             write(*,*) 'RERI from unpack=', RERI
!             write(*,*) GOUT%aerosol%rind%pixel(1:npixels)%wl(1:RIN%NW)%mreal(1:RIN%NDIM%n2(IDIM1))
             do IDIM2=1,RIN%NSD! %RIN%NSD !%RIN%NDIM%n2(IDIM1)
!               NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
               do ipix=1,npixels
                   IF(IDIM2 .GT. RIN%NDIM%n2(IDIM1)) RERI_mean(IDIM2,ipix)=RERI_mean(1,ipix) ! if number of modes is more than number of height parameters assume all other modes as first
!                  RERI_mean(IDIM2,ipix)=SUM(RERI(1:NDIM3,IDIM2,ipix))/RIN%NW !calculating spectrally mean refractive index
                   RERI_mean(IDIM2,ipix)=SUM(GOUT%aerosol%rind%pixel(ipix)%wl(1:RIN%NW)%mreal(IDIM2))/RIN%NW !calculating spectrally mean refractive index gettin direct access to GOUT since it is universal for chemistry and retrieved refr. index
!                   WRITE(*,*) 'ipix, ISD, REFR', ipix,IDIM2, GOUT%aerosol%rind%pixel(ipix)%wl(1:RIN%NW)%mreal(IDIM2)
               enddo !ipix
            enddo ! IDIM2
!          endif ! par_type .ne. par_type_CXRI_chem
        endif ! par_type .gt. par_type_RERI_beg .and.
      enddo ! IDIM1

! get size distribution and transform it into triangle bins
      do IDIM1=1,RIN%NDIM%n1
         par_type = RIN%NDIM%par_type(IDIM1)
         select case (par_type)
         case(par_type_SD_TB) ! triangular bins

           call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,SD )
           do ISD=1,RIN%NDIM%n2(IDIM1) ! aerosol component loop
              NDIM3 = RIN%NDIM%n3(ISD,IDIM1)
              do ipix=1,npixels
                 do i=1,NDIM3
!if CV was retrieved it is not ones, and SD is normalized multiplying to get correct values
                    SDout(i,ISD,ipix)=SD(i,ISD,ipix)*CV(1,ISD,ipix)

                 enddo !i
              enddo! ipix
! get the radii that correspond to the distribution
              if (NRR .GE. NDIM3) then
                 R(1:NDIM3,ISD)=RIN%RADIUS1(1:NDIM3,ISD)
                 NR(ISD)=NDIM3
              else
                 write(tmp_message,'(a)') 'Error: Radii could not be copied: NRR<NDIM3'
                 G_ERROR(trim(tmp_message))
              endif !NRR>NDIM3
           enddo! ISD

         case(par_type_SD_LB) ! lognormal bins
!AL           write (*,*) 'IDIM1, npixels',IDIM1, npixels
           call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,SD )
                 do ISD=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
                    NDIM3 = RIN%NDIM%n3(ISD,IDIM1) ! number of bins
                    NR(ISD)=GOUT%retrieval%information%ngrid(ISD) ! number of grid radius points
                    R(1:NR(ISD),ISD)=GOUT%retrieval%information%radius(1:NR(ISD),ISD) ! values of radii
                    do ipix=1,npixels
                       do i=1,NR(ISD)
!summing up lognormal distributions to get correct SD in 45 bins
                          SDout(i,ISD,ipix) = SUM(GOUT%retrieval%information%sd_lb(i,1:NDIM3)*SD(1:NDIM3,ISD,ipix)*CV(1,ISD,ipix))
!AL                   write(*,*) 'i,ISD,ipix,SD', i,ISD,ipix,SD(1:NDIM3,ISD,ipix)!GOUT%retrieval%par%pixel(ipix)%par(1:5)
                       enddo ! i
                    enddo ! ipix
                 enddo  ! ISD

         case(par_type_SD_LN) !lognormal distribution
           call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,SD )
                 do ISD=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
                    NR(ISD)=GOUT%retrieval%information%ngrid(ISD)
                    R(1:NR(ISD),ISD) = GOUT%retrieval%information%radius(1:NR(ISD),ISD)
                    do ipix=1,npixels
                        call SIZEDISDN (  0,                       & ! IPRI
                                        -NR(ISD),1,3,1,1,          & ! KN,ID,NSD,NMD minus before KN defines logarithmical scale
                                        CV(1,ISD,ipix),            &
                                        SD(2,ISD,ipix),            &
                                        SD(1,ISD,ipix),            &
                                        RIN%radius1(1,ISD),        &
                                        RIN%radius1(2,ISD),        &
                                        R(1:NR(ISD),ISD),          &
                                        SDout(1:NR(ISD),ISD,ipix), &
                                        RSD                        &
                                       )
                    enddo ! ipix
                 enddo  ! ISD
        end select
      enddo !IDIM1

! get particle volume concentration of size lower than D_MAX with interpolation between bins
      CV_PM(:,:)=0.0

!      do IDIM1=1,RIN%NDIM%n1
!         par_type = RIN%NDIM%par_type(IDIM1)
!         if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then

            do ISD=1,RIN%NSD    ! aerosol component loop
               do ipix=1,npixels
                  i=1
                  do while (((R(i,ISD) .LE. D_MAX*0.5)) .AND. (i .LE. NR(ISD))) ! NR is not to create an infinite loop
                     CV_PM(ISD,ipix)=CV_PM(ISD,ipix)+SDout(i,ISD,ipix) ! sum up all bins less than D/2
                     i=i+1
                  enddo ! R<D/2
                  CV_PM(ISD,ipix)=CV_PM(ISD,ipix)-SDout(i-1,ISD,ipix)*0.5     & ! trapezoidal integration, last bin should be 1/2
                  +LINEAR(R(i-1:i,ISD),SDout(i-1:i,ISD,ipix),2,D_MAX*0.5)*0.5 & ! interpolating between bins for what is left we divide on 2 to simulate the trapezoidal integration, DS_out(1, ISD, ipix) is assumed to be too small to bother
                  *LOG(D_MAX/2/R(i-1,ISD))/LOG(R(2,ISD)/R(1,ISD))!divide on logR2-logR1 because we are going to multiply on it afterwards and for this part it's a wrong radius step
              enddo !ipix
               CV_PM(ISD,:)=CV_PM(ISD,:)*LOG(R(2,ISD)/R(1,ISD))! multiply on DLNR for the specified mode
            enddo! ISD
!         endif! par_type is size distribution
!      enddo !IDIM1


! get aerosol vertical concentration value at the observation level
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_AVP_beg  .and. par_type .lt. par_type_AVP_end) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,h01 )
!          write(*,*),'h01=', h01
          if(par_type .eq. par_type_AVP_prof) then
            call get_hvp_lidar (  segment_meas,       & ! IN
                                  NHVP_meas,HVP_meas  & ! INOUT
                               )
            NHVP_retr = RIN%NDIM%n3(1,IDIM1) ! number of retrieved parameters describing aerosol vertical profile
            do ipix=1,npixels
            HGR_km = segment_meas%pixels(ipix)%MASL*0.001
            call get_AVP_altitudes_retr ( HGR_km,                           &
                                          NHVP_meas,HVP_meas(:,ipix)*0.001, &
                                          NHVP_retr,HVP_retr_km )

            ! Aerosol concentration profile normalization
            call grid_altitudes_LUT_with_gas ( HGR_km, HMAX_atm*0.001,       & ! IN
                                      NHVP_retr, HVP_retr_km(1:NHVP_retr),  &
                                      NH, H_km(1:NHVP_retr+2)               & ! OUT
                                    )
              distr_type = 'lut'
              do IDIM2=1,RIN%NSD!RIN%NDIM%n2(IDIM1)
                  call discrvd_single_atm_component ( distr_type, 0.0, 0.0,                 &
                                                      NHVP_retr, HVP_retr_km(1:NHVP_retr),  &
                                                      h01(1:NHVP_retr,IDIM2,ipix)*0.001,    &
                                                      NH, H_km(1:NH),                       &
                                                      xnorm(IDIM2), prof_temp(1:NH)         & ! OUT
                                                    )
              VC_ground(IDIM2,ipix)=prof_temp(NH)/xnorm(IDIM2)*0.001 ! 0.001 needs to be done to make VC_ground in [1/m] same as in print_GOUT
              IF(IDIM2 .GT. RIN%NDIM%n2(IDIM1)) VC_ground(IDIM2,ipix)=VC_ground(1,ipix) ! if number of modes is more than number of height parameters assume all other modes as first
              enddo ! IDIM2
            enddo ! ipix
            elseif(par_type .eq. par_type_AVP_par_height) then
            NHVP_retr = RIN%NDIM%n3(1,IDIM1) ! if code was initialized right it should be 1
            if(RIN%aer_prof_type .eq. 0) then
              distr_type = 'exponential'
            elseif(RIN%aer_prof_type .eq. 1) then
              distr_type = 'gaussian'
            elseif(RIN%aer_prof_type .eq. 2) then
              distr_type = 'threshold'
            endif
            ! calculating grid of altitudes at KVERT_WD points, to get full profile 
            ! from ground to top of atmosphere and normalize it correctly afterwards
            NH = KVERT_WD
            do ipix=1,npixels
              HGR_km = segment_meas%pixels(ipix)%MASL*0.001
              call grid_altitudes_log ( HGR_km, HMAX_atm*0.001,  &
                                        NH, H_km(1:NH)           &
                                     )
              do IDIM2=1,RIN%NSD ! RIN%NDIM%n2(IDIM1)
                  call discrvd_single_atm_component ( distr_type,                             &
                                                      h01(1,IDIM2,ipix)*0.001, sigma(1,IDIM2,ipix)*0.001, & ! heigh parameter of the profile, half width of profile
                                                      NH, H_km(1:NH), prof_temp(1:NH),        & ! nhinp, hinp_km, vdinp are not used for 'gaussian' and 'exponential'
                                                      NH, H_km(1:NH),                         & ! nhout, hout_km
                                                      xnorm(IDIM2), prof_temp(1:NH)           & ! OUT
                                                    )
              VC_ground(IDIM2,ipix) = prof_temp(NH)/xnorm(IDIM2)*0.001 ! 0.001 needs to be done to make VC_ground in [1/m] same as in print_GOUT
              if(IDIM2 .gt. RIN%NDIM%n2(IDIM1)) VC_ground(IDIM2,ipix) = VC_ground(1,ipix) ! if number of modes is more than number of height parameters assume all other modes as first
             enddo ! IDIM2
           enddo ! ipix
          endif ! par_type=AVP_height
        endif ! par_type .gt. par_type_AVP_beg  .and. 
      enddo ! IDIM1

! calculate particle matter
!write(*,*) 'In get_aerosol_PM'
      do ipix=1,npixels
         WF(1:RIN%NSD)=(1.6-RERI_mean(1:RIN%NSD,ipix))/(0.27)!(1.6-1.33)
         PM(ipix)=SUM(VC_ground(1:RIN%NSD,ipix)*CV_PM(1:RIN%NSD,ipix)*(2.3-1.3*WF(1:RIN%NSD))*1e6)! multiply on 10^6 since density on base of WF is in g/cm^3 and PM is in g/m^3
!         IF (PM(ipix) .LT. 0) then
!         write(*,*) 'RERI_mean=',RERI_mean(1:RIN%NSD,ipix)
!         write(*,*) 'WF=', WF(1:RIN%NSD)
!         write(*,*) 'D_MAX=', D_MAX
!         write(*,*) 'ipix,VC_grond, CV_PM', ipix, VC_ground(1:RIN%NSD,ipix),CV_PM(1:RIN%NSD,ipix)
!         write(*,*) 'PM=', PM(ipix)
!         stop
!         endif
!         write(*,*) 'CV_PM=',
      enddo

      end subroutine get_PM
!****************************
!****
!****   AEROSOL CLASSIFICATION
!****
!****************************
      subroutine get_aerosol_types(RIN,GOUT,npixels,aerosol_type_index)
!AL function performs aerosol classification by type on the base of the retrieve aerosol microphysical parameters
!AL return is the aerosol_type index, witch describes aerol types as follows:
!Aerosol type index – aerosol type:
!                 0 – Complex mixture
!                 1 – Background Aerosol
!                 2 – Water/Maritime
!                 3 — Urban Polluted
!                 4 – Mixed aerosol
!                 5 – Urban Clean
!                 6 – Smoke Smoldering
!                 7 – Smoke flaming
!                 8 – Mineral dust

      use mod_retr_general_output_derived_type
      use mod_retr_settings_derived_type
      use mod_sdata_derived_type
      use mod_par_DLS_bin, only : NRR ! number of bins in lognormal functions
      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE,KVERTM 
!      use mod_par_OS,      only : HMAX_atm
      USE MOD_RT_SOS
!      use mod_sdata, only : get_hvp
      use mod_intrpl_linear
      implicit none
! -----------------------------------------------------------------------------------------
! IN :
      type(retr_input_settings),     intent(in)  :: RIN   ! structure containing retrieval input settings
      type(output_segment_general),  intent(in)  :: GOUT  ! GOUT structure containing all the retrieved values
!      type(segment_data),            intent(in)  :: segment_meas  ! structure containing measurements information use it inly for npixels
      integer,                       intent(in)  :: npixels
! -----------------------------------------------------------------------------------------
! OUT:
      integer,dimension(KIMAGE),       intent(out)  :: aerosol_type_index ! index of aerosol type for each pixel
! -----------------------------------------------------------------------------------------
! internal
      integer                                       :: par_type
!      real,dimension(0,2)                           :: cv,std,rm,reff ! params of lognormal distribution
      real                                          :: coarse_fine_ratio ! ratio of coarse and fine mode concentrations
      real                                          :: RERI_mean !spectrally mean real refractive index ! mean aerosol refractive index
      real,dimension(KIDIM3,KIDIM2,KIMAGE)          :: sph ! aerosol sphericity faction
      real                                          :: sphericity
      real                                          :: AOT440,AOT870,SSA440,SSA1020 ! aerosol optical thickness and single scattering albedo at 440 nm
      real                                          :: Aexp870440 ! Angstrom exponent at 870/440 nm
      integer                                       :: iw, iw440, iw670, iw870, iw1020
      integer                                       :: ipix, I ! indices
! -----------------------------------------------------------------------------------------

! get indicies the needed vawelengths
    iw440=0
    iw670=0
    iw870=0
    iw1020=0
    do iw=1,RIN%NW
       if (RIN%wave(iw)-0.440 .lt. 0.010) iw440=iw ! exact wavelength are not expected close within 10 nm (0.01 um)
       if (RIN%wave(iw)-0.670 .lt. 0.010) iw670=iw
       if (RIN%wave(iw)-0.870 .lt. 0.010) iw870=iw
       if (RIN%wave(iw)-1.020 .lt. 0.010) iw1020=iw
    enddo
    if ((iw440 .eq. 0) .or. (iw670 .eq. 0) .or. (iw870 .eq. 0) .or. (iw1020 .eq. 0)) then
      write(tmp_message,'(a)') 'ERROR: data at 440, 670, 870 or 1020 nm are not present'
        G_ERROR(trim(tmp_message))
    endif ! not all needed wavelenght are present

    if (.not. GOUT%products%aerosol%sd2m_mph) then ! we already have everything calculated for fine and coarse mode
      write(tmp_message,'(a)') 'ERROR: sd2m aerosol product is not present'
        G_ERROR(trim(tmp_message))
    endif !sd2m_mph

!check if angstrom is calculated on right indicies print warning if not
!   write(*,*) 'RIN%Aexp_iwl(1:2)=', RIN%Aexp_iwl(1:2)
!   write(*,*) 'iw440, iw870=', iw440, iw870
!   if ((RIN%Aexp_iwl(1) .NE. iw440) .OR. (RIN%Aexp_iwl(2) .NE. iw870)) then
!      write(*,*) "WARNING!!! In get_aerosol_type: angstrom exponent wavelengt indicies are not corresponding to 440 and 870nm "
!      write(*,*) "Aerosol typing may be incorrect, check settings.yml"
!   endif

    do I=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(I)
        if(par_type .gt. par_type_SHD_beg  .and. par_type .lt. par_type_SHD_end) then
! unpack sphericity
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,I,npixels,sph )
        endif !par_type sphericity
    enddo !IDIM1

! TO DO get indicies of proper wl, make subroutine that halts if not present
    do ipix=1,npixels
    ! calculate VC_coarse/VC_fine ratio
        coarse_fine_ratio=GOUT%aerosol%sd2m%mph%pixel(ipix)%cv(2)/GOUT%aerosol%sd2m%mph%pixel(ipix)%cv(1)
!get spectrally mean aerosol refractive index
!calculating spectrally mean refractive index gettin direct access to GOUT since it is universal for chemistry and retrieved refr. index
        RERI_mean=0
        do I=1,RIN%NSD
           RERI_mean=RERI_mean+SUM(GOUT%aerosol%rind%pixel(ipix)%wl(iw670:iw1020)%mreal(I))/((iw1020-iw670+1))
        enddo
        RERI_mean=RERI_mean/RIN%NSD
! unpack AOD
        AOT440=GOUT%aerosol%opt%pixel(ipix)%wl(iw440)%extt
        AOT870=GOUT%aerosol%opt%pixel(ipix)%wl(iw870)%extt
! unpack SSA
        SSA440=GOUT%aerosol%opt%pixel(ipix)%wl(iw440)%ssat
        SSA1020=GOUT%aerosol%opt%pixel(ipix)%wl(iw1020)%ssat
! unpack Angstrom Exponent
!        Aexp870440=GOUT%aerosol%opt%pixel(ipix)%Aexp
        Aexp870440=-LOG(AOT870/AOT440)/LOG(870.0/440.0)
!        write(*,*) 'ipix, Aexp_gout, Aexp_int',GOUT%aerosol%opt%pixel(ipix)%Aexp, Aexp870440
! unpack sphericity
        sphericity=sph(1,1,ipix)
! perform classification
        aerosol_type_index(ipix)=0
        if (AOT440 .lt. 0.3) then
! background aerosol
           if ((Aexp870440 .gt. 0.6) .and. (Aexp870440 .lt. 1.6)) aerosol_type_index(ipix)=1 ! background aerosol
! maritime aerosol
           if ((Aexp870440 .gt. 0.4) .and. (Aexp870440 .lt. 1.2) .and. (RERI_mean .lt. 1.45) .and. (coarse_fine_ratio .gt. 1)&
              .and. (sphericity .ge. 0.45 ) .and. ((SSA1020-SSA440) .lt. 0.1)) aerosol_type_index(ipix)=2 ! maritime aerosol
        endif ! AOT440<0.3
        if (AOT440 .ge. 0.2) then
           if ((RERI_mean .gt. 1.4) .and. (SSA440 .lt. 0.94) .and. (coarse_fine_ratio .gt. 0.5)&
              .and. (coarse_fine_ratio .lt. 2.0)) then
! Urban polluted aerosol
              if ((Aexp870440 .ge. 1.2) .and. (Aexp870440 .le. 1.6)) aerosol_type_index(ipix)=3 ! Urban polluted aerosol
! mixed aerosol
              if ((Aexp870440 .ge. 0.6) .and. (Aexp870440 .lt. 1.2) .and. (sphericity .lt. 0.45 )) aerosol_type_index(ipix)=4 ! mixed aerosol
           elseif ((Aexp870440 .gt. 1.5) .and. (RERI_mean .ge. 1.45) .and. (coarse_fine_ratio .le. 1.0)  ) then
! smoke flaming
              if (SSA440 .lt. 0.93) then
                 aerosol_type_index(ipix)=7 ! smoke flaming
              else
! smoke smoldering
                 aerosol_type_index(ipix)=6 !smoke smoldering
              endif
           endif ! smoke/urban
        endif ! AOT>0.2
        if (AOT440 .ge. 0.1) then
! Urban clean aerosol
          if ((Aexp870440 .gt. 1.6) .and. (RERI_mean .lt. 1.45) .and. (SSA440 .ge. 0.94 ) .and. (coarse_fine_ratio .le. 0.5))&
           aerosol_type_index(ipix)=5 ! Urban clean aerosol
! Mineral dust
          if ((Aexp870440 .le. 0.8) .and. (RERI_mean .gt. 1.40) .and. (coarse_fine_ratio .ge. 4.0) .and. (sphericity .lt. 0.45))&
           aerosol_type_index(ipix)=8 ! Mineral dust
        endif ! AOT>0.1

    enddo !ipix
!    stop 'in get_aerosol_types'
      end subroutine get_aerosol_types

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_gout_pixel(  iu_main_output, RIN, segment_meas,  & ! IN
                                  ipix, nangles, sca_angles, AP,      &
                                  GOUT,                      & ! INOUT
                                  KERNELS1,KERNELS2          &
                               )

      use mod_par_DLS, only : KMpar
      use mod_par_inv, only : KPARS, KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_forward_model_characteristics
      use mod_alloc_kernels
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none
!	------------------------------------------------------------------------------------------------------
      integer,                       intent(in) :: iu_main_output
      integer,                       intent(in) :: ipix, nangles
      real,dimension(KMpar),         intent(in) :: sca_angles
      real,dimension(KPARS,KIMAGE),  intent(in) :: AP
      type(retr_input_settings),     intent(in) :: RIN
      type(segment_data),            intent(in) :: segment_meas
      type(output_segment_general),  intent(inout)  ::  GOUT
      type(kernels_triangle_bin),    intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),   intent(inout)  ::  KERNELS2
!	------------------------------------------------------------------------------------------------------
      type(forward_model_characteristics_particles) :: forw_aerosol
      type(forward_model_characteristics_gases) :: forw_gases
      type(forward_model_characteristics_surface  ) :: forw_surface
      integer	:: i, IW
      real :: temp
      real,dimension(KPARS) :: APSING
      real, parameter :: tiny = 1e-4
!	------------------------------------------------------------------------------------------------------
      select case(RIN%KL)
      case(1)
        APSING(1:RIN%KNSING) = EXP(AP(1:RIN%KNSING,ipix))
      case(0) 
        APSING(1:RIN%KNSING) =     AP(1:RIN%KNSING,ipix)
      end select

! Vector of retrieved parameters
      call normalize_vector_par_single_pixel(RIN,segment_meas,ipix,APSING(1:RIN%KNSING))
      GOUT%retrieval%par%pixel(ipix)%par(1:RIN%KNSING) = APSING(1:RIN%KNSING)

! Compute and set particles size distribution
      call set_gout_sd_pixel ( RIN, GOUT, ipix )

! Check if retrieved normalized parameters are in the range apsmin <= ap <= apsmax
      if(RIN%IPRI_additional_info) then
        do i=1,RIN%KNSINGF
          if ( GOUT%retrieval%par%pixel(ipix)%par(i) .lt. RIN%APSMIN(i) .or. &
               GOUT%retrieval%par%pixel(ipix)%par(i) .gt. RIN%APSMAX(i) ) then
            if(abs(GOUT%retrieval%par%pixel(ipix)%par(i)-EXP(AP(i,ipix)))/EXP(AP(i,ipix)) .gt. tiny) then
              write(iu_main_output,'(a,2(2x,i0),4e12.4,2x,a)') 'WARNING:',ipix,i, &
              RIN%APSMIN(i),EXP(AP(i,ipix)),GOUT%retrieval%par%pixel(ipix)%par(i), &
              RIN%APSMAX(i),'- ipix, i, APMIN, AP, AP_norm, APMAX, in set_gout_pixel'
            endif
          endif
        enddo
      end if

! Aerosol
      if(RIN%NSD .gt. 0) then
        !  Phase matrix scattering angles (kernel nodes)
        GOUT%aerosol%phmx%nangle   = nangles
        GOUT%aerosol%phmx%angle(:) = sca_angles(:)
        !  Total optical characteristics
        do IW=1,RIN%NW
          call total_single_scattering_properties ( iu_main_output, ipix, nangles,   & ! IN
                                                    RIN%DLSF%keyEL, RIN%NSD, RIN%products%aerosol, &
                                                    IW, RIN%WAVE(IW),                &
                                                    GOUT%aerosol,                    & ! INOUT
                                                    GOUT%products%aerosol            &
                                                  )
        enddo ! IW

        !  Angstrom exponent
        GOUT%aerosol%opt%pixel(ipix)%Aexp = 0.0
        if(RIN%Aexp_iwl(1) .ne. RIN%Aexp_iwl(2)) &
        GOUT%aerosol%opt%pixel(ipix)%Aexp = &
          -(LOG(GOUT%aerosol%opt%pixel(ipix)%wl(RIN%Aexp_iwl(1))%extt/ &
            GOUT%aerosol%opt%pixel(ipix)%wl(RIN%Aexp_iwl(2))%extt)/    &
            LOG(RIN%WAVE(RIN%Aexp_iwl(1))/RIN%WAVE(RIN%Aexp_iwl(2))))
        ! 2 theoretical modes
        if(RIN%products%aerosol%sd2m_mph .or. RIN%products%aerosol%sd2m_ext) then
          !if(RIN%use_models) then
            !write(tmp_message,'(a)') &
            !'Can not calculate theoretical 2 modes when phase matrix lut in use'
            !G_ERROR(trim(tmp_message))
          !endif
          call unpack_parameter_vector_ap( RIN, APSING, &
                                          forw_aerosol, &
                                          forw_gases,   &
                                          forw_surface, &
                                          ipix )
          !if(forw_aerosol%par_type_SD .ne. par_type_SD_LN) &

          call theoretical_modes_2 (  iu_main_output, ipix,            & ! IN
                                      RIN, RIN%NSD, RIN%products%aerosol, &
                                      forw_aerosol,                    &
                                      GOUT%retrieval,                  &
                                      GOUT%aerosol,                    & ! INOUT
                                      GOUT%products%aerosol,           &
                                      KERNELS1, KERNELS2               &
                                   )
          if ( error_present() ) return
        endif
      endif

! Surface
      !  NDVI
      GOUT%surface%pixel(ipix)%NDVI = 0.0
      if(RIN%ndvi_iwl(1) .ne. RIN%ndvi_iwl(2)) then
        ! ndvi = (surfalb865 - surfalb670) / (surfalb865 + surfalb670)
        temp = GOUT%surface%pixel(ipix)%wl(RIN%ndvi_iwl(2))%dhr + &
        GOUT%surface%pixel(ipix)%wl(RIN%ndvi_iwl(1))%dhr
        if(temp .gt. 0.0) GOUT%surface%pixel(ipix)%NDVI = &
                        ( GOUT%surface%pixel(ipix)%wl(RIN%ndvi_iwl(2))%dhr -  &
                          GOUT%surface%pixel(ipix)%wl(RIN%ndvi_iwl(1))%dhr )/temp
      endif ! RIN%ndvi_iwl(1) .ne. RIN%ndvi_iwl(2)

      return
      end subroutine set_gout_pixel

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine total_single_scattering_properties ( iu_main_output, ipix, NANG,        & ! IN
                                                      NEL, NSD, RIN_products_particles,  &
                                                      IW, WL,                            &
                                                      GOUT_particles,                    & ! INOUT
                                                      GOUT_products_particles            &
                                                    )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type

      implicit none
!	------------------------------------------------------------------------------------------------------
      integer,                       intent(in) :: iu_main_output
      integer,                       intent(in) :: NANG, NEL, NSD, IW, ipix
      real,                          intent(in) :: WL
      type(output_segment_products_particles), intent(in)    :: RIN_products_particles
      type(output_segment_particles),          intent(inout) :: GOUT_particles
      type(output_segment_products_particles), intent(inout) :: GOUT_products_particles
!	------------------------------------------------------------------------------------------------------
      integer	:: IAN, ISD
      real :: pi, sca
!	------------------------------------------------------------------------------------------------------
      PI = ACOS(-1.0)
         if(NEL .gt. 0) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht11(:) = 0.
         if(NEL .gt. 1) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht12(:) = 0.
         if(NEL .gt. 2) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht22(:) = 0.
         if(NEL .gt. 3) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht33(:) = 0.
         if(NEL .gt. 4) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht34(:) = 0.
         if(NEL .gt. 5) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht44(:) = 0.
         GOUT_particles%opt%pixel(ipix)%wl(IW)%extt  = 0.
         GOUT_particles%opt%pixel(ipix)%wl(IW)%ssat  = 0.
         GOUT_particles%opt%pixel(ipix)%wl(IW)%aextt = 0.

         do ISD=1,NSD
	   
! Save particle optical properties for output

         sca = GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(ISD)*GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(ISD)
         GOUT_particles%opt%pixel(ipix)%wl(IW)%aext(ISD) = GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(ISD) * &
                                                         (1.-GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(ISD))
         if(NEL .gt. 0) then
            GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht11(1:NANG) = GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht11(1:NANG) + &
                                                                   GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph11(1:NANG,ISD) * sca
            GOUT_particles%lidar%pixel(ipix)%wl(IW)%lr(ISD)= 4.*PI/  &
                                                                (GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(ISD)*  &
                                                                 GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph11(NANG,ISD))
         endif ! NEL .gt. 0 
         if(NEL .gt. 1) then
            GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht12(1:NANG) = GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht12(1:NANG) + &
                                                                   GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph12(1:NANG,ISD) * sca
         endif ! NEL .gt. 1
         if(NEL .gt. 2) then 
            GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht22(1:NANG) = GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht22(1:NANG) + &
                                                                   GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(1:NANG,ISD) * sca
!            GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldpar(ISD)=(1.-GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,ISD))/ &
!                                                            (1.+GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,ISD))*100.
!            GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldper(ISD)=(1.-GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,ISD))/ &
!            (1.+GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,ISD))*100.
            
!            GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldpar(ISD)=GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph11(NANG,ISD)*sca
!            GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldper(ISD)=GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,ISD)*sca
!            WRITE(*,*),'----total_single_scattering_properties-----'
!            WRITE(*,*),ISD,'ldpar, ldper = '
!            WRITE(*,*),GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldpar(ISD),GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldper(ISD)
!            WRITE(*,*),'-----------------------------------------------------'
!            WRITE(*,*),'Compute the total P11, P22 for fine and coarse mode: '
!            WRITE(*,*),'Mode',ISD
!            WRITE(*,*),'P11=',GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldpar(ISD),' ,P22=',&
!                            GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldper(ISD)

         endif ! NEL .gt. 2
         if(NEL .gt. 3) then
            GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht33(1:NANG) = GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht33(1:NANG) + &
                                                                   GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph33(1:NANG,ISD) * sca
         endif ! NEL .gt. 3
         if(NEL .gt. 4) then
            GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht34(1:NANG) = GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht34(1:NANG) + &
                                                                   GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph34(1:NANG,ISD) * sca
         endif ! NEL .gt. 4
         if(NEL .gt. 5) then
            GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht44(1:NANG) = GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht44(1:NANG) + &
                                                                   GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph44(1:NANG,ISD) * sca
         endif ! NEL .gt. 5
         enddo ! DO ISD=1,NSD	
                  
         GOUT_particles%opt%pixel(ipix)%wl(IW)%extt = SUM( GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(1:NSD) )
         GOUT_particles%opt%pixel(ipix)%wl(IW)%ssat = SUM( GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(1:NSD)*    &
                                            GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(1:NSD) )/  &
                                            GOUT_particles%opt%pixel(ipix)%wl(IW)%extt  
         GOUT_particles%opt%pixel(ipix)%wl(IW)%aextt = GOUT_particles%opt%pixel(ipix)%wl(IW)%extt * &
                                                     (1.-GOUT_particles%opt%pixel(ipix)%wl(IW)%ssat)
         sca = GOUT_particles%opt%pixel(ipix)%wl(IW)%ssat*GOUT_particles%opt%pixel(ipix)%wl(IW)%extt
         if(NEL .gt. 0) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht11(1:NANG) =  &
                        GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht11(1:NANG) / sca
         if(NEL .gt. 1) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht12(1:NANG) =  & 
                        GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht12(1:NANG) / sca
         if(NEL .gt. 2) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht22(1:NANG) =  & 
                        GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht22(1:NANG) / sca
         if(NEL .gt. 3) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht33(1:NANG) =  & 
                        GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht33(1:NANG) / sca
         if(NEL .gt. 4) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht34(1:NANG) =  & 
                        GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht34(1:NANG) / sca
         if(NEL .gt. 5) GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht44(1:NANG) =  & 
                        GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht44(1:NANG) / sca 

         if(NEL .gt. 0) GOUT_particles%lidar%pixel(ipix)%wl(IW)%lrt  = 4.*PI/  &
                              ( GOUT_particles%opt%pixel(ipix)%wl(IW)%ssat*GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht11(NANG) ) 

         if(NEL .gt. 2) GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldprt= &
                    (GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht11(NANG)-GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht22(NANG))/&
                    (GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht11(NANG)+GOUT_particles%phmx%pixel(ipix)%wl(IW)%pht22(NANG))*100.

      !DO ISD=1,NSD
      !WRITE(*,*) ISD,IW,RIN%WAVE(IW),'  ISD,IW,WL'
      !WRITE(*,*) ISD,GOUT_particles%opt%pixel(ipix)%wl(IW)%SSA(ISD),  &
      !               GOUT_particles%opt%pixel(ipix)%wl(IW)%EXT(ISD),'  ISD,SSA, EXT'
      !DO IAN=1,NANG
      !WRITE(*,'(i4,5e14.5)') ISD, &
      !GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph11(IAN,ISD),GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph12(IAN,ISD),  &
      !GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(IAN,ISD),GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph33(IAN,ISD)
      !ENDDO ! IAN
      !ENDDO ! ISD

      return
      end subroutine total_single_scattering_properties

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine theoretical_modes_2 ( iu_main_output, ipix,      & ! IN
                                       RIN, NSD, RIN_products_particles, &
                                       forw_particles,            &
                                       GOUT_retrieval,            &
                                       GOUT_particles,            & ! INOUT
                                       GOUT_products_particles,   &
                                       KERNELS1, KERNELS2         &
                                     )

      use mod_par_DLS, only : KMpar
      use mod_par_inv, only : KIDIM3
      use mod_par_OS,  only : KSD,N_SUB_CHANNEL_MAX
      use mod_par_DLS_bin, only : NRR
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_forward_model_characteristics
      use mod_alloc_kernels
      use mod_stop_report

      implicit none
!	------------------------------------------------------------------------------------------------------
      integer,intent(in) :: iu_main_output
      integer,intent(in) :: ipix, NSD
      type(retr_input_settings),intent(in) :: RIN
      type(output_segment_retrieval),intent(in) :: GOUT_retrieval
      type(output_segment_products_particles),intent(in) :: RIN_products_particles
      type(forward_model_characteristics_particles),intent(in) :: forw_particles
      type(output_segment_particles),intent(inout) :: GOUT_particles
      type(output_segment_products_particles),intent(inout) :: GOUT_products_particles
      type(kernels_triangle_bin),intent(inout) :: KERNELS1
      type(kernels_lognormal_bin),intent(inout) :: KERNELS2
!	------------------------------------------------------------------------------------------------------
      integer	:: II, ISD, IW, NWL, ierr
      real, dimension(KIDIM3,2)  ::	SDFC
      real, dimension(KSD) ::	RREAL, RIMAG
      integer, dimension(KSD) :: NBIN
      integer :: IMIN
      real, dimension(KW) :: WAVE
      real :: ssa_tmp, ext_tmp, sd2m_ext_fine, sd2m_ext_coarse, cext_tmp
      real :: reri_tmp, imri_tmp
      integer :: nangles
      real, dimension(:), allocatable :: sca_angles, ph11, ph12, ph22, ph33, ph34, ph44
      logical :: IPRI_additional_info
      integer :: ngrid_lb
      real, dimension(NRR) :: radius_lb
      logical :: IPRI_verbose
!	------------------------------------------------------------------------------------------------------
     integer                                :: NSubchannels = 1
     real, dimension(N_SUB_CHANNEL_MAX)     :: bandwidth = 1.0
     real,dimension(N_SUB_CHANNEL_MAX)      :: wl_Subchannels = 0.0
     real,dimension(KSD,N_SUB_CHANNEL_MAX)  :: RREAL_Subchannels,RIMAG_Subchannels
!    ------------------------------------------------------------------------------------------------------
      NWL = RIN%NW
      WAVE(:) = RIN%WAVE(:)
      NBIN(1:NSD) = forw_particles%NBIN(1:NSD)

      select case(NSD)
      case(1)
! Microphysical and optical properties for fine and coarse modes
          allocate(sca_angles(1:KMpar),stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to allocate sca_angles(1:KMpar))'
            G_ERROR(trim(tmp_message))
          endif
          allocate(ph11(1:KMpar),stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to allocate ph11(1:KMpar))'
            G_ERROR(trim(tmp_message))
          endif
          allocate(ph12(1:KMpar),stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to allocate ph12(1:KMpar))'
            G_ERROR(trim(tmp_message))
          endif
          allocate(ph22(1:KMpar),stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to allocate ph22(1:KMpar))'
            G_ERROR(trim(tmp_message))
          endif
          allocate(ph33(1:KMpar),stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to allocate ph33(1:KMpar))'
            G_ERROR(trim(tmp_message))
          endif
          allocate(ph34(1:KMpar),stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to allocate ph34(1:KMpar))'
            G_ERROR(trim(tmp_message))
          endif
          allocate(ph44(1:KMpar),stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to allocate ph44(1:KMpar))'
            G_ERROR(trim(tmp_message))
          endif

          IPRI_additional_info = RIN%IPRI_additional_info
          if(forw_particles%par_type_SD .ne. par_type_SD_MD) then
          if(forw_particles%par_type_SD .ne. par_type_SD_LN) then
          if( forw_particles%par_type_SD .eq. par_type_SD_LB ) then
            call sd2m_min_index_LB (iu_main_output, IPRI_additional_info, forw_particles%NBIN(1), &
                                    forw_particles%RADIUS(1:forw_particles%NBIN(1),1), IMIN )
          endif
          if( forw_particles%par_type_SD .eq. par_type_SD_TB ) then
            call sd2m_min_index_TB (iu_main_output, IPRI_additional_info, forw_particles%NBIN(1), &
                                    forw_particles%RADIUS(1:forw_particles%NBIN(1),1), &
                                    forw_particles%SD(1:forw_particles%NBIN(1),1), IMIN )
          endif
          ngrid_lb = GOUT_retrieval%information%ngrid(1)
          radius_lb(1:ngrid_lb) = GOUT_retrieval%information%radius(1:ngrid_lb,1)
          call sd2m_fine_coarse ( IMIN,forw_particles%par_type_SD,forw_particles%NBIN(1),  &
                                  forw_particles%RADIUS(:,1),forw_particles%SD(:,1),       &
                                  ngrid_lb, radius_lb(:),                   &
                                  GOUT_retrieval%information%sd_lb(:,:),    &
                                  SDFC(:,:),                                &
                                  GOUT_particles%sd2m%mph%pixel(ipix)%cv,   &
                                  GOUT_particles%sd2m%mph%pixel(ipix)%std,  &
                                  GOUT_particles%sd2m%mph%pixel(ipix)%rm,   &
                                  GOUT_particles%sd2m%mph%pixel(ipix)%reff  &
                                )
          if ( error_present() ) return
          do ISD=1,2
            do IW=1,NWL
              WL_Subchannels = WAVE(IW)
              call get_REFI_wl ( RIN,                                       &
                                 IW,IW,                                     &
                                 Nsubchannels,                              &
                                 forw_particles%RH,                         &
                                 bandwidth,                                 &
                                 WL_Subchannels,                            &
                                 GOUT_particles%chem%pixel(ipix),           &
                                 forw_particles%RREAL,forw_particles%RIMAG, &
                                 RREAL_Subchannels,RIMAG_Subchannels,       &
                                 RREAL,RIMAG )

              call PHASE_KERNEL_W ( iu_main_output,           & ! IN
                                    RIN%IPRI_verbose,         &
                                    RIN%IPRI_additional_info, &
                                    forw_particles%NBIN(1),forw_particles%RADIUS(:,1),SDFC(:,ISD),  & ! IN
                                    RIN%KNLN,                          &
                                    forw_particles%NSHAPE(1),forw_particles%RATIO(:,1),forw_particles%SHD(:,1), &
                                    IW,WAVE(IW),RREAL(1),RIMAG(1),     &
                                    nangles,sca_angles,RIN%DLSF,       & ! OUT
                                    ssa_tmp,                           &
                                    GOUT_particles%sd2m%opt%pixel(ipix)%wl(IW)%ext(ISD),  &
                                    ph11(:), ph12(:), ph22(:),         &
                                    ph33(:), ph34(:), ph44(:),         &
                                    KERNELS1, KERNELS2                 &
                                  )
            if ( error_present() ) return
            enddo ! IW
          enddo ! ISD=1,2
          endif
        else
#ifdef GRASP_MODELS
          IPRI_verbose = RIN%IPRI_verbose
          do IW=1,NWL
            GOUT_particles%sd2m%opt%pixel(ipix)%wl(IW)%ext(:) = 0.0
            do ISD=1,NSD
              call phmx_models_wl(                                &
                              IPRI_verbose,                       &
                              RIN%DLSF, IW, WAVE(IW),             &
                              NSD, ISD, NBIN,                     &
                              forw_particles%SD(1:NBIN(ISD),ISD), &
                              nangles, sca_angles,                &
                              reri_tmp, imri_tmp,                 &
                              ssa_tmp, ext_tmp,                   &
                              sd2m_ext_fine, sd2m_ext_coarse,     &
                              cext_tmp,                           &
                              ph11(:), ph12(:), ph22(:),          &
                              ph33(:), ph34(:), ph44(:),          &
                              KERNELS2                            &
                                 )
              if(error_present() .eqv. .true.) return
              GOUT_particles%sd2m%opt%pixel(ipix)%wl(IW)%ext(1) = &
              GOUT_particles%sd2m%opt%pixel(ipix)%wl(IW)%ext(1) + sd2m_ext_fine
              GOUT_particles%sd2m%opt%pixel(ipix)%wl(IW)%ext(2) = &
              GOUT_particles%sd2m%opt%pixel(ipix)%wl(IW)%ext(2) + sd2m_ext_coarse
            enddo ! ISD
          enddo ! IW
#endif
        endif ! forw_particles%par_type_SD .ne. par_type_SD_MD

          deallocate(sca_angles,stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to deallocate sca_angles'
            G_ERROR(trim(tmp_message))
          endif
          deallocate(ph11,stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to deallocate ph11'
            G_ERROR(trim(tmp_message))
          endif
          deallocate(ph12,stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to deallocate ph12'
            G_ERROR(trim(tmp_message))
          endif
          deallocate(ph22,stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to deallocate ph22'
            G_ERROR(trim(tmp_message))
          endif
          deallocate(ph33,stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to deallocate ph33'
            G_ERROR(trim(tmp_message))
          endif
          deallocate(ph34,stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to deallocate ph34'
            G_ERROR(trim(tmp_message))
          endif
          deallocate(ph44,stat=ierr)
          if (ierr /= 0) then
            write(tmp_message,'(a)') 'Error while trying to deallocate ph44'
            G_ERROR(trim(tmp_message))
          endif

      case(2)
! Effective radii for total, fine and coarse modes
        if ( forw_particles%par_type_SD .ne. par_type_SD_MD ) then
          call sd2m_reff_2modes_retrieval (  ipix, GOUT_retrieval,   &
                                            GOUT_particles%sd2m%mph%pixel(ipix)%reff &
                                         )
        endif
      case default
        write(tmp_message,'(a,i0,5a)') 'Number of modes NSD = ',NSD,' > 2 is not supported.', &
        NEW_LINE('A'), &
        'Possible solution:', &
        NEW_LINE('A'), &
        'in input settings file products.aerosol.theoretical_bimodal_parameters: false'
        G_ERROR(trim(tmp_message))
      end select ! NSD

      if ( forw_particles%par_type_SD .ne. par_type_SD_MD ) &
      GOUT_products_particles%sd2m_mph = .true.
      GOUT_products_particles%sd2m_ext = .true.

      return
      end subroutine theoretical_modes_2

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Set Generalized OUTput structure with Standardized Size Distribution

      subroutine set_gout_sd_pixel ( RIN, GOUT, ipix )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)     ::  ipix
      type(retr_input_settings),    intent(in)     ::  RIN
      type(output_segment_general), intent(inout)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer  :: i, IDIM1
      integer  :: par_type
!----------------------------------------------------------------------------------------
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then
          select case(par_type)
          case(par_type_SD_TB)
            call set_gout_sd_TB_pixel ( RIN, GOUT, IDIM1, ipix )
          case(par_type_SD_LB)
            call set_gout_sd_LB_pixel ( RIN, GOUT, IDIM1, ipix )
          case(par_type_SD_LN)
            call set_gout_sd_LN_pixel ( RIN, GOUT, IDIM1, ipix )
          end select
        exit
        endif ! par_type .gt. par_type_SD_beg .and.
      enddo ! IDIM1

      return
      end subroutine set_gout_sd_pixel

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Set Generalized OUTput structure with Size Distribution retrieved
! for Triangle bins SD model

      subroutine set_gout_sd_TB_pixel ( RIN, GOUT, IDIM1, ipix )

      use mod_par_inv, only : KIDIM2, KIDIM3, KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_intrpl_linear
      use mod_stop_report

      implicit none
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)     ::  IDIM1, ipix
      type(retr_input_settings),    intent(in)     ::  RIN
      type(output_segment_general), intent(inout)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: IDIM2, NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: SD, CV
      real,dimension(4*KIDIM3) :: radius, radius_tmp, sd_tmp
      integer :: i, i0
      integer :: n, ngrid, npixels, nsd, par_type
      logical :: icv
!----------------------------------------------------------------------------------------
      npixels = GOUT%retrieval%information%npixels
      nsd = RIN%NDIM%n2(IDIM1)
! Unpack retrieved SD
      call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,SD )

! Presence of volume concentration characteristic in settings
      icv = .false.
      do i=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(i)
        if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,i,npixels,CV )
          icv = .true.
        exit
        endif
      enddo ! i

! Set SD for particle components
      if ( .not. icv ) then
        do IDIM2=1,nsd    ! particle component loop
        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
        do i=1,NDIM3
          GOUT%retrieval%par%pixel(ipix)%sd(i,IDIM2) = SD(i,IDIM2,ipix)
        enddo ! i
        enddo ! IDIM2
      else
        do IDIM2=1,nsd    ! particle component loop
          NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
          if ( RIN%indep_par ) then
          NDIM3 = NDIM3+1
          call set_all_bins ( NDIM3, SD(1:NDIM3,IDIM2,ipix) )
          endif
          SD(1:NDIM3,IDIM2,ipix) = CV(1,IDIM2,ipix) * SD(1:NDIM3,IDIM2,ipix) / &
                 ( LOG(RIN%RADIUS1(2,IDIM2))-LOG(RIN%RADIUS1(1,IDIM2)) )
          do i=1,NDIM3
            GOUT%retrieval%par%pixel(ipix)%sd(i,IDIM2) = SD(i,IDIM2,ipix)
          enddo ! i
        enddo ! IDIM2
      endif ! .not. icv

! Set total SD
      ngrid = GOUT%retrieval%information%ngrid(0)
      radius(1:ngrid) = GOUT%retrieval%information%radius(1:ngrid,0)
      GOUT%retrieval%par%pixel(ipix)%sd(:,0) = 0.0
      do IDIM2=1,nsd
        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
        if ( RIN%indep_par .and. RIN%flag_plus ) then
        NDIM3 = NDIM3 + 1
        endif
        radius_tmp(1:NDIM3) = GOUT%retrieval%information%radius(1:NDIM3,IDIM2)
        sd_tmp(1:NDIM3) = GOUT%retrieval%par%pixel(ipix)%sd(1:NDIM3,IDIM2)
        do i0=1,ngrid
        do i=1,NDIM3
          if( (radius(i0) .gt. radius_tmp(NDIM3)) .or. &
              (radius(i0) .lt. radius_tmp(1))   ) exit
          if(abs(radius(i0)-radius_tmp(i))/radius(i0) .le. 1e-3) then
            GOUT%retrieval%par%pixel(ipix)%sd(i0,0) = &
            GOUT%retrieval%par%pixel(ipix)%sd(i0,0) + &
            GOUT%retrieval%par%pixel(ipix)%sd(i,IDIM2)
            exit
          elseif(radius(i0) .lt. radius_tmp(i)) then
            if(radius(i0) .gt. radius_tmp(1)) then
            if(radius(i0) .lt. radius_tmp(NDIM3)) then
            GOUT%retrieval%par%pixel(ipix)%sd(i0,0) = &
            GOUT%retrieval%par%pixel(ipix)%sd(i0,0) + &
            LINEAR(log(radius_tmp(1:NDIM3)),sd_tmp(1:NDIM3),NDIM3,log(radius(i0)))
            exit
            endif
            endif
          endif
          enddo ! i
          enddo ! i0
      enddo ! IDIM2

      return
      end subroutine set_gout_sd_TB_pixel

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Set Generalized OUTput structure with Size Distribution rtrieved
! for Precomputed Lognormal Bins SD model

      subroutine set_gout_sd_LB_pixel ( RIN, GOUT, IDIM1, ipix )

      use mod_par_inv, only : KIDIM2, KIDIM3, KIMAGE
      use mod_par_DLS_bin, only : NRR
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_stop_report

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)    ::  IDIM1, ipix
      type(retr_input_settings),    intent(in)    ::  RIN
      type(output_segment_general), intent(inout) ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: IDIM2, NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: SD, CV
      integer :: i
      real,dimension(NRR,KIDIM2,KIMAGE) :: SDout
      integer :: npixels, ngrid, par_type
      logical :: icv
!----------------------------------------------------------------------------------------
      npixels = GOUT%retrieval%information%npixels

      icv = .false.
      do i=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(i)
        if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,i,npixels,CV )
          icv = .true.
        exit
        endif ! par_type .gt. par_type_Cv_beg .and.
      enddo ! i

      call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,SD )

      GOUT%retrieval%par%pixel(ipix)%sd(:,0) = 0.0
      if ( .not. icv ) then
!write(*,'(a,i0,a)') 'ipix = ',ipix,'  - in set_gout_sd_LB_pixel'
        call SD_precalcul_ln_bins (  RIN,GOUT%retrieval%information,npixels,IDIM1,SD,SDout )
        do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! particle component loop
        ngrid = GOUT%retrieval%information%ngrid(IDIM2)
        do i=1,ngrid
        GOUT%retrieval%par%pixel(ipix)%sd(i,IDIM2) = &
                                  SDout(i,IDIM2,ipix)
        GOUT%retrieval%par%pixel(ipix)%sd(i,0) = &
        GOUT%retrieval%par%pixel(ipix)%sd(i,0) + SDout(i,IDIM2,ipix)
        enddo ! i
        enddo ! IDIM2
      else
        do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! particle component loop
        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
        SD(1:NDIM3+1,IDIM2,1:npixels) = 0.0
        call set_all_bins ( NDIM3+1, SD(1:NDIM3+1,IDIM2,ipix) )
        SD(1:NDIM3+1,IDIM2,ipix) = SD(1:NDIM3+1,IDIM2,ipix) * CV(1,IDIM2,ipix)
        call SD_precalcul_ln_bins (  RIN,GOUT%retrieval%information,npixels,IDIM1,SD,SDout )
        ngrid = GOUT%retrieval%information%ngrid(IDIM2)
        do i=1,ngrid
        GOUT%retrieval%par%pixel(ipix)%sd(i,IDIM2) = SDout(i,IDIM2,ipix)
        GOUT%retrieval%par%pixel(ipix)%sd(i,0) = &
        GOUT%retrieval%par%pixel(ipix)%sd(i,0) + SDout(i,IDIM2,ipix)
        enddo ! i
        enddo ! IDIM2
      endif

      return
      end subroutine set_gout_sd_LB_pixel

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Set Generalized OUTput structure with Size Distribution etrieved
! for LogNormal SD model


      subroutine set_gout_sd_LN_pixel ( RIN, GOUT, IDIM1, ipix )

      use mod_par_inv, only : KIDIM2, KIDIM3, KIMAGE
      use mod_par_DLS, only : KNpar
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_stop_report

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)    :: IDIM1, ipix
      type(retr_input_settings),    intent(in)    :: RIN
      type(output_segment_general), intent(inout) :: GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: IDIM2, NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: SD, CV
      real,dimension(KNpar,KIDIM2,KIMAGE) :: SDout
      integer :: i, npixels, par_type
!----------------------------------------------------------------------------------------
      npixels  = GOUT%retrieval%information%npixels

      do i=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(i)
        if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,i,npixels,CV )
        exit
        endif ! par_type .gt. par_type_Cv_beg .and.
      enddo ! i

      call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,SD )
      call SD_lognormal ( RIN,GOUT%retrieval%information,ipix,IDIM1,SD,CV,SDout )

      GOUT%retrieval%par%pixel(ipix)%sd(:,0) = 0.0
      do IDIM2=1,RIN%NDIM%n2(IDIM1)  ! particle component loop
      do i=1,GOUT%retrieval%information%ngrid(IDIM2)
        GOUT%retrieval%par%pixel(ipix)%sd(i,IDIM2) = SDout(i,IDIM2,ipix)
        GOUT%retrieval%par%pixel(ipix)%sd(i,0) = &
        GOUT%retrieval%par%pixel(ipix)%sd(i,0) +     SDout(i,IDIM2,ipix)
      enddo ! i
      enddo ! IDIM2

      return
      end subroutine set_gout_sd_LN_pixel

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine sets Generalized OUTput structure for segment with errors
        !> @brief of Size Distribution retrieved for Triangle Bins SD model
        !>
        !> @param[in]     iu_main_output - unit number of main output file
        !> @param[in]     RIN - settings structure
        !> @param[inout]  GOUT - general output structure
        !>

      subroutine set_gout_sd_err_estim_segment ( iu_main_output, RIN, GOUT )

      use mod_par_inv, only : KIDIM2, KIDIM3, KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_intrpl_linear
      use mod_stop_report

      implicit none
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)     ::  iu_main_output
      type(retr_input_settings),    intent(in)     ::  RIN
      type(output_segment_general), intent(inout)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: IDIM1, ipix, i
      integer :: npixels, par_type, par_type_SD
!----------------------------------------------------------------------------------------
      npixels = GOUT%retrieval%information%npixels

! Presence of Triangle Bins Size Distribution particle model
      par_type_SD = 0
      do IDIM1=1,RIN%NDIM%n1
        par_type_SD = RIN%NDIM%par_type(IDIM1)
        if ( par_type_SD .eq. par_type_SD_TB ) then
          exit
        endif ! par_type .gt. par_type_TB_beg .and.
      enddo ! i

      if ( par_type_SD .ne. par_type_SD_TB ) then
        return
      else
        ! Presence of volume concentration characteristic in settings
        do i=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(i)
          if ( par_type .gt. par_type_Cv_beg ) then
          if ( par_type .lt. par_type_Cv_end ) then
          if ( RIN%IPRI_verbose .eqv. .true. ) then
            write(iu_main_output,'(a,/,a)') &
            'Error bars for Triangle Bins of SD model are not provided', &
            'if particle concenration is a retrieved caracteristic.'
          endif
          return
          endif
          endif
        enddo ! i
        do ipix=1,npixels
          call set_gout_sd_TB_err_est_pixel ( RIN, GOUT, IDIM1, ipix )
        enddo
      endif ! par_type_SD .ne. par_type_TB

      return
      end subroutine set_gout_sd_err_estim_segment

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine sets Generalized OUTput structure for single pixel with errors
        !> @brief of Size Distribution retrieved for Triangle Bins SD model
        !>
        !> @param[in]     RIN - settings structure
        !> @param[in]     IDIM1 - index of Triangle Bins SD characteristic in settings
        !> @param[in]     ipix - pixxel index
        !> @param[inout]  GOUT - general output structure
        !>

      subroutine set_gout_sd_TB_err_est_pixel ( RIN, GOUT, IDIM1, ipix )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_stop_report

      implicit none
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)     ::  IDIM1, ipix
      type(retr_input_settings),    intent(in)     ::  RIN
      type(output_segment_general), intent(inout)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: IDIM2, NDIM3, i, ibeg, ipar
      integer :: npixels
!----------------------------------------------------------------------------------------
      npixels = GOUT%retrieval%information%npixels

      do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! particle component loop
        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
        ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
        do i=1,NDIM3
          ipar = ibeg+i-1
          GOUT%errest%par%pixel(ipix)%sd_err(i,IDIM2) = GOUT%errest%par%pixel(ipix)%TSTDP(ipar)
        enddo ! i
      enddo ! IDIM2

      return
      end subroutine set_gout_sd_TB_err_est_pixel

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
    subroutine set_gout_particles_parameters( RIN, forw_particles, &
                                              GOUT_retrieval_par_pixel)

      use mod_retr_settings_derived_type, only : retr_input_settings
      use mod_retr_general_output_derived_type, only : output_pixel_retr_par
      use mod_forward_model_characteristics, only : forward_model_characteristics_particles
      use mod_par_type_aerosol, only : par_type_SD_TB, par_type_SD_LB, &
                                      par_type_SD_MD, par_type_SHD_distr, &
                                      par_type_AVP_prof
      use mod_stop_report

      implicit none
! ..................................................................................
      type(retr_input_settings), intent(in) :: RIN
      type(forward_model_characteristics_particles), intent(in) :: forw_particles
      type(output_pixel_retr_par), intent(inout) :: GOUT_retrieval_par_pixel
! ..................................................................................
      integer :: idim1, isd, i
      integer :: par_type, nsd, n
! ..................................................................................
! Parameters of particles

      nsd = RIN%NSD

      do idim1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(idim1)

        select case ( par_type )
        case ( par_type_SD_TB )
          do isd=1,nsd
          n = forw_particles%NBIN(isd)
          do i=1,n
          GOUT_retrieval_par_pixel%Cv(i,isd) = forw_particles%SD(i,isd)
          enddo
          enddo
        case ( par_type_SD_LB )
          do isd=1,nsd
          n = forw_particles%NBIN(isd)
          do i=1,n
          GOUT_retrieval_par_pixel%Cv(i,isd) = forw_particles%SD(i,isd)
          enddo
          enddo
        case ( par_type_SD_MD )
          do isd=1,nsd
          n = forw_particles%NBIN(isd)
          do i=1,n
          GOUT_retrieval_par_pixel%Cv(i,isd) = forw_particles%SD(i,isd)
          enddo
          enddo
        case ( par_type_SHD_distr )
          do isd=1,nsd
          n = forw_particles%NBIN(isd)
          do i=1,n
          GOUT_retrieval_par_pixel%ShD(i,isd) = forw_particles%SHD(i,isd)
          enddo
          enddo
        case ( par_type_AVP_prof )
          do isd=1,nsd
          n = forw_particles%NHVP_retr
          do i=1,n
          GOUT_retrieval_par_pixel%VD(i,isd) = forw_particles%H0(i,isd)
          enddo
          enddo
        end select
      enddo

    return
    end subroutine set_gout_particles_parameters

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
    subroutine set_gout_surface_parameters( ind_wl, BRF_land, BRP_land, BRM_water, &
                                                GOUT_retrieval_par_pixel)

      use mod_retr_general_output_derived_type, only : output_pixel_retr_par
      use mod_par_inv, only : KBF
      use mod_stop_report

      implicit none
! ..................................................................................
      integer, intent(in) :: ind_wl
      real,dimension(KBF), intent(in) :: BRF_land, BRP_land, BRM_water
      type(output_pixel_retr_par), intent(inout) :: GOUT_retrieval_par_pixel
! ..................................................................................
      GOUT_retrieval_par_pixel%BRF(ind_wl,:) = BRF_land(:)
      GOUT_retrieval_par_pixel%BRP(ind_wl,:) = BRP_land(:)
      GOUT_retrieval_par_pixel%BRM(ind_wl,:) = BRM_water(:)

    return
    end subroutine set_gout_surface_parameters

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Set Generalized OUTput structure with number of surface parameters
      subroutine set_gout_info_surface ( RIN, GOUT )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      type(retr_input_settings),    intent(in)    :: RIN
      type(output_segment_general), intent(inout) :: GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: idim1, idim2
      integer :: par_type
!----------------------------------------------------------------------------------------
      do idim1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(idim1)
        if(par_type .gt. par_type_SURF1_land_beg .and. &
           par_type .lt. par_type_SURF1_land_end) then
          GOUT%retrieval%information%nbrf = RIN%NDIM%n2(idim1)
        elseif(par_type .gt. par_type_SURF2_land_beg .and. &
               par_type .lt. par_type_SURF2_land_end) then
          GOUT%retrieval%information%nbrp = RIN%NDIM%n2(idim1)
        elseif(par_type .gt. par_type_SURF_water_beg .and. &
               par_type .lt. par_type_SURF_water_end) then
          GOUT%retrieval%information%nbrm = RIN%NDIM%n2(idim1)
        endif
      enddo ! idim1

      return
      end subroutine set_gout_info_surface

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
    subroutine set_gout_cutoff_single_scattering_properties( ipix, ind_wl, i, nsd, nangles, &
                                                             GOUT_particles_in,  &
                                                             GOUT_particles_out)

      use mod_retr_general_output_derived_type
      use mod_stop_report

      implicit none
! ..................................................................................
      type(output_segment_particles), intent(in) :: GOUT_particles_in
      integer, intent(in) :: ipix, ind_wl, i, nsd, nangles
      type(output_segment_particles), intent(inout) :: GOUT_particles_out
! ..................................................................................
      real :: sca(0:nsd)
      integer :: isd, ia
! ..................................................................................

      GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ext_cut_off(0:nsd,i) = 0.
      GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ssa_cut_off(0:nsd,i) = 0.

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph11_cut_off(:,0:nsd,i) = 0.
      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph12_cut_off(:,0:nsd,i) = 0.
      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph22_cut_off(:,0:nsd,i) = 0.
      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph33_cut_off(:,0:nsd,i) = 0.
      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph34_cut_off(:,0:nsd,i) = 0.
      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph44_cut_off(:,0:nsd,i) = 0.

! Set cut off single scattering optical properties for particle components
      GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ext_cut_off(1:nsd,i) = &
      GOUT_particles_in%opt%pixel(ipix)%wl(ind_wl)%ext(1:nsd)

      GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ssa_cut_off(1:nsd,i) = &
      GOUT_particles_in%opt%pixel(ipix)%wl(ind_wl)%ssa(1:nsd)
!
      do ia=1,nangles
      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph11_cut_off(ia,1:nsd,i) = &
      GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph11(ia,1:nsd)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph12_cut_off(ia,1:nsd,i) = &
      GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph12(ia,1:nsd)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph22_cut_off(ia,1:nsd,i) = &
      GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph22(ia,1:nsd)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph33_cut_off(ia,1:nsd,i) = &
      GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph33(ia,1:nsd)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph34_cut_off(ia,1:nsd,i) = &
      GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph34(ia,1:nsd)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph44_cut_off(ia,1:nsd,i) = &
      GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph44(ia,1:nsd)
      enddo

! Set total cut off single scattering optical properties
      GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ext_cut_off(0,i) = &
      sum(GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ext_cut_off(1:nsd,i))

      sca(1:nsd) = GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ssa_cut_off(1:nsd,i) * &
                   GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ext_cut_off(1:nsd,i)
      sca(0) = sum(sca(1:nsd))

      GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ssa_cut_off(0,i) = &
      sca(0)/GOUT_particles_out%opt%pixel(ipix)%wl(ind_wl)%ext_cut_off(0,i)

      do ia=1,nangles
      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph11_cut_off(ia,0,i) = &
      sum(sca(1:nsd)*GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph11(ia,1:nsd)) / sca(0)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph12_cut_off(ia,0,i) = &
      sum(sca(1:nsd)*GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph12(ia,1:nsd)) / sca(0)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph22_cut_off(ia,0,i) = &
      sum(sca(1:nsd)*GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph22(ia,1:nsd)) / sca(0)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph33_cut_off(ia,0,i) = &
      sum(sca(1:nsd)*GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph33(ia,1:nsd)) / sca(0)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph34_cut_off(ia,0,i) = &
      sum(sca(1:nsd)*GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph34(ia,1:nsd)) / sca(0)

      GOUT_particles_out%phmx%pixel(ipix)%wl(ind_wl)%ph44_cut_off(ia,0,i) = &
      sum(sca(1:nsd)*GOUT_particles_in%phmx%pixel(ipix)%wl(ind_wl)%ph44(ia,1:nsd)) / sca(0)
      enddo

    return
    end subroutine set_gout_cutoff_single_scattering_properties

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


