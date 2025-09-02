#include "../constants_set/mod_globals.inc"
! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! file contains :

! subroutine unpack_parameter_vector_ap
! subroutine get_SURF_wl
! subroutine get_REFI_wl
! subroutine spectral_characteristics_wl
! subroutine get_SD_bins_concentration
! subroutine get_AVP_altitudes_retr
! subroutine unpack_spectral_SURF
! subroutine unpack_SURF
! subroutine unpack_C0
! subroutine unpack_SHD
! subroutine unpack_REFI
! subroutine unpack_RH
! subroutine unpack_AVP
! subroutine unpack_lidar_calibr_coeff
! subroutine unpack_SD
! subroutine unpack_AVP_std
! subroutine normalize_vector_par_single_pixel
! subroutine get_SD_normalized
! subroutine compute_distribution
! subroutine set_vd_all_retr_altitudes
! subroutine set_all_bins
!
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

module mod_forward_model_characteristics

    use mod_par_inv, only : KW,KSHAPE,KBF,KIDIM2,KIDIM3,KVERTM       
    use mod_par_OS,  only : KSD, NMG

    implicit none

!!PL Example of particle component type definition for future development	
!    type :: forward_model_characteristics_component 
!! forward_model_parameters --  type contains all perticle caracteristics driving forward model
!      integer                       ::  par_type_SD
!      real,dimension(KIDIM3,KIDIM2) ::	RIMAG,RREAL  
!      real,dimension(KVERTM)    ::  H0
!      real           			::  C0,sigma
!      integer			        ::  NBIN
!      !real,dimension(KPARS,KSD)	   ::	RADIUS,SD
!      real,dimension(KIDIM3)	::	RADIUS,SD
!      integer				    ::  NSHAPE
!      real,dimension(KSHAPE)	::	RATIO,SHD
!      integer                   ::  NHVP_retr
!      real,dimension(KVERTM)    ::  HVP_retr_km
!      real,dimension(KW)        ::  CL 
!    end type forward_model_characteristics_component


    type :: forward_model_characteristics_particles 
! forward_model_parameters --  type contains all perticle caracteristics driving forward model
      integer                       ::  par_type_SD
      logical :: calculate_rreal, calculate_rimag
      real,dimension(KIDIM3,KIDIM2) ::	RIMAG,RREAL
      real,dimension(KVERTM,KSD)    ::  H0
      real,dimension(KSD)           ::  C0,sigma
      integer,dimension(KSD)        ::  NBIN
      !real,dimension(KPARS,KSD)	 ::  RADIUS,SD
      real,dimension(KIDIM3,KSD)	::  RADIUS,SD
      integer,dimension(KSD)	    ::  NSHAPE
      real,dimension(KSHAPE,KSD)	::  RATIO,SHD
      integer                       ::  NHVP_retr
      real,dimension(KVERTM)        ::  HVP_retr_km
      real,dimension(KW)            ::  CL
      real,dimension(KSD)           :: RH
    end type forward_model_characteristics_particles

    type :: forward_model_characteristics_gases
! forward_model_parameters --  type contains gas concentrations driving forward model
      integer :: n ! namber of gases provided in settings
      real,dimension(NMG)           ::  C ! array of gas concentrations
    end type forward_model_characteristics_gases

    type :: forward_model_characteristics_surface 
! forward_model_parameters --  type contains all surface caracteristics driving forward model
      real,dimension(KIDIM3,KIDIM2) :: 	BRF_land,BRP_land,BRM_water
    end type forward_model_characteristics_surface

contains   

!!PL Continue example with components
!      subroutine initialize_forward_model_characteristics_components(forw_components)
!
!		type(forward_model_characteristics_component),dimension(KSD_clouds)  ::  forw_components
!
!        forw_components(:)%par_type_SD = 0      
!        forw_components(:)%NBIN     = 0 
!        forw_components(:)%RADIUS(:)= 0.0      
!        forw_components(:)%SD(:)    = 0.0
!        forw_components(:)%C0       = 0.0
!            
!        forw_components(:)%NSHAPE  	= 0       
!        forw_components(:)%RATIO(:) = 0.0
!        forw_components(:)%SHD(:)   = 0.0
!      
!        forw_components(:)%RIMAG(:) = 0.0
!        forw_components(:)%RREAL(:) = 0.0
!                              
!        forw_components(:)%sigma  = 0.0
!        forw_components(:)%H0(:)   = 0.0

!        forw_components(:)%NHVP_retr      = 0
!        forw_components(:)%HVP_retr_km(:) = 0.0            
! CL - lidar calibration coefficient is not a particle characteristic. 
!! There is no better solution for this moment.
!        forw_components(:)%CL(:)          = 0.0 
!
!      end subroutine initialize_forward_model_characteristics_components


      subroutine initialize_forward_model_characteristics_particles(forw_particles)

        type(forward_model_characteristics_particles), intent(inout)  ::  forw_particles

        forw_particles%par_type_SD = 0      
        forw_particles%NBIN(:)     = 0 
        forw_particles%RADIUS(:,:) = 0.0      
        forw_particles%SD(:,:)     = 0.0
        forw_particles%C0(:)       = 0.0
            
        forw_particles%NSHAPE(:)  = 0       
        forw_particles%RATIO(:,:) = 0.0
        forw_particles%SHD(:,:)   = 0.0
      
        forw_particles%calculate_rreal = .false.
        forw_particles%calculate_rimag = .false.
        forw_particles%RIMAG(:,:) = 0.0
        forw_particles%RREAL(:,:) = 0.0
                              
        forw_particles%sigma(:)   = 0.0
        forw_particles%H0(:,:)    = 0.0

        forw_particles%NHVP_retr      = 0
        forw_particles%HVP_retr_km(:) = 0.0            
! CL - lidar calibration coefficient is not a particle characteristic. 
! There is no better solution for the moment.
        forw_particles%CL(:) = 1.0
! Relative Humidity
        forw_particles%RH(:) = 0.0

      end subroutine initialize_forward_model_characteristics_particles

      subroutine initialize_forward_model_characteristics_gases(forw_gases)

        type(forward_model_characteristics_gases), intent(inout)  ::  forw_gases

        forw_gases%n = 0
        forw_gases%C(:) = 0.0

      end subroutine initialize_forward_model_characteristics_gases

      subroutine initialize_forward_model_characteristics_surface(forw_surface)

        type(forward_model_characteristics_surface), intent(inout)  ::  forw_surface

        forw_surface%BRF_land(:,:)  = 0.0
        forw_surface%BRP_land(:,:)  = 0.0
        forw_surface%BRM_water(:,:) = 0.0 
                                                                                                
      end subroutine initialize_forward_model_characteristics_surface      


! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine unpack_parameter_vector_ap(  RIN, APSING, &
                                              forw_aerosol, &
                                              forw_gases, &
                                              forw_surface, &
                                              ipix, pixel_fit, HGR_km, NHVP_meas, &
                                              HVP_meas_km, inclination_angle)

      use mod_sdata_derived_type
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type

      implicit none
!	------------------------------------------------------------------------------------------------------   
      integer,                            intent(in)     :: ipix
      real,dimension(KPARS),              intent(in)     ::  APSING
      type(retr_input_settings),          intent(in)     ::  RIN
      type(forward_model_characteristics_particles),intent(inout)  ::  forw_aerosol ! Forward model chracteristics
      type(forward_model_characteristics_gases),intent(inout)  ::  forw_gases ! Forward model chracteristics
      type(forward_model_characteristics_surface  ),intent(inout)  ::  forw_surface ! Forward model characteristics
      type(pixel),               optional,intent(in)     ::  pixel_fit
      real,                      optional,intent(in)     ::  HGR_km    ! site height above sea level 
      integer,                   optional,intent(in)     ::  NHVP_meas ! number of heights for lidar measurements
      real,dimension(KVERTM),    optional,intent(in)     ::  HVP_meas_km  ! distance for lidar measurements
      real,                      optional,intent(in)     ::  inclination_angle  ! zenith angle of lidar measurements (0 degree if vertical)

!	------------------------------------------------------------------------------------------------------
      integer  ::  IDIM1, IDIM2, par_type, isd, ibeg, iend,  igas
      integer  ::  nbins
      logical  ::  icv ! presence of concentration characteristic
      real,dimension(KIDIM3) :: tmp
!	------------------------------------------------------------------------------------------------------
      igas = 0
      forw_aerosol%par_type_SD = -999
      icv = .false.

      call initialize_forward_model_characteristics_particles(forw_aerosol)
      call initialize_forward_model_characteristics_gases(forw_gases)
      call initialize_forward_model_characteristics_surface(forw_surface)

! Unpack parameter vector ap (APSING) into characteristics driving forward model            
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
! Aerosol
        if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then
            call unpack_SD ( RIN, par_type, IDIM1, APSING, forw_aerosol%NBIN, forw_aerosol%RADIUS, forw_aerosol%SD )   ! Size Distribution
            forw_aerosol%par_type_SD = par_type
        elseif(par_type .gt. par_type_RERI_beg .and. par_type .lt. par_type_RERI_end) then             
            call unpack_REFI ( RIN, par_type, IDIM1, APSING, forw_aerosol%RREAL ) ! Real part of refr.index n(wl) or Chemistry
        elseif(par_type .gt. par_type_IMRI_beg .and. par_type .lt. par_type_IMRI_end) then
            call unpack_REFI ( RIN, par_type, IDIM1, APSING, forw_aerosol%RIMAG ) ! Imaginary part of refr.index k(wl)
        elseif(par_type .gt. par_type_SHD_beg  .and. par_type .lt. par_type_SHD_end) then
            call unpack_SHD ( RIN, par_type, IDIM1, APSING, forw_aerosol%NSHAPE, forw_aerosol%RATIO, forw_aerosol%SHD ) ! Sphericity or Shape distribution
        elseif(par_type .gt. par_type_AVP_beg  .and. par_type .lt. par_type_AVP_end) then
            if(present(HGR_km) .and. present(NHVP_meas) .and. present(HVP_meas_km)) then
              forw_aerosol%NHVP_retr = RIN%NDIM%n3(1,IDIM1)
              call unpack_AVP ( RIN, par_type, IDIM1, APSING, RIN%NSD, forw_aerosol%H0 ) ! Aerosol Vertical Profile
              call get_AVP_altitudes_retr ( HGR_km, NHVP_meas, HVP_meas_km, &
                                            inclination_angle,      &
                                            forw_aerosol%NHVP_retr, &
                                            forw_aerosol%HVP_retr_km )
              if ( RIN%indep_par ) then
              if ( par_type .eq. par_type_AVP_prof ) then
              do IDIM2=1,RIN%NSD
              call set_vd_all_retr_altitudes (forw_aerosol%NHVP_retr,   &
                                              forw_aerosol%HVP_retr_km(1:forw_aerosol%NHVP_retr), &
                                              forw_aerosol%H0(1:forw_aerosol%NHVP_retr,IDIM2) )
              enddo
              endif
              endif
            endif
        elseif(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
            if(par_type .ne. par_type_TM_C) then
            call unpack_C0 ( RIN, par_type, IDIM1, APSING, forw_aerosol%C0 )   ! Aerosol Concentration
            icv = .true.
            else
            ! Transport model concentrations
            call unpack_TMC ( RIN, par_type, IDIM1, ipix, APSING, forw_aerosol%H0 )
            endif
        elseif(par_type .gt. par_type_ADD_beg  .and. par_type .lt. par_type_ADD_end) then
            if(par_type .eq. par_type_CL .and. present(pixel_fit)) then  ! Calibration coefficient
            call unpack_lidar_calibr_coeff ( RIN, pixel_fit, IDIM1, APSING, forw_aerosol%CL )
            endif
            if(par_type .eq. par_type_AVP_par_std)  then  ! Standard deviation of vertical profile
            call unpack_AVP_std ( RIN, par_type, IDIM1, APSING, forw_aerosol%sigma )
            end if
! Relative humidity
        elseif(par_type .gt. par_type_RH_beg .and. par_type .lt. par_type_RH_end) then
            if(par_type .eq. par_type_RH) then
            call unpack_RH ( RIN, IDIM1, APSING, forw_aerosol%RH ) ! Relative humidity
            endif
! Gases
        elseif(par_type .gt. par_type_gases_concentration_beg .and. par_type .lt. par_type_gases_concentration_end) then
            igas = igas + 1
            call unpack_CG ( RIN, IDIM1, APSING, forw_gases%C(igas) )
            forw_gases%n = igas
! Surface
        elseif(par_type .gt. par_type_surface_beg .and. par_type .lt. par_type_surface_end) then
            call unpack_spectral_SURF (RIN, IDIM1, APSING, forw_surface)
        else
            if ( RIN%NDIM%par_retr(IDIM1) ) then
            write(*,*) 'IDIM1=',IDIM1,'  par_type =',par_type,'  - value is not valid'
            stop 'stop in unpack_parameter_vector_ap'
            endif
        endif ! par_type .gt. 
      enddo ! IDIM1

     if ( .not. RIN%indep_par ) then
        ! Size Distribution normalization or concentration as a parameter of lognormal SD
        if( (forw_aerosol%par_type_SD .eq. par_type_SD_LN) .or. (icv .eqv. .true.) ) then
            call get_SD_normalized ( RIN, forw_aerosol%par_type_SD, forw_aerosol%C0, &
                                     forw_aerosol%NBIN, forw_aerosol%RADIUS, forw_aerosol%SD )
        endif
      else
        ! If concentration is a retrieved parameter then an additional SD preparation is required.
        if( icv ) then
        if ( forw_aerosol%par_type_SD .eq. par_type_SD_LN ) then
            call set_SD_LN_concentration ( RIN, forw_aerosol%C0, &
                                           forw_aerosol%NBIN, forw_aerosol%SD )
        else
            ! prepare concentrations for n SD/models bins in independent concentration and shape  retrieval
            call get_SD_bins_concentrations ( RIN, forw_aerosol%par_type_SD, forw_aerosol%C0, &
                                         forw_aerosol%NBIN, forw_aerosol%RADIUS, forw_aerosol%SD )
        endif
        endif
      endif
      ! Extract precomputed lognormal bins for fine and coarse modes
      ! must be after call get_SD_normalized and call get_SD_bins_concentrations()
      if( forw_aerosol%par_type_SD .eq. par_type_SD_LB ) then
          if ( RIN%NSD .gt. 1 ) then
            nbins = sum ( forw_aerosol%NBIN(1:RIN%NSD) )
            do isd=RIN%NSD,2,-1
            ibeg = nbins - sum ( forw_aerosol%NBIN(isd:RIN%NSD) ) + 1
            iend = ibeg + forw_aerosol%NBIN(isd) - 1
            forw_aerosol%RADIUS(ibeg:iend,1) = forw_aerosol%RADIUS(1:forw_aerosol%NBIN(isd),isd)
            tmp(1:forw_aerosol%NBIN(isd)) = forw_aerosol%SD(1:forw_aerosol%NBIN(isd),isd)
            forw_aerosol%SD(1:nbins,isd) = 0.0
            forw_aerosol%SD(ibeg:iend,isd) = tmp(1:forw_aerosol%NBIN(isd))
            enddo
            do isd=1,RIN%NSD
            forw_aerosol%RADIUS(1:nbins,isd) = forw_aerosol%RADIUS(1:nbins,1)
            enddo
            forw_aerosol%NBIN(1:RIN%NSD) = nbins
!write(*,*) 'TEST LB with NSD>1 in unpack_parameter_vector_ap'
!write(*,'(a,i0)') 'nbins = ',nbins
!do isd=1,RIN%NSD
!write(*,'(2(a,i0))') 'isd = ',isd,'  NBIN(isd) = ',forw_aerosol%NBIN(isd)
!write(*,'(a)') 'RADIUS(1:nbins,isd): '
!write(*,'(10es12.4)') forw_aerosol%RADIUS(1:nbins,isd)
!write(*,'(a)') 'SD(1:nbins,isd): '
!write(*,'(10es12.4)') forw_aerosol%SD(1:nbins,isd)
!enddo
!stop 'stop test in unpack_parameter_vector_ap for LognormalBins'
          endif
      endif

      return
      end subroutine unpack_parameter_vector_ap

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine get_radiative_transfer_SOS_flags_surf ( RIN, iBRDF_land, iBPDF_land, iBRM_water )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(in)   ::  RIN
      integer,                    intent(out)  ::  iBRDF_land, iBPDF_land, iBRM_water
      integer   :: IDIM1,par_type
! -----------------------------------------------------------------------------------------
! The parameters for radiative transfer module
!   iBRDF
!   iBPDF
! -----------------------------------------------------------------------------------------
! Surface model options

      iBRDF_land = -1
      iBPDF_land = -1
      iBRM_water = -1

      do IDIM1=1,RIN%NDIM%n1
        if(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF1_land_beg .and. RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF1_land_end) then
          par_type = RIN%NDIM%par_type(IDIM1)
          select case (par_type)
          case(par_type_SURF1_land_Ross_Li_BRDF)
            iBRDF_land = 1
          case(par_type_SURF1_land_RPV_BRDF)
            iBRDF_land = 0
          case(par_type_SURF1_land_Litvinov_fast)
            iBRDF_land = 2
          case(par_type_SURF1_land_Litvinov)
            iBRDF_land = 9
            iBPDF_land = 9
          end select
        endif ! RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF1_beg .and.

        if(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF2_land_beg .and. RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF2_land_end) then
          par_type = RIN%NDIM%par_type(IDIM1)
          select case(par_type)
          case(par_type_SURF2_land_Maignan_Breon)
            iBPDF_land = 0
          case(par_type_SURF2_land_Litvinov)
            iBPDF_land = 1
          end select
        endif ! RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF2_beg .and.

        if(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF_water_beg .and. RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF_water_end) then
          par_type = RIN%NDIM%par_type(IDIM1)
             select case (par_type)
              case(par_type_SURF_water_Cox_Munk_iso)
                iBRM_water = 0
              case(par_type_SURF_water_Cox_Munk_ani)
                iBRM_water = 1
              case(par_type_SURF_water_Litvinov)
                iBRM_water = 9
              case(par_type_SURF_water_Cox_Munk_iso_2par)
                iBRM_water = 2
              case(par_type_SURF_water_Morel)
                iBRM_water = 3
              end select
            endif ! RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF1_beg .and.
      enddo ! IDIM1

      !write(*,*) 'iBRDF_land,iBPDF_land,iBRM_water:'
      !write(*,'(3i8,a)') iBRDF_land,iBPDF_land,iBRM_water,'  in get_radiative_transfer_SOS_flags_surf'

      return
      end subroutine get_radiative_transfer_SOS_flags_surf

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Returns spectral surface parameters depend on surface characteristic

      subroutine unpack_spectral_SURF ( RIN,IDIM1,APSING,forw_surface )

      use mod_par_inv, only : KPARS,KIDIM2,KIDIM3
      use mod_retr_settings_derived_type

      implicit none
! ----------------------------------------------------------------
      type(retr_input_settings),    intent(in)  ::  RIN
      integer,                      intent(in)  ::  IDIM1
      real,dimension(KPARS),        intent(in)  ::  APSING
      ! Forward model spectral surface characteristics
      type(forward_model_characteristics_surface), intent(inout)  ::  forw_surface
! ----------------------------------------------------------------
      integer  :: par_type
! ----------------------------------------------------------------
        par_type = RIN%NDIM%par_type(IDIM1)

        if(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF1_land_end) then
            ! Land surface ( RPV, Ross_Li, Litvinov)
            call unpack_SURF ( RIN, IDIM1, APSING, forw_surface%BRF_land )
        elseif(par_type .gt. par_type_SURF2_land_beg .and. par_type .lt. par_type_SURF2_land_end) then
            ! Land surface ( Maignan, Litvinov_BPDF, Nadal-Breon)
            call unpack_SURF ( RIN, IDIM1, APSING, forw_surface%BRP_land )
        elseif(par_type .gt. par_type_SURF_water_beg .and. par_type .lt. par_type_SURF_water_end) then
            ! Water surface ( Cox_Munk_iso, Cox_Munk_ani, Litvinov, Cox_Munk_iso_2par, Morel)
            call unpack_SURF ( RIN, IDIM1, APSING, forw_surface%BRM_water )
        endif

      return
      end subroutine unpack_spectral_SURF

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

end module mod_forward_model_characteristics

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Transport model concentrations

      subroutine unpack_TMC ( RIN, par_type, IDIM1, ipix, APSING1, H0 )

      use mod_par_inv, only : KPARS, KVERTM
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type
      use mod_derivative_type_transport_model, only : TM

      implicit none
! ----------------------------------------------------------------
      integer,                    intent(in)    ::  ipix
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  par_type, IDIM1
      real,dimension(KPARS),      intent(in)    ::  APSING1
      real,dimension(KVERTM,KSD), intent(out)   ::  H0
! ----------------------------------------------------------------
      integer :: IDIM2, IDIM3, II
! ----------------------------------------------------------------

      do IDIM2=1,RIN%NDIM%n2(IDIM1)
      do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            TM%c_dry(IDIM3,IDIM2,ipix) = APSING1(II)
            TM%vp(IDIM3,IDIM2,ipix) = APSING1(II) &
            * TM%hgfactor(IDIM3,IDIM2,ipix) &
            * TM%hgfactor(IDIM3,IDIM2,ipix) &
            * TM%hgfactor(IDIM3,IDIM2,ipix) &
            / TM%density_dry(IDIM2)
            TM%vp(IDIM3,IDIM2,ipix) = TM%vp(IDIM3,IDIM2,ipix) * 1e+9  ! unit conversion
            H0(IDIM3,IDIM2) = TM%vp(IDIM3,IDIM2,ipix)
!write(*,*) par_type,'  - par_type, II =',II,'  IDIM2 =',IDIM2,'  IDIM3=',IDIM3, &
!'  TM_C(IDIM2) =',TM%c(IDIM3,IDIM2,ipix),'  density_rh(IDIM3,IDIM2,ipix) =', &
!TM%density_rh(IDIM3,IDIM2,ipix),'  in unpack_TMC'
!write(*,*) par_type,'  - par_type, II =',II,'  IDIM2 =',IDIM2,'  IDIM3=',IDIM3, &
!'  H0 =',H0(IDIM3,IDIM2),'  in unpack_TMC'

      enddo ! IDIM3
      enddo ! IDIM2

      return
      end subroutine unpack_TMC

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Surface parameters at IW-th wave length
      subroutine get_SURF_wl ( RIN, IW, ind_wl,  &
                               BRF1, BRP1, BRM1, &
                               BRF, BRP, BRM )

      use mod_par_inv, only : KPARS,KIDIM2,KIDIM3,KBF,KW
      use mod_retr_settings_derived_type
      use mod_mixture_ref_index_chem
      use mod_functional_retrieval, only : retr_method_fullset, &
                                          get_functional_retrieval_parameter
      use mod_stop_report
      
      implicit none
! ----------------------------------------------------------------
      type(retr_input_settings),    intent(in)  ::  RIN
      integer,                      intent(in)  ::  IW
      integer,                      intent(in)  ::  ind_wl
      real,dimension(KIDIM3,KIDIM2),intent(in)  ::	BRP1,BRF1,BRM1
      real,dimension(KBF),          intent(out) ::	BRF,BRP,BRM
! ----------------------------------------------------------------
      integer  :: I, IDIM1, IDIM2, par_type
      integer  :: ndim2, ndim3, ind_wl1 ! ndim3
! ----------------------------------------------------------------
      BRP(:) = 0.0
      BRF(:) = 0.0
      BRM(:) = 0.0

      par_type = 0

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF1_land_end) then
        ! BRF
          ndim2 = RIN%NDIM%n2(IDIM1)
          do IDIM2=1,ndim2
            ndim3 = RIN%ndim%n3(IDIM2,IDIM1)
            if ( RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset ) then
              BRF(IDIM2) = BRF1(ind_wl,IDIM2)
            else
              call get_functional_retrieval_parameter( RIN, IDIM1, IDIM2, RIN%NW, RIN%WAVE(1:RIN%NW), BRF1(:,IDIM2), ind_wl, BRF(IDIM2) )
            endif
!write(*,'(a,f12.4,i4,es12.4)') 'WAVE,IDIM2,BRF -  ',RIN%WAVE(ind_wl),IDIM2,BRF(IDIM2)
          enddo ! IDIM2
        elseif(par_type .gt. par_type_SURF2_land_beg .and. par_type .lt. par_type_SURF2_land_end) then
        ! BRP
          ndim2 = RIN%NDIM%n2(IDIM1)
          do IDIM2=1,ndim2
            ndim3 = RIN%ndim%n3(IDIM2,IDIM1)
            if ( RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset ) then
              BRP(IDIM2) = BRP1(ind_wl,IDIM2)
            else
              call get_functional_retrieval_parameter( RIN, IDIM1, IDIM2, RIN%NW, RIN%WAVE(1:RIN%NW), BRP1(:,IDIM2), ind_wl, BRP(IDIM2) )
            endif
!write(*,'(a,f12.4,i4,es12.4)') 'WAVE,IDIM2,BRP -  ',RIN%WAVE(ind_wl),IDIM2,BRP(IDIM2)
          enddo ! IDIM2
        elseif(par_type .gt. par_type_SURF_water_beg .and. par_type .lt. par_type_SURF_water_end) then
        ! BRM water
          ndim2 = RIN%NDIM%n2(IDIM1)
          do IDIM2=1,ndim2
            ndim3 = RIN%ndim%n3(IDIM2,IDIM1)
            if ( RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset ) then
              BRM(IDIM2) = BRM1(ind_wl,IDIM2)
            else
              call get_functional_retrieval_parameter( RIN, IDIM1, IDIM2, RIN%NW, RIN%WAVE(1:RIN%NW), BRM1(:,IDIM2), ind_wl, BRM(IDIM2) )
            endif
!write(*,'(a,f12.4,i4,es12.4)') 'WAVE,IDIM2,BRM -  ',RIN%WAVE(ind_wl),IDIM2,BRM(IDIM2)
          enddo ! IDIM2
        endif ! par_type .gt. par_type_SURF1_land_beg .and.
      enddo ! IDIM1

      if(par_type .eq. 0) then
        write(tmp_message,'(a,i0,2x,a)') &
        'par_type = ',par_type,'No surface characteristic was found.'
        G_ERROR(trim(tmp_message))
      endif

      return
      end subroutine get_SURF_wl

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Complex refractive index at IW-th wave length

      subroutine get_REFI_wl ( RIN,IW,ind_wl,Nsubchannels,RH,bandwidth,WL_Subchannels,GOUT_chem_pixel, &
                               RREALL,RIMAGL,RREAL_Subchannels,RIMAG_Subchannels,RREAL,RIMAG )
      
      use mod_par_inv, only : KPARS,KIDIM2,KIDIM3,KW
      use mod_par_OS,  only : KSD, N_CHEM_MAX,N_SUB_CHANNEL_MAX
      use mod_globals, only  : GBL_FILE_PATH_LEN
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_mixture_ref_index_chem
      use mod_functional_retrieval, only : retr_method_fullset, &
                                          get_functional_retrieval_parameter
      use mod_c_utils, only: cstring2fstring
      use mod_stop_report

      implicit none
! ----------------------------------------
! IN :
      type(retr_input_settings),                 intent(in)    ::  RIN
      integer,                                   intent(in)    ::  IW
      integer,                                   intent(in)    ::  ind_wl
      integer,                                   intent(in)    ::  NSubchannels
      real,dimension(KSD),                       intent(in)    ::  RH
      real,dimension(N_SUB_CHANNEL_MAX),         intent(in)    ::  bandwidth,WL_Subchannels
! ----------------------------------------
! INOUT :
      type(output_pixel_chemistry),              intent(inout) ::  GOUT_chem_pixel
      real,dimension(KIDIM3,KIDIM2),             intent(inout) ::  RIMAGL,RREALL
! ----------------------------------------
! OUT :
      real,dimension(KSD,N_SUB_CHANNEL_MAX),     intent(out)   ::  RREAL_Subchannels,RIMAG_Subchannels
      real,dimension(KSD),                       intent(out)   ::  RREAL,RIMAG
! ----------------------------------------
! LOCAL :
      real(selected_real_kind(p=15)),dimension(RIN%NW) :: RREALMM,RIMAGMM
      real,                           dimension(RIN%NW) :: WAVELM
      integer,dimension(KSD) ::  nfract
!    ------------------------------------------------------------------------------------------------------
      integer :: par_type
      integer :: II, IDIM1, IDIM2, ISD, ind_wl1, ibeg_IWW, iend_IWW, ISUB
      integer :: ifract
      integer :: ndim2, ndim3
! ----------------------------------------------------------------
!New chemistry
      real(selected_real_kind(p=15))  :: rh_mxtr       ! Valid range: 0--1
      real(selected_real_kind(p=15)), dimension(N_CHEM_MAX,KSD) :: fv_new_chem
      real(selected_real_kind(p=15)), dimension(N_CHEM_MAX)     :: refr_chem, refi_chem
      real(selected_real_kind(p=15)), dimension(N_CHEM_MAX)     :: vfract
      character (len=GBL_FILE_PATH_LEN) , dimension(N_CHEM_MAX,KSD) :: name_new_chem
! ----------------------------------------------------------------

      RREAL(:) = 0.0
      RIMAG(:) = 0.0
      par_type = 0
      do IDIM1=1,RIN%NDIM%n1
        ndim2 = RIN%ndim%n2(idim1)
        par_type = RIN%NDIM%par_type(IDIM1)
        select case( par_type )
        case( par_type_RERI_spect )
          do IDIM2=1,ndim2
            ndim3 = RIN%ndim%n3(IDIM2,IDIM1)
            if ( RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset ) then
              RREAL(IDIM2) = RREALL(ind_wl,IDIM2)
            else
              call get_functional_retrieval_parameter( RIN, IDIM1, IDIM2, RIN%NW, RIN%WAVE(1:RIN%NW), RREALL(:,IDIM2), ind_wl, RREAL(IDIM2) )
            endif
          enddo ! IDIM2 
          if ( ndim2 .eq. 1 ) then
            RREAL(1:RIN%NSD) = RREAL(1)
          endif
!write(*,'(a,f12.4,10f12.4)') 'WAVE,IDIM2,RREAL -  ',RIN%WAVE(ind_wl),RREAL(1:RIN%NSD)
        case( par_type_IMRI_spect )
          do IDIM2=1,ndim2
            ndim3 = RIN%ndim%n3(IDIM2,IDIM1)
            if ( RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset ) then
              RIMAG(IDIM2) = RIMAGL(ind_wl,IDIM2)
            else
              call get_functional_retrieval_parameter( RIN, IDIM1, IDIM2, RIN%NW, RIN%WAVE(1:RIN%NW), RIMAGL(:,IDIM2), ind_wl, RIMAG(IDIM2) )
            endif
          enddo ! IDIM2
          if ( ndim2 .eq. 1 ) then
            RIMAG(1:RIN%NSD) = RIMAG(1)
          endif
!write(*,'(a,f12.4,10f12.4)') 'WAVE,IDIM2,RIMAG -  ',RIN%WAVE(ind_wl),RIMAG(1:RIN%NSD)
        case( par_type_RERI_const )
          ind_wl1 = 1
          do ISD=1,RIN%NSD
            RREAL(ISD) = RREALL(ind_wl1,ISD)
          enddo ! ISD`
        case( par_type_IMRI_const )
          ind_wl1 = 1
          do ISD=1,RIN%NSD
            RIMAG(ISD) = RIMAGL(ind_wl1,ISD)
          enddo ! ISD
        case( par_type_CXRI_nmix )

            if(read_chem_lut) then
                !MH Read and interpolate refractive index for the different species and aerosol modes
                call get_REFI_CHEM(RIN,IDIM1)
                read_chem_lut = .false.
            end if


          do ISD=1,RIN%NSD
            nfract(ISD) = RIN%NDIM%n3(ISD,IDIM1)
            if ( RIN%indep_par ) then
              nfract(ISD) = RIN%NDIM_plus%n3(ISD,IDIM1)
              call set_all_bins(nfract(ISD),RREALL(1:nfract(ISD),ISD))
            endif

            ! compute normalized volume fractions vfract(1:nfract) of chem components
            ! and assign them to chemistry of gout structure
            call volume_fraction_normalization (RIN%NSD,ISD,                 & !IN
                                                nfract(ISD),                 &
                                                RREALL(1:nfract(ISD),ISD),   &
                                                vfract(1:nfract(ISD)))         !OUT

            GOUT_chem_pixel%vfract(1:nfract(ISD),ISD) = vfract(1:nfract(ISD))
!write(*,*)"vfract",GOUT_chem_pixel%vfract(1:nfract(ISD),ISD)
            do ISUB=1,Nsubchannels
                
                !MHG Interpolate and compute spectral complex refractive index
                call complex_refr_index_practical_mixture (ISD,nfract(ISD),                   & !IN
                                                           vfract(1:nfract(ISD)),         &
                                                           WL_Subchannels(ISUB),          &
                                                           RREAL_Subchannels(ISD,ISUB),   & !OUT
                                                           RIMAG_Subchannels(ISD,ISUB))

                RREAL(ISD) = RREAL(ISD) + (RREAL_Subchannels(ISD,ISUB)*bandwidth(ISUB))
                RIMAG(ISD) = RIMAG(ISD) + (RIMAG_Subchannels(ISD,ISUB)*bandwidth(ISUB))

            end do !ISUB

            RREAL(ISD) = RREAL(ISD)/SUM(bandwidth(1:Nsubchannels))
            RIMAG(ISD) = RIMAG(ISD)/SUM(bandwidth(1:Nsubchannels))

          enddo ! ISD


        case( par_type_CXRI_chem )
          
          if(read_chem_lut) then
                !MH Read and interpolate refractive index for the different species and aerosol modes
                call get_REFI_CHEM(RIN,IDIM1)
                read_chem_lut = .false.
          end if

          do ISD=1,RIN%NSD
            GOUT_chem_pixel%rh(ISD)  = RH(ISD)
            rh_mxtr = RH(ISD)
            nfract(ISD) = RIN%NDIM%n3(ISD,IDIM1)
           
            vfract(1:nfract(ISD)) = RREALL(1:nfract(ISD),ISD)
            vfract(nfract(ISD)) = vfract(nfract(ISD)) * (1.0-SUM(vfract(1:nfract(ISD)-1)))
            GOUT_chem_pixel%vfract(1:nfract(ISD),ISD) = vfract(1:nfract(ISD))
            
            !MH If Maxwell-Garnett is selected as mixing methodology water and solubles are always present
            vfract(nfract(ISD)+1) = 0.0  !MH Water
            vfract(nfract(ISD)+2) = 0.0  !MH Soluble
            nfract(ISD) = nfract(ISD) + 2

            do ISUB=1,Nsubchannels
                !MH Read and interpolate refractive index for the different species but soluble and water
            
                call MIXING(RIN,                            & !IN
                            WL_Subchannels(ISUB),ISD,       &
                            rh_mxtr,                        &
                            nfract(ISD),vfract,             &
                            RREAL_Subchannels(ISD,ISUB),    &
                            RIMAG_Subchannels(ISD,ISUB))

                GOUT_chem_pixel%fwtr(ISD)  = vfract(nfract(ISD)-1)
                GOUT_chem_pixel%fslbl(ISD) = vfract(nfract(ISD))

                RREAL(ISD) = RREAL(ISD) + (RREAL_Subchannels(ISD,ISUB)*bandwidth(ISUB))
                RIMAG(ISD) = RIMAG(ISD) + (RIMAG_Subchannels(ISD,ISUB)*bandwidth(ISUB))

            enddo ! ISUB

            RREAL(ISD) = RREAL(ISD)/SUM(bandwidth(1:Nsubchannels))
            RIMAG(ISD) = RIMAG(ISD)/SUM(bandwidth(1:Nsubchannels))

          enddo ! ISD

        end select

      enddo ! IDIM1
                
      if(par_type .eq. 0) then
        write(tmp_message,'(a,i0,2x,a)') &
        'par_type = ',par_type,'No complex refractive index characteristic was found.'
        G_ERROR(trim(tmp_message))
      endif

      return
      end subroutine get_REFI_wl 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Gas concentrations

      subroutine unpack_CG ( RIN,IDIM1,APSING1,C )
      
      use mod_par_inv, only : KPARS
      use mod_retr_settings_derived_type
            
      implicit none
! ----------------------------------------------------------------	  
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  IDIM1
      real,dimension(KPARS),      intent(in)    ::  APSING1
      real,        intent(out)   ::  C
! ----------------------------------------------------------------	  
      integer              :: IDIM2,IDIM3,II
! ----------------------------------------------------------------	  
      C = 0.0
      
      do IDIM2=1,RIN%NDIM%n2(IDIM1) 
        do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1                     
            C = APSING1(II)
!write(*,*) 'II=',II,'  IDIM2=',IDIM2,'  C =',C,'  in unpack_CG'
        enddo ! IDIM3
      enddo ! IDIM2   

      return
      end subroutine unpack_CG

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Surface parameters 

      subroutine unpack_SURF ( RIN,IDIM1,APSING1,B1 )

      use mod_par_inv, only : KPARS,KIDIM2,KIDIM3
      use mod_retr_settings_derived_type

      implicit none
! ----------------------------------------------------------------
      type(retr_input_settings),    intent(in)  ::  RIN
      integer,                      intent(in)  ::  IDIM1
      real,dimension(KPARS),        intent(in)  ::  APSING1
      real,dimension(KIDIM3,KIDIM2),intent(out) ::	B1

! ----------------------------------------------------------------
      integer  :: IDIM2,IDIM3,II
! ----------------------------------------------------------------
      B1(:,:) = 0.0

      do IDIM2=1,RIN%NDIM%n2(IDIM1)
        do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            if(RIN%NDIM%n3(IDIM2,IDIM1) .eq. 1) then
              B1(1:RIN%NW,IDIM2) = APSING1(II)
            else
              B1(IDIM3,IDIM2) = APSING1(II)
            endif
        enddo ! IDIM3
      enddo ! IDIM2

      return
      end subroutine unpack_SURF

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Aerosol concentration

      subroutine unpack_C0 ( RIN,par_type,IDIM1,APSING1,C0 )
      
      use mod_par_inv, only : KPARS
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type
            
      implicit none
! ----------------------------------------------------------------	  
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  par_type,IDIM1
      real,dimension(KPARS),      intent(in)    ::  APSING1
      real,dimension(KSD),        intent(out)   ::  C0      
! ----------------------------------------------------------------	  
      integer              :: IDIM2,IDIM3,II
! ----------------------------------------------------------------	  
      C0(:)   = 0.0
      
      do IDIM2=1,RIN%NDIM%n2(IDIM1) 
        do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1                     
            C0(IDIM2) = APSING1(II)
!write(*,*) par_type,'  - par_type, II=',II,'  IDIM2=',IDIM2,'  C0(IDIM2)=',C0(IDIM2),'  in unpack_C0'      
        enddo ! IDIM3
      enddo ! IDIM2   

      return
      end subroutine unpack_C0 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Altitudes for retrieved Aerosol Vertical Profile

      subroutine get_AVP_altitudes_retr ( HGR_km,                & 
                                          NHVP_meas,HVP_meas_km, &
                                          inclination_angle,     &
                                          NHVP_retr,HVP_retr_km )
      
      use mod_par_inv, only : KVERTM
      use mod_retr_settings_derived_type
            
      implicit none
! ----------------------------------------------------------------	  
      integer,                    intent(in)    ::  NHVP_meas
      real,dimension(KVERTM),     intent(in)    ::  HVP_meas_km
      real,                       intent(in)    ::  HGR_km
      real,                       intent(in)    ::  inclination_angle
      integer,                    intent(inout) ::  NHVP_retr
      real,dimension(KVERTM),     intent(out)   ::  HVP_retr_km
! ----------------------------------------------------------------	  
      integer  ::  i
! ----------------------------------------------------------------	  
      HVP_retr_km(:) = 0.0
      
      if (NHVP_retr .eq. 1) then
         if(NHVP_meas .ne. NHVP_retr) then
         write(*,*) 'NHVP_meas=',NHVP_meas,' .ne. ','NHVP_retr=',NHVP_retr
         stop 'stop in forw_IMAGE_I'
         endif
         NHVP_retr = NHVP_meas
         HVP_retr_km(1:NHVP_retr) = HVP_meas_km(1:NHVP_meas)      
      else
         if(NHVP_retr-NHVP_meas .gt. 1 .or. NHVP_retr .lt. NHVP_meas) then
            write(*,*) 'NHVP_retr=',NHVP_retr,'  NHVP_meas=',NHVP_meas, &
            '  - inconsistent number of heights'
            stop 'stop in get_AVP_altitudes_retr'
         endif
! adding projection in case id HVP_meas is inclined (lidar case only, groundbased only)
! recalculating distance from lidar to altitude a.s.l.

         HVP_retr_km(1:NHVP_meas) = HVP_meas_km(1:NHVP_meas)*cos(inclination_angle*0.01745329251)+HGR_km
!         HVP_retr_km(1:NHVP_meas) = HVP_meas_km(1:NHVP_meas)*cos(inclination_angle*0.01745329251)
         if(NHVP_retr .gt. NHVP_meas) then
            NHVP_retr              = NHVP_meas + 1
            HVP_retr_km(NHVP_retr) = HGR_km
            if(HVP_retr_km(NHVP_retr-1) .lt. HGR_km) then
              write(*,*) 'HVP_retr_km(NHVP_retr-1)=',HVP_retr_km(NHVP_retr-1),  &
              ' can not be less than   HGR_km=',HGR_km
              do i=1,NHVP_retr
                 write(*,*) i,HVP_retr_km(i),'  - i,HVP_retr_km(i)'
              enddo
              stop 'stop in get_AVP_altitudes_retr'              
            endif
         endif
      endif

      !write(*,*) 'NHVP_retr=',NHVP_retr,'  NHVP_meas=', NHVP_meas
      !do i=1,NHVP_retr
      !write(*,*) i,HVP_retr_km(i),'  - i,HVP_retr_km(i)'
      !enddo
      !stop
      
      return
      end subroutine get_AVP_altitudes_retr 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Spectral calibration coefficient for lidar

      subroutine unpack_lidar_calibr_coeff ( RIN,pixel_fit,IDIM1,APSING1,CL )
      
      use mod_par_inv, only : KPARS,KW,KVERTM
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type
      use mod_sdata
      
      implicit none
! ----------------------------------------------------------------	  
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  IDIM1
      real,dimension(KPARS),      intent(in)    ::  APSING1
      type(pixel),                intent(in)    ::  pixel_fit
      real,dimension(KW),         intent(inout)   ::  CL       
! ----------------------------------------------------------------	  
      real,dimension(KW)   :: CL_temp 
      integer              :: IDIM2,IDIM3,II,IW,IP,IW1
! ----------------------------------------------------------------	  
      !CL(:)   = 0.0
      
      do IDIM2=1,RIN%NDIM%n2(IDIM1) 
        if(IDIM2 .gt. 1) then
          write(*,*) 'in unpack_lidar_calibr_coeff: IDIM2=',IDIM2,' can not be > 1'
          stop 'stop in unpack_lidar_calibr_coeff'
        endif
        do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1                     
            CL(IDIM3) = APSING1(II) ! lidar calibration parameters
!write(*,*) 'in unpack_lidar_calibr_coeff: II=',II,'IDIM3=',IDIM3,'  IDIM2=',IDIM2,'  CL(IDIM3)=',CL(IDIM3)
!write(*,*) 'in unpack_lidar_calibr_coeff: II=',II, 'APSING=', APSING1(II)
        enddo ! IDIM3
      enddo ! IDIM2   

! assingning CL to the correct wavelength in a full set of WL 
! write(*,*) 'in unpack_lidar_calibr_coeff: CL: ',CL(1:RIN%NW)      
      IW1=1
      CL_temp(:)=CL(:)
      do IW=1,RIN%NW
! write(*,*) 'in unpack_lidar_calibr_coeff: NIP=',pixel_fit%meas(IW)%NIP      
        do IP=1,pixel_fit%meas(IW)%NIP
          if(pixel_fit%meas(IW)%meas_type(IP) .ge. meas_type_LS .and. &
            pixel_fit%meas(IW)%meas_type(IP) .le. meas_type_VBS ) then
            CL(IW)=CL_temp(IW1)
            IW1=IW1+1
          else
            CL(IW)=1.0
          endif ! meas_type .ge. meas_type_LS .and.
        enddo ! IP
      enddo ! IW                            
!write(*,'(a,10e14.6)') 'in unpack_lidar_calibr_coeff: CL: ',CL(1:RIN%NW)      
!stop          
      
      return
      end subroutine unpack_lidar_calibr_coeff 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Profile standard deviation
      subroutine unpack_AVP_std ( RIN, par_type, IDIM1, APSING1, sigma )

      use mod_par_inv, only : KPARS
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type

      implicit none
! ----------------------------------------------------------------
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  par_type, IDIM1
      real,dimension(KPARS),      intent(in)    ::  APSING1
      real,dimension(KSD),        intent(out)   ::  sigma
! ----------------------------------------------------------------
      integer :: NSD, NDIM2, NDIM3
      integer :: IDIM2, IDIM3, II
      integer :: ibeg, iend, nvp
! ----------------------------------------------------------------
      sigma(:)   = 0.0
      NSD = RIN%NSD
      NDIM2 = RIN%NDIM%n2(IDIM1)

      if ( NDIM2 .lt. NSD ) then
        if ( NDIM2 .ne. 1 ) then
          ! ACCP
          nvp = NSD/NDIM2
          do IDIM2=1,NDIM2
          NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
          ibeg = (IDIM2-1)*nvp+1
          iend = ibeg + nvp - 1
          do IDIM3=1,NDIM3
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            sigma(ibeg:iend) = APSING1(II)
!write(*,*) '1: ',par_type,'  - par_type, II=',II,'  IDIM2=',IDIM2,'  sigma(IDIM2)=',sigma(IDIM2),'  in unpack_AVP_par_std'
          enddo ! IDIM3
          enddo ! IDIM2
        elseif ( NDIM2 .eq. 1 ) then
          ibeg = 1
          iend = NSD
          do IDIM2=1,NDIM2
          NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
          do IDIM3=1,NDIM3
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            sigma(ibeg:iend) = APSING1(II)
!write(*,*) '2: ',par_type,'  - par_type, II=',II,'  IDIM2=',IDIM2,'  sigma(IDIM2)=',sigma(IDIM2),'  in unpack_AVP_par_std'
          enddo ! IDIM3
          enddo ! IDIM2
        endif
      elseif ( NDIM2 .eq. NSD ) then
        do IDIM2=1,NDIM2
        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
        do IDIM3=1,NDIM3
          II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
          sigma(IDIM2) = APSING1(II)
!write(*,*) '3: ',par_type,'  - par_type, II=',II,'  IDIM2=',IDIM2,'  sigma(IDIM2)=',sigma(IDIM2),'  in unpack_AVP_par_std'
        enddo ! IDIM3
        enddo ! IDIM2
      endif ! NDIM2 .lt. NSD

      return
      end subroutine unpack_AVP_std

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Aerosol Vertical Profile

      subroutine unpack_AVP ( RIN,par_type,IDIM1,APSING1,NSD,H0 )

      use mod_par_inv, only : KPARS,KVERTM
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type

      implicit none
! ----------------------------------------------------------------
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  NSD,par_type,IDIM1
      real,dimension(KPARS),      intent(in)    ::  APSING1
      real,dimension(KVERTM,KSD), intent(out)   ::  H0
! ----------------------------------------------------------------
      integer :: NDIM2, NDIM3
      integer :: IDIM2, IDIM3, II
      integer :: ibeg, iend, nvp
! ----------------------------------------------------------------
      H0(:,:) = 0.0
      NDIM2 = RIN%NDIM%n2(IDIM1)

      if ( NDIM2 .lt. NSD ) then
        if ( NDIM2 .ne. 1 ) then
        if ( mod(NSD,NDIM2) .ne. 0 ) then
          write(*,'(a,i0,1x,a)') 'the number of profiles = ',NDIM2, &
          'has to be a multiple of the number of partocle components = ',NSD
          stop 'stop in unpack_AVP'
        endif
        endif
      endif
      
      if ( NDIM2 .lt. NSD ) then
        if ( NDIM2 .ne. 1 ) then
          ! ACCP
          nvp = NSD/NDIM2
          do IDIM2=1,NDIM2
          NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
          ibeg = (IDIM2-1)*nvp + 1
          iend = ibeg + nvp - 1
          do IDIM3=1,NDIM3
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            H0(IDIM3,ibeg:iend) = APSING1(II)
!write(*,*) '1: II=',II,'  IDIM3',IDIM3,'  IDIM2=',IDIM2,'  H0=',APSING1(II)
          enddo ! IDIM3
          enddo ! IDIM2
        elseif ( NDIM2 .eq. 1) then
          do IDIM2=1,NDIM2
          NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
          ibeg = 1
          iend = NSD
          do IDIM3=1,NDIM3
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            H0(IDIM3,ibeg:iend) = APSING1(II)
!write(*,*) '2: II=',II,'  IDIM3',IDIM3,'  IDIM2=',IDIM2,'  H0=',APSING1(II)
          enddo ! IDIM3
          enddo ! IDIM2
        endif
      elseif ( NDIM2 .eq. NSD ) then
        do IDIM2=1,NDIM2
        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
        do IDIM3=1,NDIM3
          II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
          H0(IDIM3,IDIM2) = APSING1(II)
!write(*,*) '3: II=',II,'  IDIM3',IDIM3,'  IDIM2=',IDIM2,'  H0=',APSING1(II)
        enddo ! IDIM3
        enddo ! IDIM2
      endif ! NDIM2 .lt. NSD

      return
      end subroutine unpack_AVP

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Shape of aerosol particles in case of retrieval of independent parameters
      subroutine unpack_SHD ( RIN,par_type,IDIM1,APSING1,NSHAPE,RATIO,SHD )

      use mod_par_inv, only : KPARS,KIDIM2,KIDIM3,KSHAPE
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type
      
      implicit none
! ----------------------------------------------------------------
      type(retr_input_settings),  intent(in)    :: RIN
      integer,                    intent(in)    :: par_type, IDIM1
      real,dimension(KPARS),      intent(in)    :: APSING1
      real,dimension(KSHAPE,KSD), intent(out)   :: RATIO, SHD
      integer,dimension(KSD),     intent(out)   :: NSHAPE
! ----------------------------------------------------------------	  
      integer :: IDIM2,IDIM3,II
      integer :: NDIM3
      real    :: sum1
! ----------------------------------------------------------------
      RATIO(:,:) = 0.0
      NSHAPE(:) = 0
      SHD(:,:) = 0.0

      if ( RIN%NDIM%n2(IDIM1) .lt. RIN%NSD ) then
        if ( RIN%NDIM%n2(IDIM1) .gt. 1 ) then
          write(*,'(3(a,i0,2x),a)') 'IDIM1 = ',IDIM1, &
          'n2(IDIM1) = ',RIN%NDIM%n2(IDIM1),'NSD = ',RIN%NSD, &
          'n2 can not be > 1 for special case to fill in shape distribution ???'
          stop 'stop in unpack_SHD'
        endif
      endif

      if ( par_type .eq. par_type_SHD_fsph) then ! retrieve % spherical particles
        NSHAPE(1:RIN%NSD) = 2
        !RIN%RATIO1(1,1:RIN%NSD) = 1.0   ! assigned before call inversion()
        !RIN%RATIO1(2,1:RIN%NSD) = 2.986 ! assigned before call inversion()
        RATIO(1,1:RIN%NSD) = RIN%RATIO1(1,1)
        RATIO(2,1:RIN%NSD) = RIN%RATIO1(2,1)
        if ( RIN%NDIM%n2(IDIM1) .eq. 1 ) then
          IDIM2 = 1
          IDIM3 = 1
          II = RIN%NDIM%ISTARSING(IDIM2,IDIM1) + IDIM3 - 1
          SHD(IDIM3,1:RIN%NSD) = APSING1(II)
          IDIM3 = 2
          SHD(IDIM3,1:RIN%NSD) = 1.0 - SHD(IDIM3-1,1:RIN%NSD)
        else
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
          IDIM3 = 1
          II = RIN%NDIM%ISTARSING(IDIM2,IDIM1) + IDIM3 - 1
          SHD(IDIM3,IDIM2) = APSING1(II)
          IDIM3 = 2
          SHD(IDIM3,IDIM2) = 1.0 - SHD(IDIM3-1,IDIM2)
          enddo
        endif
      elseif ( par_type .eq. par_type_SHD_distr ) then ! retrieve axis ratio distribution
        if ( RIN%NDIM%n2(IDIM1) .eq. 1 .and. RIN%NSD .gt. 1 ) then
          IDIM2 = 1
          NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
          if ( .not. RIN%indep_par ) then
            do IDIM3=1,NDIM3
            RATIO(IDIM3,1:RIN%NSD) = RIN%RATIO1(IDIM3,IDIM2)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1) + IDIM3 - 1
            SHD(IDIM3,1:RIN%NSD) = APSING1(II)
            enddo
            NSHAPE(1:RIN%NSD) = NDIM3
          else
            do IDIM3=1,NDIM3
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1) + IDIM3 - 1
            SHD(IDIM3+1,1:RIN%NSD) = APSING1(II)
            enddo
            NDIM3 = NDIM3 + 1
            sum1 = 1.0 + sum(SHD(2:NDIM3,IDIM2))
            SHD(1,1:RIN%NSD) = 1.0 / sum1
            do IDIM3=2,NDIM3
            RATIO(IDIM3,1:RIN%NSD) = RIN%RATIO1(IDIM3,IDIM2)
            SHD(IDIM3,1:RIN%NSD) = SHD(IDIM3,IDIM2) / sum1
            enddo
            NSHAPE(1:RIN%NSD) = NDIM3
            RATIO(1,1:RIN%NSD) = RIN%RATIO1(1,IDIM2)
          endif
        else
          if ( .not. RIN%indep_par ) then
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do IDIM3=1,NDIM3
            RATIO(IDIM3,IDIM2) = RIN%RATIO1(IDIM3,IDIM2)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1) + IDIM3 - 1
            SHD(IDIM3,IDIM2) = APSING1(II)
            enddo
            NSHAPE(IDIM2) = NDIM3
            enddo
          else
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do IDIM3=1,NDIM3
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1) + IDIM3 - 1
            SHD(IDIM3+1,IDIM2) = APSING1(II)
            enddo
            NDIM3 = NDIM3 + 1
            sum1 = 1.0 + sum(SHD(2:NDIM3,IDIM2))
            SHD(1,IDIM2) = 1.0 / sum1
            do IDIM3=2,NDIM3
            RATIO(IDIM3,IDIM2) = RIN%RATIO1(IDIM3,IDIM2)
            SHD(IDIM3,IDIM2) = SHD(IDIM3,IDIM2) / sum1
            enddo
            NSHAPE(IDIM2) = NDIM3
            RATIO(1,IDIM2) = RIN%RATIO1(1,IDIM2)
            enddo ! IDIM2
          endif
        endif
      endif ! par_type .eq.

      return
      end subroutine unpack_SHD

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Relative Humidity

      subroutine unpack_RH ( RIN,IDIM1,APSING1,RH )

      use mod_par_inv, only : KPARS
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type

      implicit none
! ----------------------------------------------------------------
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  IDIM1
      real,dimension(KPARS),      intent(in)    ::  APSING1
      real,dimension(KSD),        intent(out)   ::  RH
! ----------------------------------------------------------------
      integer              :: IDIM2, NDIM2, II
! ----------------------------------------------------------------
      RH = 0.0

      NDIM2 = RIN%NDIM%n2(IDIM1)

      if ( NDIM2 .eq. RIN%NSD ) then
        do IDIM2=1,NDIM2
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
            RH(IDIM2) = APSING1(II)
        enddo ! IDIM2
      else
            II = RIN%NDIM%ISTARSING(1,IDIM1)
            RH(1:RIN%NSD) = APSING1(II)
      endif
      !write(*,*) 'NSD =',RIN%NSD,'  NDIM2 =',NDIM2,'  RH(1:NSD):',RH(1:RIN%NSD),'  in unpack_RH'

      return
      end subroutine unpack_RH

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Real part of complex refractive index n(wl) or Chemistry components

      subroutine unpack_REFI ( RIN,par_type,IDIM1,APSING1,REFI )
      
      use mod_par_inv, only : KPARS,KIDIM2,KIDIM3
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type
      
      implicit none
! ----------------------------------------------------------------	  
      type(retr_input_settings),  intent(in)  ::  RIN
      integer,                    intent(in)  ::  par_type,IDIM1
      real,dimension(KPARS),      intent(in)  ::  APSING1
      real,dimension(KIDIM3,KIDIM2),intent(out)  ::	REFI
! ----------------------------------------------------------------	  
      integer                              :: IDIM2,IDIM3,II
! ----------------------------------------------------------------	  
      REFI(:,:) = 0.0
     
      do IDIM2=1,RIN%NDIM%n2(IDIM1) 
        do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
          II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1                     
          if(RIN%NDIM%n2(IDIM1) .eq. 1) then
            REFI(IDIM3,1:RIN%NSD) = APSING1(II)
          else
            REFI(IDIM3,IDIM2) = APSING1(II)
          endif ! RIN%NDIM%n2(IDIM1) .lt. RIN%NSD
        enddo ! IDIM3
      enddo ! IDIM2   
          
      return
      end subroutine unpack_REFI 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Notmalization of Size Distribution for forward model

      subroutine get_SD_normalized ( RIN,par_type,C0,NBIN,RADIUS,SD )

      use mod_par_inv, only : KIDIM3
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type

      implicit none
! ----------------------------------------------------------------
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  par_type
      integer,dimension(KSD),     intent(in)    ::	NBIN
      real,dimension(KIDIM3,KSD), intent(in)    ::	RADIUS
      real,dimension(KSD),        intent(in)    ::	C0
      real,dimension(KIDIM3,KSD), intent(inout) ::	SD
! ----------------------------------------------------------------
      real      :: AA
      integer   :: ISD
! ----------------------------------------------------------------
      AA = 0.0
      do ISD=1,RIN%NSD
        if(par_type .eq. par_type_SD_LN) then
          SD(NBIN(ISD)+1,ISD) = C0(ISD)
!          write(*,*) 'in get_SD_normalized 1: ',ISD,C0(ISD)
!          write(*,*) 'in get_SD_normalized 2: ',ISD,SD(1:NBIN(ISD)+1,ISD)
        else
          AA = SUM(SD(1:NBIN(ISD),ISD))
          if(par_type .eq. par_type_SD_TB) AA = AA*( LOG(RADIUS(2,ISD))-LOG(RADIUS(1,ISD)) )
          SD(1:NBIN(ISD),ISD) = SD(1:NBIN(ISD),ISD)/AA*C0(ISD)
!          write(*,*) 'in get_SD_normalized 1: ',ISD,C0(ISD),AA
!          write(*,*) 'in get_SD_normalized 2: ',ISD,SD(1:NBIN(ISD),ISD)
!          write(*,*) 'in get_SD_normalized 3: ',ISD,par_type,par_type_SD_TB
      endif
      enddo ! ISD

      return
      end subroutine get_SD_normalized

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Set concentration in SD array to be used for single scattering properties

      subroutine set_SD_LN_concentration ( RIN,C0,NBIN,SD )
      
      use mod_par_inv, only : KIDIM3
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type
      
      implicit none
! ----------------------------------------------------------------	  
      type(retr_input_settings),  intent(in)    ::  RIN
      real,dimension(KSD),        intent(in)    ::	C0
      integer,dimension(KSD),     intent(inout) ::	NBIN
      real,dimension(KIDIM3,KSD), intent(inout) ::	SD
! ----------------------------------------------------------------
      integer   :: ISD
      real,dimension(KIDIM3) ::	SD_aux
! ----------------------------------------------------------------


      do ISD=1,RIN%NSD
        SD(NBIN(ISD)+1,ISD) = C0(ISD)
!          write(*,*) 'in get_SD_LN_concentration 1: ',ISD,C0(ISD)
!          write(*,*) 'in get_SD_LN_concentration 2: ',ISD,SD(1:NBIN(ISD)+1,ISD)
      enddo

      return
      end subroutine set_SD_LN_concentration

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Concentration is applied to Size Distribution

      subroutine get_SD_bins_concentrations ( RIN,par_type,C0,NBIN,RADIUS,SD )
      
      use mod_par_inv, only : KIDIM3
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type
      
      implicit none
! ----------------------------------------------------------------	  
      type(retr_input_settings),  intent(in)    ::  RIN
      integer,                    intent(in)    ::  par_type
      real,dimension(KSD),        intent(in)    ::	C0
      integer,dimension(KSD),     intent(inout) ::	NBIN
      real,dimension(KIDIM3,KSD), intent(inout) ::	RADIUS
      real,dimension(KIDIM3,KSD), intent(inout) ::	SD
! ----------------------------------------------------------------
      integer   :: ISD
      real,dimension(KIDIM3) ::	SD_aux
! ----------------------------------------------------------------
      do ISD=1,RIN%NSD
        NBIN(ISD) = NBIN(ISD) + 1
        RADIUS(NBIN(ISD),ISD) = RIN%RADIUS1(NBIN(ISD),ISD)
        call set_all_bins ( NBIN(ISD), SD(1:NBIN(ISD),ISD) )
        SD(1:NBIN(ISD),ISD) = C0(ISD) * SD(1:NBIN(ISD),ISD)
        if(par_type .eq. par_type_SD_TB) &
        SD(1:NBIN(ISD),ISD) = SD(1:NBIN(ISD),ISD) / &
                                  ( LOG(RADIUS(2,ISD))-LOG(RADIUS(1,ISD)) )
!         write(*,*) 'in get_SD_concentrations 1: ',ISD,C0(ISD)
!         write(*,*) 'in get_SD_concentrations 2: ',ISD,SD(1:NBIN(ISD),ISD)
!         write(*,*) 'in get_SD_concentrations 3: ',ISD,par_type
      enddo
          
      return
      end subroutine get_SD_bins_concentrations

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Add a bin (first bin) used for Size Distribution normalization
      subroutine set_all_bins ( NBIN, SD )

      use mod_retr_settings_derived_type
      
      implicit none
! ----------------------------------------------------------------
      integer,               intent(in)    ::	NBIN
      real,dimension(NBIN),   intent(inout) ::	SD
! ----------------------------------------------------------------
      real,dimension(NBIN+1) ::	SD_aux
! ----------------------------------------------------------------
      SD_aux(2:NBIN) = SD(1:NBIN-1)
      SD(1) = 1.0 / ( 1+sum(SD_aux(2:NBIN)) )
      SD(2:NBIN) = SD_aux(2:NBIN) * SD(1)
          
      return
      end subroutine set_all_bins

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Vertical distribution at retrieved altitudes
      subroutine set_vd_all_retr_altitudes ( n, h, vd )

      use mod_retr_settings_derived_type
      use mod_par_OS, only : HMAX_atm

      implicit none
! ----------------------------------------------------------------
      integer,          intent(in)    ::	 n   !> number of altitudes for retrieved verticle profile
      real,dimension(n), intent(inout) ::	 h   !> altitude values (km)
      real,dimension(n), intent(inout) :: vd  !> vertical distribution function values
! ----------------------------------------------------------------
      real,dimension(n) :: dx
      real,dimension(n) :: h_rev, vd_rev
      integer :: i
      real :: xnorm
! ----------------------------------------------------------------
      h_rev(n:1:-1) = h(1:n)
      vd_rev(1:n) = 0.0
      vd_rev(n:2:-1) = vd(1:n-1) ! reverse vd elements in ascending order of altitudes
      do i=1,n-1
      dx(i) = h_rev(i+1) - h_rev(i)
      enddo
      dx(n) = HMAX_atm * 0.001 - h_rev(n)
!write(*,*) 'h_rev:'
!write(*,'(10es12.4)') h_rev(1:n)
!write(*,*) 'vd_rev:'
!write(*,'(10es12.4)') vd_rev(1:n)
!write(*,*) 'dx:'
!write(*,'(10es12.4)') dx(1:n)
      call compute_distribution ( n, dx, vd_rev )
      vd(1:n) = vd_rev(n:1:-1) ! reset vd elements in descending order of altitudes

      return
      end subroutine set_vd_all_retr_altitudes

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Compute full distribution function from n-1 retrieved parameters
! norm = 0.5*(1.+y(2))*dx(1) + 0.5*(y(2)+y(3))*dx(2) + ... + 0.5*(y(n)+0.0)*dx(n)
! n - number of altitudes for retrieved vertical distribution
! (n-1) - number of measurement altitudes
! h(1) = h_masl_km, h(2) = h_meas(1), ..., h(n) = h_meas(n-1)
! dx(n) = h_atm_max_km - h(n)
! y(1) =   1. / norm
! y(2) = y(2) / norm
! ...
! y(n) = y(n) / norm

      subroutine compute_distribution ( n, dx, y )

      use mod_retr_settings_derived_type
      
      implicit none
! ----------------------------------------------------------------
      integer,           intent(in)    ::	n
      real,dimension(n), intent(in)    ::	dx
      real,dimension(n), intent(inout) :: y
! ----------------------------------------------------------------
      real,dimension(n) ::	y_aux
      integer :: i
      real :: xnorm, xnorm1
! ----------------------------------------------------------------
      y_aux(2:n) = y(2:n)
      y_aux(1) = 1.0
      xnorm = 0.0
      do i=1,n-1
      xnorm = xnorm + 0.5*(y_aux(i)+y_aux(i+1))*dx(i)
      enddo
      xnorm = xnorm + 0.5*(y_aux(n)+0.0)*dx(n)

      do i=1,n
      y(i) = y_aux(i) / xnorm
      enddo

      !goto 44
      ! test xnorm
      xnorm = 0.0
      do i=1,n-1
      xnorm = xnorm + 0.5*(y(i)+y(i+1))*dx(i)
      enddo
      xnorm1 = xnorm
      xnorm = xnorm + 0.5*(y(n)+0.0)*dx(n)
!AL      write(*,*) 'n = ',n,'xnorm =',xnorm,'xnorm1 =',xnorm1
  44  continue

      return
      end subroutine compute_distribution

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Size Distribution for forward model

      subroutine unpack_SD ( RIN,par_type,IDIM1,APSING1,   &
                             NBIN,RADIUS,SD )
      
      use mod_par_inv, only : KPARS,KIDIM3 ! KIDIM2,
      use mod_par_OS,  only : KSD
      use mod_retr_settings_derived_type
      
      implicit none
! ----------------------------------------------------------------	  
      type(retr_input_settings),  intent(in)  ::  RIN
      integer,                    intent(in)  ::  par_type,IDIM1
      real,dimension(KPARS),      intent(in)  ::  APSING1
      integer,dimension(KSD),     intent(out) ::	NBIN
      real,dimension(KIDIM3,KSD), intent(out) ::	RADIUS,SD      
! ----------------------------------------------------------------	  
      integer  ::  IDIM2,IDIM3,II
! ----------------------------------------------------------------	  
      NBIN(:)     = 0
      RADIUS(:,:) = 0.0
      SD(:,:)     = 0.0
      
      do IDIM2=1,RIN%NDIM%n2(IDIM1) 
        NBIN(IDIM2)=RIN%NDIM%n3(IDIM2,IDIM1)
        do IDIM3=1,NBIN(IDIM2)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1                     
            SD(IDIM3,IDIM2) = APSING1(II)
            RADIUS(IDIM3,IDIM2) = RIN%RADIUS1(IDIM3,IDIM2)
        enddo ! IDIM3
      enddo ! IDIM2  
          
      return
      end subroutine unpack_SD

!s!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Normalize particular retrieved characteristics in vector ap (APSING)
! for single pixel :
!     - size distribution
!     - shape distribution
!     - aerosol vertical profile
!     - chemistry, practical mixture

      subroutine normalize_vector_par_single_pixel(RIN,segment_meas,ipix,APSING)

      use mod_par_inv, only : KVERTM, KIMAGE
      use mod_par_OS, only : HMAX_atm
      use mod_retr_settings_derived_type
      use mod_sdata_derived_type
      use mod_sdata, only : get_HVP_lidar
      use mod_sdata_meas_type, only : meas_type_lid_beg, meas_type_lid_end

      implicit none
!	------------------------------------------------------------------------------------------------------
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data), intent(in) :: segment_meas
      integer, intent(in) :: ipix
      real, dimension(KPARS), intent(inout) :: APSING
!	------------------------------------------------------------------------------------------------------
      integer ::  IDIM1, IDIM2, par_type
      integer ::  NBIN, ibeg, iend, IW
      real    ::  xnorm, dlnr, HGR_km
      integer :: NHVP_meas, NH
      real, dimension(KVERTM,KIMAGE) :: HVP_meas_m
      real, dimension(KVERTM) :: HVP_retr_km, H_km, prof_temp
      character(len=20) :: distr_type
      real    :: inclination_angle
!	------------------------------------------------------------------------------------------------------
      if ( .not. RIN%indep_par ) then
        do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        select case ( par_type )
        case ( par_type_SD_TB )
        ! Size distribution (triangle bins)
          if(any(RIN%NDIM%par_type(1:RIN%NDIM%n1) .eq. par_type_Cv)) then
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
              dlnr = log(RIN%RADIUS1(2,IDIM2))-log(RIN%RADIUS1(1,IDIM2))
              NBIN = RIN%NDIM%n3(IDIM2,IDIM1)
              ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
              iend = ibeg + NBIN - 1
              xnorm = sum(APSING(ibeg:iend))
              APSING(ibeg:iend) = APSING(ibeg:iend) / (xnorm*dlnr)
            enddo ! IDIM2
          endif
        case ( par_type_SD_LB, par_type_SD_MD )
        ! Size distribution (precomputed lognormal bins)
            if(any(RIN%NDIM%par_type(1:RIN%NDIM%n1) .eq. par_type_Cv)) then
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
              NBIN = RIN%NDIM%n3(IDIM2,IDIM1)
              ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
              iend = ibeg + NBIN - 1
              xnorm = sum(APSING(ibeg:iend))
              APSING(ibeg:iend) = APSING(ibeg:iend) / xnorm
            enddo ! IDIM2
            endif
        case ( par_type_SHD_distr )
        ! Shape distribution
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
              NBIN = RIN%NDIM%n3(IDIM2,IDIM1)
              ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
              iend = ibeg + NBIN - 1
              xnorm = sum(APSING(ibeg:iend))
              APSING(ibeg:iend) = APSING(ibeg:iend) / xnorm
            enddo ! IDIM2
        case ( par_type_AVP_prof )
        ! Aerosol Vertical Profile
            do IW=1,RIN%NW
            if (ANY((segment_meas%pixels(ipix)%meas(IW)%meas_type .GE. meas_type_lid_beg) .AND. &
            (segment_meas%pixels(ipix)%meas(IW)%meas_type .LE. meas_type_lid_end))) then
            inclination_angle=segment_meas%pixels(ipix)%meas(IW)%SZA
            exit
            endif
            enddo !IW
            HGR_km = segment_meas%pixels(ipix)%MASL*0.001
            call get_HVP_lidar ( segment_meas, NHVP_meas, HVP_meas_m )
            distr_type = 'lut'
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
              NBIN = RIN%NDIM%n3(IDIM2,IDIM1)
              ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
              iend = ibeg + NBIN - 1
              call get_AVP_altitudes_retr ( HGR_km,                      &
                                            NHVP_meas, HVP_meas_m(:,ipix)*0.001, &
                                            inclination_angle, &
                                            NBIN, HVP_retr_km )
              ! Altitudes for aerosol profile normalization
              call grid_altitudes_LUT_with_gas ( HGR_km, HMAX_atm*0.001,     & ! IN
                                        NBIN, HVP_retr_km(1:NBIN),  &
                                        NH, H_km(1:NBIN+2)          & ! OUT
                                      )
              ! Aerosol concentration profile normalization
              call discrvd_single_atm_component ( distr_type, 0.0, 0.0,       &
                                                  NBIN, HVP_retr_km(1:NBIN),  &
                                                  APSING(ibeg:iend),          &
                                                  NH, H_km(1:NH),             &
                                                  xnorm, prof_temp(1:NH)      & ! OUT
                                                )
              APSING(ibeg:iend) = APSING(ibeg:iend) / xnorm
            enddo ! IDIM2
        case ( par_type_CXRI_nmix )
        ! Chemistry, linear volume mixture
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
              NBIN = RIN%NDIM%n3(IDIM2,IDIM1)
              ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
              iend = ibeg + NBIN - 1
              xnorm = sum(APSING(ibeg:iend))
              APSING(ibeg:iend) = APSING(ibeg:iend) / xnorm
            enddo ! IDIM2
        end select
        enddo ! IDIM1
      endif ! .not. RIN%indep_par

      return
      end subroutine normalize_vector_par_single_pixel

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
