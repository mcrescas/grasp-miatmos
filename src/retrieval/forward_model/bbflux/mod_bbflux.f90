! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

      MODULE mod_bbflux
!
      IMPLICIT NONE
      CONTAINS
!
      SUBROUTINE bbflux_pixel (                             &
                                 iu_main_output,            & ! IN
                                 RIN,ipix,h1,lat1,          &
                                 NHVP_meas,HVP_meas,        &
                                 nwl,wl,AP,                 &
                                 MDPR,                      & ! molecular depolarization, added by Qiaoyun HU
                                 pixel_fit,                 & ! INOUT
                                 GOUT_aerosol,              &
                                 GOUT_bbflux_pixel,         &
                                 GOUT_forcing_pixel,        &
                                 KERNELS1,KERNELS2          &
                              )
      use mod_par_inv, only : KW, KVERTM, KPARS, KIDIM2, KIDIM3, KBF, KSHAPE
      use mod_par_OS,  only : KSD, HMAX_atm, N_SUB_CHANNEL_MAX
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_alloc_kernels
      use mod_c_utils
!XH   modules to convert seconds to date string
      use mod_time_utils, only : convert_time_to_string
!XH   modules related to gas absorption
      use mod_abs_kd
!XH   Gaussian quadrature
      use Mod_BRM, only : gauss_l
!XH   module to unpack aerosol and surface properties
      use mod_forward_model_characteristics
!	------------------------------------------------------------------------------------------------------
! IN :
      integer,                    intent(in)  ::  iu_main_output
      integer,                    intent(in)  ::  ipix
      real*8,                     intent(in)  ::  h1,lat1
      integer,                    intent(in)  ::  NHVP_meas ! number of heights for lidar measurements
      real,dimension(KVERTM),     intent(in)  ::  HVP_meas  ! heights for lidar measurements
      integer,                    intent(in)  ::  nwl
      real,dimension(KW),         intent(in)  ::  wl
      real,dimension(KPARS),      intent(in)  ::  AP
      real,dimension(KW),         intent(in)  ::  MDPR  ! added by Qiaoyun HU
      type(output_segment_particles),   intent(in)  ::  GOUT_aerosol
      type(pixel),                      intent(in)  ::  pixel_fit
      
!	------------------------------------------------------------------------------------------------------
! INOUT :
      type(retr_input_settings),     intent(inout)  ::  RIN
      !type(pixel),                   intent(inout)  ::  pixel_fit
      type(kernels_triangle_bin),    intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),   intent(inout)  ::  KERNELS2
      type(output_pixel_bbflux),     intent(inout)  ::  GOUT_bbflux_pixel
      type(output_pixel_forcing),    intent(inout)  ::  GOUT_forcing_pixel
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      integer                             ::  keyEL_origin
      real                                ::  AOD_550
      real                                ::  HOBS_km      ! height of observations
      real                                ::  HGR_km       ! height above sea level
      real                                ::  HMAX_atm_km  ! top of atmosphere
      real                                ::  inclination_angle  ! inclination angle of HVP_meas
      real,dimension(KVERTM)              ::  HVP_meas_km
      real,dimension(KPARS)               ::  APSING
      type(pixel)                         ::  pixel_fit_temp
      type(output_segment_particles)      ::  GOUT_aerosol_temp
      type(output_segment_gases)          ::  GOUT_gases_temp
      type(output_segment_surface)        ::  GOUT_surface_temp
      character (len=GBL_FILE_PATH_LEN)   ::  internal_file_path
      character (len=GBL_FILE_PATH_LEN)   ::  external_file_path
!	------------------------------------------------------------------------------------------------------
!      type(forward_model_characteristics) :: FMCHAR
      type(forward_model_characteristics_particles)  :: forw_aerosol
      type(forward_model_characteristics_gases)  :: forw_gases
      type(forward_model_characteristics_surface)    :: forw_surface
!	------------------------------------------------------------------------------------------------------
      integer	                            ::  II,IW,ILV
      integer	                            ::  IDIM1
      integer                             ::  NHVP_retr
      real,dimension(KVERTM)              ::  HVP_retr_km
!	------------------------------------------------------------------------------------------------------
      !real,dimension(KIDIM3,KSD)          ::  RADIUS,SD
      !real,dimension(KSHAPE,KSD)          ::  RATIO,SHD
!	------------------------------------------------------------------------------------------------------
      integer	                            ::  par_type, par_type_SD
!	------------------------------------------------------------------------------------------------------
!     spectral dependent parameters
      real,dimension(KW)                  ::  AODS
      real,dimension(KBF,KW)              ::  BRFS_land, BRPS_land,BRMS_water
      real,dimension(KSD,KW)              ::  RREALS, RIMAGS
      integer                             ::  iBRF_land,iBRP_land,iBRM_water
      real,dimension(KBF)                 ::  BRF_land,BRP_land,BRM_water
      real,dimension(KSD)                 ::  RREAL, RIMAG
      integer                             ::  INU
      real                                ::  WNM, CSOL, tau_mol
      real                                ::  WVL
      type(output_pixel_bbflux)           ::  GOUT_bbflux_pixel_temp
!	------------------------------------------------------------------------------------------------------
!     k-distribution data
      integer                             ::  IATM
      real                                ::  PSRF, UH2O, UO3
      integer,dimension(NCORPS)           ::  IABS, ICONT
      type (DATA_ABS)                     ::  abs_data_forw_im
!	------------------------------------------------------------------------------------------------------
!     daily average
      character(len=12)                   ::  datestr
      integer, parameter                  ::  NSZA = 4
      double precision, dimension(NSZA)   ::  SZA, WT
      real                                ::  daytime, minsza
!	------------------------------------------------------------------------------------------------------
!    ------------------------------------------------------------------------------------------------------
     real, dimension(N_SUB_CHANNEL_MAX)            :: bandwidth = 1.0
     real,dimension(N_SUB_CHANNEL_MAX)             :: wl_Subchannels
     integer                                       :: NSubchannels = 1
     real,dimension(KSD,N_SUB_CHANNEL_MAX)         :: RREAL_Subchannels,RIMAG_Subchannels
!    ------------------------------------------------------------------------------------------------------
      call cstring2fstring(RIN%DLSF%external_file_path, external_file_path)
      call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)
      
!XH   information that may be used and changed in the radiative transfer part
      pixel_fit_temp = pixel_fit
!	------------------------------------------------------------------------------------------------------
!XH   perform broadband flux calculation only for triangle bins and bi-modal lognormal distribution
      if (RIN%DLSF%IWL .ne. 0) then
         write(*,'(a,i0,/,a)') 'STOP in bbflux_IMAGE_I, RIN%DLSF%IWL = ', &
         RIN%DLSF%IWL,'Broadband flux calculation is not supported for precomputed lognormal bins!'
         stop
      end if
!	------------------------------------------------------------------------------------------------------
!XH   perform scalar radiative transfer calculation for forcing
      keyEL_origin   = RIN%DLSF%keyEL
      RIN%DLSF%keyEL = 1
!	------------------------------------------------------------------------------------------------------
      HOBS_km     = pixel_fit%HOBS * 0.001
      HGR_km      = pixel_fit%MASL * 0.001
      HMAX_atm_km = HMAX_atm       * 0.001
      DO IW=1,RIN%NW
         if(ANY((pixel_fit%meas(IW)%meas_type .GE. meas_type_lid_beg) .AND.&
            (pixel_fit%meas(IW)%meas_type .LE. meas_type_lid_end)))    then
            inclination_angle=pixel_fit%meas(IW)%SZA
            exit
         endif
      ENDDO !IW

      if (NHVP_meas .gt. 1) then
         HVP_meas_km(1:NHVP_meas) = HVP_meas(1:NHVP_meas) * 0.001
      end if
!	------------------------------------------------------------------------------------------------------

      select case(RIN%KL) 
      case(1)
        APSING(1:RIN%KNSING) = EXP(AP(1:RIN%KNSING))
      case(0) 
        APSING(1:RIN%KNSING) =     AP(1:RIN%KNSING)
      end select

!XH   Unpack parameter vector AP (driving forward model)
      call unpack_parameter_vector_ap(  RIN,APSING,                &
                                        forw_aerosol,              &
                                        forw_gases,                &
                                        forw_surface,              &
                                        ipix, pixel_fit=pixel_fit, &
                                        HGR_km=HGR_km,             &
                                        NHVP_meas=NHVP_meas,       &
                                        HVP_meas_km=HVP_meas_km,   &
                                        inclination_angle=inclination_angle )

!XH   get surface model type
      call get_radiative_transfer_SOS_flags_surf ( RIN,iBRF_land,iBRP_land,iBRM_water )

!XH   get retrieved parameters for the instrument bands
      AODS(1:nwl) = GOUT_aerosol%opt%pixel(ipix)%wl(1:nwl)%extt
      DO IW=1,nwl
!        Surface parameters at IW-th wavelength
         call get_SURF_wl ( RIN,IW,IW,forw_surface%BRF_land,forw_surface%BRP_land,forw_surface%BRM_water,  &
                                      BRFS_land(:,IW),BRPS_land(:,IW),BRMS_water(:,IW) )
!        Complex refractive index at IW-th wavelength
         WL_Subchannels = RIN%WAVE(IW)
         call get_REFI_wl ( RIN,                                     &
                            IW,IW,                                   &
                            Nsubchannels,                            &
                            forw_aerosol%RH,                         &
                            bandwidth,                               &
                            WL_Subchannels,                          &
                            GOUT_aerosol%chem%pixel(ipix),           &
                            forw_aerosol%RREAL,forw_aerosol%RIMAG,   &
                            RREAL_Subchannels,RIMAG_Subchannels,     &
                            RREALS(:,IW),RIMAGS(:,IW) )
      END DO
!XH   renormalize solar spectrum
      CSOL=CSLR/SUM(FSLR)
!XH   absorbers: H2O, CO2, O3, N2O, CO, CH4, O2
      IABS=(/1,1,1,1,1,1,1/)
!XH   continuum: H2O, CO2, O3, N2, O2, NO2, SO2
      ICONT=(/1,1,1,0,1,1,1/)
!XH   atmospheric profile: 0. User, 1. Tropical, 2. MLS, 3. MLW, 4. SAS, 5. SAW, 6. USST62
      IATM=6
!XH   surface pressure, -1 for McClatchey surface pressure
      PSRF=-1.0
!XH   water vapor content (g/cm2), -1 for McClatchey content
      UH2O=-1.0
!XH   ozone content (Dobson = cm.atm*1000), -1 for McClatchey content
      UO3 =-1.0
!XH   obtain k-distribution data
      call GET_ABS_DATA(TRIM(internal_file_path),IABS,ICONT,IATM,PSRF,UH2O,UO3,abs_data_forw_im)
!XH   setup interested levels
      GOUT_bbflux_pixel_temp%NHLV=GOUT_bbflux_pixel%NHLV
      GOUT_bbflux_pixel_temp%HLV =GOUT_bbflux_pixel%HLV
!XH   find out the duration of daylight and minimum SZA
      call convert_time_to_string(pixel_fit%t,"%F",datestr)
      call DAYTIME_MY(datestr,real(h1,4),real(lat1,4),daytime,minsza)
      call gauss_l(NSZA,NSZA,real(minsza,8),90.D0,SZA,WT)
      GOUT_bbflux_pixel%BBUFX0 = 0.0
      GOUT_bbflux_pixel%BBDFX0 = 0.0
      GOUT_bbflux_pixel%BBUFXA = 0.0
      GOUT_bbflux_pixel%BBDFXA = 0.0

!!XH   print instantaneaous flux at 20.0, 40.0, 60.0, and 80.0 degrees
!      do II = 0, 4
!!XH      set solar zenith angle
!         abs_data_forw_im%SZA= 20.0*II
!         call instant_bbflux (                                              &
!                                RIN,ipix,h1,lat1,                           & ! IN
!                                HOBS_km,HGR_km,                             &
!                                HMAX_atm_km,HVP_meas_km,                    &
!                                NHVP_meas,HVP_meas,                         &
!                                forw_aerosol%NHVP_retr,forw_aerosol%HVP_retr_km,        &
!                                IWb,IWe,wl,                                 &
!                                BRFS_land,BRPS_land,BRMS_water,             &
!                                RIN%NSD,forw_aerosol%NBIN,forw_aerosol%RADIUS,forw_aerosol%SD,&
!                                RREALS,RIMAGS,                              &
!                                forw_aerosol%NSHAPE,forw_aerosol%RATIO,forw_aerosol%SHD,      &
!                                forw_aerosol%H0,forw_aerosol%sigma_aerosol,forw_aerosol%CL,   &
!                                abs_data_forw_im,                           &
!                                pixel_fit,                                  & ! INOUT
!                                KERNELS1,KERNELS2,                          &
!                                GOUT_bbflux_pixel_temp,                     &
!                                GOUT_aerosol,                               &
!                                GOUT_surface                                &
!                             )
!         print *, 'Pixel at latitude: ',lat1,' longitude: ',h1
!         print *, abs_data_forw_im%sza
!         print *, '     HLV(km)   Flux0Up(w/m^2) Flux0Down(w/m^2)   Flux Up(w/m^2) Flux Down(w/m^2) NetForcing(w/m^2)'
!         do ILV = 1, GOUT_bbflux_pixel_temp%NHLV
!            print *, GOUT_bbflux_pixel_temp%HLV(ILV), GOUT_bbflux_pixel_temp%BBUFX0(ILV), GOUT_bbflux_pixel_temp%BBDFX0(ILV), &
!                                                      GOUT_bbflux_pixel_temp%BBUFXA(ILV), GOUT_bbflux_pixel_temp%BBDFXA(ILV), &
!                                                      GOUT_bbflux_pixel_temp%BBUFX0(ILV) -GOUT_bbflux_pixel_temp%BBDFX0(ILV)  &
!                                                     -GOUT_bbflux_pixel_temp%BBUFXA(ILV) +GOUT_bbflux_pixel_temp%BBDFXA(ILV)
!         end do
!      end do

      do II = 1, NSZA
!XH      set solar zenith angle
         abs_data_forw_im%SZA=REAL(SZA(II),4)
         call instant_bbflux (                                              &
                                iu_main_output,                             & ! IN
                                RIN,ipix,h1,lat1,                           &
                                HOBS_km,HGR_km,                             &
                                HMAX_atm_km,HVP_meas_km,                    &
                                NHVP_meas,HVP_meas,                         &
                                forw_aerosol%NHVP_retr,forw_aerosol%HVP_retr_km, &
                                nwl,wl,                                     &
                                iBRF_land,iBRP_land,iBRM_water,             &
                                BRFS_land,BRPS_land,BRMS_water,             &
                                RIN%NSD,forw_aerosol%NBIN,forw_aerosol%RADIUS,forw_aerosol%SD, &
                                RREALS,RIMAGS,                              &
                                forw_aerosol%NSHAPE,forw_aerosol%RATIO,forw_aerosol%SHD,       &
                                forw_aerosol%H0,forw_aerosol%sigma,forw_aerosol%CL, &
                                MDPR,                                       &  ! added by Qiaoyun HU
                                abs_data_forw_im,                           &
                                forw_gases%n,forw_gases%C,                  &
                                pixel_fit_temp,                             & ! INOUT
                                KERNELS1,KERNELS2,                          &
                                GOUT_bbflux_pixel_temp,                     &
                                GOUT_aerosol_temp,                          &
                                GOUT_gases_temp,                            &
                                GOUT_surface_temp                           &
                             )
         GOUT_bbflux_pixel%BBUFX0=GOUT_bbflux_pixel%BBUFX0+REAL(WT(II),4)*GOUT_bbflux_pixel_temp%BBUFX0
         GOUT_bbflux_pixel%BBDFX0=GOUT_bbflux_pixel%BBDFX0+REAL(WT(II),4)*GOUT_bbflux_pixel_temp%BBDFX0
         GOUT_bbflux_pixel%BBUFXA=GOUT_bbflux_pixel%BBUFXA+REAL(WT(II),4)*GOUT_bbflux_pixel_temp%BBUFXA
         GOUT_bbflux_pixel%BBDFXA=GOUT_bbflux_pixel%BBDFXA+REAL(WT(II),4)*GOUT_bbflux_pixel_temp%BBDFXA
      end do
      GOUT_bbflux_pixel%BBUFX0=CSOL*daytime/24.0/(90.0-minsza)*GOUT_bbflux_pixel%BBUFX0
      GOUT_bbflux_pixel%BBDFX0=CSOL*daytime/24.0/(90.0-minsza)*GOUT_bbflux_pixel%BBDFX0
      GOUT_bbflux_pixel%BBUFXA=CSOL*daytime/24.0/(90.0-minsza)*GOUT_bbflux_pixel%BBUFXA
      GOUT_bbflux_pixel%BBDFXA=CSOL*daytime/24.0/(90.0-minsza)*GOUT_bbflux_pixel%BBDFXA
!XH   forcing product
      GOUT_forcing_pixel%NHLV   = GOUT_bbflux_pixel%NHLV
      GOUT_forcing_pixel%HLV    = GOUT_bbflux_pixel%HLV
      GOUT_forcing_pixel%NETFORC= GOUT_bbflux_pixel%BBUFX0 - GOUT_bbflux_pixel%BBDFX0     &
                                 -GOUT_bbflux_pixel%BBUFXA + GOUT_bbflux_pixel%BBDFXA
!XH   obtain AOD at 550 nm
      if (0.55 .le. wl(1)) then
         AOD_550 = AODS(1)*EXP(LOG(AODS(2)/AODS(1))/LOG(wl(2)/wl(1))*LOG(0.55/wl(1)))
      else if (0.55 .gt. wl(nwl)) then
         AOD_550 = AODS(nwl)*EXP(LOG(AODS(nwl-1)/AODS(nwl))/LOG(wl(nwl-1)/wl(nwl))*LOG(0.55/wl(nwl)))
      else
         do II = 2, nwl
            if (0.55 .gt. wl(II-1) .and. 0.55 .le. wl(II)) then
               AOD_550 = AODS(II-1)*EXP(LOG(AODS(II)/AODS(II-1))/LOG(wl(II)/wl(II-1))*LOG(0.55/wl(II-1)))
               exit
            end if
         end do
      end if
      GOUT_forcing_pixel%FORCEFF = GOUT_forcing_pixel%NETFORC/AOD_550

!      print *, 'Pixel at latitude: ',lat1,' longitude: ',h1
!      print *, '     HLV(km)   Flux0Up(w/m^2) Flux0Down(w/m^2)   Flux Up(w/m^2) Flux Down(w/m^2) NetForcing(w/m^2)'
!      do II = 1, GOUT_bbflux_pixel%NHLV
!         print *, GOUT_bbflux_pixel%HLV(II), GOUT_bbflux_pixel%BBUFX0(II), GOUT_bbflux_pixel%BBDFX0(II),   &
!                                             GOUT_bbflux_pixel%BBUFXA(II), GOUT_bbflux_pixel%BBDFXA(II),   &
!                                             GOUT_bbflux_pixel%BBUFX0(II) -GOUT_bbflux_pixel%BBDFX0(II)    &
!                                            -GOUT_bbflux_pixel%BBUFXA(II) +GOUT_bbflux_pixel%BBDFXA(II)
!      end do
!	------------------------------------------------------------------------------------------------------
!XH   set back the original number of elements
      RIN%DLSF%keyEL = keyEL_origin
!	------------------------------------------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE bbflux_pixel

      SUBROUTINE instant_bbflux (                                  &
                                   iu_main_output,                 &
                                   RIN,ipix,h1,lat1,               & ! IN
                                   HOBS_km,HGR_km,                 &
                                   HMAX_atm_km,HVP_meas_km,        &
                                   NHVP_meas,HVP_meas,             &
                                   NHVP_retr,HVP_retr_km,          &
                                   nwl,wl,                         &
                                   iBRF_land,iBRP_land,iBRM_water, &
                                   BRFS_land,BRPS_land,BRMS_water, &
                                   NSD,NBIN,RADIUS,SD,             &
                                   RREALS,RIMAGS,                  &
                                   NSHAPE,RATIOS,SHD,              &
                                   H0,sigma_aerosol,CL,            &
                                   MDPR,                           &  ! added by Qiaoyun HU
                                   abs_data_forw_im,               &
                                   NGAS,CGAS,                      &
                                   pixel_fit,                      & ! INOUT
                                   KERNELS1,KERNELS2,              &
                                   GOUT_bbflux_pixel,              &
                                   GOUT_aerosol,                   &
                                   GOUT_gases,                     &
                                   GOUT_surface                    &
                                )
      use mod_par_inv, only : KW, KVERTM, KIDIM3, KBF, KSHAPE
      use mod_par_OS,  only : KSD, NMG,N_SUB_CHANNEL_MAX,N_WL_CHANNEL_MAX
      use mod_molecular_scattering, only : rayleia
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_alloc_kernels
      USE MOD_RT_SOS
      use mod_forward_model
!XH   modules related to gas absorption
      use mod_abs_kd
      use mod_alloc_gas_lut

    use sub_gas_kd, only: DATA_GAS,NLEVEL_GAS
    use mod_alloc_gas_lut, only: filter_channel_type, filter_transmission

!	------------------------------------------------------------------------------------------------------
! IN :
      integer,                    intent(in)  ::  iu_main_output
      type(retr_input_settings),  intent(in)  ::  RIN
      real*8,                     intent(in)  ::  h1,lat1
      real,                       intent(in)  ::  HOBS_km      ! height of observations
      real,                       intent(in)  ::  HGR_km       ! height above sea level
      real,                       intent(in)  ::  HMAX_atm_km  ! top of atmosphere
      real,dimension(KVERTM),     intent(in)  ::  HVP_meas_km
      integer,                    intent(in)  ::  NHVP_meas ! number of heights for lidar measurements
      real,dimension(KVERTM),     intent(in)  ::  HVP_meas  ! heights for lidar measurements
      integer,                    intent(in)  ::  NHVP_retr
      real,dimension(KVERTM),     intent(in)  ::  HVP_retr_km
      integer,                    intent(in)  ::  nwl,ipix
      real,dimension(KW),         intent(in)  ::  wl
      integer,                    intent(in)  ::  iBRF_land,iBRP_land,iBRM_water
      real,dimension(KBF,KW),     intent(in)  ::  BRFS_land, BRPS_land, BRMS_water
      integer,                    intent(in)  ::  NSD
      integer,dimension(KSD),     intent(in)  ::  NBIN
      real,dimension(KIDIM3,KSD), intent(in)  ::  RADIUS,SD
      real,dimension(KSD,KW),     intent(in)  ::  RREALS, RIMAGS
      integer,dimension(KSD),     intent(in)  ::  NSHAPE
      real,dimension(KSHAPE,KSD), intent(in)  ::  RATIOS,SHD
      real,dimension(KSD),        intent(in)  ::  sigma_aerosol
      real,dimension(KW),         intent(in)  ::  CL
      real,dimension(KW),         intent(in)  :: MDPR
      integer,                    intent(in)  :: NGAS
      real,dimension(NMG),        intent(in)  :: CGAS

!	------------------------------------------------------------------------------------------------------
! INOUT :
      real,dimension(KVERTM,KSD),    intent(inout)  ::  H0
      type (DATA_ABS),               intent(inout)  ::  abs_data_forw_im
      type(pixel),                   intent(inout)  ::  pixel_fit
      type(kernels_triangle_bin),    intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),   intent(inout)  ::  KERNELS2
      type(output_pixel_bbflux),     intent(inout)  ::  GOUT_bbflux_pixel
      type(output_segment_particles),intent(inout)  ::  GOUT_aerosol
      type(output_segment_gases),    intent(inout)  ::  GOUT_gases
      type(output_segment_surface),  intent(inout)  ::  GOUT_surface
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      integer                           ::  NANG, II, NLV,NSPECIES
      real,dimension(KMpar)             ::  ANGL
      type(DATA_GAS)                    ::  gas_abs_data_forw_im
      logical                           ::  igab,ihyper,ikdist,ifilter,istdat,ATMOS_EMIS,SOLAR_EMIS
      real,dimension(NLEVEL_GAS,NMG)    ::  GAS_C_REF
      type(filter_transmission)             ::  filters_trans
      real,dimension(KSD,N_SUB_CHANNEL_MAX)  ::  RREAL_Subchannels,RIMAG_Subchannels
      REAL,DIMENSION(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX)   ::    WL_Planck
!	------------------------------------------------------------------------------------------------------
!     spectral dependent parameters
      real,dimension(KBF)                     ::  BRF_land,BRP_land,BRM_water
      real,dimension(KSD)                     ::  RREAL, RIMAG
      integer                                 ::  INU, Nsubchannels
      real                                    ::  WNM, tau_mol
      real                                    ::  WVL
      real, dimension(KW,N_SUB_CHANNEL_MAX)   ::  bandwidth,WL_Subchannels
      type(output_pixel_bbflux)               ::  GOUT_bbflux_pixel_temp
!	------------------------------------------------------------------------------------------------------
      igab = .FALSE.
      ihyper  = .FALSE.
      ikdist = .FALSE.
      ifilter = .FALSE.
      istdat = .FALSE.
      ATMOS_EMIS = .FALSE.
      SOLAR_EMIS = .FALSE.
      Nsubchannels = 1

!XH   setup interested levels
      GOUT_bbflux_pixel_temp%NHLV=GOUT_bbflux_pixel%NHLV
      GOUT_bbflux_pixel_temp%HLV =GOUT_bbflux_pixel%HLV
!XH   spectral integration from 2500 cm-1 to 50000 cm-1
!XH   interval is 100 cm-1 for wavenumber < 14400 cm-1
!XH   interval is 400 cm-1 for wavenumber > 14400 cm-1
      GOUT_bbflux_pixel%BBUFX0 = 0.0
      GOUT_bbflux_pixel%BBDFX0 = 0.0
      GOUT_bbflux_pixel%BBUFXA = 0.0
      GOUT_bbflux_pixel%BBDFXA = 0.0
      LOOP_SPECTRAL: DO INU = 1, NBNU
         IF (INU .LT. 120) THEN
            WNM= 2550.0+100.0*(INU-1  )
         ELSE
            WNM=14600.0+400.0*(INU-120)
         END IF
         WVL=1.0E+04/WNM
!XH      interpolate from retrieved surface parameters at instrument bands to those at other wavelengths
         call intrp_SURF_wl(1,nwl,wl,WVL,BRFS_land,BRPS_land,BRMS_water,BRF_land,BRP_land,BRM_water)
!XH      interpolate from retrieved refractive indices at instrument bands to those at other wavelengths
         call intrp_REFI_wl(1,nwl,wl,WVL,NSD,RREALS,RIMAGS,RREAL,RIMAG)
         RREAL_Subchannels(:,1) = RREAL
         RIMAG_Subchannels(:,1) = RIMAG
!         write(*,*) RREAL_Subchannels(:,1)
!         write(*,*) RREALS(1,1:nwl)
!         write(*,*) RREALS(2,1:nwl)
!         write(*,*) RIMAG_Subchannels(:,1)
!         write(*,*) RIMAGS(1,1:nwl)
!         write(*,*) RIMAGS(2,1:nwl)
!         write(*,*) wl(1:nwl)
!         write(*,*) WVL
!XH      molecular scattering optical thickness
         call rayleia(dble(WVL),h1,lat1,tau_mol)
         call forward_model_pixel_wl (  iu_main_output,                      & ! IN
                                        RIN,ipix,igab,ihyper,ikdist,ifilter,istdat,ATMOS_EMIS,SOLAR_EMIS,       &
                                        1,WVL,1,.false.,                     &
                                        NBIN,RADIUS,SD,                      &
                                        NSHAPE,RATIOS,SHD,                   &
                                        RREAL,RIMAG,                         &
                                        RIN%OSHF,                            &
                                        iBRF_land,iBRP_land,iBRM_water,      &
                                        BRF_land,BRP_land,BRM_water,tau_mol, &
                                        HOBS_km,HGR_km,HMAX_atm_km,          &
                                        NHVP_meas,HVP_meas_km,               &
                                        NHVP_retr,HVP_retr_km,               &
                                        H0,sigma_aerosol,CL(1),              &
                                        abs_data_forw_im,                    &
                                        gas_abs_data_forw_im,                &
                                        Nsubchannels,                        &
                                        WL_Subchannels(1,:),                 &
                                        WL_Planck,                           &
                                        bandwidth(1,:),                      &
                                        filters_trans,                &
                                        RREAL_Subchannels,RIMAG_Subchannels, &
                                        GAS_C_REF,NSPECIES,                  &
                                        NGAS, CGAS,                          &
                                        pixel_fit,                           & ! INOUT
                                        GOUT_aerosol,                        &
                                        GOUT_gases,                          &
                                        GOUT_surface,                        &
                                        1., 0., 1,                           & ! AL Mu=1., tau_mol_i=0.0, indwl_i=1 quick and dirty fix may not work!!!!!
                                        MDPR(1),                             &
                                        GOUT_bbflux_pixel_temp,              &
                                        NANG,ANGL,                           & ! OUT
                                        KERNELS1,KERNELS2                    &
                                     )
         GOUT_bbflux_pixel%BBUFX0=GOUT_bbflux_pixel%BBUFX0+FSLR(INU)*GOUT_bbflux_pixel_temp%BBUFX0
         GOUT_bbflux_pixel%BBDFX0=GOUT_bbflux_pixel%BBDFX0+FSLR(INU)*GOUT_bbflux_pixel_temp%BBDFX0
         GOUT_bbflux_pixel%BBUFXA=GOUT_bbflux_pixel%BBUFXA+FSLR(INU)*GOUT_bbflux_pixel_temp%BBUFXA
         GOUT_bbflux_pixel%BBDFXA=GOUT_bbflux_pixel%BBDFXA+FSLR(INU)*GOUT_bbflux_pixel_temp%BBDFXA
      END DO LOOP_SPECTRAL
!      print *, abs_data_forw_im%sza
!      print *, '     HLV(km)   Flux0Up(w/m^2) Flux0Down(w/m^2)   Flux Up(w/m^2) Flux Down(w/m^2)'
!      do II = 1, GOUT_bbflux_pixel%NHLV
!         print *, GOUT_bbflux_pixel%HLV(II), GOUT_bbflux_pixel%BBUFX0(II), GOUT_bbflux_pixel%BBDFX0(II),   &
!                                             GOUT_bbflux_pixel%BBUFXA(II), GOUT_bbflux_pixel%BBDFXA(II),   &
!                                             GOUT_bbflux_pixel%BBUFX0(II) -GOUT_bbflux_pixel%BBDFX0(II)    &
!                                            -GOUT_bbflux_pixel%BBUFXA(II) +GOUT_bbflux_pixel%BBDFXA(II)
!      end do
!      stop
!
      RETURN
      END SUBROUTINE instant_bbflux

      SUBROUTINE intrp_SURF_wl(IWb,IWe,wl,WVL,BRFS_land,BRPS_land,BRMS_water,   &
                                              BRF_land,  BRP_land,BRM_water  )
!
      use mod_par_inv, only : KW,KBF
!
      integer,                    intent(in)  ::  IWb, IWe
      real,dimension(KW),         intent(in)  ::  wl
      real,                       intent(in)  ::  WVL
      real,dimension(KBF,KW),     intent(in)  ::  BRFS_land, BRPS_land, BRMS_water
      real,dimension(KBF),        intent(out) ::  BRF_land,BRP_land,BRM_water
!
      integer :: IW
!XH   assuming wl(IW) < wl(IW+1) always
      IF (WVL .LE. wl(IWb)) THEN
         BRF_land = BRFS_land(:,IWb)
         BRP_land = BRPS_land(:,IWb)
         BRM_water= BRMS_water(:,IWb)
      ELSE IF (WVL .GE. wl(IWe)) THEN
         BRF_land = BRFS_land(:,IWe)
         BRP_land = BRPS_land(:,IWe)
         BRM_water= BRMS_water(:,IWb)
      ELSE
         DO IW=IWb,IWe-1
            IF (wl(IW) .LT. WVL .AND. WVL .LT. wl(IW+1)) THEN
               BRF_land = BRFS_land(:,IW)+(WVL-wl(IW))/(wl(IW+1)-wl(IW))                  &
                                            *(BRFS_land(:,IW+1)-BRFS_land(:,IW))
               BRP_land = BRPS_land(:,IW)+(WVL-wl(IW))/(wl(IW+1)-wl(IW))                  &
                                            *(BRPS_land(:,IW+1)-BRPS_land(:,IW))
               BRM_water=BRMS_water(:,IW)+(WVL-wl(IW))/(wl(IW+1)-wl(IW))                  &
                                            *(BRMS_water(:,IW+1)-BRMS_water(:,IW))
               EXIT
            END IF
         END DO
      END IF
!
      RETURN
      END SUBROUTINE intrp_SURF_wl

      SUBROUTINE intrp_REFI_wl(IWb,IWe,wl,WVL,NSD,RREALS,RIMAGS,  &
                                                  RREAL, RIMAG  )
!
      use mod_par_inv, only : KW,KBF
      use mod_par_OS,  only : KSD
!
      integer,                    intent(in)  ::  IWb, IWe, NSD
      real,dimension(KW),         intent(in)  ::  wl
      real,                       intent(in)  ::  WVL
      real,dimension(KSD,KW),     intent(in)  ::  RREALS, RIMAGS
      real,dimension(KSD),        intent(out) ::  RREAL, RIMAG
!
      integer :: IW
!XH   assuming wl(IW) < wl(IW+1) always
      IF (WVL .LE. wl(IWb)) THEN
         RREAL(1:NSD) = RREALS(1:NSD,IWb)
         RIMAG(1:NSD) = RIMAGS(1:NSD,IWb)
      ELSE IF (WVL .GE. wl(IWe)) THEN
         RREAL(1:NSD) = RREALS(1:NSD,IWe)
         RIMAG(1:NSD) = RIMAGS(1:NSD,IWe)
      ELSE
         DO IW=IWb,IWe-1
            IF (wl(IW) .LT. WVL .AND. WVL .LT. wl(IW+1)) THEN
               RREAL(1:NSD) = RREALS(1:NSD,IW)+(WVL-wl(IW))/(wl(IW+1)-wl(IW))                           &
                                              *(RREALS(1:NSD,IW+1)-RREALS(1:NSD,IW))
               RIMAG(1:NSD) = EXP(LOG(RIMAGS(1:NSD,IW))+(WVL-wl(IW))/(wl(IW+1)-wl(IW))                  &
                                                       *(LOG(RIMAGS(1:NSD,IW+1))-LOG(RIMAGS(1:NSD,IW))))
               EXIT
            END IF
         END DO
      END IF
!
      RETURN
      END SUBROUTINE intrp_REFI_wl

      PURE FUNCTION JULIAN_DAY_MY(DATE)
      INTEGER :: JULIAN_DAY_MY
      CHARACTER(*), INTENT(IN) :: DATE
!
      INTEGER,PARAMETER,DIMENSION(1:12) :: DD0=(/0,31,59,90,120,151,181,&
                                                 212,243,273,304,334/)
      INTEGER :: YYYY,MM,DD
!
      READ(DATE,'(I4,X,I2.2,X,I2.2)') YYYY,MM,DD
      JULIAN_DAY_MY=DD0(MM)+DD
      IF (MM .GT. 2) THEN
         IF (MOD(YYYY,100) .EQ. 0) THEN
            IF (MOD(YYYY,400) .EQ. 0) JULIAN_DAY_MY=JULIAN_DAY_MY+1
         ELSE
            IF (MOD(YYYY,4) .EQ. 0) JULIAN_DAY_MY=JULIAN_DAY_MY+1
         END IF
      END IF
!
      RETURN
      END FUNCTION JULIAN_DAY_MY

      SUBROUTINE DAYTIME_MY(DATE,LON,LAT,                               &
                            DURATION,MINSZA)
!
      CHARACTER(*),INTENT(IN) :: DATE
      REAL,        INTENT(IN) :: LON, LAT
      REAL,        INTENT(OUT):: DURATION, MINSZA
!
      REAL, PARAMETER :: PI=3.141592653589793
      REAL, PARAMETER :: D2R=PI/180.0
      REAL, PARAMETER :: B1=0.006918
      REAL, PARAMETER :: B2=0.399912
      REAL, PARAMETER :: B3=0.070257
      REAL, PARAMETER :: B4=0.006758
      REAL, PARAMETER :: B5=0.000907
      REAL, PARAMETER :: B6=0.002697
      REAL, PARAMETER :: B7=0.001480
      INTEGER :: NDAY
      REAL :: XLN, XLT, DELTA, ELEV, TET, TMP
!
      NDAY = JULIAN_DAY_MY(DATE)
      XLN  = LON*D2R
      XLT  = LAT*D2R
      TET  = 2.0*PI*FLOAT(NDAY)/365.0
!     solar declination in radians
      DELTA= B1-B2*COS(TET)+B3*SIN(TET)-B4*COS(2.0*TET)                 &
            +B5*SIN(2.0*TET)-B6*COS(3.0*TET)+B7*SIN(3.0*TET)
      MINSZA=ABS(XLT-DELTA)/D2R
      TMP=TAN(XLT)*TAN(DELTA)
      IF (TMP .GT. 1.0) THEN
         DURATION = 0.0
      ELSE IF (TMP .LT. -1.0) THEN
         DURATION = 24.0
      ELSE
         DURATION=2.0/15.0*ACOS(-TMP)/D2R
      END IF
!
      RETURN
      END SUBROUTINE DAYTIME_MY
!
      END MODULE mod_bbflux
