! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

      module mod_retr_general_output_derived_type
      
      use mod_time_utils, only : KIND_TIME 
      use mod_index_cloud
      use mod_retr_settings_derived_type
      use mod_par_inv,     only : KPARS, KIDIM3, KIDIM2, KIDIM1, KPAR, &
                                  KBF, KVERTM, KW, KKNOISE
      use mod_par_DLS,     only : KMpar, KNpar
      use mod_par_DLS_bin, only : NRR, KCpar
      use mod_par_OS,      only : KSD, KNT, NMG
      use mod_sdata_derived_type

      implicit none

#ifdef WARN_DRY
#warning "__RETRIEVAL_OUTPUT_DEFINITION__ binded"
#endif   
              
      !integer,parameter :: KF = 3 ! number of functions for retrieved parameters: 
                                   ! 1 - ext, 2 - ssa, 3 - lr
! Error estimates: indices for UW0 matrix
      integer,parameter	::	index_ext = 1
      integer,parameter	::	index_ssa = 2
      !integer,parameter ::  index_lr  = 3
!MEH:
      integer,parameter ::  index_aext  = 3
      integer,parameter	::	index_lr  = 4
      

      integer,parameter	::	bias_basic = 0
      integer,parameter	::	bias_pos   = 1
      integer,parameter	::	bias_neg   = 2
! -----------------------------------------------------------------------------------------
! -----------------------------------------------------------------------------------------
! Retrieval output
! -----------------------------------------------------------------------------------------
! Structure contains retrieval output data :
! res       - single pixel total residual (meas + smothness constrain )
! resa,resr - detailed absolut and relative measurement residuals for single pixel 
! niter       - number of iterations
! rest        - total residual for multi-pixel retrieval
! resat,resrt - detailed absolut and relative measurement residuals for segment
! par         - retrieved aerosol and surface reflectance parameters
      type, bind(C) :: output_pixel_residual
         integer(kind=C_INT) ::  niter
         real(kind=C_FLOAT)  ::  res
         real(kind=C_FLOAT)  ::  resa(KKNOISE)
         real(kind=C_FLOAT)  ::  resr(KKNOISE)
      end type output_pixel_residual
      type, bind(C) :: output_segment_residual 
         integer(kind=C_INT)      ::  niter
         real(kind=C_FLOAT)       ::  rest  
         real(kind=C_FLOAT)       ::  resat(KKNOISE) 
         real(kind=C_FLOAT)       ::  resrt(KKNOISE)
         type(output_pixel_residual)  ::  pixel(KIMAGE)  
      end type output_segment_residual

      type, bind(C) :: output_pixel_retr_par
         real(kind=C_FLOAT)  ::  par(KPARS)
         ! Retrieved SD values
         real(kind=C_FLOAT)  ::  sd(4*KIDIM3,0:KSD) ! index=0 for total SD
         ! Cv values
         real(kind=C_FLOAT)  ::  Cv(KIDIM3,KSD)
         ! ShD values
         real(kind=C_FLOAT)  ::  ShD(KIDIM3,KSD)
         ! normalized VD values
         real(kind=C_FLOAT)  ::  VD(KIDIM3,KSD)
         ! Values of surface parameters
         real(kind=C_FLOAT)  ::  BRF(KW,KBF)  ! land parameters
         real(kind=C_FLOAT)  ::  BRP(KW,KBF)  ! land parameters
         real(kind=C_FLOAT)  ::  BRM(KW,KBF)  ! water parameters
      end type output_pixel_retr_par

      type, bind(C) :: output_segment_retr_par
         type(output_pixel_retr_par)  ::  pixel(KIMAGE)
      end type output_segment_retr_par


! Real dimensions of arrays in Grneral OUTput structure
!
! NRR - ngrid in type output_segment_retr_par
! KMpar - nangle in type output_segment_ph_matrix
! KNT - nhlv in output_pixel_bbflux
! KNT - nhlv in output_pixel_forcing 
!
! KIMAGE  - npixels - number of pixels in segment
! KSD     - nsd     - number of pacticle components
! KKNOISE - nnoises - number of noises
! KCpar   - nbins   - number of precomputed lognormal bins
! KW      - nwl     - number of wavelengths for retrieved parameters
!           wl(1:nwl) - values of wavelengths for retrieved parameters
!           ndim - copy of ndim substructure from settings structure
!
      type, bind(C) :: output_info
         integer(kind=C_INT)  ::  npixels
         integer(kind=C_INT)  ::  nsd      ! KSD
         integer(kind=C_INT)  ::  nnoises  ! KKNOISE
         integer(kind=C_INT)  ::  nbins    ! KCpar
         integer(kind=C_INT)  ::  ngas     ! NMG
         integer(kind=C_INT)  ::  nwl      ! KW
         real(kind=C_FLOAT)   ::  wl(KW)
         type(retr_par_number_NDIM)  ::  ndim
         ! ngrid - number of grid radius points for retrieved SD
         ! radius - grid radius values (um) for retrieved SD
         integer(kind=C_INT)  ::  ngrid(0:KSD)
         real(kind=C_FLOAT)   ::  radius(4*KIDIM3,0:KSD) ! index=0 radii for total SD
         ! ngrid_lb - number of grid radius points for precalculated lognormal bins
         ! radius_lb(NRR) - grid radii (um)
         ! sd_lb(NRR,KCpar) - values of SD (cv=1) for each precalculated lognormal bin
         real(kind=C_FLOAT)   ::  sd_lb(NRR,KCpar)
         integer(kind=C_INT)  ::  NHLV ! number of levels for forcing and flux
         real(kind=C_FLOAT), dimension(KNT)  ::  HLV ! values of levels (meters!) for forcing and flux
         real(kind=C_FLOAT)   ::  delta_ut; ! Real time in seconds. This information is set by the framework, out of fortran part.
         real(kind=C_FLOAT)   ::  delta_ct; ! User time in seconds. This information is set by the framework, out of fortran part.
         logical(kind=C_BOOL) :: flag_plus ! presence of characteristics with n-1 independent parameteres
         type(retr_par_number_NDIM)  ::  ndim_plus ! if flag_plus=T, ndim_plus structure contains all parameters for output 
         integer(kind=C_INT)  ::  nchem(KSD) ! N_CHEM_MAX
         integer(kind=C_INT)  ::  nbrf ! KBF
         integer(kind=C_INT)  ::  nbrp ! KBF
         integer(kind=C_INT)  ::  nbrm ! KBF
      end type output_info

      type, bind(C) :: output_pixel_coordinates
        real(kind=C_FLOAT)  ::	x_lon   ! position of pixel
        real(kind=C_FLOAT)  ::	y_lat
        integer(KIND_TIME)  ::	t_masl
      end type output_pixel_coordinates

      type, bind(C) :: output_segment_coordinates
         type(output_pixel_coordinates)  ::  pixel(KIMAGE)
      end type output_segment_coordinates

! -----------------------------------------------------------------------------------------
! Fitting
! -----------------------------------------------------------------------------------------
      type, bind(C) :: output_segment_fitting
         type(segment_data) :: segment_fit
      end type output_segment_fitting
! -----------------------------------------------------------------------------------------

      type, bind(C) :: output_segment_retrieval
         type(output_segment_residual)  ::  res
         type(output_segment_retr_par)  ::  par  
         type(output_segment_fitting)   ::  fit
         type(output_info)              ::  information
      end type output_segment_retrieval
! -----------------------------------------------------------------------------------------
! -----------------------------------------------------------------------------------------
! Optical characteristics      
! -----------------------------------------------------------------------------------------
! ext,ssa,aext   - spectral ext, ssa and aext for each aerosol component
! Aexp      - Angstrom exponent (wl(4)/wl(5))
      type, bind(C) :: output_pixel_opt_wl 	
         real(kind=C_FLOAT)  ::  extt 
         real(kind=C_FLOAT)  ::  ssat 
         real(kind=C_FLOAT)  ::  aextt
         real(kind=C_FLOAT)  ::  ext(KSD)
         real(kind=C_FLOAT)  ::  ssa(KSD)
         real(kind=C_FLOAT)  ::  aext(KSD)
         real(kind=C_FLOAT)  ::  ext_cut_off(0:KSD,4)
         real(kind=C_FLOAT)  ::  ssa_cut_off(0:KSD,4)
      end type output_pixel_opt_wl
      type, bind(C) :: output_pixel_opt 	
         real(kind=C_FLOAT)         ::  Aexp
         type(output_pixel_opt_wl)  ::  wl(KW)
      end type output_pixel_opt
      type, bind(C) :: output_segment_opt 
         type(output_pixel_opt)  ::  pixel(KIMAGE)
      end type output_segment_opt
! -----------------------------------------------------------------------------------------
! Refractive index
      type, bind(C) :: output_pixel_rindex_wl 	
         real(kind=C_FLOAT)  ::  mreal(KSD)
         real(kind=C_FLOAT)  ::  mimag(KSD)
      end type output_pixel_rindex_wl
      type, bind(C) :: output_pixel_rindex 	
         type(output_pixel_rindex_wl)  ::  wl(KW)
      end type output_pixel_rindex
      type, bind(C) :: output_segment_rindex 
         type(output_pixel_rindex)  ::  pixel(KIMAGE)
      end type output_segment_rindex
! -----------------------------------------------------------------------------------------
! Phase matrix :
      type, bind(C) :: output_pixel_ph_matrix_wl      
         real(kind=C_FLOAT),dimension(KMpar,KSD) ::  ph11,  ph12,  ph22,  ph33,  ph34,  ph44
         real(kind=C_FLOAT),dimension(KMpar)     ::  pht11, pht12, pht22, pht33, pht34, pht44
         real(kind=C_FLOAT),dimension(KMpar,0:KSD,4) :: ph11_cut_off, ph12_cut_off, &
                                                      ph22_cut_off, ph33_cut_off, &
                                                      ph34_cut_off, ph44_cut_off
      end type output_pixel_ph_matrix_wl
      type, bind(C) :: output_pixel_ph_matrix      
         type(output_pixel_ph_matrix_wl)  ::  wl(KW)
      end type output_pixel_ph_matrix
      type, bind(C) :: output_segment_ph_matrix 
         integer(kind=C_INT)                 ::  nangle
         real(kind=C_FLOAT),dimension(KMpar) ::  angle
         type(output_pixel_ph_matrix)        ::  pixel(KIMAGE)
      end type output_segment_ph_matrix
! Lidar and depolarization ratios :
      type, bind(C) :: output_pixel_lidar_ratio_wl      
         real(kind=C_FLOAT),dimension(KSD)   ::  lr
         real(kind=C_FLOAT),dimension(KSD)   ::  ldpar
         real(kind=C_FLOAT),dimension(KSD)   ::  ldper
         real(kind=C_FLOAT)                 ::  lrt
         real(kind=C_FLOAT)                 ::  ldprt
      end type output_pixel_lidar_ratio_wl
      type, bind(C) :: output_pixel_lidar_ratio      
         type(output_pixel_lidar_ratio_wl)  ::  wl(KW)
      end type output_pixel_lidar_ratio
      type, bind(C) :: output_segment_lidar_ratio 
         type(output_pixel_lidar_ratio)     ::  pixel(KIMAGE)
      end type output_segment_lidar_ratio
! -----------------------------------------------------------------------------------------
! Chemistry parameters
      type, bind(C) :: output_pixel_chemistry
         real(kind=C_FLOAT)  ::  rh(KSD)
         real(kind=C_FLOAT)  ::  fwtr(KSD)
         real(kind=C_FLOAT)  ::  fslbl(KSD)
         real(kind=C_FLOAT)  ::  vfract(N_CHEM_MAX,KSD)
      end type output_pixel_chemistry
      type, bind(C) :: output_segment_chemistry 
         type(output_pixel_chemistry)  :: pixel(KIMAGE)
      end type output_segment_chemistry
! -----------------------------------------------------------------------------------------
! Two mode aerosol characteristics
! Structure contains output data (microphysical parameters):
! 0 - total, 1 - fine mode, 2 - coarse mode 
! reff        - volume median radius
! std         - standard deviation
! cv          - concentration 
! rm          - median radius 
! ext         - ext each aerosol component
      type, bind(C) :: output_pixel_sd2m_mph 	
         real(kind=C_FLOAT),dimension(0:2) :: cv,std,rm,reff
      end type output_pixel_sd2m_mph 
      type, bind(C) :: output_segment_sd2m_mph 
         type(output_pixel_sd2m_mph) :: pixel(KIMAGE)
      end type output_segment_sd2m_mph

      type, bind(C) :: output_pixel_sd2m_opt_wl
         real(kind=C_FLOAT),dimension(1:2) :: ext
      end type output_pixel_sd2m_opt_wl
      type, bind(C) :: output_pixel_sd2m_opt 	
         type(output_pixel_sd2m_opt_wl) :: wl(KW)
      end type output_pixel_sd2m_opt

      type, bind(C) :: output_segment_sd2m_opt
         type(output_pixel_sd2m_opt) :: pixel(KIMAGE)
      end type output_segment_sd2m_opt

      type, bind(C) :: output_segment_sd2m 
         type(output_segment_sd2m_mph) :: mph
         type(output_segment_sd2m_opt) :: opt
      end type output_segment_sd2m      
! -----------------------------------------------------------------------------------------
! -----------------------------------------------------------------------------------------
! Particulate Matter
      type, bind(C) :: output_pixel_PM
         real(kind=C_FLOAT) :: pm(2)
      end type output_pixel_PM
      type, bind(C) :: output_segment_PM
         type(output_pixel_PM) :: pixel(KIMAGE)
      end type output_segment_PM
! -----------------------------------------------------------------------------------------
! Typing (aerosol and presumably clouds)
! 0 – Complex mixture
! 1 – Background Aerosol
! 2 – Water/Maritime
! 3 — Urban Polluted
! 4 – Mixed aerosol
! 5 – Urban Clean
! 6 – Smoke Smoldering
! 7 – Smoke flaming
! 8 – Mineral dust
      type, bind(C) :: output_pixel_types
         integer(kind=C_INT)  ::  index  ! describes aerosol type
      end type output_pixel_types
      type, bind(C) :: output_segment_types
         type(output_pixel_types)  :: pixel(KIMAGE)
      end type output_segment_types
! -----------------------------------------------------------------------------------------
! -----------------------------------------------------------------------------------------
! Gases
      type, bind(C) :: output_pixel_gases_wl
         real(kind=C_FLOAT) :: abs(0:NMG) ! index=0 stays for total gas absorption
      end type output_pixel_gases_wl
      type, bind(C) :: output_pixel_gases
         type(output_pixel_gases_wl)  ::  wl(KW)
      end type output_pixel_gases
      type, bind(C) :: output_segment_gases
         type(output_pixel_gases) :: pixel(KIMAGE)
      end type output_segment_gases
! -----------------------------------------------------------------------------------------
! -----------------------------------------------------------------------------------------
! Surface      
! -----------------------------------------------------------------------------------------
! Surface products
      type, bind(C) :: output_pixel_surface_wl      
         real(kind=C_FLOAT)  ::  dhr      ! Directional Hemispherical Reflectance
         real(kind=C_FLOAT)  ::  bhr_iso  ! isotropic BiHemispherical Reflectance
      end type output_pixel_surface_wl
      type, bind(C) :: output_pixel_surface 
         real(kind=C_FLOAT)             ::  NDVI           
         type(output_pixel_surface_wl)  ::  wl(KW)
      end type output_pixel_surface
      type, bind(C) :: output_segment_surface 
         type(output_pixel_surface)  :: pixel(KIMAGE)
      end type output_segment_surface
! -----------------------------------------------------------------------------------------
! -----------------------------------------------------------------------------------------
! Radiative forcing
! -----------------------------------------------------------------------------------------
! Radiative broad band flux and forcing
      type, bind(C) :: output_pixel_bbflux
         integer(kind=C_INT)                 ::  NHLV
         real(kind=C_FLOAT), dimension(KNT)  ::  BBUFX0,BBDFX0,BBUFXA,BBDFXA,HLV
      end type output_pixel_bbflux
      type, bind(C) :: output_segment_bbflux
         type(output_pixel_bbflux)  :: pixel(KIMAGE)
      end type output_segment_bbflux

      type, bind(C) :: output_pixel_forcing
         integer(kind=C_INT)                 ::  NHLV
         real(kind=C_FLOAT), dimension(KNT)  ::  NETFORC,FORCEFF,HLV
      end type output_pixel_forcing
      type, bind(C) :: output_segment_forcing
         type(output_pixel_forcing)  :: pixel(KIMAGE)
      end type output_segment_forcing

      type, bind(C) :: output_segment_rad_forcing
         type(output_segment_bbflux)   :: bbflux
         type(output_segment_forcing)  :: forcing
      end type output_segment_rad_forcing
! -----------------------------------------------------------------------------------------
! -----------------------------------------------------------------------------------------
! Error estimations
! -----------------------------------------------------------------------------------------
! ERRP - Standard deviations of retrieved parameter logarithms (~relative errors)
! BIASP - Standard deviation of systematic errors of retrieved parameter logarithms
! STDP - Standard deviation sqrt(ERRP*ERRP+BIASP*BIASP)
! ERR_ - Standard deviations of retrieved optical characteristic logarithms (~relative errors)
! BIAS_ - Standard deviations of systematic errors of retrieved optical characteristic logarithms
! TSTD_ - Standard deviations sqrt(ERR_*ERR_+BIAS_*BIAS_)
! structure par      contains BIAS & ERR for all retrieved (of aerosol, clouds and surface)
! structure aerosol1 contains BIAS & ERR for ext & ssa - optical thickness and single scattering albedo of aerosol
! structure aerosol2 contains BIAS & ERR for lr        - lidar ratio of aerosol
! sd_err - error estimates for triangle bins SD model
      type, bind(C) :: output_pixel_err_estim_par
         real(kind=C_FLOAT) ::  ERRP(KPARS)
         real(kind=C_FLOAT) ::  BIASP(KPARS)
         real(kind=C_FLOAT) ::  TSTDP(KPARS)
         real(kind=C_FLOAT) ::  sd_err(4*KIDIM3,0:KSD) ! index=0 for total SD errors
                                                       ! can't be filled if number of particle components > 1
         logical(kind=C_BOOL) :: use_mask
         integer(kind=C_INT) :: num
         integer(kind=C_INT) :: idim1(1:3)
         integer(kind=C_INT) :: par_type(1:3)
         logical(kind=C_BOOL) :: KNSINGF_mask(KPARS)
      end type output_pixel_err_estim_par
      type, bind(C) :: output_segment_err_estim_par 
         type(output_pixel_err_estim_par)  ::  pixel(KIMAGE)
      end type output_segment_err_estim_par

      type, bind(C) :: output_pixel_err_estim_particles_opt_wl
         real(kind=C_FLOAT) ::  ERR_ext(KSD)
         real(kind=C_FLOAT) ::  BIAS_ext(KSD)
         real(kind=C_FLOAT) ::  TSTD_ext(KSD)
         real(kind=C_FLOAT) ::  ERR_extt
         real(kind=C_FLOAT) ::  BIAS_extt
         real(kind=C_FLOAT) ::  TSTD_extt
         real(kind=C_FLOAT) ::  ERR_ssa(KSD)
         real(kind=C_FLOAT) ::  BIAS_ssa(KSD)
         real(kind=C_FLOAT) ::  TSTD_ssa(KSD)
         real(kind=C_FLOAT) ::  ERR_ssat
         real(kind=C_FLOAT) ::  BIAS_ssat
         real(kind=C_FLOAT) ::  TSTD_ssat
!MEH:
         real(kind=C_FLOAT) ::  ERR_aext(KSD)
         real(kind=C_FLOAT) ::  BIAS_aext(KSD)
         real(kind=C_FLOAT) ::  TSTD_aext(KSD)
         real(kind=C_FLOAT) ::  ERR_aextt
         real(kind=C_FLOAT) ::  BIAS_aextt
         real(kind=C_FLOAT) ::  TSTD_aextt
      end type output_pixel_err_estim_particles_opt_wl

      type, bind(C) :: output_pixel_err_estim_particles_opt
         type (output_pixel_err_estim_particles_opt_wl) :: wl(KW)
      end type output_pixel_err_estim_particles_opt

          type, bind(C) :: output_segment_err_estim_particles_opt
         type(output_pixel_err_estim_particles_opt)  ::  pixel(KIMAGE)
      end type output_segment_err_estim_particles_opt
      
      type, bind(C) :: output_pixel_err_estim_particles_lidar_wl
         real(kind=C_FLOAT) ::  ERR_lr(KSD)
         real(kind=C_FLOAT) ::  BIAS_lr(KSD)
         real(kind=C_FLOAT) ::  TSTD_lr(KSD)
         real(kind=C_FLOAT) ::  ERR_lrt
         real(kind=C_FLOAT) ::  BIAS_lrt
         real(kind=C_FLOAT) ::  TSTD_lrt
      end type output_pixel_err_estim_particles_lidar_wl

      type, bind(C) :: output_pixel_err_estim_particles_lidar  
         type(output_pixel_err_estim_particles_lidar_wl) :: wl(KW)
      end type output_pixel_err_estim_particles_lidar

      type, bind(C) :: output_segment_err_estim_particles_lidar
         type(output_pixel_err_estim_particles_lidar)  ::  pixel(KIMAGE)
      end type output_segment_err_estim_particles_lidar

!! MEH : structure for SD error estimates when it is not part of the retrieval

        type, bind(C) :: output_pixel_err_estim_mic_rad
          real(kind=C_FLOAT) ::  ERR_sdt
          real(kind=C_FLOAT) ::  BIAS_sdt
          real(kind=C_FLOAT) ::  TSTD_sdt
        end type output_pixel_err_estim_mic_rad

        type, bind(C) :: output_pixel_err_estim_mic
        type(output_pixel_err_estim_mic_rad) :: ngrid(0)
        end type output_pixel_err_estim_mic

        type, bind(C) :: output_segment_err_estim_mic
          type(output_pixel_err_estim_mic) :: pixel(KIMAGE)
        end type output_segment_err_estim_mic

!! *********

      type, bind(C) :: output_segment_err_estim_particles 
         type(output_segment_err_estim_particles_opt)    ::  opt
         type(output_segment_err_estim_particles_lidar)  ::  lidar
         type(output_segment_err_estim_mic) :: mic
      end type output_segment_err_estim_particles

      type, bind(C) :: output_segment_err_estim 
         type(output_segment_err_estim_par)          ::  par
         type(output_segment_err_estim_particles)    ::  aerosol
         !type(output_segment_err_estim_mic_sd) :: sd
      end type output_segment_err_estim
! -----------------------------------------------------------------------------------------
! -----------------------------------------------------------------------------------------
! Aerosol
! -----------------------------------------------------------------------------------------
!      type, bind(C) :: output_segment_aerosol 
      type, bind(C) :: output_segment_particles 
         type(output_segment_opt)         ::  opt
         type(output_segment_rindex)      ::  rind
         type(output_segment_ph_matrix)   ::  phmx
         type(output_segment_lidar_ratio) ::  lidar
         type(output_segment_sd2m)        ::  sd2m
         type(output_segment_chemistry)   ::  chem
         type(output_segment_PM)          ::  pm
         type(output_segment_types)       ::  types
      end type output_segment_particles
!      end type output_segment_aerosol
! -----------------------------------------------------------------------------------------

! -----------------------------------------------------------------------------------------
! General output
! -----------------------------------------------------------------------------------------
! FRAMEWORK INTERFACE: The bound structure called output_segment_general is placed in output/grasp_output.h
      type, bind(C) :: output_segment_general
          type(output_segment_products)      ::  products
          type(output_segment_coordinates)   ::  coord
          type(output_segment_retrieval)     ::  retrieval
          type(output_segment_particles)     ::  aerosol
          type(output_segment_gases)         ::  gases
          type(output_segment_surface)       ::  surface
          type(output_segment_err_estim)     ::  errest
          type(output_segment_rad_forcing)   ::  forcing
      endtype output_segment_general
! -----------------------------------------------------------------------------------------

      contains
      
      subroutine for_avoiding_warnings_in_an_empty_module_general_output_derived()
      end subroutine for_avoiding_warnings_in_an_empty_module_general_output_derived                

end module mod_retr_general_output_derived_type

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm


