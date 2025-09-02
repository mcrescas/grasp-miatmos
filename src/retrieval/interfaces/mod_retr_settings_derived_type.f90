! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

#ifdef WARN_DRY
#warning "__RETRIEVAL_SETTINGS_DEFINITION__ binded"
#endif                  
                
module mod_retr_settings_derived_type
      use iso_c_binding
      use mod_par_inv, only   : KIDIM1,KIDIM2,KIDIM3,KKNOISE,KPARS,KW,KWM,KIP
      use mod_par_OS,  only   : KSD, NMG, NG_SPEC_RANGE_MAX,N_CHEM_MAX, N_FILTERS_MAX
      use mod_globals, only   : GBL_FILE_PATH_LEN
      use mod_par_type_aerosol
      use mod_par_type_surface

      implicit none

!	----------------------------------------------------------------------------------------
! DLS_bin code parameters: 
! keyEL         - number of scattering matrix elements  
! IWL=0, key=0  - use original Kernels 
! IWL=1, key=2  - use spectral Kernels for precalculated lognormal bins     
! keyEL=1/2 linear/spline kernel interpolation over size parameters, scatt.angles
     type, bind(C) :: iP_flags_for_DLS
        integer(kind=C_INT)             ::  IWL
        integer(kind=C_INT)             ::  key
        integer(kind=C_INT)             ::  keyEL
        integer(kind=C_INT)             ::  keyLS
        character(kind=C_CHAR)  ::  distname_O(GBL_FILE_PATH_LEN)
        character(kind=C_CHAR)  ::  distname_N(GBL_FILE_PATH_LEN)
        character(kind=C_CHAR)  ::  internal_file_path(GBL_FILE_PATH_LEN)
        character(kind=C_CHAR)  ::  external_file_path(GBL_FILE_PATH_LEN)
     end type iP_flags_for_DLS 
!	----------------------------------------------------------------------------------------
!  The the atmospheric characteristics that are retrieved for EACH PIXES    

! NDIM1 - number of different retrieved characteristics
! NDIM2 - number of atmospheric layers
! NDIM3 - number of aerosol components at each layer
! NDIM4 - number of parameters used for size distribution  (e.g. number of size bins)
! ISTARSING - definition of the retrieved parameter places in the segment of the vector of unknown AP for single pixel 
! par_type - aerosol parameter type
! par_retr - define if aerosol parameter is retrieved
     type, bind(C) :: retr_par_number_NDIM
         integer(kind=C_INT)          :: n1
         integer(kind=C_INT)          :: n2(KIDIM1) 
         integer(kind=C_INT)          :: n3(KIDIM2,KIDIM1)
         integer(kind=C_INT)          :: ISTARSING(KIDIM2,KIDIM1)
         integer(kind=C_INT)          :: par_type(KIDIM1)
         logical(kind=C_BOOL)         :: par_retr(KIDIM1)
     end type retr_par_number_NDIM      
!	----------------------------------------------------------------------------------------
! The parameters define LIGHT SCATTERING REGIMEs for OS 
! of BOTH modeling and retrieving      !!!

! IMSC = 0 - multiple scattering regime for forward calculations
!      = 1 - single scattering regime 
!      = 2 - multiple scattering regime for derivative calculations
! NG1
! NG2
      type, bind(C) :: OSH_par
         integer(kind=C_INT) :: IMSC
         integer(kind=C_INT) :: NG
         integer(kind=C_INT) :: NN
         integer(kind=C_INT) :: NF
      end type OSH_par
!	----------------------------------------------------------------------------------------
! single_pixel_constraints
!      type, bind(C) :: single_pixel_constraints
!         integer(kind=C_INT) :: IO(KIDIM2,KIDIM1)
!         real(kind=C_FLOAT)  :: GSM(KIDIM2,KIDIM1)
!      end type single_pixel_constraints
!	----------------------------------------------------------------------------------------
! single_pixel_apriori_estimates
      type, bind(C) :: single_pixel_constraints_apriori
         integer(kind=C_INT) :: IO(KIDIM3,KIDIM2,KIDIM1)
         real(kind=C_FLOAT)  :: GSM(KIDIM3,KIDIM2,KIDIM1)
      end type single_pixel_constraints_apriori
!	----------------------------------------------------------------------------------------
! single_pixel_apriori_smoothness
      type, bind(C) :: single_pixel_constraints_smoothness
         integer(kind=C_INT) :: IO(KIDIM2,KIDIM1)
         real(kind=C_FLOAT)  :: GSM(KIDIM2,KIDIM1)
      end type single_pixel_constraints_smoothness
!	----------------------------------------------------------------------------------------
! single_pixel_smoothness_estimates_and_weights
      type, bind(C) :: single_pixel_smoothness_estimates_and_weights
         integer(kind=C_INT) :: IO(KIDIM2,KIDIM1)
         real(kind=C_FLOAT)  :: EST(KIDIM3,KIDIM2,KIDIM1)
         real(kind=C_FLOAT)  :: WGT(KIDIM3,KIDIM2,KIDIM1)
      end type single_pixel_smoothness_estimates_and_weights
!	----------------------------------------------------------------------------------------
! multi_pixel_constraints
      type, bind(C) :: multi_pixel_constraints	  
         integer(kind=C_INT) :: IOT(KIDIM2,KIDIM1)
         integer(kind=C_INT) :: IOX(KIDIM2,KIDIM1)
         integer(kind=C_INT) :: IOY(KIDIM2,KIDIM1)
         real(kind=C_FLOAT)  :: GSMT(KIDIM2,KIDIM1)
         real(kind=C_FLOAT)  :: GSMX(KIDIM2,KIDIM1)
         real(kind=C_FLOAT)  :: GSMY(KIDIM2,KIDIM1)
         real(kind=C_FLOAT)  :: time_diff_threshold_sec
      end type multi_pixel_constraints
!	----------------------------------------------------------------------------------------
! The parameters define NOISE
! INOISE  - the number of different noise sources              
! SGMS    - std of noise in i -th source                      
! SGMS    - std of noise in i -th source
! BIAS    - bias in i -th noise source
! INN     - EQ.1.THEN error is absolute with
! DNN     - variation of the noise of the i-th source
! NMT     - number of meas types with particular noise
! MT      - measurement types
! NWLP    - number of wavelength in pixel for meas type
! IWLP    - index of wavelength in pixel
!	----------------------------------------------------------------------------------------
      type, bind(C) :: NOISE_par
         integer(kind=C_INT)  ::  meas_rnoise ! 0 - disable, 1 - measurement_fitting, 2 - sdata
         integer(kind=C_INT)  :: INOISE
         real(kind=C_FLOAT)   :: SGMS(KKNOISE)
         real(kind=C_FLOAT)   :: BIAS(KKNOISE)
         real(kind=C_FLOAT)   :: BIAS_EQ(KKNOISE)
         integer(kind=C_INT)  :: INN(KKNOISE)
         real(kind=C_FLOAT)   :: DNN(KKNOISE)
         integer(kind=C_INT)  :: NMT(KKNOISE)          ! number of meas types with particular noise
         integer(kind=C_INT)  :: MT(KIP,KKNOISE)       ! measurement types
         integer(kind=C_INT)  :: NWLP(KIP,KKNOISE)     ! number of wavelength in pixel for meas type  
         integer(kind=C_INT)  :: IWLP(KWM,KIP,KKNOISE) ! index of wavelength in pixel 
      end type NOISE_par
!	----------------------------------------------------------------------------------------
!  Inter-pixel  fitting
      type, bind(C) :: inter_pixel_fitting
         integer(kind=C_INT) :: INVSING
      end type inter_pixel_fitting
!	----------------------------------------------------------------------------------------
!  Edge sizes
      type, bind(C) :: edges_size
         integer(kind=C_INT) :: nx
         integer(kind=C_INT) :: ny        
         integer(kind=C_INT) :: nt         
      end type edges_size
!	----------------------------------------------------------------------------------------
!	gases
!	----------------------------------------------------------------------------------------
      type, bind(C) :: gases_opt
         logical(kind=C_BOOL)   ::  igab

         integer(kind=C_INT)    ::  nlut_name
         
         character(kind=C_CHAR) ::  lut_name(GBL_FILE_PATH_LEN, NMG) 
         integer(kind=C_INT)    ::  ngas_filters
         character(kind=C_CHAR) ::  path_to_luts(GBL_FILE_PATH_LEN)
         character(kind=C_CHAR) ::  path_to_filters(GBL_FILE_PATH_LEN)
         
         character(kind=C_CHAR) ::  kdist(GBL_FILE_PATH_LEN)
         integer(kind=C_INT)    ::  integration_method !line-by-line: 0; k-distribution: 1
         integer(kind=C_INT)    ::  filters_meas_type(N_FILTERS_MAX)
         real(kind=C_FLOAT)     ::  filters_spectral_resolution(N_FILTERS_MAX)
         integer(kind=C_INT)    ::  nfilters_index_of_wavelength_involved(KW)
         integer(kind=C_INT)    ::  filters_index_of_wavelength_involved(KW,N_FILTERS_MAX)
         character(kind=C_CHAR) ::  filters_file(GBL_FILE_PATH_LEN,KW,N_FILTERS_MAX)
        
         integer(kind=C_INT)    ::  nsubchannels(KW)
         integer(kind=C_INT)    ::  ngases_spectral_range(NMG)
         real(kind=C_FLOAT)     ::  spectral_ranges_min(NG_SPEC_RANGE_MAX, NMG)
         real(kind=C_FLOAT)     ::  spectral_ranges_max(NG_SPEC_RANGE_MAX, NMG)
      end type gases_opt
!   ----------------------------------------------------------------------------------------
!   emission
      type, bind(C) :: emission_opt
         logical(kind=C_BOOL)   ::  planck
         logical(kind=C_BOOL)   ::  solar_irradiance
         real(kind=C_FLOAT)     ::  threshold_for_starting_wavelength
         character(kind=C_CHAR) ::  folder(GBL_FILE_PATH_LEN)
      end type emission_opt
! ----------------------------------------------------------------------------------------
!   emission
      type, bind(C) :: atmospheric_vertical_profile_opt
         logical(kind=C_BOOL)   ::  istdat
         integer(kind=C_INT)    ::  stdat ! // us_standard/us: 0; mid_latitude_summer/ms: 1; mid_latitude_winter/mw: 2; surbartic_summer/ss: 3; subartic_winter/sw: 4; tropical_atmosphere:tr: 5;
         character(kind=C_CHAR) ::  vtp(GBL_FILE_PATH_LEN)
      end type atmospheric_vertical_profile_opt
! ----------------------------------------------------------------------------------------
! Functional retrieval approach for
! SPECTRAL SURF, SPECTRAL COMPLEX REFRACTIVE INDEX
      type, bind(C) :: functional_retrieval
        ! method = 1 / 2 / 3
        ! 1 - full set of retrieved parameters
        ! 2 - subset of retrieved parameters
        ! 3 - coefficients of function
        integer(kind=C_INT) :: method(KIDIM2,KIDIM1)
        ! function = 1 / 2
        ! 1 - constant
        ! 2 - linear in logarithmic scales
        integer(kind=C_INT) :: function(KIDIM2,KIDIM1)
        ! total number of nodes in output for functional approach
        integer(kind=C_INT) :: nn(KIDIM2,KIDIM1)
        ! used if subset of parameters is retrieved:
        ! indices of parameters involved in retrieval for TB, LB, MD, SHD, VP, SURF, RERI, IMRI
        integer(kind=C_INT) :: ipar(KIDIM3,KIDIM2,KIDIM1)
      end type functional_retrieval
!	---------------------------------------------------------------------------------------
! Transport model
      type, bind(C) :: transport_settings
        ! approach for phase matrix vertical profiles (1 - column average, 2 - tracer average)
        integer(kind=C_INT) :: flag_av_vprof
        ! number of tracers in transport model
        integer(kind=C_INT) :: ntrc
        ! tracer types
        character(kind=C_CHAR) :: trcs(5,KIDIM2)
        ! flag of hydrophilic tracers (0/1)
        integer(kind=C_INT) :: flag_hphi(KIDIM2)
        ! density of dry tracers
        real(kind=C_FLOAT) :: density(KIDIM2)
        ! number of levels in transport model
        integer(kind=C_INT) :: nlev
      end type transport_settings
!	----------------------------------------------------------------------------------------
! p11_integrated_cut_off
      type, bind(C) :: p11_integrated_cut_off
        integer(kind=C_INT) :: nrmax
        ! maximum radius (in um) for p11_intd_cut_off_? [1.0, 2.5, 4.0, 10.0]
        real(kind=C_FLOAT) :: rmax(4)
        integer(kind=C_INT) :: ntb(KSD,4)
        real(kind=C_FLOAT) :: rmax_tb(KSD,4)
        logical(kind=C_BOOL) :: cutoff_meas_diff
      end type p11_integrated_cut_off
!	----------------------------------------------------------------------------------------
! Chemistry
      type, bind(C) :: chemistry_opt
        character(kind=C_CHAR) :: folder(GBL_FILE_PATH_LEN)
        character(kind=C_CHAR) :: soluble(GBL_FILE_PATH_LEN)
        character(kind=C_CHAR) :: species(GBL_FILE_PATH_LEN,N_CHEM_MAX,KSD)
        integer(kind=C_INT)    :: nspecies(KSD)
      end type chemistry_opt
!    --------------------------------------------------------------------------------------

!  Output products
#ifdef WARN_DRY
#warning "__RETRIEVAL_PRODUCTS_DEFINITION__ binded"
#endif      
                 
      type, bind(C) :: output_segment_products_retrieval
         logical(kind=C_BOOL) ::  res
         logical(kind=C_BOOL) ::  par
         logical(kind=C_BOOL) ::  fit
      end type output_segment_products_retrieval

      type, bind(C) :: output_segment_products_particles
         logical(kind=C_BOOL) ::  opt    ! ext ssa Aexp
         logical(kind=C_BOOL) ::  rind
         logical(kind=C_BOOL) ::  chem
         logical(kind=C_BOOL) ::  phmx   
         logical(kind=C_BOOL) ::  lidar  ! lr ldpr
         logical(kind=C_BOOL) ::  sd2m_mph
         logical(kind=C_BOOL) ::  sd2m_ext
         logical(kind=C_BOOL) ::  PM ! particulate matter
         logical(kind=C_BOOL) ::  types ! aerosol type
      end type output_segment_products_particles

      type, bind(C) :: output_segment_products_gases
         logical(kind=C_BOOL) ::  absorption 
         logical(kind=C_BOOL) :: concentration;
      end type output_segment_products_gases

      type, bind(C) :: output_segment_products_surface
         logical(kind=C_BOOL) ::  surf   ! print main surface characteristics
         logical(kind=C_BOOL) ::  bhr_iso ! calculate and print isotropic bihemispherical reflectance
      end type output_segment_products_surface

      type, bind(C) :: output_segment_products_forcing
         logical(kind=C_BOOL) ::  bbflux
         logical(kind=C_BOOL) ::  forcing
      end type output_segment_products_forcing

      type, bind(C) :: output_segment_products_errest_particles
         logical(kind=C_BOOL) ::  mic        ! MEH: for sd when it is not part of the retireved param
         logical(kind=C_BOOL) ::  opt        ! ext ssa
         logical(kind=C_BOOL) ::  lidar      ! lr     
      end type output_segment_products_errest_particles

      type, bind(C) :: output_segment_products_errest
         logical(kind=C_BOOL)                           ::  par
         type(output_segment_products_errest_particles) ::  aerosol   
      end type output_segment_products_errest

      type, bind(C) :: output_segment_products
         type(output_segment_products_retrieval)  ::  retrieval
         type(output_segment_products_particles)  ::  aerosol
         type(output_segment_products_gases)      ::  gases
         type(output_segment_products_surface)    ::  surface
         type(output_segment_products_forcing)    ::  forcing          
         type(output_segment_products_errest)     ::  errest
      end type output_segment_products

!	----------------------------------------------------------------------------------------
!  Retrieval INput structure (RIN)
      type, bind(C) :: retr_input_settings
        integer(kind=C_INT)            ::  KNSING
        integer(kind=C_INT)            ::  KNSINGF                                             
        integer(kind=C_INT)            ::  KL                                             
        logical(kind=C_BOOL)           ::  ISTOP                                             
        logical(kind=C_BOOL)           ::  IPRI_additional_info                                                                                         
        logical(kind=C_BOOL)           ::  IPRI_verbose
!XH     LUT mode for radiative transfer: 0 - disable (default); 1 - generate; 2 - use
        integer(kind=C_INT)            ::  IMODE_LUT
        integer(kind=C_INT)            ::  NSD                                             
        integer(kind=C_INT)            ::  NLYRS(2)                                             
        integer(kind=C_INT)            ::  NLVLS_GEOM
        integer(kind=C_INT)            ::  ipplane
        integer(kind=C_INT)            ::  iPOBS
        integer(kind=C_INT)            ::  isurf_land(2)
        integer(kind=C_INT)            ::  isurf_water
        integer(kind=C_INT)            ::  Aexp_iwl(2)
        integer(kind=C_INT)            ::  ndvi_iwl(2)
        integer(kind=C_INT)            ::  naod_errest_iwl
        integer(kind=C_INT)            ::  aod_errest_iwl(KW)
        integer(kind=C_INT)            ::  nssa_errest_iwl
        integer(kind=C_INT)            ::  ssa_errest_iwl(KW)
!MEH:
        integer(kind=C_INT)            ::  naext_errest_iwl
        integer(kind=C_INT)            ::  aext_errest_iwl(KW)
        integer(kind=C_INT)            ::  nlidar_errest_iwl
        integer(kind=C_INT)            ::  lidar_errest_iwl(KW)
        real(kind=C_FLOAT)             ::  SHIFT        
! sca_ang_norm_p11 - value of scattering angle (in degrees) if provided p11
! measurements are normalized by p11(sca_ang_norm_p11)
! by delault sca_ang_norm_p11 = -180.0; valid values 0.0 - 90.0
        real(kind=C_FLOAT)            ::  sca_ang_norm_p11
        type(p11_integrated_cut_off)   ::  CUTOFF
        integer(kind=C_INT)            ::  NW
        real(kind=C_FLOAT)             ::  WAVE(KW) 
        integer(kind=C_INT)            ::  IBIN
        real(kind=C_FLOAT)             ::  tiny_wvl_models
        integer(kind=C_INT)            ::  IMQ
        integer(kind=C_INT)            ::  IPSTOP
        real(kind=C_FLOAT)             ::  LM_MIN ! minimum value of Levenberg-Marquardt correction term
        real(kind=C_FLOAT)             ::  CCOR_MIN ! minimum residual coefficient
        real(kind=C_FLOAT)             ::  CCOR_MAX ! maximum residual coefficient
        logical(kind=C_BOOL)           ::  INPUT
        logical(kind=C_BOOL)           ::  ITRONC
        logical(kind=C_BOOL)           ::  BOA_REF
        character(kind=C_CHAR)         ::  LUT_path(GBL_FILE_PATH_LEN)
        integer(kind=C_INT)            ::  MAXP
        real(kind=C_FLOAT)             ::  EPSP
        real(kind=C_FLOAT)             ::  EPSQ
        real(kind=C_FLOAT)             ::  DL
        integer(kind=C_INT)            ::  mol_prof_type ! type of the molecular vertical profile used
        integer(kind=C_INT)            ::  aer_prof_type ! type of the aerosol vertical profile used
        real(kind=C_FLOAT)             ::  PM_diam(2)    ! diameters at wich PM is calculated
        integer(kind=C_INT)            ::  nPM_diam

        type(iP_flags_for_DLS)         ::  DLSF
        type(retr_par_number_NDIM)     ::  NDIM
        type(OSH_par)                  ::  OSHF
        type(OSH_par)                  ::  OSHD
        type(single_pixel_constraints_apriori)    ::  SPCA
        type(single_pixel_constraints_smoothness) ::  SPCS
        type(single_pixel_smoothness_estimates_and_weights) :: SMS
        type(multi_pixel_constraints)  ::  MPCS
        type(NOISE_par)                ::  NOISE
        type(inter_pixel_fitting)      ::  IPFP   ! INVSING
! **
        real(kind=C_FLOAT)             ::  APSING(KPARS)
        real(kind=C_FLOAT)             ::  APSMIN(KPARS)
        real(kind=C_FLOAT)             ::  APSMAX(KPARS) 
        logical(kind=C_BOOL)           ::  APSERREST(KPARS) ! If parameter is marked to be calculated its error estimation
        
        real(kind=C_FLOAT)             ::  RMIN(KSD),RMAX(KSD)
        real(kind=C_FLOAT)             ::  RATIO1(KIDIM3,KSD)
        real(kind=C_FLOAT)             ::  RADIUS1(KIDIM3,KSD)
        integer(kind=C_INT)            ::  IWW_SINGL(KPARS)
        integer(kind=C_INT)            ::  NBIN(KSD)
        integer(kind=C_INT)            ::  KNLN(KSD)

        type(gases_opt)                     ::  gases
        type(emission_opt)                  ::  emission
        type(atmospheric_vertical_profile_opt)  ::  atmospheric_vertical_profile
        type(chemistry_opt)            ::  chemistry

        type(edges_size)               ::  edges
        type(functional_retrieval)     ::  FRETR

        logical(kind=C_BOOL)           ::  use_tmodel ! use transport model
        type(transport_settings)       ::  TMSET

        logical(kind=C_BOOL)           :: indep_par ! approach to retrieve independent parameteres
        logical(kind=C_BOOL)           :: flag_plus ! presence of characteristics with independent parameteres (indep_par=T)
        type(retr_par_number_NDIM)     :: ndim_plus ! if flag_plus=T, ndim_plus structure contains all parameters for output

        character(kind=C_CHAR) ::  plotting_output_file(GBL_FILE_PATH_LEN)
        character(kind=C_CHAR) ::  main_output_file(GBL_FILE_PATH_LEN)
        character(kind=C_CHAR) ::  sdata_sim_file(GBL_FILE_PATH_LEN)

        logical(kind=C_BOOL)           ::  debug_covariance_matrix
        logical(kind=C_BOOL)           ::  debug_errest_lm
! eps_err - absolute value of truncation threshold of Fourier and order-of-scattering
!           series expansions in radiative transfer calculations
        real(kind=C_FLOAT)             ::  eps_err   

        type(output_segment_products)  :: products                    

        !character(kind=C_CHAR,len=GBL_FILE_PATH_LEN) ::  internal_file_path
        !character(kind=C_CHAR,len=GBL_FILE_PATH_LEN) ::  external_file_path         
	  end type retr_input_settings
!	----------------------------------------------------------------------------------------
!	----------------------------------------------------------------------------------------

      contains
      
      subroutine for_avoiding_warnings_in_an_empty_module_settings_derived_type()
      end subroutine for_avoiding_warnings_in_an_empty_module_settings_derived_type                

end module mod_retr_settings_derived_type

