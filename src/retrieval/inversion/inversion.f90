! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

!> @file inversion.f90
! ********************************************************************************************
! **  The GRASP - Generalized Retrieval of Aerosol and Surface Properties                  ***
! **                                                                                       ***
! **  This program implements the retrieval detailed properties of aerosol and surface     ***
! **  from diverse types of remote sensing (passive and active) and  laboratory            ***
! **  (e.g. scattering phase matrix) observations.                                         ***
! **  The introductory information can be found in the following publications:             ***
! **  - Dubovik, O., T. Lapyonok, P. Litvinov, M. Herman, D. Fuertes, F. Ducos,            *** 
! **    A. Lopatin, A. Chaikovsky, B. Torres, Y. Derimian, X. Huang, M. Aspetsberger,      *** 
! **    and C. Federspiel, “GRASP: a versatile algorithm for characterizing the atmosphere”,**
! **    SPIE: Newsroom, DOI:10.1117/2.1201408.005558, Published Online: September 19, 2014. ** 
! **    http://spie.org/x109993.xml                                                           **
! **                                                                                       ***
! **  - Dubovik, O., M. Herman, A. Holdak, T. Lapyonok, D. Tanré, J. L. Deuzé, F. Ducos,   ***
! **    A. Sinyuk, and A. Lopatin, “Statistically optimized inversion algorithm for        ***
! **    enhanced retrieval of aerosol properties from spectral multi-angle polarimetric    *** 
! **    satellite observations”, Atmos. Meas. Tech., 4, 975-1018, 2011.                    ***
! **                                                                                        **
! *** NOTE: This program inherits many methodological aspects from the retrieval code      *** 
! **        previously developed for AERONET processing. Nonetheless, the program is       *** 
! **        entirely rewritten and redesigned and includes several conceptually new        *** 
! **        features and options:                                                          ***
! **        - the program is highly modular and flexible in a sense that it can be applied ***
! **          to a variety of measurements and can be set to retrieve different parameters ***
! **        - the inversion can be implemented for each "observed pixel" independently and *** 
! **          also in multi-pixel regime, i.e. when the retrieval is implemented for       *** 
! **          a segment of data (a set of pixels) simultaneously. The multi-pixel retrieval***
! **          allows for improved constraining of retrieved aerosol properties by a priori *** 
! **          limitations of space and time variability of aerosol and surface properties. ***
! **                                                                                       ***
! ********************************************************************************************
! **       This program implements general scheme of non-linear Multi-Term LSM Fitting     ***
! **                    (see Dubovik 2004, Dubovik et al. 2011, etc.)                      ***
! **                                                                                       ***
! **       The general scheme of p and q iterations is realized                            ***
! ********************************************************************************************
#include "../constants_set/mod_globals.inc"
#include "../interfaces/solver_macros.inc"
      module mod_grasp_inversion

      implicit none
          
      INTERFACE
        SUBROUTINE iteration_callback() BIND(C)
          USE, INTRINSIC :: ISO_C_BINDING
          IMPLICIT NONE
        END SUBROUTINE iteration_callback
      END INTERFACE
    
      contains

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
  subroutine inversion (  RIN,                  &
                          iu_main_output,       &
                          segment_meas,         &
                          iguess,edges,         &
                          GOUT,		              &
                          KERNELS1,KERNELS2,US  &
                       )
      use mod_globals
      use mod_par_inv
      use mod_par_OS
      use MOD_RT_SOS
      use MOD_RT_SOS_LUT,   ONLY : INIT_LUT, DLLC_LUT, INIT_LUT1, DLLC_LUT1
      use MOD_RT_SOS_SETUP, ONLY : SETUP_RT_SOS, RESET_RT_SOS, RT_SOS_SET
      use inversion_subsystem
      use mod_forward_model
      use mod_par_DLS,     only : KMpar
      use mod_index_cloud
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_molecular_scattering, only: rayleia
      use mod_edges
      use mod_covariance_matrix
      use mod_smoothness_estimates
      use mod_stop_report
      use mod_sdata_meas_type
      use mod_sdata, only : write_sdata_pixels, print_segment, &
                            get_HVP_lidar, get_MASL,           &
                            set_segment_meas_vector_fs,        &
                            assign_noise_index, add_rnoise_segment, &
                            set_index_clouds, add_bias_segment !MEH new for bias!!

      use mod_add_meas_rnoise, only : add_meas_rnoise_segment

!XH   for broad band flux
      use mod_bbflux, only : bbflux_pixel

      use mod_errest_flags, only : ERREST, initialize_ERREST

      use mod_derivative_type_transport_model, only : TM, tracer_average, column_average

      implicit none

!	--------------------------------------------------------------------------------------------                
      type(retr_input_settings),     intent(inout)  ::  RIN
      type(segment_data),            intent(inout)  ::  segment_meas
      real,dimension(KPARS,KIMAGE),   intent(in)     ::  iguess
      type(segment_edges),           intent(in)     ::  edges
      integer,                       intent(in)     ::  iu_main_output 
      type(output_segment_general),  intent(inout)  ::  GOUT
      type(kernels_triangle_bin),    intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),   intent(inout)  ::  KERNELS2
      real,dimension(KMESS,KPARS,KIMAGE),intent(inout) ::  US

!	--------------------------------------------------------------------------------------------
      type(ind_clouds)               :: index_clouds
      real,dimension(KKNOISE)        :: resa,resr,resat,resrt
      character(20)                  :: INOISEC
      character(155)                 :: CFMT
      type(nonzero)                  :: UFNZ
      type(nonzero)                  :: UFNZ_errest
      integer                        :: nnz,nnz_err
      real                           :: solver_timer
!	------------------------------------------------------------------------------------------
      integer                        :: npixels
      integer                        :: ipix,ipixstart,ipixstop,KNF,IW
      integer                        :: I,IS1,J
      integer                        :: IAC,IKQ,IIQ
      integer                        :: IKS,IKS1,NISM,IKSIM
!	--------------------------------------------------------------------------------------------
      real                           :: ccor_min, ccor_max, ccor, EPSINT
      real                           :: AQBEFORE, AQAFTER, AAQ
      real                           :: AGEN, AGENP, AGENQ
      real                           :: TCINT, EPSPP, EPSQ_test
      real                           :: AFS0, AB_edge

      integer                        :: LP
      logical                        :: apriori_estim, apriori_smooth
      integer                        :: nangles
      real, dimension(KMpar)          :: sca_angles
      integer                        :: KMIMAGE	  	  													           
      integer, dimension(KKNOISE,KIMAGE)       :: IKI ! number of meas for noise (:,:)
      logical, dimension(KKNOISE,KIMAGE)       :: IKI_shift ! presence of SHIFT
      integer, dimension(KMESS,KKNOISE,KIMAGE) :: KNOISEI
      real, dimension(KPARS)          :: APMIN,APMAX
      type(pixel_vector)             :: pixel_vec_fit_deriv(KIMAGE)
      real, dimension(KPARS,KIMAGE) :: AP, APQ, AP0
      real, dimension(KMESS,KIMAGE) :: CS
      real, dimension(KPARS,KIMAGE) :: FFMSQ, FFMS0
      real, dimension(KPARS,KIMAGE) :: FFMS, QS, AAI, FF_edge

      real, dimension(KIMAGE)       :: AMEAS, ASMO, AEST, ALS, ALSP, ALSQ

!MEH
      real, dimension(KIMAGE)       :: ALS_pos, ALS_neg
								 
      real, dimension(KPARS,KPARS)                :: SMSING, SMSING0
      type(smoothness_estimates)                 :: SMSEST
      real, dimension(KIMAGE,KPARS,KIMAGE)        :: SMMULTI

      real, dimension(KPARS,KPARS,KIMAGE) :: UFS
      real, dimension(:,:,:), ALLOCATABLE :: UFS_errest
      real, dimension(:,:), allocatable :: UF

      integer                                :: alloc_stat

      real, dimension(KPAR)                  :: FFMI0,QIM
!MEH:
      real, dimension(KPAR)                  :: QIM_pos, QIM_neg, QIM_tot

      integer, dimension(KMPSM)               :: KSM
      integer, dimension(KMPSM,KIMAGE)        :: IMSM

      logical		                             :: status_funct
      !integer, dimension(:,:,:),allocatable   :: MNOISEI
      integer, dimension(KIP,KWM,KIMAGE)      :: MNOISEI
      integer :: iu_tmp
!	------------------------------------------------------------------------------------------
      type(pixel_vector),dimension(KIMAGE) :: segment_vec_meas
      type(pixel_vector),dimension(KIMAGE) :: segment_vec_fit
!	------------------------------------------------------------------------------------------
! *** deep_random_switch -- this switch enables to set more random noise generations
      logical, parameter				  ::	deep_random_switch = .true.
!	------------------------------------------------------------------------------------------
      integer                            :: INVSING,NW
      real, dimension(KW,KIMAGE)         :: tau_mol
      real*8                             :: wl1,h1,lat1
      integer                            :: NHVP_meas ! number of heights for vertical profile
      real, dimension(KVERTM,KIMAGE)     :: HVP_meas ! heights for vertical profile
      real, dimension(KIMAGE)            :: MASL ! meters above sea level
!	------------------------------------------------------------------------------------------
      integer,save                       :: inversion_run_count = 1
      integer, dimension(KIMAGE)         :: KM1_pix
      integer                            :: KM_segm,KM1_segm,KM1_segm_temp
      integer                            :: ind_wl

      real, dimension(KPARS,KIMAGE)       :: FFS0,FFS
!      real, dimension(KMESS,KPARS,KIMAGE) :: US ! allocated before call inversion routine
      real                               :: ALM
      integer                            :: IERR
      real, dimension(KIMAGE)             :: ARES2
!	------------------------------------------------------------------------------------------
! PhF
      real*8, dimension(KPAR)     :: b	
!	------------------------------------------------------------------------------------------ 
      character(len=20)  ::  sparse_solver_name
      logical            ::  test_UFNZ
!	------------------------------------------------------------------------------------------
      logical            ::  IPRI_additional_info
      logical, parameter ::  lcmatrix = .false.!! !.true.: with covarience matrix
! lresult=.true. only single scattering properties are calculated for parameters retrieved
! at all wavelengths RIN%WAVE(1:RIN%NW)
      logical            ::  lresult, ledges
      logical, dimension(KPARS,KIMAGE) :: dermask
      logical, dimension(KPARS) :: dermask_aerpar
      logical            ::  edges_present, SvR_meas_present, SvR_meas_present_status
!	------------------------------------------------------------------------------------------
      logical            ::  molprofskipped ! AL for output when molecular profile provided but not used
!  ------------------------------------------------------------------------------------------
      integer            :: ipm
!  ------------------------------------------------------------------------------------------
!!! MEH New definitions for bias in gradient only in last new iteration for bias (before error estimates)
      REAL, DIMENSION(KMESS,KIMAGE) :: FMEAS,FFIT
      REAL,DIMENSION(KPARS,KIMAGE) :: FFSE_zer
      real, dimension(KPARS,KIMAGE) :: QSE_pos, QSE_neg, QSE_zer
      real, dimension(KPARS,KIMAGE) :: QSE_tot
      real, dimension(KPARS,KIMAGE) :: FFS0_aux,FFS_aux
      real*8, dimension(KPAR)     :: bE_zer

!MEH
      integer ::  option_bias
      logical :: use_bias_eq

! MEH: change! o hacer un loop! si "size_distribution_lognormal" y "error_size_distribution" entonces: size_dist = true, la otra opcion es hacer el condicional directamente antes de llamar MIC_ERR...()
      logical :: size_dist = .true.


! in mod_retr_general_output_derived_type:
!      integer,parameter	::	bias_basic = 0
!      integer,parameter	::	bias_pos = 1
!      integer,parameter	::	bias_neg = 2

!   2017/03/13  modified by Qiaoyun HU
!
!   Molecular depolarization ratios for (MDPR) different elastic channels need to be taken into account, because the true values 
!   physically depend on the system, i.e. the optics and the configuration of the each channel. MDPR(wl) should be instrument dependent, 
!   so that more convinent way is to set them as input in the SDATA file. For temporal test, I introduce an array (called MDPR) which 
!   contains the molecular depolarization ratio here, only valid for LILAS system, more compatible version will be available after 
!   coorperating with other colleagues.
!
!    ------------------------------------------------------------------------------------------------------
      real,dimension(KW)   ::  MDPR=0
!    ------------------------------------------------------------------------------------------------------      
      character (len=GBL_FILE_PATH_LEN)   ::  main_output_file
      character (len=GBL_FILE_PATH_LEN)   ::  plotting_output_file
      character (len=GBL_FILE_PATH_LEN)   ::  internal_file_path
      character (len=GBL_FILE_PATH_LEN)   ::  external_file_path
      character (len=GBL_FILE_PATH_LEN)   ::  sdata_sim_file, aux_path_to_luts, aux_path_to_filters
      character (len=GBL_FILE_PATH_LEN)   ::  aux_lut_name,aux_vtp,aux_STD_ATM,aux_kdist
!    ------------------------------------------------------------------------------------------------------
      real    :: tau_mol_gases
      integer :: i_filter
      real :: lm_temp(KPARS),lm_term(KPARS)
!    ------------------------------------------------------------------------------------------------------
      character(len=20)  :: str
      integer :: nel
      character (len=GBL_FILE_PATH_LEN)   ::  path_data_base
      logical :: add_rnoise = .false.
!    ------------------------------------------------------------------------------------------------------

      call cstring2fstring(RIN%main_output_file, main_output_file)
      call cstring2fstring(RIN%plotting_output_file, plotting_output_file)
      call cstring2fstring(RIN%sdata_sim_file, sdata_sim_file)
      call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)
      call cstring2fstring(RIN%DLSF%external_file_path, external_file_path)

#if defined(SUPERLU_MT)
         sparse_solver_name = 'SUPERLU_MT'
#elif defined(SUPERLU)
         sparse_solver_name = 'SUPERLU'
#elif defined(VIENNA_CL)
         sparse_solver_name = 'VIENNA_CL'
#elif defined(MUMPS)
         sparse_solver_name = 'MUMPS'
#else
#error No SPARSE_SOLVER configured!
#endif
! ------------------------------------------------------------------------------------------
!#warning "inversion_run_count - TEMP"
      !write(*,'(a,i4,a)') '***** start inversion_run_count =', inversion_run_count,' **********'

!      write(*,*) ".out_x =",edges%group_y(1)%out_x(1,1,1)
!      write(*,*) ".icloud=",edges%group_y(1)%icloud(1,1,1)

      nel = RIN%DLSF%keyEL

      lresult = .false.
      ledges = .false.
      molprofskipped = .false.


      test_UFNZ = .false. ! for comparison of UF and UFNZ
      solver_timer = 0.0

! Initialize flags for error estimates calculations
      call initialize_ERREST(ERREST)
      ERREST%LM_matrix = RIN%debug_errest_lm
      if ( ERREST%LM_matrix ) then
      ERREST%LM_vector = .true.
      else
      ERREST%LM_vector = .false.
      endif

      npixels = segment_meas%npixels
      INVSING = RIN%IPFP%INVSING
      KNF     = npixels*RIN%KNSINGF
      NW      = RIN%NW
      IPRI_additional_info = RIN%IPRI_additional_info
      SvR_meas_present_status = SvR_meas_present( segment_meas )
      if(RIN%IPRI_verbose) then
        write(iu_main_output,'(a)') 'in inversion:'
        write(iu_main_output,'(4x,a,i0)') 'npixels = ',npixels
        write(iu_main_output,'(4x,a,i0)') 'nsd = ',RIN%NSD
        if ( RIN%IPSTOP .gt. 0 ) then
        write(iu_main_output,'(4x,a,es11.4)') 'lm_min = ',RIN%LM_MIN
        endif
        write(iu_main_output,'(4x,a,es11.4)') 'ccor_min = ',RIN%CCOR_MIN
        write(iu_main_output,'(4x,a,es11.4)') 'ccor_max = ',RIN%CCOR_MAX
        if( SvR_meas_present_status ) then
          write(iu_main_output,'(4x,a,f8.5)') 'eps_err =',RIN%eps_err
          write(iu_main_output,'(4x,a,i0)') 'nlvls_geom = ',RIN%NLVLS_GEOM
          write(iu_main_output,'(4x,a,i0,2x,a)') 'aerosol profile   type = ', &
                        RIN%aer_prof_type,'(0 - exponential, 1 - gaussian, 2 – threshold)'
          write(iu_main_output,'(4x,a,i0,2x,a)') 'molecular profile type = ', &
                        RIN%mol_prof_type,'(0 - exponential, 1 - stdatm)'
        endif
        if( RIN%use_tmodel ) then
          if( RIN%TMSET%flag_av_vprof .eq. column_average ) then
          write(iu_main_output,'(4x,a)') 'Transport model is in use: Column average single scattering matrix.'
          endif
          if( RIN%TMSET%flag_av_vprof .eq. tracer_average ) then
          write(iu_main_output,'(4x,a)') 'Transport model is in use: Trace average single scattering matrix.'
          endif
        endif
      endif
      if ( IPRI_additional_info ) then
      write(iu_main_output,'(4x,2a)') 'external_file_path = ',trim(external_file_path)
      endif

!	------------------------------------------------------------------------------------------
! Print segment for debugging
      if(IPRI_additional_info) then
        call print_segment('segment_meas', segment_meas)
      endif
! Validate constants
      call validator_constants ( RIN )
      if ( error_present() ) return
! Validate Radiative Transfer (OSH) parameters
      if( SvR_meas_present_status ) then
      call validator_SOS_RT_parameters ( RIN )
      if ( error_present() ) return
      endif
! Validate Inversion parameters
      call validator_inversion_settings ( RIN, SvR_meas_present_status )
      if ( error_present() ) return
! Validate presence of surf. and vert.distr. characteristics in settings
      call validator_settings_sdata ( RIN, segment_meas, SvR_meas_present_status )
      if ( error_present() ) return
! Validate sdata (measurements)
      call validator_sdata ( RIN, segment_meas, SvR_meas_present_status )
      if ( error_present() ) return
!	------------------------------------------------------------------------------------------
! Assign General output product flags
      call set_GOUT_product_flags (RIN,GOUT%products)
!	------------------------------------------------------------------------------------------
#if defined(OSH)
! Setup structure for SOS RT routine
      call SETUP_RT_SOS(RIN)

! Set number of particle component for RT when Transport model (tracer average) is in use.
      if ( RIN%use_tmodel ) then
      if ( RIN%TMSET%flag_av_vprof .eq. tracer_average) then
      RT_SOS_SET%NA = RIN%TMSET%nlev
      endif
      endif

! load look up table when needed
      if (RT_SOS_SET%ILUT) call INIT_LUT(RIN)
      if ( error_present() ) return
      if (RT_SOS_SET%ILUT .or. RT_SOS_SET%IWUT) call INIT_LUT1(RIN)
! Setup flag for SOS RT routine to control if downward and upward measurement
! pixels are present in segment
      if(SvR_meas_present_status) then
      call SETUP_RT_SOS_CNTRL_ICMB(iu_main_output,RIN,segment_meas)
      endif
! Reset flags for SOS RT routine
      call RESET_RT_SOS()
#endif
! Set mask for retrieved surface parameters (land or water only) in order
! to avoid calling forward model for surface derivatives equal to 0.
      call set_surf_derivative_mask ( RIN, segment_meas, dermask )
! Set mask for retrieved aerosol parameters in order
! to avoid calling forward model for surface derivatives equal to 0.
      call set_aerosol_only_derivative_mask ( RIN, dermask_aerpar )
!	------------------------------------------------------------------------------------------
      !write(str,*) RIN%NW
      !CFMT = '(4x,a,'//trim(adjustl(str))//'(f10.5))'
      if(RIN%IPRI_verbose) then
        write(iu_main_output,'(a)') 'wavelengths(um):'
        write(iu_main_output,'(10f10.5)') RIN%WAVE(1:RIN%NW)
      endif
!	------------------------------------------------------------------------------------------
! Molecular scattering
        do ipix=1,npixels
          h1   = segment_meas%pixels(ipix)%MASL
          lat1 = segment_meas%pixels(ipix)%y
          do IW=1,NW
          wl1  = RIN%WAVE(IW)
          call rayleia(wl1,h1,lat1,tau_mol(IW,ipix))
          enddo ! IW
        enddo ! ipix

!AL Checking if any depolarization lidars are present and assignin proper molecular depolarization
!AL A bypass hack, parameter should be taken from external file, values are for lilas and MPL (0.532 0.4%)

      do ipix=1,npixels
         do IW=1,segment_meas%pixels(ipix)%nwl ! TO DO CHANGE to nw depending on pixel or use ind_wl!!!!
!            ind_wl=segment_meas%pixels(ipix)%meas(IW)%ind_wl
!WRITE(*,*) 'L500, inversion.f90: gaspar = ',segment_meas%pixels(ipix)%meas(IW)%gaspar
            if(any(                                                                       &
               (segment_meas%pixels(ipix)%meas(iw)%meas_type(:) .EQ. meas_type_DPAR) .OR. &
               (segment_meas%pixels(ipix)%meas(iw)%meas_type(:) .EQ. meas_type_DPER) .OR. &
               (segment_meas%pixels(ipix)%meas(iw)%meas_type(:) .EQ. meas_type_DP)        &
              )) then

                  MDPR(IW)=segment_meas%pixels(ipix)%meas(IW)%gaspar
!AL                 if (ABS(RIN%WAVE(IW) - 0.355) .LE. 10e-3) then
!                     MDPR(IW)=0.006
!                 elseif (ABS(RIN%WAVE(IW) - 0.532) .LE. 10e-3) then
!                     MDPR(IW)=0.004
!                 elseif (ABS(RIN%WAVE(IW) - 1.064) .LE. 10e-3) then
!                     MDPR(IW)=0.004
!                 else
!                     write(iu_main_output,'(A,F5.3,A)') 'WARNING: Unsupported wavelenght of', RIN%WAVE(IW), 'um for lidar depolarization!'
!                     write(iu_main_output,*) 'MDPR was set to 0.'
!AL                 endif
            endif
!            write (*,*) 'ipix, iw, ind_wl, wave(ind_wl), meas_type, mdpr(ind_wl)', ipix, iw, ind_wl, RIN%WAVE(ind_wl), segment_meas%pixels(ipix)%meas(iw)%meas_type(1:2), MDPR(ind_wl)
         enddo ! IW
      enddo ! ipix
!	------------------------------------------------------------------------------------------
!AL Checking if any molecular profiles given and putting a warning
      do ipix=1,npixels
         do IW=1,NW
            if (any(segment_meas%pixels(ipix)%meas(IW)%IFMP(:) .eq. 1)) then
               wl1=segment_meas%pixels(ipix)%meas(IW)%wl
               molprofskipped = .true.
               write(iu_main_output,'(A,F5.3,A)') 'WARNING: Molecular profile at ', wl1, 'um ignored!'
            endif 
         enddo !IW
         if (molprofskipped) then
            write(iu_main_output,*) 'Unsupported feature, standard atmosphere will be used instead.'
         endif
      enddo !ipix
!  ------------------------------------------------------------------------------------------
! Set Retrieval OUTput structure with general for segment variables 
      call set_segm_retr_output ( RIN,                & ! IN
                                  segment_meas,       &
                                  GOUT                & ! OUT
                                )
!	------------------------------------------------------------------------------------------
! Check if aerosol profile is a part of retrieved parameters (presence of lidar measurements)
      call get_HVP_lidar ( segment_meas,       & ! IN
                           NHVP_meas,HVP_meas  & ! INOUT
                         )
      if ( error_present() ) return
!      write(*,*) 'AFTER get_HVP_lidar: NHVP_meas=',NHVP_meas

      if ( NHVP_meas .gt.1 ) then
! Validate descending order of altitudes and lidar signal normalization
      call validator_lidar ( segment_meas )
      if ( error_present() ) return
      endif
!	------------------------------------------------------------------------------------------
! Validate if standard_deviation_synthetic non zero values are provided to be added to measurements
      if ( RIN%NOISE%meas_rnoise .gt. 0 ) then ! measurement_fitting / sdata
      if(any(RIN%NOISE%SGMS(1:RIN%NOISE%INOISE) .gt. 0.0)) then
        add_rnoise = .true.
      else
        write(iu_main_output,'(a)') 'WARNING: standard_deviation_synthetic = 0. No random noise is added.'
      endif
      endif
! Validate if it is possible to add random noise to measurements of sdata structure
      if ( add_rnoise ) then
      if ( RIN%NOISE%meas_rnoise .eq. 2 ) then ! sdata
        call validator_polarization_meas_for_random_noise ( RIN, segment_meas )
        if ( error_present() ) return
      endif
      endif

! Assign noise indices to measurement types
      call assign_noise_index(RIN,segment_meas,MNOISEI)

! Add random noise to measurements of input sdata structure
      if ( add_rnoise ) then
      if ( RIN%NOISE%meas_rnoise .eq. 2 ) then ! sdata
      if ( .not. RIN%ISTOP ) then
        call add_meas_rnoise_segment ( RIN, deep_random_switch, &
                                      MNOISEI, segment_meas &
                                     )
      endif
      endif
      endif

! Initialize and fill in segment measurement vector FS
      call set_segment_meas_vector_FS ( RIN,INVSING,       & ! IN
                                        segment_meas,      & 
                                        segment_vec_meas   & ! OUT
                                      )
      if ( error_present() ) return


! If aplicable, add random noise to segment measurement vector
      if ( add_rnoise ) then
      if ( RIN%NOISE%meas_rnoise .eq. 1 ) then ! measurement_fitting
        call add_rnoise_segment ( RIN,deep_random_switch,    & ! IN
                                 segment_meas,              &
                                 segment_vec_meas,          & ! INOUT
                                 MNOISEI                    & ! IN
                               )
        if ( error_present() ) return
      endif
      endif
    
!MEH
    use_bias_eq = .false.
    !if(any(RIN%NOISE%BIAS_EQ(1:RIN%NOISE%INOISE) .ne. 0.0) .and. INVSING .EQ. 0) then
    if(any(RIN%NOISE%BIAS_EQ(1:RIN%NOISE%INOISE) .ne. 0.0)) then
        use_bias_eq = .true.
    endif

! If aplicable, add bias to segment measurement vector
    if(any(RIN%NOISE%BIAS(1:RIN%NOISE%INOISE) .ne. 0.0)) then
      call add_bias_segment ( RIN,    & ! IN
                                segment_meas,              &
                                segment_vec_meas,          & ! INOUT
                                MNOISEI                    & ! IN
                              )
      if ( error_present() ) return
    endif

! CS - calculate covariance matrix
! IKI(I,:)       - total number of measurements of i-th source
! IKI_shift(I,:) - presence of shift for measurements of i-th source
! KNOISE(1,K)  - specific numbers affected by i-th source        
! ARES - parameters adjusting the weight of a priori constraints during iterations
! if RESIDIAL > ARES, the weight (ccor) of a priori constraints is increased, once
! RESIDIAL < ARES the weight of a priori constraints is fixed
      call covariance_matrix_segment (RIN,INVSING,           & ! IN
                                      segment_meas,          & 
                                      segment_vec_meas,      &
                                      MNOISEI(:,:,:),        & 
                                      IKI(:,:),              &
                                      IKI_shift(:,:),        &
                                      KNOISEI(:,:,:),        & ! OUT
                                      CS(:,:),ARES2(:)       &
                                     )
! MH
!       write(*,*) '############# C_f  ###############'
!       write(*,*) CS(:,:)

!      write(*,'(a)') 'ARES2 :'
!      write(*,'(10e12.4)') ARES2(1:npixels)
!	------------------------------------------------------------------------------------------

      segment_vec_fit = segment_vec_meas

!	------------------------------------------------------------------------------------------
! Definition of a priori and smoothness matrices 

! Initial guess and
! Definition of Vector of a priori astimates 

!      if(inversion_run_count .eq. 2) then
! delete or comment after testing edges
!        RIN%INPUT = .true.
!      endif

      call iguess_apriori (inversion_run_count,  & ! delete
                           iu_main_output,       & ! IN
                           RIN,                  &
                           npixels,iguess,       &
                           AP,AP0,APMIN,APMAX    &	! OUT
	                       )
      if ( error_present() ) return
      !if(inversion_run_count .eq. 2) then
        !write(iu_main_output,*) 'New test'
        !AP(21,1) = log(0.16819E+00)
        !AP(22,1) = log(0.17209E+00)
        !AP(23,1) = log(0.18540E+00)
        !AP(24,1) = log(0.22048E+00)
        !AP(25,1) = log(0.37086E+00)
        !AP(26,1) = log(0.45338E+00)
      !endif

         !if(RIN%IPRI_verbose)  write(iu_main_output,*) 'after iguess_apriori, AP,AP0:'
         !DO I=1,RIN%KNSING
         !if(RIN%IPRI_verbose)  write(iu_main_output,'(i5,1000e14.5)')     &
             !I,(exp(AP(I,ipix)),exp(AP0(I,ipix)),RIN%APSING(I),ipix=1,1)!npixels)
         !ENDDO

      if ( RIN%use_tmodel ) then
        call set_transport_model_segment_iguess ( RIN, AP, npixels, TM )
      endif

      if(IPRI_additional_info) write(iu_main_output,*) 'Before smoothterm_single_pixel'

! Single pixel constrains: a priori smoothness constraints for single-pixel
      call smoothterm_single_pixel_smoothness ( RIN, & ! IN
                                                IKS, SMSING, SMSEST, & ! OUT
                                                apriori_smooth  &
                                              )
      if ( error_present() ) return

! Single pixel constrains: a priori estimates
      call smoothterm_single_pixel_apriori ( RIN,  & ! IN
                                             IKS1,SMSING0,apriori_estim  & ! OUT
                                           )

      if(INVSING .gt. 0) then

! Multi pixel constrains: a priori smoothness multi-pixel constraints
         if(IPRI_additional_info)  write(iu_main_output,*) 'Before smoothterm_multi_pixel'
         call set_index_clouds ( RIN, segment_meas, index_clouds )
         call smoothterm_multi_pixel ( iu_main_output,RIN, & ! IN
                                       index_clouds,      &
                                       NISM,KSM,IMSM,     & ! OUT
                                       IKSIM,SMMULTI      &
                                     )

!write(*,*) 'after smoothterm_multi_pixel: NISM, IKSIM - ',NISM,IKSIM
        if(IPRI_additional_info)  write(iu_main_output,*) 'After smoothterm_multi_pixel'
!*** Multi pixel constrains: a priori estimates of retrieved parameters near
!***                         the edges of the inverteded segment
        FF_edge  (:,:) = 0.0
        AB_edge        = 0.0

        ledges =  &
        edges_present(iu_main_output,segment_meas%NX,segment_meas%NY,segment_meas%NT,RIN,edges)
        if(IPRI_additional_info)  write(iu_main_output,*) 'Before smoothterm_mutli_pixel_edges, ledges=',ledges
        if(ledges)  &
        call smoothterm_mutli_pixel_edges(  iu_main_output,RIN,index_clouds,edges,  &   ! IN
                                            NISM,KSM,IKSIM,IMSM,SMMULTI,            &   ! INOUT
                                            FF_edge,AB_edge ) ! OUT
        if(IPRI_additional_info)  write(iu_main_output,*) 'After smoothterm_mutli_pixel_edges'
!write(*,*) 'after smoothterm_mutli_pixel_edges NISM,IKSIM: ',NISM,IKSIM
!write(*,*) 'in inversion:  FF_edge(:,1): '
!write(*,'(10e14.4)')  FF_edge(:,1)

!write(*,*) 'AB_edge =',AB_edge,'  ledges =',ledges ! delete or comment after edges testing
	
      endif ! INVSING .GT. 0
	  
      if(RIN%IMQ .eq. 2 .or. test_UFNZ) then
!     Allocate arrays for UF matrix test
         allocate(UF(KPAR,KPAR),stat=alloc_stat)
            if (alloc_stat /= 0) stop 'error while trying to allocate UF'
      endif ! RIN%IMQ .le. 2 .or.
						  
!     Allocate Compressed Column Storage (sparse matrix)
      call allocate_sparse_matrix_storage ( UFNZ, KNF*(RIN%KNSINGF+npixels-1), KNF  )

!     Allocate UFS_errest matrix and Compressed Column Storage (sparse matrix) for error estimates
      if( .not. RIN%ISTOP ) then
        if( RIN%products%errest%par .or. &
          RIN%products%errest%aerosol%opt .or. &
          RIN%products%errest%aerosol%lidar ) then
          ERREST%product = .true.
        endif
        if ( ERREST%product ) then
          if ( .not. ERREST%LM_matrix ) then
            call allocate_sparse_matrix_storage ( UFNZ_errest, KNF*(RIN%KNSINGF+npixels-1), KNF )
            allocate(UFS_errest(KPARS,KPARS,KIMAGE),stat=alloc_stat)
            if (alloc_stat /= 0) stop 'error while trying to allocate UFS_errest'
            UFS_errest(:,:,:) = 0.0
          endif
          if ( ERREST%LM_vector ) then
            allocate(ERREST%LM_term(KPARS,KIMAGE),stat=alloc_stat)
            if (alloc_stat /= 0) stop 'error while trying to allocate LM_term'
            ERREST%LM_term(:,:) = 0.0
            allocate(ERREST%AP_iguess(KPARS,KIMAGE),stat=alloc_stat)
            if (alloc_stat /= 0) stop 'error while trying to allocate AP_iguess'
            ERREST%AP_iguess(:,:) = AP(:,:)
          endif
        endif
      endif
!	------------------------------------------------------------------------------------------
! Copy segment_meas structure into segment_fit structure

      GOUT%retrieval%fit%segment_fit = segment_meas ! place in the code is important
!	------------------------------------------------------------------------------------------
! Numbers of virtual degree of freedom for single pixel and multipixel to be used 
! for residual calculation and 
! for scalling of a priori constrains by measurement residual  
      KM_segm = SUM(segment_vec_meas(1:npixels)%KMIMAGE)
      KM1_segm_temp = 0
      do ipix=1,npixels
         KMIMAGE = segment_vec_meas(ipix)%KMIMAGE
         KM1_pix(ipix) = KMIMAGE - RIN%KNSINGF
         if(apriori_smooth) KM1_pix(ipix) = KM1_pix(ipix) + IKS
         if(apriori_estim)  KM1_pix(ipix) = KM1_pix(ipix) + IKS1
         KM1_segm_temp = KM1_segm_temp + KM1_pix(ipix)
         if(KM1_pix(ipix) .ge. 0) then
            ERREST%pix_mask(ipix) = .true.
            ERREST%pix = .true.
         endif
         if(KM1_pix(ipix) .le. 0) then
            KM1_pix(ipix)  = KMIMAGE
         endif
      enddo ! ipix 
      IERR = SUM(KM1_pix(1:npixels))

      if(RIN%IPRI_verbose .and. KM1_segm_temp .ne. IERR) &
      write(*,'(2(a,i8))') &
      '!!! WARNING: number of measurements for residual calculation KM1 =', &
      KM1_segm_temp,'  KM =',IERR
      if(INVSING .gt. 0)  then
        KM1_segm = KM1_segm_temp + IKSIM
        if(KM1_segm .ge. 0) then
          ERREST%segm = .true.
        endif
        if(KM1_segm .le. 0) then
          KM1_segm    = KM_segm
        endif
      endif
!	------------------------------------------------------------------------------------------

      ipixstart = 1
      select case(INVSING)
      case(0)   ! single pixel
         ipixstop = 1
      case(1,2) ! multi pixel
         ipixstop = npixels  
      case default  
        write(tmp_message,'(a,i0,a)') 'INVSING = ',INVSING,' value is not valid'
        G_ERROR(trim(tmp_message))
      end select

!C******************************************************
!C***         p - iterations
!C******************************************************
!C*** LP -number of iterations
!C*** LTP-number of iterations for choosing TP
!C******************************************************
!C*** DETERMINING of delta AP()     ***
!C******************************************************
!C**  Calculating "measurements" model from AP() vector:
!C******************************************************
  
      ccor_min = RIN%ccor_min
      ccor_max = RIN%ccor_max
      ccor     = ccor_min

      !ALSP(:)   = 0.0
      !FFMS(:,:) = 0.0
      !US(:,:,:) = 0.0
      QS(:,:)   = 0.0 ! do not remove
      
      IF(RIN%KNSING .GT. RIN%KNSINGF) THEN 
        DO ipix=1,npixels
        APQ(RIN%KNSINGF+1:RIN%KNSING,ipix) = AP(RIN%KNSINGF+1:RIN%KNSING,ipix)
        ENDDO
      ENDIF

177   CONTINUE

!do while(ipixstop .LT. npixels)

      LP = 0

! *********************************************************************************************************************
! *********************************************************************************************************************
! Forward model and Residual for INITIAL GUESS

      ALS(ipixstart:ipixstop) = 0.0
      ASMO(:) = 0.0
      AEST(:) = 0.0 

      do ipix=ipixstart,ipixstop
         segment_vec_fit(ipix)%FS(:) = 0.0
      enddo
      call inversion_forward_model(                              &
                  iu_main_output,                                &
                  ipixstart,                                     &
                  ipixstop,                                      &
                  RIN,                                           &
                  RIN%OSHF,                                      &
                  0,                                             &
                  lresult,                                       &
                  tau_mol,                                       &
                  NHVP_meas,                                     &
                  HVP_meas,                                      &
                  AP,                                            &
                  MDPR,                                          & ! molecular depolarization, added by Qiaoyun HU
                  GOUT%retrieval%fit%segment_fit%pixels,         &
                  segment_vec_fit,                               &
                  GOUT%aerosol,                                  &
                  GOUT%gases,                                    &
                  GOUT%surface,                                  &
                  GOUT%retrieval,                                &
                  nangles,                                       &
                  sca_angles,                                    &
                  KERNELS1,                                      &
                  KERNELS2                                       &
            )

      if ( error_present() ) return
      do ipix=ipixstart,ipixstop
         KMIMAGE=segment_vec_meas(ipix)%KMIMAGE

!write(*,*) 'LP=0 AP:'
!write(*,'(10e14.4)') AP(1:RIN%KNSING,ipix)

         !DO I=1,KMIMAGE
         !!WRITE(iu_main_output,*)   &
         !WRITE(0,*)   &
         !ipix,I,segment_vec_fit(ipix)%FS(I),'   - ipix,I,FPS(I,ipix)) AFTER forward_model_pixel in inversion'
         !ENDDO

         if(IPRI_additional_info .and. INVSING .eq. 0) then
         do I=1,RIN%KNSING
            write(iu_main_output,'(2i5,e13.5,a)')   &
            ipix,I,EXP(AP(I,ipix)),' ipix,I,EXP(AP(I)) AFTER forward_model_pixel in inversion'
         enddo
         endif ! IPRI_additional_info .AND.

! residual: measurment term (F-FP)^T (C)^(-1)(F-FP)

         call residual_meas_term (  IPRI_additional_info,iu_main_output,  & ! IN
                                    KMIMAGE,                              &
                                    segment_vec_meas(ipix)%FS(1:KMIMAGE), &
                                    segment_vec_fit (ipix)%FS(1:KMIMAGE), &
                                    CS(1:KMIMAGE,ipix),                   &
                                    AMEAS(ipix)                           & ! OUT
                                 )
!if (ipix .eq. 3) then
!call print_segment('segment_meas', segment_meas) ! print for debugging
!write(*,*) 'FS_meas:'
!write(*,'(10e14.6)') segment_vec_meas(ipix)%FS(1:KMIMAGE)
!write(*,*) 'FS_fit:'
!write(*,'(10e14.6)') segment_vec_fit (ipix)%FS(1:KMIMAGE)
!write(*,*) 'CS:'
!write(*,'(10e14.6)') CS(1:KMIMAGE,ipix)
!write(*,*) 'ipix',ipix,'  AMEAS(ipix)=',AMEAS(ipix)
!stop 'test: after residual_meas_term'
!endif

! residual: a priori smoothness term (single-pixel): (AP)^T(D_single)^T(D_single) AP
         if (apriori_smooth) &
         call residual_apriori_smooth_term ( IPRI_additional_info,iu_main_output, & ! IN
                                             RIN%KNSINGF,                         &
                                             AP(1:RIN%KNSINGF,ipix),              &
                                             SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                             SMSEST,                             &
                                             ASMO(ipix)                           & ! OUT
                                           ) 

! residual: (AP-AP0)^T C^(-1) (AP-AP0); A PRIORI ESTIMATE TERM of the residual
         if (apriori_estim) &
         call residual_apriori_estim_term ( IPRI_additional_info,iu_main_output,  & ! IN
                                            RIN%KNSINGF,                          &
                                            AP(1:RIN%KNSINGF,ipix),               &
                                            AP0(1:RIN%KNSINGF,ipix),              &
                                            SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                            AEST(ipix)                            & ! OUT
                                         ) 
! residual: provides simple meas residuals (F-FP)^T (F-FP) for each noise,
!           the residual inlcludes both redidual for absolute and relative vlaues
         call residual_details ( RIN,                      & ! IN
                                 npixels,ipix,             & 
                                 IKI,IKI_shift,KNOISEI,    & 						 
                                 segment_vec_meas,         &
                                 segment_vec_fit,          &
                                 resa,resr,resat,resrt     & ! OUT 		
                               )
!C***      Prepare residual values required for the output
         call set_pixel_retr_output_residual (  INVSING,LP,ipix,0.0,         & ! IN
                                                RIN%NOISE%INOISE,resa,resr,  &
                                                GOUT%retrieval%res           & ! INOUT
                                             )
     
         if(RIN%IPRI_verbose .and. INVSING .ge. 1)  then
            write(INOISEC,*) RIN%NOISE%INOISE
            CFMT = '(10x,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),4x,a,i0,6x,a)'
            write(iu_main_output,TRIM(CFMT)) &
            (I,':',resa(I),resr(I)*100.0,' %',I=1,RIN%NOISE%INOISE),'  pixel # ', ipix, &
            'Residual using INITIAL GUESS'
         endif ! RIN%IPRI_verbose

        ccor = AMEAS(ipix)/(ARES2(ipix)*KMIMAGE)
        if(ccor .lt. ccor_min) ccor = ccor_min
        if(ccor .gt. ccor_max) ccor = ccor_max
        ALS(ipix) = AMEAS(ipix)
        if(apriori_smooth)  ALS(ipix) = ALS(ipix) + ccor*ASMO(ipix)
        if(apriori_estim)   ALS(ipix) = ALS(ipix) + ccor*AEST(ipix) 
        ALS (ipix) = ALS (ipix)/KM1_pix(ipix)            
        ALSP(ipix) = ALS(ipix)
      enddo ! ipix=ipixstart,ipixstop
      !write(*,'(a,10e14.4)') '1: ALS:     ',ALS(1:npixels)
      !write(*,'(a,10i14)') '1: KM1_pix: ',KM1_pix(1:npixels) 

      select case(INVSING)
      case(0)   ! pixel by pixel scenario
         AGEN = ALS(ipixstop) !/KM1_pix(ipixstop)      
         ! Prepare residual values required for the output
         call set_pixel_retr_output_residual (  INVSING,LP,ipixstop,AGEN,    & ! IN
                                                RIN%NOISE%INOISE,resa,resr,  &
                                                GOUT%retrieval%res           & ! INOUT
                                             )

      case(1,2) ! multi pixel scenario
         ccor = SUM(AMEAS(1:npixels))/  &
                SUM(ARES2(1:npixels)*segment_vec_meas(1:npixels)%KMIMAGE)  
         if(ccor .lt. ccor_min) ccor = ccor_min
         if(ccor .gt. ccor_max) ccor = ccor_max
         AGEN = SUM(AMEAS(1:npixels))
         if(apriori_smooth)  AGEN = AGEN+ccor*SUM(ASMO(1:npixels))
         if(apriori_estim)   AGEN = AGEN+ccor*SUM(AEST(1:npixels))
!C*****************************************************************************************
!C***   inter-pixel smoothness term: (AP)^T (D_inter)^T(D_inter) AP) + edge terms (see subroutine)
!C***                                 See  Dubovik et al. [2011]
!C***      call residual_inter_pixel_term(IPRI_additional_info,iu_iP_main_output,RIN%KNSINGF,npixels,AP,SMMULTI,AFS0)
!C*****************************************************************************************

         call residual_inter_pixel_term(IPRI_additional_info,iu_main_output,RIN%KNSINGF,npixels,AP,SMMULTI, &
                                        ledges,FF_edge,AB_edge,AFS0)
         AGEN = (AGEN+ccor*AFS0)/KM1_segm

         if(IPRI_additional_info) then
            write(iu_main_output,*) SQRT(AGEN)*100.0,'  AGEN ERR %'
            write(iu_main_output,'(2e16.7,i12,a)') AFS0,ccor,KM1_segm,'  AFTER0=AFS0,ccor,KM1_segm'
         endif ! IPRI_additional_info
      end select ! INVSING 
!C*****************************************************************************************
!C***      Prepare residual values required for the output
!C*****************************************************************************************
      call set_total_retr_output_residual (  LP,AGEN,                      & ! IN
                                             RIN%NOISE%INOISE,resat,resrt, &
                                             GOUT%retrieval%res            & ! INOUT
                                          )
      if(RIN%IPRI_verbose) then
         write(INOISEC,*) RIN%NOISE%INOISE
         if(INVSING .eq. 0) then
           CFMT = '(/,f10.5,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),4x,a,i0,6x,a)'
           write(iu_main_output,TRIM(CFMT)) SQRT(AGEN)*100.0,      & 
           (I,':',resa(I),resr(I)*100.0,' %',I=1,RIN%NOISE%INOISE),'  pixel # ',ipixstop,  &
           'Residual using INITIAL GUESS'
         else
           CFMT = '(/,f10.5,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),6x,a,/)'
           write(iu_main_output,TRIM(CFMT)) SQRT(AGEN)*100.0,      &
           (I,':',resat(I),resrt(I)*100.0,' %',I=1,RIN%NOISE%INOISE), &
           'Residual using INITIAL GUESS for TOTAL SEGMENT'
         endif ! INVSING .eq. 0
      endif ! RIN%IPRI_verbose
!      write(*,'(a,10e14.4)') '0: ALS:     ',ALS(1:npixels)
!      write(*,'(a,10i14)')   '0: KM1_pix: ',KM1_pix(1:npixels)
!      write(*,'(a,10e14.4)') '0: AMEAS:   ',AMEAS(1:npixels)                                         
!      write(*,'(a,10e14.4)') '0: ASMO:    ',ASMO(1:npixels)                                         
!      write(*,'(a,10e14.4)') '0: AEST:    ',AEST(1:npixels)                                         
!      write(*,'(a,10e14.4)') '0: ARES2:   ',ARES2(1:npixels)                                         
!      write(*,'(a,10i14)')   '0: KMIMAGE: ',segment_vec_meas(1:npixels)%KMIMAGE                                         
!      write(*,'(2(a,f14.5))')'0: AGEN=',SQRT(AGEN)*100.,'  AFS0=',AFS0                                         



      !if ( lresult ) then
 !       WRITE(iu_main_output,*)
 !       DO I=1,RIN%KNSING
 !       WRITE(iu_main_output,'(i5,1000e14.5)')  &
 !        I,EXP(AP(I,ipixstart:ipixstop))
 !       ENDDO ! I

! **  Calculate optical characteristics (ext,ssa,lr) at RIN%WAVE wavelengths for initial guess
         lresult = .true.
! inversion_forward_model output:
! - cross sections ext sca abs (1/um)
! - scattering matrix elements ph11, p12, ..., p44 (unitless)
         call inversion_forward_model(                      &
                     iu_main_output,                        &
                     ipixstart,                             &
                     ipixstop,                              &
                     RIN,                                   &
                     RIN%OSHF,                              &
                     0,                                     &
                     lresult,                               &
                     tau_mol,                               &
                     NHVP_meas,                             &
                     HVP_meas,                              &
                     AP,                                    &
                     MDPR,                                  & ! molecular depolarization, added by Qiaoyun HU
                     GOUT%retrieval%fit%segment_fit%pixels, &
                     segment_vec_fit,                       &
                     GOUT%aerosol,                          &
                     GOUT%gases,                            &
                     GOUT%surface,                          &
                     GOUT%retrieval,                        &
                     nangles,                               &
                     sca_angles,                            &
                     KERNELS1,                              &
                     KERNELS2                               &
                  )
         if ( error_present() ) return
         lresult = .false.
         do ipix=ipixstart,ipixstop
            call set_gout_pixel(  iu_main_output, RIN, segment_meas, & ! IN
                                  ipix, nangles, sca_angles, AP,     &
                                  GOUT,                              & ! INOUT
                                  KERNELS1,KERNELS2                  &
                               )
            if ( error_present() ) return
         enddo ! ipix
        ! **  Write retrieval results after each iteration
        !call print_main_output ( iu_main_output, RIN, segment_meas, GOUT )
        call iteration_callback()
      !endif ! lresult

      if(RIN%ISTOP .or. AGEN .lt. 1e-8) then
        select case(INVSING)
        case(0) ! single pixel scenario
          if(ipixstop .lt. npixels) then
            ipixstart = ipixstart + 1
            ipixstop  = ipixstop  + 1
            goto 177 ! next pixel forward computations with iguess (lp=0)
          else
            goto 221 ! forward computations with iguess are done for all pixels (lp=0)
          endif
        case(1,2) ! multi pixel scenario
          goto 221 ! forward computations with iguess are done for all pixels (lp=0)
        end select
      endif ! RIN%ISTOP

12    continue

!C***********************************************************
!C*** DERIVATIVES CALCULATION:                            ***
!C***********************************************************

      DO ipix=ipixstart,ipixstop
        US(:,:,ipix)  = 0.0  !- matrix of Jacobians U
        UFS(:,:,ipix) = 0.0  !- Fisher matrix: (U)^T(C)^(-1) U +...
        FFMS(:,ipix)  = 0.0  !- gradient: contribution of measurements
        FFS(:,ipix)   = 0.0  !- contribution of single-pixel smoothness constraints to the gradient
        FFS0(:,ipix)  = 0.0  !- contribution of a priori estimates to the gradient
        if ( ERREST%product ) then
        if ( .not. ERREST%LM_matrix ) then
        UFS_errest(:,:,ipix) = 0.0  !- Fisher matrix: (U)^T(C)^(-1) U +... for error estimates
        endif
        endif
      ENDDO

      IF(RIN%OSHF%IMSC .NE. RIN%OSHD%IMSC) THEN
        !write(*,*) 'before inversion_forward_model in inversion'
        call inversion_forward_model(                              &
                    iu_main_output,                                &
                    ipixstart,                                     &
                    ipixstop,                                      &
                    RIN,                                           &
                    RIN%OSHD,                                      &
                    0,                                             &
                    lresult,                                       &
                    tau_mol,                                       &
                    NHVP_meas,                                     &
                    HVP_meas,                                      &
                    AP,                                            &
                    MDPR,                                          & ! molecular depolarization, added by Qiaoyun HU
                    GOUT%retrieval%fit%segment_fit%pixels,         &
                    pixel_vec_fit_deriv,                           &
                    GOUT%aerosol,                                  &
                    GOUT%gases,                                    &
                    GOUT%surface,                                  &
                    GOUT%retrieval,                                &
                    nangles,                                       &
                    sca_angles,                                    &
                    KERNELS1,                                      &
                    KERNELS2                                       &
              )

        if ( error_present() ) return
!write(*,*) 'after inversion_forward_model in inversion'
!do ipix=ipixstart,ipixstop
!   KMIMAGE=segment_vec_meas(ipixstop)%KMIMAGE
!   write(*,*) 'deriv AP:'
!   write(*,'(10es14.4)') AP(1:RIN%KNSING,ipix)
!   write(*,*) 'FS:'
!   write(*,'(10es14.4)') segment_vec_fit(ipix)%FS(1:KMIMAGE)
!   write(*,*) 'FS_deriv:'
!   write(*,'(10es14.4)') pixel_vec_fit_deriv(ipix)%FS(1:KMIMAGE)
!   stop
!enddo

      ENDIF ! RIN%OSHF%IMSC .NE. RIN%OSHD%IMSC

      DO ipix=ipixstart,ipixstop
      IF (apriori_smooth) THEN
! smoothness term in gradient of minimized residual
        CALL gradient_apriori_smooth_term ( RIN%KNSINGF,                 &  ! IN
                                            AP(1:RIN%KNSINGF,ipix),      &
                                            SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                            SMSEST,                      &
                                            FFS(1:RIN%KNSINGF,ipix)      & ! OUT
                                          ) 
      ENDIF ! apriori_smooth

      IF (apriori_estim) THEN
! a priori estimates term in gradient of minimized residual
        CALL gradient_apriori_estim_term ( RIN%KNSINGF,                & ! IN
                                           AP(1:RIN%KNSINGF,ipix),     &
                                           AP0(1:RIN%KNSINGF,ipix),    &
                                           SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                           FFS0(1:RIN%KNSINGF,ipix)    & ! OUT
                                         ) 
      ENDIF ! apriori_estim
      ENDDO

      call inversion_jacobian_matrix(                             &
                  iu_main_output,                                 &
                  lp,                                             &
                  ipixstart,                                      &
                  ipixstop,                                       &
                  RIN,                                            &
                  lresult,                                        &
                  tau_mol,                                        &
                  NHVP_meas,                                      &
                  HVP_meas,                                       &
                  AP,                                             &
                  MDPR,                                           & ! molecular depolarization, added by Qiaoyun HU
                  ALS,                                            &
                  GOUT%retrieval%fit%segment_fit%pixels,          &
                  pixel_vec_fit_deriv,                            &
                  GOUT%aerosol,                                   &
                  GOUT%gases,                                     &
                  GOUT%surface,                                   &
                  GOUT%retrieval,                                 &
                  nangles,                                        &
                  sca_angles,                                     &
                  KERNELS1,                                       &
                  KERNELS2,                                       &
                  APMIN,                                          &
                  APMAX,                                          &
                  dermask,                                        &
                  US                                              &
            )
      if ( error_present() ) return

!WRITE(*,*) 'ALS', ALS(1)

      DO ipix=ipixstart,ipixstop
!******************************************************************************
! Measurement term in Fisher matrix for each pixel (Up)^T W^(-1) (Up);  UFS(:,:,ipix)
! Measurement term in gradient of minimized residual for each pixel (Up)^T W^(-1) (fp - f)); FFMS(:,ipix)
!******************************************************************************

       KMIMAGE = segment_vec_meas(ipix)%KMIMAGE
!write(*,*) 'ipix, KMIMAGE', ipix, KMIMAGE

       CALL FISHMX ( KMIMAGE,RIN%KNSINGF,   &
                     segment_vec_meas(ipix)%FS(:), &
                     segment_vec_fit(ipix)%FS(:),  &
                     CS(:,ipix),            &
                     US(:,:,ipix),          & 
                     UFS(:,:,ipix),         & ! OUT
                     FFMS(:,ipix)           & 
                  )
       if ( ERREST%product ) then
       if ( .not. ERREST%LM_matrix ) then
       UFS_errest(:,:,ipix) = UFS(:,:,ipix)
       endif
       endif

    FMEAS(1:KMIMAGE,ipix) = segment_vec_meas(ipix)%FS(1:KMIMAGE)
    FFIT(1:KMIMAGE,ipix) = segment_vec_fit(ipix)%FS(1:KMIMAGE)

!stop
!write(*,'(2(a,i0,2x))') 'ipix = ',ipix,'lp = ',lp+1
!write(*,*) 'FS: '
!write(*,'(2(a,i0,2x))') 'ipix = ',ipix,'lp = ',lp+1!write(*,*) 'FS: '
!write(*,*) segment_vec_meas(ipix)%FS(:)
!write(*,*) 'FPS: '
!write(*,*) segment_vec_fit(ipix)%FS(:)
!if (ipix .eq.1 ) then
!write(*,*) 'CS: '
!write(*,'(10e12.4)') CS(:,ipix)
!write(*,*) CS(1:13,ipix)
!endif
!write(*,*) 'US: '
!write(*,'(10es12.4)') US(:,:,ipix)
!if (ipix .eq. 1) then
!write(*,*) 'UFS: '
!write(*,*) UFS(:,:,ipix)

!write(*,'(10es12.4)') UFS(14,1:32,1)
!write(*,*) 'US: '
!write(*,'(10es12.4)') US(1:13,14,ipix)
!endif !ipix
!write(*,*) 'FFMS1: '
!write(*,*) FFMS(:,ipix)

!write(*,*) 'ipix=',ipix
!write(*,*)
!write(*,*) 'FS: '
!write(*,'(10e12.4)') segment_vec_meas(ipix)%FS(1:KMIMAGE)
!write(*,*) 'FPS: '
!write(*,'(10e12.4)') segment_vec_fit(ipix)%FS(1:KMIMAGE)

!write(*,*) 'UFS after FISHMX :'
!write(*,*) 'UFS:'
!do i=1,RIN%KNSINGF
!write(*,'(a,i0)') 'i = ',i
!write(*,'(10e12.4)') UFS(i,1:RIN%KNSINGF,ipix)
!enddo
!write(*,*) 'FFMS:'
!write(*,'(10e12.4)') FFMS(1:RIN%KNSINGF,ipix)
!stop
!C*****************************************************
!C*   INCLUSION of SMOOTHNESS  AND A PRIORI  ESTIMATES *
!C*   HERE the weight of a priori term is enhanced at
!C*   earlier iterations and decreasing  to assumed one
!C*   with decrease of total residual to expected
!C*   the methodology described by  Dubovik [2004]    *
!C********************************************************************************

!write(*,*) 'FFS:'
!write(*,'(10e12.4)') FFS(1:RIN%KNSING,ipix)      
!C******************************************************************************
! Adding A priori smoothness term  to Fisher matrix for each pixel (single-pixel):
!       (Up)^T W^(-1) (Up) + (D_singl)^T(D_singl) : - UFS(:,:,ipix)
!!******************************************************************************
! Adding A priori smoothnes contribution in gradient of minimized residual for each pixel (single-pixel):
!       (Up)^T W^(-1) (fp - f)) + ((D_singl)^T(D_singl)Ap-0) : - FFMS(:,ipix)
!C*****************************************************************************
      IF(apriori_smooth) THEN
!*** calculation of contribution of single-pixel smoothness constraints to
!*** the gradient of minimized residual
        CALL gradient_apriori_smooth_term ( RIN%KNSINGF,                 &  ! IN
                                            AP(1:RIN%KNSINGF,ipix),      &
                                            SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                            SMSEST,                      &
                                            FFS(1:RIN%KNSINGF,ipix)      & ! OUT
                                          ) 
!*** inlcuding smooth conributions to Fisher matrix and gradient
         UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) = &
         UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) + ccor*SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF)
         FFMS(1:RIN%KNSINGF,ipix) = FFMS(1:RIN%KNSINGF,ipix) + ccor*FFS(1:RIN%KNSINGF,ipix)
!! save this term to add in the new gradient for errors:
         FFS_aux(1:RIN%KNSINGF,ipix) = ccor*FFS(1:RIN%KNSINGF,ipix)

         if ( ERREST%product ) then
         if ( .not. ERREST%LM_matrix ) then
         UFS_errest(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) = &
         UFS_errest(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) + SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF)
         endif
         endif
      ENDIF
!C******************************************************************************
! Adding A priori estimate term  to Fisher matrix for each pixel (single-pixel)
!       (Up)^T W^(-1) (Up) + (D_singl)^T(D_singl)+ (Wa)^(-1) : - UFS(:,:,ipix)
!C******************************************************************************
! Adding A priori estimates contribution in gradient of minimized residual for each pixel (single-pixel):
!       (Up)^T W^(-1) (fp - f)) + ((D_singl)^T(D_singl)Ap - 0) + (Wa)^(-1) (Ap - A0)) : - FFMS(:,ipix)
!C****************************************************************************
      IF(apriori_estim) THEN
!*** calculation of contribution of single-pixel a priori estimates to
!*** the gradient of minimized residual
        CALL gradient_apriori_estim_term (  RIN%KNSINGF,                & ! IN
                                            AP(1:RIN%KNSINGF,ipix),     &
                                            AP0(1:RIN%KNSINGF,ipix),    &
                                            SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                            FFS0(1:RIN%KNSINGF,ipix)    & ! OUT
                                         ) 
!*** inlcuding a priori estimation conributions to Fisher matrix and gradient
         UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) = &
         UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) + ccor*SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF)
         FFMS(1:RIN%KNSINGF,ipix) = &
         FFMS(1:RIN%KNSINGF,ipix) + ccor*FFS0(1:RIN%KNSINGF,ipix)
!! save this term to add in the new gradient for errors:
         FFS0_aux(1:RIN%KNSINGF,ipix) = ccor*FFS0(1:RIN%KNSINGF,ipix)
         if ( ERREST%product ) then
         if ( .not. ERREST%LM_matrix ) then
         UFS_errest(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) = &
         UFS_errest(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) + SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF)
         endif
         endif
      ENDIF

      if (RIN%IPRI_additional_info) then
      DO IS1=1,npixels
      DO I=1,RIN%KNSINGF
            if(abs(UFS(i,i,IS1)) .le. 0.0) then
              write(tmp_message,'(2a,2(a,i0,2x),a,es12.4,2(2a,i0))') &
              'Before Levenberg–Marquardt type constraint inclusion:', &
              NEW_LINE('A'), &
              'ipix = ',IS1,'i = ',i,'Fisher matrix (UFS) diagonal element = ',UFS(i,i,IS1), &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_of_Levenberg-Marquardt = ',RIN%IPSTOP, &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_for_stopping = ',RIN%MAXP
              G_ERROR(trim(tmp_message))
            endif
      ENDDO ! I
      ENDDO ! IS1
      endif

!*******************************************************************************
!* Levenberg–Marquardt type constraints inclusion                              *
!*******************************************************************************
!*  This correction is included according                                      *
!*  the methodology described by Dubovik & King [2000]                         *
!*  and Dubovik [2004]                                                         *
!*******************************************************************************
      IF ( RIN%IPSTOP .GT. 0 ) THEN
      IF ( LP .LE. RIN%IPSTOP ) THEN
        if ( ERREST%product ) then
        if ( ERREST%LM_vector ) then
        ERREST%LM_term(:,ipix) = 0.0
        endif
        endif
        DO I=1,RIN%KNSINGF
          ALM = (APMAX(I)-APMIN(I))*0.5
          !write(iu_main_output,'(a,3i4,3es13.5)') 'lp, ipix, i, ALM, UFS_noLM, LM_term - ', &
          !lp+1, ipix, i, ALM, UFS(I,I, ipix), (ALS(ipix)*KM1_pix(ipix))/(ALM*ALM*RIN%KNSINGF)
          !UFS(I,I,ipix)=UFS(I,I,ipix)+ALS(ipix)/(ALM*ALM*RIN%KNSINGF)
          lm_term(I) = ( ALS(ipix)*KM1_pix(ipix) ) / ( ALM*ALM*RIN%KNSINGF )
          if ( lm_term(I) .lt. RIN%LM_MIN ) then
          lm_term(I) = RIN%LM_MIN
          endif
          UFS(I,I,ipix) = UFS(I,I,ipix) + lm_term(I)
          !write(*,*) ipix,ALS(ipix),AGEN,'  ipix,ALS(ipix),AGEN'
          !additional stabilization of a diagonal of the error estimation matrix
          if ( ERREST%product ) then
          if ( ERREST%LM_vector ) then
          ERREST%LM_term(I,ipix) = lm_term(I)
          endif
          endif
        ENDDO ! I
        if(RIN%IPRI_additional_info) then
        write(iu_main_output,'(a)') 'Levenberg–Marquardt type constraint inclusions:'
        write(iu_main_output,'(2(a,i0,2x))') 'lp = ',lp+1,'ipix = ',ipix
        write(iu_main_output,'(10e14.6)') (lm_term(I),i=1,RIN%KNSINGF)
        endif
        !write(*,'(a,10e14.4)') '2: ALS:     ',ALS(1:npixels)
        !write(*,'(a,10i14)') '2: KM1_pix: ',KM1_pix(1:npixels)
      ELSE
        ERREST%LM_matrix = .false.
        ERREST%LM_vector = .false.
      ENDIF ! LP .LE. IPSTOP
      ENDIF
!*******************************************************************************
      ENDDO ! ipix
!write(*,*) 'FFMS2: '
!write(*,'(10e16.6)') FFMS
!write(*,*) 'FFS: '
!write(*,'(10e16.6)') FFS
!write(*,*) 'FFS0: '
!write(*,'(10e16.6)') FFS0

!stop 'stop in iP_MTG.f90 after normal_system'
!write(*,*) 'FFMS3:'
!write(*,'(10e16.6)') FFMS

!********************************************************************************
!**           Solving linear system of equation for
!**                    each p-iterations
!********************************************************************************
!   The solution is organized as q-iterations (Dubovik and King, 2000; Dubovik 2004)
!   The following linear system is solved: A d(AP) = F(AP)-F
!   the p-th sollution approximation is searched as:    AP+1=AP - d(AP)
!            (d(AP))q+1=(d(AP))q - QS, where QS is iterative solution of system: A QS= A(d(AP))q-[F(AP)-F], i.e.
!            (d(AP))q+1=(d(AP))q - H [A(d(AP))q-[F(AP)-F]];
!   *NOTE* that if H=A^(-1), then we get normal Newton-Gauss:AP+1=AP-A^(-1)[F(AP)-F]
!C********************************************************************************
SELECT CASE(INVSING)
CASE(0) ! Single Pixel inversion
      QS(:,ipixstop)  = 0.0  ! do not delete
      if(RIN%IMQ .eq. 1 .or. RIN%IMQ .eq. 2) then
!********************************************************************************
!*** The linear system A QS = F(AP)-F using SVD  solver :
!********************************************************************************
        do ipix=ipixstart,ipixstop
!write(*,*)
!write(*,*) 'UFS:'
!write(*,'(10e12.4)') UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix)
!write(*,*) 'FFMS:'
!write(*,'(10e12.4)') FFMS(1:RIN%KNSINGF,ipix)
!write(*,*) 
!write(*,*) 'SVD UFS diagonal: LP=',LP
!do i=1,RIN%KNSINGF
!write(*,'(e12.4)') UFS(i,i,ipix)
!enddo
          if(LP .gt. RIN%IPSTOP) then
          do i=1,RIN%KNSINGF
            if(UFS(i,i,ipix) .eq. 0.0) then
              !UFS(i,i,ipix) = 1e-30
              write(tmp_message,'(a,i0,2(2a,i0),3a)') &
              'Fisher matrix (UFS) diagonal element is ZERO for retrieved parameter # ',i, &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_of_Levenberg-Marquardt = ',RIN%IPSTOP, &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_for_stopping = ',RIN%MAXP, &
              NEW_LINE('A'), &
              'Suggestion: set maximum_iterations_of_Levenberg-Marquardt', &
              ' equal to maximum_iterations_for_stopping in settings file.'
              G_ERROR(trim(tmp_message))
            endif
          enddo
          endif

          call ITERQ (  RIN%IMQ,RIN%KNSINGF,                    & ! IN
                        UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix),  &
                        FFMS(1:RIN%KNSINGF,ipix),               &
                        QS(1:RIN%KNSINGF,ipix)                  & ! OUT
                     ) 
        enddo ! ipix
      elseif(RIN%IMQ .eq. 3) then
!C********************************************************************************
!C*** The linear system A QS = F(AP)-F using sparse matrix solver :
!C********************************************************************************
        do ipix=ipixstart,ipixstop
!write(*,*) 'SuperLU UFS diagonal:: LP=',LP
!do i=1,RIN%KNSINGF
!write(*,'(e12.4)') UFS(i,i,ipix)
!enddo
          if(LP .gt. RIN%IPSTOP) then
            do i=1,RIN%KNSINGF
            if(UFS(i,i,ipix) .eq. 0.0) then
              !UFS(i,i,ipix) = 1e-30
              write(tmp_message,'(a,i0,2(2a,i0),3a)') &
              'Fisher matrix (UFS) diagonal element is ZERO for retrieved parameter # ',i, &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_of_Levenberg-Marquardt = ',RIN%IPSTOP, &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_for_stopping = ',RIN%MAXP, &
              NEW_LINE('A'), &
              'Suggestion: set maximum_iterations_of_Levenberg-Marquardt', &
              ' equal to maximum_iterations_for_stopping in settings file.'
              G_ERROR(trim(tmp_message))
            endif
            enddo
          else
            if ( ERREST%product ) then
            if ( .not. ERREST%LM_matrix ) then
            do i=1,RIN%KNSINGF
            if(UFS_errest(i,i,ipix) .eq. 0.0) then
              !UFS_errest(i,i,ipix) = 1e-30
              write(tmp_message,'(a,i0,2(2a,i0),3a)') &
              'Fisher matrix (UFS_errest) diagonal element is ZERO for retrieved parameter # ',i, &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_of_Levenberg-Marquardt = ',RIN%IPSTOP, &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_for_stopping = ',RIN%MAXP, &
              NEW_LINE('A'), &
              'Suggestion: set maximum_iterations_of_Levenberg-Marquardt', &
              ' equal to maximum_iterations_for_stopping in settings file.'
              G_ERROR(trim(tmp_message))
            endif
            enddo
            endif
            endif
          endif ! LP .ge. RIN%IPSTOP

!C*** preparation of matrix A = UF as large vertor with non-zero ellements   :
          call UF_nonzero_one_pixel ( RIN%KNSINGF,UFS(:,:,ipix), & ! IN
                                      UFNZ,nnz                   & ! OUT
                                    ) 
          if ( error_present() ) return
          b(1:RIN%KNSINGF) = FFMS(1:RIN%KNSINGF,ipix)
!write(*,*) 'before b:'
!write(*,'(10e16.5)') b(1:RIN%KNSINGF)
!write(*,*) 'UFNZ:'
!do i=1,nnz
!write(*,'(2i5,e16.5)') UFNZ%col(i),UFNZ%row(i),UFNZ%val(i)
!enddo
          !if(RIN%IPRI_verbose)  write(iu_main_output,*)  &
          !'UF inversion, before sparse_solver ',trim(sparse_solver_name),'  nnz=',nnz  
          if(sparse_solver_name .eq. 'SUPERLU') &
          call solver_init_SUPERLU(RIN%KNSINGF, nnz, UFNZ, b)
          call sparse_solver ( iu_main_output,       & ! IN
                               RIN%KNSINGF,nnz,UFNZ, &
                               b,                    & ! INOUT
                               solver_timer          &
                             )
          if ( error_present() ) then
            if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_cleanup_SUPERLU(RIN%KNSINGF, nnz, UFNZ, b)
            return
          end if
          !if(RIN%IPRI_verbose)  write(iu_main_output,*)  &
          !'UF inversion, after  sparse_solver ',trim(sparse_solver_name)
          QS(1:RIN%KNSINGF,ipix) = b(1:RIN%KNSINGF)
          call sparse_solver_cleanup(RIN%KNSINGF, nnz, UFNZ, b)
          ! Matrix for error estimates
          if ( LP .le. RIN%IPSTOP ) then
          if ( ERREST%product ) then
          if ( .not. ERREST%LM_matrix ) then
            call UF_nonzero_one_pixel ( RIN%KNSINGF,UFS_errest(:,:,ipix), & ! IN
                                        UFNZ_errest,nnz_err               & ! OUT
                                      )
            if ( error_present() ) return
          endif
          endif
          endif
        enddo ! ipix
      endif ! RIN%IMQ .eq. 1 .or.
!**********************************************************************************************
CASE(1,2) !***  Multi Pixel inversion ****
!**********************************************************************************************
      FFMS0(:,:) = 0.0 ! ask Oleg
      DO I=1,RIN%KNSINGF
        DO IS1=1,npixels
!**********************************************************************************************
!*** Adding inter-pixel smothness contributions ONLY!!! to the diagonal ellement of Fisher matrix
          UFS(I,I,IS1) = UFS(I,I,IS1) + ccor*SMMULTI(IS1,I,IS1)
          if ( LP .le. RIN%IPSTOP ) then
          if ( ERREST%product ) then
          if ( .not. ERREST%LM_matrix ) then
          UFS_errest(I,I,IS1) = UFS_errest(I,I,IS1) + SMMULTI(IS1,I,IS1)
          endif
          endif
          endif
!**********************************************************************************************
!*** Adding inter-pixel smothness contributions to gradient estimation at p-th iteration
!*** * NOTE*: if there are a priori estimates values near adges, outsode of inverted segment
!***          part of their contribtuon is inlcuded in this term
          FFMS0(I,IS1) = FFMS0(I,IS1) + ccor*DOT_PRODUCT(SMMULTI(IS1,I,1:npixels),AP(I,1:npixels))
!**********************************************************************************************
!*** !   adding second part of contribtuon form a priori estimates near edges to the gradient
!od '-' replaced with'+'
          !IF(ledges) FFMS0(I,IS1) = FFMS0(I,IS1) - ccor*FF_edge(I,IS1)
          IF(ledges) FFMS0(I,IS1) = FFMS0(I,IS1) + ccor*FF_edge(I,IS1)
        ENDDO ! IS1
      ENDDO ! I

      if(RIN%IPRI_verbose) then
      ! test diagonal elements of matrix
      DO IS1=1,npixels
      DO I=1,RIN%KNSINGF
            if(abs(UFS(i,i,IS1)) .le. 0.0) then
              write(tmp_message,'(2(a,i0,2x),a,es12.4,2(2a,i0),3a)') 'ipix = ',IS1,'i = ',i, &
              'Fisher matrix (UFS) diagonal element = ',UFS(i,i,IS1), &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_of_Levenberg-Marquardt = ',RIN%IPSTOP, &
              NEW_LINE('A'), &
              'retrieval.convergence.maximum_iterations_for_stopping = ',RIN%MAXP, &
              NEW_LINE('A'), &
              'Suggestion: set maximum_iterations_of_Levenberg-Marquardt', &
              ' equal to maximum_iterations_for_stopping in settings file.'
              G_ERROR(trim(tmp_message))
            endif
      ENDDO ! I
      ENDDO ! IS1
      endif

      if(RIN%IPRI_additional_info) then
      ! test diagonal elements of matrix
        write(iu_main_output,'(a)') 'Matrix diagonal elements:'
        DO IS1=1,npixels
          write(iu_main_output,'(2(a,i0,2x))') 'lp = ',lp+1,'ipix = ',IS1
          write(iu_main_output,'(10es14.6)') (UFS(i,i,IS1),i=1,RIN%KNSINGF)
        ENDDO ! IS1
        write(iu_main_output,*)
        !DO IS1=1,npixels
        !write(*,'(2(a,i0,2x))') 'lp = ',lp+1,'ipix = ',IS1
        !write(*,*) 'UFS before solver (multi pixel scenario):'
        !do i=1,RIN%KNSINGF
        !write(*,'(a,i0)') 'i = ',i
        !write(*,'(10e12.4)') UFS(i,1:RIN%KNSINGF,IS1)
        !enddo
        !ENDDO ! IS1
      endif

!**********************************************************************************************
      IKQ =1
      IF(RIN%IMQ .eq. 3) THEN ! Sparse matrix normal system solver for segment
          !write(*,*) 'before UF_nonzero,  &
          !'  size(UFNZ)(bit)=',size(UFNZ%val)*8+size(UFNZ%row)*4+size(UFNZ%col)*4+size(UFNZ%nval)*4
!**********************************************************************************************
!** preparation of matrix UF = [UFS +(D_inter)^T (D_inter)] as large vertor with non-zero ellements:
! *NOTE* that in "UF_nonzero" non-diagonal ellement of (D_inter)^T (D_inter) are added before
!        generating final UF_nonzero vector
!**********************************************************************************************
          CALL UF_nonzero ( RIN%KNSINGF,npixels,UFS, & ! IN
                            ccor,SMMULTI,            &
                            UFNZ,nnz                 & ! OUT
                          )
          if ( error_present() ) return
!tl          nnz_err = nnz
          !WRITE(*,*) 'nnz=',nnz
          if ( test_UFNZ ) call UFNZ_symmetry_check ( KNF,nnz,UFNZ,UF )
          !  Matrix for error estimates
          if ( LP .le. RIN%IPSTOP ) then
          if ( ERREST%product ) then
          if ( .not. ERREST%LM_matrix ) then
            CALL UF_nonzero ( RIN%KNSINGF,npixels,UFS_errest, & ! IN
                              1.0,SMMULTI,                    &
                              UFNZ_errest,nnz_err             & ! OUT
                            )
            if ( error_present() ) return
          endif
          endif
          endif
      ELSE IF(RIN%IMQ .eq. 2 .or. test_UFNZ)  THEN ! SVD normal system solver for segment
          ! filling in UF matrix 
          call UF_matrix (  RIN,KNF,npixels,UFS,        & ! IN
                            ccor,SMMULTI,NISM,KSM,IMSM, &
                            UF,nnz                      & ! OUT
                         )
      ELSE IF(RIN%IMQ .eq. 1)  THEN ! simple linear iterations ~ Dubovik et al. [1995] ~
          call matrix_Q_iter (RIN%KNSINGF,npixels,UFS,SMMULTI,NISM,KSM,IMSM,ccor,AAI)
          IKQ = INT(1e+7)/npixels  !IKQ = 10
      ENDIF ! RIN%IMQ .eq. 3		 

      IIQ = 0
      !QS(:,:) = 0.0
! begin solving equation for multi-pixel scenario and its correction
121   CONTINUE 
!**********************************************************************************************
! inclusion of INTER-PIXEL SMOOTHNESS CONSTRAINTS See Dubovik et al.[2011, 2008])
!**********************************************************************************************
      call inter_pix_smooth_constr_incl ( RIN%KNSINGF,npixels,      &
                                          UFS,QS,FFMS,FFMS0,        &
                                          SMMULTI,NISM,KSM,IMSM,ccor, &
                                          FFMSQ                     &
                                        )
      AQAFTER = 0.0
      DO ipix=1,npixels
        CALL residual ( IPRI_additional_info,iu_main_output,     & ! IN
                        RIN%KNSINGF,                 &
                        FFMSQ(1:RIN%KNSINGF,ipix),   &
                        FFMSQ(1:RIN%KNSINGF,ipix),   &
                        AAQ                          & ! OUT
                      ) 
        AQAFTER = AQAFTER+AAQ      
      ENDDO  ! ipix
      IF(IIQ .EQ. 0) AQBEFORE = AQAFTER

      EPSQ_test = (AQAFTER-AQBEFORE)
      if(RIN%IMQ .eq. 1) EPSQ_test = ABS(EPSQ_test)
      EPSQ_test = EPSQ_test/AQBEFORE

      IF(IPRI_additional_info) THEN
        write(iu_main_output,*)     &
        SQRT(AQAFTER/KNF),SQRT(AQBEFORE/KNF),IIQ,IKQ,EPSQ_test,RIN%EPSQ,  &
        '  AQAFTER,AQBEFORE,IIQ,IKQ,EPSQ_test,EPSQ' 
      ENDIF
      
      IF(IIQ .EQ. 0 .OR. (IIQ .LT. IKQ .AND. EPSQ_test .GT. RIN%EPSQ)) THEN		
        AQBEFORE = AQAFTER
        IF(RIN%IMQ .GE. 2) THEN
          FFMI0(:) = 0.0
          DO ipix=1,npixels
            DO I=1,RIN%KNSINGF
              FFMI0((ipix-1)*RIN%KNSINGF+I) = FFMSQ(I,ipix)
            ENDDO ! I
          ENDDO ! ipix

          if(RIN%IPRI_verbose) &
          write(iu_main_output,'(a,i0,a)') 'KNF = ',KNF, &
          ' - number of retrieved parameters for TOTAL SEGMENT'

            IF(RIN%IMQ .eq. 2) THEN
              if(RIN%IPRI_verbose)  write(iu_main_output,*) 'UF inversion by SVD'  
              CALL ITERQ (  RIN%IMQ,KNF,           &
                            UF(1:KNF,1:KNF),       &
                            FFMI0(1:KNF),          &
                            QIM(1:KNF)             & ! OUT
                         )
            ELSEIF(RIN%IMQ .EQ. 3) THEN
              b(1:KNF) = FFMI0(1:KNF)

              !write(*,*) 'b:'
              !write(*,'(10e16.5)') b(1:KNF)
              !write(*,*) 'UFNZ:'
              !do i=1,nnz
                !write(*,'(2i5,e16.5)') UFNZ%col(i),UFNZ%row(i),UFNZ%val(i)
              !enddo 
              !if(RIN%IPRI_verbose)  write(iu_main_output,*)  &
              !'UF inversion, before sparse_solver ',trim(sparse_solver_name),'  nnz=',nnz
              if(sparse_solver_name .eq. 'SUPERLU') &
              call solver_init_SUPERLU(KNF, nnz, UFNZ, b)
              call sparse_solver (  iu_main_output, & ! IN
                                    KNF,nnz,UFNZ,      &
                                    b,                 & ! INOUT
                                    solver_timer       &							   
                                 )
              if ( error_present() ) then
                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_init_SUPERLU(KNF, nnz, UFNZ, b)
                return
              end if
              !if(RIN%IPRI_verbose)  write(iu_main_output,*)  &
              !'UF inversion, after  sparse_solver ',trim(sparse_solver_name)         
              QIM(1:KNF) = b(1:KNF)
              call sparse_solver_cleanup(KNF, nnz, UFNZ, b)
            ENDIF ! RIN%IMQ .eq. 2
        ENDIF ! RIN%IMQ .ge. 2 

! Solution correction
        SELECT CASE(RIN%IMQ)
        CASE(1) 
          DO ipix=1,npixels
            DO I=1,RIN%KNSINGF
              QS(I,ipix) = QS(I,ipix)-FFMSQ(I,ipix)/AAI(I,ipix)
            ENDDO ! I
          ENDDO ! ipix
        CASE(2,3)
          DO ipix=1,npixels
            DO I=1,RIN%KNSINGF
              QS(I,ipix) = QS(I,ipix)-QIM((ipix-1)*RIN%KNSINGF+I)
            ENDDO ! I
          ENDDO ! ipix
        CASE DEFAULT
          write(tmp_message,'(a,i0,2x,a)') 'Q-interetions: IMQ = ',RIN%IMQ,'unknown value'
          G_ERROR(trim(tmp_message))
        END SELECT ! RIN%IMQ

        IIQ = IIQ+1
        GO TO 121
      ENDIF ! IIQ .EQ. 0 .OR. (IIQ .LT. IKQ .AND. EPSQ_test .GT. RIN%EPSQ)
! end solving equation for multi-pixel scenario and its correction
      IF(IPRI_additional_info) WRITE(iu_main_output,*) SQRT(AQAFTER/KNF),SQRT(AQBEFORE/KNF),IIQ,IKQ,  &
                                              '  AQAFTER, AQBEFORE, IIQ, IKQ'
END SELECT ! INVSING  

!C*************************************************************
!C***   END of Solving linear system of equation for 
!C**                    each p-iterations
!C*************************************************************

!C*************************************************************
!C** Determining the optimum length of the correction 
!C**             DELTA AP()
!C*************************************************************     
      LP     = LP+1
      TCINT  = 1.0
      IAC    = 0

! FD hardcoded for debugging
!      IF(RIN%IPRI_verbose) write(6,'(a,i3)') 'LP iteration # ',LP

LOOP_IAC: DO WHILE (IAC .LT. 5)

      APQ(1:RIN%KNSINGF,ipixstart:ipixstop) =                  &
                      AP(1:RIN%KNSINGF,ipixstart:ipixstop) -   &
                      TCINT*QS(1:RIN%KNSINGF,ipixstart:ipixstop)
!      DO ipix=ipixstart,ipixstop
!        IF(RIN%KL .EQ. 1) THEN
!          WHERE(APQ(1:RIN%KNSINGF,ipix) .LT. APMIN(1:RIN%KNSINGF))  &
!          APQ(1:RIN%KNSINGF,ipix)=APMIN(1:RIN%KNSINGF)
!          WHERE(APQ(1:RIN%KNSINGF,ipix) .GT. APMAX(1:RIN%KNSINGF))  &
!          APQ(1:RIN%KNSINGF,ipix)=APMAX(1:RIN%KNSINGF)
!        ELSE
!          WHERE(APQ(1:RIN%KNSINGF,ipix) .LT. RIN%APSMIN(1:RIN%KNSINGF)) &
!          APQ(1:RIN%KNSINGF,ipix)=RIN%APSMIN(1:RIN%KNSINGF)
!          WHERE(APQ(1:RIN%KNSINGF,ipix) .GT. RIN%APSMAX(1:RIN%KNSINGF)) &
!          APQ(1:RIN%KNSINGF,ipix)=RIN%APSMAX(1:RIN%KNSINGF)
!        ENDIF ! RIN%KL .EQ. 1
!      ENDDO ! ipix

!write(iu_main_output,*) IAC,TCINT,'  IAC, TCINT'
!write(*,*) "QS:"
!write(),'(10e12.4)') QS(1:RIN%KNSINGF,ipixstart:ipixstop)

      ALSQ(:) = 0.0
      ASMO(:) = 0.0
      AEST(:) = 0.0

      DO ipix=ipixstart,ipixstop
         segment_vec_fit(ipix)%FS(:) = 0.0
         call MIN_AP_MAX(1,RIN%KNSINGF,APMIN,APMAX,APQ(:,ipix))
      ENDDO

      call inversion_forward_model(                              &
                  iu_main_output,                                &
                  ipixstart,                                     &
                  ipixstop,                                      &
                  RIN,                                           &
                  RIN%OSHF,                                      &
                  0,                                             &
                  lresult,                                       &
                  tau_mol,                                       &
                  NHVP_meas,                                     &
                  HVP_meas,                                      &
                  APQ,                                           &
                  MDPR,                                          & ! molecular depolarization, added by Qiaoyun HU
                  GOUT%retrieval%fit%segment_fit%pixels,         &
                  segment_vec_fit,                               &
                  GOUT%aerosol,                                  &
                  GOUT%gases,                                    &
                  GOUT%surface,                                  &
                  GOUT%retrieval,                                &
                  nangles,                                       &
                  sca_angles,                                    &
                  KERNELS1,                                      &
                  KERNELS2                                       &
            )

      DO ipix=ipixstart,ipixstop
        KMIMAGE=segment_vec_meas(ipix)%KMIMAGE
       !DO I=1,KMIMAGE
        !!WRITE(iu_main_output,*)   &
        !WRITE(*,*)   &
        !ipix,I,segment_vec_fit(ipix)%FS(I),'   - ipix,I,FPS(I,ipix)) AFTER forward_model_pixel'
       !ENDDO

! residual (F-FP)^T C (F-FP)                  
         CALL residual_meas_term (  IPRI_additional_info,iu_main_output,  & ! IN
                                    KMIMAGE,                  &
                                    segment_vec_meas(ipix)%FS(1:KMIMAGE), &
                                    segment_vec_fit (ipix)%FS(1:KMIMAGE), &
                                    CS(1:KMIMAGE,ipix),       & 
                                    AMEAS(ipix)               & ! OUT
                                 )

!write(*,*) 'AMEAS =',AMEAS(ipix)
!write(*,*) 'FS_meas:'
!write(*,'(10e14.6)') segment_vec_meas(ipix)%FS(1:KMIMAGE)
!write(*,*) 'FS_fit:'
!write(*,'(10e14.6)') segment_vec_fit (ipix)%FS(1:KMIMAGE)
!write(*,*) 'CS:'
!write(*,'(10e14.6)') CS(1:KMIMAGE,ipix)
!stop 'test in inversion'

! residual AP^T SMSING AP
         IF (apriori_smooth) &
         CALL residual_apriori_smooth_term ( IPRI_additional_info,iu_main_output,  & ! IN
                                             RIN%KNSINGF,              &
                                             APQ(1:RIN%KNSINGF,ipix),  &
                                             SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                             SMSEST,                   &
                                             ASMO(ipix)                & ! OUT
                                           ) 

! residual (AP-AP0)^T SMSING0 (AP-AP0); A PRIORI ESTIMATE TERM of the residual
         IF (apriori_estim) &
         CALL residual_apriori_estim_term ( IPRI_additional_info,iu_main_output,  & ! IN
                                            RIN%KNSINGF,              &
                                            APQ(1:RIN%KNSINGF,ipix),  &
                                            AP0(1:RIN%KNSINGF,ipix),  &
                                            SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                            AEST(ipix)                & ! OUT
                                          ) 
           
!write(*,*) 'ipix=',ipix,'  ASMO(ipix)=',ASMO(ipix),'  AEST(ipix)=',AEST(ipix)
!******* END of A PRIORI TERMS *******************************

!tl      write(*,'(a,5i5)') 'ipix,KMIMAGE,KNSING,IKS,IKS1: ',ipix,KMIMAGE,RIN%KNSING,IKS,IKS1
!tl      write(*,'(a,3e13.5)') 'AMEAS,ASMO,AESTQ: ',AMEAS(ipix),ASMO(ipix),AEST(ipix)
	  	  	                   
        ccor = AMEAS(ipix)/(ARES2(ipix)*KMIMAGE)
        if(ccor .lt. ccor_min) ccor = ccor_min
        if(ccor .gt. ccor_max) ccor = ccor_max
        ALSQ(ipix) = AMEAS(ipix)
        if(apriori_smooth)  ALSQ(ipix) = ALSQ(ipix) + ccor*ASMO(ipix)
        if(apriori_estim)   ALSQ(ipix) = ALSQ(ipix) + ccor*AEST(ipix) 
        ALSQ(ipix) = ALSQ(ipix)/KM1_pix(ipix)
        ENDDO ! ipix=ipixstart,ipixstop
!      write(*,'(a,10e14.4)') '3: ALSQ:    ',ALSQ(1:npixels)
!      write(*,'(a,10i14)')   '3: KM1_pix: ',KM1_pix(1:npixels)
!      write(*,'(a,10e14.4)') '3: AMEAS:   ',AMEAS(1:npixels)                                         
!      write(*,'(a,10e14.4)') '3: ASMO:    ',ASMO(1:npixels)                                         
!      write(*,'(a,10e14.4)') '3: AEST :   ',AEST(1:npixels)                                         
!      write(*,'(a,10e14.4)') '3: ARES2:   ',ARES2(1:npixels)                                         
!      write(*,'(a,10i14)')   '3: KMIMAGE: ',segment_vec_meas(1:npixels)%KMIMAGE                                         
                  
      select case(INVSING)
      case(0)    ! pixel by pixel scenario
         AGENQ = ALSQ(ipixstop) !/KM1_pix(ipixstop)      
      case(1,2)  ! multi pixel scenario
         ccor = SUM(AMEAS(1:npixels))/  &
                SUM(ARES2(1:npixels)*segment_vec_meas(1:npixels)%KMIMAGE)  
         if(ccor .lt. ccor_min) ccor = ccor_min
         if(ccor .gt. ccor_max) ccor = ccor_max
         AGENQ = SUM(AMEAS(1:npixels))
         if(apriori_smooth)  AGENQ = AGENQ+ccor*SUM(ASMO(1:npixels))
         if(apriori_estim)   AGENQ = AGENQ+ccor*SUM(AEST(1:npixels)) 
! inter-pixel smoothnes constraint term of residual, See  Dubovik et al. [2011]
         call residual_inter_pixel_term(IPRI_additional_info,iu_main_output,RIN%KNSINGF,npixels,APQ,SMMULTI,  &
                                                            ledges,FF_edge,AB_edge,AFS0)
         AGENQ = (AGENQ+ccor*AFS0)/KM1_segm
         if(IPRI_additional_info) then
            write(iu_main_output,*) SQRT(AGENQ)*100.0,'  AGENQ (ERR %)'
            write(iu_main_output,'(2e16.7,i12,a)') AFS0,ccor,KM1_segm,'  AFTER0=AFS0,ccor,KM1_segm'
         endif ! IPRI_additional_info
      end select ! INVSING 

      EPSINT = (AGEN-AGENQ)/AGEN
      if(IPRI_additional_info) then
        write(iu_main_output,*) EPSINT,'  EPSINT'
        write(iu_main_output,*) SQRT(AGENQ)*100.,SQRT(AGEN)*100.,'  AGENQ,AGEN (ERR%)'
      endif ! IF(IPRI_additional_info)

      IF(IPRI_additional_info)  &
      WRITE(iu_main_output,*) IAC,TCINT,EPSINT,'  IAC, TCINT,EPSINT'
      
      IF(EPSINT .LE. 0.) THEN
        IAC = IAC+1
        TCINT = TCINT*0.5
      ELSE
        EXIT LOOP_IAC
      ENDIF ! EPSINT .LE. 0.

ENDDO LOOP_IAC

      IF(EPSINT .LE. 0.) THEN
        IAC   = 0
          call inversion_forward_model(                              &
                      iu_main_output,                                &
                      ipixstart,                                     &
                      ipixstop,                                      &
                      RIN,                                           &
                      RIN%OSHF,                                      &
                      0,                                             &
                      lresult,                                       &
                      tau_mol,                                       &
                      NHVP_meas,                                     &
                      HVP_meas,                                      &
                      AP,                                            &
                      MDPR,                                          & ! molecular depolarization, added by Qiaoyun HU
                      GOUT%retrieval%fit%segment_fit%pixels,         &
                      segment_vec_fit,                               &
                      GOUT%aerosol,                                  &
                      GOUT%gases,                                    &
                      GOUT%surface,                                  &
                      GOUT%retrieval,                                &
                      nangles,                                       &
                      sca_angles,                                    &
                      KERNELS1,                                      &
                      KERNELS2                                       &
                  )

        ALSP(ipixstart:ipixstop) = ALS(ipixstart:ipixstop)
        AGENP = AGEN    
      ELSE            
        do ipix=ipixstart,ipixstop
! Vector of retrieved parameters after LP-th ineration
          AP(1:RIN%KNSINGF,ipix) = APQ(1:RIN%KNSINGF,ipix)
          ALSP(ipix) = ALSQ(ipix)
        enddo  ! ipix
        AGENP = AGENQ
      ENDIF ! EPSINT .LE. 0.

!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------

      SELECT CASE(INVSING)
      CASE(0)
         DO ipix=ipixstart,ipixstop
            EPSPP=(SQRT(ALS(ipix))-SQRT(ALSP(ipix)))/SQRT(ALS(ipix)) 
! TL 02-12-2014
            if(EPSPP .eq. 1. .and. ALSP(ipix) .eq. 0.0 ) EPSPP = RIN%EPSP
            IF(IPRI_additional_info)  THEN
               if(RIN%IPRI_verbose) WRITE(iu_main_output,*)  &
               EPSPP,SQRT(ALSP(ipix))*100.0, SQRT(ALS(ipix))*100.0,ipix, &
               '  EPSPP,ALSP(ipix),ALS(ipix),ipix'
            ENDIF ! IPRI_additional_info
         ENDDO ! ipix
      CASE(1,2)
          EPSPP=(SQRT(AGEN)-SQRT(AGENP))/SQRT(AGEN)
! TL 02-12-2014
          if(EPSPP .eq. 1. .and. AGENP .eq. 0.0 ) EPSPP = RIN%EPSP 
!         WRITE(iu_main_output,*) EPSPP,'EPSPP'
         IF(IPRI_additional_info)  THEN
            if(RIN%IPRI_verbose)  write(iu_main_output,*) &
            SQRT(AGEN)*100.0,SQRT(AGENP)*100.0,'  AGEN,AGENP ERR%'
         ENDIF
      END SELECT

      DO ipix=ipixstart,ipixstop
        call residual_details ( RIN,                    & ! IN
                                npixels,ipix,           & 
                                IKI,IKI_shift,KNOISEI,  & 					 	 
                                segment_vec_meas,       & 
                                segment_vec_fit,        &
                                resa,resr,resat,resrt   & ! OUT 		
                              )

          call set_pixel_retr_output_residual ( INVSING,LP,ipix,ALSP(ipix),  & ! IN
                                                RIN%NOISE%INOISE,resa,resr,  &
                                                GOUT%retrieval%res           & ! INOUT
                                              )

          if(RIN%IPRI_verbose .and. INVSING .ge. 1)  then
              write(INOISEC,*) RIN%NOISE%INOISE  
              CFMT = '(10x,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),2(6x,a,i0))'
              write(iu_main_output,TRIM(CFMT))     &
              (I,':',resa(I),resr(I)*100.0,' %',I=1,RIN%NOISE%INOISE),'pixel # ', ipix,  &
              'Residual after iteration # ', LP
          endif ! RIN%IPRI_verbose
      ENDDO ! ipix=ipixstart,ipixstop
 
      call set_total_retr_output_residual (  LP,AGENP,                     & ! IN
                                             RIN%NOISE%INOISE,resat,resrt, &
                                             GOUT%retrieval%res            & ! INOUT
                                          )
      if(RIN%IPRI_verbose) then 
        write(INOISEC,*) RIN%NOISE%INOISE
        if(INVSING .eq. 0) then
          CFMT = '(f10.5,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),2(6x,a,i0))'
          write(iu_main_output,TRIM(CFMT)) SQRT(AGENP)*100.0, &
          (I,':',resa(I),resr(I)*100.0,' %',I=1,RIN%NOISE%INOISE),'pixel # ', ipixstop, &
          'Residual after iteration # ', LP
        else
          CFMT = '(/,f10.5,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),6x,a,i0,a/)'
          write(iu_main_output,TRIM(CFMT)) SQRT(AGENP)*100.0, &
          (I,':',resat(I),resrt(I)*100.0,' %',I=1,RIN%NOISE%INOISE), &
          'Residual after iteration # ', LP,'  for TOTAL SEGMENT'
        endif ! INVSING .eq. 0
      endif ! RIN%IPRI_verbose	   
               
!      write(*,'(a,10e14.4)') 'last: ALSQ:    ',ALSQ(1:npixels)
!      write(*,'(a,10i14)')   'last: KM1_pix: ',KM1_pix(1:npixels)
!      write(*,'(a,10e14.4)') 'last: AMEAS:   ',AMEAS(1:npixels)
!      write(*,'(a,10e14.4)') 'ladt: ASMO:    ',ASMO(1:npixels)
!      write(*,'(a,10e14.4)') 'last: AEST:    ',AEST(1:npixels)
!      write(*,'(a,10e14.4)') 'last: ARES2:   ',ARES2(1:npixels)                                         
!      write(*,'(a,10i14)')   'last: KMIMAGE: ',segment_vec_meas(1:npixels)%KMIMAGE                                         
!      write(*,'(3(a,f14.5))')'last: AGEN=',SQRT(AGEN)*100.,'  AGENP=',SQRT(AGENP)*100., &
!                                  '  AFS0=',AFS0
!      do ipix=1,npixels
!      write(*,*) &
!      ipix,ALSP(ipix),AMEAS(ipix),ASMO(ipix),AEST(ipix),ARES2(ipix),segment_vec_meas(ipix)%KMIMAGE,KM1_pix(ipix), &
!      '  - 3:  ipix,ALSP,AMEAS,ASMO,AEST,ARES2,KMIMAGE,KM1_pix'
!      enddo
!      write(*,*) 'AGENP=',AGENP

! Update residuals
      DO ipix=ipixstart,ipixstop
        ALS(ipix) = ALSP(ipix)
      ENDDO ! ipix      
      AGEN = AGENP

 221  CONTINUE

! change flag lresult to compute only single scattering properties
! and fill in retrieval output with them
      IF ( RIN%ISTOP ) THEN
      ! mode: forward
        lresult = .true.
      ELSEIF ( ipixstop .EQ. npixels ) THEN
      ! mode: inversion, all inversion scenarios
        IF ( LP .EQ. RIN%MAXP ) THEN
          lresult = .true.
        ELSEIF ( INVSING .GE. 0 ) THEN
          lresult = .true.
        ELSEIF ( EPSPP .LE. RIN%EPSP ) THEN
          lresult = .true.
        ENDIF
      ELSE
        IF ( INVSING .EQ. 0 ) THEN
          lresult = .true.
        ENDIF
      ENDIF

      IF ( lresult ) THEN
 !       WRITE(iu_main_output,*)
 !       DO I=1,RIN%KNSING
 !       WRITE(iu_main_output,'(i5,1000e14.5)')  &
 !        I,EXP(AP(I,ipixstart:ipixstop))
 !       ENDDO ! I

! **  Calculate optical characteristics (ext,ssa,lr) at RIN%WAVE wavelengths for retrieved parameters
         call inversion_forward_model(                              &
                     iu_main_output,                                &
                     ipixstart,                                     &
                     ipixstop,                                      &
                     RIN,                                           &
                     RIN%OSHF,                                      &
                     0,                                             &
                     lresult,                                       &
                     tau_mol,                                       &
                     NHVP_meas,                                     &
                     HVP_meas,                                      &
                     AP,                                            &
                     MDPR,                                          & ! molecular depolarization, added by Qiaoyun HU
                     GOUT%retrieval%fit%segment_fit%pixels,         &
                     segment_vec_fit,                               &
                     GOUT%aerosol,                                  &
                     GOUT%gases,                                    &
                     GOUT%surface,                                  &
                     GOUT%retrieval,                                &
                     nangles,                                       &
                     sca_angles,                                    &
                     KERNELS1,                                      &
                     KERNELS2                                       &
                  )
         if ( error_present() ) return
         lresult = .false.
         do ipix=ipixstart,ipixstop
            call set_gout_pixel(  iu_main_output, RIN, segment_meas, & ! IN
                                  ipix, nangles, sca_angles, AP,     &
                                  GOUT,                      & ! INOUT
                                  KERNELS1,KERNELS2          &
                               )
            if ( error_present() ) return
         enddo ! ipix
        ! **  Write retrieval results after each iteration
        !call print_main_output ( iu_main_output, RIN, segment_meas, GOUT )
        call iteration_callback()
      ENDIF ! lresult

!C*****************************************************
!C*** CHECKING if more iterations are necessary***
!C*** INVSING=0 - pixel-by-pixel inversion;
!C*** INVSING>0 - multi-pixel inversion
!C*** The condition below is set that 
!C    multi-pixel inversion is followed by
!C    pixel-by-pixel inversion using solution of 
!C    multi-pixel inversion and then it stops
!C*****************************************************

      IF(.NOT. RIN%ISTOP) THEN
        IF(EPSPP .GT. RIN%EPSP .AND. LP .LT. RIN%MAXP) THEN
          if(INVSING .gt. 0) then ! multipixel scenario  
            if(lcmatrix) then
! recalculate covariance matrix CS
!do ipix=1,npixels
!write(*,*) 'ipix=',ipix
!write(*,*) 'CM1: MNOISEI  :'
!write(*,'(10i8)') MNOISEI(1:3,1:NW,ipix) 
!write(*,*) 'CM1: IKI      :'
!write(*,'(10i8)') IKI(1:2,ipix)
!write(*,*) 'CM1: IKI_shift:'
!write(*,'(10l8)') IKI_shift(1:2,ipix)
!write(*,*) 'CM1: KNOISEI  :'
!write(*,'(10i8)') KNOISEI(1:3,1:NW,ipix)
!enddo 
              call covariance_matrix_segment (RIN,INVSING,           & ! IN
                                              segment_meas,          & 
                                              segment_vec_meas,      &
                                              MNOISEI(:,:,:),        & 
                                              IKI(:,:),              &
                                              IKI_shift(:,:),        &
                                              KNOISEI(:,:,:),        & ! OUT
                                              CS(:,:),ARES2(:),      &
                                              GOUT%retrieval%res     &
                                             )
!write(*,*) '**********************************'
!do ipix=1,npixels
!write(*,*) 'ipix=',ipix
!write(*,*) 'CM2: MNOISEI  :'
!write(*,'(10i8)') MNOISEI(1:3,1:NW,ipix) 
!write(*,*) 'CM2: IKI      :'
!write(*,'(10i8)') IKI(1:2,ipix)
!write(*,*) 'CM2: IKI_shift:'
!write(*,'(10l8)') IKI_shift(1:2,ipix)
!write(*,*) 'CM2: KNOISEI  :'
!write(*,'(10i8)') KNOISEI(1:3,1:NW,ipix)
!enddo

! residual (F-FP)^T C (F-FP)                  
              do ipix=1,npixels
                KMIMAGE=segment_vec_fit(ipix)%KMIMAGE
                call residual_meas_term ( IPRI_additional_info,iu_main_output,  & ! IN
                                          KMIMAGE,                  &
                                          segment_vec_meas(ipix)%FS(1:KMIMAGE),  &
                                          segment_vec_fit(ipix)%FS(1:KMIMAGE),   &
                                          CS(1:KMIMAGE,ipix),       & 
                                          AMEAS(ipix)               & ! OUT
                                        )
! residual AP^T SMSING AP
                if (apriori_smooth) &
                call residual_apriori_smooth_term ( IPRI_additional_info,iu_main_output, & ! IN 
                                                    RIN%KNSINGF,             &
                                                    AP(1:RIN%KNSINGF,ipix),  &
                                                    SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                                    SMSEST,                  &
                                                    ASMO(ipix)               & ! OUT
                                                  ) 

! residual (AP-AP0)^T SMSING0 (AP-AP0); A PRIORI ESTIMATE TERM of the residual
                if (apriori_estim) &
                call residual_apriori_estim_term (  IPRI_additional_info,iu_main_output,  & ! IN
                                                    RIN%KNSINGF,              &
                                                    AP(1:RIN%KNSINGF,ipix),   &
                                                    AP0(1:RIN%KNSINGF,ipix),  &
                                                    SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                                    AEST(ipix)                & ! OUT
                                                 ) 

                ccor = AMEAS(ipix)/(ARES2(ipix)*KMIMAGE)
                if(ccor .lt. ccor_min) ccor = ccor_min
                if(ccor .gt. ccor_max) ccor = ccor_max
                ALSP(ipix) = AMEAS(ipix)
                if(apriori_smooth)  ALSP(ipix) = ALSP(ipix) + ccor*ASMO(ipix)
                if(apriori_estim)   ALSP(ipix) = ALSP(ipix) + ccor*AEST(ipix) 
                ALSP(ipix)  = ALSP(ipix)/KM1_pix(ipix)
                enddo ! ipix
!      write(*,'(a,10e14.4)') '4: ALSP:    ',ALSP(1:npixels)
!      write(*,'(a,10i14)')   '4: KM1_pix: ',KM1_pix(1:npixels)
!      write(*,'(a,10e14.4)') '4: AMEAS:   ',AMEAS(1:npixels)                                         
!      write(*,'(a,10e14.4)') '4: ASMO:    ',ASMO(1:npixels)                                         
!      write(*,'(a,10e14.4)') '4: AEST :   ',AEST(1:npixels)                                         
!      write(*,'(a,10e14.4)') '4: ARES2:   ',ARES2(1:npixels)                                         
!      write(*,'(a,10i14)')   '4: KMIMAGE: ',segment_vec_meas(1:npixels)%KMIMAGE                                         
              ccor = SUM(AMEAS(1:npixels))/  &
                     SUM(ARES2(1:npixels)*segment_vec_meas(1:npixels)%KMIMAGE)  
              if(ccor .lt. ccor_min) ccor = ccor_min
              if(ccor .gt. ccor_max) ccor = ccor_max
              AGENP = SUM(AMEAS(ipixstart:ipixstop))
              if(apriori_smooth)  AGENP = AGENP+ccor*SUM(ASMO(1:npixels))
              if(apriori_estim)   AGENP = AGENP+ccor*SUM(AEST(1:npixels)) 
! inter-pixel smoothnes constraint term of residual, See  Dubovik et al. [2011]
              call residual_inter_pixel_term(IPRI_additional_info,iu_main_output,RIN%KNSINGF,npixels,AP,SMMULTI,  & 
                                                                    ledges,FF_edge,AB_edge,AFS0)
              AGENP = (AGENP+ccor*AFS0)/KM1_segm
              call set_total_retr_output_residual ( LP,AGENP,                      & ! IN
                                                    RIN%NOISE%INOISE,resat,resrt,  &
                                                    GOUT%retrieval%res             & ! INOUT
                                                  )      
!      do ipix=1,npixels
!      write(*,*) &
!      ipix,ALSP(ipix),AMEAS(ipix),ASMO(ipix),AEST(ipix),ARES2(ipix),segment_vec_meas(ipix)%KMIMAGE,KM1_pix(ipix), &
!      '  - 4:  ipix,ALSP,AMEAS,ASMO,AEST,ARES2,KMIMAGE,KM1_pix'
!      enddo
!      write(*,*) 'AGENP=',AGENP

! Redefine residiuals for next iteration
              ALS(1:npixels) = ALSP(1:npixels)
              AGEN = AGENP 
            endif ! lcmatrix
          endif ! INVSING .gt. 0                
          GOTO 12
        ELSE
          IF(INVSING .NE. 2) THEN
            IF(INVSING .EQ. 0 .AND. ipixstop .LT. npixels) THEN
              ipixstart = ipixstart+1
              ipixstop  = ipixstop +1
              GOTO 177         
            ELSEIF(INVSING .EQ. 1) THEN
              INVSING   = 0
              ipixstart = 1
              ipixstop  = 1
              if(any(RIN%NOISE%SGMS(1:RIN%NOISE%INOISE) .gt. 0.0)) then
                write(tmp_message,'(a)') 'Can not add twice random noise to meas vector.'
                G_ERROR(trim(tmp_message))
              endif
              ! Assign noise indices to measurement types
              call assign_noise_index(RIN,segment_meas,MNOISEI)
              call covariance_matrix_segment (RIN,INVSING,      & ! IN
                                              segment_meas,     &
                                              segment_vec_meas, &
                                              MNOISEI,          &
                                              IKI(:,:),         & ! OUT
                                              IKI_shift(:,:),   &
                                              KNOISEI(:,:,:),   &
                                              CS(:,:),ARES2     &
                                             )
              GOTO 177         
            ENDIF ! INVSING .EQ. 0 .AND.
          ENDIF ! INVSING .NE. 2 
        ENDIF ! EPSPP .GT. RIN%EPSP 
      ENDIF ! .NOT. RIN%ISTOP

!...................................................................................
! CALCULATIONS AFTER FINAL INVERSION    ! CALCULATIONS AFTER INVERSION FINAL
!...................................................................................

      GOUT%aerosol%phmx%nangle   = nangles
      GOUT%aerosol%phmx%angle(:) = sca_angles(:)
! ......................................................................
      if (RIN%products%surface%bhr_iso) then
        call bhr_iso_segment( RIN, segment_meas, GOUT )
        if ( error_present() ) return
        GOUT%products%surface%bhr_iso = .true.
      endif

!XH   testing broadband flux calculation
      if (RIN%products%forcing%bbflux .or. RIN%products%forcing%forcing) then
        GOUT%retrieval%information%NHLV = 10
! units of levels in kilometers
        GOUT%retrieval%information%HLV(1:GOUT%retrieval%information%NHLV)=  &
                              (/120.00,15.00,10.00,6.00,5.00,4.00,3.00,2.00,1.00,0.06/)

         do ipix=1,npixels
!XH         define the list of levels
!            GOUT%forcing%bbflux%pixel(ipix)%NHLV =KNT
!            do i=1, KNT
!               GOUT%forcing%bbflux%pixel(ipix)%HLV(i) =(KNT-i)*100.0/(KNT-1)
!            end do
!XH         level list consitent with Yevgeny's
            GOUT%forcing%bbflux%pixel(ipix)%NHLV = GOUT%retrieval%information%NHLV
            GOUT%forcing%bbflux%pixel(ipix)%HLV(1:GOUT%retrieval%information%NHLV)=  &
                              GOUT%retrieval%information%HLV(1:GOUT%retrieval%information%NHLV)
!XH         longitude and latitude of the pixel
            h1   = segment_meas%pixels(ipix)%MASL
            lat1 = segment_meas%pixels(ipix)%y
            call bbflux_pixel (                                                &
                                 iu_main_output,                               & ! IN
                                 RIN,ipix,h1,lat1,                             &
                                 NHVP_meas,HVP_meas(:,ipix),                   &
                                 RIN%NW,RIN%WAVE,AP(:,ipix),                   &
                                 MDPR,                                         & ! molecular depolarization, added by Qiaoyun HU
                                 GOUT%retrieval%fit%segment_fit%pixels(ipix),  & ! INOUT
                                 GOUT%aerosol,                                 &
                                 GOUT%forcing%bbflux%pixel(ipix),              &
                                 GOUT%forcing%forcing%pixel(ipix),             &
                                 KERNELS1,KERNELS2                             &
                              )
!!XH         temporary output of forcing product
!            print *, 'Pixel at latitude: ',lat1,' longitude: ',h1,' pixel # ',ipix
!            print *, '     HLV(km)   Flux0Up(w/m^2) Flux0Down(w/m^2)   Flux Up(w/m^2) Flux Down(w/m^2) NetForcing(w/m^2)'
!            do I = GOUT%forcing%bbflux%pixel(ipix)%NHLV, 1, -1
!               print *, GOUT%forcing%bbflux%pixel(ipix)%HLV(I), GOUT%forcing%bbflux%pixel(ipix)%BBUFX0(I),  &
!                        GOUT%forcing%bbflux%pixel(ipix)%BBDFX0(I), GOUT%forcing%bbflux%pixel(ipix)%BBUFXA(I),  &
!                        GOUT%forcing%bbflux%pixel(ipix)%BBDFXA(I), GOUT%forcing%forcing%pixel(ipix)%NETFORC(I)
!            end do
         end do ! ipix=1,npixels
         GOUT%products%forcing%bbflux  = .true.
         GOUT%products%forcing%forcing = .true.
! Convert units of levels from kilometers to meters
         GOUT%retrieval%information%HLV(1:GOUT%retrieval%information%NHLV) =  &
         GOUT%retrieval%information%HLV(1:GOUT%retrieval%information%NHLV) * 1e+3
      end if
!XH   end of broadband flux calculation

      if(RIN%IPRI_verbose) then
      if(RIN%IMQ .eq. 3) then
        if(sparse_solver_name .eq. 'SUPERLU') then
          write(iu_main_output,'(3a,f16.5,/)')  'Solver ',trim(sparse_solver_name), &
          ' system solving CPU_time(sec) =',solver_timer
        else
          write(iu_main_output,'(3a,f16.5,/)')  'Solver ',trim(sparse_solver_name), &
          '  CPU_time(sec) =',solver_timer
        endif
      endif
      endif

!AL ************ Calculation of aerosol particulate matter ********************
      if (RIN%products%aerosol%PM) then
!        write(*,*) 'before PM!'
        do ipm=1,2
          if(RIN%PM_diam(ipm) .gt. 0.0) then
            call get_PM(RIN%PM_diam(ipm),RIN,GOUT,segment_meas,GOUT%aerosol%pm%pixel(:)%PM(ipm))
          endif ! PM_diam gt 0.0
        enddo !ipm
        GOUT%products%aerosol%pm = .true.
!        stop
!        write(*,*) 'PM asserted!'
      endif
!AL ************ Aerosol classification ********************
      if (RIN%products%aerosol%types) then
      if (GOUT%products%aerosol%sd2m_mph .or. GOUT%products%aerosol%sd2m_mph) then
          call get_aerosol_types(RIN,GOUT,segment_meas%npixels,GOUT%aerosol%types%pixel(:)%index)
          GOUT%products%aerosol%types = .true.
!      write(*,*) 'types asserted!'
      endif
      endif

!***********************************************************
!***  Calculation of Error and Bias Estimates     **********
!***********************************************************
      if ( .not. RIN%ISTOP .and. LP .gt. 0 ) then
      if ( ERREST%product ) then
      if( (INVSING .gt. 0 .and. ERREST%segm) .or. (INVSING .eq. 0 .and. ERREST%pix) ) then
!write(*,'(a,l2)') 'LM_matrix =',ERREST%LM_matrix
!do i=1,RIN%KNSINGF
!write(*,'(3x,2i4,2es12.4,a)') lp,i,UFS_errest(i,i,1),UFS(i,i,1),'  - 1: lp,i,UFS_errest,UFS'
!enddo
!write(*,*)
        !if ( ERREST%LM_matrix ) then
        !    UFS_errest(1:RIN%KNSINGF,1:RIN%KNSINGF,1:npixels) = &
        !    UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,1:npixels)
        !endif
        if ( RIN%IMQ .eq. 1 ) then
            if ( ERREST%LM_matrix .or. (.not. ERREST%LM_matrix .and. LP .gt. RIN%IPSTOP) ) then
            CALL UF_nonzero ( RIN%KNSINGF,npixels,UFS, & ! IN
                              1.0,SMMULTI,             &
                              UFNZ,nnz                 & ! OUT
                            )
            else
            CALL UF_nonzero ( RIN%KNSINGF,npixels,UFS_errest, & ! IN
                              1.0,SMMULTI,                    &
                              UFNZ_errest,nnz_err             & ! OUT
                            )
            endif
            if ( error_present() ) return
            FFMI0(:) = 0.0
            DO ipix=1,npixels
              DO I=1,RIN%KNSINGF
                FFMI0((ipix-1)*RIN%KNSINGF+I) = FFMSQ(I,ipix)
              ENDDO ! I
            ENDDO ! ipix
            b(1:KNF) = FFMI0(1:KNF)
            if ( ERREST%LM_matrix .or. (.not. ERREST%LM_matrix .and. LP .gt. RIN%IPSTOP) ) then
              if ( sparse_solver_name .eq. 'SUPERLU' ) then
              call solver_init_SuperLU(KNF, nnz, UFNZ, b)
              endif
              call sparse_solver (  iu_main_output,  & ! IN
                                    KNF, nnz, UFNZ, &
                                    b,              & ! INOUT
                                    solver_timer    &
                                )
              if ( error_present() ) then
              if(sparse_solver_name .eq. 'SUPERLU') then
              call solver_cleanup_SuperLU(KNF, nnz, UFNZ, b)
              endif
              return
              endif
            else
              if ( sparse_solver_name .eq. 'SUPERLU' ) then
              call solver_init_SuperLU(KNF, nnz_err, UFNZ_errest, b)
              endif
              call sparse_solver (  iu_main_output,          & ! IN
                                    KNF,nnz_err,UFNZ_errest, &
                                    b,                       & ! INOUT
                                    solver_timer             &
                                 )
              if ( error_present() ) then
              if(sparse_solver_name .eq. 'SUPERLU') then
              call solver_cleanup_SuperLU(KNF, nnz_err, UFNZ_errest, b)
              endif
              return
              endif
            endif
            QIM(1:KNF) = b(1:KNF)
            if(sparse_solver_name .eq. 'SUPERLU') then
            if ( ERREST%LM_matrix .or. (.not. ERREST%LM_matrix .and. LP .gt. RIN%IPSTOP) ) then
            call solver_cleanup_SuperLU(KNF, nnz, UFNZ, b)
            else
            call solver_cleanup_SuperLU(KNF, nnz_err, UFNZ_errest, b)
            endif
            endif
        endif ! RIN%IMQ .eq. 1
!stop

!j=1
!write(*,*) j,'  j  UFS:'
!do i=1,RIN%KNSING
!write(*,*) 'i=',i
!write(*,'(10f12.4)') UFS_errest(1:RIN%KNSING,i,j)
!enddo 
!write(*,*)


!!----------------------------------------------------------
!! MEH: Bias from the misfit and Levenberg-Marquardt:

        if ( ERREST%LM_vector .and. LP .le. RIN%IPSTOP ) then
        select case(INVSING)
        case(0,1) ! pixel by pixel scenario
            if(RIN%IMQ .eq. 1 .or. RIN%IMQ .eq. 2) then
              do ipix=ipixstart,ipixstop
                FFSE_zer(1:RIN%KNSINGF,ipix) = FFMS(1:RIN%KNSINGF,ipix) + &
                ERREST%LM_term(1:RIN%KNSINGF,ipix) * &
                (AP(1:RIN%KNSINGF,ipix)-ERREST%AP_iguess(1:RIN%KNSINGF,ipix))
                QSE_zer(:,ipix)  = 0.0
                call ITERQ (  RIN%IMQ,RIN%KNSINGF,                    & ! IN
                              UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix),  &
                              FFSE_zer(1:RIN%KNSINGF,ipix),               &
                              QSE_zer(1:RIN%KNSINGF,ipix)                  & ! OUT
                           )
              enddo ! ipix
            elseif(RIN%IMQ .eq. 3) then
              do ipix=1,ipixstop
                FFSE_zer(1:RIN%KNSINGF,ipix) = FFMS(1:RIN%KNSINGF,ipix) + &
                ERREST%LM_term(1:RIN%KNSINGF,ipix) * &
                (AP(1:RIN%KNSINGF,ipix)-ERREST%AP_iguess(1:RIN%KNSINGF,ipix))
                !QSE_zer without bias: it is from misfit and LM
                QSE_zer(:,ipix)  = 0.0
                call UF_nonzero_one_pixel ( RIN%KNSINGF,UFS(:,:,ipix), & ! IN
                                            UFNZ,nnz                   & ! OUT
                                          )
                if ( error_present() ) return
                bE_zer(1:RIN%KNSINGF) = FFSE_zer(1:RIN%KNSINGF,ipix)
                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_init_SUPERLU(RIN%KNSINGF, nnz, UFNZ, bE_zer)
                call sparse_solver ( iu_main_output,       & ! IN
                                 RIN%KNSINGF,nnz,UFNZ, &
                                 bE_zer,                    & ! INOUT
                                 solver_timer          &
                               )
                if ( error_present() ) then
                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_cleanup_SUPERLU(RIN%KNSINGF, nnz, UFNZ, bE_zer)
                return
                end if
                QSE_zer(1:RIN%KNSINGF,ipix) = bE_zer(1:RIN%KNSINGF)
                call sparse_solver_cleanup(RIN%KNSINGF, nnz, UFNZ, bE_zer)
    !            WRITE(*,*) 'QSE_zer(1:RIN%KNSINGF,ipix)', QSE_zer(1:RIN%KNSINGF,ipix)
              enddo ! ipix
           endif !RIN%IMQ
            !WRITE(*,*) 'QSE_zer with LM', QSE_zer(1:RIN%KNSINGF,1:npixels)
        case(2) ! multi pixel scenario
        !WRITE(*,*) 'some changes are needed'
        do ipix=1,npixels
            do i=1,RIN%KNSINGF
            j = RIN%KNSINGF*(ipix-1)+i
            QIM(j) = QIM(j) + ERREST%LM_term(i,ipix) * &
            (AP(i,ipix)-ERREST%AP_iguess(i,ipix))
            enddo
        enddo
        end select
        else
        select case(INVSING)
        case(0,1) !single-pixel
        if(RIN%IMQ .eq. 1 .or. RIN%IMQ .eq. 2) then
            do ipix=ipixstart,ipixstop
              FFSE_zer(1:RIN%KNSINGF,ipix) = FFMS(1:RIN%KNSINGF,ipix)
              QSE_zer(:,ipix)  = 0.0
              call ITERQ (  RIN%IMQ,RIN%KNSINGF,                    & ! IN
                            UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix),  &
                            FFSE_zer(1:RIN%KNSINGF,ipix),               &
                            QSE_zer(1:RIN%KNSINGF,ipix)                  & ! OUT
                         )
            enddo ! ipix
        elseif(RIN%IMQ .eq. 3) then
            do ipix=1,ipixstop
                FFSE_zer(1:RIN%KNSINGF,ipix) = FFMS(1:RIN%KNSINGF,ipix)
                QSE_zer(:,ipix)  = 0.0
                call UF_nonzero_one_pixel ( RIN%KNSINGF,UFS(:,:,ipix), & ! IN
                                            UFNZ,nnz                   & ! OUT
                                          )
                if ( error_present() ) return
                bE_zer(1:RIN%KNSINGF) = FFSE_zer(1:RIN%KNSINGF,ipix)
                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_init_SUPERLU(RIN%KNSINGF, nnz, UFNZ, bE_zer)
                call sparse_solver ( iu_main_output,       & ! IN
                                 RIN%KNSINGF,nnz,UFNZ, &
                                 bE_zer,                    & ! INOUT
                                 solver_timer          &
                               )
                if ( error_present() ) then
                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_cleanup_SUPERLU(RIN%KNSINGF, nnz, UFNZ, bE_zer)
                return
                end if
                QSE_zer(1:RIN%KNSINGF,ipix) = bE_zer(1:RIN%KNSINGF)
                call sparse_solver_cleanup(RIN%KNSINGF, nnz, UFNZ, bE_zer)
           enddo !ipix
        endif !RIN%IMQ
       case(2) !multi-pixel
       !! MEH
       WRITE(*,*) 'some changes are needed'
        do ipix=1,npixels
            do i=1,RIN%KNSINGF
            j = RIN%KNSINGF*(ipix-1)+i
            QIM(j) = QIM(j)
            enddo
        enddo
        end select
        endif
!!----------------------------------------------------------
!! MEH: Bias in equation
!! if RIN%BIAS_EQ(:) take a value in the settings, call the subrutine to add bias in the systematic component
!! We add both, positive and negative bias. Then we provide the average between them as total bias.

    if ( use_bias_eq ) then
        ! MEH: QS for the measurement with added positive bias
        QSE_pos(:,ipixstop)  = 0.0
        QIM_pos(:) = 0.0
        option_bias = bias_pos
        call BIAS_estimates (RIN, iu_main_output,  &
                                   segment_meas,    &
                                   segment_vec_meas, &
                                   pixel_vec_fit_deriv, &
                                   AP, &
                                   APMIN, APMAX, &
                                   segment_vec_fit, &
                                   nangles, sca_angles, &
                                   tau_mol, NHVP_meas, HVP_meas, &
                                   dermask, dermask_aerpar, &
                                   lresult, lp, &
                                   KERNELS1, KERNELS2, &
                                   GOUT, INVSING, US,&
                                   sparse_solver_name, &
                                   QSE_pos, QIM_pos,&    !out
                                   option_bias, KM1_pix, ALS_pos)

        !write(*,*) 'QIM_pos', QIM_pos(1:KNF)


        ! MEH: QS for the measurement with added negative bias
        QSE_neg(:,ipixstop)  = 0.0
        QIM_neg(:)  = 0.0
        option_bias = bias_neg
        call BIAS_estimates (RIN, iu_main_output,  &
                                   segment_meas,    &
                                   segment_vec_meas, &
                                   pixel_vec_fit_deriv, &
                                   AP, &
                                   APMIN, APMAX, &
                                   segment_vec_fit, &
                                   nangles, sca_angles, &
                                   tau_mol, NHVP_meas, HVP_meas, &
                                   dermask, dermask_aerpar, &
                                   lresult, lp, &
                                   KERNELS1, KERNELS2, &
                                   GOUT, INVSING, US,&
                                   sparse_solver_name, &
                                   QSE_neg, QIM_neg,&    !out
                                   option_bias, KM1_pix, ALS_neg)


        ! MEH: Temporarly select case. Bias in multi-pixel is in progress.
        select case(INVSING)
        case(0,1) ! pixel by pixel scenario
        ! Total bias:
        QSE_tot(1:RIN%KNSINGF,1:npixels) = SQRT(QSE_zer(1:RIN%KNSINGF,1:npixels)*QSE_zer(1:RIN%KNSINGF,1:npixels) + &
            (0.5*QSE_pos(1:RIN%KNSINGF,1:npixels)*QSE_pos(1:RIN%KNSINGF,1:npixels)) + &
            (0.5*QSE_neg(1:RIN%KNSINGF,1:npixels)*QSE_neg(1:RIN%KNSINGF,1:npixels)))

        case(2) ! multi pixel
        !QSE_tot(1:RIN%KNSINGF,1:npixels) = QS(1:RIN%KNSINGF,1:npixels)
        QIM_tot(1:KNF) = SQRT(QIM(1:KNF)*QIM(1:KNF) + &
                        (0.5*QIM_pos(1:KNF)*QIM_pos(1:KNF)) + &
                        (0.5*QIM_neg(1:KNF)*QIM_neg(1:KNF)))
        end select
        else
        select case(INVSING)
        case(0,1) ! pixel by pixel scenario
            QSE_tot(1:RIN%KNSINGF,1:npixels) = QSE_zer(1:RIN%KNSINGF,1:npixels)
        case(2)
            QIM_tot(1:KNF) = QIM(1:KNF)
!            QSE_zer(1:RIN%KNSINGF,1:npixels) = QIM(1:RIN%KNSINGF,1:npixels)
        end select
    endif

!-----------------------
!! MEH: Total error estimates:
         call output_err_estim_initialization ( GOUT%errest )
!!***
!! MEH: 
        if (RIN%products%errest%aerosol%mic) then
         call output_err_estim_initialization_mic(GOUT%errest)
        endif
!!***
         call get_MASL(segment_meas, MASL)
         solver_timer = 0.0
         option_bias = bias_basic
         ! MEH: total error estimates: systematic component from misfit
         if ( ERREST%LM_matrix  .or. (.not. ERREST%LM_matrix .and. LP .gt. RIN%IPSTOP) ) then
            call ERR_estimates ( iu_main_output, sparse_solver_name, &
                                UF, UFS, nnz, UFNZ,             &
                                QSE_tot, QIM_tot, AGENP, ALS,            &
                                AP, APMIN, APMAX,               &
                                RIN, INVSING, npixels, MASL,    &
                                ERREST%pix_mask, dermask_aerpar,&
                                tau_mol, NHVP_meas, HVP_meas,   &
                                GOUT, solver_timer,             &
                                KERNELS1, KERNELS2,             &
                                use_bias_eq, option_bias, QSE_zer, QIM, ALS)
         else
            call ERR_estimates ( iu_main_output, sparse_solver_name,   &
                                UF, UFS_errest, nnz_err, UFNZ_errest, &
                                QSE_zer, QIM, AGENP, ALS,            &
                                AP, APMIN, APMAX,               &
                                RIN, INVSING, npixels, MASL,    &
                                ERREST%pix_mask, dermask_aerpar,&
                                tau_mol, NHVP_meas, HVP_meas,   &
                                GOUT, solver_timer,             &
                                KERNELS1, KERNELS2,             &
                                use_bias_eq, option_bias, QSE_zer, QIM, ALS)
        endif
        if (use_bias_eq) then
            option_bias = bias_pos
            ! MEH: total error estimates: systematic component from consider positive bias
            if ( ERREST%LM_matrix  .or. (.not. ERREST%LM_matrix .and. LP .gt. RIN%IPSTOP) ) then
               call ERR_estimates ( iu_main_output, sparse_solver_name, &
                                   UF, UFS, nnz, UFNZ,             &
                                   QSE_pos, QIM_pos, AGENP, ALS,            &
                                   AP, APMIN, APMAX,               &
                                   RIN, INVSING, npixels, MASL,    &
                                   ERREST%pix_mask, dermask_aerpar,&
                                   tau_mol, NHVP_meas, HVP_meas,   &
                                   GOUT, solver_timer,             &
                                   KERNELS1, KERNELS2,             &
                                   use_bias_eq, option_bias, QSE_pos, QIM_pos, ALS_pos)
            else
               call ERR_estimates ( iu_main_output, sparse_solver_name,   &
                                   UF, UFS_errest, nnz_err, UFNZ_errest, &
                                   QSE_pos, QIM_pos, AGENP, ALS,            &
                                   AP, APMIN, APMAX,               &
                                   RIN, INVSING, npixels, MASL,    &
                                   ERREST%pix_mask, dermask_aerpar,&
                                   tau_mol, NHVP_meas, HVP_meas,   &
                                   GOUT, solver_timer,             &
                                   KERNELS1, KERNELS2,             &
                                   use_bias_eq, option_bias, QSE_pos, QIM_pos, ALS_pos)
            endif
            option_bias = bias_neg
            ! MEH: total error estimates: systematic component from consider negative bias
            if ( ERREST%LM_matrix  .or. (.not. ERREST%LM_matrix .and. LP .gt. RIN%IPSTOP) ) then
               call ERR_estimates ( iu_main_output, sparse_solver_name, &
                                   UF, UFS, nnz, UFNZ,             &
                                   QSE_neg, QIM_neg, AGENP, ALS,            &
                                   AP, APMIN, APMAX,               &
                                   RIN, INVSING, npixels, MASL,    &
                                   ERREST%pix_mask, dermask_aerpar,&
                                   tau_mol, NHVP_meas, HVP_meas,   &
                                   GOUT, solver_timer,             &
                                   KERNELS1, KERNELS2,             &
                                   use_bias_eq, option_bias, QSE_neg, QIM_neg, ALS_neg)
            else
               call ERR_estimates ( iu_main_output, sparse_solver_name,   &
                                   UF, UFS_errest, nnz_err, UFNZ_errest, &
                                   QSE_neg, QIM_neg, AGENP, ALS,            &
                                   AP, APMIN, APMAX,               &
                                   RIN, INVSING, npixels, MASL,    &
                                   ERREST%pix_mask, dermask_aerpar,&
                                   tau_mol, NHVP_meas, HVP_meas,   &
                                   GOUT, solver_timer,             &
                                   KERNELS1, KERNELS2,             &
                                   use_bias_eq, option_bias, QIM_neg, QSE_neg, ALS_neg)
            endif
        endif

        if ( error_present() ) return
         if(RIN%IPRI_verbose .eqv. .true.) then
         if(ERREST%LM_matrix) then
            write(iu_main_output,'(/,a,/)')  &
            'Error estimates have been calculated using Fisher matrix with Levenberg–Marquardt type constraints.'
         else
            write(iu_main_output,'(/,a,/)')  &
            'Error estimates have been calculated using Fisher matrix w/out Levenberg–Marquardt type constraints.'
         endif
         endif
! MH - to print the error estimate for all the parameters
!        write(*,*) '############'
!        write(*,*) GOUT%errest%par%pixel(1)%ERRP(1:RIN%KNSINGF)
!        write(*,*) '############'        
        !write(*,*) 'after  ERR_estimates'
        if(RIN%IPRI_verbose .eqv. .true.) then
        if(RIN%IMQ .eq. 3) then
          if(sparse_solver_name .eq. 'SUPERLU') then
            write(iu_main_output,'(3a,f16.5,/)')  &
            'For ERR_estimates: Solver ',trim(sparse_solver_name), &
            ' system solving CPU_time(sec) =',solver_timer
          else
            write(iu_main_output,'(3a,f16.5,/)')  &
            'For ERR_estimates: Solver ',trim(sparse_solver_name), &
            '  CPU_time(sec) =',solver_timer
          endif
        endif
        endif
        ! set error estimates in gout for triangle bins SD model
        call set_gout_sd_err_estim_segment ( iu_main_output, RIN, GOUT )

      else
         if(RIN%IPRI_verbose) then
            write(*,*) 'Warning in inversion !!!' 
            write(*,*) 'Do not calculate Error and Bias Estimates because of '
            write(*,*) 'number of measurements is less than number of retrieved parameters.'
            write(*,'(x,a,i3,a)') 'INVSING=',INVSING,'  errest_segm = .F. or errest_pix_mask(1:npixels) = .F.'
         endif !
      endif ! INVSING .eq. 2 .and. errest_segm) .or.
      endif ! ERREST%product
      endif ! .not. RIN%ISTOP

! ......................................................................

      if( inversion_run_count .eq. -1 ) then
! can be deleted after testing edges
        if( .not. RIN%INPUT ) then
        write(*,*) 'input file : ',trim(internal_file_path)//"input_iguess_1.dat"
        open (newunit=iu_tmp, FILE=trim(internal_file_path)//"input_iguess_1.dat",status='unknown')
          do I=1,RIN%KNSING
            write(iu_tmp,*) I,( GOUT%retrieval%par%pixel(1:npixels)%par(I) )
          enddo
        close (iu_tmp)
        endif
      elseif(inversion_run_count .eq. -2) then
! can be deleted after testing edges
        if( .not. RIN%INPUT ) then
        open (newunit=iu_tmp, FILE=trim(internal_file_path)//"input_iguess_2.dat",status='unknown')
          do I=1,RIN%KNSING
            write(iu_tmp,*) I,( GOUT%retrieval%par%pixel(1:npixels)%par(I) )
          enddo
        close (iu_tmp)
        endif
      endif

!***************************************************************************
!*** Print simulated data into file         ********************************
! meas_rnoise = 0 - disabled, 1 - measurement_fitting, 2 - sdata
      if ( add_rnoise ) then
      if ( RIN%NOISE%meas_rnoise .eq. 2 ) then ! sdata
      if ( RIN%ISTOP ) then
        call add_meas_rnoise_segment ( RIN, deep_random_switch, &
                                      MNOISEI, GOUT%retrieval%fit%segment_fit &
                                     )
      endif
      endif
      endif

      !if(RIN%ISTOP .and. sdata_sim_file .ne. '') then
      ! option was removed from settings
         !status_funct = &
				 !write_sdata_pixels ( sdata_sim_file, npixels, GOUT%retrieval%fit%segment_fit )
         !if ( error_present() ) return
      !endif ! RIN%ISTOP .and. RIN%sdata_sim_file

!*** FITTING (FS and FPS (measurements and modeled measurements calculated for retrieved parameters)) ***
      if(RIN%IPRI_verbose) then
        !if(.not. RIN%products%retrieval%fit)  then
! If applicable, add random noise to segment measurement vector
          if ( RIN%NOISE%meas_rnoise .eq. 1 ) then ! measurement_fitting
          if(any(RIN%NOISE%SGMS(1:RIN%NOISE%INOISE) .gt. 0.0)) then
            call print_fitting_FS ( iu_main_output, RIN, segment_meas, &
                                    segment_vec_meas, segment_vec_fit )
          endif ! any(RIN%NOISE%SGMS
          endif
        !endif ! RIN%products%retrieval%fit
      endif ! RIN%IPRI_verbose
!***************************************************************************
!     Deallocate arrays UF
      if(allocated(UF)) then
         deallocate(UF,stat=alloc_stat)
         if (alloc_stat /= 0) stop 'error while trying to deallocate UF'
      endif
!	-------------------------------------------------------------------------
      if(main_output_file .ne. '-') close(iu_main_output) 

!	-------------------------------------------------------------------------
!     Deallocate Compressed Column Storage (sparse matrix)
      call deallocate_sparse_matrix_storage ( UFNZ ) 
!     Deallocate UFS_errest matrix and Compressed Column Storage (sparse matrix) for error estimates
      if ( ERREST%product ) then
      if ( ERREST%LM_matrix ) then
        call deallocate_sparse_matrix_storage ( UFNZ_errest )
        if(allocated(UFS_errest)) then
        deallocate(UFS_errest,stat=alloc_stat)
        if (alloc_stat /= 0) stop 'error while trying to deallocate UFS_errest'
        endif
      endif
      if ( ERREST%LM_matrix ) then
        if(allocated(ERREST%LM_term)) then
        deallocate(ERREST%LM_term,stat=alloc_stat)
        if (alloc_stat /= 0) stop 'error while trying to deallocate LM_term'
        endif
        if(allocated(ERREST%AP_iguess)) then
        deallocate(ERREST%AP_iguess,stat=alloc_stat)
        if (alloc_stat /= 0) stop 'error while trying to deallocate AP_iguess'
        endif
      endif
      endif

!	-------------------------------------------------------------------------

#if defined(OSH)
      IF (RT_SOS_SET%ILUT) CALL DLLC_LUT(RIN)
      IF (RT_SOS_SET%ILUT .OR. RT_SOS_SET%IWUT) CALL DLLC_LUT1(RIN)
#endif
!	-------------------------------------------------------------------------
      !write(iu_main_output,'(a,i4,a)') '*****   end inversion_run_count =', inversion_run_count,' **********'
      !inversion_run_count = inversion_run_count +1

  return
  end subroutine inversion

!!ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

end module mod_grasp_inversion



