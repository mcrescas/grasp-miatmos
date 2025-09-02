! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

module mod_errest_flags

    use mod_par_inv, only : KIMAGE
	  
    implicit none
! -----------------------------------------------------------------------
!  Flags to drive error estimates calculations

    type errest_flags
      logical :: product
      !> mask to compute error estimates; it depends on both meas data and settings
      !> and is defined in inversion routine
      logical, dimension(KIMAGE) :: pix_mask
      !> flag is equal to .false. if all elements of pix_mask=.false.
      logical :: pix
      !> flag for segment defined in inversion routine
      logical :: segm
      !> flag to use Fisher matrix with Levenberg-Marquardt stabilisation
      logical :: LM_matrix
      !> flag to use Levenberg-Marquardt stabilisation in vector of solved system
      logical :: LM_vector
      ! initial guess for retrieved parameters
      real, dimension(:,:), allocatable :: AP_iguess
      ! Levenberg-Maquardt stabilization term
      real, dimension(:,:), allocatable :: LM_term
    end type errest_flags

    type(errest_flags) :: ERREST
!	---------------------------------------------------------------------
      contains
      
      subroutine initialize_ERREST(ERREST)
      type(errest_flags) :: ERREST

      ERREST%product = .false.
      ERREST%pix_mask(:) = .false.
      ERREST%pix = .false.
      ERREST%segm = .false.
      ERREST%LM_matrix = .false.
      ERREST%LM_vector = .false.

      end subroutine initialize_ERREST

end module mod_errest_flags

#include "../constants_set/mod_globals.inc"
#include "../interfaces/solver_macros.inc"
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine ERR_estimates ( iu_main_output,               &
                                sparse_solver_name,           &
                                UF, UFS, nnz_err, UFNZ,       &
                                QS, QIM, AGENP, ALS,          &
                                AP, APMIN, APMAX,             &
                                RIN, INVSING, npixels, MASL,  &
                                errest_pix, dermask_aerpar,   &
                                tau_mol, NHVP_meas, HVP_meas, &
                                GOUT, solver_timer,           &
                                KERNELS1, KERNELS2,           &
                                use_bias_eq, flag_bias, QSE_bias, QIM_bias, ALS_temp)

      use mod_par_inv, only : KPARS, KIMAGE, KPAR, KVERTM
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type 
      use mod_alloc_kernels
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings),         intent(in)  ::  RIN
      integer,                           intent(in)  ::  npixels
      integer,                           intent(in)  ::  iu_main_output
      character(*),                      intent(in)  ::  sparse_solver_name
      integer,                           intent(in)  ::  INVSING
      integer,                           intent(in)  ::  nnz_err
      type(nonzero),                     intent(inout)  ::  UFNZ    
      real,                              intent(in)  ::  AGENP 
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      real,dimension(KPARS,KPARS,KIMAGE),intent(inout)  ::  UFS     
      real,dimension(KPARS,KIMAGE),      intent(in)  ::  QS
      real,dimension(KPAR),              intent(in)  ::  QIM    
      real,dimension(KPAR,KPAR),         intent(in)  ::  UF
      logical, dimension(KIMAGE),        intent(in)  ::  errest_pix
      real,dimension(KPARS),             intent(in)  ::  APMIN,APMAX
      real,dimension(KW,KIMAGE),         intent(in)  ::  tau_mol
      integer,                           intent(in)  ::  NHVP_meas
      real,dimension(KVERTM,KIMAGE),     intent(in)  ::  HVP_meas  ! heights for lidar measurements
      real,dimension(KIMAGE),            intent(in)  ::  MASL
!MEH:
      real,dimension(KPAR),              intent(in)  ::  QIM_bias
!	-------------------------------------------------------------------------------------
      logical, dimension(KPARS),         intent(in)  ::  dermask_aerpar
!MEH
        real,dimension(KPARS,KIMAGE),      intent(in)  ::  QSE_bias ! to calculate the systematic component for opt_param (derivates parameters)
        real, dimension(KIMAGE), intent(in)       :: ALS_temp
        logical,          intent(in)  ::  use_bias_eq
        integer, intent(in) :: flag_bias
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      real,dimension(KPARS,KIMAGE),       intent(inout)  ::  AP
      type(output_segment_general),      intent(inout)  ::  GOUT
      type(kernels_triangle_bin),        intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),       intent(inout)  ::  KERNELS2
!	----------------------------------------------------------------------
! LOCAL :
      real*8, dimension(KPAR) ::  b
      integer ::  IMQ, KNSINGF
      integer ::  KNF

!	----------------------------------------------------------------------
      KNSINGF = RIN%KNSINGF
      IMQ = RIN%IMQ
      KNF = KNSINGF*npixels

      select case(INVSING)
      case(2)   ! multi pixel scenario
          if(IMQ .eq. 3 .or. IMQ .eq. 1) then
              if(sparse_solver_name .eq. 'SUPERLU') &
              call solver_init_SUPERLU(KNF, nnz_err, UFNZ, b)
          endif

          call ERR_estimates_multi_pixel_scenario ( iu_main_output,        &
                                                    sparse_solver_name,    &
                                                    RIN, npixels, MASL,    &
                                                    UF, QIM, AGENP, ALS,   &
                                                    nnz_err, UFNZ, GOUT,   &
                                                    solver_timer,          &
                                                    AP, APMIN, APMAX,      &
                                                    dermask_aerpar,        &
                                                    tau_mol, NHVP_meas, HVP_meas, &
                                                    KERNELS1, KERNELS2,            &
                                                    use_bias_eq, flag_bias, QIM_bias, ALS_temp)
          if ( error_present() ) then
            if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_cleanup_SUPERLU(KNF, nnz_err, UFNZ, b)
            return
          endif
          if(IMQ .eq. 3 .or. IMQ .eq. 1) then
            if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_cleanup_SUPERLU(KNF, nnz_err, UFNZ, b)
          endif

      case(0,1) ! pixel by pixel scenario
          call ERR_estimates_single_pixel_scenario (iu_main_output,     &
                                                    sparse_solver_name, &
                                                    RIN, npixels, MASL, &
                                                    UFS, QS, ALS,       &
                                                    AP, APMIN, APMAX,   &
                                                    UFNZ, GOUT,         &
                                                    solver_timer,       &
                                                    errest_pix, dermask_aerpar,   &
                                                    tau_mol, NHVP_meas, HVP_meas, &
                                                    KERNELS1, KERNELS2,            &
                                                    use_bias_eq, flag_bias, QSE_bias, ALS_temp)
          if ( error_present() ) return
      end select

      return
      end subroutine ERR_estimates

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine ERR_estimates_multi_pixel_scenario ( iu_main_output,        &
                                                      sparse_solver_name,    &
                                                      RIN, npixels, MASL,    &
                                                      UF, QIM, AGENP, ALS,   &
                                                      nnz_err, UFNZ, GOUT,   &
                                                      solver_timer,          &
                                                      AP, APMIN, APMAX,      &
                                                      dermask_aerpar,        &
                                                      tau_mol, NHVP_meas, HVP_meas, &
                                                      KERNELS1, KERNELS2,            &
                                                      use_bias_eq, flag_bias, QIM_bias, ALS_temp)

      use mod_par_inv, only : KPARS, KIMAGE, KPAR, KW, KVERTM
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type 
      use mod_alloc_kernels
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings),         intent(in)  ::  RIN
      integer,                           intent(in)  ::  iu_main_output
      character(*),                      intent(in)  ::  sparse_solver_name
      integer,                           intent(in)  ::  npixels
      integer,                           intent(in)  ::  nnz_err
      type(nonzero),                     intent(inout)  ::  UFNZ    
      real,                              intent(in)  ::  AGENP 
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      real,dimension(KPAR),              intent(in)  ::  QIM
      real,dimension(KPAR,KPAR),         intent(in)  ::  UF
      real,dimension(KPARS),             intent(in)  ::  APMIN,APMAX
      real,dimension(KW,KIMAGE),         intent(in)  ::  tau_mol
      integer,                           intent(in)  ::  NHVP_meas
      real,dimension(KVERTM,KIMAGE),     intent(in)  ::  HVP_meas  ! heights for lidar measurements
      logical,dimension(KPARS),          intent(in)  ::  dermask_aerpar
      real,dimension(KIMAGE),            intent(in)  :: MASL
!MEH:
      real,dimension(KPAR),              intent(in)  ::  QIM_bias
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      real,dimension(KPARS,KIMAGE),      intent(inout)  ::  AP
      type(output_segment_general),      intent(inout)  ::  GOUT
      type(kernels_triangle_bin),        intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),       intent(inout)  ::  KERNELS2
!	----------------------------------------------------------------------
! MEH --> CHECK IN MULTI-PIXEL!
    real, dimension(KIMAGE), intent(in)       :: ALS_temp
    logical,          intent(in)  ::  use_bias_eq
    integer, intent(in) :: flag_bias
! LOCAL :
      real,dimension(:,:,:),allocatable ::  UW0
      integer ::  alloc_stat
      integer ::  KNSINGF, NW
      integer ::  KNF, NOF, NSD1
! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
!	----------------------------------------------------------------------
      NW = RIN%nw
      KNSINGF = RIN%KNSINGF

      if(RIN%products%errest%par) then
        if ( RIN%indep_par ) then
          write(tmp_message,'(3a)') 'PAR_ERR_estimates is under development.', &
          NEW_LINE('A'), &
          'Segment can be processed with products.error_estimation.parameters: false'
          G_ERROR(trim(tmp_message))
          call PAR_ERR_estimates_multi_pixel_scenario_indep_par (              &
                                                        iu_main_output,        & ! IN
                                                        sparse_solver_name,    &
                                                        RIN, npixels, MASL,    &
                                                        UF, QIM, AGENP, AP,    &
                                                        NHVP_meas, HVP_meas,   &
                                                        nnz_err,               &
                                                        UFNZ, GOUT%errest%par, & ! INOUT
                                                        solver_timer           &
                                                                )
        else
          call PAR_ERR_estimates_multi_pixel_scenario ( iu_main_output,        & ! IN
                                                        sparse_solver_name,    &
                                                        RIN, npixels,          &
                                                        UF, QIM, AGENP,        &
                                                        nnz_err,               &
                                                        UFNZ, GOUT%errest%par, & ! INOUT
                                                        solver_timer           &
                                                      )

        endif
        if ( error_present() ) return
        GOUT%products%errest%par = .true.
      endif

      if(RIN%products%errest%aerosol%opt .or. RIN%products%errest%aerosol%lidar) then
          call OPT_ERR_estimates_number_of_optchar (RIN, NOF)
          call OPT_ERR_estimates_number_of_sd (RIN, NSD1)
          allocate(UW0(NW*NOF*NSD1,KNSINGF,npixels), stat=alloc_stat)
          if (alloc_stat /= 0) then
            write(tmp_message,'(a)') 'error while trying to allocate UW0'
            G_ERROR(trim(tmp_message))
          endif
          call OPT_ERR_estimates_deriv_matrix ( iu_main_output,      &
                                                RIN, npixels,        &
                                                ALS, GOUT,           &
                                                AP, APMIN, APMAX,    &
                                                dermask_aerpar,      &
                                                tau_mol, NHVP_meas, HVP_meas, &
                                                KERNELS1,KERNELS2,   &
                                                NOF, NSD1, UW0,       &
                                                use_bias_eq, ALS_temp)
          if ( error_present() ) return
          call OPT_ERR_estimates_multi_pixel_scenario ( iu_main_output,  &
                                                        RIN, npixels,    &
                                                        UF, QIM, AGENP,  &
                                                        nnz_err,         &
                                                        UFNZ, GOUT,      &
                                                        solver_timer,    &
                                                        NOF, NSD1, UW0,   &
                                                        use_bias_eq, flag_bias, QIM_bias)
          if ( error_present() ) return
          GOUT%products%errest%aerosol%opt = .true.
          if(RIN%products%errest%aerosol%lidar .and. (RIN%DLSF%keyEL .gt. 0)) &
          GOUT%products%errest%aerosol%lidar = .true.

          deallocate(UW0,stat=alloc_stat)
          if (alloc_stat /= 0) then
            write(tmp_message,'(a)') 'error while trying to deallocate UW0'
            G_ERROR(trim(tmp_message))
          endif
      endif

      return
      end subroutine ERR_estimates_multi_pixel_scenario

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine ERR_estimates_single_pixel_scenario ( iu_main_output,     &
                                                      sparse_solver_name, &
                                                      RIN, npixels, MASL, &
                                                      UFS, QS, ALS,       &
                                                      AP, APMIN, APMAX,   &
                                                      UFNZ, GOUT,         &
                                                      solver_timer,       &
                                                      errest_pix, dermask_aerpar,   &
                                                      tau_mol, NHVP_meas, HVP_meas, &
                                                      KERNELS1, KERNELS2,            &
                                                      use_bias_eq, flag_bias, QSE_bias, ALS_temp)

      use mod_par_inv, only : KPARS, KIMAGE, KW, KVERTM
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type 
      use mod_alloc_kernels
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings),         intent(in)  ::  RIN
      integer,                           intent(in)  ::  iu_main_output
      character(*),                      intent(in)  ::  sparse_solver_name
      integer,                           intent(in)  ::  npixels
      type(nonzero),                     intent(inout)  ::  UFNZ
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      real,dimension(KPARS,KPARS,KIMAGE),intent(inout)  ::  UFS
      real,dimension(KPARS,KIMAGE),      intent(in)  ::  QS
      logical, dimension(KIMAGE),        intent(in)  ::  errest_pix
      real,dimension(KPARS),             intent(in)  ::  APMIN, APMAX
      real,dimension(KW,KIMAGE),         intent(in)  ::  tau_mol
      integer,                           intent(in)  ::  NHVP_meas
      real,dimension(KVERTM,KIMAGE),     intent(in)  ::  HVP_meas  ! heights for lidar measurements
      logical, dimension(KPARS),         intent(in)  ::  dermask_aerpar
      real,dimension(KIMAGE),            intent(in)  :: MASL

!MEH
        real,dimension(KPARS,KIMAGE),      intent(in)  ::  QSE_bias
        real, dimension(KIMAGE), intent(in)       :: ALS_temp
        logical,          intent(in)  ::  use_bias_eq
        integer, intent(in) :: flag_bias
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      real,dimension(KPARS,KIMAGE),      intent(inout)  ::  AP
      type(output_segment_general),      intent(inout)  ::  GOUT
      type(kernels_triangle_bin),        intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),       intent(inout)  ::  KERNELS2
!	----------------------------------------------------------------------
! LOCAL :
!! MEH:
      real,dimension(:,:,:),allocatable ::  UW0_mic !in case of sd when is not part of the retireval
      real,dimension(:,:,:),allocatable ::  UW0
      integer ::  alloc_stat , i
      integer ::  NW, KNSINGF
      integer ::  NOF, NSD1



! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
!	----------------------------------------------------------------------
      NW = RIN%nw
      KNSINGF = RIN%KNSINGF
      

!MH - print the Fisher Matrix     
     !!OPEN(20, FILE = 'FisherMatrix.txt')
     !!WRITE(20, *) 'UFS(:,:,1)'
     !!WRITE(20,'(350i12)') (i, i = 1, KNSINGF)
     !!do i = 1, KNSINGF
        !!WRITE(20,'(350es12.4)') UFS(1:KNSINGF,i,1)
     !!enddo
     !!CLOSE(20)

      if(RIN%products%errest%par) then
        if ( RIN%indep_par ) then
          write(tmp_message,'(3a)') 'PAR_ERR_estimates is under development.', &
          NEW_LINE('A'), &
          'Segment can be processed with products.error_estimation.parameters: false'
          G_ERROR(trim(tmp_message))
          call PAR_ERR_estimates_single_pixel_scenario_indep_par (             &
                                                        iu_main_output,        & ! IN
                                                        sparse_solver_name,    &
                                                        RIN, npixels, MASL,    &
                                                        UFS, QS, ALS, AP,      &
                                                        NHVP_meas, HVP_meas,   &
                                                        errest_pix,            &
                                                        UFNZ, GOUT%errest%par, & ! INOUT
                                                        solver_timer           &
                                                                 )
        else
          call PAR_ERR_estimates_single_pixel_scenario (iu_main_output,        & ! IN
                                                        sparse_solver_name,    &
                                                        RIN, npixels,          &
                                                        UFS, QS, ALS,          &
                                                        errest_pix,            &
                                                        UFNZ, GOUT%errest%par, & ! INOUT
                                                        solver_timer           &
                                                       )
        endif
        if ( error_present() ) return
        GOUT%products%errest%par = .true.
      endif

      if(RIN%products%errest%aerosol%opt .or. RIN%products%errest%aerosol%lidar) then
          call OPT_ERR_estimates_number_of_optchar (RIN, NOF)
          call OPT_ERR_estimates_number_of_sd (RIN, NSD1)
          allocate(UW0(NW*NOF*NSD1,KNSINGF,npixels), stat=alloc_stat)
!MEH: change dimension for the variables!
          allocate(UW0_mic(22*1*2,KNSINGF,npixels), stat=alloc_stat)
          if (alloc_stat /= 0) then
            write(tmp_message,'(a)') 'error while trying to allocate UW0'
            G_ERROR(trim(tmp_message))
          endif

!MEH : if SD_LN and RIN%products%errest%aerosol%mic call:
        if (RIN%products%errest%aerosol%mic) then
          call MIC_ERR_estimates_deriv_matrix ( iu_main_output,          &
                                                    RIN, npixels,        &
                                                    ALS, GOUT,           &
                                                    AP, APMIN, APMAX,    &
                                                    dermask_aerpar,      &
                                                    tau_mol, NHVP_meas, HVP_meas, &
                                                    KERNELS1,KERNELS2,   &
                                                    NOF, NSD1, UW0_mic,      &
                                                    use_bias_eq, ALS_temp)

           call MIC_ERR_estimates_single_pixel_scenario (iu_main_output,    &
                                                    sparse_solver_name,     &
                                                    RIN, npixels,           &
                                                    UFS, QS, ALS,           &
                                                    errest_pix,             &
                                                    UFNZ, GOUT,             &
                                                    solver_timer,           &
                                                    NOF, NSD1, UW0_mic,          &
                                                    use_bias_eq, flag_bias, QSE_bias)
         if ( error_present() ) return
        GOUT%products%errest%aerosol%mic = .true.
        endif

          call OPT_ERR_estimates_deriv_matrix ( iu_main_output,      &
                                                RIN, npixels,        &
                                                ALS, GOUT,           &
                                                AP, APMIN, APMAX,    &
                                                dermask_aerpar,      &
                                                tau_mol, NHVP_meas, HVP_meas, &
                                                KERNELS1,KERNELS2,   &
                                                NOF, NSD1, UW0,       &
                                                use_bias_eq, ALS_temp)
          if ( error_present() ) return
          call OPT_ERR_estimates_single_pixel_scenario (iu_main_output,         &
                                                        sparse_solver_name,     &
                                                        RIN, npixels,           &
                                                        UFS, QS, ALS,           &
                                                        errest_pix,             &
                                                        UFNZ, GOUT,             &
                                                        solver_timer,           &
                                                        NOF, NSD1, UW0,          &
                                                        use_bias_eq, flag_bias, QSE_bias)
          if ( error_present() ) return
          GOUT%products%errest%aerosol%opt = .true.
          if(RIN%products%errest%aerosol%lidar .and. (RIN%DLSF%keyEL .gt. 0)) &
          GOUT%products%errest%aerosol%lidar = .true.

          

          deallocate(UW0_mic,stat=alloc_stat)
          deallocate(UW0,stat=alloc_stat)
          if (alloc_stat /= 0) then
            write(tmp_message,'(a)') 'error while trying to deallocate UW0'
            G_ERROR(trim(tmp_message))
          endif
      endif

      return
      end subroutine ERR_estimates_single_pixel_scenario

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! ERR_ssa = sqrt ( (UW0_sca*QS1_sca + UW0_ext*QS1_ext - 2 * UW0_sca*QS1_ext) * RES )
! (QS1_sca) = UF^(-1)*(UW0_sca)^T
! (QS1_ext) = UF^(-1)*(UW0_ext)^T
! BIAS_ssa = BIAS_sca - BIAS_ext
! r = 2 * UW0_sca*QS1_ext / (2 * sqrt(UW0_sca*QS1_sca) * sqrt(UW0_ext*QS1_ext)) 
!
      subroutine OPT_ERR_estimates_single_pixel_scenario (iu_main_output,         &
                                                          sparse_solver_name,     &
                                                          RIN, npixels,           &
                                                          UFS, QS, ALS,           &
                                                          errest_pix,             &
                                                          UFNZ, GOUT,             &
                                                          solver_timer,           &
                                                          NOF, NSD1, UW0,          &
                                                          use_bias_eq, flag_bias, QSE_bias)

      use mod_par_inv, only : KPARS, KIMAGE, KW
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type 
      use mod_alloc_kernels
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings),         intent(in)  ::  RIN
      integer,                           intent(in)  ::  npixels
      integer,                           intent(in)  ::  iu_main_output
      character(*),                      intent(in)  ::  sparse_solver_name
      type(nonzero),                     intent(inout)  ::  UFNZ
      real,dimension(KPARS,KPARS,KIMAGE),intent(inout)  ::  UFS     
      real,dimension(KPARS,KIMAGE),      intent(in)  ::  QS
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      logical, dimension(KIMAGE),        intent(in)  ::  errest_pix
      integer,                           intent(in)  ::  NOF, NSD1
      real,dimension(RIN%NW*NOF*NSD1,RIN%KNSINGF,npixels),intent(in) ::  UW0

!MEH
!        logical,          intent(in)  ::  use_bias_eq
!        integer, intent(in) :: flag_bias
      real,dimension(KPARS,KIMAGE),      intent(in)  ::  QSE_bias
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      type(output_segment_general),      intent(inout)  ::  GOUT
!	----------------------------------------------------------------------
! LOCAL :
      real, dimension(KPARS) :: FFS, QS1
      real, dimension(KPARS,KW,NSD1) :: QS1_ext
      real, dimension(KW,NSD1,KIMAGE) :: ERR_ext,  ERR_sca,  ERR_mix
      real, dimension(KW,NSD1,KIMAGE) :: BIAS_ext, BIAS_sca, r
      real, dimension(KIMAGE) :: RES
      real*8, dimension(KPAR) :: b
      integer :: i, j1, IOF, ISD, IW, ipix
      real :: ERR_temp, BIAS_temp
      integer :: NW, IMQ, KNSINGF
      integer :: nnz_pix
      integer :: KNF
      logical :: use_bias_eq
      integer :: flag_bias

! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
!	----------------------------------------------------------------------
      RES(1:npixels) = ALS(1:npixels)
      NW     = RIN%nw
      KNSINGF= RIN%KNSINGF
      IMQ    = RIN%IMQ
!write(*,*) 'QS:'
!write(*,'(10e14.5)') QS(1:KNSINGF,1)
!write(*,*) 'UFS(i,i):'
!write(*,'(10e14.5)') (UFS(i,i,1),i=1,KNSINGF)
!write(*,*) 'ALS:'
!write(*,'(10e14.5)') ALS(1:npixels)

      if ( RIN%IPRI_additional_info ) then
        if ( RIN%naod_errest_iwl   .gt. 0 .or. &
             RIN%nssa_errest_iwl   .gt. 0 .or. &
             RIN%naext_errest_iwl   .gt. 0 .or. &
             RIN%nlidar_errest_iwl .gt. 0      &
           ) then
          write(iu_main_output,'(a,/,a,/,a)') &
          'WARNING in OPT_ERR_estimates_single_pixel_scenario : ', &
          'opt. charact. error estimate mask is not applied in single pixel scenario.', &
          'Error estimates are calculated for all optical characteristics.'
        endif
      endif

loop_pixel: do ipix=1,npixels

        if(.not. errest_pix(ipix)) cycle
        if(RIN%IPRI_verbose .or. RIN%IPRI_additional_info) then
          do i=1,RIN%KNSINGF
            if(UFS(i,i,ipix) .eq. 0.0) then
              !UFS(i,i,ipix) = 1e-30
              !if(RIN%IPRI_verbose)  &
              write(tmp_message,'(a,2i4,a)') &
              'Warning OPT_ERR_estimates: UFS diagonal element is ZERO for', &
              ipix,i,'  ipix,i'
              G_ERROR(trim(tmp_message))
            endif
          enddo
        endif ! IPRI_verbose .or.
        if(IMQ .eq. 3 .or. IMQ .eq. 1) then
          call UF_nonzero_one_pixel ( KNSINGF,UFS(:,:,ipix), & ! IN
                                      UFNZ, nnz_pix          & ! OUT
                                    )
          if ( error_present() ) return
       endif


! opt_funct - ext, sca, lr
!! MEH temporarly print
       !!OPEN(11, FILE = 'TestOutput_optical.txt')
       !!OPEN(12, FILE = 'TestOutput_optical_EXT_SCA.txt')
       !!WRITE(11,*) 'b(:)'
       !!WRITE(12, *) 'b(:) ext: 1 - sca: 2'

!! MEH temporarly print
       !!OPEN(13, FILE = 'Matriz_Cov_OPT.txt')
       !!write(13,*) 'ALS(ipix): ', ALS(ipix)
       !!write(13,'(4x,50i12)') (IW, IW = 1, NW)
              
LOOP_opt_funct: do IOF=1,NOF
        
   do ISD=1,NSD1
          do IW=1,NW 
             j1 = (IOF-1)*NSD1*NW+(ISD-1)*NW+IW
             FFS(:) = 0.0
              do i=1,KNSINGF
                FFS(i) = UW0(j1,i,ipix)
              enddo ! i
              if(IMQ .eq. 2) then
                call ITERQ (  IMQ,KNSINGF,                   & ! IN
                              UFS(1:KNSINGF,1:KNSINGF,ipix), &
                              FFS(1:KNSINGF),                &
                              QS1(1:KNSINGF)                 & ! OUT
                            )
              else if(IMQ .eq. 3 .or. IMQ .eq. 1) then
                b(1:KNSINGF) = FFS(1:KNSINGF)
                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_init_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
                call sparse_solver (  iu_main_output,       & ! IN
                                      KNSINGF, nnz_pix, UFNZ, &
                                      b,                    & ! INOUT
                                      solver_timer          &
                                    )
                if ( error_present() ) return
                QS1(1:KNSINGF) = b(1:KNSINGF)
! MH - prints
                !!write(11,'(i4,350es12.4)') IW, b(1:KNSINGF)
                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_cleanup_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
              endif ! IMQ .eq. 2
             
              ERR_temp  = 0.0
              BIAS_temp = 0.0
              do i=1,KNSINGF
                ERR_temp  = ERR_temp  + UW0(j1,i,ipix)*QS1(i)
                BIAS_temp = BIAS_temp + UW0(j1,i,ipix)*QSE_bias(i,ipix) !UW0(j1,i,ipix)*QS(i,ipix)
              enddo ! i
              
              select case ( IOF )
              case ( 1 )
                ! save for extinction to be used for ssa (IOF=2)
                QS1_ext(1:KNSINGF,IW,ISD) = QS1(1:KNSINGF)
                !!write(12,'(i4,350es12.4)') IOF, QS1(1:KNSINGF)
                ERR_ext(IW,ISD,ipix) = ERR_temp
                BIAS_ext(IW,ISD,ipix) = BIAS_temp
              case ( 2 )
                !!write(12,'(i4,350es12.4)') IOF, QS1(1:KNSINGF)
                ! compute for ssa
                ERR_sca(IW,ISD,ipix) = ERR_temp
                BIAS_sca(IW,ISD,ipix) = BIAS_temp
                ERR_mix(IW,ISD,ipix) = 0.0
                do i=1,KNSINGF
                   ERR_mix(IW,ISD,ipix) = ERR_mix(IW,ISD,ipix) + UW0(j1,i,ipix)*QS1_ext(i,IW,ISD)
                   !write(*,'(350es12.4)') 'UW0', UW0(j1,i,ipix)
                enddo ! i
               ! write(*,*) 'QS1(1:KNSINGF)'
               ! write(*,'(350es12.4)') QS1(1:KNSINGF)
                ERR_mix(IW,ISD,ipix) = 2.0 * ERR_mix(IW,ISD,ipix) 
                ERR_temp = ERR_sca(IW,ISD,ipix) + ERR_ext(IW,ISD,ipix) - ERR_mix(IW,ISD,ipix)
                BIAS_temp = BIAS_sca(IW,ISD,ipix) - BIAS_ext(IW,ISD,ipix)
                r(IW,ISD,ipix) = sqrt(ERR_sca(IW,ISD,ipix)) * sqrt(ERR_ext(IW,ISD,ipix))
! MH - check if only with 0.5 is ok! (instead of 2 in ERR_mix)
                r(IW,ISD,ipix) = 0.5 * ERR_mix(IW,ISD,ipix) / r(IW,ISD,ipix) 
             end select
              ERR_temp = sqrt(ERR_temp*ALS(ipix))  !ALS(ipix) es el residuo
!write(*,*) 'IOF',IOF,'ERR_temp',ERR_temp
              call set_opt_err (  IOF, IW, ISD, ipix,  & ! IN
                                  use_bias_eq, flag_bias, &
                                  ERR_temp, BIAS_temp, &
                                  GOUT%errest%aerosol  & ! INOUT
                               )
              if ( error_present() ) return
           enddo ! IW
           !! MEH Temporarly print
              !!if (IOF .EQ. 1) write(13,'(3i4,350es12.4)') ISD, ipix, IOF, ERR_ext(1:NW,ISD,ipix)
              !!if (IOF .EQ. 2) THEN
                 !!write(13,'(3i4,350es12.4)')  ISD, ipix, IOF, ERR_sca(1:NW,ISD,ipix)
                 !!write(13,'(3i4,350es12.4)')  ISD, ipix, IOF, ERR_mix(1:NW,ISD,ipix)*0.5
                 !!write(13,'(3i4,350es12.4)')  ISD, ipix, IOF, r(1:NW,ISD,ipix)
              !!endif
        enddo ! ISD
enddo LOOP_opt_funct
     
!!CLOSE(11)
!!CLOSE(12)
!!CLOSE(13)

enddo loop_pixel

      if ( RIN%IPRI_additional_info ) then
        call print_ssa_errest_components (  iu_main_output, RIN, NSD1, npixels, RES, &
                                            ERR_ext,  ERR_sca,  ERR_mix, &
                                            BIAS_ext, BIAS_sca, r )
      endif

      return
      end subroutine OPT_ERR_estimates_single_pixel_scenario

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! ERR_ssa = sqrt ( (UW0_sca*b1_sca + UW0_ext*b1_ext - 2 * UW0_sca * b1_ext) * RES )
! (b1_sca) = UF^(-1)*(UW0_sca)^T
! (b1_ext) = UF^(-1)*(UW0_ext)^T
! BIAS_ssa = BIAS_sca - BIAS_ext
! r = 2 * UW0_sca*b1_ext / sqrt(UW0_sca*b1_sca) / sqrt(UW0_ext*b1_ext)
!
      subroutine OPT_ERR_estimates_multi_pixel_scenario ( iu_main_output, &
                                                          RIN, npixels,   &
                                                          UF, QIM, AGENP, &
                                                          nnz_err,        &
                                                          UFNZ, GOUT,     &
                                                          solver_timer,   &
                                                          NOF, NSD1, UW0,  &
                                                          use_bias_eq, flag_bias, QIM_bias)

      use mod_globals, only : GBL_FILE_PATH_LEN
      use mod_par_inv, only : KPARS, KIMAGE, KPAR, KW
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type 
      use mod_alloc_kernels
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings),         intent(in)  ::  RIN
      integer,                           intent(in)  ::  npixels
      integer,                           intent(in)  ::  iu_main_output
      integer,                           intent(in)  ::  nnz_err
      type(nonzero),                     intent(inout)  ::  UFNZ    
      real,                              intent(in)  ::  AGENP
      real,dimension(KPAR),              intent(in)  ::  QIM
      real,dimension(KPAR,KPAR),         intent(in)  ::  UF
      integer,                           intent(in)  ::  NOF, NSD1
      real,dimension(RIN%NW*NOF*NSD1,RIN%KNSINGF,npixels),intent(in) ::  UW0
!MEH:
      real,dimension(KPAR),              intent(in)  ::  QIM_bias
      logical,          intent(in)  ::  use_bias_eq
      integer, intent(in) :: flag_bias
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      type(output_segment_general),      intent(inout)  ::  GOUT
!	----------------------------------------------------------------------
! LOCAL :
      real ::  DL1
      integer ::  IWW
      real*8, dimension(:,:), allocatable ::  ATEMP
      real*8, dimension(KPAR) ::  b
      real,   dimension(KPAR) ::  b1
      real, dimension(KPAR,KW,NSD1) :: b1_ext
      real, dimension(KW,NSD1,KIMAGE) :: ERR_ext,  ERR_sca,  ERR_mix
      real, dimension(KW,NSD1,KIMAGE) :: BIAS_ext, BIAS_sca, r
      real, dimension(KIMAGE) :: RES
      logical ::  ltest
      integer ::  alloc_stat
      integer ::  i, i1, j, j1, IOF, ISD, IW, ipix
      real ::  tiny, ERR_temp, BIAS_temp
      integer ::  NW, IMQ, KNSINGF
      integer ::  KNF
      integer, dimension(4)    :: num_opt_errest_iwl
      integer, dimension(KW,4) :: opt_errest_iwl
!      logical :: use_bias_eq
!      integer :: flag_bias

! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
!	----------------------------------------------------------------------
      RES(1:npixels) = AGENP
      NW     = RIN%nw
      KNSINGF= RIN%KNSINGF
      IMQ    = RIN%IMQ
      KNF = KNSINGF*npixels

      num_opt_errest_iwl(1) = RIN%naod_errest_iwl
      num_opt_errest_iwl(2) = RIN%nssa_errest_iwl
      num_opt_errest_iwl(3) = RIN%naext_errest_iwl
      num_opt_errest_iwl(4) = RIN%nlidar_errest_iwl
      opt_errest_iwl(1:num_opt_errest_iwl(1),1) = &
                            RIN%aod_errest_iwl(1:num_opt_errest_iwl(1))
      opt_errest_iwl(1:num_opt_errest_iwl(2),2) = &
                            RIN%ssa_errest_iwl(1:num_opt_errest_iwl(2))
      opt_errest_iwl(1:num_opt_errest_iwl(3),3) = &
                            RIN%aext_errest_iwl(1:num_opt_errest_iwl(3))
      opt_errest_iwl(1:num_opt_errest_iwl(4),4) = &
                            RIN%lidar_errest_iwl(1:num_opt_errest_iwl(4))

      ltest = .false.
      if(ltest) then
        allocate(ATEMP(KNF,KNF),stat=alloc_stat)
        if (alloc_stat /= 0) then
          write(tmp_message,'(a)') 'error while trying to allocate ATEMP'
          G_ERROR(trim(tmp_message))
        endif
          ATEMP(:,:) = 0.0
          do i=1,nnz_err
            ATEMP(UFNZ%row(i),UFNZ%col(i)) = UFNZ%val(i)
          enddo ! i
      endif ! ltest

loop_pixel: do ipix=1,npixels
!      write(*,*) 'ipix=',ipix
!      write(*,*) '1: UW0'
!      write(*,'(10e14.5)') UW0(1:NW*NOF*NSD1,1:KNSINGF,ipix)

! opt_funct - ext, ssa, aext, lr
LOOP_opt_funct: do IOF=1,NOF            
        if(num_opt_errest_iwl(IOF) .eq. 0) cycle
        do ISD=1,NSD1
          do i1=1,num_opt_errest_iwl(IOF)
          IW = opt_errest_iwl(i1,IOF)
              j1 = (IOF-1)*NSD1*NW+(ISD-1)*NW+IW
              b(:) = 0.0d0
              do i=1,KNSINGF
                j = KNSINGF*(ipix-1)+i
                b(j) = UW0(j1,i,ipix)
              enddo ! i
              if(IMQ .eq. 2) then
                call ITERQ (  IMQ,KNF,               & ! IN
                              real(UF(1:KNF,1:KNF)), &
                              real(b(1:KNF)),        &
                              b1(1:KNF)              & ! OUT
                           )
              else if(IMQ .eq. 3 .or. IMQ .eq. 1) then
                call sparse_solver (  iu_main_output,     & ! IN
                                      KNF, nnz_err, UFNZ, &
                                      b,                  & ! INOUT
                                      solver_timer        &
                                   )
                if ( error_present() ) return
                b1(1:KNF) = b(1:KNF)
              endif ! IMQ .eq. 2
              ERR_temp  = 0.0
              BIAS_temp = 0.0
              do i=1,KNSINGF
                j = KNSINGF*(ipix-1)+i
                ERR_temp  = ERR_temp  + UW0(j1,i,ipix)*b1(j)
                BIAS_temp = BIAS_temp + UW0(j1,i,ipix)*QIM_bias(j) !QIM(j)
              enddo ! i
              
              select case ( IOF )
              case (1)
                ! save for extinction to be used for ssa (IOF=2)
                b1_ext(1:KNF,IW,ISD) = b1(1:KNF)
                ERR_ext(IW,ISD,ipix) = ERR_temp
                BIAS_ext(IW,ISD,ipix) = BIAS_temp
              case ( 2 )
                ! compute for ssa
                ERR_sca(IW,ISD,ipix) = ERR_temp
                BIAS_sca(IW,ISD,ipix) = BIAS_temp
                ERR_mix(IW,ISD,ipix) = 0.0
                do i=1,KNSINGF
                j = KNSINGF*(ipix-1)+i
                ERR_mix(IW,ISD,ipix) = ERR_mix(IW,ISD,ipix) + UW0(j1,i,ipix)*b1_ext(j,IW,ISD)
                enddo ! i
                ERR_mix(IW,ISD,ipix) = 2.0 * ERR_mix(IW,ISD,ipix)
                ERR_temp = ERR_sca(IW,ISD,ipix) + ERR_ext(IW,ISD,ipix) - ERR_mix(IW,ISD,ipix)
                BIAS_temp = BIAS_sca(IW,ISD,ipix) - BIAS_ext(IW,ISD,ipix)
                r(i1,ISD,ipix) = sqrt(ERR_sca(IW,ISD,ipix)) * sqrt(ERR_ext(IW,ISD,ipix))
                r(i1,ISD,ipix) = ERR_mix(IW,ISD,ipix) / r(i1,ISD,ipix)
              end select
              ERR_temp = sqrt(ERR_temp*AGENP)
! TL
!use_bias_eq = .false.
!flag_bias = bias_basic
              call set_opt_err (  IOF, IW, ISD, ipix,    & ! IN
                                  use_bias_eq, flag_bias, &
                                  ERR_temp, BIAS_temp,   &
                                  GOUT%errest%aerosol    & ! INOUT
                               )
              if ( error_present() ) return

          enddo ! i1
        enddo ! ISD

enddo LOOP_opt_funct

enddo loop_pixel

      if ( RIN%IPRI_additional_info ) then
        call print_ssa_errest_components (  iu_main_output, RIN, NSD1, npixels, RES, &
                                            ERR_ext,  ERR_sca,  ERR_mix, &
                                            BIAS_ext, BIAS_sca, r )
      endif

      if(ltest) then
         deallocate(ATEMP,stat=alloc_stat)
         if (alloc_stat /= 0) then
           write(tmp_message,'(a)') 'error while trying to deallocate ATEMP'
           G_ERROR(trim(tmp_message))
         endif
      endif ! ltest

      return
      end subroutine OPT_ERR_estimates_multi_pixel_scenario
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! MEH: subroutine for SD when it is not part of retrieved parameters
      subroutine MIC_ERR_estimates_single_pixel_scenario (iu_main_output,         &
                                                          sparse_solver_name,     &
                                                          RIN, npixels,           &
                                                          UFS, QS, ALS,           &
                                                          errest_pix,             &
                                                          UFNZ, GOUT,             &
                                                          solver_timer,           &
                                                          NOF, NSD1, UW0_mic,          &
                                                          use_bias_eq, flag_bias, QSE_bias)

      use mod_par_inv, only : KPARS, KIMAGE, KW
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_alloc_kernels
      use mod_stop_report

      implicit none
!    ----------------------------------------------------------------------
! IN :
      type(retr_input_settings),         intent(in)  ::  RIN
      integer,                           intent(in)  ::  npixels
      integer,                           intent(in)  ::  iu_main_output
      character(*),                      intent(in)  ::  sparse_solver_name
      type(nonzero),                     intent(inout)  ::  UFNZ
      real,dimension(KPARS,KPARS,KIMAGE),intent(inout)  ::  UFS
      real,dimension(KPARS,KIMAGE),      intent(in)  ::  QS
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      logical, dimension(KIMAGE),        intent(in)  ::  errest_pix
      integer,                           intent(in)  ::  NOF, NSD1
      real,dimension(RIN%NW*1*NSD1,RIN%KNSINGF,npixels),intent(in) ::  UW0_mic
      !nof = 1
!MEH
!        logical,          intent(in)  ::  use_bias_eq
!        integer, intent(in) :: flag_bias
      real,dimension(KPARS,KIMAGE),      intent(in)  ::  QSE_bias
!    ----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      type(output_segment_general),      intent(inout)  ::  GOUT
!    ----------------------------------------------------------------------
! LOCAL :
      real, dimension(KPARS) :: FFS, QS1
      real, dimension(KPARS,KW,NSD1) :: QS1_ext
      real, dimension(KW,NSD1,KIMAGE) :: ERR_ext,  ERR_sca,  ERR_mix
      real, dimension(KW,NSD1,KIMAGE) :: BIAS_ext, BIAS_sca, r
      real, dimension(KIMAGE) :: RES
      real*8, dimension(KPAR) :: b
      integer :: i, j1, IOF, ISD, IW, ipix
      real :: ERR_temp, BIAS_temp
      integer :: NW, IMQ, KNSINGF
      integer :: nnz_pix
      integer :: KNF
      logical :: use_bias_eq
      integer :: flag_bias
      integer :: radSD

! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
!    ----------------------------------------------------------------------

      RES(1:npixels) = ALS(1:npixels)
      NW     = RIN%nw
      KNSINGF= RIN%KNSINGF
      IMQ    = RIN%IMQ
      radSD = GOUT%retrieval%information%ngrid(1)
!write(*,*) 'QS:'
!write(*,'(10e14.5)') QS(1:KNSINGF,1)
!write(*,*) 'UFS(i,i):'
!write(*,'(10e14.5)') (UFS(i,i,1),i=1,KNSINGF)
!write(*,*) 'ALS:'
!write(*,'(10e14.5)') ALS(1:npixels)


loop_pixel: do ipix=1,npixels

        if(.not. errest_pix(ipix)) cycle
        if(RIN%IPRI_verbose .or. RIN%IPRI_additional_info) then
          do i=1,RIN%KNSINGF
            if(UFS(i,i,ipix) .eq. 0.0) then
              !UFS(i,i,ipix) = 1e-30
              !if(RIN%IPRI_verbose)  &
              write(tmp_message,'(a,2i4,a)') &
              'Warning OPT_ERR_estimates: UFS diagonal element is ZERO for', &
              ipix,i,'  ipix,i'
              G_ERROR(trim(tmp_message))
            endif
          enddo
        endif ! IPRI_verbose .or.
        if(IMQ .eq. 3 .or. IMQ .eq. 1) then
          call UF_nonzero_one_pixel ( KNSINGF,UFS(:,:,ipix), & ! IN
                                      UFNZ, nnz_pix          & ! OUT
                                    )
          if ( error_present() ) return
       endif


LOOP_opt_funct: do IOF=1,1
        
   do ISD=1,NSD1
          do IW=1,radSD
             j1 = (IOF-1)*NSD1*radSD+(ISD-1)*radSD+IW
             FFS(:) = 0.0
              do i=1,KNSINGF
                FFS(i) = UW0_mic(j1,i,ipix)
              enddo ! i
              if(IMQ .eq. 2) then
                call ITERQ (  IMQ,KNSINGF,                   & ! IN
                              UFS(1:KNSINGF,1:KNSINGF,ipix), &
                              FFS(1:KNSINGF),                &
                              QS1(1:KNSINGF)                 & ! OUT
                            )
              else if(IMQ .eq. 3 .or. IMQ .eq. 1) then
                b(1:KNSINGF) = FFS(1:KNSINGF)
                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_init_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
                call sparse_solver (  iu_main_output,       & ! IN
                                      KNSINGF, nnz_pix, UFNZ, &
                                      b,                    & ! INOUT
                                      solver_timer          &
                                    )
                if ( error_present() ) return
                QS1(1:KNSINGF) = b(1:KNSINGF)

                if(sparse_solver_name .eq. 'SUPERLU') &
                call solver_cleanup_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
              endif ! IMQ .eq. 2
             
              ERR_temp  = 0.0
              BIAS_temp = 0.0
              do i=1,KNSINGF
                ERR_temp  = ERR_temp  + UW0_mic(j1,i,ipix)*QS1(i)
                BIAS_temp = BIAS_temp + UW0_mic(j1,i,ipix)*QSE_bias(i,ipix) !UW0(j1,i,ipix)*QS(i,ipix)
              enddo ! i

              ERR_temp = sqrt(ERR_temp*ALS(ipix))

              call set_mic_err (  IOF, IW, ISD, ipix,  & ! IN
                                  use_bias_eq, flag_bias, &
                                  ERR_temp, BIAS_temp, &
                                  GOUT%errest  & ! INOUT
                               )
              if ( error_present() ) return
           enddo ! IW
        enddo ! ISD
enddo LOOP_opt_funct

enddo loop_pixel

      return
      end subroutine MIC_ERR_estimates_single_pixel_scenario

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine OPT_ERR_estimates_deriv_matrix ( iu_main_output,      &
                                                  RIN, npixels,        &
                                                  ALS, GOUT,           &
                                                  AP, APMIN, APMAX,    &
                                                  dermask_aerpar,      &
                                                  tau_mol, NHVP_meas, HVP_meas, &
                                                  KERNELS1,KERNELS2,   &
                                                  NOF, NSD1, UW0,       &
                                                  use_bias_eq, ALS_temp)

      use mod_par_inv, only : KPARS, KIMAGE, KPAR, KW, KVERTM
      use mod_par_OS,  only : KSD
      use mod_par_DLS, only : KMpar
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type 
      use mod_alloc_kernels
      use mod_forward_model
      use mod_covariance_matrix, only : delta_derivatives
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings),         intent(in)  ::  RIN
      integer,                           intent(in)  ::  npixels
      integer,                           intent(in)  ::  iu_main_output
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      real,dimension(KPARS),             intent(in)  ::  APMIN, APMAX
      real,dimension(KW,KIMAGE),         intent(in)  ::  tau_mol
      integer,                           intent(in)  ::  NHVP_meas
      real,dimension(KVERTM,KIMAGE),     intent(in)  ::  HVP_meas  ! heights for lidar measurements
      logical, dimension(KPARS),         intent(in)  ::  dermask_aerpar
      integer,                           intent(in)  ::  NOF, NSD1
!	----------------------------------------------------------------------
! INOUT :
      real,dimension(KPARS,KIMAGE),       intent(inout)  ::  AP
      type(output_segment_general),      intent(inout)  ::  GOUT
      type(kernels_triangle_bin),        intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),       intent(inout)  ::  KERNELS2
      real,dimension(RIN%NW*NOF*NSD1,RIN%KNSINGF,npixels),intent(inout) ::  UW0
!	----------------------------------------------------------------------
! LOCAL :
      type(output_segment_particles)  ::  GOUT_aerosol_temp
      type(output_segment_gases)  ::  GOUT_gases_temp
      type(output_segment_surface)    ::  GOUT_surface_temp
      type(output_segment_retrieval)  ::  GOUT_retrieval_temp
      type(output_segment_products_particles)  ::  GOUT_products_aerosol_temp
      real,dimension(KPARS)		        ::  AP_temp
      real                            ::  DL1
      integer ::  IWW
      logical ::  lresult
      integer ::  alloc_stat
      integer ::  i, i1, j, j1, ii, ii1, IOF, ISD, IW, ipix
      integer ::  IWb, IWe
      integer            ::  NW, IMQ, KNSING, KNSINGF
      real,dimension(KW) ::  WAVE
      integer  ::  nnz_pix
      integer  ::  KN, KNF
! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
      integer :: NANG
      real,dimension(KMPAR) :: ANGL
      type(pixel) :: pixel_fit_temp
      type(pixel_vector) :: pixel_vec_fit_temp
      integer,dimension(KW) :: ind_wl, ind_wl_i
      real,dimension(KW)  ::   MDPR=0.0

! MEH 24/01/2020
!      real,dimension(RIN%NW*2*1,RIN%KNSINGF,npixels) ::  UW0_rind_aux
!      integer :: NOFRI
!      integer :: k
! MEH 27/01/2020
!      real :: dlnr, CVret_fine, CVret_coarse
!      real :: CVtemp_fine, CVtemp_coarse      
!---------------

       real, dimension(KIMAGE), intent(in)       :: ALS_temp
       logical :: use_bias_eq
      
! AL MDPR needed to run forward model,
! AL but since forward is using only single scattering properties
! AL it won't be used and initialised as dummy
!	----------------------------------------------------------------------
      lresult=.true.

      NW     = RIN%nw
      WAVE(:)= RIN%wave(:)
      KNSING = RIN%KNSING
      KNSINGF= RIN%KNSINGF
      IMQ    = RIN%IMQ
      do iw=1,NW
        ind_wl(iw) = iw
        ind_wl_i(iw) = iw
! AL ind_wl_i is not used for calculations, filled as dummy argument
! AL to correspond for forward model interface
      enddo

      UW0(:,:,:) = 0.0
      
! MEH 24/01/2020
!      UW0_rind_aux(:,:,:) = 0.0
!      NOFRI = 2
      
!MEH Temporarly prints
!OPEN(41, FILE = 'UW0_RRIEeff.txt')
!WRITE(41,*) '# UW0 for RI effective', NW, KNSINGF, ipix

!OPEN(42, FILE = 'UW0_IRIEeff.txt')
!WRITE(42,*) '# UW0 for RI effective', NW, KNSINGF, ipix
!------

      
!-------------------
loop_pixel: do ipix=1,npixels

      pixel_fit_temp = GOUT%retrieval%fit%segment_fit%pixels(ipix)
      AP_temp(1:RIN%KNSING) = AP(1:RIN%KNSING,ipix)

! MEH 27/01/2020
!      dlnr = log(RIN%radius1(2,1))- log(RIN%radius1(1,1))
!      CVret_fine = SUM(exp(AP(1:10,ipix)))*dlnr
!      CVret_coarse = SUM(exp(AP(11:25,ipix)))*dlnr
!      !WRITE(*,*) 'CVret_fine', CVret_fine
!      !WRITE(*,*) 'CVret_coarse', CVret_coarse
!------

      
LOOP_WL: do IW=1,NW
      IWb = IW
      IWe = IW
LOOP_parameters: do I=1,RIN%KNSINGF
      IWW = RIN%IWW_SINGL(I)
      if(IWW .NE. 0 .AND. IWW .NE. IW) cycle LOOP_parameters
      if(.not. dermask_aerpar(i)) cycle LOOP_parameters

    if (use_bias_eq) then
      DL1 = delta_derivatives(ALS_temp(ipix),APMIN(I),APMAX(I),AP(I,ipix),RIN%DL)
    else
      DL1 = delta_derivatives(ALS(ipix),APMIN(I),APMAX(I),AP(I,ipix),RIN%DL)
    endif

      AP_temp(I) = AP(I,ipix)+DL1

! MEH 27/01/2020
!      if(1 .GE. I .AND. I .LE. 10) then
!         CVtemp_fine = SUM(exp(AP_temp(1:10)))*dlnr
!         CVtemp_coarse = CVret_coarse
!      else if(11 .GE. I .AND. I .LE. 25) then
!         CVtemp_coarse = SUM(exp(AP_temp(11:25)))*dlnr
!      endif
!-------
      
      call forward_model_pixel(          &
              iu_main_output,            &
              RIN,                       &
              RIN%OSHD,                  &
              ipix,                      &
              IWb,                       &
              IWe,                       &
              IWW,                       &
              lresult,                   &
              tau_mol(:,ipix),           &
              NHVP_meas,                 &
              HVP_meas(:,ipix),          &
              NW,                        &
              WAVE,                      &
              ind_wl,                    &
              AP_temp,                   &
              pixel_fit_temp,            &
              pixel_vec_fit_temp,        &
              GOUT_aerosol_temp,         &
              GOUT_gases_temp,           &
              GOUT_surface_temp,         &
              GOUT_retrieval_temp,       &
              ind_wl_i,                  &
              MDPR,                      & ! molecular depolarization, added by Qiaoyun HU
              NANG,                      &
              ANGL,                      &
              KERNELS1,                  &
              KERNELS2                   &
      )


      call total_single_scattering_properties (                    &
                                  iu_main_output, ipix, NANG,      &
                                  RIN%DLSF%keyEL, RIN%NSD, RIN%products%aerosol, &
                                  IW, RIN%WAVE(IW),                &
                                  GOUT_aerosol_temp,               &
                                  GOUT_products_aerosol_temp       &
                                              )
      AP_temp(I) = AP(I,ipix)
      
! 2018-12-26
! routine set_UW0 outputs UW0 for ext, SCA, lr instead of ext, SSA, lr
      call set_UW0 ( IW,RIN%KL,                    & ! IN
                     NW,RIN%NSD,NOF,NSD1,          &
                     GOUT%aerosol%opt%pixel(ipix)%wl(IW),        &
                     GOUT%aerosol%lidar%pixel(ipix)%wl(IW),      &
                     GOUT_aerosol_temp%opt%pixel(ipix)%wl(IW),   &
                     GOUT_aerosol_temp%lidar%pixel(ipix)%wl(IW), &
                     DL1,                          &
                     UW0(:,I,ipix)                 & ! INOUT
                     )
      
      !WRITE(*,*) 'RRI ret', GOUT%aerosol%rind%pixel(ipix)%wl(IW)
      !WRITE(*,*) 'RRI temp', GOUT_aerosol_temp%rind%pixel(ipix)%wl(IW)
      
!! MEH 24/01/2020 --------------------
     ! call set_UW0_rind_aux ( IW,RIN%KL,                           & ! IN
     !                NW,RIN%NSD,NOFRI,NSD1,                      &
     !                GOUT%aerosol%rind%pixel(ipix)%wl(IW),     & 
     !                CVret_fine, CVret_coarse,                 &
     !                GOUT_aerosol_temp%rind%pixel(ipix)%wl(IW),& 
     !                CVtemp_fine, CVtemp_coarse,               & 
     !                DL1,                                      &
     !                UW0_rind_aux(:,I,ipix)                        & ! INOUT
     !                )         
!!------------------
      
   enddo LOOP_parameters ! I

!! MEH - Output file for UW0 RI effective   
   !WRITE(40,'(350e14.5)') UW0_rind(IW,1:KNSINGF,ipix)

   !do k = 1,2
   !   j = (k-1)*1*NW+(1-1)*NW+IW
   !   if(k .EQ. 1) then
   !      WRITE(41,'(350e14.5)') UW0_rind_aux(j,1:KNSINGF,ipix)
   !   else
   !      WRITE(42,'(350e14.5)') UW0_rind_aux(j,1:KNSINGF,ipix)
   !   endif
   !enddo
   
enddo LOOP_WL ! IW


!      write(*,*) 'ipix=',ipix,'  AP:'
!      write(*,'(10e14.5)') AP(1:KNSING,ipix)
!OPEN(39, FILE = 'UW0_prueba.txt')
!write(39, *) 'NW, NOF, NSD1, KNSINGF'
!write(39,*) NW, NOF, NSD1, KNSINGF
!write(39,*) '1: UW0'
!write(39,'(350e14.5)') UW0(1:NW*NOF*NSD1,1:KNSINGF,ipix)
!CLOSE(39)



enddo loop_pixel

!CLOSE(41)
!CLOSE(42)

! MEH ------
!OPEN(39, FILE = 'UW0_prueba.txt')
!write(39, *) 'NW, NOF, NSD1, KNSINGF'
!write(39,*) NW, NOF, NSD1, KNSINGF
!write(39,*) '1: UW0'
!write(39,'(350e14.5)') UW0(1:NW*NOF*NSD1,1:KNSINGF,ipix)
!CLOSE(39)
      return
      end subroutine OPT_ERR_estimates_deriv_matrix
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!! MEH: new subroutine to estimate UW0 for SD when it is not part of retrieval
      subroutine MIC_ERR_estimates_deriv_matrix ( iu_main_output,      &
                                                  RIN, npixels,        &
                                                  ALS, GOUT,           &
                                                  AP, APMIN, APMAX,    &
                                                  dermask_aerpar,      &
                                                  tau_mol, NHVP_meas, HVP_meas, &
                                                  KERNELS1,KERNELS2,   &
                                                  NOF, NSD1, UW0_mic,       &
                                                  use_bias_eq, ALS_temp)

      use mod_par_inv, only : KPARS, KIMAGE, KPAR, KW, KVERTM
      use mod_par_OS,  only : KSD
      use mod_par_DLS, only : KMpar
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_alloc_kernels
      use mod_forward_model
      use mod_covariance_matrix, only : delta_derivatives
      use mod_stop_report

      implicit none
!    ----------------------------------------------------------------------
! IN :
      type(retr_input_settings),         intent(in)  ::  RIN
      integer,                           intent(in)  ::  npixels
      integer,                           intent(in)  ::  iu_main_output
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      real,dimension(KPARS),             intent(in)  ::  APMIN, APMAX
      real,dimension(KW,KIMAGE),         intent(in)  ::  tau_mol
      integer,                           intent(in)  ::  NHVP_meas
      real,dimension(KVERTM,KIMAGE),     intent(in)  ::  HVP_meas  ! heights for lidar measurements
      logical, dimension(KPARS),         intent(in)  ::  dermask_aerpar
      integer,                           intent(in)  ::  NOF, NSD1
!    ----------------------------------------------------------------------
! INOUT :
      real,dimension(KPARS,KIMAGE),       intent(inout)  ::  AP
      type(output_segment_general),      intent(inout)  ::  GOUT
      type(kernels_triangle_bin),        intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),       intent(inout)  ::  KERNELS2
      real,dimension(RIN%NW*1*NSD1,RIN%KNSINGF,npixels),intent(inout) ::  UW0_mic
!    ----------------------------------------------------------------------
! LOCAL :
!MEH:
      type(output_segment_retr_par) :: GOUT_retrieval_par_temp


      type(output_segment_particles)  ::  GOUT_aerosol_temp
      type(output_segment_gases)  ::  GOUT_gases_temp
      type(output_segment_surface)    ::  GOUT_surface_temp
      type(output_segment_retrieval)  ::  GOUT_retrieval_temp
      type(output_segment_products_particles)  ::  GOUT_products_aerosol_temp
      real,dimension(KPARS)                ::  AP_temp
      real                            ::  DL1
      integer ::  IWW
      logical ::  lresult
      integer ::  alloc_stat
      integer ::  i, i1, j, j1, ii, ii1, IOF, ISD, IR, ipix
      integer ::  IWb, IWe
      integer            ::  NW, IMQ, KNSING, KNSINGF
      real,dimension(KW) ::  WAVE
      integer  ::  nnz_pix
      integer  ::  KN, KNF
! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
      integer :: NANG
      real,dimension(KMPAR) :: ANGL
      type(pixel) :: pixel_fit_temp
      type(pixel_vector) :: pixel_vec_fit_temp
      integer,dimension(KW) :: ind_wl, ind_wl_i
      real,dimension(KW)  ::   MDPR=0.0
!---------------
       real, dimension(KIMAGE), intent(in)       :: ALS_temp
       logical :: use_bias_eq


!MEH
       real,dimension(KIMAGE)               ::  out_sd, temp_sd
       real,dimension(KIDIM3,KIDIM2,KIMAGE) ::  CV, SD_temp, CV_temp
       real,dimension(KNpar,KIDIM2,KIMAGE) :: SDout
       integer                              ::  idim, IDIM1,IDIM2, par_type, i_idim2, i_idim3,k, radSD
       logical                              ::  icv
! AL MDPR needed to run forward model,
! AL but since forward is using only single scattering properties
! AL it won't be used and initialised as dummy
!    ----------------------------------------------------------------------
      lresult=.true.

      NW     = RIN%nw
      WAVE(:)= RIN%wave(:)
      KNSING = RIN%KNSING
      KNSINGF= RIN%KNSINGF
      IMQ    = RIN%IMQ

      radSD = GOUT%retrieval%information%ngrid(1)

      UW0_mic(:,:,:) = 0.0
      
!! MEH Temporarly prints
!OPEN(51, FILE = 'UW0_SD1.txt')
!WRITE(51,*) '# UW0 for SD: 1', radSD, KNSINGF
!
!OPEN(52, FILE = 'UW0_SD2.txt')
!WRITE(52,*) '# UW0 for SD: 2', radSD, KNSINGF
!
!OPEN(53, FILE = 'UW0_SD_tot.txt')
!WRITE(53,*) '# UW0 for SD: tot', radSD, KNSINGF
!------

!MEH: Implementation for SD when it is not part of the retrieval
! Here we only take the values for the sd

    icv = .false.
    out_sd(:) = 0.0
    do IDIM1=1,RIN%NDIM%n1
      par_type = RIN%NDIM%par_type(IDIM1)
      if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
        ! if Aerosol Concentration retreived
        call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,CV )
        icv = .true.
      exit
      endif ! RIN%NDIM%par_type(i) .gt. par_type_SD_beg .and.
    enddo ! IDIM1

!-------------------
loop_pixel: do ipix=1,npixels

      pixel_fit_temp = GOUT%retrieval%fit%segment_fit%pixels(ipix)
      AP_temp(1:RIN%KNSING) = AP(1:RIN%KNSING,ipix)


LOOP_RAD: do IR=1,radSD  !cambiar por radios aca

LOOP_parameters: do I=1,RIN%KNSINGF

    if (use_bias_eq) then
      DL1 = delta_derivatives(ALS_temp(ipix),APMIN(I),APMAX(I),AP(I,ipix),RIN%DL)
    else
      DL1 = delta_derivatives(ALS(ipix),APMIN(I),APMAX(I),AP(I,ipix),RIN%DL)
    endif

      AP_temp(I) = AP(I,ipix)+DL1

    ! MEH: We redifine the vector to calculate the SD_temp (we should take all the values
    ! and only change the perturbated value.
    GOUT_retrieval_par_temp%pixel(ipix) = GOUT%retrieval%par%pixel(ipix)
    GOUT_retrieval_par_temp%pixel(ipix)%par(I) = EXP(AP_temp(I))


    !!! MEH:
    !!!!!! Calculamos el SD_temp para las derivadas
    do IDIM1=1,RIN%NDIM%n1
      par_type = RIN%NDIM%par_type(IDIM1)
      if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then
       select case(par_type)
        case(par_type_SD_LN)
        do i_idim2=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(i_idim2)
          if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
            call unpack_retr_param_to_print ( RIN,GOUT_retrieval_par_temp,i_idim2,npixels,CV_temp )
          exit
          endif ! par_type .gt. par_type_Cv_beg .and.
        enddo ! i_idim2

        call unpack_retr_param_to_print ( RIN,GOUT_retrieval_par_temp,IDIM1,npixels,SD_temp )
        !WRITE(*,*) 'SD_temp(1,IDIM2,1:1)',SD_temp(1,RIN%NDIM%n2(RIN%NDIM%n1),1:1)
        call SD_lognormal ( RIN,GOUT%retrieval%information,ipix,IDIM1,SD_temp,CV_temp,SDout )

        GOUT_retrieval_par_temp%pixel(ipix)%sd(:,0) = 0.0
        do IDIM2=1,RIN%NDIM%n2(IDIM1)  ! particle component loop
        do i_idim3=1,GOUT%retrieval%information%ngrid(IDIM2)
            GOUT_retrieval_par_temp%pixel(ipix)%sd(i_idim3,IDIM2) = SDout(i_idim3,IDIM2,ipix)
            GOUT_retrieval_par_temp%pixel(ipix)%sd(i_idim3,0) = &
            GOUT_retrieval_par_temp%pixel(ipix)%sd(i_idim3,0) +     SDout(i_idim3,IDIM2,ipix)
        enddo ! i_idim2
        enddo ! IDIM2
        end select
        exit
       endif ! par_type .gt. par_type_SD_beg .and.
    enddo ! IDIM1

      AP_temp(I) = AP(I,ipix)
      
!! MEH
        call set_UW0_SD_LN ( IR,RIN%KL, ipix,                & ! IN !maybe I dont need the wavelength
                                 radSD,RIN%NSD,NOF,NSD1, RIN,             &
                                 GOUT%retrieval%par,         &
                                 CV,                               &
                                 GOUT_retrieval_par_temp,    &
                                 CV_temp,                          &
                                 DL1,                              &
                                 UW0_mic(:,I,ipix)                               & ! INOUT
                               )


   enddo LOOP_parameters ! I

!! MEH - Output file for UW0 of SD
!    do k = 1,3
!       j = (k-1)*radSD+IW
!       if(k .EQ. 1) then
!          WRITE(51,'(350e14.5)') UW0(j,1:KNSINGF,ipix)
!       else if(k .EQ. 2) then
!          WRITE(52,'(350e14.5)') UW0(j,1:KNSINGF,ipix)
!       else
!          WRITE(53,'(350e14.5)') UW0(j,1:KNSINGF,ipix)
!       endif
!    enddo

enddo LOOP_RAD ! IR


enddo loop_pixel

!CLOSE(51)
!CLOSE(52)
!CLOSE(53)


      return
      end subroutine MIC_ERR_estimates_deriv_matrix

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! 2018-12-26
! routine outputs UW0 for ext, SCA, lr instead of ext, SSA, lr
      subroutine set_UW0 ( IW,KL,                & ! IN
                           NW,NSD,NOF,NSD1,      & 
                           GOUT_aerosol_opt_pixel_wl,         &
                           GOUT_aerosol_lidar_pixel_wl,       &
                           GOUT_aerosol_opt_pixel_wl_temp,    &
                           GOUT_aerosol_lidar_pixel_wl_temp,  &
                           DL1,                  &
                           UW0                   & ! INOUT
                         )

      use mod_retr_general_output_derived_type

      implicit none
!	----------------------------------------------------------------------
! IN :
      integer,                          intent(in)  ::  IW, KL, NW
      integer,                          intent(in)  ::  NSD, NOF, NSD1
      real,                             intent(in)  ::  DL1
      type(output_pixel_opt_wl),        intent(in)  ::  GOUT_aerosol_opt_pixel_wl
      type(output_pixel_opt_wl),        intent(in)  ::  GOUT_aerosol_opt_pixel_wl_temp
      type(output_pixel_lidar_ratio_wl),intent(in)  ::  GOUT_aerosol_lidar_pixel_wl
      type(output_pixel_lidar_ratio_wl),intent(in)  ::  GOUT_aerosol_lidar_pixel_wl_temp     
!	----------------------------------------------------------------------
! INOUT :
      real,dimension(NW*NOF*NSD1), intent(inout)  ::  UW0
!	----------------------------------------------------------------------
! LOCAL : 
      integer  ::  J, ISD, IOF

! Error estimates: indexes for UW0 matrix 
!      integer,parameter	::	index_ext = 1
!      integer,parameter	::	index_ssa = 2 ! works for sca
!      integer,parameter	::	index_lr  = 3
!MEH: for AAOD (I changed the value of index_lr)
       !integer,parameter    ::    index_aext =3
       !integer,parameter    ::    index_lr  = 4

! NOF  - number of optical functions for error estimates
! NSD1 - number of SD mode + 1(total) 
!	----------------------------------------------------------------------

      
      do IOF=1,NOF

         do ISD=1,NSD
         J=(IOF-1)*NSD1*NW+(ISD-1)*NW+IW
         select case(IOF)
         case(index_ext) 
            if(KL .eq. 1) then
               UW0(J) = (log(GOUT_aerosol_opt_pixel_wl%ext(ISD))-log(GOUT_aerosol_opt_pixel_wl_temp%ext(ISD)))/DL1
            else
               UW0(J) = (GOUT_aerosol_opt_pixel_wl%ext(ISD)-GOUT_aerosol_opt_pixel_wl_temp%ext(ISD))/DL1            
            endif
         case(index_ssa) ! sca to compute err estimetes for ssa
            if(KL .eq. 1) then
               UW0(J) = ( log(GOUT_aerosol_opt_pixel_wl%ssa(ISD) * &
                              GOUT_aerosol_opt_pixel_wl%ext(ISD)) - &
                          log(GOUT_aerosol_opt_pixel_wl_temp%ssa(ISD) * &
                              GOUT_aerosol_opt_pixel_wl_temp%ext(ISD)) )/DL1
            else
               UW0(J) = ( GOUT_aerosol_opt_pixel_wl%ssa(ISD) * &
                          GOUT_aerosol_opt_pixel_wl%ext(ISD) - &
                          GOUT_aerosol_opt_pixel_wl_temp%ssa(ISD) * &
                          GOUT_aerosol_opt_pixel_wl_temp%ext(ISD) )/DL1
            endif
         case(index_lr)
            if(KL .eq. 1) then
               UW0(J) = (log(GOUT_aerosol_lidar_pixel_wl%lr(ISD))-log(GOUT_aerosol_lidar_pixel_wl_temp%lr(ISD)))/DL1
            else
               UW0(J) = (GOUT_aerosol_lidar_pixel_wl%lr(ISD)-GOUT_aerosol_lidar_pixel_wl_temp%lr(ISD))/DL1            
            endif
        case(index_aext)
           if(KL .eq. 1) then
              UW0(J) = (log(GOUT_aerosol_opt_pixel_wl%aext(ISD))-log(GOUT_aerosol_opt_pixel_wl_temp%aext(ISD)))/DL1
           else
              UW0(J) = (GOUT_aerosol_opt_pixel_wl%aext(ISD)-GOUT_aerosol_opt_pixel_wl_temp%aext(ISD))/DL1
           endif

        end select
         enddo ! ISD
                  
         if(NSD1 .eq. 3) then
         J=(IOF-1)*NSD1*NW+(NSD1-1)*NW+IW

         select case(IOF)
         case(index_ext) 
            if(KL .eq. 1) then
               UW0(J) = ( log(GOUT_aerosol_opt_pixel_wl%extt)-log(GOUT_aerosol_opt_pixel_wl_temp%extt))/DL1
            else
               UW0(J) = (GOUT_aerosol_opt_pixel_wl%extt-GOUT_aerosol_opt_pixel_wl_temp%extt)/DL1
            endif
         case(index_ssa) ! sca to compute err estimetes for ssa
            if(KL .eq. 1) then
               UW0(J) = ( log(GOUT_aerosol_opt_pixel_wl%ssat * &
                              GOUT_aerosol_opt_pixel_wl%extt) - &
                          log(GOUT_aerosol_opt_pixel_wl_temp%ssat * &
                              GOUT_aerosol_opt_pixel_wl_temp%extt) )/DL1
            else
               UW0(J) = ( GOUT_aerosol_opt_pixel_wl%ssat * &
                          GOUT_aerosol_opt_pixel_wl%extt - &
                          GOUT_aerosol_opt_pixel_wl_temp%ssat * &
                          GOUT_aerosol_opt_pixel_wl_temp%extt )/DL1
            endif
         case(index_lr)
            if(KL .eq. 1) then
               UW0(J) = (log(GOUT_aerosol_lidar_pixel_wl%lrt)-log(GOUT_aerosol_lidar_pixel_wl_temp%lrt))/DL1 
            else
               UW0(J) = (GOUT_aerosol_lidar_pixel_wl%lrt-GOUT_aerosol_lidar_pixel_wl_temp%lrt)/DL1
            endif        
        case(index_aext)
           if(KL .eq. 1) then
              UW0(J) = (log(GOUT_aerosol_opt_pixel_wl%aextt)-log(GOUT_aerosol_opt_pixel_wl_temp%aextt))/DL1
           else
              UW0(J) = (GOUT_aerosol_opt_pixel_wl%aextt-GOUT_aerosol_opt_pixel_wl_temp%aextt)/DL1
           endif
        end select
         endif !  NSD1 .eq. 3

      enddo ! IOF

      return
      end subroutine set_UW0
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! MEH 2020/01/24
! routine outputs UW0_rind for RRI_eff and IRI_eff
      subroutine set_UW0_rind_aux ( IW,KL,                & ! IN
                           NW,NSD,NOFRI,NSD1,      & 
                           GOUT_aerosol_rindex_pixel_wl,         &
                           CVret_fine, CVret_coarse,             &
                           GOUT_aerosol_rindex_pixel_wl_temp,    &
                           CVtemp_fine, CVtemp_coarse,           &
                           DL1,                  &
                           UW0_rind_aux                   & ! INOUT
                         )

      use mod_retr_general_output_derived_type

      implicit none
!	----------------------------------------------------------------------
! IN :
      integer,                          intent(in)  ::  IW, KL, NW
      integer,                          intent(in)  ::  NSD, NOFRI, NSD1
      real,                             intent(in)  ::  DL1
      type(output_pixel_rindex_wl),     intent(in)  ::  GOUT_aerosol_rindex_pixel_wl
      type(output_pixel_rindex_wl),     intent(in)  ::  GOUT_aerosol_rindex_pixel_wl_temp
      real,                             intent(in)  ::  CVret_fine, CVret_coarse
      real,                             intent(in)  ::  CVtemp_fine, CVtemp_coarse
!	----------------------------------------------------------------------
! INOUT :
      real,dimension(NW*2*1), intent(inout)  ::  UW0_rind_aux  !I need to re-define the dimension! 
!	----------------------------------------------------------------------
! LOCAL : 
      integer  ::  J, ISD, IOF
      
! Error estimates: indexes for UW0_rind_aux matrix 
      integer,parameter	::	index_rrieff = 1
      integer,parameter	::	index_irieff = 2

! NOF  - number of optical functions for error estimates
! NSD1 - number of SD mode + 1(total) 
!	----------------------------------------------------------------------

      
      
     do IOF= 1,NOFRI
        J=(IOF-1)*1*NW+(1-1)*NW+IW

        select case(IOF)
           case(index_rrieff)
              if(KL .eq. 1) then
                 UW0_rind_aux(J) = (log(((GOUT_aerosol_rindex_pixel_wl%mreal(1) * CVret_fine) + &  
                      (GOUT_aerosol_rindex_pixel_wl%mreal(2) * CVret_coarse)) /  &             
                      (CVret_fine + CVret_coarse)) - &
                      log(((GOUT_aerosol_rindex_pixel_wl_temp%mreal(1) * CVtemp_fine) + &
                      (GOUT_aerosol_rindex_pixel_wl_temp%mreal(2) * CVtemp_coarse)) /  &
                      (CVtemp_fine + CVtemp_coarse))) / DL1
              else
                 UW0_rind_aux(J) = (((GOUT_aerosol_rindex_pixel_wl%mreal(1) * CVret_fine) + &
                      (GOUT_aerosol_rindex_pixel_wl%mreal(2) * CVret_coarse)) /  &
                      (CVret_fine + CVret_coarse) - &
                      ((GOUT_aerosol_rindex_pixel_wl_temp%mreal(1) * CVtemp_fine) + &
                      (GOUT_aerosol_rindex_pixel_wl_temp%mreal(2) * CVtemp_coarse)) /  &
                      (CVtemp_fine + CVtemp_coarse)) / DL1
              endif
           case(index_irieff)
              if(KL .eq. 1) then
                 UW0_rind_aux(J) = (log(((GOUT_aerosol_rindex_pixel_wl%mimag(1) * CVret_fine) + &  
                      (GOUT_aerosol_rindex_pixel_wl%mimag(2) * CVret_coarse)) /  &             
                      (CVret_fine + CVret_coarse)) - &
                      log(((GOUT_aerosol_rindex_pixel_wl_temp%mimag(1) * CVtemp_fine) + &
                      (GOUT_aerosol_rindex_pixel_wl_temp%mimag(2) * CVtemp_coarse)) /  &
                      (CVtemp_fine + CVtemp_coarse))) / DL1
              else
                 UW0_rind_aux(J) = (((GOUT_aerosol_rindex_pixel_wl%mimag(1) * CVret_fine) + &
                      (GOUT_aerosol_rindex_pixel_wl%mimag(2) * CVret_coarse)) /  &
                      (CVret_fine + CVret_coarse) - &
                      ((GOUT_aerosol_rindex_pixel_wl_temp%mimag(1) * CVtemp_fine) + &
                      (GOUT_aerosol_rindex_pixel_wl_temp%mimag(2) * CVtemp_coarse)) /  &
                      (CVtemp_fine + CVtemp_coarse)) / DL1
              endif      
           end select
        enddo
       
return
end subroutine set_UW0_rind_aux
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!! MEH:
! routine to calculate UW0 for SD when it is not part of retrieved parameters
      subroutine set_UW0_SD_LN ( IW,KL, ipix,                & ! IN !maybe I dont need the wavelength
                           radSD,NSD,NOF,NSD1, RIN,             &
                           GOUT_retrieval_par,         &
                           CV,                               &
                           GOUT_retrieval_par_temp,    &
                           CV_temp,                          &
                           DL1,                              &
                           UW0_mic                               & ! INOUT
                         )

      use mod_retr_general_output_derived_type

      implicit none
!    ----------------------------------------------------------------------
! IN :
      integer,                          intent(in)  ::  IW, KL, radSD, ipix
      integer,                          intent(in)  ::  NSD, NOF, NSD1
      real,                             intent(in)  ::  DL1
      type(output_segment_retr_par),    intent(in)  :: GOUT_retrieval_par
      type(output_segment_retr_par),    intent(in)  :: GOUT_retrieval_par_temp

      real,dimension(KIDIM3,KIDIM2,KIMAGE),intent(in) ::  CV, CV_temp
      type(retr_input_settings),         intent(in)  ::  RIN
!    ----------------------------------------------------------------------
! INOUT :
      real,dimension(radSD*1*3), intent(inout)  ::  UW0_mic
!    ----------------------------------------------------------------------
! LOCAL :
      integer  ::  J, ISD, IOF, rad


! Error estimates: indexes for UW0 matrix
      integer,parameter    ::    index_SD_LN = 1


! NOF  - number of optical functions for error estimates
! NSD1 - number of SD mode + 1(total)
!    ----------------------------------------------------------------------


      do IOF=1,1
!! remove the commented lines:
!         do ISD=1,NSD !
!         !J=(ISD-1)*1+(1-1)*rad+IW
!         J=(IOF-1)*NSD1*radSD+(ISD-1)*radSD+IW
!         !write(*,*) 'J', J
!         select case(IOF)
!         case(index_SD_LN)
!            if(KL .eq. 1) then
!               !UW0(J) = (log(GOUT_retrieval_par%pixel(ipix)%sd(IW,ISD)/CV(1,ISD,ipix))-log(GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,ISD)/CV_temp(1,ISD,ipix)))/DL1
!               UW0(J) = (log(GOUT_retrieval_par%pixel(ipix)%sd(IW,ISD))-log(GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,ISD)))/DL1
!            else
!               !UW0(J) = (GOUT_retrieval_par%pixel(ipix)%sd(IW,ISD)/CV(1,ISD,ipix)-GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,ISD)/CV_temp(1,ISD,ipix))/DL1
!               UW0(J) = (GOUT_retrieval_par%pixel(ipix)%sd(IW,ISD)-GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,ISD))/DL1
!            endif
!         end select
!         enddo ! ISD

        if(NSD1 .eq. 3) then
        J=(IOF-1)*NSD1*radSD+(NSD1-1)*radSD+IW

        select case(IOF)
        case(index_SD_LN)
           if(KL .eq. 1) then
!             UW0(J) = (log(GOUT_retrieval_par%pixel(ipix)%sd(IW,1)/CV(1,1,ipix)+GOUT_retrieval_par%pixel(ipix)%sd(IW,2)/CV(1,2,ipix))-log(GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,1)/CV_temp(1,1,ipix)+GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,2)/CV_temp(1,2,ipix)))/DL1
            UW0_mic(J) = (log(GOUT_retrieval_par%pixel(ipix)%sd(IW,1)+GOUT_retrieval_par%pixel(ipix)%sd(IW,2))-log(GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,1)+GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,2)))/DL1
           else
             UW0_mic(J) = ((GOUT_retrieval_par%pixel(ipix)%sd(IW,1)+GOUT_retrieval_par%pixel(ipix)%sd(IW,2))-(GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,1)+GOUT_retrieval_par_temp%pixel(ipix)%sd(IW,2)))/DL1
           endif
        end select
        endif
      enddo ! IOF

      return
      end subroutine set_UW0_SD_LN

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!! MEH: routine to calculate the contribution of total, random and bias for SD (when it is not part of the retrieval)
    subroutine set_mic_err ( IOF, IW, ISD, ipix,   & ! IN
                            use_bias_eq, flag_bias, &
                            ERR, BIAS,           &
                            GOUT_errest_mic  & ! INOUT
                           )
    use mod_retr_general_output_derived_type
    use mod_stop_report

    implicit none
!    ----------------------------------------------------------------------------------
! IN :
    integer, intent(in) :: IOF, IW, ISD, ipix
    real, intent(in) :: ERR, BIAS
    logical, intent(in) :: use_bias_eq
    integer, intent(in) :: flag_bias
!    ----------------------------------------------------------------------------------
! INOUT :
    type(output_segment_err_estim), intent(inout)  ::  GOUT_errest_mic

!    ----------------------------------------------------------------------------------
! LOCAL :
    real :: ERR_out, BIAS_out, TSTD_out

! Error estimates: indexes for UW0 matrix
      integer,parameter    ::    index_SD_LN = 1
!    ----------------------------------------------------------------------------------
! Error estimates (IOF) : indexes for UW0 matrix
! NOF  - number of optical functions for error estimates

!
! in case use_bias_eq=.false. the subroutine is called once with flag_bias=bias_basic
! in case use_bias_eq=.true.  the subroutine is called 3 times in the following order:
! 1st - flag_bias=bias_basic, 2nd - flag_bias=bias_pos, 3rd - flag_bias=bias_neg
!    ----------------------------------------------------------------------------------

    if(ISD .eq. 1 .or. ISD .eq. 2) then
        !write(*,*) 'I dont save the results for the moment: ver si lo que sigue hay que dejar o no! como afecta las salidas!?'

        if (ISD .eq. 1) then
        ! assign *_extt, *_ssat, *_aext, *_lrt with values for ISD=1
        ! if ISD=3, assigned *_extt, *_ssat, *_lrt will be replaced with total values
          select case(IOF)
          case(index_SD_LN)
            ERR_out  = GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%ERR_sdt
            BIAS_out = GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%BIAS_sdt
            TSTD_out = GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%TSTD_sdt
            call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                         ERR, BIAS,                   &
                                         ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                       )
            GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%ERR_sdt  = ERR_out
            GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%BIAS_sdt = BIAS_out
            GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%TSTD_sdt = TSTD_out
          case default
            write(tmp_message,'(a,i0,a)') &
            'Index of Optical Function (IOF) = ',IOF,' value is not valid'
            G_ERROR(trim(tmp_message))
          end select
       endif

    elseif(ISD .eq. 3) then

       select case(IOF)
       case(index_SD_LN)
            ERR_out  = GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%ERR_sdt
            BIAS_out = GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%BIAS_sdt
            TSTD_out = GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%TSTD_sdt
            call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                         ERR, BIAS,                   &
                                         ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                       )
            GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%ERR_sdt  = ERR_out
            GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%BIAS_sdt = BIAS_out
            GOUT_errest_mic%aerosol%mic%pixel(ipix)%ngrid(IW)%TSTD_sdt = TSTD_out

       case default
          write(tmp_message,'(a,i0,a)') &
          'Index of Optical Function (IOF) = ',IOF,' value is not valid'
          G_ERROR(trim(tmp_message))
       end select

    else

       write(tmp_message,'(a,i0,a)') 'ISD = ',ISD,' value is not valid'
       G_ERROR(trim(tmp_message))

endif ! ISD .lt. 3
                  
return
end subroutine set_mic_err

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
    subroutine set_opt_err ( IOF, IW, ISD, ipix,   & ! IN
                              use_bias_eq, flag_bias, &
                              ERR, BIAS,           &
                              GOUT_errest_aerosol  & ! INOUT
                             )
      use mod_retr_general_output_derived_type
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------------------
! IN :
      integer, intent(in) :: IOF, IW, ISD, ipix
      real, intent(in) :: ERR, BIAS
      logical, intent(in) :: use_bias_eq
      integer, intent(in) :: flag_bias
!	----------------------------------------------------------------------------------
! INOUT :
      type(output_segment_err_estim_particles), intent(inout)  ::  GOUT_errest_aerosol

!	----------------------------------------------------------------------------------
! LOCAL :
      real :: ERR_out, BIAS_out, TSTD_out
!	----------------------------------------------------------------------------------
! Error estimates (IOF) : indexes for UW0 matrix
! NOF  - number of optical functions for error estimates
!
! in mod_retr_general_output_derived_type:
!      integer,parameter	::	index_ext = 1
!      integer,parameter	::	index_ssa = 2
!      integer,parameter	::	index_aext = 3
!      integer,parameter    ::  index_lr  = 4
!
! in case use_bias_eq=.false. the subroutine is called once with flag_bias=bias_basic
! in case use_bias_eq=.true.  the subroutine is called 3 times in the following order:
! 1st - flag_bias=bias_basic, 2nd - flag_bias=bias_pos, 3rd - flag_bias=bias_neg
!	----------------------------------------------------------------------------------

      if(ISD .eq. 1 .or. ISD .eq. 2) then

         select case(IOF)
         case(index_ext)
            ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_ext(ISD)
            BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_ext(ISD)
            TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_ext(ISD)
            call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                         ERR, BIAS,                   &
                                         ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                       )
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_ext(ISD)  = ERR_out
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_ext(ISD) = BIAS_out
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_ext(ISD) = TSTD_out
         case(index_ssa)
            ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_ssa(ISD)
            BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_ssa(ISD)
            TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_ssa(ISD)
            call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                         ERR, BIAS,                   &
                                         ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                       )
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_ssa(ISD)  = ERR_out
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_ssa(ISD) = BIAS_out
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_ssa(ISD) = TSTD_out
        case(index_aext)
           ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_aext(ISD)
           BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_aext(ISD)
           TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_aext(ISD)
           call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                        ERR, BIAS,                   &
                                        ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                      )
           GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_aext(ISD)  = ERR_out
           GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_aext(ISD) = BIAS_out
           GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_aext(ISD) = TSTD_out
         case(index_lr)
            ERR_out  = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%ERR_lr(ISD)
            BIAS_out = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%BIAS_lr(ISD)
            TSTD_out = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%TSTD_lr(ISD)
            call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                         ERR, BIAS,                   &
                                         ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                       )
            GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%ERR_lr(ISD)  = ERR_out
            GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%BIAS_lr(ISD) = BIAS_out
            GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%TSTD_lr(ISD) = TSTD_out
         case default
            write(tmp_message,'(a,i0,a)') &
            'Index of Optical Function (IOF) = ',IOF,' value is not valid'
            G_ERROR(trim(tmp_message))
         end select
         if (ISD .eq. 1) then
         ! assign *_extt, *_ssat, *_lrt with values for ISD=1
         ! if ISD=3, assigned *_extt, *_ssat, *_lrt will be replaced with total values
            select case(IOF)
            case(index_ext)
              ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_extt
              BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_extt
              TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_extt
              call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                           ERR, BIAS,                   &
                                           ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                         )
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_extt  = ERR_out
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_extt = BIAS_out
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_extt = TSTD_out
            case(index_ssa)
              ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_ssat
              BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_ssat
              TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_ssat
              call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                           ERR, BIAS,                   &
                                           ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                         )
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_ssat  = ERR_out
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_ssat = BIAS_out
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_ssat = TSTD_out
            case(index_aext)
              ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_aextt
              BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_aextt
              TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_aextt
              call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                           ERR, BIAS,                   &
                                           ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                         )
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_aextt  = ERR_out
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_aextt = BIAS_out
              GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_aextt = TSTD_out
            case(index_lr)
              ERR_out  = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%ERR_lrt
              BIAS_out = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%BIAS_lrt
              TSTD_out = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%TSTD_lrt
              call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                           ERR, BIAS,                   &
                                           ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                         )
              GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%ERR_lrt  = ERR_out
              GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%BIAS_lrt = BIAS_out
              GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%TSTD_lrt = TSTD_out
            case default
              write(tmp_message,'(a,i0,a)') &
              'Index of Optical Function (IOF) = ',IOF,' value is not valid'
              G_ERROR(trim(tmp_message))
            end select
         endif

      elseif(ISD .eq. 3) then
 
         select case(IOF)
         case(index_ext)
            ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_extt
            BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_extt
            TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_extt
            call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                         ERR, BIAS,                   &
                                         ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                        )
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_extt  = ERR_out
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_extt = BIAS_out
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_extt = TSTD_out
         case(index_ssa)
            ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_ssat
            BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_ssat
            TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_ssat
            call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                         ERR, BIAS,                   &
                                         ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                        )
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_ssat  = ERR_out
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_ssat = BIAS_out
            GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_ssat = TSTD_out
        case(index_aext)
           ERR_out  = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_aextt
           BIAS_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_aextt
           TSTD_out = GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_aextt
           call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                        ERR, BIAS,                   &
                                        ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                       )
           GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%ERR_aextt  = ERR_out
           GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%BIAS_aextt = BIAS_out
           GOUT_errest_aerosol%opt%pixel(ipix)%wl(IW)%TSTD_aextt = TSTD_out
         case(index_lr)
            ERR_out  = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%ERR_lrt
            BIAS_out = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%BIAS_lrt
            TSTD_out = GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%TSTD_lrt
            call calculate_err_and_bias ( use_bias_eq, flag_bias,      & ! IN
                                         ERR, BIAS,                   &
                                         ERR_out, BIAS_out, TSTD_out  & ! INOUT
                                        )
            GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%ERR_lrt  = ERR_out
            GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%BIAS_lrt = BIAS_out
            GOUT_errest_aerosol%lidar%pixel(ipix)%wl(IW)%TSTD_lrt = TSTD_out
         case default
            write(tmp_message,'(a,i0,a)') &
            'Index of Optical Function (IOF) = ',IOF,' value is not valid'
            G_ERROR(trim(tmp_message))
         end select
      
      else

         write(tmp_message,'(a,i0,a)') 'ISD = ',ISD,' value is not valid'
         G_ERROR(trim(tmp_message))

      endif ! ISD .lt. 3
                        
      return
      end subroutine set_opt_err

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine calculate_err_and_bias (                            &
                                        use_bias_eq, flag_bias,     & ! IN
                                        ERR, BIAS,                  &
                                        ERR_out, BIAS_out, TSTD_out & ! INOUT
                                       )
      use mod_retr_general_output_derived_type, only : bias_basic, &
                                                       bias_pos,   &
                                                       bias_neg
      implicit none
!	----------------------------------------------------------------------------------
! IN :
      logical, intent(in) :: use_bias_eq
      integer, intent(in) :: flag_bias
      real, intent(in) :: ERR, BIAS
!	----------------------------------------------------------------------------------
! INOUT :
      real, intent(inout) :: ERR_out, BIAS_out, TSTD_out
!	----------------------------------------------------------------------------------
! LOCAL :
      real :: BIAS2
!	----------------------------------------------------------------------------------
!
! in mod_retr_general_output_derived_type:
!
!      integer,parameter	::	bias_basic = 0
!      integer,parameter	::	bias_pos = 1
!      integer,parameter	::	bias_neg = 2 
!
! in case use_bias_eq=.false. the subroutine is called once with flag_bias=bias_basic
! in case use_bias_eq=.true.  the subroutine is called 3 times in the following order:
! 1st - flag_bias=bias_basic, 2nd - flag_bias=bias_pos, 3rd - flag_bias=bias_neg
!	----------------------------------------------------------------------------------
      if ( flag_bias .eq. bias_basic ) then
      ERR_out  = 0.0
      BIAS_out = 0.0
      TSTD_out = 0.0
      endif

      BIAS2 = BIAS * BIAS

      if ( use_bias_eq ) then
      if ( flag_bias .eq. bias_pos .or. flag_bias .eq. bias_neg ) then
      BIAS2 = 0.5 * BIAS2
      endif
      endif

      if ( flag_bias .eq. bias_basic ) then
      ERR_out = ERR
      endif

      BIAS_out = BIAS_out + BIAS2

      if ( .not. use_bias_eq .or. (use_bias_eq .and. flag_bias .eq. bias_neg) ) then
      TSTD_out = sqrt(ERR*ERR + BIAS_out)
      BIAS_out = sqrt(BIAS_out)
      endif

      return
      end subroutine calculate_err_and_bias

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine PAR_ERR_estimates_single_pixel_scenario_indep_par (   &
                                                iu_main_output,        & ! IN
                                                sparse_solver_name,    &
                                                RIN, npixels, MASL,    &
                                                UFS, QS, ALS, AP,      &
                                                NHVP_meas, HVP_meas,   &
                                                errest_pix,            &
                                                UFNZ, GOUT_errest_par, & ! INOUT
                                                solver_timer           &
                                                         )

      use mod_par_inv, only : KPARS, KIMAGE, KPAR, KVERTM
      use mod_par_OS, only : HMAX_atm
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_globals, only : sp, dp
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings), intent(in)  ::  RIN
      integer,                   intent(in)  ::  npixels
      integer,                   intent(in)  ::  iu_main_output
      character(*),              intent(in)  ::  sparse_solver_name
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      real,dimension(KPARS,KIMAGE),      intent(in)  ::  QS
      real,dimension(KPARS,KPARS,KIMAGE),intent(in)  ::  UFS
      logical, dimension(KIMAGE),        intent(in)  ::  errest_pix
      real,dimension(KPARS,KIMAGE),  intent(in)  ::  AP
      integer,                      intent(in)  ::  NHVP_meas
      real,dimension(KVERTM,KIMAGE), intent(in)  ::  HVP_meas  ! heights for lidar measurements
      real,dimension(KIMAGE),        intent(in)  ::  MASL
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      type(nonzero),                     intent(inout)  ::  UFNZ    
      type(output_segment_err_estim_par),intent(inout)  ::  GOUT_errest_par		
!	----------------------------------------------------------------------
! LOCAL :
      logical                              ::  IPRI_additional_info
      integer                              ::  KNSINGF
      integer                              ::  IMQ, npix
      real, dimension(KPARS)               ::  FFS, QS1
      integer                              ::  nnz_pix
      real*8, dimension(KPAR)              ::  b
      integer                              ::  i, i1, j, k, ipix
      logical, dimension(KPARS)            ::  par_errest_mask
      logical, dimension(KPARS)            ::  par_errest_mask_plus
      real (sp), dimension(KPARS,KPARS) :: U
      real (sp), dimension(KPARS+1) :: x, y
      integer :: n, n1
      integer :: par_type, deriv_type
      real ::  ERR_temp, BIAS_temp
      integer :: num_plus, idim1_plus(3),par_type_plus(3)
      logical, dimension(KPARS)  :: KNSINGF_mask
      integer, dimension(KPARS)  :: iplus
      integer  ::  ibeg, iend, idim1, idim2
!	----------------------------------------------------------------------
      KNSINGF = RIN%KNSINGF
      IMQ = RIN%IMQ
      IPRI_additional_info = RIN%IPRI_additional_info
      par_errest_mask(1:KNSINGF) = RIN%APSERREST(1:KNSINGF)
      if ( RIN%IPRI_verbose ) then
        if ( any( .not. par_errest_mask(1:KNSINGF)) ) then
          write(iu_main_output,'(a,/,a,/,a)') &
          'WARNING in PAR_ERR_estimates_single_pixel_scenario_indep_par : ', &
          'parameter error estimate mask is not applied in single pixel scenario.', &
          'Error estimates are calculated for all retrieved parameters.'
        endif
      endif

      if ( RIN%flag_plus ) then
        call set_errest_derivative_mask(RIN, num_plus, idim1_plus, &
                                            par_type_plus, KNSINGF_mask)
      endif
      call set_errest_par_indices(RIN, iplus)

      select case(IMQ)
      case(2) !
        if ( RIN%flag_plus ) then
          write(iu_main_output,'(a,l2,2x,a,i0)') &
          'RIN%flag_plus =',RIN%flag_plus, &
          'The option is not supported for IMQ = ',IMQ
          stop 'stop in PAR_ERR_estimates_single_pixel_scenario_indep_par'
        endif
        do ipix=1,npixels
        if ( .not. errest_pix(ipix) ) cycle
            do i=1,KNSINGF
              FFS(:) = 0.0
              FFS(i) = 1.0
              call ITERQ (IMQ,KNSINGF,               & ! IN
                          UFS(1:KNSINGF,1:KNSINGF,ipix), &
                          FFS(1:KNSINGF),            &
                          QS1(1:KNSINGF)             & ! OUT
                        )
              GOUT_errest_par%pixel(ipix)%ERRP(i)  = sqrt( QS1(i)*ALS(ipix) )
              GOUT_errest_par%pixel(ipix)%BIASP(i) = QS(i,ipix)
              ! OD 2015-10-09          
              ! GOUT_errest_par%pixel(ipix)%BIASP(i) = sqrt(QS(i,ipix)*QS(i,ipix)*ALS(ipix))
              GOUT_errest_par%pixel(ipix)%TSTDP(i) = &
              sqrt( GOUT_errest_par%pixel(ipix)%ERRP(i)*GOUT_errest_par%pixel(ipix)%ERRP(i) + &
                    GOUT_errest_par%pixel(ipix)%BIASP(i)*GOUT_errest_par%pixel(ipix)%BIASP(i) )
            enddo ! i
        enddo ! ipix
      case(3,1) ! sparse matrix solver
        do ipix=1,npixels
          if ( .not. errest_pix(ipix) ) cycle
          call UF_nonzero_one_pixel (KNSINGF, UFS(:,:,ipix), UFNZ, nnz_pix)
          if(sparse_solver_name .eq. 'SUPERLU') &
          call solver_init_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
          do i=1,KNSINGF
            if ( .not. par_errest_mask(i) ) cycle
            if ( KNSINGF_mask(i) ) cycle
            b(:) = 0.0
            b(i) = 1.0
            call sparse_solver ( iu_main_output,         & ! IN
                                KNSINGF, nnz_pix, UFNZ, &
                                b,                      & ! INOUT
                                solver_timer            &
                               )
            if ( error_present() ) return
            QS1(i) = b(i)
            GOUT_errest_par%pixel(ipix)%ERRP(iplus(i))  = sqrt( QS1(i)*ALS(ipix) )
            GOUT_errest_par%pixel(ipix)%BIASP(iplus(i)) = QS(i,ipix)
            GOUT_errest_par%pixel(ipix)%TSTDP(iplus(i)) = &
            sqrt( GOUT_errest_par%pixel(ipix)%ERRP(iplus(i)) * &
                  GOUT_errest_par%pixel(ipix)%ERRP(iplus(i)) + &
                  GOUT_errest_par%pixel(ipix)%BIASP(iplus(i))* &
                  GOUT_errest_par%pixel(ipix)%BIASP(iplus(i)) )
!write(*,'(3i5,2es14.4,a)') ipix,i,iplus(i), &
!GOUT_errest_par%pixel(ipix)%ERRP(iplus(i)), &
!GOUT_errest_par%pixel(ipix)%BIASP(iplus(i)), &
!'  - ipix,i,iplus(i),ERRP,BIASP'
          enddo ! i
          if(sparse_solver_name .eq. 'SUPERLU') &
          call solver_cleanup_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
        enddo ! ipix
        if ( RIN%flag_plus ) then
        ! used analitical derivatives in presence of characteristics with n-1 retrieved parameters
          call set_par_errest_mask_plus(RIN, par_errest_mask_plus)
          do ipix=1,npixels
            call UF_nonzero_one_pixel (KNSINGF, UFS(:,:,ipix), UFNZ, nnz_pix)
            if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_init_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)

            do j=1,num_plus
            idim1 = idim1_plus(j)
            par_type = par_type_plus(j)
            do idim2=1,RIN%NDIM_plus%n2(idim1)
              n = RIN%NDIM_plus%n3(idim2,idim1)
              ibeg = RIN%NDIM%ISTARSING(idim2,idim1)
              iend = ibeg + RIN%NDIM%n3(idim2,idim1) - 1
              select case ( par_type )
              case ( par_type_SD_TB )
                deriv_type = 1
                x(1:n) = RIN%RADIUS1(1:n,idim2)
                y(1:n-1) = exp(AP(ibeg:iend,ipix))
                n1 = n
              case ( par_type_AVP_prof )
                deriv_type = 1
                x(2:NHVP_meas+1) = HVP_meas(1:NHVP_meas,ipix) * 0.001 ! m -> km
                x(1) = MASL(ipix) * 0.001 ! m -> km
                x(NHVP_meas+2) = HMAX_atm * 0.001 ! m -> km
                y(1:n-1) = exp(AP(ibeg:iend,ipix))
                y(n) = 0.0
                n1 = n + 1
              case ( par_type_SD_LB, par_type_SD_MD, par_type_SHD_distr )
                deriv_type = 2
                x(1:n) = 0.0_sp ! not used in PAR_ERR_estimates_deriv_matrix for the deriv_type
                y(1:n-1) = exp(AP(ibeg:iend,ipix))
                n1 = n
              end select
!write(*,'(i5,a)') deriv_type,'  - deriv_type'
!do i=1,n1
!write(*,'(2i5,2es12.4,a)') j,i,x(i),y(i),'  - j,i,x(i),y(i)  plus'
!enddo
              call PAR_ERR_estimates_deriv_matrix( iu_main_output, &
                                              deriv_type, n1, x, y, U )
!write(*,'(a)') 'U(i,1:n-1) after PAR_ERR_estimates_deriv_matrix'
!do i=1,n
!write(*,'(i5,10es14.4)') i,U(i,1:n-1)
!enddo
              do i=1,n
                b(1:KNSINGF) = 0.0_dp
                if ( .not. par_errest_mask_plus(i) ) cycle
                b(ibeg:iend) = U(i,1:n-1)
                call sparse_solver (  iu_main_output,       & ! IN
                                      KNSINGF, nnz_pix, UFNZ, &
                                      b,                    & ! INOUT
                                      solver_timer          &
                                    )
                if ( error_present() ) return
                QS1(1:KNSINGF) = b(1:KNSINGF)
                ERR_temp  = 0.0
                BIAS_temp = 0.0
                do k=1,KNSINGF
!write(*,'(2i5,3es14.4,a)') i,k,U(i,k),QS1(k),QS(k,ipix),'  - i,k,U(i,k),QS1(k),QS(k,ipix)  plus'
                  ERR_temp  = ERR_temp  + U(i,k)*QS1(k)
                  BIAS_temp = BIAS_temp + U(i,k)*QS(k,ipix)
                enddo ! k
                ERR_temp = sqrt(ERR_temp*ALS(ipix))
                i1 = RIN%NDIM_plus%ISTARSING(idim2,idim1) + i - 1
                GOUT_errest_par%pixel(ipix)%ERRP(i1)  = ERR_temp
                GOUT_errest_par%pixel(ipix)%BIASP(i1) = BIAS_temp
                GOUT_errest_par%pixel(ipix)%TSTDP(i1) = &
                sqrt( GOUT_errest_par%pixel(ipix)%ERRP(i1)*GOUT_errest_par%pixel(ipix)%ERRP(i1) + &
                      GOUT_errest_par%pixel(ipix)%BIASP(i1)*GOUT_errest_par%pixel(ipix)%BIASP(i1) )
!write(*,'(3i5,2es14.4,a)') ipix,i,i1, &
!GOUT_errest_par%pixel(ipix)%ERRP(i1), &
!GOUT_errest_par%pixel(ipix)%BIASP(i1), &
!'  - ipix,i,i1,ERRP,BIASP'
              enddo ! i
            enddo ! idim2
            enddo ! j
!stop 999
            if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_cleanup_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
          enddo ! ipix
        endif ! RIN%flag_plus
      end select

      return
      end subroutine PAR_ERR_estimates_single_pixel_scenario_indep_par

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine PAR_ERR_estimates_multi_pixel_scenario_indep_par (              &
                                                          iu_main_output,        & ! IN
                                                          sparse_solver_name,    &
                                                          RIN, npixels, MASL,    &
                                                          UF, QIM, AGENP, AP,    &
                                                          NHVP_meas, HVP_meas,   &
                                                          nnz_err,               &
                                                          UFNZ, GOUT_errest_par, & ! INOUT
                                                          solver_timer           &
                                                        )
      use mod_par_inv, only : KPARS, KPAR, KIMAGE, KVERTM
      use mod_par_OS, only : HMAX_atm
      use mod_fisher_matrix_ccs
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_globals, only : sp, dp
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings), intent(in)  ::  RIN
      integer,                   intent(in)  ::  npixels
      integer,                   intent(in)  ::  iu_main_output
      character(*),              intent(in)  ::  sparse_solver_name
      integer,                   intent(in)  ::  nnz_err
      real,dimension(KPAR),      intent(in)  ::  QIM    
      real,                      intent(in)  ::  AGENP 
      real,dimension(KPAR,KPAR), intent(in)  ::  UF
      real,dimension(KPARS,KIMAGE),  intent(in)  ::  AP
      integer,                      intent(in)  ::  NHVP_meas
      real,dimension(KVERTM,KIMAGE), intent(in)  ::  HVP_meas  ! heights for lidar measurements
      real,dimension(KIMAGE),        intent(in)  ::  MASL
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      type(nonzero),                     intent(inout)  ::  UFNZ    
      type(output_segment_err_estim_par),intent(inout)  ::  GOUT_errest_par		
!	----------------------------------------------------------------------
! LOCAL :
      logical                              ::  IPRI_additional_info
      integer                              ::  KNSINGF, KNF
      integer                              ::  IMQ
      real*8                               ::  TEMP
      real*8, dimension(:,:), allocatable  ::  ATEMP
      real*8, dimension(KPAR)              ::  b
      real,   dimension(KPAR)              ::  b1
      logical                              ::  ltest
      integer                              ::  alloc_stat
      integer                              ::  i, i1, j, j1, jpix, ii, ii1, ipix
      real                                 ::  tiny
      logical, dimension(KPARS)            ::  par_errest_mask
      logical, dimension(KPARS)            ::  par_errest_mask_plus
      integer :: n
      real (sp), dimension(KPARS,KPARS) :: U
      real (sp), dimension(KPARS+1) :: x, y
      real ::  ERR_temp, BIAS_temp
      integer :: ibeg, iend, idim1, idim2, k, n1
      integer :: par_type
      integer :: num_plus, idim1_plus(3), par_type_plus(3)
      logical, dimension(KPARS)  :: KNSINGF_mask
      integer, dimension(KPARS)  :: iplus
      integer :: deriv_type
!	----------------------------------------------------------------------
      tiny = 1e-4
      KNSINGF = RIN%KNSINGF
      KNF     = KNSINGF*npixels
      par_errest_mask(1:KNSINGF) = RIN%APSERREST(1:KNSINGF)

      IPRI_additional_info = RIN%IPRI_additional_info
      IMQ = RIN%IMQ

      ltest = .false.
      if(ltest) then
        allocate(ATEMP(KNF,KNF),stat=alloc_stat)
        if (alloc_stat /= 0) then
          write(tmp_message,'(a)') &
          'error while trying to allocate ATEMP'
          G_ERROR(trim(tmp_message))
        endif
        ATEMP(:,:) = 0.0
        do i=1,nnz_err
          ATEMP(UFNZ%row(i),UFNZ%col(i)) = UFNZ%val(i)
        enddo ! i
      endif ! ltest

      if ( RIN%flag_plus ) then
        call set_errest_derivative_mask(RIN, num_plus, idim1_plus, &
                                            par_type_plus, KNSINGF_mask)
        call set_errest_par_indices(RIN, iplus)
      else
        do i=1,KNSINGF
        iplus(i) = i
        enddo
      endif

      select case(IMQ)
      case(2) ! SVD method
        if ( RIN%flag_plus ) then
          write(iu_main_output,'(a,l2,2x,a,i0)') &
          'RIN%flag_plus =',RIN%flag_plus, &
          'The option is not supported for IMQ = ',IMQ
          stop 'stop in PAR_ERR_estimates_multi_pixel_scenario_indep_par'
        endif
        do ipix=1,npixels
        do i=1,KNSINGF
          if ( .not. par_errest_mask(i) ) cycle
          j = KNSINGF*(ipix-1)+i
          b(:) = 0.0d0
          b(j) = 1.0d0
          call ITERQ (  IMQ, KNF,             & ! IN
                        real(UF(1:KNF,1:KNF)),&
                        real(b(1:KNF)),       &
                        b1(1:KNF)             & ! OUT
                     )
          !call ITERQ ( !IMQ, KNF,                & ! IN
                        !real(ATEMP(1:KNF,1:KNF)),&
                        !real(b(1:KNF)),          &
                        !b1(1:KNF)                & ! OUT
                      !)
          GOUT_errest_par%pixel(ipix)%ERRP(i)  = sqrt( b1(j)*AGENP )
          GOUT_errest_par%pixel(ipix)%BIASP(i) = QIM(j)
          GOUT_errest_par%pixel(ipix)%TSTDP(i) = &
          sqrt( GOUT_errest_par%pixel(ipix)%ERRP(i)*GOUT_errest_par%pixel(ipix)%ERRP(i) + &
                GOUT_errest_par%pixel(ipix)%BIASP(i)*GOUT_errest_par%pixel(ipix)%BIASP(i) )

          if(ltest) then
            b(1:KNF) = b1(1:KNF)
            !write(*,*) 'col=',j
            !write(*,'(10e14.5)') (b(i1),i1=1,KNF)

            do ii=1,KNF
              TEMP = 0.0d0
              do ii1=1,KNF
                TEMP=TEMP+ATEMP(ii,ii1)*b(ii1)
              enddo ! ii1
              if(TEMP .gt. tiny) then
                write(*,'(a,i5,a,e14.5)') 'i=',ii,'  TEMP=',TEMP
              endif ! TEMP .gt. tiny
            enddo ! ii
          endif ! ltest
        enddo ! i
        enddo ! ipix
        if(ltest) then
          deallocate(ATEMP,stat=alloc_stat)
          if (alloc_stat /= 0) then
            write(tmp_message,'(a)') &
            'error while trying to deallocate ATEMP'
            G_ERROR(trim(tmp_message))
          endif
        endif ! ltest

      case(3,1)
        do ipix=1,npixels
        do i=1,KNSINGF
          if ( .not. par_errest_mask(i) ) cycle
          if ( KNSINGF_mask(i) ) cycle
          j = KNSINGF*(ipix-1)+i
          b(:) = 0.0d0
          b(j) = 1.0d0
          call sparse_solver (  iu_main_output,     & ! IN
                                KNF, nnz_err, UFNZ, &
                                b,                  & ! INOUT
                                solver_timer        & ! OUT
                             )
          if ( error_present() ) return
          b1(1:KNF) = b(1:KNF)
          GOUT_errest_par%pixel(ipix)%ERRP(iplus(i))  = sqrt( b1(j)*AGENP )
          GOUT_errest_par%pixel(ipix)%BIASP(iplus(i)) = QIM(j)
          GOUT_errest_par%pixel(ipix)%TSTDP(iplus(i)) = &
          sqrt( GOUT_errest_par%pixel(ipix)%ERRP(iplus(i)) * &
                GOUT_errest_par%pixel(ipix)%ERRP(iplus(i)) + &
                GOUT_errest_par%pixel(ipix)%BIASP(iplus(i))* &
                GOUT_errest_par%pixel(ipix)%BIASP(iplus(i)) )
!write(*,'(3i5,2es12.4,a)') &
!ipix,i,iplus(i),GOUT_errest_par%pixel(ipix)%ERRP(iplus(i)),GOUT_errest_par%pixel(ipix)%BIASP(iplus(i)), &
!'  - ipix,i,iplus(i),ERRP,BIASP main'
        enddo ! i
        enddo ! ipix
        !write(*,*) 'col=',j
        !write(*,'(10es12.4)') (b(i1),i1=1,KNF) 

        if ( RIN%flag_plus ) then
        ! use analitical derivatives in presence of characteristics with n-1 retrieved parameters
          call set_par_errest_mask_plus(RIN, par_errest_mask_plus)
          do ipix=1,npixels
            jpix = KNSINGF*(ipix-1)
            do j=1,num_plus
            idim1 = idim1_plus(j)
            par_type = par_type_plus(j)
            do idim2=1,RIN%NDIM_plus%n2(idim1)
              n = RIN%NDIM_plus%n3(idim2,idim1)
              ibeg = RIN%NDIM%ISTARSING(idim2,idim1)
              iend = ibeg + RIN%NDIM%n3(idim2,idim1) - 1
              select case ( par_type )
              case ( par_type_SD_TB )
                deriv_type = 1
                x(1:n) = RIN%RADIUS1(1:n,idim2)
                y(1:n-1) = exp(AP(ibeg:iend,ipix))
                n1 = n
              case ( par_type_AVP_prof )
                deriv_type = 1
                x(2:NHVP_meas+1) = HVP_meas(1:NHVP_meas,ipix) * 0.001 ! m -> km
                x(1) = MASL(ipix) * 0.001 ! m -> km
                x(NHVP_meas+2) = HMAX_atm
                y(1:n-1) = exp(AP(ibeg:iend,ipix))
                y(n) = 0.0
                n1 = n + 1
              case ( par_type_SD_LB, par_type_SD_MD, par_type_SHD_distr )
                deriv_type = 2
                x(1:n) = 0.0_sp ! not used in PAR_ERR_estimates_deriv_matrix for deriv_type
                y(1:n-1) = exp(AP(ibeg:iend,ipix))
                n1 = n
              end select
!write(*,'(i5,a)') deriv_type,'  - deriv_type'
!do i=1,n1
!write(*,'(a,i0,2i5,2es12.4,a)') 'ipix =',ipix,j,i,x(i),y(i),'  - j,i,x(i),y(i)  plus'
!enddo
              call PAR_ERR_estimates_deriv_matrix( iu_main_output, &
                                                deriv_type, n1, x, y, U )
!write(*,'(a)') 'U(i,1:n-1) after PAR_ERR_estimates_deriv_matrix'
!do i=1,n
!write(*,'(a,i0,i5,10es14.4)') 'ipix =',ipix,i,U(i,1:n-1)
!enddo
              do i=1,n
                b(:) = 0.0_dp
                if ( .not. par_errest_mask_plus(i) ) cycle
                b(jpix+ibeg:jpix+iend) = U(i,1:n-1)
                call sparse_solver (  iu_main_output,     & ! IN
                                      KNF, nnz_err, UFNZ, &
                                      b,                  & ! INOUT
                                      solver_timer        &
                                   )
                if ( error_present() ) return
                b1(1:KNF) = b(1:KNF)
                ERR_temp  = 0.0
                BIAS_temp = 0.0
                do k=1,n-1
                  j1 = jpix + k
                  ERR_temp = ERR_temp  + U(i,k)*b1(j1)
                  !write(*,'(a,i0,3i5,4es12.4,a)') 'ipix =',ipix,i,k,j1,U(i,k),b1(j1),ERR_temp,AGENP, &
                  !'  - i,k,j1,U(i,k),b1(j1),ERR_temp,AGENP'

                  BIAS_temp = BIAS_temp + U(i,k)*QIM(j1)
                enddo ! k
!write(*,*) ipix,i,ERR_temp*AGENP,'  ipix,i,ERR_temp*AGENP'
                ERR_temp = sqrt(ERR_temp*AGENP)
                i1 = RIN%NDIM_plus%ISTARSING(idim2,idim1) + i - 1
!write(*,*) ipix,i,i1,ERR_temp,'  ipix,i,i1,sqrt(ERR_temp)'
                GOUT_errest_par%pixel(ipix)%ERRP(i1)  = ERR_temp
                GOUT_errest_par%pixel(ipix)%BIASP(i1) = BIAS_temp
                GOUT_errest_par%pixel(ipix)%TSTDP(i1) = &
                sqrt( GOUT_errest_par%pixel(ipix)%ERRP(i1)*GOUT_errest_par%pixel(ipix)%ERRP(i1) + &
                      GOUT_errest_par%pixel(ipix)%BIASP(i1)*GOUT_errest_par%pixel(ipix)%BIASP(i1) )
!write(*,'(8i5,2es12.4,a)') &
!ipix, i, i1, jpix, ibeg, iend, jpix+ibeg, jpix+iend, &
!GOUT_errest_par%pixel(ipix)%ERRP(i1), GOUT_errest_par%pixel(ipix)%BIASP(i1), &
!'  - ipix, i, i1, jpix, ibeg, iend, jpix+ibeg, jpix+iend, ERRP, BIASP  plus'
              enddo ! i
            enddo ! idim2
            enddo ! j
          enddo ! ipix
        endif ! RIN%flag_plus
      end select 
!stop 999
      return
      end subroutine PAR_ERR_estimates_multi_pixel_scenario_indep_par
            
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine PAR_ERR_estimates_single_pixel_scenario (             &
                                                iu_main_output,        & ! IN
                                                sparse_solver_name,    &
                                                RIN, npixels,          &
                                                UFS, QS, ALS,          &
                                                errest_pix,            &
                                                UFNZ, GOUT_errest_par, & ! INOUT
                                                solver_timer           &
                                                         )

      use mod_par_inv, only : KPARS, KIMAGE, KPAR
      use mod_fisher_matrix_ccs
      use mod_retr_general_output_derived_type
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings), intent(in)  ::  RIN
      integer,                   intent(in)  ::  npixels
      integer,                   intent(in)  ::  iu_main_output
      character(*),              intent(in)  ::  sparse_solver_name
      real,dimension(KIMAGE),            intent(in)  ::  ALS
      real,dimension(KPARS,KIMAGE),      intent(in)  ::  QS
      real,dimension(KPARS,KPARS,KIMAGE),intent(in)  ::  UFS
      logical, dimension(KIMAGE),        intent(in)  ::  errest_pix
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      type(nonzero),                     intent(inout)  ::  UFNZ
      type(output_segment_err_estim_par),intent(inout)  ::  GOUT_errest_par
!	----------------------------------------------------------------------
! LOCAL :
      logical                              ::  IPRI_additional_info
      integer                              ::  KNSINGF
      integer                              ::  IMQ, npix
      real, dimension(KPARS)               ::  FFS, QS1
      integer                              ::  nnz_pix
      real*8, dimension(KPAR)              ::  b
      integer                              ::  i, ipix
      logical, dimension(KPARS)            ::  par_errest_mask
      integer :: iu_tmp
!	----------------------------------------------------------------------
      KNSINGF = RIN%KNSINGF
      IMQ = RIN%IMQ
      IPRI_additional_info = RIN%IPRI_additional_info
      if ( RIN%IPRI_verbose ) then
        par_errest_mask(1:KNSINGF) = RIN%APSERREST(1:KNSINGF)
        if ( any( .not. par_errest_mask(1:KNSINGF)) ) then
          write(iu_main_output,'(a,/,a,/,a)') &
          'WARNING in PAR_ERR_estimates_single_pixel_scenario : ', &
          'parameter error estimate mask is not applied in single pixel scenario.', &
          'Error estimates are calculated for all retrieved parameters.'
        endif
      endif


!MEH
      if(RIN%debug_covariance_matrix .eqv. .true.) then
         OPEN(newunit=iu_tmp, FILE = 'test_cov_matrix.txt')
      endif
      !!!OPEN(50, FILE = 'Bias.txt')

      select case(IMQ)
      case(2) !
         do ipix=1,npixels
        if(.not. errest_pix(ipix)) cycle
            do i=1,KNSINGF
              FFS(:) = 0.0
              FFS(i) = 1.0
              call ITERQ (IMQ,KNSINGF,               & ! IN
                          UFS(1:KNSINGF,1:KNSINGF,ipix), &
                          FFS(1:KNSINGF),            &
                          QS1(1:KNSINGF)             & ! OUT
                          )
              GOUT_errest_par%pixel(ipix)%ERRP(i)  = sqrt( QS1(i)*ALS(ipix) )
              GOUT_errest_par%pixel(ipix)%BIASP(i) = QS(i,ipix)
              ! OD 2015-10-09
              ! GOUT_errest_par%pixel(ipix)%BIASP(i) = sqrt(QS(i,ipix)*QS(i,ipix)*ALS(ipix))
              GOUT_errest_par%pixel(ipix)%TSTDP(i) = &
              sqrt( GOUT_errest_par%pixel(ipix)%ERRP(i)*GOUT_errest_par%pixel(ipix)%ERRP(i) + &
                    GOUT_errest_par%pixel(ipix)%BIASP(i)*GOUT_errest_par%pixel(ipix)%BIASP(i) )
            enddo ! i
        enddo ! ipix
      case(3,1) ! sparse matrix solver
         do ipix=1,npixels
            call UF_nonzero_one_pixel (KNSINGF, UFS(:,:,ipix), UFNZ, nnz_pix)
            if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_init_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
!MH - print of residual value and parameters
            if(RIN%debug_covariance_matrix .eqv. .true.) then
               write(iu_tmp,*) 'ALS(ipix): ', ALS(ipix)
               write(iu_tmp,*) 'b(i): '
               write(iu_tmp,'(12x,350i12)') (i, i = 1, KNSINGF)
            endif
            do i=1,KNSINGF
              b(:) = 0.0
              b(i) = 1.0
              call sparse_solver (  iu_main_output,         & ! IN
                                    KNSINGF, nnz_pix, UFNZ, &
                                    b,                      & ! INOUT
                                    solver_timer            &
                                  )
              if ( error_present() ) return
              QS1(i) = b(i)
!MH - print the covariance matrix
              if(RIN%debug_covariance_matrix .eqv. .true.) then
                 write(iu_tmp,'(i12,350es12.4)') i, b(1:KNSINGF)
              endif

              GOUT_errest_par%pixel(ipix)%ERRP(i)  = sqrt( QS1(i)*ALS(ipix) )
              GOUT_errest_par%pixel(ipix)%BIASP(i) = QS(i,ipix)
              GOUT_errest_par%pixel(ipix)%TSTDP(i) = &
              sqrt( GOUT_errest_par%pixel(ipix)%ERRP(i)*GOUT_errest_par%pixel(ipix)%ERRP(i) + &
                    GOUT_errest_par%pixel(ipix)%BIASP(i)*GOUT_errest_par%pixel(ipix)%BIASP(i) )

              !!!WRITE(50,*) GOUT_errest_par%pixel(ipix)%BIASP(i)
           enddo ! i

          
            if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_cleanup_SUPERLU(KNSINGF, nnz_pix, UFNZ, b)
         enddo ! ipix
!         write(*,*) 'col=',ipix
!         write(*,*) 'npixel', npixels
!         write(*,*) 'KNSINGF', KNSINGF
!         write(*,'(10es12.4)') (b(i),i=1,KNSINGF)
      end select

         if(RIN%debug_covariance_matrix .eqv. .true.) then
            CLOSE(iu_tmp)
         endif

!!!      CLOSE(50)
      return
      end subroutine PAR_ERR_estimates_single_pixel_scenario

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine PAR_ERR_estimates_multi_pixel_scenario ( iu_main_output,        & ! IN
                                                          sparse_solver_name,    &
                                                          RIN, npixels,          &
                                                          UF, QIM, AGENP,        &
                                                          nnz_err,               &
                                                          UFNZ, GOUT_errest_par, & ! INOUT
                                                          solver_timer           &
                                                        )
      use mod_par_inv, only : KPARS, KPAR
      use mod_fisher_matrix_ccs
      use mod_retr_general_output_derived_type
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings), intent(in)  ::  RIN
      integer,                   intent(in)  ::  npixels
      integer,                   intent(in)  ::  iu_main_output
      character(*),              intent(in)  ::  sparse_solver_name
      integer,                   intent(in)  ::  nnz_err
      real,dimension(KPAR),      intent(in)  ::  QIM
      real,                      intent(in)  ::  AGENP
      real,dimension(KPAR,KPAR), intent(in)  ::  UF
!	----------------------------------------------------------------------
! INOUT :
      real,                              intent(inout)  ::  solver_timer
      type(nonzero),                     intent(inout)  ::  UFNZ
      type(output_segment_err_estim_par),intent(inout)  ::  GOUT_errest_par
!	----------------------------------------------------------------------
! LOCAL :
      logical                              ::  IPRI_additional_info
      integer                              ::  KNSINGF, KNF
      integer                              ::  IMQ
      real*8                               ::  TEMP
      real*8, dimension(:,:), allocatable  ::  ATEMP
      real*8, dimension(KPAR)              ::  b
      real,   dimension(KPAR)              ::  b1
      logical                              ::  ltest
      integer                              ::  alloc_stat
      integer                              ::  i, i1, j, ii, ii1, ipix
      real                                 ::  tiny
      logical, dimension(KPARS)            ::  par_errest_mask
      integer :: iu_tmp
!	----------------------------------------------------------------------
      tiny = 1e-4
      KNSINGF = RIN%KNSINGF
      KNF     = KNSINGF*npixels
      par_errest_mask(1:KNSINGF) = RIN%APSERREST(1:KNSINGF)

      IPRI_additional_info = RIN%IPRI_additional_info
      IMQ = RIN%IMQ

      if(RIN%debug_covariance_matrix .eqv. .true.) then      
         open(newunit=iu_tmp, FILE = 'test_cov_matrix.txt')
         write(iu_tmp,*) 'KNSINGF: ', KNSINGF
      endif

      !!!OPEN(31, FILE = 'TestOutput2.txt')  
      !!!write(31,*) 'AGENP: ', AGENP
      !!!write(31,*) 'b(i): '
      !!!write(31,'(12x,16500i12)') (i, i = 1, KNF) 
      
      ltest = .false.
      if(ltest) then
        allocate(ATEMP(KNF,KNF),stat=alloc_stat)
        if (alloc_stat /= 0) then
          write(tmp_message,'(a)') &
          'error while trying to allocate ATEMP'
          G_ERROR(trim(tmp_message))
        endif
        ATEMP(:,:) = 0.0
        do i=1,nnz_err
          ATEMP(UFNZ%row(i),UFNZ%col(i)) = UFNZ%val(i)
        enddo ! i
      endif ! ltest

      select case(IMQ)
      case(2) ! SVD method
        do ipix=1,npixels
        do i=1,KNSINGF
          if ( .not. par_errest_mask(i) ) cycle
          j = KNSINGF*(ipix-1)+i
          b(:) = 0.0d0
          b(j) = 1.0d0
          call ITERQ (  IMQ, KNF,             & ! IN
                        real(UF(1:KNF,1:KNF)),&
                        real(b(1:KNF)),       &
                        b1(1:KNF)             & ! OUT
                     )
          !call ITERQ ( !IMQ, KNF,                & ! IN
                        !real(ATEMP(1:KNF,1:KNF)),&
                        !real(b(1:KNF)),          &
                        !b1(1:KNF)                & ! OUT
                      !)
          GOUT_errest_par%pixel(ipix)%ERRP(i)  = sqrt( b1(j)*AGENP )
          GOUT_errest_par%pixel(ipix)%BIASP(i) = QIM(j)
          GOUT_errest_par%pixel(ipix)%TSTDP(i) = &
          sqrt( GOUT_errest_par%pixel(ipix)%ERRP(i)*GOUT_errest_par%pixel(ipix)%ERRP(i) + &
                GOUT_errest_par%pixel(ipix)%BIASP(i)*GOUT_errest_par%pixel(ipix)%BIASP(i) )

          if(ltest) then
            b(1:KNF) = b1(1:KNF)
            !!write(*,*) 'col=',j
            !!write(*,'(10e14.5)') (b(i1),i1=1,KNF)

            do ii=1,KNF
              TEMP = 0.0d0
              do ii1=1,KNF
                TEMP=TEMP+ATEMP(ii,ii1)*b(ii1)
              enddo ! ii1
              if(TEMP .gt. tiny) then
                write(*,'(a,i5,a,e14.5)') 'i=',ii,'  TEMP=',TEMP
              endif ! TEMP .gt. tiny
            enddo ! ii
          endif ! ltest
        enddo ! i
        enddo ! ipix
        if(ltest) then
          deallocate(ATEMP,stat=alloc_stat)
          if (alloc_stat /= 0) then
            write(tmp_message,'(a)') &
            'error while trying to deallocate ATEMP'
            G_ERROR(trim(tmp_message))
          endif
        endif ! ltest

      case(3,1)
         
      do ipix=1,npixels
            
!MH - print of residual value and parameters
      if(RIN%debug_covariance_matrix .eqv. .true.) then      
        write(iu_tmp,*) 'AGENP: ', AGENP
        write(iu_tmp,*) 'b(i): '
        ! Number of pixel x Number of parameters ---> check always! or generalize
        write(iu_tmp,'(12x,16500i12)') (i, i = 1, KNSINGF)
      endif

      do i=1,KNSINGF
          if ( .not. par_errest_mask(i) ) cycle
          j = KNSINGF*(ipix-1)+i
          b(:) = 0.0d0
          b(j) = 1.0d0
          call sparse_solver (  iu_main_output,     & ! IN
                                KNF, nnz_err, UFNZ, &
                                b,                  & ! INOUT
                                solver_timer        & ! OUT
                             )
          if ( error_present() ) return
!MH - print
     if(RIN%debug_covariance_matrix .eqv. .true.) then      
        write(iu_tmp,'(i12,16500es12.4)') i, b(1:KNSINGF)
     endif   
          !!!WRITE(31,'(i12,16500es12.4)') i, b(1:KNF)           
          GOUT_errest_par%pixel(ipix)%ERRP(i)  = sqrt( b(j)*AGENP )
          GOUT_errest_par%pixel(ipix)%BIASP(i) = QIM(j)
          GOUT_errest_par%pixel(ipix)%TSTDP(i) = &
          sqrt( GOUT_errest_par%pixel(ipix)%ERRP(i)*GOUT_errest_par%pixel(ipix)%ERRP(i) + &
                GOUT_errest_par%pixel(ipix)%BIASP(i)*GOUT_errest_par%pixel(ipix)%BIASP(i) )
        enddo ! i
        enddo ! ipix
        !write(*,*) 'col=',j
        !write(*,'(10es12.4)') (b(i1),i1=1,KNF)

      end select

      if(RIN%debug_covariance_matrix .eqv. .true.) then      
         CLOSE(iu_tmp)
      endif
      !!!CLOSE(31)
      return
      end subroutine PAR_ERR_estimates_multi_pixel_scenario

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine OPT_ERR_estimates_number_of_optchar (RIN, NOF)

      use mod_retr_settings_derived_type
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings), intent(in)  ::  RIN
!	----------------------------------------------------------------------
! OUT :
      integer,                   intent(out) ::  NOF
! NOF  - number of optical functions for error estimates
!	----------------------------------------------------------------------
      NOF = 0

      if( RIN%products%errest%aerosol%lidar .and. (RIN%DLSF%keyEL .gt. 0)) then
         NOF = 4 ! ext, ssa, and lr !aext
      else
         NOF = 3 ! ext, ssa !and aext
      endif !

      end subroutine OPT_ERR_estimates_number_of_optchar

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine OPT_ERR_estimates_number_of_sd (RIN, NSD1)

      use mod_retr_settings_derived_type
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
! IN :
      type(retr_input_settings), intent(in)  ::  RIN
!	----------------------------------------------------------------------
! OUT :
      integer,                   intent(out) ::  NSD1
! NSD1 - number of SD modes + 1(total)
!	----------------------------------------------------------------------
      NSD1 = 0

      select case(RIN%NSD)
      case(1)
         NSD1 = RIN%NSD
      case(2)
         NSD1 = RIN%NSD+1  ! fine, coarse, total
      case default
         write(tmp_message,'(a,i0,a)') &
         'NSD = ',RIN%NSD,' - is not supported'
         G_ERROR(trim(tmp_message))
      end select

      end subroutine OPT_ERR_estimates_number_of_sd

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine prepares derivative mask for error estimates in case of retrieval
        !> @brief of independent concentration and shape parameters
        !>
        !> @param[in]  RIN - input settings structure
        !> @param[out]  num_plus - number of retrieved characteristic with n-1 parameters
        !> @param[out]  idim1_plus - characteristic number
        !> @param[out]  par_type_plus - characteristic type
        !> @param[out]  KNSINGF_mask - derivative mask for retrieved characteristics
        !>
      subroutine set_errest_derivative_mask ( RIN, num_plus, idim1_plus, &
                                    par_type_plus, KNSINGF_mask )

      use mod_par_inv, only : KPARS
      use mod_retr_settings_derived_type
      use mod_stop_report

      implicit none
! ----------------------------------------------------------------------------------
      type(retr_input_settings), intent(in) :: RIN
      integer, intent(out) :: num_plus
      integer, intent(out) :: idim1_plus(3)
      integer, intent(out) :: par_type_plus(3)
      logical, intent(out) :: KNSINGF_mask(KPARS)
! ----------------------------------------------------------------------------------
      integer :: IDIM1, IDIM2, ibeg, iend
! ----------------------------------------------------------------------------------
      num_plus = 0
      idim1_plus(:) = 0
      par_type_plus(:) = 0
      KNSINGF_mask(:) = .false.

      do IDIM1=1,RIN%NDIM%n1
      if ( .not. RIN%NDIM%par_retr(IDIM1) ) cycle
        do IDIM2=1,RIN%NDIM%n2(IDIM1)
        if ( RIN%NDIM%n3(IDIM2,IDIM1) .eq. RIN%NDIM_plus%n3(IDIM2,IDIM1) ) cycle
          num_plus = num_plus + 1
          idim1_plus(num_plus) = IDIM1
          par_type_plus(num_plus) = RIN%NDIM_plus%par_type(IDIM1)
          ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
          iend = ibeg + RIN%NDIM%n3(IDIM2,IDIM1) - 1
          KNSINGF_mask(ibeg:iend) = .true.
        enddo
      enddo

      if ( num_plus .eq. 0 ) then
         write(tmp_message,'(a,i0,a,l)') &
         'num_plus = ',num_plus,' - can not be zero with RIN%flag_plus = ',RIN%flag_plus
         G_ERROR(trim(tmp_message))
      endif

      return
      end subroutine set_errest_derivative_mask

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine prepares parameter error estimate mask required by settings for output
        !>
        !> @param[in]   RIN - input settings structure
        !> @param[out]  par_errest_mask_plus - mask to calculate error estimates for dependent parameters
        !>
      subroutine set_par_errest_mask_plus ( RIN, par_errest_mask_plus )

      use mod_par_inv, only : KPARS
      use mod_retr_settings_derived_type
      use mod_stop_report

      implicit none
! ----------------------------------------------------------------------------------
      type(retr_input_settings), intent(in) :: RIN
      logical, dimension(KPARS), intent(out) :: par_errest_mask_plus
! ----------------------------------------------------------------------------------
      logical, dimension(KPARS) :: par_errest_mask
      integer, dimension(KPARS) :: iplus
      integer :: i, idim1, idim2, ibeg, iend
      integer :: ndim3
! ----------------------------------------------------------------------------------
      par_errest_mask(:) = RIN%APSERREST(:)
      par_errest_mask_plus(:) = .false.

      call set_errest_par_indices(RIN, iplus)
      do i=1,RIN%KNSINGF
        par_errest_mask_plus(iplus(i)) = par_errest_mask(i)
      enddo

      do idim1=1,RIN%NDIM%n1
      do idim2=1,RIN%NDIM%n2(idim1)
        ndim3 = RIN%NDIM%n3(idim2,idim1)
        if ( RIN%NDIM_plus%n3(idim2,idim1) .gt. ndim3 ) then
          ibeg = RIN%NDIM%ISTARSING(idim2,idim1)
          iend = ibeg + ndim3 - 1
          if ( all(par_errest_mask(ibeg:iend) .eqv. .true.) ) then
          par_errest_mask_plus(RIN%NDIM_plus%ISTARSING(idim2,idim1)) = .true.
          endif
        endif
      enddo
      enddo

      return
      end subroutine set_par_errest_mask_plus

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine returns indices of n-1 parameters in complete vector of parameters
        !>
        !> @param[in]  RIN - input settings structure
        !> @param[out]  iplus - indices of n-1 parameters in complete vector of parameters
        !>
      subroutine set_errest_par_indices(RIN, iplus)

      use mod_par_inv, only : KPARS
      use mod_retr_settings_derived_type

      implicit none
! ----------------------------------------------------------------------------------
      type(retr_input_settings), intent(in) :: RIN
      integer, intent(out) :: iplus(KPARS)
! ----------------------------------------------------------------------------------
      integer :: i, i1, IDIM1, IDIM2, IDIM3
! ----------------------------------------------------------------------------------

      i  = 0
      i1 = 0
      do IDIM1=1,RIN%NDIM%n1
      do IDIM2=1,RIN%NDIM%n2(IDIM1)
        if ( RIN%NDIM_plus%n3(IDIM2,IDIM1) .gt. RIN%NDIM%n3(IDIM2,IDIM1) ) then
        i1 = i1 + 1
        endif
        do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
        i  = i + 1
        i1 = i1 + 1
        iplus(i) = i1
        enddo
      enddo
      enddo

      return
      end subroutine set_errest_par_indices

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine PAR_ERR_estimates_deriv_matrix ( iu_main_output, &
                                                deriv_type, n, x, y, U )

      use mod_globals, only : sp
      use mod_par_inv, only : KPARS
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------------------
! IN :
      integer, intent(in) :: iu_main_output     !> main output unit number
      integer, intent(in) :: deriv_type         !> type of distribution [1,2]
      integer, intent(in) :: n                  !> number of parameters
      real (sp), dimension(KPARS+1), intent(in) :: y   !> retrieved parameters (function values)
      real (sp), dimension(KPARS+1), intent(in) :: x   !> array of arguments
!	----------------------------------------------------------------------------------
! OUT :
      real (sp), dimension(KPARS,KPARS), intent(out) :: U !> matrix of derivatives
!	----------------------------------------------------------------------------------
! LOCAL : 
      integer :: i, k
      real (sp) :: b1, b2, p
      real (sp), dimension(n) :: a, dx
!	----------------------------------------------------------------------------------
! n = 4
! i = [1,4]
! k = [2,4]
!
! a(k) = P(k) / P(1) - retrieved parameters
! a(1) = 1.0
!
! Triangle bins Size distribution and Vertical distribution (deriv_type = 1):
! --------------------------------
! P(i) = a(i) / ( 0.5  * ( (a(1)+a(2))*(x(2)-x(1))+(a(2)+(a(3))*(x(3)-x(2))+(a(3)+(a(4))*(x(4)-x(3) ) ) - value of distribution for x(i)
! b1 = 0.5 * [ (a(1)+a(2))*(x(2)-x(1)) + (a(2)+(a(3))*(x(3)-x(2)) + (a(3)+(a(4))*(x(4)-x(3)) ] = 0.5 * [
! a(1) * (x(2)-x(1)) +
! a(2) * (x(2)-x(1)) + a(2) * (x(3)-x(2)) +
! a(3) * (x(3)-x(2)) + a(3) * (x(4)-x(3)) +
! a(4) * (x(4)-x(3)) ] = 0.5 * [
! a(1) * (x(2)-x(1)) +
! a(2) * (x(2)-x(1) + x(3)-x(2)) +
! a(3) * (x(3)-x(2) + x(4)-x(3)) +
! a(4) * (x(4)-x(3)) ] = 0.5 * [
! a(1)*(x(2)-x(1)) + a(2)*(-x(1)+x(3)) + a(3)*(-x(2)+x(4)) + a(4)*(x(4)-x(3)) ]
!
! b1 = 0.5 * [ a(1)*(x(2)-x(1)) + a(2)*(-x(1)+x(3)) + a(3)*(-x(2)+x(4)) + a(4)*(x(4)-x(3)) ]
! b2 = 1.0 / ( a(1)*(x(2)-x(1)) + a(2)*(-x(1)+x(3)) + a(3)*(-x(2)+x(4)) + a(4)*(x(4)-x(3)) )^2 = 1.0 / (b1*b1)
!
! dP(1)/da(2) = b2 *
! 		( da(1)/da(2) * b1  -
! 		    0.5 * [ da(1)/da(2)*(x(2)-x(1)) + da(2)/da(2)*(-x(1)+x(3)) + da(3)/da(2)*(-x(2)+x(4)) + da(4)/da(2)*(x(4)-x(3)) ] * a(1)  =
!		= b2 * ( - 0.5*(-x(1)+x(3)) * a(1) )
!
! dP(1)/da(3) = b2 *
! 		( da(1)/da(3) * b1  -
! 		    0.5 * [ da(1)/da(3)*(x(2)-x(1)) + da(2)/da(3)*(-x(1)+x(3)) + da(3)/da(3)*(-x(2)+x(4)) + da(4)/da(3)*(x(4)-x(3)) ] * a(1)  =
!		= b2 * ( - 0.5*(-x(2)+x(4)) * a(1) )
!
! dP(1)/da(4) = b2 *
! 		( da(1)/da(4) * b1  -
! 		    0.5 * [ da(1)/da(4)*(x(2)-x(1)) + da(2)/da(4)*(-x(1)+x(3)) + da(3)/da(4)*(-x(2)+x(4)) + da(4)/da(4)*(x(4)-x(3)) ] * a(1)  =
!		= b2 * ( - 0.5*(x(4)-x(3)) * a(1) )
!
! dP(2)/da(2) = b2 *
! 		( da(2)/da(2) * b1  -
! 		    0.5 * [ da(1)/da(2)*(x(2)-x(1)) + da(2)/da(2)*(-x(1)+x(3)) + da(3)/da(2)*(-x(2)+x(4)) + da(4)/da(2)*(x(4)-x(3)) ] * a(2)  =
!		= b2 * ( b1 - 0.5*(-x(1)+x(3))*a(2) )
!
! dP(2)/da(3) = b2 *
! 		( da(2)/da(3) * b1  -
! 		    0.5 * [ da(1)/da(3)*(x(2)-x(1)) + da(2)/da(3)*(-x(1)+x(3)) + da(3)/da(3)*(-x(2)+x(4)) + da(4)/da(3)*(x(4)-x(3)) ] * a(2)  =
!		= b2 * ( - 0.5*(-x(2)+x(4))*a(2) )
!
! dP(2)/da(4) = b2 *
! 		( da(4)/da(4) * b1  -
! 		    0.5 * [ da(1)/da(4)*(x(2)-x(1)) + da(2)/da(4)*(-x(1)+x(3)) + da(3)/da(4)*(-x(2)+x(4)) + da(4)/da(4)*(x(4)-x(3)) ] * a(2)  =
!		= b2 * ( - 0.5*(x(4)-x(3))*a(2) )
!
! k=2
! dP(1)/da(2) = b2 * (    - 0.5*(-x(1)+x(3))*a(1) )
! dP(2)/da(2) = b2 * ( b1 - 0.5*(-x(1)+x(3))*a(2) )
! dP(3)/da(2) = b2 * (    - 0.5*(-x(1)+x(3))*a(3) )
! dP(4)/da(2) = b2 * (    - 0.5*(-x(1)+x(3))*a(4) )
! k=3
! dP(1)/da(3) = b2 * (    - 0.5*(-x(2)+x(4))*a(1) )
! dP(2)/da(3) = b2 * (    - 0.5*(-x(2)+x(4))*a(2) )
! dP(3)/da(3) = b2 * ( b1 - 0.5*(-x(2)+x(4))*a(3) )
! dP(4)/da(3) = b2 * (    - 0.5*(-x(2)+x(4))*a(4) )
! k=4
! dP(1)/da(4) = b2 * (    - 0.5*(x(4)-x(3))*a(1) )
! dP(2)/da(4) = b2 * (    - 0.5*(x(4)-x(3))*a(2) )
! dP(3)/da(4) = b2 * (    - 0.5*(x(4)-x(3))*a(3) )
! dP(4)/da(4) = b2 * ( b1 - 0.5*(x(4)-x(3))*a(4) )
!
! dlnP(i)/dlna(k) = dP(i)/da(k) * a(k)/P(i)
!
! Lognormal bins and Models distributions and Shape distribution (deriv_type = 2):
! --------------------------------
! P(i) = a(i) / ( a(1)+a(2)+a(3)+a(4) ) - value of distribution for x(i)
! b1 = a(1)+a(2)+a(3)+a(4)
! b2 = 1.0 / (a(1)+a(2)+a(3)+a(4))^2 = 1.0 / (b1*b1)
!
! dP(1)/da(2) = b2 *
! 		( da(1)/da(2) * b1  -
! 		    ( da(1)/da(2) + da(2)/da(2) + da(3)/da(2) + da(4)/da(2) ) * a(1) )
! k=2
! dP(1)/da(2) = b2 * (    - a(1) )
! dP(2)/da(2) = b2 * ( b1 - a(2) )
! dP(3)/da(2) = b2 * (    - a(3) )
! dP(4)/da(2) = b2 * (    - a(4) )
! k=3
! dP(1)/da(3) = b2 * (    - a(1) )
! dP(2)/da(3) = b2 * (    - a(2) )
! dP(3)/da(3) = b2 * ( b1 - a(3) )
! dP(4)/da(3) = b2 * (    - a(4) )
! k=4
! dP(1)/da(4) = b2 * (    - a(1) )
! dP(2)/da(4) = b2 * (    - a(2) )
! dP(3)/da(4) = b2 * (    - a(3) )
! dP(4)/da(4) = b2 * ( b1 - a(4) )
!
! dlnP(i)/dlna(k) = dP(i)/da(k) * a(k)/P(i)
!
!	----------------------------------------------------------------------------------
      U(:,:) = 0.0_sp
! Define array for n parameters
      a(1) = 1.0_sp
      a(2:n) = y(1:n-1)

      select case ( deriv_type )
      case ( 1 )
        do i=2,n-1
        dx(i) = x(i+1) - x(i-1)
        enddo
        dx(n) = x(n) - x(n-1)
        ! Coefficients
        b1 = 0.5_sp * sum(a(1:n)*dx(1:n))
        b2 = 1.0_sp / (b1*b1)
        ! Derivatives
        do i=1,n
        p = a(i) / b1
        do k=2,n
          if ( i .eq. k ) then
          U(i,k-1) = b2 * ( b1 - 0.5_sp * dx(k) * a(i) ) * ( a(k)/p )
          else
          U(i,k-1) = b2 * ( - 0.5_sp * dx(k) * a(i) ) * ( a(k)/p )
          endif
        enddo ! i
        enddo ! k
      case ( 2 )
        ! Coefficients
        b1 = sum(a(1:n))
        b2 = 1.0_sp / (b1*b1)
        ! Derivatives
        do i=1,n
        p = a(i) / b1
        do k=2,n
          if ( i .eq. k ) then
          U(i,k-1) = b2 * ( b1 - a(i) ) * ( a(k)/p )
          else
          U(i,k-1) = b2 * ( -a(i) ) * ( a(k)/p )
          endif
          !write(*,'(2i5,4es12.4,a)') i,k,b1,b2,a(i),U(i,k-1), &
          !         '  - i,k,b1,b2,a(i),U(i,k) in PAR_ERR_estimates_deriv_matrix'
        enddo ! k
        enddo ! i
      end select

      return
      end subroutine PAR_ERR_estimates_deriv_matrix

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!! MEH:
!! subroutine to calculate the systematic error adding positive and negative biases
subroutine BIAS_estimates (RIN, iu_main_output,  &
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
                           GOUT, INVSING, US_temp,&
                           sparse_solver_name, &
                           QSE_temp, QIM_temp, &
                           option_bias, KM1_pix, ALS_temp)    !out


use mod_par_inv
use mod_par_OS
use MOD_RT_SOS
use inversion_subsystem
use mod_par_DLS,     only : KMpar
use mod_fisher_matrix_ccs
use mod_retr_settings_derived_type
use mod_retr_general_output_derived_type
use mod_covariance_matrix
use mod_smoothness_estimates
use mod_stop_report
use mod_sdata_meas_type
use mod_edges
use mod_index_cloud


use mod_sdata, only : get_HVP_lidar, get_MASL,           &
                      set_segment_meas_vector_fs,        &
                      assign_noise_index,                &
                      set_index_clouds,add_bias_segment !MEH new for bias!!
implicit none

!    --------------------------------------------------------------------------------------------
type(retr_input_settings),     intent(in)    ::  RIN
type(segment_data),            intent(in)    ::  segment_meas
integer,                       intent(in)    ::  iu_main_output
type(output_segment_general),  intent(in)    ::  GOUT
type(kernels_triangle_bin),    intent(inout) ::  KERNELS1
type(kernels_lognormal_bin),   intent(inout) ::  KERNELS2
real, dimension(KIMAGE),       intent(out)   :: ALS_temp
integer, optional,             intent(in)    :: option_bias
!    --------------------------------------------------------------------------------------------
real,dimension(KMESS,KPARS,KIMAGE)           ::  US_temp
type(nonzero)                  :: UFNZ_temp
integer                        :: nnz !,nnz_err
real                           :: solver_timer_temp
type(output_segment_general)   ::  GOUT_temp
!    ------------------------------------------------------------------------------------------
integer                        :: npixels
integer                        :: ipix,ipixstart,ipixstop,KNF,ipix2
integer                        :: IKS,IKS1
!    --------------------------------------------------------------------------------------------
real                           :: ccor_min, ccor_max, ccor
integer                        :: LP
logical                        :: apriori_estim, apriori_smooth
integer                        :: nangles
real, dimension(KMpar)         :: sca_angles
integer                        :: KMIMAGE
integer, dimension(KKNOISE,KIMAGE)       :: IKI ! number of meas for noise (:,:)
logical, dimension(KKNOISE,KIMAGE)       :: IKI_shift ! presence of SHIFT
integer, dimension(KMESS,KKNOISE,KIMAGE) :: KNOISEI
real, dimension(KPARS)                   :: APMIN,APMAX
type(pixel_vector)                  :: pixel_vec_fit_deriv(KIMAGE)
real, dimension(KPARS,KIMAGE)       :: AP, AP0, APQ
real, dimension(KMESS,KIMAGE)       :: CS
real, dimension(KPARS,KIMAGE)       :: FFMS
real, dimension(KIMAGE)             :: AMEAS, ASMO, AEST, ALSP, ALSQ, ALS
real, dimension(KPARS,KPARS)        :: SMSING, SMSING0
type(smoothness_estimates)          :: SMSEST
real, dimension(KPARS,KPARS,KIMAGE) :: UFS
real, dimension(:,:,:), ALLOCATABLE :: UFS_errest
integer, dimension(KIP,KWM,KIMAGE)  :: MNOISEI


!multi-pixel
real, dimension(:,:), allocatable :: UF_temp
real                           :: AB_edge
real, dimension(KPARS,KIMAGE) :: FF_edge, AAI
type(segment_edges)     ::  edges
integer                        :: IKSIM, NISM, alloc_stat
real, dimension(KIMAGE,KPARS,KIMAGE)        :: SMMULTI
integer, dimension(KMPSM)               :: KSM
integer, dimension(KMPSM,KIMAGE)        :: IMSM
logical            ::  ledges
type(ind_clouds)               :: index_clouds
logical            ::  edges_present, test_ufnz
integer                            :: KM_segm,KM1_segm,KM1_segm_temp, I, IIQ, IKQ, IS1
integer                            :: IERR
real                           :: AQBEFORE, AQAFTER, AAQ
real, dimension(KPARS,KIMAGE) :: FFMSQ, FFMS0
real                           :: AFS0
real                           :: AGEN
real, dimension(KPAR)                  :: FFMI0,QIM_temp
!
!    ------------------------------------------------------------------------------------------
type(pixel_vector),dimension(KIMAGE) :: segment_vec_meas_temp
type(pixel_vector),dimension(KIMAGE) :: segment_vec_fit_temp

type(pixel_vector),dimension(KIMAGE) :: segment_vec_meas
type(pixel_vector),dimension(KIMAGE) :: segment_vec_fit
!    ------------------------------------------------------------------------------------------
integer                            :: INVSING !,NW
real, dimension(KW,KIMAGE)         :: tau_mol
integer                            :: NHVP_meas ! number of heights for vertical profile
real, dimension(KVERTM,KIMAGE)     :: HVP_meas ! heights for vertical profile
!    ------------------------------------------------------------------------------------------
integer, dimension(KIMAGE)         :: KM1_pix
real, dimension(KPARS,KIMAGE)      :: FFS0,FFS
real, dimension(KIMAGE)            :: ARES2
!    ------------------------------------------------------------------------------------------
character(len=20)  ::  sparse_solver_name
!    ------------------------------------------------------------------------------------------
logical            ::  IPRI_additional_info
logical            ::  lresult !, ledges
logical, dimension(KPARS,KIMAGE) :: dermask
logical, dimension(KPARS)        :: dermask_aerpar  ! if I dont use remove from argument
!  ------------------------------------------------------------------------------------------
REAL, DIMENSION(KMESS,KIMAGE) :: FMEAS,FFIT
real, dimension(KPARS,KIMAGE) :: QSE_temp
real*8, dimension(KPAR)       :: bE_temp
real,dimension(KW)            ::  MDPR=0

!MEH:
real                           :: TCINT, EPSQ_test
!------------------------------------------------------------------------------------------

GOUT_temp = GOUT
!check this:
GOUT_temp%retrieval%fit%segment_fit = segment_meas

segment_vec_meas_temp = segment_vec_meas
segment_vec_fit_temp = segment_vec_fit
solver_timer_temp = 0.0
npixels = segment_meas%npixels
IPRI_additional_info = RIN%IPRI_additional_info
KNF     = npixels*RIN%KNSINGF

!!--------------------------------------
!!MEH: Allocate Compressed Column Storage (sparse matrix)
  call allocate_sparse_matrix_storage ( UFNZ_temp, KNF*(RIN%KNSINGF+npixels-1), KNF  )


!!--------------------------------------
ipixstart = 1
select case(INVSING)
case(0)   ! single pixel
   ipixstop = npixels

!------------------------------------------------------------------------------------------
    !! MEH: Add bias in measurements
    if (any(RIN%NOISE%BIAS_EQ(1:RIN%NOISE%INOISE) .ne. 0.0 )) then
        !WRITE(*,*) 'EXP(segment_vec_meas)', EXP(segment_vec_meas_temp(1)%FS(1:120))
    !! MEH: Assign noise indices to measurement types
        call assign_noise_index(RIN,segment_meas,MNOISEI)
    !! MEH: If aplicable, add bias to segment measurement vector
        call add_bias_segment ( RIN,                           & ! IN
                                    segment_meas,              &
                                    segment_vec_meas_temp,     & ! INOUT
                                    MNOISEI,                    & ! IN
                                    option_bias)
        if ( error_present() ) return

        !WRITE(*,*) 'ipix, EXP(segment_vec_meas)', ipix, EXP(segment_vec_meas_temp(1)%FS(1:120))

        ccor_min = RIN%ccor_min
        ccor_max = RIN%ccor_max
        ccor     = ccor_min

        !! MEH: CS - calculate covariance matrix for the measuerements with bias
          call covariance_matrix_segment (RIN,INVSING,           & ! IN
                                          segment_meas,          &
                                          segment_vec_meas_temp,      &
                                          MNOISEI(:,:,:),        &
                                          IKI(:,:),              &
                                          IKI_shift(:,:),        &
                                          KNOISEI(:,:,:),        & ! OUT
                                          CS(:,:),ARES2(:)       &
                                         )


            call smoothterm_single_pixel_smoothness ( RIN, & ! IN
                                                        IKS, SMSING, SMSEST, & ! OUT
                                                        apriori_smooth  &
                                                      )
            if ( error_present() ) return
            call smoothterm_single_pixel_apriori ( RIN,  & ! IN
                                                     IKS1,SMSING0,apriori_estim  & ! OUT
                                                   )
            if ( error_present() ) return



            !! MEH: residual (F-FP)^T C (F-FP)
            do ipix=1,npixels
                KMIMAGE=segment_vec_fit(ipix)%KMIMAGE
                call residual_meas_term ( IPRI_additional_info,iu_main_output,  & ! IN
                                          KMIMAGE,                  &
                                          segment_vec_meas_temp(ipix)%FS(1:KMIMAGE),  &
                                          segment_vec_fit_temp(ipix)%FS(1:KMIMAGE),   &
                                          CS(1:KMIMAGE,ipix),       &
                                          AMEAS(ipix)               & ! OUT
                                        )
                if (apriori_smooth) &
                call residual_apriori_smooth_term ( IPRI_additional_info,iu_main_output, & ! IN
                                                    RIN%KNSINGF,             &
                                                    AP(1:RIN%KNSINGF,ipix),  &
                                                    SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                                    SMSEST,                  &
                                                    ASMO(ipix)               & ! OUT
                                                  )
!WRITE(*,*) 'ASMO(ipix)', ASMO(ipix)
                if (apriori_estim) &
                call residual_apriori_estim_term (  IPRI_additional_info,iu_main_output,  & ! IN
                                                    RIN%KNSINGF,              &
                                                    AP(1:RIN%KNSINGF,ipix),   &
                                                    AP0(1:RIN%KNSINGF,ipix),  &
                                                    SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                                    AEST(ipix)                & ! OUT
                                                 )


KM1_pix(ipix) =0.0

KMIMAGE = segment_vec_meas(ipix)%KMIMAGE
KM1_pix(ipix) = KMIMAGE - RIN%KNSINGF

                ccor = AMEAS(ipix)/(ARES2(ipix)*KMIMAGE)
                if(ccor .lt. ccor_min) ccor = ccor_min
                if(ccor .gt. ccor_max) ccor = ccor_max
                ALSP(ipix) = AMEAS(ipix)
                ALSP(ipix)  = (ALSP(ipix)/ KM1_pix(ipix))


        !do ipix=ipixstart,ipixstop
        !   KMIMAGE=segment_vec_meas_temp(ipixstop)%KMIMAGE
        !   write(*,*) 'deriv AP:'
        !   write(*,'(10es14.4)') EXP(AP(1:RIN%KNSING,ipix))
        !   write(*,*) 'FS:'
        !   write(*,'(10es14.4)') EXP(segment_vec_fit_temp(ipix)%FS(1:KMIMAGE))
        !   write(*,*) 'FS_deriv:'
        !   write(*,'(10es14.4)') pixel_vec_fit_deriv(ipix)%FS(1:KMIMAGE)
        !enddo


    !!-------------------------------------------
    !! MEH: New jacobians matrix is calculated from the measurements with added bias (US_temp)
    !!-------------------------------------------
    ! MEH: AP (the solution) is the initial guess
    ! Recalculate the residual (for measurements with bias and the fit as input data)
          US_temp(:,:,ipix)  = 0.0  !- matrix of Jacobians U
          UFS(:,:,ipix) = 0.0  !- Fisher matrix: (U)^T(C)^(-1) U +...
          FFMS(:,ipix)  = 0.0  !- gradient: contribution of measurements
          FFS(:,ipix)   = 0.0  !- contribution of single-pixel smoothness constraints to the gradient
          FFS0(:,ipix)  = 0.0  !- contribution of a priori estimates to the gradient

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
                    ALSP,                                          &
                    GOUT_temp%retrieval%fit%segment_fit%pixels,          &
                    pixel_vec_fit_deriv,                            &
                    GOUT_temp%aerosol,                                   &
                    GOUT_temp%gases,                                     &
                    GOUT_temp%surface,                                   &
                    GOUT_temp%retrieval,                                 &
                    nangles,                                        &
                    sca_angles,                                     &
                    KERNELS1,                                       &
                    KERNELS2,                                       &
                    APMIN,                                          &
                    APMAX,                                          &
                    dermask,                                        &
                    US_temp                                         &
              )


        if ( error_present() ) return



    !! MEH: Calculate the new gradient and fisher matrix
            call FISHMX ( KMIMAGE,RIN%KNSINGF,   &
                          segment_vec_meas_temp(ipix)%FS(:), &
                          segment_vec_fit_temp(ipix)%FS(:),  &
                          CS(:,ipix),            &
                          US_temp(:,:,ipix),          &
                          UFS(:,:,ipix),         & ! OUT
                          FFMS(:,ipix)           &
                       )

          if(apriori_smooth) then
            call gradient_apriori_smooth_term ( RIN%KNSINGF,                 &  ! IN
                                                AP(1:RIN%KNSINGF,ipix),      &
                                                SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                                SMSEST,                      &
                                                FFS(1:RIN%KNSINGF,ipix)      & ! OUT
                                              )
             UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) = &
             UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) + SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF)

          endif
          if(apriori_estim) then
            call gradient_apriori_estim_term (  RIN%KNSINGF,                & ! IN
                                                AP(1:RIN%KNSINGF,ipix),     &
                                                AP0(1:RIN%KNSINGF,ipix),    &
                                                SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF), &
                                                FFS0(1:RIN%KNSINGF,ipix)    & ! OUT
                                             )

             UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) = &
             UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) + SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF)

          endif
    enddo !ipix

    !! MEH: Selection of the inversion method
    if(RIN%IMQ .eq. 1 .or. RIN%IMQ .eq. 2) then
      do ipix=ipixstart,ipixstop
        QSE_temp(:,ipix)  = 0.0
        call ITERQ (  RIN%IMQ,RIN%KNSINGF,                    & ! IN
                      UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix),  &
                      FFMS(1:RIN%KNSINGF,ipix),               &
                      QSE_temp(1:RIN%KNSINGF,ipix)                  & ! OUT
                   )
      enddo ! ipix
    elseif(RIN%IMQ .eq. 3) then
    ! MEH: New QS for the measurement with added bias: QS_temp
      do ipix=ipixstart,ipixstop
        QSE_temp(:,ipix)  = 0.0
          call UF_nonzero_one_pixel ( RIN%KNSINGF,UFS(:,:,ipix), & ! IN
                                      UFNZ_temp,nnz                   & ! OUT
                                    )
          if ( error_present() ) return
          bE_temp(1:RIN%KNSINGF) = FFMS(1:RIN%KNSINGF,ipix)
          if(sparse_solver_name .eq. 'SUPERLU') &
              call solver_init_SUPERLU(RIN%KNSINGF, nnz, UFNZ_temp, bE_temp)
              call sparse_solver ( iu_main_output,       & ! IN
                                   RIN%KNSINGF,nnz,UFNZ_temp, &
                                   bE_temp,                    & ! INOUT
                                   solver_timer_temp          &
                                 )
              if ( error_present() ) then
          if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_cleanup_SUPERLU(RIN%KNSINGF, nnz, UFNZ_temp, bE_temp)
            return
          end if
          QSE_temp(1:RIN%KNSINGF,ipix) = bE_temp(1:RIN%KNSINGF)
          call sparse_solver_cleanup(RIN%KNSINGF, nnz, UFNZ_temp, bE_temp)
      enddo ! ipix
    endif !RIN%IMQ
endif ! bias

ALS_temp = ALSP

case(1,2) ! multi pixel
   ipixstop = npixels
!   write(*,*) '!!! Warning: Error estimtes: Systematic component of error without assumed bias not provided yet. It is under progress.'


!------------------------------------------------------------------------------------------
    !! MEH: Add bias in measurements
    if (any(RIN%NOISE%BIAS_EQ(1:RIN%NOISE%INOISE) .ne. 0.0 )) then
!        WRITE(*,*) 'EXP(segment_vec_meas)', EXP(segment_vec_meas_temp(1)%FS(1:120))
    !! MEH: Assign noise indices to measurement types
        call assign_noise_index(RIN,segment_meas,MNOISEI)
    !! MEH: If aplicable, add bias to segment measurement vector
        call add_bias_segment ( RIN,                           & ! IN
                                    segment_meas,              &
                                    segment_vec_meas_temp,     & ! INOUT
                                    MNOISEI,                    & ! IN
                                    option_bias)
        if ( error_present() ) return

!        WRITE(*,*) 'ipix, EXP(segment_vec_meas)', ipix, EXP(segment_vec_meas_temp(1)%FS(1:120))



    !! MEH: CS - calculate covariance matrix for the measuerements with bias
          call covariance_matrix_segment (RIN,INVSING,           & ! IN
                                          segment_meas,          &
                                          segment_vec_meas_temp,      &
                                          MNOISEI(:,:,:),        &
                                          IKI(:,:),              &
                                          IKI_shift(:,:),        &
                                          KNOISEI(:,:,:),        & ! OUT
                                          CS(:,:),ARES2(:)       &
                                         )

           ! write(*,*) 'CS(1:10,ipix)', CS(1:10,1)

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



    ! Multi pixel constrains: a priori smoothness multi-pixel constraints
    !         if(IPRI_additional_info)  write(iu_main_output,*) 'Before smoothterm_multi_pixel'
     call set_index_clouds ( RIN, segment_meas, index_clouds )
     call smoothterm_multi_pixel ( iu_main_output,RIN, & ! IN
                                   index_clouds,      &
                                   NISM,KSM,IMSM,     & ! OUT
                                   IKSIM,SMMULTI      &
                                 )

    !*** Multi pixel constrains: a priori estimates of retrieved parameters near
    !***                         the edges of the inverteded segment
        FF_edge  (:,:) = 0.0
        AB_edge        = 0.0

        ledges =  &
        edges_present(iu_main_output,segment_meas%NX,segment_meas%NY,segment_meas%NT,RIN,edges)
!        if(IPRI_additional_info)  write(iu_main_output,*) 'Before smoothterm_mutli_pixel_edges, ledges=',ledges
        if(ledges)  &
        call smoothterm_mutli_pixel_edges(  iu_main_output,RIN,index_clouds,edges,  &   ! IN
                                            NISM,KSM,IKSIM,IMSM,SMMULTI,            &   ! INOUT
                                            FF_edge,AB_edge ) ! OUT
    !        if(IPRI_additional_info)  write(iu_main_output,*) 'After smoothterm_mutli_pixel_edges'
        !write(*,*) 'after smoothterm_mutli_pixel_edges NISM,IKSIM: ',NISM,IKSIM
        !write(*,*) 'in inversion:  FF_edge(:,1): '
        !write(*,'(10e14.4)')  FF_edge(:,1)

        !write(*,*) 'AB_edge =',AB_edge,'  ledges =',ledges ! delete or comment after edges testing
        
    

    if(RIN%IMQ .eq. 2 .or. test_UFNZ) then
    !     Allocate arrays for UF matrix test
       allocate(UF_temp(KPAR,KPAR),stat=alloc_stat)
          if (alloc_stat /= 0) stop 'error while trying to allocate UF'
    endif ! RIN%IMQ .le. 2 .or.
      

    !

    ! Numbers of virtual degree of freedom for single pixel and multipixel to be used
    ! for residual calculation and
    ! for scalling of a priori constrains by measurement residual
          KM_segm = SUM(segment_vec_meas_temp(1:npixels)%KMIMAGE)
          KM1_segm_temp = 0
          do ipix=1,npixels
             KMIMAGE = segment_vec_meas_temp(ipix)%KMIMAGE
             KM1_pix(ipix) = KMIMAGE - RIN%KNSINGF
             if(apriori_smooth) KM1_pix(ipix) = KM1_pix(ipix) + IKS
             if(apriori_estim)  KM1_pix(ipix) = KM1_pix(ipix) + IKS1
             KM1_segm_temp = KM1_segm_temp + KM1_pix(ipix)
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
!            if(KM1_segm .ge. 0) then
!              ERREST%segm = .true.
!            endif
            if(KM1_segm .le. 0) then
              KM1_segm    = KM_segm
            endif
          endif

    write(*,*) 'KM1_pix', KM1_pix(1:npixels)

    ccor_min = RIN%ccor_min
    ccor_max = RIN%ccor_max
    ccor     = ccor_min

    ! Residual
    do ipix=1,ipixstop
     KMIMAGE=segment_vec_meas_temp(ipix)%KMIMAGE

    ! residual: measurment term (F-FP)^T (C)^(-1)(F-FP)
     call residual_meas_term (  IPRI_additional_info,iu_main_output,  & ! IN
                                KMIMAGE,                              &
                                segment_vec_meas_temp(ipix)%FS(1:KMIMAGE), &
                                segment_vec_fit_temp (ipix)%FS(1:KMIMAGE), &
                                CS(1:KMIMAGE,ipix),                   &
                                AMEAS(ipix)                           & ! OUT
                             )

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

     ccor = AMEAS(ipix)/(ARES2(ipix)*KMIMAGE)
     if(ccor .lt. ccor_min) ccor = ccor_min
     if(ccor .gt. ccor_max) ccor = ccor_max
     ALS(ipix) = AMEAS(ipix)
     if(apriori_smooth)  ALS(ipix) = ALS(ipix) + ASMO(ipix)
     if(apriori_estim)   ALS(ipix) = ALS(ipix) + AEST(ipix)
     ALS (ipix) = ALS (ipix)/KM1_pix(ipix)
     ALSP(ipix) = ALS(ipix)
    enddo !ipix


    ccor = SUM(AMEAS(1:npixels))/  &
           SUM(ARES2(1:npixels)*segment_vec_meas_temp(1:npixels)%KMIMAGE)
    if(ccor .lt. ccor_min) ccor = ccor_min
    if(ccor .gt. ccor_max) ccor = ccor_max
    AGEN = SUM(AMEAS(1:npixels))
!    if(apriori_smooth)  AGEN = AGEN+ccor*SUM(ASMO(1:npixels))
!    if(apriori_estim)   AGEN = AGEN+ccor*SUM(AEST(1:npixels))
    !C*****************************************************************************************
    !C***   inter-pixel smoothness term: (AP)^T (D_inter)^T(D_inter) AP) + edge terms (see subroutine)
    !C***                                 See  Dubovik et al. [2011]
    !C***      call residual_inter_pixel_term(IPRI_additional_info,iu_iP_main_output,RIN%KNSINGF,npixels,AP,SMMULTI,AFS0)
    !C*****************************************************************************************

    call residual_inter_pixel_term(IPRI_additional_info,iu_main_output,RIN%KNSINGF,npixels,AP,SMMULTI, &
                                   ledges,FF_edge,AB_edge,AFS0)
    AGEN = (AGEN+ccor*AFS0)/KM1_segm


    !C*** DERIVATIVES CALCULATION:

    DO ipix=1,ipixstop
      US_temp(:,:,ipix)  = 0.0  !- matrix of Jacobians U
      UFS(:,:,ipix) = 0.0  !- Fisher matrix: (U)^T(C)^(-1) U +...
      FFMS(:,ipix)  = 0.0  !- gradient: contribution of measurements
      FFS(:,ipix)   = 0.0  !- contribution of single-pixel smoothness constraints to the gradient
      FFS0(:,ipix)  = 0.0  !- contribution of a priori estimates to the gradient
    !  if ( ERREST%product ) then
    !  if ( .not. ERREST%LM_matrix ) then
    !  UFS_errest(:,:,ipix) = 0.0  !- Fisher matrix: (U)^T(C)^(-1) U +... for error estimates
    !  endif
    !  endif
    ENDDO



    DO ipix=1,ipixstop
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
                GOUT_temp%retrieval%fit%segment_fit%pixels,          &
                pixel_vec_fit_deriv,                            &
                GOUT_temp%aerosol,                                   &
                GOUT_temp%gases,                                     &
                GOUT_temp%surface,                                   &
                GOUT_temp%retrieval,                                 &
                nangles,                                        &
                sca_angles,                                     &
                KERNELS1,                                       &
                KERNELS2,                                       &
                APMIN,                                          &
                APMAX,                                          &
                dermask,                                        &
                US_temp                                              &
          )
    if ( error_present() ) return

    !write(*,*) 'US_temp(:,:)', US_temp(1:5,1:5,1)


    !! Fisher Matrix
    DO ipix=1,ipixstop
    !******************************************************************************
    ! Measurement term in Fisher matrix for each pixel (Up)^T W^(-1) (Up);  UFS(:,:,ipix)
    ! Measurement term in gradient of minimized residual for each pixel (Up)^T W^(-1) (fp - f)); FFMS(:,ipix)
    !******************************************************************************

     KMIMAGE = segment_vec_meas_temp(ipix)%KMIMAGE
    !write(*,*) 'ipix, KMIMAGE', ipix, KMIMAGE

     CALL FISHMX ( KMIMAGE,RIN%KNSINGF,   &
                   segment_vec_meas_temp(ipix)%FS(:), &
                   segment_vec_fit_temp(ipix)%FS(:),  &
                   CS(:,ipix),            &
                   US_temp(:,:,ipix),          &
                   UFS(:,:,ipix),         & ! OUT
                   FFMS(:,ipix)           &
                )

    !write(*,*) 'UFS(:,:,:)', UFS(1:5,1:5,1)
    !write(*,*) 'FFMS(:,:)', FFMS(1:5,1)

!C*****************************************************
!C*   INCLUSION of SMOOTHNESS  AND A PRIORI  ESTIMATES *
!C********************************************************************************
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
         UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) + SMSING(1:RIN%KNSINGF,1:RIN%KNSINGF)

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
         UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix) + SMSING0(1:RIN%KNSINGF,1:RIN%KNSINGF)

      ENDIF
ENDDO ! ipix

    !write(*,*) 'UFS(1:RIN%KNSINGF,1:RIN%KNSINGF,ipix)', UFS(1:5,1:5,1)


!********************************************************************************
!**           Solving linear system of equation for
!**                    each p-iterations
!********************************************************************************
QSE_temp(:,:)  = 0.0
QIM_temp(:) = 0.0
FFMS0(:,:) = 0.0 ! ask Oleg
DO I=1,RIN%KNSINGF
  DO IS1=1,npixels
!**********************************************************************************************
!*** Adding inter-pixel smothness contributions ONLY!!! to the diagonal ellement of Fisher matrix
    UFS(I,I,IS1) = UFS(I,I,IS1) + SMMULTI(IS1,I,IS1)

  ENDDO ! IS1
ENDDO ! I

!**********************************************************************************************
! Solving the linear system:
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
                      1.0,SMMULTI,            &
                      UFNZ_temp,nnz                 & ! OUT
                    )
    if ( error_present() ) return
!tl          nnz_err = nnz
    !WRITE(*,*) 'nnz=',nnz
    if ( test_UFNZ ) call UFNZ_symmetry_check ( KNF,nnz,UFNZ_temp,UF_temp )

ELSE IF(RIN%IMQ .eq. 2 .or. test_UFNZ)  THEN ! SVD normal system solver for segment
    ! filling in UF matrix
    call UF_matrix (  RIN,KNF,npixels,UFS,        & ! IN
                      1.0,SMMULTI,NISM,KSM,IMSM, &
                      UF_temp,nnz                      & ! OUT
                   )
ELSE IF(RIN%IMQ .eq. 1)  THEN ! simple linear iterations ~ Dubovik et al. [1995] ~
    call matrix_Q_iter (RIN%KNSINGF,npixels,UFS,SMMULTI,NISM,KSM,IMSM,1.0,AAI)
    IKQ = INT(1e+7)/npixels  !IKQ = 10
ENDIF ! RIN%IMQ .eq. 3


IIQ = 0

!**********************************************************************************************
! inclusion of INTER-PIXEL SMOOTHNESS CONSTRAINTS See Dubovik et al.[2011, 2008])
!**********************************************************************************************
      call inter_pix_smooth_constr_incl ( RIN%KNSINGF,npixels,      &
                                          UFS,QSE_temp,FFMS,FFMS0,        &
                                          SMMULTI,NISM,KSM,IMSM,1.0, &
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

      
  IF(IIQ .EQ. 0 .OR. (IIQ .LT. IKQ .AND. EPSQ_test .GT. RIN%EPSQ)) THEN
    AQBEFORE = AQAFTER
    IF(RIN%IMQ .GE. 2) THEN
          FFMI0(:) = 0.0
          DO ipix=1,npixels
            DO I=1,RIN%KNSINGF
              FFMI0((ipix-1)*RIN%KNSINGF+I) = FFMSQ(I,ipix)
            ENDDO ! I
          ENDDO ! ipix

        IF(RIN%IMQ .eq. 2) THEN
!              if(RIN%IPRI_verbose)  write(iu_main_output,*) 'UF inversion by SVD'
          CALL ITERQ (  RIN%IMQ,KNF,           &
                        UF_temp(1:KNF,1:KNF),       &
                        FFMI0(1:KNF),          &
                        QIM_temp(1:KNF)             & ! OUT
                     )
        ELSEIF(RIN%IMQ .EQ. 3) THEN
            

          bE_temp(1:KNF) = FFMI0(1:KNF)

          !write(*,*) 'bE_temp(1:KNF)', bE_temp(1:KNF)

          !write(*,*) 'b:'
          !write(*,'(10e16.5)') b(1:KNF)
          !write(*,*) 'UFNZ:'
!              do i=1,nnz
!                write(*,'(2i5,e16.5)') UFNZ_temp%col(i),UFNZ_temp%row(i),UFNZ_temp%val(i)
!              enddo
          !if(RIN%IPRI_verbose)  write(iu_main_output,*)  &
          !'UF inversion, before sparse_solver ',trim(sparse_solver_name),'  nnz=',nnz
          if(sparse_solver_name .eq. 'SUPERLU') &
          call solver_init_SUPERLU(KNF, nnz, UFNZ_temp, bE_temp)
          call sparse_solver (  iu_main_output, & ! IN
                                KNF,nnz,UFNZ_temp,      &
                                bE_temp,                 & ! INOUT
                                solver_timer_temp       &
                             )
          !write(*,*) 'new bE_temp(1:KNF)', bE_temp(1:KNF)

          if ( error_present() ) then
            if(sparse_solver_name .eq. 'SUPERLU') &
            call solver_init_SUPERLU(KNF, nnz, UFNZ_temp, bE_temp)
            return
          end if
          !if(RIN%IPRI_verbose)  write(iu_main_output,*)  &
          !'UF inversion, after  sparse_solver ',trim(sparse_solver_name)
          QIM_temp(1:KNF) = bE_temp(1:KNF)
          !write(*,*) 'QIM_temp(1:KNF)', QIM_temp(1:KNF)

          call sparse_solver_cleanup(KNF, nnz, UFNZ_temp, bE_temp)
        ENDIF ! RIN%IMQ .eq. 2
    ENDIF ! RIN%IMQ .ge. 2
  ENDIF

ALS_temp = ALS


endif
case default
  write(tmp_message,'(a,i0,a)') 'INVSING = ',INVSING,' value is not valid'
  G_ERROR(trim(tmp_message))
end select
!    -------------------------------------------------------------------------
! MEH:    Deallocate Compressed Column Storage (sparse matrix)
      call deallocate_sparse_matrix_storage ( UFNZ_temp )
!!***************************************************************************
!!     Deallocate arrays UF
!      if(allocated(UF_temp)) then
!         deallocate(UF_temp,stat=alloc_stat)
!         if (alloc_stat /= 0) stop 'error while trying to deallocate UF'
!      endif
end subroutine BIAS_estimates


!! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
