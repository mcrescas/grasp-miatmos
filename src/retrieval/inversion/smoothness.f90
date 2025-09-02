! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

!> @file smoothness.f90
        !> File contains routines related to smoothness constrains.
        !>
        !> @authors Oleg Dubovik and Tatsiana Lapionak
        !> 

! file contains:
! subroutine inter_pix_smooth_constr_incl
! subroutine ordsing_for_smooth_single_pixel
! SUBROUTINE smoothterm_single_pixel_apriori
! SUBROUTINE smoothterm_single_pixel_smoothness
! SUBROUTINE smoothterm_multi_pixel
! SUBROUTINE SMOOM
! SUBROUTINE DIFERW
! SUBROUTINE DIFERW_dp
! SUBROUTINE smoothterm_mutli_pixel_edges
! SUBROUTINE MAT_T_MAT

#include "../constants_set/mod_globals.inc"

!mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

module mod_smoothness_estimates

    use mod_par_inv, only : KIDIM2, KIDIM1, KPARS

    implicit none

    type :: smoothness_estimates
      logical :: apply
      real, dimension(KPARS) :: EST
      real, dimension(KPARS) :: WGT
      real, dimension(KPARS,KPARS) :: DIF
      real, dimension(KPARS,KPARS) :: DIF_WGT
    end type smoothness_estimates

end module mod_smoothness_estimates

!mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine calculates the change of quadratic form gradient at p - interation
        !> @brief for the entire segment of pixels with inclusion of inter-pixel constraints.
        !> 
        !!> @param[in]      KNSING  - number of parameters driving forwar model
        !!> @param[in]      npixels - number of inverted pixels
        !!> @param[in]      UFS     - Fisher matrix 
        !!> @param[in]      QS      - 
        !!> @param[in]      FFMS    - gradient: contribution of measurements
        !!> @param[in]      FFMS0   - contribution of a priroi estimates to the gradient
        !!> @param[in]      SMMULTI - multipixel smoothness matrix
        !!> @param[in]      NISM    - number of groups in which smoothness can be implemented
        !!> @param[in]      KSM     - number of elements in each of NISM groups
        !!> @param[in]      IMSM    - pixel numbers which belong to the group
        !!> @param[in]      ccor    -
        !!> @param[out]     FFMSQ   - 
        !>
               
      subroutine inter_pix_smooth_constr_incl ( KNSING,npixels,           &
                                                UFS,QS,FFMS,FFMS0,        &
                                                SMMULTI,NISM,KSM,IMSM,ccor, &
                                                FFMSQ                     &
                                              )

      use mod_par_inv, only : KPARS,KIMAGE,KMPSM
            
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!***********************************************************************************
!> This subroutine calculates the change of quadratic form gradient at p - interation
!> for the entire segment of pixels with inclusion of inter-pixel constraints :
!
! the normal system: gradient = (UF) QS - Fp
!
!
! where:  UF = (Up)^T W^(-1) (Up) + ((D_singl)^T(D_singl)) + Wa^(-1) + (D_inter)^T (D_inter)
!         FP =  (UF) Ap - [(Up)^T W^(-1)(fp - f) + Wa^(-1)(Ap-A0) + (D_inter)^T (D_inter)Ap + AB_edge]
!
!     *NOTE* we represent: UF=UF_diag + UF_non_diag, i.e.:
!         (UF) QS = (UF_diag + UF_non_diag) QS
!         FP = FP1 + FP2; 
!            - FP1- all contribution used already in sinngle pixel retrieval;
!              FP1 =(Up)^T W^(-1)(fp - f)+Wa^(-1)(Ap-A0) +((D_singl)^T(D_singl))Ap;
!            - FP2- the contribution using multi-pixel constraints and "edge" information
!              FP2 = (D_inter)^T (D_inter)Ap + FF_edge
!***********************************************************************************
      implicit none
! -------------------------------------------------------------------------------        
      integer,                            intent(in)  :: KNSING,npixels,NISM
      real,                               intent(in)  :: ccor
      real,dimension(KPARS,KPARS,KIMAGE), intent(in)  :: UFS
      integer,dimension(KMPSM),           intent(in)  :: KSM
      integer,dimension(KMPSM,KIMAGE),    intent(in)  :: IMSM 
      real,dimension(KIMAGE,KPARS,KIMAGE),intent(in)  :: SMMULTI
      real,dimension(KPARS,KIMAGE),       intent(in)  :: QS
      real,dimension(KPARS,KIMAGE),       intent(in)  :: FFMS,FFMS0
      real,dimension(KPARS,KIMAGE),       intent(out) :: FFMSQ
! -------------------------------------------------------------------------------  
      integer                      :: ipix,I,IS,IS1,IS2
      real                         :: TAAQ1,TAAQ2,TAAQ3,TAAQ4
      real,dimension(KPARS,KIMAGE) :: FMSU0

! -------------------------------------------------------------------------------
      FMSU0(:,:) = 0.0
      do IS=1,NISM
        do I=1,KNSING
          do IS1=1,KSM(IS)
            do IS2=1,KSM(IS)
              if(IS1 .ne. IS2) FMSU0(I,IMSM(IS,IS1))=FMSU0(I,IMSM(IS,IS1))+  &          ! (UF_non_diag) QS
                               ccor*SMMULTI(IMSM(IS,IS1),I,IMSM(IS,IS2))*QS(I,IMSM(IS,IS2))
            enddo ! IS2
          enddo ! IS1      
        ENDDO ! I
      enddo ! IS

      FFMSQ(:,:) = 0.0
      do ipix=1,npixels
        do I=1,KNSING
          TAAQ1 = DOT_PRODUCT(UFS(I,1:KNSING,ipix),QS(1:KNSING,ipix)) !(UF_diag) QS
          TAAQ2 = FMSU0(I,ipix) ! (UF_non_diag) QS
          TAAQ3 = FFMS0(I,ipix) ! FP2
          TAAQ4 = FFMS(I,ipix)  ! FP1
          FFMSQ(I,ipix) = TAAQ1+TAAQ2-TAAQ3-TAAQ4
!write(100,*) 'ipix= ',ipix,' I= ',I
!write(100,*) TAAQ1,TAAQ2,TAAQ3,TAAQ4,' - TAAQ1,TAAQ2,TAAQ3,TAAQ4'
        enddo ! I 
      enddo  ! ipix

      return
      end subroutine inter_pix_smooth_constr_incl

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss      
        !!> @brief Routine calculates 
        !> 
        !!> @param[in]      KNSING  - number of parameters driving forwar model
        !!> @param[in]      npixels - number of inverted pixels
        !!> @param[in]      UFS     - Fisher matrix
        !!> @param[in]      SMMULTI - multipixel smoothness matrix
        !!> @param[in]      NISM    - number of groups in which smoothness can be implemented
        !!> @param[in]      KSM     - number of elements in each of NISM groups
        !!> @param[in]      IMSM    - pixel numbers which belong to the group
        !!> @param[in]      ccor    -
        !!> @param[out]     AAI     - 
        !>
         
      subroutine matrix_Q_iter (KNSING,npixels,UFS,SMMULTI,NISM,KSM,IMSM,ccor,AAI)

      use mod_par_inv, only : KPARS,KIMAGE,KMPSM
            
      implicit none
! -------------------------------------------------------------------------------        
      integer,                            intent(in)  :: KNSING,npixels,NISM
      real,                               intent(in)  :: ccor
      real,dimension(KPARS,KPARS,KIMAGE), intent(in)  :: UFS
      integer,dimension(KMPSM),           intent(in)  :: KSM
      integer,dimension(KMPSM,KIMAGE),    intent(in)  :: IMSM 
      real,dimension(KIMAGE,KPARS,KIMAGE),intent(in)  :: SMMULTI
      real,dimension(KPARS,KIMAGE),       intent(out) :: AAI
! -------------------------------------------------------------------------------  
      integer :: ipix,I,I1,IS,IS1,IS2
! -------------------------------------------------------------------------------  
      
      AAI(:,:) = 0.0
      do ipix=1,npixels
        do I=1,KNSING
          do I1=1,KNSING
            AAI(I,ipix) = AAI(I,ipix)+ABS(UFS(I,I1,ipix))
          enddo
        enddo
        do IS=1,NISM
          do I=1,KNSING
            do IS1=1,KSM(IS)
              do IS2=1,KSM(IS)
                if(IS1 .NE. IS2) AAI(I,IMSM(IS,IS1))=AAI(I,IMSM(IS,IS1))+  &
                                 ccor*ABS(SMMULTI(IMSM(IS,IS1),I,IMSM(IS,IS2)))
              enddo
            enddo      
          enddo
        enddo
      enddo ! ipix 

      return
      end subroutine matrix_Q_iter

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !!> @brief Routine calculates ordinates to be used for non-equal binning 
        !!> @brief (aerosol particle radii and wave lengths) in smoothness.
        !> 
        !!> @param[in]      RIN     - invertion setting structure
        !!> @param[inout]   ORDSING (KIDIM3,KIDIM2,KIDIM1) is used for non-equal binning (aerosol particle radii and wave lengths) in smoothness.
        !
        
      subroutine ordsing_for_smooth_single_pixel(RIN,ORDSING)
      
! ORDSING will be used for non-equal binning (aerosol particle radii and wave lengths) in smoothness.

      use mod_retr_settings_derived_type 
      use mod_par_inv, only : KIDIM1,KIDIM2,KIDIM3
      
      implicit none 
!	-------------------------------------------------------------------------
      type(retr_input_settings),            intent(in)     :: RIN
      real,dimension(KIDIM3,KIDIM2,KIDIM1), intent(inout)  :: ORDSING

!	-------------------------------------------------------------------------
      integer   ::  IDIM1,IDIM2,IDIM3,par_type
!	-------------------------------------------------------------------------
!	-------------------------------------------------------------------------
      ORDSING(:,:,:) = 0.0 

      do IDIM1=1,RIN%NDIM%n1
      par_type = RIN%NDIM%par_type(IDIM1)
      do IDIM2=1,RIN%NDIM%n2(IDIM1) 
      do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
        if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then
          if(RIN%IBIN .eq. 1)  ORDSING(IDIM3,IDIM2,IDIM1) = RIN%RADIUS1(IDIM3,IDIM2)
          if(RIN%IBIN .eq. -1) ORDSING(IDIM3,IDIM2,IDIM1) = LOG(RIN%RADIUS1(IDIM3,IDIM2)) 
        elseif(par_type .gt. par_type_RERI_beg .and. par_type .lt. par_type_IMRI_end) then
!OD       ORDSING(IDIM3,IDIM2,IDIM1)=RIN%WAVE(IDIM3)
          ORDSING(IDIM3,IDIM2,IDIM1)=LOG(RIN%WAVE(IDIM3))
        elseif(par_type .gt. par_type_SHD_beg  .and. par_type .lt. par_type_SHD_end) then
          ORDSING(IDIM3,IDIM2,IDIM1)=RIN%RATIO1(IDIM3,IDIM2)
        !elseif(par_type .gt. par_type_AVP_beg  .and. par_type .lt. par_type_AVP_end) then
!OD       ORDSING(IDIM3,IDIM2,IDIM1)=RIN%WAVE(IDIM3)
        elseif(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF_water_end) then
!OD       ORDSING(IDIM3,IDIM2,IDIM1)=RIN%WAVE(IDIM3)
          ORDSING(IDIM3,IDIM2,IDIM1)=LOG(RIN%WAVE(IDIM3))
        endif ! par_type .gt. par_type_SD_beg .and.
      enddo  ! IDIM3
      enddo  ! IDIM2   
      enddo  ! IDIM1

      return
      end subroutine ordsing_for_smooth_single_pixel

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine determines smoothness term for single pixel (a priori estimates)
        !>
        !> @param[in]    RIN          - invertion setting structure
        !> @param[out]   KSM          -
        !> @param[out]   SMSING       - a priori estimates term of smoothness matrix for single pixel
        !> @param[out]   apriori_present - presence of a priori estimates
        !>

      SUBROUTINE smoothterm_single_pixel_apriori ( RIN,          & ! IN
                                                   KSM, SMSING, apriori_present  & ! OUT
                                                 )
!************************************************************
!*** Determining smoothness term for single pixel         ***
!************************************************************
      use mod_par_inv, only : KIDIM1, KIDIM2, KIDIM3, KPARS, KDIF
      use mod_retr_settings_derived_type

      implicit none
! ---------------------------------------------------------------------
! IN
      type(retr_input_settings), intent(in)  :: RIN
! ---------------------------------------------------------------------
! OUT
      integer,                          intent(out) :: KSM
      real, dimension(KPARS,KPARS),      intent(out) :: SMSING
      logical,                          intent(out) :: apriori_present
! ---------------------------------------------------------------------
! LOCAL 
      real, dimension(KDIF,KDIF)   :: DIF, SM1
      real, dimension(KDIF,KDIF)   :: DIFWGT
      integer :: I, I1, J, J1, KM1, IDIM1, IDIM2, IDIM3
      logical :: IPRINT2
      
      integer :: NDIM3, IO, ISTARSING
      real    :: GSM
      real    :: G, xnorm
! ---------------------------------------------------------------------
      IPRINT2 = .false.
      IF(IPRINT2) WRITE(*,*) "in smoothterm_single_pixel_apriori"
      KSM         = 0
      SMSING(:,:) = 0.0
      DIF(:,:) = 0.0
      xnorm = 0.0  ! in SMOOM smoothness matrix (SM1) is normalized

      DO IDIM1=1,RIN%NDIM%n1
      IF(.not. RIN%NDIM%par_retr(IDIM1)) EXIT
      DO IDIM2=1,RIN%NDIM%n2(IDIM1)
         NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
         ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
         DO IDIM3=1,NDIM3
            IO  = RIN%SPCA%IO(IDIM3,IDIM2,IDIM1)
            GSM = RIN%SPCA%GSM(IDIM3,IDIM2,IDIM1)
            IF(GSM .GT. 0.) THEN
              G = 1.0
              DO I=1,NDIM3
                DIF(I,I) = 1.0
              ENDDO
              KM1 = NDIM3-IO
              KSM = KSM+KM1
              IF(IPRINT2) WRITE(*,'(2(a,i0))') "before SMOOM KM1 = ",KM1,"  NDIM3 = ",NDIM3
              DIFWGT(:,:) = DIF(:,:)
              CALL SMOOM( KM1,NDIM3,      & ! IN
                          DIF(:,:), DIFWGT(:,:), &
                          SM1(:,:), xnorm & ! OUT
                        )
              IF(IPRINT2) WRITE(*,*) "after SMOOM"
              DO J1=1,NDIM3
              DO I1=1,NDIM3
                SMSING(ISTARSING+I1-1,ISTARSING+J1-1) = &
                SMSING(ISTARSING+I1-1,ISTARSING+J1-1)+GSM*SM1(I1,J1)
!                WRITE(*,*) ISTARSING+I1-1,ISTARSING+J1-1,SM1(I1,J1),' I1,J1,SM1'
              ENDDO ! I1
              ENDDO ! J1
            ENDIF ! GSM .GT. 0.
         ENDDO ! IDIM3
      ENDDO ! IDIM2
      ENDDO ! IDIM1
	   
      apriori_present = .false.
      DO I=1,RIN%KNSINGF
         IF(SMSING(I,I) .LE. 0.0) CYCLE
         apriori_present = .true.
         EXIT
      ENDDO

      IF(IPRINT2) WRITE(*,*) 'before return from smoothterm_single_pixel_apriori'

      RETURN
      END SUBROUTINE smoothterm_single_pixel_apriori

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine determines smoothness term for single pixel (a priori smoothness)
        !>
        !> @param[in]    RIN          - invertion setting structure
        !> @param[out]   KSM          - 
        !> @param[out]   SMSING       - smoothness term of smoothness matrix for single pixel
        !> @param[out]   SMSEST       - structure of single pixel smoothness estimates
        !> @param[out]   apriori_present - presence of a priori estimates
        !>

      subroutine smoothterm_single_pixel_smoothness ( RIN,                 & ! IN
                                                     KSM, SMSING, SMSEST, & ! OUT
                                                     apriori_present      &
                                                   )
!************************************************************
!*** Determining smoothness term for single pixel         ***
!************************************************************
      use mod_par_inv, only : KIDIM1, KIDIM2, KIDIM3, KPARS, KDIF
      use mod_retr_settings_derived_type
      use mod_smoothness_estimates
      use mod_stop_report

      implicit none
! ---------------------------------------------------------------------
! IN
      type(retr_input_settings), intent(in)  :: RIN
! ---------------------------------------------------------------------
! OUT
      integer,                     intent(out) :: KSM
      real, dimension(KPARS,KPARS), intent(out) :: SMSING
      type(smoothness_estimates),  intent(out) :: SMSEST
      logical,                     intent(out) :: apriori_present
! ---------------------------------------------------------------------
! LOCAL 
      real, dimension(:,:,:), allocatable :: ORDSING
      real, dimension(KDIF,KDIF)   :: DIF, DIFWGT, SM1
      real, dimension(KDIF)        :: XNEW
      integer :: I, I1, J, J1, KM1, IDIM1, IDIM2, IDIM3
      integer :: ibeg, iend1, iend2
      logical :: IPRINT2
      integer :: NDIM3, IO, ISTARSING
      real    :: GSM
      real    :: G, xnorm
      integer :: alloc_stat
      logical :: IPRI_verbose
! ---------------------------------------------------------------------
      IPRI_verbose = RIN%IPRI_verbose
! ---------------------------------------------------------------------
! initialize smothness estimate structure
      SMSEST%apply = .false.
      !if (any(RIN%SMS%EST(:,:,:) .ne. 0.0)) then ! commented OD 2021-11-16
      if (any(RIN%SMS%WGT(:,:,:) .ne. 1.0)) then
        SMSEST%apply = .true.
        !write(tmp_message,'(3a)') &
        !'Smoothness estimate option is not supported in current code version.', &
        !NEW_LINE('A'), &
        !'Smoothness estimates must be equal to 1., weights must be equal to 0. in settings.'
        !G_ERROR(trim(tmp_message))
      endif
      !endif
      SMSEST%EST(:) = 0.0
      SMSEST%WGT(:) = 1.0
      SMSEST%DIF(:,:)     = 0.0
      SMSEST%DIF_WGT(:,:) = 0.0

!C*****************************************************************************************
!C*** ODRSING will be used for non-equal binning in smoothness:
!C*****************************************************************************************
      allocate(ORDSING(KIDIM3,KIDIM2,KIDIM1),stat=alloc_stat)
      if (alloc_stat /= 0) then
        write(tmp_message,'(a)') &
        'error while trying to allocate ORDSING'
        G_ERROR(trim(tmp_message))
      endif

      call ordsing_for_smooth_single_pixel(RIN,ORDSING)

      IPRINT2 = .false. ! .true. !
      if ( IPRINT2 ) write(*,*) "in smoothterm_single_pixel_smoothness"
      KSM         = 0
      SMSING(:,:) = 0.0
      XNEW(:)     = 0.0
      xnorm = 0.0  ! in SMOOM smoothness matrix is normalized

      do IDIM1=1,RIN%NDIM%n1
      if ( .not. RIN%NDIM%par_retr(IDIM1) ) exit
      do IDIM2=1,RIN%NDIM%n2(IDIM1)
         NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
         ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
         IO        = RIN%SPCS%IO(IDIM2,IDIM1)
         GSM       = RIN%SPCS%GSM(IDIM2,IDIM1)
         if(IO .eq. 0 .and. GSM .gt. 0)  then
          write(tmp_message,'(4i4,es11.4,3a)') IDIM1,IDIM2,IDIM3,IO,GSM, &
          '  - IDIM1,IDIM2,IDIM3,IO,GSM', &
          NEW_LINE('A'), &
          'For single pixel smoothness constrains: IO=0 and GSM>0 combination is not valid.'
          G_ERROR(trim(tmp_message))
         endif

         if ( GSM .gt. 0.) then
            do IDIM3=1,NDIM3
            XNEW(IDIM3) = ORDSING(IDIM3,IDIM2,IDIM1)
            if ( IPRINT2 ) write(*,*) IDIM3,XNEW(IDIM3) ,' IDIM3,XNEW(IDIM3)'
            enddo ! IDIM3
            if ( IPRINT2 ) then
            write(*,*) "before DIFERW"
            write(*,*) IDIM3,IDIM2,IDIM1,IO,'  IDIM3,IDIM2,IDIM1, NDIM3(..), IO(..)'
            endif ! IPRINT2
			
            call DIFERW ( IPRI_verbose, &
                          NDIM3,IO,     & ! IN
                          XNEW(:),      &
                          G,DIF(:,:)    & ! OUT
                        )
            IF ( error_present() ) RETURN

            if ( IPRINT2 ) write(*,*) "after DIFERW"
!OD?        IF (G.NE.1) AA=SQRT(0.05*0.05/(G*RIN%SPCS%GSM(I)))

            KM1=NDIM3-IO
            KSM=KSM+KM1
            if ( IPRINT2 ) write(*,*) "before SMOOM"

            if ( IPRINT2 ) then
              write(*,*) NDIM3, IO, '  - NDIM3, IO'
              write(*,*) idim2,RIN%SMS%EST(1:KM1,IDIM2,IDIM1), &
                                   '  - idim2, RIN%SMS%EST(1:KM1,IDIM2,IDIM1)'
              write(*,*)
              write(*,*) idim2,RIN%SMS%WGT(1:KM1,IDIM2,IDIM1), &
                                   '  - idim2, RIN%SMS%WGT(1:KM1,IDIM2,IDIM1)'
            endif
            if ( SMSEST%apply ) then
              ibeg = ISTARSING
              iend1 = ibeg + NDIM3 - 1
              iend2 = ibeg + KM1 - 1
              do i=1,KM1
                SMSEST%WGT(ibeg+i-1) = RIN%SMS%WGT(i,IDIM2,IDIM1)
              enddo
              SMSEST%EST(ibeg:iend2) = RIN%SMS%EST(1:KM1,IDIM2,IDIM1)
              SMSEST%DIF(ibeg:iend2,ibeg:iend1) = DIF(1:KM1,1:NDIM3)
              do I=1,NDIM3
                DIFWGT(1:KM1,I) = SMSEST%WGT(ibeg:iend2)*DIF(1:KM1,I)
              enddo ! I
            else
                DIFWGT(:,:) = DIF(:,:) ! w/o smoothness estimates
            endif ! SMSEST%apply

            call SMOOM( KM1, NDIM3,             & ! IN
                        DIF(:,:), DIFWGT(:,:), &
                        SM1(:,:), xnorm        & ! OUT
                      )

            if ( IPRINT2 ) write(*,*) "after SMOOM"
            do I1=1,NDIM3
            do J1=1,NDIM3
               SMSING(ISTARSING+I1-1,ISTARSING+J1-1)=               &
               SMSING(ISTARSING+I1-1,ISTARSING+J1-1)+GSM*SM1(I1,J1)
!OD            WRITE(*,*) NDIM%ISTARSING(IDIM2,IDIM1)+I1-1,   &
!OD            NDIM%ISTARSING(IDIM2,IDIM1)+J1-1,SM1(I1,J1),'   I,J'
            enddo ! J1
            enddo ! I1
            if ( IPRINT2 ) then
              write(*,'(a,i0,2x,a)') 'ISTARSING = ',ISTARSING,'SMSING(ISTARSING+I1-1,ISTARSING+J1-1):'
              do J1=1,NDIM3
              write(*,'(i4,100es12.4)') J1,(SMSING(ISTARSING+I1-1,ISTARSING+J1-1),I1=1,NDIM3)
              enddo ! J1
            endif
            if ( SMSEST%apply ) then
              SMSEST%WGT(ibeg:iend2) = GSM*SMSEST%WGT(ibeg:iend2)
              SMSEST%DIF_WGT(ibeg:iend2,ibeg:iend1) = GSM*DIFWGT(1:KM1,1:NDIM3)
              if ( xnorm .ne. 0. ) then
              SMSEST%WGT(ibeg:iend1) = SMSEST%WGT(ibeg:iend1) / xnorm
              SMSEST%DIF_WGT(ibeg:iend1,ibeg:iend1) = &
              SMSEST%DIF_WGT(ibeg:iend1,ibeg:iend1) / xnorm
              endif ! xnorm .ne. 0.
            endif ! SMSEST%apply
		  endif ! GSM .GT. 0.
      enddo ! IDIM2
      enddo ! IDIM1

      !write(*,*) 'EST(1:RIN%KNSING)'
      !write(*,'(10es12.4)') SMSEST%EST(1:RIN%KNSING)
      !write(*,*) 'WGT(1:RIN%KNSING)'
      !write(*,'(10es12.4)') SMSEST%WGT(1:RIN%KNSING)
      !write(*,*) 'DIF(1:RIN%KNSING,1:RIN%KNSING)'
      !write(*,'(10es12.4)') SMSEST%DIF(1:RIN%KNSING,1:RIN%KNSING)
      !write(*,*) 'DIF_WGT(1:RIN%KNSING,1:RIN%KNSING)'
      !write(*,'(10es12.4)') SMSEST%DIF_WGT(1:RIN%KNSING,1:RIN%KNSING)

      !stop 'stop test in smoothterm_single_pixel_smoothness'

      apriori_present = .false.
      do I=1,RIN%KNSINGF
         if ( SMSING(I,I) .le. 0.0 ) cycle
         apriori_present = .true.
         exit
      enddo

      deallocate(ORDSING,stat=alloc_stat)
      if (alloc_stat /= 0) stop &
      'error while trying to deallocate ORDSING in smoothterm_single_pixel_smoothness'

      if ( IPRINT2 ) write (*,*) 'before return from smoothterm_single_pixel_smoothness'

      return
      end subroutine smoothterm_single_pixel_smoothness

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine determines smoothness term for milti pixel inversion
        !> 
        !> @param[in]    iu_main_output - type of constrains 'smoothness' or 'apriori' estimates
        !> @param[in]    RIN            - invertion setting structure
        !> @param[in]    index_clouds   - contains indices for recurcive function CHECK  
        !> @param[out]   NISM           - number of groups in which smoothness can be implemented
        !> @param[out]   KSM            - number of elements in each of NISM groups 
        !> @param[out]   IMSM           - pixel numbers which belong to the group
        !> @param[out]   IKSIM          - number of equivalent measurements defining smoothness (used for residual)
        !> @param[out]   SMMULTI        - smoothness term of smoothness matrix for multi pixel
        !>

SUBROUTINE smoothterm_multi_pixel ( iu_main_output,RIN,  & ! IN
                                    index_clouds,       &
                                    NISM,KSM,IMSM,      & ! OUT
                                    IKSIM,SMMULTI       &
                                 )

      use mod_par_inv, only : KIMAGE,KITIME,KIX,KIY,KPARS,KIMAGE,KMPSM,KDIF
      use mod_index_cloud
      use mod_retr_settings_derived_type
      use mod_stop_report
      
      IMPLICIT NONE

! ------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu_main_output
      type(ind_clouds),            intent(in)  ::  index_clouds
      type(retr_input_settings),   intent(in)  ::  RIN
! ------------------------------------------------------------
! OUT :
      INTEGER,                             INTENT(OUT) :: NISM,IKSIM
      INTEGER,DIMENSION(KMPSM),            INTENT(OUT) :: KSM   
      INTEGER,DIMENSION(KMPSM,KIMAGE),     INTENT(OUT) :: IMSM  
      REAL,DIMENSION(KIMAGE,KPARS,KIMAGE), INTENT(OUT) :: SMMULTI 

      !type(mp_smoothness),dimension(KIMAGE)                    :: mps
! ------------------------------------------------------------
! LOCAL :	  	  
      INTEGER                   :: NYSM,NXSM,NTSM 
      INTEGER,DIMENSION(KIX)    :: IXX
      INTEGER,DIMENSION(KIY)    :: IYY
      INTEGER,DIMENSION(KITIME) :: ITT

      REAL,DIMENSION(KDIF,KDIF) :: DIF, SM1
      REAL,DIMENSION(KDIF,KDIF) :: DIFWGT
      REAL,DIMENSION(KDIF)      :: XNEW
      DOUBLE PRECISION, DIMENSION(KDIF) :: XNEW_dp

      INTEGER :: ITIME,IX,IY,INX,INY,IDIM1,IDIM2,IDIM3,   &
	               I1,I2,J1,KM1,INT,IS,IS1,IS2
      LOGICAL :: IPRINT2, IPRI_verbose
      REAL    :: G, xnorm
      REAL    :: GSMX,GSMY,GSMT
      INTEGER :: NDIM3,ISTARSING,IOX,IOY,IOT
! ------------------------------------------------------------

      IPRI_verbose = RIN%IPRI_verbose

! ------------------------------------------------------------
! KMPSM - parameter, max number of groups for multi-pixel smoothness
! NISM  - number of groups in which smoothness can be implemented
! IKSIM - number of equivalent measurements defining smoothness 
!         (used for residual)   
! KSM (1:KMPSM)           - number of elements in each of NISM groups
! IMSM(1:KMPSM,1:KIMAGE)  - pixel numbers which belong to the group
! SMMULTI(1:KIMAGE,1:KPARS,1:KIMAGE)  - smoothness matrix
! ------------------------------------------------------------
!moved into read inversion setting input
      !if(KITIME .gt. KDIF) then 
	    !write(*,*) 'KITIME=',KITIME,' .gt. KDIF=',KDIF
	    !write(*,*) 'Check inversion parameters: KDIF should be max(KPARS,KITIME,KIX,KIY).'
	    !stop 'stop in smoothterm_multi_pixel'
      !endif
      !if(KIX .gt. KDIF) then 
	    !write(*,*) 'KIX=',KIX,' .gt. KDIF=',KDIF
	    !write(*,*) 'Check inversion parameters: KDIF should be max(KPARS,KITIME,KIX,KIY).'
	    !stop 'stop in smoothterm_multi_pixel'
      !endif
      !if(KIY .gt. KDIF) then 
	    !write(*,*) 'KIY=',KIY,' .gt. KDIF=',KDIF
	    !write(*,*) 'Check inversion parameters: KDIF should be max(KPARS,KITIME,KIX,KIY).'
	    !stop 'stop in smoothterm_multi_pixel'
      !endif	  

      xnorm = 0.0  ! smoothnes matrix is normalized
      XNEW(:)   = 0.0

      IPRINT2   = .false.
      NISM      = 0
      IKSIM     = 0
      KSM(:)    = 0
      IMSM(:,:) = 0
      SMMULTI(:,:,:)  = 0.0
      !SMIM2(:,:,:)  = 0.0
	  
      IF(IPRINT2) WRITE(*,*) "in smoothterm_multi_pixel" 

! ** ITIME, IX

      DO ITIME=1,index_clouds%NT
      DO IX=1,index_clouds%NX
        NYSM = 0

        DO IY=1,index_clouds%NY
        IF(index_clouds%ICLOUD(IX,IY,ITIME) .GT. 0) THEN
        NYSM = NYSM + 1
        XNEW(NYSM) = index_clouds%Y(IX,IY,ITIME)
        IYY(NYSM)  = index_clouds%INIMAGE(IX,IY,ITIME)
        IF(IPRINT2) WRITE(*,*) ITIME,IX,IY,NYSM,XNEW(NYSM),'  1: ITIME,IX,IY,NYSM,XNEW(NYSM)'
        ENDIF
        ENDDO ! IY

        IF(NYSM .GT. 1) THEN
          NISM=NISM+1
          KSM(NISM)=NYSM
          DO INY=1,NYSM
          IMSM(NISM,INY)=IYY(INY)
          ENDDO ! INY

          DO IDIM1=1,RIN%NDIM%n1
          IF(.not. RIN%NDIM%par_retr(IDIM1)) EXIT
          DO IDIM2=1,RIN%NDIM%n2(IDIM1) 
            !WRITE(*,'(7i5,/a)') ITIME,IX,IDIM2,IDIM1,'  ITIME,IX,IDIM2,IDIM1'
            NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
            ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
            IOY       = RIN%MPCS%IOY(IDIM2,IDIM1)
            GSMY      = RIN%MPCS%GSMY(IDIM2,IDIM1)
            IF(GSMY .GT. 0. .AND. IOY .GT. 0) THEN
            CALL DIFERW ( IPRI_verbose, &
                          NYSM,IOY,     & ! IN
                          XNEW(:),      &
                          G,DIF(:,:)    & ! OUT
                        )
            IF ( error_present() ) RETURN

            DIFWGT(:,:) = DIF(:,:)

            DO I2=1,NDIM3
              KM1 = NYSM-IOY
              IF(KM1 .LE. 0) KM1 = 1
              CALL SMOOM( KM1,NYSM,               & ! IN
                          DIF(:,:), DIFWGT(:,:), &
                          SM1(:,:), xnorm        & ! OUT
                         )
              IKSIM=IKSIM+1
              DO I1=1,NYSM
              DO J1=1,NYSM             
              !SMIM(NISM,ISTARSING+I2-1,I1,J1)=                &
              !SMIM(NISM,ISTARSING+I2-1,I1,J1)+GSMY*SM1(I1,J1)
              SMMULTI(IMSM(NISM,I1),ISTARSING+I2-1,IMSM(NISM,J1)) =  &
              SMMULTI(IMSM(NISM,I1),ISTARSING+I2-1,IMSM(NISM,J1)) + GSMY*SM1(I1,J1)
              ENDDO ! J1
              !if ( IDIM1 .ge. 7 ) then
              !write(*,'(a,i0,2x,a)') 'I1 = ',I1,'** ITIME, IX SM1(I1,1:NYSM):'
              !write(*,'(10es13.5)') SM1(I1,1:NYSM)
              !endif
              ENDDO ! I1
            ENDDO ! I2
            ENDIF ! GSMY .GT. 0. .AND. IOY .GT. 0
          ENDDO ! IDIM2
          ENDDO ! IDIM1
        ENDIF ! NYSM .GT. 1

      ENDDO ! IX
      ENDDO ! ITIME				
      IF(IPRINT2) write(*,'(a,i5)') '1: nism=',nism 

! ** ITIME, IY
	   
      DO ITIME=1,index_clouds%NT
      DO IY=1,index_clouds%NY

        NXSM = 0
        DO IX=1,index_clouds%NX
        IF(index_clouds%ICLOUD(IX,IY,ITIME) .GT. 0) THEN
        NXSM=NXSM+1
        XNEW(NXSM)=index_clouds%X(IX,IY,ITIME)
        IXX(NXSM)=index_clouds%INIMAGE(IX,IY,ITIME)
        IF(IPRINT2) WRITE(*,*) ITIME,IY,IX,NXSM,XNEW(NXSM),'  2: ITIME,IY,IX,NXSM,XNEW(NXSM)'
        ENDIF
        ENDDO ! IX
         
        IF(NXSM .GT. 1) THEN
          NISM=NISM+1
          KSM(NISM)=NXSM
          DO INX=1,NXSM
          IMSM(NISM,INX)=IXX(INX)
          ENDDO ! INX
		 
          DO IDIM1=1,RIN%NDIM%n1
          IF(.not. RIN%NDIM%par_retr(IDIM1)) EXIT
          DO IDIM2=1,RIN%NDIM%n2(IDIM1) 
            !WRITE(*,'(7i5,/a)') ITIME,IY,IDIM2,IDIM1,'  ITIME,IY,IDIM2,IDIM1'
            NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
            ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
            IOX       = RIN%MPCS%IOX(IDIM2,IDIM1)
            GSMX      = RIN%MPCS%GSMX(IDIM2,IDIM1)
            IF(GSMX .GT. 0. .AND. IOX .GT. 0) THEN
            CALL DIFERW ( IPRI_verbose, &
                          NXSM,IOX,     & ! IN
                          XNEW(:),      &
                          G,DIF(:,:)    & ! OUT
                        )
            IF ( error_present() ) RETURN

            DIFWGT(:,:) = DIF(:,:)

            DO I2=1,NDIM3
              KM1 = NXSM-IOX
              IF(KM1 .LE. 0) KM1 = 1
              CALL SMOOM( KM1,NXSM,              & ! IN
                          DIF(:,:), DIFWGT(:,:), &
                          SM1(:,:), xnorm        & ! OUT
                        )
              IKSIM=IKSIM+1
              DO I1=1,NXSM
              DO J1=1,NXSM
              !SMIM(NISM,ISTARSING+I2-1,I1,J1)=    &
              !SMIM(NISM,ISTARSING+I2-1,I1,J1)+GSMX*SM1(I1,J1)
              SMMULTI(IMSM(NISM,I1),ISTARSING+I2-1,IMSM(NISM,J1)) =  &
              SMMULTI(IMSM(NISM,I1),ISTARSING+I2-1,IMSM(NISM,J1)) + GSMX*SM1(I1,J1)		 
              ENDDO ! J1
              !if ( IDIM1 .ge. 7 ) then
              !write(*,'(a,i0,2x,a)') 'I1 = ',I1,'** ITIME, IY SM1(I1,1:NXSM):'
              !write(*,'(10es13.5)') SM1(I1,1:NXSM)
              !endif
              ENDDO ! I1
            ENDDO ! I2
            ENDIF ! GSMX .GT. 0. .AND. IOX .GT. 0
          ENDDO ! IDIM2
          ENDDO ! IDIM1
        ENDIF ! NXSM .GT. 1

      ENDDO ! IY
      ENDDO ! ITIME
      IF(IPRINT2) write(*,'(a,i5)') '2: nism=',nism

! ** IX, IY

      DO IX=1,index_clouds%NX
      DO IY=1,index_clouds%NY

        NTSM=0
        DO ITIME=1,index_clouds%NT
        IF(index_clouds%ICLOUD(IX,IY,ITIME) .GT. 0) THEN
        NTSM=NTSM+1
        !write(*,*) 'dble(T)=',dble(index_clouds%T(IX,IY,ITIME))
        XNEW_dp(NTSM)=dble(index_clouds%T(IX,IY,ITIME))
        ITT(NTSM)=index_clouds%INIMAGE(IX,IY,ITIME)
        IF(IPRINT2) WRITE(*,*) IX,IY,ITIME,NTSM,XNEW_dp(NTSM),'  3: IX,IY,ITIME,NTSM,XNEW(NTSM)'
        ENDIF
        ENDDO ! ITIME

        IF(NTSM .GT. 1) THEN
          NISM=NISM+1
          KSM(NISM)=NTSM
          DO INT=1,NTSM
          IMSM(NISM,INT)=ITT(INT)
          ENDDO ! INT

          DO IDIM1=1,RIN%NDIM%n1
          IF(.not. RIN%NDIM%par_retr(IDIM1)) EXIT
          DO IDIM2=1,RIN%NDIM%n2(IDIM1) 
            !WRITE(*,'(7i5,/a)') IX,IY,IDIM2,IDIM1,'  IX,IY,IDIM2,IDIM1'
            NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
            ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
            IOT       = RIN%MPCS%IOT(IDIM2,IDIM1)
            GSMT      = RIN%MPCS%GSMT(IDIM2,IDIM1)
            IF(GSMT .GT. 0. .AND. IOT .GT. 0) THEN
            CALL DIFERW_dp ( IPRI_verbose, &
                          NTSM,IOT,     & ! IN
                          XNEW_dp(:),   &
                          G,DIF(:,:),   & ! OUT
                          RIN%MPCS%time_diff_threshold_sec &  ! IN
                           )
            IF ( error_present() ) RETURN

            DIFWGT(:,:) = DIF(:,:)

            DO I2=1,NDIM3
              KM1 = NTSM-IOT
              IF(KM1 .LE. 0) KM1 = 1
              CALL SMOOM( KM1,NTSM,              & ! IN
                          DIF(:,:), DIFWGT(:,:), &
                          SM1(:,:), xnorm        & ! OUT
                        )
              IKSIM=IKSIM+1
              DO I1=1,NTSM
              DO J1=1,NTSM        
              SMMULTI(IMSM(NISM,I1),ISTARSING+I2-1,IMSM(NISM,J1)) =  &
              SMMULTI(IMSM(NISM,I1),ISTARSING+I2-1,IMSM(NISM,J1)) + GSMT*SM1(I1,J1) 
              ENDDO ! J1
              !if ( IDIM1 .ge. 7 ) then
              !write(*,'(4(a,i0,2x),a,es13.5,2x,a)') 'I1 = ',I1,'IDIM1 = ',IDIM1,'IDIM2 = ', &
              !IDIM2,'IDIM3 = ',I2,'xnorm=',xnorm,'** IX, IY SM1(I1,1:NTSM):'
              !write(*,'(10es13.5)') SM1(I1,1:NTSM)
              !endif
              ENDDO ! I1
            ENDDO ! I2
            ENDIF ! GSMT .GT. 0. .AND. IOT .GT. 0
          ENDDO ! IDIM2
          ENDDO ! IDIM1
        ENDIF ! NTSM .GT. 1

      ENDDO ! IY
      ENDDO ! IX
		IF(IPRINT2) write(*,'(a,i5)') '3: nism=',nism		

      IF(NISM .GT. KMPSM) THEN
         write(tmp_message,'(2(a,i0,1x))') 'NISM = ',NISM,'.GT. KMPSM = ',KMPSM
         G_ERROR(trim(tmp_message))
      ENDIF
	  
      IF(IPRINT2) THEN
         WRITE(*,*) 'END of smoothterm_multi_pixel!!!'
         WRITE(*,'(a12,2i5)') 'NISM,IKSIM: ',NISM,IKSIM
         DO IS=1,NISM
         write(*,'(a12,2i5)') 'KSM(IS),IS: ',KSM(IS),IS
         DO IS1=1,KSM(IS)
         write(*,*) IMSM(IS,IS1),IS, IS1,' IMSM(IS,IS1),IS, IS1' 
         ENDDO ! IS1
         ENDDO ! IS
      ENDIF ! IPRINT2

!**************
! TL
		!type mp_smoothness

			!integer					             ::  ksm
			!integer,dimension(KIMAGE,KPARS)	 ::  imsm	
			!real,   dimension(KIMAGE,KPARS)  ::  smim	

		!end type mp_smoothness

      !mps(1:KIMAGE)%ksm =       
      !mps(1:KIMAGE)%imsm(1:KIMAGE,1:KPARS) = 0
      !do i1=1,KIMAGE
      !mps(i1)%smim(1:KIMAGE,1:KPARS) = 0.0
      !enddo ! i1

!goto 66
      !SMMULTI(:,:,:)  = 0.0
      !DO IS=1,NISM
      !DO I1=1,KNSINGF
      !DO IS1=1,KSM(IS)
      !DO IS2=1,KSM(IS)
         !IF(IS1 .NE. IS2) THEN
            !SMMULTI(IMSM(IS,IS1),I1,IMSM(IS,IS2)) = SMIM(IS,I1,IS1,IS2)
            !mps(IMSM(IS,IS2))%smim(IMSM(IS,IS1),I1) = SMMULTI(IMSM(IS,IS1),I1,IMSM(IS,IS2))
            !IF(IMSM(IS,IS1) .GT. KIMAGE .OR. IMSM(IS,IS2) .GT. KIMAGE)  &
            !STOP 'STOP in smoothterm_multi_pixel: SMMULTI index problem'	
            !write(*,*) IMSM(IS,IS1),I1,IMSM(IS,IS2),	' - IMSM(IS,IS1),I1,IMSM(IS,IS2)'	   
         !ENDIF ! IS1 .NE. IS2
      !ENDDO ! IS2
      !ENDDO ! IS1      
		!ENDDO ! I1
      !ENDDO ! IS
!66 continue
!**************
!stop 'stop: test in smoothterm_multi_pixel'

! NISM  - number of groups in which smoothness can be implemented
! IKSIM - number of equivalent measurements defining smoothness 
      if(RIN%IPRI_verbose) then
        write(iu_main_output,'(a)') 'in smoothterm_multi_pixel:'
        write(iu_main_output,'(4x,2(a,i0,2x))') 'NISM = ',NISM,'IKSIM = ',IKSIM
      endif

RETURN
END SUBROUTINE smoothterm_multi_pixel

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine calculates smoothness matrix
        !> 
        !> @param[in]    KM  - number of rows
        !> @param[in]    KN  - number of columns
        !> @param[in]    DIF - matrix of differences 
        !> @param[in]    DIFWGT - matrix of differences with weights
        !> @param[out]   SM1 - smoothness matrix
        !> @param[inout] xnorm - smoothness normalization coefficient
        !>

      SUBROUTINE SMOOM( KM, KN, DIF, DIFWGT, SM1, xnorm )

!**************************************************
!  THIS SUBROUTINE CALCULATES "SMOOTHNESS MATRIX":
!           SM1=(DIF)^T (DIF)
!**************************************************
!  INPUT:
!        KM  I        - number of lines
!        KN  I        - number of columns
!        DIF R(KM,KN) - matrix of differences
!        DIFWGT R(KM,KN) - matrix of differences multiplied by
!                          vector of weights in smoothness matrix
!  OUTPUT:
!        SM1 R(KN,KN)  - smoothness matrix
!                        SM=(DIF)**T(C)(DIF)
!        SM1 is normalized if input xnorm=0.0
!            is not normalized if input xnorm=-999.0
!        GF           - the parameter coming from
!              DELTA(ordinate), this parameter can be
!              can be used for indicating norm of
!              of derivatives A assumed in Lagrange
!              parameter:
!               Gamma=Sigma(mes)**2/(GF*A**2)
!                see Dubovik & King [2000]
!***************************************************
      USE mod_par_inv, only : KDIF
	  
      IMPLICIT NONE
! IN :
      INTEGER, INTENT(IN) ::  KM, KN
      REAL, DIMENSION(KDIF,KDIF), INTENT(IN) :: DIF
      REAL, DIMENSION(KDIF,KDIF), INTENT(IN) :: DIFWGT
! OUT :
      REAL, DIMENSION(KDIF,KDIF), INTENT(OUT) :: SM1
      REAL, INTENT(INOUT) :: xnorm
! LOCAL :
      INTEGER :: I, I1
      LOGICAL :: lstop = .false.
!C***************************************************

      IF(KDIF .LT. KM) THEN
        WRITE(*,'(3(a,i0))') 'KDIF = ',KDIF,' .lt. KM = ',KM, &
        ' not valid combination; KDIF must be greater than KM'
         lstop = .true.
      ENDIF
      IF(KDIF .LT. KN) THEN
        WRITE(*,'(3(a,i0))') 'KDIF = ',KDIF,' .lt. KN = ',KN, &
        ' not valid combination; KDIF must be greater than KN'
         lstop = .true.
      ENDIF
      if(lstop) &
      stop 'stop in SMOOM'

      SM1(1:KN,1:KN) = 0.
!do i=1,2
!write(*,*) 'in SMOOM: DIF',DIF(1:2,I)
!enddo

      IF(KN .GE. 1) THEN  ! CD 06/09/2013
          DO I =1,KN
          DO I1=1,KN
            SM1(I,I1) = SUM(DIF(1:KM,I)*DIFWGT(1:KM,I1))
            !write(*,*) I,I1,SM1(I,I1),DIFWGT(1:KM,I1),'  - i,i1,SM1(I,I1),DIFWGT(1:KM,I1)'
          ENDDO ! I1
          ENDDO ! I

!OD 2015-09-15 Smoothness matrix normalization
! For edges normalization is also used in subroutines DIF_before_calcul
! and DIF_after_calcul
        IF( xnorm .NE. -999.0 ) THEN
          ! search for maximal diagonal element
          xnorm = 0.
          DO I=1,KN
            IF(xnorm .LT. SM1(I,I)) xnorm = SM1(I,I)
          ENDDO ! I

          IF(xnorm .NE. 0.) THEN
            DO I =1,KN
            DO I1=1,KN
              SM1(I,I1) = SM1(I,I1) / xnorm
            ENDDO ! I1
!write(*,*) 'in SMOOM: I=',I,'  SM1=',SM1(I,1:KN)
            ENDDO ! I
!write(*,*) 'in SMOOM: xnorm=',xnorm
          ENDIF ! xnorm .NE. 0.
!write(*,*) 'in SMOOM: xnorm=',xnorm
        ENDIF ! xnorm .NE. -999.0
      ENDIF ! KN.GT.1

      RETURN
      END SUBROUTINE SMOOM  

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine defines matrix of n-th differences
        !> 
        !> @param[in]    IPRI_verbose - print info for grasp verbose mode
        !> @param[in]    KN  - number of parameters
        !> @param[in]    X_dp  - values of ordinates Xi of the grid point
        !> @param[in]    time_diff_sec  - value of time difference in seconds
        !> @param[inout] IO  - order of differences
        !> @param[out]   G_sp  - coefficient relating differences with derivatives
        !> @param[out]   DIF_sp - matrix of differences
        !>

      SUBROUTINE DIFERW_dp ( IPRI_verbose, KN, IO, X_dp, G_sp, DIF_sp, time_diff_threshold_sec_sp )
!C**********************************************************
!C*** THIS SUBROUTINE DEFINES MATRIX of N-th DIFFERENCES ***
!C*** for the function Y(X) defined in the points Xi     ***
!C    INPUT:
!C        KN     I   - number of parameters
!C        IO     I   - order of differences
!C        X(KN)  DP  - the values of ordinates (or time) Xi of the ***
!C                     grid point
!C
!C***
!C    OUTPUT:
!C      G    R   - coefficient relating differences with
!C                 derivatives
!C            - G.NE.1 if Xi+1-Xi is non constant 
!C            - G=1/(A**IO) if Xi+1-Xi=const=A
!C              this parameter can be used for indicating norm
!C              of derivatives A assumed in Lagrange parameter: 
!C               Gamma=Sigma(mes)**2/(GF*A**2)
!C                see Dubovik & King [2000]
!C      DIF  R(KN-IO,KN) -matrix of IO-th differences     
!C****                                                   ***
!C**********************************************************
      USE mod_par_inv, only : KDIF
      USE mod_stop_report

      IMPLICIT NONE
! ----------------------------------------------------------
! IN :
      LOGICAL, INTENT(IN)    ::  IPRI_verbose
      INTEGER, INTENT(IN)    ::  KN
      INTEGER, INTENT(INOUT) ::  IO ! do not change INOUT
      DOUBLE PRECISION, DIMENSION(KDIF), INTENT(IN) :: X_dp
      REAL, INTENT(IN) :: time_diff_threshold_sec_sp
! OUT :
      REAL,                     INTENT(OUT)  ::  G_sp
      REAL, DIMENSION(KDIF,KDIF),INTENT(OUT)  ::  DIF_sp
! ----------------------------------------------------------
! LOCAL :
      DOUBLE PRECISION :: G_dp
      DOUBLE PRECISION, DIMENSION(KDIF,KDIF) :: DIF_dp
      DOUBLE PRECISION, DIMENSION(KN) :: DX_dp, DX1_dp
      DOUBLE PRECISION, DIMENSION(KN,KN) :: DIFT_dp
      INTEGER :: I, J, II, IIO
      LOGICAL :: diff_const
      DOUBLE PRECISION, PARAMETER :: tiny = 1.0d-3
      !DOUBLE PRECISION, PARAMETER :: time_diff_sec = 10800d+0 ! 3 hours
      DOUBLE PRECISION :: DD
      DOUBLE PRECISION :: time_diff_threshold_sec_dp
! ----------------------------------------------------------
!      write(*,*) "AH KDIF = ",KDIF,"  size(X) = ",size(X)
      G_sp = 0.0
      DIF_sp (:,:) = 0.0  ! CD 06/09/2013

      G_dp = 0.0d0
      DIF_dp (:,:) = 0.0d0  ! CD 06/09/2013
      DIFT_dp(:,:) = 0.0d0  ! CD 06/09/2013

      IF(IO .GE. KN) THEN 
         IF(IPRI_verbose .EQV. .TRUE.) &
         WRITE(*,*) IO,KN,' IO,KN, IO.GE.KN - PROBLEM with SMOOTHNESS!!!'
         IO=KN-1
         IF(IPRI_verbose .EQV. .TRUE.) &
         WRITE(*,*) IO,KN,' IO,KN  - IO has been changed    !!!'
         IF(IO .eq. 0) RETURN 
      ENDIF ! IO
    
      IF(KN .GE. 1) THEN ! CD 06/09/2013
         DO J=1,KN
            DIFT_dp(J,J) = 1.0d0
            DX_dp(J)     = X_dp(J)
         ENDDO ! J

!write(*,*) 'in DIFERW IO=',IO,'  KN=',KN
         IF(IO .GT. 0) THEN
!C**********************************************************
!C*** Checking if Xi+1-Xi=const or not                   ***
			   diff_const = .true.
         IF(KN .GT. 2) THEN
            DO J=2,KN-1
            IF(ABS(((X_dp(J-1)-X_dp(J))-(X_dp(J)-X_dp(J+1)))/(X_dp(J-1)-X_dp(J))) .GT. tiny) THEN
            diff_const = .false.
            EXIT 
            ENDIF
            ENDDO ! J
         ELSEIF(KN .GT. 1) THEN
!write(*,*) 'in DIFERW X:',X(1:2)
            IF(ABS((X_dp(2)-X_dp(1))/X_dp(1)) .GT. tiny) THEN
            diff_const = .false. 
            ENDIF    
         ENDIF ! KN.GT.2

         IF(diff_const) THEN
!C**********************************************************
!C*** Calculating matrix of differences and G            ***
!C***   for  Xi+1-Xi=const                               ***
               G_dp = 1.0d0/(ABS((X_dp(1)-X_dp(2)))**IO)
               IF(G_dp .NE. G_dp) THEN
               WRITE(tmp_message,'(2(a,es12.4,1x))') 'G =',G_dp, &
               '!!!  (X(1)-X(2)) =',(X_dp(1)-X_dp(2))
               G_ERROR(trim(tmp_message))
               ENDIF
!CD            WRITE(6,*) KN,IO,' KN,IO'
               DO IIO=1,IO 
               DO I=1,KN-IIO
               DO J=1,KN
               DIF_dp(I,J)  = DIFT_dp(I,J)-DIFT_dp(I+1,J)
               ENDDO ! J
               ENDDO ! I
               
               DO J=1,KN
               DO I=1,KN-IIO
               DIFT_dp(I,J) = DIF_dp(I,J)
               ENDDO ! I
               ENDDO ! J
               ENDDO ! IIO
	        ELSE ! diff_const
!C**********************************************************
!C*** Calculating matrix of differences and G            ***
!C***   for  Xi+1-Xi not const                           ***
               G_dp = 1.0d0
!cs            write(6,*)'check5'
               DO I=1,KN
               DX_dp(I)  = X_dp(I)
               DX1_dp(I) = X_dp(I)
               ENDDO ! I
               DO IIO=1,IO
               DO I=1,KN-IIO
               DO J=1,KN
               IF((DX_dp(I)-DX_dp(I+1)) .EQ. 0.0d0) THEN
               WRITE(tmp_message,'(2es12.4,2x,a)') DX_dp(I),DX_dp(I+1), &
               '- DX(I), DX(I+1), DX=0 !!!'
               G_ERROR(trim(tmp_message))
               ENDIF
               IF ( time_diff_threshold_sec_sp .eq. 0e+0 ) THEN
                !DIF_dp(I,J) = 1.0_dp/ABS((DX_dp(I)-DX_dp(I+1)))*(DIFT_dp(I,J)-DIFT_dp(I+1,J))
                DIF_dp(I,J) = (DIFT_dp(I,J)-DIFT_dp(I+1,J))/ABS(DX_dp(I)-DX_dp(I+1))
               ELSE
                time_diff_threshold_sec_dp = time_diff_threshold_sec_sp
                DD = abs(DX_dp(I)-DX_dp(I+1))
                if ( DD .lt. time_diff_threshold_sec_dp ) then
                DD = time_diff_threshold_sec_dp
                endif
                DIF_dp(I,J) = (DIFT_dp(I,J)-DIFT_dp(I+1,J))/DD
               ENDIF
               ENDDO ! J
               ENDDO ! I
                
               DO I=1,KN-IIO
               DO J=1,KN
               DIFT_dp(I,J)=DIF_dp(I,J)
               ENDDO ! J
               ENDDO ! I
               DO I=1,KN-IIO
               DX_dp(I) = (DX1_dp(I)+DX1_dp(I+1))*0.5d0
               ENDDO ! I
               DO I=1,KN-IIO
               DX1_dp(I) = DX_dp(I)
               ENDDO ! I
               ENDDO ! IIO
	        ENDIF ! diff_const

! OD 2015-07-20
!          DIF(:,:) = 0.0
!          DO I=1,KN-IO
!          DO J=1,KN
!            DIF_dp(I,J) = DIFT_dp(I,J)
!          ENDDO ! J
!          ENDDO ! I

         ELSE ! IO 
!C*********For a priori estimates *************************
            DO J=1,KN
            DIF_dp(J,J) = 1.0d0  !CD DIF_dp(J,J)=0.0d0 !18-01-2013
            ENDDO ! J
            G_dp = 1.0d0
         ENDIF ! IO .GT. 0
      ENDIF ! KN .GT. 1

      G_sp = G_dp
      DIF_sp = DIF_dp

      RETURN
      END SUBROUTINE DIFERW_dp

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine defines matrix of n-th differences
        !> 
        !> @param[in]    KN  - number of parameters
        !> @param[in]    X  - values of ordinates Xi of the grid point
        !> @param[inout] IO  - order of differences
        !> @param[out]   G  - coefficient relating differences with derivatives
        !> @param[out]   DIF - matrix of differences 
        !>

      SUBROUTINE DIFERW(IPRI_verbose,KN,IO,X,G,DIF)
!C**********************************************************
!C*** THIS SUBROUTINE DEFINES MATRIX of N-th DIFFERENCES ***
!C*** for the function Y(X) defined in the points Xi     ***
!C    INPUT:
!C        KN     I   - number of parameters
!C        IO     I   - order of differences
!C        X(KN)  R   - the values of ordinates Xi of the ***
!C                     grid point
!C
!C***
!C    OUTPUT:
!C      G    R   - coefficient relating differences with
!C                 derivatives
!C            - G.NE.1 if Xi+1-Xi is non constant 
!C            - G=1/(A**IO) if Xi+1-Xi=const=A
!C              this parameter can be used for indicating norm
!C              of derivatives A assumed in Lagrange parameter: 
!C               Gamma=Sigma(mes)**2/(GF*A**2)
!C                see Dubovik & King [2000]
!C      DIF  R(KN-IO,KN) -matrix of IO-th differences     
!C****                                                   ***
!C**********************************************************
      USE mod_par_inv, only : KDIF
      USE mod_stop_report

      IMPLICIT NONE
! ----------------------------------------------------------
! IN :
      LOGICAL,             INTENT(IN)       ::  IPRI_verbose
      INTEGER,             INTENT(IN)       ::  KN
      INTEGER,             INTENT(INOUT)    ::  IO ! do not change INOUT
      REAL,DIMENSION(KDIF),INTENT(IN)       ::  X	
! ----------------------------------------------------------
! OUT :
      REAL,                     INTENT(OUT)  ::  G
      REAL,DIMENSION(KDIF,KDIF),INTENT(OUT)  ::  DIF	  
    
! ----------------------------------------------------------
! LOCAL :
      REAL,DIMENSION(KN)         ::  DX,DX1
      REAL,DIMENSION(KN,KN)      ::  DIFT      
      INTEGER                    ::  I,J,II,IIO
      LOGICAL                    ::  diff_const	  
! ----------------------------------------------------------
!      write(*,*) "AH KDIF = ",KDIF,"  size(X) = ",size(X)
      G = 0.0
      DIFT(:,:) = 0.  ! CD 06/09/2013
      DIF (:,:) = 0.  ! CD 06/09/2013 
      
      IF(IO .GE. KN) THEN 
         IF(IPRI_verbose .EQV. .TRUE.) &
         WRITE(*,*) IO,KN,' IO,KN, IO.GE.KN - PROBLEM with SMOOTHNESS!!!'
         IO=KN-1
         IF(IPRI_verbose .EQV. .TRUE.) &
         WRITE(*,*) IO,KN,' IO,KN  - IO has been changed    !!!'
         IF(IO .eq. 0) RETURN 
      ENDIF ! IO
    
      IF(KN .GE. 1) THEN ! CD 06/09/2013
         DO J=1,KN
            DIFT(J,J) = 1.0
            DX(J)     = X(J)
         ENDDO ! J

!write(*,*) 'in DIFERW IO=',IO,'  KN=',KN
         IF(IO .GT. 0) THEN
!C**********************************************************
!C*** Checking if Xi+1-Xi=const or not                   ***
			   diff_const = .true.
         IF(KN .GT. 2) THEN
            DO J=2,KN-1
            IF(ABS(((X(J-1)-X(J))-(X(J)-X(J+1)))/(X(J-1)-X(J))) .GT. 1.0e-3) THEN
            diff_const = .false.
            EXIT 
            ENDIF
            ENDDO ! J
         ELSEIF(KN .GT. 1) THEN
!write(*,*) 'in DIFERW X:',X(1:2)
            IF(ABS((X(2)-X(1))/X(1)) .GT. 1.0e-3) THEN
            diff_const = .false. 
            ENDIF    
         ENDIF ! KN.GT.2
         IF(diff_const) THEN
!C**********************************************************
!C*** Calculating matrix of differences and G            ***
!C***   for  Xi+1-Xi=const                               ***
               G = 1.0/(ABS((X(1)-X(2)))**IO)
               IF(G .NE. G) THEN
               WRITE(tmp_message,'(2(a,es12.4,2x))') 'G =',G, &
               '!!!  (X(1)-X(2)) =',(X(1)-X(2))
               G_ERROR(trim(tmp_message))
               ENDIF
!CD            WRITE(6,*) KN,IO,' KN,IO'
               DO IIO=1,IO 
               DO I=1,KN-IIO
               DO J=1,KN
               DIF(I,J)  = DIFT(I,J)-DIFT(I+1,J)
               ENDDO ! J
               ENDDO ! I
               
               DO J=1,KN
               DO I=1,KN-IIO
               DIFT(I,J) = DIF(I,J)
               ENDDO ! I
               ENDDO ! J
               ENDDO ! IIO

	        ELSE ! diff_const
!C**********************************************************
!C*** Calculating matrix of differences and G            ***
!C***   for  Xi+1-Xi not const                           ***
               G = 1.0
!cs            write(6,*)'check5'
               DO I=1,KN
               DX(I)  = X(I)
               DX1(I) = X(I)
               ENDDO ! I
               DO IIO=1,IO
               DO I=1,KN-IIO
               DO J=1,KN
               IF((DX(I)-DX(I+1)) .EQ. 0.0) THEN
               WRITE(tmp_message,'(2es12.4,2x,a)') DX(I),DX(I+1), &
               '- DX(I), DX(I+1), DX=0 !!!'
               G_ERROR(trim(tmp_message))
               ENDIF
               !DIF(I,J) = 1.0/ABS((DX(I)-DX(I+1)))*(DIFT(I,J)-DIFT(I+1,J))
               DIF(I,J) = (DIFT(I,J)-DIFT(I+1,J))/ABS(DX(I)-DX(I+1))
               ENDDO ! J
               ENDDO ! I
                
               DO I=1,KN-IIO
               DO J=1,KN
               DIFT(I,J)=DIF(I,J)
               ENDDO ! J
               ENDDO ! I
               DO I=1,KN-IIO
               DX(I)  = (DX1(I)+DX1(I+1))*0.5
               ENDDO ! I
               DO I=1,KN-IIO
               DX1(I) = DX(I)
               ENDDO ! I
               ENDDO ! IIO
	        ENDIF ! diff_const

! OD 2015-07-20
!          DIF(:,:) = 0.0
!          DO I=1,KN-IO
!          DO J=1,KN
!            DIF(I,J) = DIFT(I,J)
!          ENDDO ! J
!          ENDDO ! I

         ELSE ! IO 
!C*********For a priori estimates *************************
            DO J=1,KN
            DIF(J,J) = 1.0  !CD DIF(J,J)=0.0 !18-01-2013
            ENDDO ! J
            G = 1.0
         ENDIF ! IO .GT. 0
      ENDIF ! KN .GT. 1

      RETURN
      END SUBROUTINE DIFERW

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine calculates matrices for applying constraints forcing
        !> @brief smooth connection between the parameters at the edge of the block of 
        !> @brief inverted pixels and those retrieved perviouly outside of this block 
        !> @brief next to the edge 
        !>
        !> @author Oleg Dubovik and Tatsiana Lapionak
        !> @date 20 APR 2015
        !>
        !> @param[in]    iu_main_output - unit number of main output file of retrieval
        !> @param[in]    RIN - structure of settings for retrieval
        !> @param[in]    index_clouds   TO_BE_DOCUMENTED
        !> @param[in]    edges - structure of egdes data
        !> @param[inout] NISM  - number of groups in which smoothness can be implemented
        !> @param[inout] KSM   - number of elements in each of NISM groups 
        !> @param[inout] IKSIM - number of equivalent measurements defining smoothness (used for residual)
        !> @param[inout] IMSM  - pixel numbers which belong to the group
        !> @param[inout] SMMULTI - smoothness matrix
        !> @param[out]   FF_edge        TO_BE_DOCUMENTED
        !> @param[out]   AB_edge        TO_BE_DOCUMENTED 
               
! 2019-06-27
! NOTE! Time (index_clouds%T()) must be converted into double precision in order
! to be passed into DIFERW_dp insted of DIFERW as it was implemented in 
! subroutine smoothterm_mutli_pixel

      subroutine smoothterm_mutli_pixel_edges(  iu_main_output,RIN,index_clouds,edges,  & ! IN
                                                NISM,KSM,IKSIM,IMSM,SMMULTI,            & ! INOUT
                                                FF_edge,AB_edge )                ! OUT
!C*********************************************************
!C the subroutine prepares matrices for applying constraints forcing
!C smooth connection between the parameters at the edge of the block of 
!C inverted pixels and those retrieved perviouly outside of this block 
!C next to the edge
!C********************************************************* 

      use mod_par_inv, only : KIX,KIY,KITIME,KIMAGE,   &
                              KIEDGE,KPARS,KMPSM,KDIF
      use mod_index_cloud
      use mod_retr_settings_derived_type
      use mod_time_utils
      use mod_edges
      use mod_stop_report
      
      implicit none
! ------------------------------------------------------------
! IN :
      integer,                             intent(in)  ::  iu_main_output
      type(ind_clouds),                    intent(in)  ::  index_clouds
      type(retr_input_settings),           intent(in)  ::  RIN
      type(segment_edges),                 intent(in)  ::  edges
! ------------------------------------------------------------
! IN / OUT:
      integer,                             intent(inout) :: NISM,IKSIM
      real,dimension(KIMAGE,KPARS,KIMAGE), intent(inout) :: SMMULTI
      integer,dimension(KMPSM),            intent(inout) :: KSM
      integer,dimension(KMPSM,KIMAGE),     intent(inout) :: IMSM  
      real,dimension(KPARS,KIMAGE),        intent(out)   :: FF_edge
      real,                                intent(out)   :: AB_edge
! ------------------------------------------------------------
! LOCAL :
!      INTEGER,DIMENSION(KIX_E)                :: IXE
!	     INTEGER,DIMENSION(KIY_E)                :: IYE
!      INTEGER,PARAMETER :: KMPSM_E = KIX_E*KIMAGE_E+KIY_E*KIMAGE_E+KIX_E*KIY_E
!      INTEGER,DIMENSION(KMPSM_E,KIMAGE) :: IMSM_edge
!      INTEGER,DIMENSION(KMPSM_E) :: NIO,KSM_AP,KSM_A
!      INTEGER,PARAMETER :: KMPSM_E = KIX_E*KIMAGE_E+KIY_E*KIMAGE_E+KIX_E*KIY_E
      integer,parameter :: KIX_E    = max(KIEDGE,KIX)
      integer,parameter :: KIY_E    = max(KIEDGE,KIY)
      integer,parameter :: KITIME_E = max(KIEDGE,KITIME)
      integer,parameter :: KIMAGE_E = 2*(3*KIX*KITIME+3*KIY*KITIME+3*KIX*KIY)

      integer :: NISM_edge
      integer,dimension(KMPSM,KIMAGE) :: IMSM_edge
      integer,dimension(KMPSM)        :: NIO,KSM_AP,KSM_A
!      integer,dimension(:,:), allocatable :: IMSM_edge
!      integer,dimension(:),   allocatable :: NIO,KSM_AP,KSM_A

      integer,dimension(max(KIMAGE,KIMAGE_E))  :: IX_A,IX_AP,IY_A,IY_AP,IT_A,IT_AP
!      integer,dimension(max(KIX_E,KIY_E,KIMAGE_E),max(KIX_max,KIY_max,KIMAGE_E))  ::    &
      integer,dimension(max(KIX_E,KIY_E,KITIME_E),max(KIX_E,KIY_E,KITIME_E))  ::    &  ! ???
                                                          NX_EDGE_A,NX_EDGE_AP,     &
                                                          NY_EDGE_A,NY_EDGE_AP,     & 
                                                          NT_EDGE_A,NT_EDGE_AP
      real,dimension(KDIF,KDIF)               :: DIF,DIF_A,DIF_AP,SM_A,SM_AP
      real,dimension(KDIF,KDIF)               :: DIFWGT
      real,dimension(KIMAGE,KPARS,KIMAGE)     :: SMIM_edge !???

      real,dimension(KDIF)                    :: XX_edge 
      real,dimension(KIMAGE_E)                :: XX_edge_A !???
      real,dimension(KIMAGE)                  :: XX_edge_AP !???       
      real,dimension(KPARS,KIMAGE_E)          :: APX !???

      real,dimension(:,:,:),  allocatable  :: X_edge,Y_edge
      real,dimension(:),      allocatable  :: T_edge
      integer,dimension(:),   allocatable  :: IT
      real,dimension(:,:,:,:),allocatable  :: AP_edge
      integer                              :: alloc_stat

      integer :: IX,IY,IDIM1,IDIM2,IDIM3,NDIM3,I1,I2,J1,I,J,ii1,KNF
      real    :: G, FF, AB, xnorm
      integer :: NSUM 
      real    :: GSM
      integer :: ISTARSING,IOX,IOY,IOT,NX_edge,NY_edge,    &
                 NT_edge,NIM,NNN_E,IOI,N_I_EDGE,I_EDGE,II_E,IT_edge,IT_main, &
                 IS1, IS2, IKSIM_E,IX_edge,IY_edge,IS_edge,NPIXT,ipix   
      real    :: AB_edge_XYT 
      logical :: IPRINT2, lnism, IPRI_verbose
! ------------------------------------------------------------

      IPRI_verbose = RIN%IPRI_verbose

! ------------------------------------------------------------
!  The below text needs to be updated for "edges"
! ------------------------------------------------------------
! KMPSM - parameter, max number of groups for multi-pixel smoothness
! NISM  - number of groups in which smoothness can be implemented
! IKSIM - number of equivalent measurements defining smoothness
!         (used for residual)
! KSM (1:KMPSM)           - number of elements in each of NISM groups
! IMSM(1:KMPSM,1:KIMAGE)  - pixel numbers which belong to the group
! SMMULTI(1:KIMAGE,1:KPARS,1:KIMAGE)  - smoothness matrix
! ------------------------------------------------------------
!moved into read inversion setting input
!if(KITIME .gt. KDIF) then
!write(*,*) 'KITIME=',KITIME,' .gt. KDIF=',KDIF
!write(*,*) 'Check inversion parameters: KDIF has to be max(KPARS,KITIME,KIX,KIY).'
!stop 'stop in smoothterm_multi_pixel'
!endif
!if(KIX .gt. KDIF) then
!write(*,*) 'KIX=',KIX,' .gt. KDIF=',KDIF
!write(*,*) 'Check inversion parameters: KDIF has to be max(KPARS,KITIME,KIX,KIY).'
!stop 'stop in smoothterm_multi_pixel'
!endif
!if(KIY .gt. KDIF) then
!write(*,*) 'KIY=',KIY,' .gt. KDIF=',KDIF
!write(*,*) 'Check inversion parameters: KDIF has to be max(KPARS,KITIME,KIX,KIY).'
!stop 'stop in smoothterm_multi_pixel'
!endif
!      XNEW(:)   = 0.0

!Allocate X_edge array
      if(.not. allocated(X_edge)) then
        allocate(X_edge(KIX_E,KIY_E,KITIME_E),stat=alloc_stat)       
        if (alloc_stat /= 0) then
        stop 'error while trying to allocate X_edge(:,:,:) in smoothterm_mutli_pixel_edges'
        endif              
      endif
! Allocate Y_edge array
      if(.not. allocated(Y_edge)) then
        allocate(Y_edge(KIX_E,KIY_E,KITIME_E),stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to allocate Y_edge(:,:,:) in smoothterm_mutli_pixel_edges'
        endif              
      endif
! Allocate T_edge array
      if(.not. allocated(T_edge)) then
        allocate(T_edge(KITIME_E),stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to allocate T_edge(:) in smoothterm_mutli_pixel_edges'
        endif              
      endif
! Allocate IT array
      if(.not. allocated(IT)) then
        allocate(IT(KITIME_E),stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to allocate IT(:) in smoothterm_mutli_pixel_edges'
        endif              
      endif
! Allocate AP_edge array
      if(.not. allocated(AP_edge)) then
        allocate(AP_edge(KPARS,KIX_E,KIY_E,KITIME_E),stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to allocate AP_edge(:,:,:,:) in smoothterm_mutli_pixel_edges'
        endif              
      endif

      IPRINT2 = .false.
      IKSIM_E = 0
      NISM_edge = 0
      IMSM_edge(:,:) = 0
      FF_edge  (:,:) = 0.0
      AB_edge = 0.0
      SMIM_edge(:,:,:) = 0.0
      xnorm = -999.0 ! smoothness matrix is normalized

      KNF = RIN%KNSINGF
      N_I_EDGE = edges%N_I_EDGE
!write(*,*)  'in smoothterm_mutli_pixel_edges: ',KNF,N_I_EDGE,'  - KNF,N_I_EDGE'
!write(*,*)  'in smoothterm_mutli_pixel_edges: I_EDGE(1:N_I_EDGE):',edges%I_EDGE(1:N_I_EDGE)
!*      NX_before NX_after NY_before NY_after NT_before NT_after

LOOP_I_EDGE: DO ii1=1,N_I_EDGE ! MAX of N_I_EDGE = 6
!*  I_EDGE=1,6 - corresponds to X_before X_after Y_before Y_after T_before T_after
! X - longitude, Y - latitude
          X_edge(:,:,:)    = 0.0
          Y_edge(:,:,:)    = 0.0
          T_edge(:)        = 0.0
          AP_edge(:,:,:,:) = 0.0
          I_EDGE  = edges%I_EDGE(ii1)
!write(*,*) 'in smoothterm_mutli_pixel_edges: ',I_EDGE          
          select case(I_EDGE)
          case(1,2)
! X edges
            II_E = I_EDGE
            NX_edge = edges%group_X(II_E)%nx
            NY_edge = edges%group_X(II_E)%ny
            NT_edge = edges%group_X(II_E)%nt
!write(*,*) 'in smoothterm_mutli_pixel_edges: ',II_E, NX_edge, NY_edge, NT_edge, &
!                                         '   - II_E, NX_edge, NY_edge, NT_edge'
            DO IT_edge=1,NT_edge
              IT(IT_edge) = edges%group_X(II_E)%it(IT_edge)
!write(*,*) 'IT_edge=',IT_edge,'  IT(IT_edge)=',IT(IT_edge)
              X_edge(1:NX_edge,1:NY_edge,IT(IT_edge)) = edges%group_X(II_E)%x(1:NX_edge,1:NY_edge,IT(IT_edge))
              Y_edge(1:NX_edge,1:NY_edge,IT(IT_edge)) = edges%group_X(II_E)%y(1:NX_edge,1:NY_edge,IT(IT_edge)) 
              do IY=1,NY_edge
              do IX=1,NX_edge
                if(edges%group_X(II_E)%icloud(IX,IY,IT(IT_edge)) .eq. 1)  &
                AP_edge(1:KNF,IX,IY,IT(IT_edge)) = LOG(edges%group_X(II_E)%AP(1:KNF,IX,IY,IT(IT_edge)))
!write(*,*) IX,IY,edges%group_X(II_E)%icloud(IX,IY,IT(IT_edge)),X_edge(IX,IY,IT(IT_edge)),Y_edge(IX,IY,IT(IT_edge)),  & 
!      '  - IX,IY,ICLOUD_edge(IX,IY,IT(IT_edge)),X_edge(IX,IY,IT(IT_edge)),Y_edge(IX,IY,IT(IT_edge))'
              enddo ! IX
              enddo ! IY
            enddo ! IT_edge  
          case(3,4)
! Y_edge
            II_E = I_EDGE-2
            NX_edge = edges%group_Y(II_E)%nx
            NY_edge = edges%group_Y(II_E)%ny
            NT_edge = edges%group_Y(II_E)%nt
!write(*,*) 'in smoothterm_mutli_pixel_edges: ',II_E, NX_edge, NY_edge, NT_edge, &
!                                         '   - II_E, NX_edge, NY_edge, NT_edge'
            DO IT_edge=1,NT_edge
              IT(IT_edge) = edges%group_Y(II_E)%it(IT_edge)
!write(*,*) 'IT_edge=',IT_edge,'  IT(IT_edge)=',IT(IT_edge)
              X_edge(1:NX_edge,1:NY_edge,IT(IT_edge)) = edges%group_Y(II_E)%x(1:NX_edge,1:NY_edge,IT(IT_edge))
              Y_edge(1:NX_edge,1:NY_edge,IT(IT_edge)) = edges%group_Y(II_E)%y(1:NX_edge,1:NY_edge,IT(IT_edge)) 
              do IY=1,NY_edge
              do IX=1,NX_edge
                if(edges%group_Y(II_E)%icloud(IX,IY,IT(IT_edge)) .eq. 1)  &
                AP_edge(1:KNF,IX,IY,IT(IT_edge)) = LOG(edges%group_Y(II_E)%AP(1:KNF,IX,IY,IT(IT_edge)))
!write(*,*) IX,IY,edges%group_Y(II_E)%icloud(IX,IY,IT(IT_edge)),X_edge(IX,IY,IT(IT_edge)),Y_edge(IX,IY,IT(IT_edge)),  & 
!      '  - IX,IY,ICLOUD_edge(IX,IY,IT(IT_edge)),X_edge(IX,IY,IT(IT_edge)),Y_edge(IX,IY,IT(IT_edge))'
              enddo ! IX
              enddo ! IY
            enddo ! IT_edge            
          case(5,6)
! T_edge          
            II_E = I_EDGE-4
            NX_edge = edges%group_T(II_E)%nx
            NY_edge = edges%group_T(II_E)%ny
            NT_edge = edges%group_T(II_E)%nt
!write(*,*) 'in smoothterm_mutli_pixel_edges: ',II_E, NX_edge, NY_edge, NT_edge, &
!                                         '   - II_E, NX_edge, NY_edge, NT_edge'
            DO IT_edge=1,NT_edge
              IT(IT_edge) = edges%group_T(II_E)%it(IT_edge)
!write(*,*) 'IT_edge=',IT_edge,'  IT(IT_edge)=',IT(IT_edge)
              X_edge(1:NX_edge,1:NY_edge,IT(IT_edge)) = edges%group_T(II_E)%x(1:NX_edge,1:NY_edge,IT(IT_edge))
              Y_edge(1:NX_edge,1:NY_edge,IT(IT_edge)) = edges%group_T(II_E)%y(1:NX_edge,1:NY_edge,IT(IT_edge)) 
              do IY=1,NY_edge
              do IX=1,NX_edge
                if(edges%group_T(II_E)%icloud(IX,IY,IT(IT_edge)) .eq. 1)  &
                AP_edge(1:KNF,IX,IY,IT(IT_edge)) = LOG(edges%group_T(II_E)%AP(1:KNF,IX,IY,IT(IT_edge)))
!write(*,*) IX,IY,edges%group_T(II_E)%icloud(IX,IY,IT(IT_edge)),X_edge(IX,IY,IT(IT_edge)),Y_edge(IX,IY,IT(IT_edge)),  & 
!      '  - IX,IY,ICLOUD_edge(IX,IY,IT(IT_edge)),X_edge(IX,IY,IT(IT_edge)),Y_edge(IX,IY,IT(IT_edge))'
              enddo ! IX
              enddo ! IY
            enddo ! IT_edge            
          end select
          
          select case(I_EDGE)
          case(1) ! I_EDGE=1, X_before **********************************************
!            II_E = ii1
            II_E = I_EDGE
            NX_EDGE_A(:,:)  = 0
            NX_EDGE_AP(:,:) = 0 
            DO IT_edge=1,NT_edge
            IT(IT_edge) = edges%group_X(II_E)%it(IT_edge)
            DO IY=1,NY_edge
              DO IX=NX_edge,1,-1
!              IF(NX_EDGE_A(IY,IT(IT_edge)) .LT. 3) THEN
              IF(NX_EDGE_A(IY,IT(IT_edge)) .LE. KIEDGE) THEN
                IF(edges%group_X(II_E)%icloud(IX,IY,IT(IT_edge)) .GT. 0) THEN
                  NX_EDGE_A(IY,IT(IT_edge)) = NX_EDGE_A(IY,IT(IT_edge))+edges%group_X(II_E)%icloud(IX,IY,IT(IT_edge))
                  IX_A(NX_EDGE_A(IY,IT(IT_edge))) = IX
                  XX_edge_A(NX_EDGE_A(IY,IT(IT_edge))) = X_edge(IX,IY,IT(IT_edge))
!write(*,*) IT_edge,IY,IX,IT(IT_edge),XX_edge_A(NX_EDGE_A(IY,IT(IT_edge))),  & 
!        '  IT_edge,IY,IX,IT(IT_edge),XX_edge_A(NX_EDGE_A(IY,IT(IT_edge)))'
                ENDIF
              ENDIF
              ENDDO ! IX
              DO IX=1,index_clouds%NX
!              IF(NX_EDGE_AP(IY,IT(IT_edge)) .LT. 3) THEN
              IF(NX_EDGE_AP(IY,IT(IT_edge)) .LE. KIEDGE) THEN
!write(*,*) IX,IY,IT(IT_edge),index_clouds%ICLOUD(IX,IY,IT(IT_edge)),  & 
!        '  IX,IY,IT(IT_edge),index_clouds%ICLOUD(IX,IY,IT(IT_edge))'
		            IF(index_clouds%ICLOUD(IX,IY,IT(IT_edge)) .GT. 0) THEN
                  NX_EDGE_AP(IY,IT(IT_edge)) = NX_EDGE_AP(IY,IT(IT_edge))+index_clouds%ICLOUD(IX,IY,IT(IT_edge))
                  IX_AP(NX_EDGE_AP(IY,IT(IT_edge))) = IX
                  XX_edge_AP(NX_EDGE_AP(IY,IT(IT_edge))) = index_clouds%X(IX,IY,IT(IT_edge))
!write(*,*) IT_edge,IY,IX,IT(IT_edge),XX_edge_AP(NX_EDGE_AP(IY,IT(IT_edge))),  & 
!        '  IT_edge,IY,IX,IT(IT_edge),XX_edge_AP(NX_EDGE_AP(IY,IT(IT_edge)))'
                ENDIF
              ENDIF
              ENDDO ! IX
!write(*,*) 'X_before   **************** IY,IT(IT_edge): ',IY,IT(IT_edge)
!write(*,*) index_clouds%NX,NX_EDGE_A(IY,IT(IT_edge)),'  index_clouds%NX,NX_EDGE_A'
!write(*,*) NX_EDGE_AP(IY,IT(IT_edge)),NX_EDGE_A(IY,IT(IT_edge)),  &
!        '  NX_EDGE_AP(IY,IT(IT_edge)),NX_EDGE_A(IY,IT(IT_edge))'
!write(*,*) 'X_before   ****************'
!stop 'edges test stop'
              IF(NX_EDGE_AP(IY,IT(IT_edge)) .GE. 1 .AND. NX_EDGE_A(IY,IT(IT_edge)) .GE. 1) THEN
                lnism = .false.
                NISM_edge = NISM_edge+1
                DO IDIM1=1,RIN%NDIM%n1
                DO IDIM2=1,RIN%NDIM%n2(IDIM1)
                  NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
                  ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
                  IOI       = RIN%MPCS%IOX(IDIM2,IDIM1)
                  GSM       = RIN%MPCS%GSMX(IDIM2,IDIM1)
                  IF(GSM .GT. 0. .AND. IOI .GT. 0) THEN
                    lnism = .true.
                    NSUM = NX_EDGE_AP(IY,IT(IT_edge))+NX_EDGE_A(IY,IT(IT_edge))
                    call KSM_calcul(NSUM,NX_EDGE_A(IY,IT(IT_edge)),NX_EDGE_AP(IY,IT(IT_edge)),  &
                                    IOI,KSM_A(NISM_edge),KSM_AP(NISM_edge))
                    XX_edge(:) = 0.0
                    APX(:,:) = 0.0
                    DO IX=KSM_A(NISM_edge),1,-1
                      XX_edge(KSM_A(NISM_edge)-IX+1) = XX_edge_A(IX)
                      APX(1:KNF,KSM_A(NISM_edge)-IX+1) = AP_edge(1:KNF,IX_A(IX),IY,IT(IT_edge))
                    ENDDO ! IX
                    DO IX=1,KSM_AP(NISM_edge)
                      XX_edge(KSM_A(NISM_edge)+IX) = XX_edge_AP(IX)
                      IMSM_edge(NISM_edge,IX) = index_clouds%INIMAGE(IX_AP(IX),IY,IT(IT_edge))
!write(*,*) 'NISM_edge,IX,IMSM_edge(NISM_edge,IX),IT(IT_edge):  ',NISM_edge,IX,IMSM_edge(NISM_edge,IX),IT(IT_edge)
                    ENDDO ! IX
                    DIF(:,:) = 0.0
                    NNN_E = KSM_AP(NISM_edge)+KSM_A(NISM_edge)
                    NIO(NISM_edge) = NNN_E-IOI
!write(*,*) 'X_before, before DIFERW XX_edge: '
!write(*,*) 'NNN_E,IOI:  ',NNN_E,IOI
!write(*,'(10e14.4)') XX_edge(1:2)
!stop
                    CALL DIFERW ( IPRI_verbose, &
                                  NNN_E,IOI,    &
                                  XX_edge(:),G, &
                                  DIF(:,:) )
                    IF ( error_present() ) RETURN
!write(*,*) 'G=',G
!do i=1,IOI
!write(*,'(a,i4,10e14.4)') 'i,DIF - ',i,DIF(i,1:NNN_E)
!enddo
                    CALL DIF_before_calcul(NIO(NISM_edge),KSM_A(NISM_edge),NNN_E,DIF,DIF_A,DIF_AP)
!***********************************************************************************************
!**** The following quadratic form is calculated for the residal :
!**** ((D1)* ap - (D2) * ab)T ((D1)* ap - (D2) * ab)
!**** The equivalent is:  (ap)T (D1)T (D1) ap - 2 (ap)T (D1)T (D2) ab + (ab)T (D2)T (D2)ab
!****
!**** BELOW we alculate the followibg vlaues:
!**** - SM_AP = (D1)T (D1), SMIM_edge - represent the total term from all I_EDGE;
!**** - FF_B = (ab)T (D2)T (D2)ab = ((D2)ab)**2- the total term from all I_EDGE;
!**** - FF   - (D1)T (D2) ab      - the total term from all I_EDGE;
!****   (ap)t FF - needs to be calcated for contribtuion of edge constraints into total residual;
!**** - Aslo: (-1) (D1)T (D2) ab  is the  a priori edge  term contribtuion to the right side  ****
!****                                                                     of Normal System:
!*************************************************************************************************
!**** Here we define SM_AP= (DIF_AP)**T (DIF_AP)   *********
                    SM_AP(:,:) = 0.0
                    DIFWGT(:,:) = DIF_AP(:,:)
                    CALL SMOOM( NIO(NISM_edge),KSM_AP(NISM_edge), &
                                DIF_AP(:,:), DIFWGT, &
                                SM_AP(:,:), xnorm )
!write(*,*) 'NIO(NISM_edge),KSM_AP(NISM_edge): ',NIO(NISM_edge),KSM_AP(NISM_edge)
!write(*,*) 'SM_AP:'
!do i=1,KSM_AP(NISM_edge)
!write(*,*) 'i=',i,SM_AP(i,1:KSM_AP(NISM_edge))
!enddo
!**** Here we define SM_A= (DIF_AP)**T DIF_A        *********
                    SM_A(:,:) = 0.0
                    CALL MAT_T_MAT ( KSM_AP(NISM_edge),KSM_A(NISM_edge),   &
                                     NIO(NISM_edge),                       &
                                     DIF_AP(:,:),DIF_A(:,:),SM_A(:,:) )
!write(*,*) 'KSM_AP(NISM_edge),KSM_A(NISM_edge): ',KSM_AP(NISM_edge),KSM_A(NISM_edge)
!write(*,*) 'SM_A:'
!do i=1,KSM_AP(NISM_edge)
!write(*,*) 'i=',i,SM_A(i,1:KSM_A(NISM_edge))
!enddo

                    IKSIM_E = IKSIM_E+NIO(NISM_edge)
                    DO I2=1,NDIM3
                      AB_edge = AB_edge +  &
                      AB_edge_XYT(KIMAGE_E,GSM,I2,ISTARSING,NIO(NISM_edge),KSM_A(NISM_edge),DIF_A,APX)
                      CALL FF_edge_calcul(KIMAGE_E,GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                          KSM_A(NISM_edge),KSM_AP(NISM_edge),SM_A,APX,FF_edge)
!**** Here we define the endge a priori term contribtuion for the right side of Noramal System: ****
!**** SMIM_edge - smoothness  edge term that should be added to general a priori term         ****
                      CALL smoothness_edge_term(GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                                KSM_AP(NISM_edge),SM_AP,SMIM_edge)
!write(*,*) 'case(1) I2=',I2,'  after CALL smoothness_edge_term'
                    ENDDO ! I2=1,NDIM3
                  ENDIF ! IF(GSM .GT. 0. .AND. IOI .GT. 0) THEN
                ENDDO ! IDIM2
                ENDDO ! IDIM1
                if(.not. lnism) NISM_edge = NISM_edge-1
              ENDIF ! NX_EDGE_AP(IY,IT(IT_edge)) .GE. 1 .AND. 
            ENDDO ! DO IY=1,NY_edge
            ENDDO ! DO IT_edge=1,NT_edge
!write(*,*) 'IS1=',IS1,'  FF_edge(:,1): '
!write(*,'(10e14.4)')  FF_edge(:,1)
!stop 'case(1): test stop in smoothterm_mutli_pixel_edges'
          case(2) ! I_EDGE=2, X_after **********************************************
!            II_E = ii1
            II_E = I_EDGE
            NX_EDGE_A(:,:)  = 0
            NX_EDGE_AP(:,:) = 0
            DO IT_edge=1,NT_edge
            IT(IT_edge) = edges%group_X(II_E)%it(IT_edge)
            DO IY=1,NY_edge
              DO IX=1,NX_edge
!              IF(NX_EDGE_A(IY,IT(IT_edge)) .LT. 3) THEN
              IF(NX_EDGE_A(IY,IT(IT_edge)) .LE. KIEDGE) THEN
                IF(edges%group_X(II_E)%icloud(IX,IY,IT(IT_edge)) .GT. 0) THEN
                  NX_EDGE_A(IY,IT(IT_edge)) = NX_EDGE_A(IY,IT(IT_edge))+edges%group_X(II_E)%icloud(IX,IY,IT(IT_edge))
                  IX_A(NX_EDGE_A(IY,IT(IT_edge))) = IX
                  XX_edge_A(NX_EDGE_A(IY,IT(IT_edge))) = X_edge(IX,IY,IT(IT_edge))
!write(*,*) IT_edge,IY,IX,IT(IT_edge),XX_edge_A(NX_EDGE_A(IY,IT(IT_edge))),  & 
!        '  IT_edge,IY,IX,IT(IT_edge),XX_edge_A(NX_EDGE_A(IY,IT(IT_edge)))'
                ENDIF
              ENDIF
              ENDDO ! IX
              DO IX=index_clouds%NX,1,-1
!              IF(NX_EDGE_AP(IY,IT(IT_edge)) .LT. 3) THEN
              IF(NX_EDGE_AP(IY,IT(IT_edge)) .LE. KIEDGE) THEN
                IF(index_clouds%ICLOUD(IX,IY,IT(IT_edge)) .GT. 0) THEN
                  NX_EDGE_AP(IY,IT(IT_edge)) = NX_EDGE_AP(IY,IT(IT_edge))+index_clouds%ICLOUD(IX,IY,IT(IT_edge))
                  IX_AP(NX_EDGE_AP(IY,IT(IT_edge))) = IX
                  XX_edge_AP(NX_EDGE_AP(IY,IT(IT_edge))) = index_clouds%X(IX,IY,IT(IT_edge))
!write(*,*) IT_edge,IY,IX,IT(IT_edge),XX_edge_AP(NX_EDGE_AP(IY,IT(IT_edge))),  & 
!        '  IT_edge,IY,IX,IT(IT_edge),XX_edge_AP(NX_EDGE_AP(IY,IT(IT_edge)))'
                ENDIF
              ENDIF
              ENDDO ! IX
!write(*,*) 'X_after    **************** IY,IT(IT_edge): ',IY,IT(IT_edge)
!write(*,*) index_clouds%NX,NX_EDGE_A(IY,IT(IT_edge)),'  NX_EDGE_AP,NX_EDGE_A'
!write(*,*) NX_EDGE_AP(IY,IT(IT_edge)),NX_EDGE_A(IY,IT(IT_edge)),  & 
!        '  NX_EDGE_AP(IY,IT(IT_edge)),NX_EDGE_A(IY,IT(IT_edge))'
!write(*,*) 'X_after    ****************'
!stop 'X_after edges test stop'
              IF(NX_EDGE_AP(IY,IT(IT_edge)) .GE. 1 .AND. NX_EDGE_A(IY,IT(IT_edge)) .GE. 1) THEN
                lnism = .false.
                NISM_edge = NISM_edge+1
                DO IDIM1=1,RIN%NDIM%n1
                DO IDIM2=1,RIN%NDIM%n2(IDIM1)
                  NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
                  ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
                  IOI       = RIN%MPCS%IOX(IDIM2,IDIM1)
                  GSM       = RIN%MPCS%GSMX(IDIM2,IDIM1)
                  IF(GSM .GT. 0. .AND. IOI .GT. 0) THEN
                    lnism = .true.
                    NSUM = NX_EDGE_AP(IY,IT(IT_edge))+NX_EDGE_A(IY,IT(IT_edge))
                    call KSM_calcul(NSUM,NX_EDGE_A(IY,IT(IT_edge)),NX_EDGE_AP(IY,IT(IT_edge)),  &
                                    IOI,KSM_A(NISM_edge),KSM_AP(NISM_edge))
                    XX_edge(:) = 0.0
                    APX(:,:) = 0.0
                    DO IX=1,KSM_A(NISM_edge)
                      XX_edge(KSM_AP(NISM_edge)+IX) = XX_edge_A(IX)
                      APX(1:KNF,IX) = AP_edge(1:KNF,IX_A(IX),IY,IT(IT_edge))
                    ENDDO ! IX

                    DO IX=KSM_AP(NISM_edge),1,-1
!tlod                    DO IX=1,KSM_AP(NISM_edge)
!tlod                      XX_edge(KSM_AP(NISM_edge)-IX+1) = XX_edge_AP(index_clouds%NX-IX+1)
                      XX_edge(IX) = XX_edge_AP(KSM_AP(NISM_edge)-IX+1)
                      IMSM_edge(NISM_edge,IX) = index_clouds%INIMAGE(IX_AP(IX),IY,IT(IT_edge))
!write(*,*) 'NISM_edge,KSM_AP(NISM_edge)-IX+1,IMSM_edge(NISM_edge,KSM_AP(NISM_edge)-IX+1),IT(IT_edge):  ',  & 
!            NISM_edge,KSM_AP(NISM_edge)-IX+1,IMSM_edge(NISM_edge,KSM_AP(NISM_edge)-IX+1),IT(IT_edge)
!write(*,*)  IX,NISM_edge,IMSM_edge(NISM_edge,KSM_AP(NISM_edge)-IX+1),index_clouds%INIMAGE(IX_AP(IX),IY,IT(IT_edge)), &
!         '  IX,NISM_edge,IMSM_edge(NISM_edge,KSM_AP(NISM_edge)-IX+1),index_clouds%INIMAGE(IX_AP(IX),IY,IT(IT_edge))'
                    ENDDO

                    DIF(:,:) = 0.0
                    !NNN_E = NISM_edge+KSM_A(NISM_edge)
                    NNN_E = KSM_AP(NISM_edge)+KSM_A(NISM_edge)
                    NIO(NISM_edge) = NNN_E-IOI
!write(*,*) 'X_after : ',NISM_edge,KSM_AP(NISM_edge),KSM_A(NISM_edge),'  NISM_edge,KSM_AP(NISM_edge),KSM_A(NISM_edge)'
!write(*,*) 'NNN_E,IOI  ',NNN_E,IOI
!write(*,*) 'XX_edge:'
!write(*,'(10e14.4)') XX_edge(1:NNN_E) 
                    CALL DIFERW ( IPRI_verbose, &
                                  NNN_E,IOI,    &
                                  XX_edge(:),G, &
                                  DIF(:,:) )
                    IF ( error_present() ) RETURN
!write(*,*) 'G=',G,'  DIF: '
!do i=1,IOI
!write(*,'(10e14.4)') DIF(i,1:NNN_E)
!enddo
                    CALL DIF_after_calcul(NIO(NISM_edge),KSM_AP(NISM_edge),NNN_E,DIF,DIF_A,DIF_AP)
!***********************************************************************************************
!**** For the residal we need calculate the following quadratic form:
!**** ((D1)* ap - (D2) * ab)T ((D1)* ap - (D2) * ab)
!**** The equivalent is:  (ap)T (S1)T (S1) ap - 2 (ap)T (S1)T (S2) ab + (ab)T (S2)T (S2)ab
!****
!**** BELOW we calculate the following values:
!**** - SM_AP = (D1)T (D1), SMIM_edge - represent the total term from all I_EDGE;
!**** - FF_B = (ab)T (D2)T (D2)ab = ((D2)ab)**2- the total term from all I_EDGE;
!**** - FF   - (D1)T (D2) ab      - the total term from all I_EDGE;
!****   (ap)t FF - needs to be calcated for contribtuion of edge constraints into total residual;
!**** - Aslo: (-1) (D1)T (D2) ab  is the  a priori edge  term contribtuion to the right side  ****
!****                                                                     of Normal System:
!*************************************************************************************************
!**** Here we define SM_AP= (DIF_AP)**T (DIF_AP)   *********
                    SM_AP(:,:) = 0.0
                    DIFWGT(:,:) = DIF_AP(:,:)
                    CALL SMOOM( NIO(NISM_edge),NNN_E,  &
                                DIF_AP(:,:), DIFWGT(:,:), &
                                SM_AP(:,:), xnorm )
!**** Here we define SM_A= (DIF_AP)**T DIF_A        *********
                    SM_A(:,:) = 0.0
                    CALL MAT_T_MAT( KSM_AP(NISM_edge),KSM_A(NISM_edge),   &
                                    NIO(NISM_edge),                       &
                                    DIF_AP(:,:),DIF_A(:,:),SM_A(:,:) )
                    IKSIM_E = IKSIM_E+NIO(NISM_edge)
                    DO I2=1,NDIM3
                      AB_edge = AB_edge +  &
                      AB_edge_XYT(KIMAGE_E,GSM,I2,ISTARSING,NIO(NISM_edge),KSM_A(NISM_edge),DIF_A,APX)
                      CALL FF_edge_calcul(KIMAGE_E,GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                          KSM_A(NISM_edge),KSM_AP(NISM_edge),SM_A,APX,FF_edge)
!**** Here we define the endge a priori term contribtuion for the right side of Noramal System: ****
!**** SMIM_edge - smoothness  edge term that should be addred to general a priori term         ****
                      CALL smoothness_edge_term(GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                                KSM_AP(NISM_edge),SM_AP,SMIM_edge)
                    ENDDO ! I2=1,NDIM3
!                   ENDDO
                  ENDIF ! IF(GSM .GT. 0. .AND. IOI .GT. 0) THEN
                ENDDO ! IDIM2
                ENDDO ! IDIM1
                if (.not. lnism) NISM_edge =NISM_edge-1
              ENDIF ! NX_EDGE_AP(IT,IY) .GE. 1 .AND. 
            ENDDO ! DO IY=1,NY_edge
            ENDDO ! DO IT_edge=1,NT_edge
!stop 'case(2): test stop in smoothterm_mutli_pixel_edges'
          case(3) ! I_EDGE=3, Y_before *********************************************
!            II_E = ii1-2
            II_E = I_EDGE-2
            NY_EDGE_A(:,:)  = 0
            NY_EDGE_AP(:,:) = 0
            DO IT_edge=1,NT_edge
!            write(*,*) 'II_E,IT_edge,NT_edge: ',II_E,IT_edge,NT_edge
            IT(IT_edge) = edges%group_Y(II_E)%it(IT_edge)
            DO IX=1,NX_edge
              DO IY=NY_edge,1,-1
!              IF(NY_EDGE_A(IX,IT(IT_edge)) .LT. 3) THEN
              IF(NY_EDGE_A(IX,IT(IT_edge)) .LE. KIEDGE) THEN
                IF(edges%group_Y(II_E)%icloud(IX,IY,IT(IT_edge)) .GT. 0) THEN
                  NY_EDGE_A(IX,IT(IT_edge)) = NY_EDGE_A(IX,IT(IT_edge))+edges%group_Y(II_E)%icloud(IX,IY,IT(IT_edge))
                  IY_A(NY_EDGE_A(IX,IT(IT_edge))) = IY
                  XX_edge_A(NY_EDGE_A(IX,IT(IT_edge))) = Y_edge(IX,IY,IT(IT_edge))
!write(*,*) NX_EDGE_A(IX,IT(IT_edge)),IY_A(NX_EDGE_A(IX,IT(IT_edge))),       &
!        '  NX_EDGE_A(IX,IT(IT_edge)),IY_A(NX_EDGE_A(IX,IT(IT_edge)))'
!write(*,*) IT_edge,IX,IY,IT(IT_edge),XX_edge_A(NY_EDGE_A(IX,IT(IT_edge))),  & 
!        '  IT_edge,IX,IY,IT(IT_edge),,XX_edge_A(NY_EDGE_A(IX,IT(IT_edge)))'

                ENDIF
              ENDIF
              ENDDO ! IY
              DO IY=1,index_clouds%NY
!              IF(NY_EDGE_AP(IX,IT(IT_edge)) .LT. 3) THEN
              IF(NY_EDGE_AP(IX,IT(IT_edge)) .LE. KIEDGE) THEN
                IF(index_clouds%ICLOUD(IX,IY,IT(IT_edge)) .GT. 0) THEN
                  NY_EDGE_AP(IX,IT(IT_edge)) = NY_EDGE_AP(IX,IT(IT_edge))+index_clouds%ICLOUD(IX,IY,IT(IT_edge))
                  IY_AP(NY_EDGE_AP(IX,IT(IT_edge))) = IY
                  XX_edge_AP(NY_EDGE_AP(IX,IT(IT_edge))) = index_clouds%Y(IX,IY,IT(IT_edge))
!write(*,*) NY_EDGE_AP(IX,IT(IT_edge)),IY_AP(NY_EDGE_AP(IX,IT(IT_edge))),      & 
!        '  NY_EDGE_AP(IX,IT(IT_edge)),IY_AP(NY_EDGE_AP(IX,IT(IT_edge)))'
!write(*,*) IT_edge,IX,IY,IT(IT_edge),XX_edge_AP(NY_EDGE_AP(IX,IT(IT_edge))),  & 
!        '  IT_edge,IX,IY,IT(IT_edge),XX_edge_AP(NY_EDGE_AP(IX,IT(IT_edge)))'
                ENDIF
              ENDIF
              ENDDO ! IY
!write(*,*) 'Y_before   **************** IX,IT(IT_edge): ',IX,IT(IT_edge)
!write(*,*) index_clouds%NY,NY_EDGE_A(IX,IT(IT_edge)),'  NY_EDGE_AP,NY_EDGE_A'
!write(*,*) NY_EDGE_AP(IX,IT(IT_edge)),NY_EDGE_A(IX,IT(IT_edge)),  & 
!        '  NY_EDGE_AP(IX,IT(IT_edge)),NY_EDGE_A(IX,IT(IT_edge))'
!write(*,*) 'Y_before   ****************'
!stop 'Y_before edges test stop'
              IF(NY_EDGE_AP(IX,IT(IT_edge)) .GE. 1 .AND. NY_EDGE_A(IX,IT(IT_edge)) .GE. 1) THEN
                lnism = .false.
                NISM_edge = NISM_edge+1
                DO IDIM1=1,RIN%NDIM%n1
                DO IDIM2=1,RIN%NDIM%n2(IDIM1)
                  NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
                  ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
                  IOI       = RIN%MPCS%IOY(IDIM2,IDIM1)
                  GSM       = RIN%MPCS%GSMY(IDIM2,IDIM1)
                  IF(GSM .GT. 0. .AND. IOI .GT. 0) THEN
                  lnism = .true.
                  NSUM = NY_EDGE_AP(IX,IT(IT_edge))+NY_EDGE_A(IX,IT(IT_edge))
                  call KSM_calcul(NSUM,NY_EDGE_A(IX,IT(IT_edge)),NY_EDGE_AP(IX,IT(IT_edge)),  &
                                  IOI,KSM_A(NISM_edge),KSM_AP(NISM_edge))
                  XX_edge(:) = 0.0
                  APX(:,:) = 0.0
                  DO IY=KSM_A(NISM_edge),1,-1
                    XX_edge(KSM_A(NISM_edge)-IY+1) = XX_edge_A(IY)
                    APX(1:KNF,KSM_A(NISM_edge)-IY+1) = AP_edge(1:KNF,IX,IY_A(IY),IT(IT_edge))
                  ENDDO
                  DO IY=1,KSM_AP(NISM_edge)
                    XX_edge(KSM_A(NISM_edge)+IY)=XX_edge_AP(IY)
                    IMSM_edge(NISM_edge,IY) = index_clouds%INIMAGE(IX,IY_AP(IY),IT(IT_edge))
                  ENDDO

                  DIF(:,:) = 0.0
                  NNN_E = KSM_AP(NISM_edge)+KSM_A(NISM_edge)
                  NIO(NISM_edge) = NNN_E-IOI
                  CALL DIFERW ( IPRI_verbose, &
                                NNN_E,IOI,    &
                                XX_edge(:),G, &
                                DIF(:,:) )
                  IF ( error_present() ) RETURN
                  CALL DIF_before_calcul(NIO(NISM_edge),KSM_A(NISM_edge),NNN_E,DIF,DIF_A,DIF_AP)
!write(*,*) 'IKSIM_E, NIO(NISM_edge), NNN_E, IOI - ',IKSIM_E,NIO(NISM_edge),NNN_E,IOI
!do i=1,IOI
!write(*,'(a,i4,10e14.4)') 'i,DIF_A - ',i,DIF_A(i,1:NNN_E)
!enddo
!do i=1,IOI
!write(*,'(a,i4,10e14.4)') 'i,DIF_AP - ',i,DIF_AP(i,1:NNN_E)
!enddo
!***********************************************************************************************
!**** For the residal we need calculate the following quadratic form:
!**** ((D1)* ap - (D2) * ab)T ((D1)* ap - (D2) * ab)
!**** The equivalent is:  (ap)T (S1)T (S1) ap - 2 (ap)T (S1)T (S2) ab + (ab)T (S2)T (S2)ab
!****
!**** BELOW we alculate the followibg vlaues:
!**** - SM_AP = (D1)T (D1), SMIM_edge - represent the total term from all I_EDGE;
!**** - FF_B = (ab)T (D2)T (D2)ab = ((D2)ab)**2- the total term from all I_EDGE;
!**** - FF   - (D1)T (D2) ab      - the total term from all I_EDGE;
!****   (ap)t FF - needs to be calcated for contribtuion of edge constraints into total residual;
!**** - Aslo: (-1) (D1)T (D2) ab  is the  a priori edge  term contribtuion to the right side  ****
!****                                                                     of Normal System:
!*************************************************************************************************
!**** Here we define SM_AP= (DIF_AP)**T (DIF_AP)   *********
                  SM_AP(:,:) = 0.0
!od&tl                  CALL SMOOM (  NIO(NISM_edge),NNN_E,  &
                  DIFWGT(:,:) = DIF_AP(:,:)
                  CALL SMOOM( NIO(NISM_edge),KSM_AP(NISM_edge), &
                              DIF_AP(:,:), DIFWGT(:,:), &
                              SM_AP(:,:), xnorm )
!write(*,*) 'NIO(NISM_edge),KSM_AP(NISM_edge): ',NIO(NISM_edge),KSM_AP(NISM_edge)
!write(*,*) 'SM_AP:'
!do i=1,KSM_AP(NISM_edge)
!write(*,*) 'i=',i,SM_AP(i,1:KSM_AP(NISM_edge))
!enddo
!**** Here we define SM_A= (DIF_AP)**T DIF_A        *********
                  SM_A(:,:) = 0.0
                  CALL MAT_T_MAT( KSM_AP(NISM_edge),KSM_A(NISM_edge),  &
                                  NIO(NISM_edge),                      &
                                  DIF_AP(:,:),DIF_A(:,:),SM_A(:,:) )
!write(*,*) 'KSM_AP(NISM_edge),KSM_A(NISM_edge): ',KSM_AP(NISM_edge),KSM_A(NISM_edge)
!write(*,*) 'SM_A:'
!do i=1,KSM_AP(NISM_edge)
!write(*,*) 'i=',i,SM_A(i,1:KSM_A(NISM_edge))
!enddo ! i
                  IKSIM_E = IKSIM_E+NIO(NISM_edge)
                  DO I2=1,NDIM3
                    AB_edge = AB_edge +  &
                    AB_edge_XYT(KIMAGE_E,GSM,I2,ISTARSING,NIO(NISM_edge),KSM_A(NISM_edge),DIF_A,APX)
                    CALL FF_edge_calcul(KIMAGE_E,GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                        KSM_A(NISM_edge),KSM_AP(NISM_edge),SM_A,APX,FF_edge)
!**** Herewe define the endge a priori term contribtuion for the right side of Noramal System: ****
!**** SMIM_edge - smoothness  edge term that should be addred to general a priori term         ****
                    CALL smoothness_edge_term(GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                              KSM_AP(NISM_edge),SM_AP,SMIM_edge)
!write(*,*) 'case(3) I2=',I2,'  after CALL smoothness_edge_term'
                  ENDDO ! I2=1,NDIM3!                   ENDDO
                ENDIF ! NY_EDGE_AP(IT(IT_edge),IX) .GE. 1 .AND.     ?????
                ENDDO ! IDIM2
                ENDDO ! IDIM1
                if(.not. lnism) NISM_edge = NISM_edge-1
              ENDIF ! NY_EDGE_AP(IT(IT_edge),IX) .GE. 1 .AND.
            ENDDO ! DO IX=1,NX_edge
            ENDDO ! DO IT_edge=1,NT_edge
!stop 'Y_before edges test stop'
          case(4) ! I_EDGE=4, YB after main retrieved block ************************
!            II_E = ii1-2
            II_E = I_EDGE-2
            NY_EDGE_A(:,:)  = 0.0
            NY_EDGE_AP(:,:) = 0.0
            DO IT_edge=1,NT_edge
            IT(IT_edge) = edges%group_Y(II_E)%it(IT_edge)
            IT(IT_edge) = IT(IT_edge)
            DO IX=1,NX_edge
              DO IY=1,NY_edge
!                IF(NY_EDGE_A(IX,IT(IT_edge)) .LT. 3) THEN
                IF(NY_EDGE_A(IX,IT(IT_edge)) .LE. KIEDGE) THEN
                  IF(edges%group_Y(II_E)%icloud(IX,IY,IT(IT_edge)) .GT. 0) THEN
                    NY_EDGE_A(IX,IT(IT_edge)) = NY_EDGE_A(IX,IT(IT_edge))+edges%group_Y(II_E)%icloud(IX,IY,IT(IT_edge))
                    IY_A(NY_EDGE_A(IX,IT(IT_edge))) = IY
                    XX_edge_A(NY_EDGE_A(IX,IT(IT_edge))) = X_edge(IX,IY,IT(IT_edge))
                  ENDIF
                ENDIF
              ENDDO
              DO IY=index_clouds%NY,1,-1
!                IF(NY_EDGE_AP(IX,IT(IT_edge)) .LT. 3) THEN
                IF(NY_EDGE_AP(IX,IT(IT_edge)) .LE. KIEDGE) THEN
                  IF(index_clouds%ICLOUD(IX,IY,IT(IT_edge)) .GT. 0) THEN
                    NY_EDGE_AP(IX,IT(IT_edge)) = NY_EDGE_AP(IX,IT(IT_edge))+index_clouds%ICLOUD(IX,IY,IT(IT_edge))
                    IY_AP(NY_EDGE_AP(IX,IT(IT_edge))) = IY
                    XX_edge_AP(NY_EDGE_AP(IX,IT(IT_edge))) = index_clouds%Y(IX,IY,IT(IT_edge))
                  ENDIF
                ENDIF
              ENDDO
!write(*,*) 'Y_after   **************** IX,IT(IT_edge): ',IX,IT(IT_edge)
!write(*,*) index_clouds%NY,NY_EDGE_A(IX,IT(IT_edge)),'  NY_EDGE_AP,NY_EDGE_A'
!write(*,*) NY_EDGE_AP(IX,IT(IT_edge)),NY_EDGE_A(IX,IT(IT_edge)),  & 
!        '  NY_EDGE_AP(IX,IT(IT_edge)),NY_EDGE_A(IX,IT(IT_edge))'
!write(*,*) 'Y_after   ****************'
!stop 'Y_before edges test stop'
              IF(NY_EDGE_AP(IX,IT(IT_edge)) .GE. 1 .AND. NY_EDGE_A(IX,IT(IT_edge)) .GE. 1) THEN
                lnism = .false.
                NISM_edge = NISM_edge+1
                DO IDIM1=1,RIN%NDIM%n1
                DO IDIM2=1,RIN%NDIM%n2(IDIM1)
                  NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
                  ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
                  IOI       = RIN%MPCS%IOY(IDIM2,IDIM1)
                  GSM       = RIN%MPCS%GSMY(IDIM2,IDIM1)
                  IF(GSM .GT. 0. .AND. IOI .GT. 0) THEN
                    lnism = .true.
                    NSUM = NY_EDGE_AP(IX,IT(IT_edge))+NY_EDGE_A(IX,IT(IT_edge))
                    call KSM_calcul(NSUM,NY_EDGE_A(IX,IT(IT_edge)),NY_EDGE_AP(IX,IT(IT_edge)),  &
                                    IOI,KSM_A(NISM_edge),KSM_AP(NISM_edge))
                    XX_edge(:) = 0.0
                    APX(:,:) = 0.0
                    DO IY=1,KSM_A(NISM_edge)
                      XX_edge(KSM_AP(NISM_edge)+IY) = XX_edge_A(IY)
                      APX(1:KNF,IX) = AP_edge(1:KNF,IX,IY_A(IY),IT(IT_edge))
                    ENDDO
!                    DO IY=KSM_AP(NISM_edge),1,-1
                    DO IY=1,KSM_AP(NISM_edge)
                      XX_edge(IY) = XX_edge_AP(KSM_AP(NISM_edge)-IY+1)
                      IMSM_edge(NISM_edge,IY) = index_clouds%INIMAGE(IY,IY_AP(IY),IT(IT_edge))
                    ENDDO
                    DIF(:,:) = 0.0
                    NNN_E = KSM_AP(NISM_edge)+KSM_A(NISM_edge)
                    NIO(NISM_edge) = NNN_E-IOI
                    CALL DIFERW ( IPRI_verbose, &
                                  NNN_E,IOI,    &
                                  XX_edge(:),G, &
                                  DIF(:,:) )
                    IF ( error_present() ) RETURN
                    CALL DIF_after_calcul(NIO(NISM_edge),KSM_AP(NISM_edge),NNN_E,DIF,DIF_A,DIF_AP)
!***********************************************************************************************
!**** For the residal we need calculate the following quadratic form:
!**** ((D1)* ap - (D2) * ab)T ((D1)* ap - (D2) * ab)
!**** The equivalent is:  (ap)T (S1)T (S1) ap - 2 (ap)T (S1)T (S2) ab + (ab)T (S2)T (S2)ab
!****
!**** BELOW we alculate the followibg vlaues:
!**** - SM_AP = (D1)T (D1), SMIM_edge - represent the total term from all I_EDGE;
!**** - FF_B = (ab)T (D2)T (D2)ab = ((D2)ab)**2- the total term from all I_EDGE;
!**** - FF   - (D1)T (D2) ab      - the total term from all I_EDGE;
!****   (ap)t FF - needs to be calcated for contribtuion of edge constraints into total residual;
!**** - Aslo: (-1) (D1)T (D2) ab  is the  a priori edge  term contribtuion to the right side  ****
!****                                                                     of Normal System:
!*************************************************************************************************
!**** Here we define SM_AP= (DIF_AP)**T (DIF_AP)   *********
                    SM_AP(:,:) = 0.0
                    DIFWGT(:,:) = DIF_AP(:,:)
                    CALL SMOOM( NIO(NISM_edge),NNN_E, &
                                DIF_AP(:,:), DIFWGT(:,:), &
                                SM_AP(:,:), xnorm )
!**** Here we define SM_A= (DIF_AP)**T DIF_A        *********
                    SM_A(:,:) = 0.0
                    CALL MAT_T_MAT( KSM_AP(NISM_edge),KSM_A(NISM_edge),   &
                                    NIO(NISM_edge),                       &
                                    DIF_AP(:,:),DIF_A(:,:),SM_A(:,:) )
                    IKSIM_E = IKSIM_E+NIO(NISM_edge)
                    DO I2=1,NDIM3
                      AB_edge = AB_edge +  &
                      AB_edge_XYT(KIMAGE_E,GSM,I2,ISTARSING,NIO(NISM_edge),KSM_A(NISM_edge),DIF_A,APX)
                      CALL FF_edge_calcul(KIMAGE_E,GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                          KSM_A(NISM_edge),KSM_AP(NISM_edge),SM_A,APX,FF_edge)
!**** Here we define the endge a priori term contribtuion for the right side of Noramal System: ****
!**** SMIM_edge - smoothness  edge term that should be addred to general a priori term         ****
                      CALL smoothness_edge_term(GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                                KSM_AP(NISM_edge),SM_AP,SMIM_edge)
                    ENDDO ! I2=1,NDIM3

                  ENDIF ! IF((GSM.GT.0.).AND.IOY.GT.0) THEN
                ENDDO ! IDIM2
                ENDDO ! IDIM1
                if(.not. lnism) NISM_edge = NISM_edge-1 
              ENDIF ! NX_EDGE_AP(IT,IY) .GE. 1 .AND. 
            ENDDO ! DO IX=1,NX_edge
            ENDDO ! DO IT=1,NT_edge
!stop 'Y_after edges test stop'
          case(5) ! I_EDGE=5, TB before main retrieved block !****************************
!            II_E = ii1-4
            II_E = I_EDGE-4
            NT_EDGE_A(:,:)  = 0
            NT_EDGE_AP(:,:) = 0
!tl            DO IX=1,NX_edge
            DO IY=1,NY_edge
            DO IX=1,NX_edge
              DO IT_edge=NT_edge,1,-1
!                IF(NT_EDGE_A(IX,IY) .LT. 3) THEN
                IF(NT_EDGE_A(IX,IY) .LE. KIEDGE) THEN
                  IF(edges%group_T(II_E)%icloud(IX,IY,IT(IT_edge)) .GT. 0) THEN
!write(*,*) 'IX=',IX,'  IY=',IY,'  NX_EDGE_A(IX,IY)=',NX_EDGE_A(IX,IY)
!write(*,*) 'IT_edge=',IT_edge,'  IT(IT_edge)=',IT(IT_edge)
                    NT_EDGE_A(IX,IY) = NT_EDGE_A(IX,IY)+edges%group_T(II_E)%icloud(IX,IY,IT(IT_edge))
                    IT_A(NT_EDGE_A(IX,IY)) = IT(IT_edge)
                    XX_edge_A(NT_EDGE_A(IX,IY)) = T_edge(IT_edge)
                  ENDIF
                ENDIF
              ENDDO
              DO IT_main=1,index_clouds%NT
!                IF(NT_EDGE_AP(IX,IY) .LT. 3) THEN
                IF(NT_EDGE_AP(IX,IY) .LE. KIEDGE) THEN
                  IF(index_clouds%ICLOUD(IX,IY,IT_main) .GT. 0) THEN
                    NT_EDGE_AP(IX,IY) = NT_EDGE_AP(IX,IY)+index_clouds%ICLOUD(IX,IY,IT_main)
                    IT_AP(NT_EDGE_AP(IX,IY)) = IT_main
                    XX_edge_AP(NT_EDGE_AP(IX,IY)) = index_clouds%T(IX,IY,IT_main)
                  ENDIF
                ENDIF
              ENDDO
!write(*,*) 'T_before   **************** IX,IY: ',IX,IY
!write(*,*) index_clouds%NT,NT_EDGE_A(IX,IY),'  NT_EDGE_AP,NT_EDGE_A'
!write(*,*) NT_EDGE_AP(IX,IY),NT_EDGE_A(IX,IY),  & 
!        '  NT_EDGE_AP(IX,IY),NT_EDGE_A(IX,IY)'
!write(*,*) 'T_before   ****************'
!stop 'T_before edges test stop'
              IF(NT_EDGE_AP(IX,IY) .GE. 1 .AND. NT_EDGE_A(IX,IY) .GE. 1) THEN
                lnism = .false.
                NISM_edge = NISM_edge+1
                DO IDIM1=1,RIN%NDIM%n1
                DO IDIM2=1,RIN%NDIM%n2(IDIM1)
                  NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
                  ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
                  IOI       = RIN%MPCS%IOT(IDIM2,IDIM1)
                  GSM       = RIN%MPCS%GSMT(IDIM2,IDIM1)
                  IF(GSM .GT. 0. .AND. IOI .GT. 0) THEN
                    lnism = .true.
                    NSUM = NT_EDGE_AP(IX,IY)+NT_EDGE_A(IX,IY)
                    call KSM_calcul(NSUM,NT_EDGE_A(IX,IY),NT_EDGE_AP(IX,IY),  &
                                    IOI,KSM_A(NISM_edge),KSM_AP(NISM_edge))
                    XX_edge(:) = 0.0
                    APX(:,:) = 0.0
                    DO IT_edge=KSM_A(NISM_edge),1,-1
                      XX_edge(KSM_A(NISM_edge)-IT_edge+1) = XX_edge_A(IT_edge)
                      APX(1:KNF,KSM_A(NISM_edge)-IT_edge+1) = AP_edge(1:KNF,IX,IY,IT_edge)
                    ENDDO
!write(*,*) 'NISM_edge,KSM_AP(NISM_edge): ',NISM_edge,KSM_AP(NISM_edge)
                    DO IT_main=1,KSM_AP(NISM_edge)
                      XX_edge(KSM_A(NISM_edge)+(RIN%edges%nt+IT_main)) = XX_edge_AP(IT_main)
                      IMSM_edge(NISM_edge,IT_main) = index_clouds%INIMAGE(IX,IY,IT_AP(IT_main))
!write(*,*) 'NISM_edge,IMSM_edge(NISM_edge,IT_main),IT_main:  ',  &
!            NISM_edge,IMSM_edge(NISM_edge,IT_main),IT_main
                    ENDDO
                    DIF(:,:) = 0.0
                    NNN_E = KSM_AP(NISM_edge)+KSM_A(NISM_edge)
                    NIO(NISM_edge) = NNN_E-IOI
! 2019-06-27
! NOTE! Time (index_clouds%T()) must be converted into double precision in order
! to be passed into DIFERW_dp insted of DIFERW as it was implemented in 
! subroutine smoothterm_mutli_pixel
                    CALL DIFERW ( IPRI_verbose, &
                                  NNN_E,IOI,    &
                                  XX_edge(:),G, &
                                  DIF(:,:) )
                    IF ( error_present() ) RETURN
                    CALL DIF_before_calcul(NIO(NISM_edge),KSM_A(NISM_edge),NNN_E,DIF,DIF_A,DIF_AP)
!***********************************************************************************************
!**** For the residal we need calculate the following quadratic form:
!**** ((D1)* ap - (D2) * ab)T ((D1)* ap - (D2) * ab)
!**** The equivalent is:  (ap)T (S1)T (S1) ap - 2 (ap)T (S1)T (S2) ab + (ab)T (S2)T (S2)ab
!****
!**** BELOW we alculate the followibg vlaues:
!**** - SM_AP = (D1)T (D1), SMIM_edge - represent the total term from all I_EDGE;
!**** - FF_B = (ab)T (D2)T (D2)ab = ((D2)ab)**2- the total term from all I_EDGE;
!**** - FF   - (D1)T (D2) ab      - the total term from all I_EDGE;
!****   (ap)t FF - needs to be calcated for contribtuion of edge constraints into total residual;
!**** - Aslo: (-1) (D1)T (D2) ab  is the  a priori edge  term contribtuion to the right side  ****
!****                                                                     of Normal System:
!*************************************************************************************************
!**** Here we define SM_AP= (DIF_AP)**T (DIF_AP)   *********
                    SM_AP(:,:) = 0.0
!                    CALL SMOOM (  NIO(NISM_edge),NNN_E,  &
                    DIFWGT(:,:) = DIF_AP(:,:)
                    CALL SMOOM( NIO(NISM_edge),KSM_AP(NISM_edge), &
                                DIF_AP(:,:), DIFWGT(:,:), &
                                SM_AP(:,:), xnorm )
!**** Here we define SM_A= (DIF_AP)**T DIF_A        *********
                    SM_A(:,:) = 0.0
                    CALL MAT_T_MAT (  KSM_AP(NISM_edge),KSM_A(NISM_edge),  &
                                      NIO(NISM_edge),                      &
                                      DIF_AP(:,:),DIF_A(:,:),SM_A(:,:) )
                    IKSIM_E = IKSIM_E+NIO(NISM_edge)
                    DO I2=1,NDIM3
                      AB_edge = AB_edge +  &
                      AB_edge_XYT(KIMAGE_E,GSM,I2,ISTARSING,NIO(NISM_edge),KSM_A(NISM_edge),DIF_A,APX)
                      CALL FF_edge_calcul(KIMAGE_E,GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                          KSM_A(NISM_edge),KSM_AP(NISM_edge),SM_A,APX,FF_edge)
!**** Here we define the endge a priori term contribtuion for the right side of Noramal System: ****
!**** SMIM_edge - smoothness  edge term that should be addred to general a priori term         ****
                      CALL smoothness_edge_term(GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                                KSM_AP(NISM_edge),SM_AP,SMIM_edge)
                    ENDDO ! I2=1,NDIM3
                  ENDIF ! GSM .GT. 0. .AND.
                ENDDO ! IDIM2
                ENDDO ! IDIM1
                if(.not. lnism) NISM_edge = NISM_edge
              ENDIF ! NX_EDGE_AP(IT,IY) .GE. 1 .AND. 
            ENDDO ! IY=1,NY_edge
            ENDDO ! DO IX=1,NX_edge
!stop 'T_before edges test stop'
          case(6) ! I_EDGE=6, T_after ******************************************************
!            II_E = ii1-4
            II_E = I_EDGE-4
            NT_EDGE_A(:,:)  = 0
            NT_EDGE_AP(:,:) = 0
            DO IY=1,NY_edge
            DO IX=1,NX_edge
              DO IT_edge=1,NT_edge
!                IF(NT_EDGE_A(IX,IY) .LT. 3) THEN
                IF(NT_EDGE_A(IX,IY) .LE. KIEDGE) THEN
                  IF(edges%group_T(II_E)%icloud(IX,IY,IT(IT_edge)) .GT. 0) THEN
                    NT_EDGE_A(IX,IY) = NT_EDGE_A(IX,IY)+edges%group_T(II_E)%icloud(IX,IY,IT(IT_edge))
                    IT_A(NT_EDGE_A(IX,IY)) = IT(IT_edge)
                    XX_edge_A(NT_EDGE_A(IX,IY)) = T_edge(IT_edge)
                  ENDIF
                ENDIF
              ENDDO
              DO IT_main=index_clouds%NT,1,-1
!                IF(NT_EDGE_AP(IX,IY) .LT. 3) THEN
                IF(NT_EDGE_AP(IX,IY) .LE. KIEDGE) THEN
                  IF(index_clouds%ICLOUD(IX,IY,IT_main) .GT. 0) THEN
                    NT_EDGE_AP(IX,IY) = NT_EDGE_AP(IX,IY)+index_clouds%ICLOUD(IX,IY,IT_main)
                    IT_AP(NT_EDGE_AP(IX,IY)) = IT_main
                    XX_edge_AP(NT_EDGE_AP(IX,IY)) = index_clouds%T(IX,IY,IT_main)
                  ENDIF
                ENDIF
              ENDDO
!write(*,*) 'T_after   **************** IX,IY: ',IX,IY
!write(*,*) index_clouds%NT,NT_EDGE_A(IX,IY),'  NT_EDGE_AP,NT_EDGE_A'
!write(*,*) NT_EDGE_AP(IX,IY),NT_EDGE_A(IX,IY),  & 
!        '  NT_EDGE_AP(IX,IY),NT_EDGE_A(IX,IY)'
!write(*,*) 'T_after   ****************'
!stop 'T_after edges test stop'
              IF(NT_EDGE_AP(IX,IY) .GE. 1 .AND. NT_EDGE_A(IX,IY) .GE. 1) THEN
                lnism = .false.
                NISM_edge = NISM_edge+1
                DO IDIM1=1,RIN%NDIM%n1
                DO IDIM2=1,RIN%NDIM%n2(IDIM1)
                  NDIM3     = RIN%NDIM%n3(IDIM2,IDIM1)
                  ISTARSING = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
                  IOI       = RIN%MPCS%IOT(IDIM2,IDIM1)
                  GSM       = RIN%MPCS%GSMT(IDIM2,IDIM1)
                  IF(GSM .GT. 0. .AND. IOI .GT. 0) THEN
                    lnism = .true.
                    NSUM = NT_EDGE_AP(IX,IY)+NT_EDGE_A(IX,IY)
                    call KSM_calcul(NSUM,NT_EDGE_A(IX,IY),NT_EDGE_AP(IX,IY),  &
                                    IOI,KSM_A(NISM_edge),KSM_AP(NISM_edge))
                    XX_edge(:) = 0.0
                    APX(:,:) = 0.0
                    DO IT_edge=1,KSM_A(NISM_edge)
                      XX_edge(KSM_AP(NISM_edge)+IT_edge) = XX_edge_A(IT_edge)
                      APX(1:KNF,IT_edge) = AP_edge(1:KNF,IX,IY,IT_edge)
                    ENDDO
                    DO IT_main=1,KSM_AP(NISM_edge)
!tl                      XX_edge(KSM_AP(NISM_edge)-IT_m(IT_main)+1) = XX_edge_AP(index_clouds%NT-IT_main+1)
                      XX_edge(IT_main) = XX_edge_AP(KSM_AP(NISM_edge)-IT_main+1)
!write(*,*) 'IT_main=',IT_main,'  IT_AP(IT_main)=',IT_AP(IT_main),  & 
!   '  index_clouds%INIMAGE(IX,IY,IT_AP(IT_main))=',index_clouds%INIMAGE(IX,IY,IT_AP(IT_main))
!write(*,*) 'NISM_edge=',NISM_edge,'  index_clouds%NT-IT_main+1=',index_clouds%NT-IT_main+1
                      !IMSM_edge(NISM_edge,index_clouds%NT-IT_main+1) = index_clouds%INIMAGE(IX,IY,IT_AP(IT_main))
                      IMSM_edge(NISM_edge,IT_main) = index_clouds%INIMAGE(IX,IY,IT_AP(IT_main))
                    ENDDO

                    DIF(:,:) = 0.0
                    NNN_E = KSM_AP(NISM_edge)+KSM_A(NISM_edge)
                    NIO(NISM_edge) = NNN_E-IOI
                    CALL DIFERW ( IPRI_verbose, &
                                  NNN_E,IOI,    &
                                  XX_edge(:),G, &
                                  DIF(:,:) )
                    IF ( error_present() ) RETURN
                    CALL DIF_after_calcul(NIO(NISM_edge),KSM_AP(NISM_edge),NNN_E,DIF,DIF_A,DIF_AP)
!***********************************************************************************************
!**** For the residal we need calculate the following quadratic form:
!**** ((D1)* ap - (D2) * ab)T ((D1)* ap - (D2) * ab)
!**** The equivalent is:  (ap)T (S1)T (S1) ap - 2 (ap)T (S1)T (S2) ab + (ab)T (S2)T (S2)ab
!****
!**** BELOW we alculate the followibg vlaues:
!**** - SM_AP = (D1)T (D1), SMIM_edge - represent the total term from all I_EDGE;
!**** - FF_B = (ab)T (D2)T (D2)ab = ((D2)ab)**2- the total term from all I_EDGE;
!**** - FF   - (D1)T (D2) ab      - the total term from all I_EDGE;
!****   (ap)t FF - needs to be calcated for contribtuion of edge constraints into total residual;
!**** - Aslo: (-1) (D1)T (D2) ab  is the  a priori edge  term contribtuion to the right side  ****
!****                                                                     of Normal System:
!*************************************************************************************************
!**** Here we define SM_AP= (DIF_AP)**T (DIF_AP)   *********
                    SM_AP(:,:) = 0.0
                    DIFWGT(:,:) = DIF_AP(:,:)
                    CALL SMOOM( NIO(NISM_edge),NNN_E, &
                                DIF_AP(:,:), DIFWGT(:,:), &
                                SM_AP(:,:), xnorm )
!**** Here we define SM_A= (DIF_AP)**T DIF_A        *********
                    SM_A(:,:) = 0.0
                    CALL MAT_T_MAT (  KSM_AP(NISM_edge),KSM_A(NISM_edge),  &
                                      NIO(NISM_edge),                      &
                                      DIF_AP(:,:),DIF_A(:,:),SM_A(:,:)  )
                    IKSIM_E = IKSIM_E+NIO(NISM_edge)
                    DO I2=1,NDIM3
                      AB_edge = AB_edge +  &
                      AB_edge_XYT(KIMAGE_E,GSM,I2,ISTARSING,NIO(NISM_edge),KSM_A(NISM_edge),DIF_A,APX)
                      CALL FF_edge_calcul(KIMAGE_E,GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                          KSM_A(NISM_edge),KSM_AP(NISM_edge),SM_A,APX,FF_edge)
!**** Here we define the endge a priori term contribtuion for the right side of Noramal System: ****
!**** SMIM_edge - smoothness  edge term that should be added to general a priori term         ****
                      CALL smoothness_edge_term(GSM,I2,ISTARSING,IMSM_edge(NISM_edge,:),  &
                                                KSM_AP(NISM_edge),SM_AP,SMIM_edge)
                    ENDDO ! I2=1,NDIM3

                  ENDIF ! GSM .GT. 0. .AND.
                ENDDO ! IDIM2
                ENDDO ! IDIM1
                if(.not. lnism) NISM_edge = NISM_edge-1
              ENDIF ! NX_EDGE_AP(IT,IY) .GE. 1 .AND. 
            ENDDO ! DO IX=1,NY_edge
            ENDDO ! DO IX=1,NY_edge
!stop 'T_after edges test stop'
          end select ! I_EDGE  !***********************************************
          if(RIN%IPRI_verbose)  &
          write(iu_main_output,'(a,i2,a,i5,a)') 'I_EDGE=',I_EDGE,'  NISM_edge=',NISM_edge, &
          '     in smoothterm_mutli_pixel_edges '
ENDDO LOOP_I_EDGE    ! MAX of N_I_EDGE = 6

! Deallocate X_edge array
      if(allocated(X_edge)) then
        deallocate(X_edge,stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to deallocate X_edge(:,:,:) in smoothterm_mutli_pixel_edges'
        endif              
      endif
! Deallocate Y_edge array
      if(allocated(Y_edge)) then
        deallocate(Y_edge,stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to deallocate Y_edge(:,:,:) in smoothterm_mutli_pixel_edges'
        endif              
      endif
! Deallocate T_edge array
      if(allocated(T_edge)) then
        deallocate(T_edge,stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to deallocate T_edge(:) in smoothterm_mutli_pixel_edges'
        endif              
      endif
! Deallocate IT array
      if(allocated(IT)) then
        deallocate(IT,stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to deallocate IT(:) in smoothterm_mutli_pixel_edges'
        endif              
      endif
! Deallocate AP_edge array
      if(allocated(AP_edge)) then
        deallocate(AP_edge,stat=alloc_stat)
        if (alloc_stat /= 0) then
        stop 'error while trying to deallocate AP_edge(:,:,:,:) in smoothterm_mutli_pixel_edges'
        endif              
      endif

!write(*,*) 'NISM_edge=',NISM_edge
goto 55
      if(NISM_edge .GT. 0) THEN
        IKSIM = IKSIM+IKSIM_E
        do IS_edge=1,NISM_edge
          NISM = NISM+1
!          IS = NISM
          KSM(NISM) = KSM_AP(NISM_edge)
          IMSM(NISM,1:KSM(NISM)) = IMSM_edge(IS_edge,1:KSM(NISM))
          do I=1,RIN%KNSINGF
            do IS1=1,KSM_AP(IS_edge)
!write(*,*) I,IS_edge,IS1,IMSM(IS_edge,IS1),'  - I,IS_edge,IS1,IMSM(IS_edge,IS1) in smoothterm_mutli_pixel_edges'
            do IS2=1,KSM_AP(IS_edge)
!write(*,*) I,IS_edge,IS2,IMSM(IS_edge,IS2),'  - I,IS_edge,IS2,IMSM(IS_edge,IS2) in smoothterm_mutli_pixel_edges'
              SMMULTI(IMSM(NISM,IS1),I,IMSM(NISM,IS2)) = SMMULTI(IMSM(NISM,IS1),I,IMSM(NISM,IS2))+ &
              SMIM_edge(IMSM_edge(IS_edge,IS1),I,IMSM_edge(IS_edge,IS2))
!write(*,*) 'IS_edge,IS1,IS2,IMSM(NISM,IS1),I,IMSM(NISM,IS2),SMMULTI(IMSM(NISM,IS1),I,IMSM(NISM,IS2)),', &
!'IMSM_edge(IS_edge,IS1),IMSM_edge(IS_edge,IS2) : ', &
!IS_edge,IS1,IS2,IMSM(NISM,IS1),I,IMSM(NISM,IS2),SMMULTI(IMSM(NISM,IS1),I,IMSM(NISM,IS2)), &
!IMSM_edge(IS_edge,IS1),IMSM_edge(IS_edge,IS2)
            enddo ! IS2
            enddo ! IS1      
          enddo ! I
        enddo ! IS_edge
      endif ! NISM_edge .GT. 0
55 continue

! OD 2015-09-16 Testing needed for IO=2 multi pixel constrains

      if(NISM_edge .GT. 0) THEN
        IKSIM = IKSIM+IKSIM_E
        do IS_edge=1,NISM_edge
          NISM = NISM+1
! bug ???          KSM(NISM) = KSM_AP(NISM_edge)
          KSM(NISM) = KSM_AP(IS_edge)
          IMSM(NISM,1:KSM(NISM)) = IMSM_edge(IS_edge,1:KSM(NISM))
        enddo ! IS_edge
        SMMULTI(:,:,:) = SMMULTI(:,:,:) + SMIM_edge(:,:,:)
      endif ! NISM_edge .GT. 0

      if(IPRINT2) then
      endif
!stop 'test stop in smoothterm_mutli_pixel_edges'

      END SUBROUTINE smoothterm_mutli_pixel_edges
      
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !!> @brief Routine calculates Fisher matrix
        !>
        !> @param[in]    KM1 - number of columns of U1
        !> @param[in]    KM2 - number of columns of U2
        !> @param[in]    KN  - number of rows
        !> @param[in]    U1  - matrix
        !> @param[in]    U2  - matrix
        !> @param[out]   UFS - Fisher matrix 
        !>
      SUBROUTINE MAT_T_MAT ( KM1,KM2,KN,U1,U2,UFS )
!C**************************************************
!C  THIS SUBROUTINE CALCULATES "FISHER MATRIX":
!C            (U1)**T ((U2)
!C                and
!C!C**************************************************
!C  INPUT:
!C        KN  I         - number of lines
!C        KM1 I         - number of columns of U1
!C        KM2 I         - number of columns of U2
!C        U1   R(KM1,KN) -  matrix
!C        U2   R(KM2,KN) -  matrix 
!C  OUTPUT:
!C         UF R(KN,KN) - matrix(U1)**T ((U2)
!C***************************************************
      USE mod_par_inv, only : KDIF

      IMPLICIT NONE

! ----------------------------------------------------------
! IN :
      INTEGER,                       INTENT(IN)  :: KM1,KM2,KN
      REAL,DIMENSION(KDIF,KDIF),     INTENT(IN)  :: U1,U2
!      REAL,DIMENSION(KMESS),        INTENT(IN)  :: CS
!      REAL,DIMENSION(KMESS),        INTENT(IN)  :: FS,FPS
! ----------------------------------------------------------
! OUT :
      REAL,DIMENSION(KDIF,KDIF),     INTENT(OUT) :: UFS
! ----------------------------------------------------------
! LOCAL :	  
      INTEGER :: I,I1
      REAL :: AM
! ----------------------------------------------------------

!C*** calculating "Fisher matrix" 
!write(*,*) 'in MAT_T_MAT: KM1=',KM1,'  KM2=',KM2,'  KN=',KN
!write(*,*) 'in MAT_T_MAT: U1:'
!do i=1,KM1
!write(*,'(10e14.4)') U1(1:KN,i)
!enddo
!write(*,*) 'in MAT_T_MAT: U2:'
!do i=1,KM2
!write(*,'(10e14.4)') U2(1:KN,i)
!enddo
      DO I=1,KM1
       DO I1=1,KM2
          UFS(I,I1)=SUM(U1(1:KN,I)*U2(1:KN,I1)) 
       ENDDO ! I1
!write(*,*) 'in MAT_T_MAT 1: I=',I,'  UFS(I,1:KM2):'
!write(*,'(10e14.4)') UFS(I,1:KM2)
      ENDDO ! I

!      AM = 0.
!      DO I=1,KDIF
!        IF(AM .LT. abs(UFS(I,I))) AM = abs(UFS(I,I))
!      ENDDO ! I
!      IF(AM .NE. 0.) THEN
!        DO I =1,KDIF
!          DO I1=1,KDIF
!            UFS(I,I1) = UFS(I,I1)/AM
!          ENDDO ! I1
!        ENDDO ! I
!write(*,*) 'in MAT_T_MAT 12: I=',1,'  UFS=',UFS(1,1:1)
!      ENDIF ! AM.NE.0.

      RETURN
      END SUBROUTINE MAT_T_MAT

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !!> @brief Function returns 
        !>
        !> @param[in]    KIMAGE_E   - 
        !> @param[in]    GSM        - 
        !> @param[in]    I2         - 
        !> @param[in]    ISTARSING  - 
        !> @param[in]    NIO        - 
        !> @param[in]    KSM_A      -  
        !> @param[in]    DIF_A      -
        !> @param[in]    APX        -
        !>

      real function AB_edge_XYT(KIMAGE_E,GSM,I2,ISTARSING,NIO,KSM_A,DIF_A,APX)

      use mod_par_inv, only : KDIF,KPARS

      implicit none
! ----------------------------------------------------------
! IN :
      integer,                       intent(in)  :: KIMAGE_E,I2,ISTARSING
      integer,                       intent(in)  :: NIO,KSM_A
      real,                          intent(in)  :: GSM
      real,dimension(KDIF,KDIF),     intent(in)  :: DIF_A
      real,dimension(KPARS,KIMAGE_E),intent(in)  :: APX
! ----------------------------------------------------------
! LOCAL :
      integer :: I
      real    :: AB
! ----------------------------------------------------------
!OD&TL ???
      AB_edge_XYT = 0.0
      do I=1,NIO
        AB = DOT_PRODUCT(DIF_A(I,1:KSM_A),APX(ISTARSING+I2-1,1:KSM_A))
        AB_edge_XYT = AB_edge_XYT+GSM*AB*AB
      enddo ! I

      end function AB_edge_XYT

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !!> @brief Routine calculates 
        !>
        !> @param[in]    KIMAGE_E   - 
        !> @param[in]    GSM        - 
        !> @param[in]    I2         - 
        !> @param[in]    ISTARSING  - 
        !> @param[in]    IMSM_edge  - 
        !> @param[in]    KSM_A      -  
        !> @param[in]    KSM_AP     -
        !> @param[in]    SM_A       -
        !> @param[in]    APX        -
        !> @param[inout] FF_edge    -
        !>
      subroutine FF_edge_calcul(KIMAGE_E,GSM,I2,ISTARSING,IMSM_edge,KSM_A,KSM_AP,SM_A,APX,FF_edge)

      use mod_par_inv, only : KDIF,KPARS,KMPSM,KIMAGE

      implicit none
! ----------------------------------------------------------
! IN :
      integer,                        intent(in)  :: I2,ISTARSING,KIMAGE_E
      integer,                        intent(in)  :: KSM_A,KSM_AP
      real,                           intent(in)  :: GSM
      real,dimension(KDIF,KDIF),      intent(in)  :: SM_A
      real,dimension(KPARS,KIMAGE_E), intent(in)  :: APX
      integer,dimension(KIMAGE),      intent(in)  :: IMSM_edge
      real,dimension(KPARS,KIMAGE),intent(inout)  :: FF_edge
! ----------------------------------------------------------
! LOCAL :
      integer :: I, ii
      real    :: FF
! ----------------------------------------------------------
      FF = 0.0
      do I=1,KSM_AP
        FF = GSM*DOT_PRODUCT(SM_A(I,1:KSM_A),APX(ISTARSING+I2-1,1:KSM_A))
!write(*,*) 'I=',I,'  IMSM_edge(I)=',IMSM_edge(I)
!do ii=1, KSM_A
!write(*,*) I,I2,ISTARSING+I2-1,GSM,SM_A(I,ii),exp(APX(ISTARSING+I2-1,ii)), &
!'  - I,I2,ISTARSING+I2-1,GSM,SM_A(I,ii),exp(APX(ISTARSING+I2-1,ii))'
!enddo ! ii
        FF_edge(ISTARSING+I2-1,IMSM_edge(I)) =  &
        FF_edge(ISTARSING+I2-1,IMSM_edge(I)) + FF
      enddo

      end subroutine FF_edge_calcul

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine calculates smoothness term for edges of inverted segment.
        !>
        !> @param[in]    GSM        - 
        !> @param[in]    I2         - 
        !> @param[in]    ISTARSING  - 
        !> @param[in]    IMSM_edge  - 
        !> @param[in]    KSM_AP     -
        !> @param[in]    SM_AP       -
        !> @param[inout] SMIM_edge    -
        !>
      subroutine smoothness_edge_term(GSM,I2,ISTARSING,IMSM_edge,KSM_AP,SM_AP,SMIM_edge)

      use mod_par_inv, only : KDIF,KPARS,KIMAGE

      implicit none
! ----------------------------------------------------------
! IN :
      integer,                        intent(in)  :: I2,ISTARSING
      integer,                        intent(in)  :: KSM_AP
      real,                           intent(in)  :: GSM
      real,dimension(KDIF,KDIF),      intent(in)  :: SM_AP
      integer,dimension(KIMAGE),      intent(in)  :: IMSM_edge
      real,dimension(KIMAGE,KPARS,KIMAGE),intent(inout)  :: SMIM_edge
! ----------------------------------------------------------
! LOCAL :
      integer :: I1,J1
! ----------------------------------------------------------
      do I1=1,KSM_AP
      do J1=1,KSM_AP
        SMIM_edge(IMSM_edge(I1),ISTARSING+I2-1,IMSM_edge(J1)) =  &
        SMIM_edge(IMSM_edge(I1),ISTARSING+I2-1,IMSM_edge(J1)) +  & 
        GSM*SM_AP(I1,J1)
!write(*,*) 'I1=',I1,'  J1=',J1,'  GSM=',GSM,'  SM_AP(I1,J1)=',SM_AP(I1,J1),  &
!           '  SMIM_edge(IMSM_edge(I1),ISTARSING+I2-1,IMSM_edge(J1))=',       &
!              SMIM_edge(IMSM_edge(I1),ISTARSING+I2-1,IMSM_edge(J1))

      enddo ! J1
      enddo ! I1

      return
      end subroutine smoothness_edge_term

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !!> @brief Routine calculates 
        !>
        !> @param[in]    NSUM   - 
        !> @param[in]    N_EDGE_A        - 
        !> @param[in]    N_EDGE_AP         - 
        !> @param[inout] IOI  - 
        !> @param[out]   KSM_A      -  
        !> @param[out]   KSM_AP     -
        !>
      subroutine KSM_calcul(NSUM,N_EDGE_A,N_EDGE_AP,IOI,KSM_A,KSM_AP)

      implicit none
! ----------------------------------------------------------
! IN :
      integer,intent(in)     :: NSUM
      integer,intent(in)     :: N_EDGE_A,N_EDGE_AP
      integer,intent(inout)  :: IOI 
      integer,intent(out)    :: KSM_A,KSM_AP
! ----------------------------------------------------------
! ----------------------------------------------------------

      if(NSUM .lt. IOI+1) IOI = NSUM-1

      if(N_EDGE_A .gt. IOI) then
        KSM_A = IOI
      else
        KSM_A = N_EDGE_A
      endif

      if(N_EDGE_AP .gt. IOI) then
        KSM_AP = IOI
      else
        KSM_AP = N_EDGE_AP
      endif

      return
      end subroutine KSM_calcul 
      
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !!> @brief Routine calculates 
        !>
        !> @param[in]    NIO    - 
        !> @param[in]    KSM_A  - 
        !> @param[in]    NNN_E  - 
        !> @param[in]    DIF    - 
        !> @param[in]    DIF_A  - 
        !> @param[in]    DIF_AP -  
        !>
      subroutine DIF_before_calcul( NIO, KSM_A, NNN_E, DIF, DIF_A, DIF_AP )

      use mod_par_inv, only : KDIF
      
      implicit none
! ----------------------------------------------------------
! IN :
      integer,intent(in)                    :: NIO,KSM_A,NNN_E
      real,dimension(KDIF,KDIF),intent(in)  :: DIF
      real,dimension(KDIF,KDIF),intent(out) :: DIF_A,DIF_AP
! ----------------------------------------------------------
! LOCAL:
      real,dimension(KDIF,KDIF) :: SM1
      real,dimension(KDIF,KDIF) :: DIF_temp
      real,dimension(KDIF,KDIF) :: DIFWGT
      integer  :: I,I1,J
      real :: xnorm
! ----------------------------------------------------------
        DIF_temp(:,:) = DIF(:,:)
        xnorm = 0.0 ! smoothnes matrix is normalized
        DIFWGT(:,:) = DIF(:,:)
        CALL SMOOM ( KDIF,KDIF, & ! IN
                     DIF(:,:), DIFWGT(:,:), &
                     SM1(:,:), xnorm  & ! OUT
                  )
        IF(xnorm .NE. 0.0) THEN
          DO I =1,KDIF
          DO I1=1,KDIF
            DIF_temp(I,I1) = DIF_temp(I,I1)/sqrt(xnorm)
          ENDDO ! I1
!write(*,*) 'in DIF_before_calcul: I=',I,'  DIF_temp=',DIF_temp(I,1:1)
          ENDDO ! I
        ENDIF ! xnorm .NE. 0.0

      DIF_A(:,:)  = 0.0
      do I=1,NIO
      do J=1,KSM_A
        DIF_A(I,J) = DIF_temp(I,J)
!write(*,*) 'in DIF_before_calcul: I=',I,'  J=',J,'  DIF_A(I,J)=',DIF_A(I,J)
      enddo
      enddo

      DIF_AP(:,:) = 0.0
      do I=1,NIO
      do J=KSM_A+1,NNN_E
        DIF_AP(I,J-KSM_A) = DIF_temp(I,J)
!write(*,*) 'in DIF_before_calcul: I=',I,'  J=',J,'  J-KSM_A(NISM_edge)=',J-KSM_A,'  KSM_A=',KSM_A,  &
!'  DIF_AP(I,J-KSM_A)=',DIF_AP(I,J-KSM_A)
      enddo
      enddo

      return
      end subroutine DIF_before_calcul 
      
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !!> @brief Routine calculates 
        !>
        !> @param[in]    NIO    - 
        !> @param[in]    KSM_AP  - 
        !> @param[in]    NNN_E  - 
        !> @param[in]    DIF    - 
        !> @param[in]    DIF_A  - 
        !> @param[in]    DIF_AP -  
        !>
      subroutine DIF_after_calcul( NIO, KSM_AP, NNN_E, DIF, DIF_A, DIF_AP )

      use mod_par_inv, only : KDIF
      
      implicit none
! ----------------------------------------------------------
! IN :
      integer,intent(in)                    :: NIO,KSM_AP,NNN_E
      real,dimension(KDIF,KDIF),intent(in)  :: DIF
      real,dimension(KDIF,KDIF),intent(out) :: DIF_A,DIF_AP
! ----------------------------------------------------------
! LOCAL:
      real,dimension(KDIF,KDIF) :: SM1
      real,dimension(KDIF,KDIF) :: DIF_temp
      real,dimension(KDIF,KDIF) :: DIFWGT
      integer  :: I,I1,J
      real :: xnorm
! ----------------------------------------------------------
        DIF_temp(:,:) = DIF(:,:)
        xnorm = 0.0  ! smoothnes matrix is normalized
        DIFWGT(:,:) = DIF(:,:)
        CALL SMOOM ( KDIF, KDIF, & ! IN
                     DIF(:,:), DIFWGT(:,:), &
                     SM1(:,:), xnorm & ! OUT
                    )
        IF(xnorm .NE. 0.0) THEN
          DO I =1,KDIF
          DO I1=1,KDIF
            DIF_temp(I,I1) = DIF_temp(I,I1)/sqrt(xnorm)
          ENDDO ! I1
!write(*,*) 'in DIF_after_calcul: I=',I,'  DIF_temp=',DIF_temp(I,1:KN)
          ENDDO ! I
        ENDIF ! xnorm .NE. 0.0

      DIF_AP(:,:) = 0.0
      do I=1,NIO
      do J=1,KSM_AP
        DIF_AP(I,J) = DIF_temp(I,J)
!write(*,*) 'in DIF_after_calcul: I=',I,'  J=',J,'  DIF_AP(I,J)=',DIF_AP(I,J)
      enddo
      enddo

      DIF_A(:,:)  = 0.0
      do I=1,NIO
      do J=KSM_AP+1,NNN_E
        DIF_A(I,J-KSM_AP) = DIF_temp(I,J)
!write(*,*) 'in DIF_after_calcul: I=',I,'  J=',J,'  J-KSM_AP=',J-KSM_AP,'  KSM_AP=',KSM_AP,  & 
!'  DIF_A(I,J-KSM_AP)=',DIF_A(I,J-KSM_AP)
      enddo
      enddo

      return
      end subroutine DIF_after_calcul 
      
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
