! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

#include "../constants_set/mod_globals.inc"
!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine validator_constants ( RIN )

      use mod_par_inv
      use mod_par_OS
      use mod_par_DLS_bin, only : KWLpar, KCpar, NRR
      use mod_par_DLS, only : KNpar
      use mod_retr_settings_derived_type
      use mod_derivative_type_transport_model, only : tracer_average
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(in) ::  RIN
      integer ::  i, j, IDIM1, IDIM2, par_type
! -----------------------------------------------------------------------------------------
      if(KPARS .gt. KDIF) then
        write(tmp_message,'(2(a,i0),3x,a,a,a)' ) &
        'KPARS = ',KPARS,' .gt. KDIF = ',KDIF,'KDIF is used in smoothness subroutines', &
        NEW_LINE('A'), &
        'Check inversion constants: KDIF = max(KPARS,KITIME,KIX,KIY).'
        G_ERROR(trim(tmp_message))
      endif
      if (KVERTM .gt. KNBVM) then
        write(tmp_message,'(3(a,i0))') &
        'KVERTM = ',KVERTM,' .gt. KNBVM = ',KNBVM, &
        ' not valid combination (KNBVM = max(KVERTM,NBVM)) NBVM = ',NBVM
        G_ERROR(trim(tmp_message))
      endif

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then
          exit
        endif
      enddo

      if(par_type .eq. par_type_SD_MD) then
        if(NRR .gt. 4*KIDIM3) then
          write(tmp_message,'(2(a,i0,3x),2a)') &
          'NRR (see mod_par_DLS_bin) = ',NRR, &
          ' > 4*KIDIM3 (see KIDIM3 in mod_par_inv) = ',4*KIDIM3, &
          NEW_LINE('A'),'NRR can not be larger than 4*KIDIM3 for precomputed Lognormal Bin model.'
          G_ERROR(trim(tmp_message))
        endif
        do IDIM2=1,RIN%NDIM%n2(IDIM1)
          if(RIN%NDIM%n3(IDIM2,IDIM1) .gt. KCpar) then
            write(tmp_message,'(2(a,i0,3x),a,2(a,i0,3x),a)') &
            'IDIM1 = ',IDIM1,'IDIM2 = ',IDIM2,NEW_LINE('A'), &
            'Number of models = ',RIN%NDIM%n3(IDIM2,IDIM1), &
              'can not be larger than constant KCpar = ',KCpar,' in mod_par_DLS_bin.'
              G_ERROR(trim(tmp_message))
            endif
        enddo
        if(RIN%NW .gt. KWLpar) then
          write(tmp_message,'(2(a,i0,2x),a)') &
          'Number of wavelengths = ',RIN%NW, &
          'in settings can not be larger than constant KWLpar = ',KWLpar, &
          'in mod_par_DLS_bin.f90'
          G_ERROR(trim(tmp_message))
        endif
      elseif(par_type .eq. par_type_SD_LN) then
        if(KNpar .gt. 4*KIDIM3) then
          write(tmp_message,'(2(a,i0,3x),2a)') &
          'KNpar (see mod_par_DLS) = ',KNpar, &
          ' > 4*KIDIM3 (see KIDIM3 in mod_par_inv) = ',4*KIDIM3, &
          NEW_LINE('A'),'KNpar can not be larger than 4*KIDIM3 for LogNormal Size Distribution model.'
          G_ERROR(trim(tmp_message))
        endif
      endif

      if ( RIN%use_tmodel ) then
      if ( RIN%TMSET%flag_av_vprof .eq. tracer_average ) then
      if ( KSD .lt. KVERTM ) then
        write(tmp_message,'(2(a,i0,3x),2a)') &
        'KSD (see mod_par_OS) = ',KSD, &
        'KVERTM (see mod_par_inv) = ',KVERTM, &
        NEW_LINE('A'), &
        'KSD must be >= KVERTM for Transport model + tracer average + LogNormal SD model.'
        G_ERROR(trim(tmp_message))
      endif
      endif
      endif

      return
      end subroutine validator_constants

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine validator_SOS_RT_parameters ( RIN )

      use mod_par_OS
      use mod_retr_settings_derived_type
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(in) ::  RIN
      !type(stop_report_type),     intent(inout) ::  stop_report
      integer ::  i,j
! -----------------------------------------------------------------------------------------
      if(RIN%NLYRS(1)+1 .gt. KNT) then
        write(tmp_message,'(2(a,i0),2a)') &
        'RIN%NLYRS(1)+1 = ',RIN%NLYRS(1)+1,' .gt. KNT = ',KNT, &
        NEW_LINE('A'), &
        'Check settings and parameters in both Settings and mod_par_OS'
        G_ERROR(trim(tmp_message))
      endif
      if(RIN%NLYRS(2)+1 .gt. KNT) then
        write(tmp_message,'(2(a,i0),2a)') &
        'RIN%NLYRS(2)+1 = ',RIN%NLYRS(2)+1,' .gt. KNT = ',KNT, &
        NEW_LINE('A'), &
        'Check settings and parameters in both Settings and mod_par_OS'
        G_ERROR(trim(tmp_message))
      endif
      if(RIN%NLVLS_GEOM .gt. KVERT_WD) then
        write(tmp_message,'(2(a,i0),2a)') &
        'RIN%NLVLS_GEOM = ',RIN%NLVLS_GEOM,' .gt. KVERT_WD = ',KVERT_WD, &
        NEW_LINE('A'), &
        'Check settings and parameters in both Settings and mod_par_OS'
        G_ERROR(trim(tmp_message))
      endif

      return
      end subroutine validator_SOS_RT_parameters

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss 

      subroutine validator_inversion_settings ( RIN, SvR_meas_present_status )

      use mod_par_inv
      use mod_retr_settings_derived_type
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(in) ::  RIN
      logical, intent(in) :: SvR_meas_present_status
      integer ::  i, j, IDIM1, IDIM2
      integer ::  nwmax1, nwmax2, par_type, nbins, NSHAPE
      logical ::  lpresent
! -----------------------------------------------------------------------------------------
! Validate KNSING,KNSINGF - number of parameters driving forwar model and 
!                        number of retrieved parameters 
      if(RIN%KNSING .gt. KPARS) then
        write(tmp_message,'(2(a,i4),2a)') 'KNSING = ',RIN%KNSING,' .GT. KPARS = ',KPARS, &
        NEW_LINE('A'), &
        'KPARS in module mod_par_inv'
        G_ERROR(trim(tmp_message))
      endif ! KNSING.GT.KPARS
      if(RIN%KNSINGF .gt. RIN%KNSING) then
        write(tmp_message,'(2(a,i0))') 'KNSINGF = ',RIN%KNSINGF,' .GT. KNSING = ',RIN%KNSING
        G_ERROR(trim(tmp_message))
      endif ! KNSING .GT. KPARS
! Validate retrieved parametres order 
      do IDIM1=2,RIN%NDIM%n1
        if(RIN%NDIM%par_retr(IDIM1)) then
          if(.not. RIN%NDIM%par_retr(IDIM1-1)) then
            write(tmp_message,'(2(a,i0))') 'Retrieved characteristic # ',IDIM1, &
            ' can not be followed by not retrived characteristic # ',IDIM1-1
            G_ERROR(trim(tmp_message))
          endif ! .not. RIN%NDIM%par_retr(IDIM1-1)
        endif ! RIN%NDIM%par_retr(IDIM1)
      enddo
! Validate NW - number of wavelengths for refractive index and surface
goto 111
      nwmax1  = 0
      nwmax2  = 0
      do IDIM1=1,RIN%NDIM%n1 
      if(RIN%NDIM%par_type(IDIM1) .eq. par_type_RERI_spect) then  
        do IDIM2=1,RIN%NDIM%n2(IDIM1)
          nwmax1 = max( nwmax1,RIN%NDIM%n3(IDIM2,IDIM1) )
        enddo
      endif ! RIN%NDIM%par_type(IDIM1) .eq. par_type_RERI_spect
      if(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF1_land_beg .and. & 
      RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF_water_end) then
        do IDIM2=1,RIN%NDIM%n2(IDIM1)
          nwmax2 = max( nwmax2,RIN%NDIM%n3(IDIM2,IDIM1) )
        enddo       
      endif ! RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF1_land_beg .and.
      enddo ! IDIM1
      if(nwmax1 .ne. 0 .and. nwmax2 .ne. 0) then
        if(nwmax1 .ne. nwmax2) then
          write(tmp_message,'(2(a,i0))') 'NW for refr.index = ',nwmax1,' .ne. NW for surface = ',nwmax2
          G_ERROR(trim(tmp_message))
        endif ! nwmax1 .ne. nwmax2
        if(nwmax1 .gt. KW) then
          write(tmp_message,'(2(a,i0))') 'NW = ',nwmax1,'  KW = ',KW,'  KW in module mod_par_inv'
          G_ERROR(trim(tmp_message))
        endif ! nwmax1 .gt. KW
      endif ! nwmax1 .ne. 0 .and.
111 continue

!! Check keyEL and presence of polarized surface
!      do IDIM1=1,RIN%NDIM%n1
!        if(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF2_land_beg .and. RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF2_land_end) then
!          if(RIN%DLSF%keyEL .lt. 4) then
!            write(tmp_message,'(a,i4,2a)') 'keyEL=',RIN%DLSF%keyEL, &
!            NEW_LINE('A'), &
!            'Number of elements_of_phase_matrix has to be set to 4 if polarized surface is present.'
!            G_ERROR(trim(tmp_message))
!          endif
!        endif ! RIN%NDIM%par_type(IDIM1) .gt.
!      enddo ! IDIM1

! Validate IO - difference order for single pixel a priori estimate constraints
      if( any(RIN%SPCA%IO(:,:,:) .ne. 0) ) then
        write(tmp_message,'(a)') 'RIN%SPCA%IO .ne. 0', &
        ' IO is difference order for single pixel a priori estimate constraints'
        G_ERROR(trim(tmp_message))
      endif ! RIN%iPOBS .lt. 1 .or.

! Validate iPOBS - fit polarization observation option
      if(RIN%iPOBS .lt. 1 .or. RIN%iPOBS .gt. 5) then
        write(tmp_message,'(a,i0,a)') 'iPOBS = ',RIN%iPOBS,'  - Unknown value ( 1 <= iPOBS <= 5 )'
        G_ERROR(trim(tmp_message))
      endif ! RIN%iPOBS .lt. 1 .or.

! Validate edges parameters
      if( KIEDGE .lt. RIN%edges%nx .or.  &
          KIEDGE .lt. RIN%edges%ny .or.  &
          KIEDGE .lt. RIN%edges%nt )     &
      then
        write(tmp_message,'(6(a,i0,a),3a)') &
        'KIEDGE = ',KIEDGE,'  RIN%edges_max%nx = ',RIN%edges%nx, &
        NEW_LINE('A'), &
        'KIEDGE = ',KIEDGE,'  RIN%edges_max%ny = ',RIN%edges%ny, &
        NEW_LINE('A'), &
        'KIEDGE = ',KIEDGE,'  RIN%edges_max%nt = ',RIN%edges%nt, &
        NEW_LINE('A'), &
        'Constant KIEDGE can not be smaller than', &
        NEW_LINE('A'), &
        'settings values for RIN%edges%nx,RIN%edges%ny,RIN%edges%nt'
        G_ERROR(trim(tmp_message))
      endif

! Validate number of chemical aerosol component
!      if ( .not. RIN%indep_par ) then
!        do IDIM1=1,RIN%NDIM%n1
!        if(RIN%NDIM%par_type(IDIM1) .eq. par_type_CXRI_nmix) then
!          if(RIN%NSD .eq. 1) then
!            if(RIN%NDIM%n3(1,IDIM1) .ne. 4) then
!              write(tmp_message,'(a,i0,a)') &
!              'Number of chem. components nfract = ',RIN%NDIM%n3(1,IDIM1),' .ne. 4 for one component aerosol.'
!              G_ERROR(trim(tmp_message))
!            endif
!          elseif(RIN%NSD .eq. 2) then
!            if(RIN%NDIM%n3(1,IDIM1) .ne. 4) then
!              write(tmp_message,'(a,i0,a)') &
!              'Number of chem. components nfract = ',RIN%NDIM%n3(1,IDIM1),' .ne. 4 for fine mode.'
!              G_ERROR(trim(tmp_message))
!            endif
!            if(RIN%NDIM%n3(RIN%NSD,IDIM1) .ne. 3) then
!              write(tmp_message,'(a,i0,a)') &
!              'Number of chem. components nfract = ',RIN%NDIM%n3(RIN%NSD,IDIM1),' .ne. 3 for coarse mode.'
!              G_ERROR(trim(tmp_message))
!            endif
!          endif
!        exit
!        endif ! par_type(IDIM1) .eq. par_type_CXRI_nmix
!        enddo ! IDIM1
!      else
!        do IDIM1=1,RIN%NDIM%n1
!        if(RIN%NDIM%par_type(IDIM1) .eq. par_type_CXRI_nmix) then
!          if(RIN%NSD .eq. 1) then
!            if(RIN%NDIM%n3(1,IDIM1) .ne. 3) then
!              write(tmp_message,'(a,i0,a)') &
!              'Number of chem. components nfract = ',RIN%NDIM%n3(1,IDIM1),' .ne. 3 for one component aerosol.'
!              G_ERROR(trim(tmp_message))
!            endif
!          elseif(RIN%NSD .eq. 2) then
!            if(RIN%NDIM%n3(1,IDIM1) .ne. 3) then
!              write(tmp_message,'(a,i0,a)') &
!              'Number of chem. components nfract = ',RIN%NDIM%n3(1,IDIM1),' .ne. 3 for fine mode.'
!              G_ERROR(trim(tmp_message))
!            endif
!            if(RIN%NDIM%n3(2,IDIM1) .ne. 2) then
!              write(tmp_message,'(a,i0,a)') &
!              'Number of chem. components nfract = ',RIN%NDIM%n3(2,IDIM1),' .ne. 2 for coarse mode.'
!              G_ERROR(trim(tmp_message))
!            endif
!          endif
!        exit
!        endif ! par_type(IDIM1) .eq. par_type_CXRI_nmix
!        enddo ! IDIM1
!      endif

! Validate number of precomputed lognormal bins
      do IDIM1=1,RIN%NDIM%n1
        if(RIN%NDIM%par_type(IDIM1) .eq. par_type_SD_LB) then
          if(RIN%NSD .eq. 2) then
            nbins = sum( RIN%NDIM%n3(1:RIN%NSD,IDIM1) )
            if(nbins .gt. KIDIM3) then
              write(tmp_message, '(2(a,i0,2x),a)') &
              'Number of precomputed lognormal bins nbins = ',nbins, &
              'can not be > than constant KIDIM3 = ',KIDIM3,'in module mod_par_inv'
              G_ERROR(trim(tmp_message))
            endif
          endif
        exit
        endif ! par_type(IDIM1) .eq. par_type_CXRI_nmix
      enddo ! IDIM1

! Validate normal system solver
      if(RIN%IMQ .eq. 2) then
        write(tmp_message,'(a)') 'Normal system solver option singular_value_decomposition', &
        ' is currently not supported.'
        G_ERROR(trim(tmp_message))
      endif

! Validate product flags
      if(RIN%products%aerosol%pm) then
        write(tmp_message,'(a)') &
        'Aerosol particulate matter product is not supported, please set retrieval.products.aeerosol.particulate_matter to FALSE'
        G_ERROR(trim(tmp_message))
      endif
!      if(RIN%NSD .eq. 1) then
      if(RIN%products%aerosol%pm .or. RIN%products%aerosol%types) then
      if(.not. RIN%products%aerosol%sd2m_mph .and. &
         .not. RIN%products%aerosol%sd2m_ext) then
        write(tmp_message,'(2a,2(a,l2),3a,2(a,l2))') &
        'To provide these products:', &
        NEW_LINE('A'), &
        'products.aerosol.pm =',RIN%products%aerosol%pm, &
        '  products.aerosol.types =',RIN%products%aerosol%types, &
        NEW_LINE('A'), &
        'one of the following product options must be .true.', &
        NEW_LINE('A'), &
        'products.aerosol.sd2m_mph =',RIN%products%aerosol%sd2m_mph, &
        '  products.aerosol.sd2m_ext =',RIN%products%aerosol%sd2m_ext
        G_ERROR(trim(tmp_message))
      endif
      endif

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .eq. par_type_SD_MD) then
        if(RIN%products%aerosol%sd2m_mph) then
        write(tmp_message,'(3a)') &
        'Can not provide products.aerosol.sd2m_mph with models.', &
        NEW_LINE('A'), &
        'products.aerosol.sd2m_mph has to be .false. in input settings.'
        G_ERROR(trim(tmp_message))
        endif
        endif
      enddo

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .eq. par_type_SD_LN .and. RIN%NSD .eq. 1 ) then
        if(RIN%products%aerosol%sd2m_mph .or. RIN%products%aerosol%sd2m_ext) then
        write(tmp_message,'(3a)') &
        'Can not provide both products.aerosol.sd2m_mph and products.aerosol.sd2m_ext for one mode lognormal SD.', &
        NEW_LINE('A'), &
        'Set both products.aerosol.sd2m_mph and products.aerosol.sd2m_ext to .false. in input settings.'
        G_ERROR(trim(tmp_message))
        endif
        endif
      enddo
!      endif

      if(RIN%products%forcing%forcing .or. RIN%products%forcing%bbflux) then
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if(par_type .eq. par_type_SD_LB) then
            write(tmp_message,'(3a)') &
            'Radiative forcing product (products.forcing) can not be provided', &
            NEW_LINE('A'), &
            'for precomputed lognormal bins. Choose another size distribution model.'
            G_ERROR(trim(tmp_message))
          endif ! par_type .gt. par_type_SD_beg .and.
        enddo ! IDIM1
      endif
      if(RIN%products%aerosol%lidar) then
      if(RIN%DLSF%keyEL .lt. 1) then
        write(tmp_message,'(3a)') &
        'Lidar product (lidar ratio) was requested in settings (products.aerosol.lidar = .true.).', &
        NEW_LINE('A'), &
        'Lidar ratio can be provided if', &
        'kernel.phase_matrix_package.elements_of_phase_matrix set => 1 .'
        G_ERROR(trim(tmp_message))
      endif
      endif

! Validate presence of vertical profile parameter (standard deviation)
! if vertical profile is gaussian
      if( any(RIN%NDIM%par_type(1:RIN%NDIM%n1) .eq. par_type_AVP_par_height) ) then
      if(RIN%aer_prof_type .eq. 1) then
        lpresent = .false.
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if(par_type .eq. par_type_AVP_par_std) then
            lpresent = .true.
          exit
          endif ! RIN%NDIM%par_type(IDIM1) .eq.
        enddo ! IDIM1
        if(.not. lpresent) then
          write(tmp_message,'(3a)') 'Aerosol vertical profile standard deviation characteristic must be', &
          NEW_LINE('A'), &
          'provided in settings along with gaussian aerosol vertical profile type.'
          G_ERROR(trim(tmp_message))
        endif
      endif
      endif

! Validate presence of concentration characteristic for Lognormal SD model
      if( any(RIN%NDIM%par_type(1:RIN%NDIM%n1) .eq. par_type_SD_LN) ) then
        lpresent = .false.
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if(par_type .eq. par_type_Cv) then
            lpresent = .true.
          exit
          elseif(par_type .eq. par_type_TM_C) then
            lpresent = .true.
          exit
          endif
        enddo ! IDIM1
        if(.not. lpresent) then
          write(tmp_message,'(3a)') 'For Lognormal size distribution model concentration', &
          NEW_LINE('A'), &
          'characteristic must to be provided in settings.'
          G_ERROR(trim(tmp_message))
        endif
      endif

! Validate binning flag (IBIN) of SD
      if(RIN%IBIN .ge. 0) then
        write(tmp_message,'(a,i0,3a)') &
        'Binning flag of SD RIN%IBIN = ',RIN%IBIN, &
        ' radii are equal in absolute scale (see settings file)', &
        NEW_LINE('A'), &
        'is not supported by the code.'
        G_ERROR(trim(tmp_message))
      endif
! Validate Cv characteristic for models type of size disrtribution
      do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if(par_type .gt. par_type_SD_beg  .and. par_type .lt. par_type_SD_end) exit
      enddo ! IDIM1
      if ( par_type .eq. par_type_SD_MD ) then
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if ( par_type .eq. par_type_Cv ) then
          if ( RIN%NSD .gt. 1) then
            if ( RIN%NDIM%n2(IDIM1) .lt. RIN%NSD ) then
              write(tmp_message,'(3a,i0)') &
              'Number of aerosol modes for aerosol concentration characteristic', &
              NEW_LINE('A'), &
              ' in settings must be equal to ', RIN%NSD
              G_ERROR(trim(tmp_message))
            endif
            exit
          endif !
          endif ! par_type .eq.
        enddo ! IDIM1
      endif

! Validate both igab and int_method settings
      do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if(par_type .gt. par_type_gases_concentration_beg .and. &
             par_type .lt. par_type_gases_concentration_end) then
if(.not. RIN%gases%igab .and. (RIN%gases%integration_method.eq. 1))then
            write(tmp_message,'(a)') &
            'Settings inconsistency: k-distribution cannot be used if gases are not included.'
            G_ERROR(trim(tmp_message))
          exit
          endif
          endif
      enddo ! IDIM1

goto 55
! Validate shape disrtribution
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if ( par_type .eq. par_type_SHD_distr) then
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
          NSHAPE = RIN%NDIM%n3(IDIM2,IDIM1) + 1
          if ( NSHAPE .eq. 2 ) then
            write(tmp_message,'(5a,i0)') &
            'Number of ratios in settings must be > 2 for aspect ratio distribution', &
            NEW_LINE('A'), &
            'or use sphere fraction characteristic', &
            NEW_LINE('A'), &
            'number of ratios = ',NSHAPE
            G_ERROR(trim(tmp_message))
          endif
          enddo ! IDIM2
        exit
        endif
      enddo ! IDIM1
55 continue
! Presence of nrmax and TB or LN SD
      if ( RIN%CUTOFF%nrmax .gt. 0 ) then
        lpresent = .false.
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if ( par_type .eq. par_type_SD_TB .or. &
              par_type .eq. par_type_SD_LN) then
            lpresent = .true.
          endif
        enddo ! IDIM1
        if ( .not. lpresent ) then
          write(tmp_message,'(a)') &
          'TB or LN size distribution model can work with meas type p11 inegrated cut off.'
          G_ERROR(trim(tmp_message))
        endif
      endif

      if ( RIN%use_tmodel ) then
        lpresent = .false.
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if ( par_type .eq. par_type_SD_LN) then
            lpresent = .true.
          endif
        enddo ! IDIM1
        if ( .not. lpresent ) then
          write(tmp_message,'(a)') &
          'Only LN SD model is supported with Transport model.'
          G_ERROR(trim(tmp_message))
        endif
      endif

      return
      end subroutine validator_inversion_settings

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine validator_sdata ( RIN, segment, lSvR )

      use mod_sdata
      use mod_retr_settings_derived_type
      use mod_par_inv,   only : KIP, KVERTM, KIMAGE
      use mod_par_OS,    only : NBVM
      use mod_equal_arrays_check
      use mod_stop_report

      implicit none
! ----------------------------------------------------------------------
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data), intent(in) :: segment
      logical, intent(in) :: lSvR ! =T if SvR measurements are present in sdata
! ----------------------------------------------------------------------
      integer :: ipix, iw, ip, ip1, npixels, meas_type, meas_type1, nmeas_type
      logical :: tod_present, aod_present, abs_present
      real :: meas
      real, dimension(NBVM,KIP) :: vis, fiv, vis1, fiv1
      real :: sza, sza1
      integer :: NBV, NBV1
      logical :: status, status1
      integer :: IDIM1, IDIM2, NDIM3, par_type
      integer                        :: NHVP_meas ! number of heights for vertical profile
      real, dimension(KVERTM,KIMAGE) :: HVP_meas  ! heights for vertical profile
! ----------------------------------------------------------------------
      npixels = segment%npixels 
! validate inversion regime
      !if(npixels .eq. 1 .and. RIN%IPFP%INVSING .gt. 0) then
        !write(tmp_message,'(a,i0,3a)') 'npixels = ',npixels,' can not be inverted with ', &
        !NEW_LINE('A'), &
        !'retrieval.regime_of_multipixel_constraints.inversion_regime = multi_pixel'
        !G_ERROR(trim(tmp_message))
      !endif

! validate surface parameters
      if(lSvR) then
        if(any(NINT(segment%pixels(1:npixels)%land_percent) .ne. 100.) .and. RIN%isurf_water .eq. -999) then
          write(tmp_message,'(3a)') 'Water surface parameters are not provided for pixels with water surface.', &
          NEW_LINE('A'), &
          'Inversion settings and SDATA are inconsistent.'
          G_ERROR(trim(tmp_message))
        endif
        if(any(NINT(segment%pixels(1:npixels)%land_percent) .eq. 100.) .and. RIN%isurf_land(1) .eq. -999) then
          write(tmp_message,'(3a)') 'Land surface parameters are not provided for pixels with land surface.', &
          NEW_LINE('A'), &
          'Inversion settings and SDATA are inconsistent.'
          G_ERROR(trim(tmp_message))
        endif
      else
        !print *, 'lSvR =',lSvR,'  ndim1 = ',RIN%NDIM%n1
        !print *, 'par_type: ',RIN%NDIM%par_type(1:RIN%NDIM%n1)
        do idim1=1,RIN%NDIM%n1
        if ( (RIN%NDIM%par_type(idim1) .gt. par_type_surface_beg) .and. &
             (RIN%NDIM%par_type(idim1) .lt. par_type_surface_end) ) then
          write(tmp_message,'(3a)') 'SDATA segment does not contain SvR measurements.', &
          NEW_LINE('A'), &
          'Surface parameters (characteristics) have to be removed from Settings.'
          G_ERROR(trim(tmp_message))
        endif
        enddo
      endif ! lSvR
! validate sets of angles for radiance and polarization
goto 777
      if ( RIN%iPOBS .eq. 4 .or. RIN%iPOBS .eq. 2 ) then
      loop_pix: do ipix=1,npixels
        do iw=1,segment%pixels(ipix)%nwl
        nmeas_type = segment%pixels(ipix)%meas(iw)%NIP
        do ip=1,nmeas_type
          meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
          NBV = segment%pixels(ipix)%meas(iw)%NBVM(ip)
          if ( meas_type .ge. meas_type_SvR_beg .and. meas_type .le. meas_type_SvR_end ) then
            if ( nmeas_type-ip .gt. 0 ) then
            meas_type1 = segment%pixels(ipix)%meas(iw)%meas_type(ip+1)
            NBV1 = segment%pixels(ipix)%meas(iw)%NBVM(ip+1)
            if ( meas_type1 .ge. meas_type_SvR_beg .and. meas_type1 .le. meas_type_SvR_end ) then
            if ( NBV .ne. NBV1 ) then
              write(tmp_message,'(4(a,i0,3x),a,2(a,i0,3x),a,2(a,i0,3x),2a)') 'ipix = ',ipix,'iw = ',iw, &
              'ip = ',ip,'ip+1 = ',ip+1, &
              NEW_LINE('A'), &
              'meas_type(ip) = ',meas_type,'meas_type(ip+1) = ',meas_type1, &
              NEW_LINE('A'), &
              'NBVM(ip) = ',segment%pixels(ipix)%meas(iw)%NBVM(ip), &
              'NBVM(ip+1) = ',segment%pixels(ipix)%meas(iw)%NBVM(ip+1), &
              NEW_LINE('A'), &
              'number of valid measurements can not be different in order to fit relative polarisation.'
              G_ERROR(trim(tmp_message))
            else
              call get_pixel_geom ( IW, IP,   segment%pixels(ipix), NBV, sza,  vis(:,IP),    fiv(:,IP)    )
              call get_pixel_geom ( IW, IP+1, segment%pixels(ipix), NBV, sza1, vis1(:,IP+1), fiv1(:,IP+1) )
              status = equal_arrays_check(NBV, vis(:,IP), vis1(:,IP+1))
              status1 = equal_arrays_check(NBV, fiv(:,IP), fiv1(:,IP+1))
              if ( .not. status .or. .not. status1) then
              write(tmp_message,'(4(a,i0,3x),a,2(a,i0,3x),a,2(a,i0,3x),2a)') 'ipix = ',ipix,'iw = ',iw, &
              'ip = ',ip,'ip+1 = ',ip+1, &
              NEW_LINE('A'), &
              'meas_type(ip) = ',meas_type,'meas_type(ip+1) = ',meas_type1, &
              NEW_LINE('A'), &
              'NBVM(ip) = ',segment%pixels(ipix)%meas(iw)%NBVM(ip), &
              'NBVM(ip+1) = ',segment%pixels(ipix)%meas(iw)%NBVM(ip+1), &
              NEW_LINE('A'), &
              'values of valid measurements can not be different in order to fit relative polarization.'
              G_ERROR(trim(tmp_message))
              endif
            endif
            endif
            endif ! nmeas_type-ip .gt. 0
          endif ! meas_type .ge. meas_type_SvR_beg .and.
        enddo ! ip
        enddo ! iw
      enddo loop_pix
      endif
777 continue

! validate sets of angles for p11 and p12

! validate values of radiances and polarization
      call validator_sdata_values_segment ( RIN, segment )
      if ( error_present() ) return

! validate measurement types doubled in single pixel
      do ipix=1,npixels
      do iw=1,segment%pixels(ipix)%nwl
      nmeas_type = segment%pixels(ipix)%meas(iw)%NIP
      if ( nmeas_type .gt. 1 ) then
        do ip=1,nmeas_type-1
          meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
          do ip1=ip+1,nmeas_type
            if ( meas_type .eq. segment%pixels(ipix)%meas(iw)%meas_type(ip1) ) then
              write(tmp_message,'(4(a,i0,3x),a,2(a,i0,3x),4a)') 'ipix = ',ipix,'iw = ',iw, &
              'ip = ',ip,'ip1 = ',ip1, &
              NEW_LINE('A'), &
              'meas_type(ip) = ',meas_type, &
              'meas_type(ip1) = ',segment%pixels(ipix)%meas(iw)%meas_type(ip1), &
              NEW_LINE('A'), &
              'Measurements of the same meas type can not be present', &
              NEW_LINE('A'), &
              'for single wavelength in single pixel.'
              G_ERROR(trim(tmp_message))
            endif
          enddo
        enddo ! ip
      endif
      enddo ! iw
      enddo

! validate presence of tod, aod, abs measurement types in segment
      tod_present = .false.
      aod_present = .false.
      abs_present = .false.
      do ipix=1,npixels
      do iw=1,segment%pixels(ipix)%nwl
        nmeas_type = segment%pixels(ipix)%meas(iw)%NIP
        do ip=1,nmeas_type
          meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
          if(meas_type .eq. meas_type_tod) then
            tod_present = .true.
          elseif(meas_type .eq. meas_type_aod) then
            aod_present = .true.
          elseif(meas_type .eq. meas_type_aaod) then
            abs_present = .true.
          endif
        enddo ! ip
      enddo ! iw
      enddo
      if ( tod_present ) then
        if ( aod_present .or. abs_present ) then
        write(tmp_message,'(3(a,l,3x),2a)') 'tod_present = ',tod_present, &
        'aod_present = ',aod_present,'abs_present = ',abs_present, &
        NEW_LINE('A'), &
        'tod OR aod OR abs measurement type can be present in segment.'
        G_ERROR(trim(tmp_message))
        endif
      else
        if ( aod_present .and. abs_present ) then
        write(tmp_message,'(3(a,l,3x),2a)') 'tod_present = ',tod_present, &
        'aod_present = ',aod_present,'abs_present = ',abs_present, &
        NEW_LINE('A'), &
        'Only one measurement type (tod/aod/abs) can be present in segment.'
        G_ERROR(trim(tmp_message))
        endif
      endif

! Validate number of profile values and number of lidar signal measurements
      if ( .not. RIN%indep_par ) then
        do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if( par_type .eq. par_type_AVP_prof ) then
          IDIM2 = 1
          NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
          call get_HVP_lidar ( segment, NHVP_meas, HVP_meas )
          if ( NDIM3 .ne. NHVP_meas ) then
            write(tmp_message,'(a,i0,2a,i0)') 'Number of profile values in settings = ',NDIM3, &
            NEW_LINE('A'), &
            'must be equal to number of lidar signal measurements in sdata = ',NHVP_meas
            G_ERROR(trim(tmp_message))
          endif ! par_type .eq.
        endif
        enddo ! IDIM1
      endif

! validate decending order of altitudes and normalization of lidar signal
      call validator_lidar ( segment )
      if ( error_present() ) return

! validate geometries of p11 cut off measurements in order to substract cutoff measurements.
      if ( RIN%CUTOFF%cutoff_meas_diff .eqv. .true. ) then
      if ( RIN%CUTOFF%nrmax .gt. 1 ) then
        do ipix=1,npixels
        do iw=1,segment%pixels(ipix)%nwl
        nmeas_type = segment%pixels(ipix)%meas(iw)%NIP
        do ip=1,nmeas_type
          meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
          NBV = segment%pixels(ipix)%meas(iw)%NBVM(ip)
          if ( meas_type .eq. meas_type_p11_intd_cut_off_1) then
            meas_type1 = meas_type
            NBV1 = NBV
            ip1 = ip
            call get_pixel_geom ( iw, ip, segment%pixels(ipix), NBV1, sza1, vis(:,ip), fiv(:,ip) )
            cycle
          endif
          if ( meas_type .ge. meas_type_p11_intd_cut_off_2 .and. &
               meas_type .le. meas_type_p11_intd_cut_off_4 ) then
            if ( NBV .ne. NBV1 ) then
              write(tmp_message,'(3(a,i0,3x),a,2(a,i0,3x),a,2(a,i0,3x),2a)') &
              'ipix = ',ipix,'iw = ',iw,'ip = ',ip, &
              NEW_LINE('A'), &
              'meas_type(ip) = ',meas_type,'meas_type1 = ',meas_type1, &
              NEW_LINE('A'), &
              'NBVM(ip) = ',NBV,'NBVM1 = ',NBV1, &
              NEW_LINE('A'), &
              'number of valid measurements can not be different in order to substract cutoff measurements.'
              G_ERROR(trim(tmp_message))
            else
              call get_pixel_geom ( iw, ip, segment%pixels(ipix), NBV, sza, vis(:,ip), fiv(:,ip) )
              status  = equal_arrays_check(NBV, vis(:,ip), vis(:,ip1))
              status1 = equal_arrays_check(NBV, fiv(:,ip), fiv(:,ip1))
              if ( sza .ne. sza1 .or. .not. status .or. .not. status1) then
              write(tmp_message,'(3(a,i0,3x),a,2(a,i0,3x),a,2(a,i0,3x),2a)') &
              'ipix = ',ipix,'iw = ',iw,'ip = ',ip, &
              NEW_LINE('A'), &
              'meas_type(ip) = ',meas_type,'meas_type1 = ',meas_type1, &
              NEW_LINE('A'), &
              'NBVM(ip) = ',NBV,'NBVM1 = ',NBV1, &
              NEW_LINE('A'), &
              'values of valid measurements can not be different in order to substract cutoff measurements.'
              G_ERROR(trim(tmp_message))
              endif
            endif
          endif
        enddo ! ip
        enddo ! iw
        enddo ! ipix
      endif
      endif

      return
      end subroutine validator_sdata

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine validator_lidar ( segment )

      use mod_sdata
      use mod_stop_report

      implicit none
!	------------------------------------------------------------------------------------------------------
      type(segment_data),  intent(in)     ::  segment
!	------------------------------------------------------------------------------------------------------
      integer :: meas_type,ipix,iw,ip,i
      integer :: NHVP
      real, dimension(KVERTM) :: HVP
      real :: LS_INT
      real,parameter :: tiny = 1.0e-1 !AL allow 10% threshold to avoid false errors due to accuracy losses and possible presence of synth noise
!	------------------------------------------------------------------------------------------------------
      NHVP = 0

      do ipix=1,segment%npixels
      HVP(:) = 0.0
      do iw=1,segment%pixels(ipix)%nwl
      do ip=1,segment%pixels(ipix)%meas(iw)%NIP
        meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
        if(meas_type .ge. meas_type_lid_beg) then
        if(meas_type .le. meas_type_lid_end) then
          NHVP = segment%pixels(ipix)%meas(iw)%NBVM(ip)
          HVP(1:NHVP) = segment%pixels(ipix)%HVP(1:NHVP)
! validate descendig order of altitudes
          if ( HVP(1) .lt. HVP(NHVP) ) then
            write(tmp_message,'(3(a,i0),2(2x,a,es11.4),2a)') 'ipix = ',ipix, &
            ' iw =', iw, ' ip =', ip, &
            ' HVP(1) =',HVP(1),' HVP(NHVP) =',HVP(NHVP), &
            NEW_LINE('A'), &
            'Lidar altitudes in sdata are not in descending order.'
            G_ERROR(trim(tmp_message))
          endif
          ! validate lidar signal normalization
          if(meas_type .eq. meas_type_LS) then
             LS_INT = 0.0
             do i=NHVP-1,1,-1
                LS_INT = LS_INT + 0.5*(segment%pixels(ipix)%meas(iw)%LS(i) + &
                segment%pixels(ipix)%meas(iw)%LS(i+1))*(HVP(i)-HVP(i+1))
             enddo
             if ( abs((LS_INT-1.0)/LS_INT) .gt. tiny ) then
                write(tmp_message,'(3(a,i0),2x,a,es11.4,2a)') 'ipix = ',ipix, &
                ' iw = ', iw, ' ip = ', ip, &
                ' LS_INT =',LS_INT, NEW_LINE('A'), &
                'Elastic Lidar signal in sdata is not normalized.'
                G_ERROR(trim(tmp_message))
             endif
          endif ! MEAS type=LS
          if(meas_type .eq. meas_type_RL) then
             LS_INT = 0.0
             do i=NHVP-1,1,-1
                LS_INT = LS_INT + 0.5*(segment%pixels(ipix)%meas(iw)%RL(i) + &
                segment%pixels(ipix)%meas(iw)%RL(i+1))*(HVP(i)-HVP(i+1))
             enddo
             if ( abs((LS_INT-1.0)/LS_INT) .gt. tiny ) then
                write(tmp_message,'(3(a,i0),2x,a,es11.4,2a)') 'ipix = ',ipix, &
                ' iw = ', iw, ' ip = ', ip, &
                ' LS_INT =',LS_INT, NEW_LINE('A'), &
                'Raman Lidar signal in sdata is not normalized.'
                G_ERROR(trim(tmp_message))
             endif
         endif ! MEAS type=RL
        endif
        endif
      enddo ! ip
      enddo ! iw
      enddo ! ipix

      return
      end subroutine validator_lidar

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine validator_sdata_values_segment ( RIN, segment )

      use mod_sdata
      use mod_retr_settings_derived_type
      use mod_stop_report
      use mod_par_inv, only : KW, KIP, KNBVM

      implicit none
! ----------------------------------------------------------------------
      type(retr_input_settings),  intent(in) :: RIN
      type(segment_data),         intent(in) :: segment
      integer :: ipix, iw, ip, iwl
      integer :: npixels, nwl, nmeas_type, nvalid_meas
      integer :: meas_type
      real :: meas
      integer :: cloudy
      real, dimension(KW) :: sza

! ----------------------------------------------------------------------
      !real, parameter :: masl_min = -100.
      !real, parameter :: masl_max = 8000.
      real, parameter :: lon_min = -180. ! x
      real, parameter :: lon_max =  180. ! x
      real, parameter :: lat_min = -90.  ! y
      real, parameter :: lat_max =  90.  ! y
      real, parameter :: land_percent_min =    0.
      real, parameter :: land_percent_max =  100.

      real, parameter :: sza_min =  0.
      real, parameter :: sza_max = 90.

      integer, parameter :: nwl_min =  1
      integer, parameter :: nwl_max = KW

      integer, parameter :: nmeas_type_min =  1
      integer, parameter :: nmeas_type_max = KIP

      integer, parameter :: nvalid_meas_min =  1
      integer, parameter :: nvalid_meas_max = KNBVM

      real :: HOBS_min
      real, parameter :: HOBS_max = 950000.

! ----------------------------------------------------------------------
      npixels = segment%npixels

! validate values of radiances and polarization
loop_pix: do ipix=1,npixels
      cloudy = segment%pixels(ipix)%cloudy
      if(.not. (cloudy .eq. 1 .or. cloudy .eq. 0)) then
          write(tmp_message,'(2(a,i0,3x),a)') 'ipix = ',ipix, &
          'cloudy = ',cloudy,'not valid value (0/1)'
          G_ERROR(trim(tmp_message))
      endif
      call check_sdata_real ( ipix, 0, 0, 0, 'lon', segment%pixels(ipix)%x, lon_min, lon_max )
      if ( error_present() ) return
      call check_sdata_real ( ipix, 0, 0, 0, 'lat', segment%pixels(ipix)%y, lat_min, lat_max )
      if ( error_present() ) return
      !call check_sdata_real ( ipix, 0, 0, 0, 'masl', segment%pixels(ipix)%MASL, masl_min, masl_max )
      !if ( error_present() ) return
      call check_sdata_real ( ipix, 0, 0, 0, 'land_percent', segment%pixels(ipix)%land_percent, land_percent_min, land_percent_max )
      if ( error_present() ) return
      call check_sdata_real ( ipix, 0, 0, 0, 'land_percent', segment%pixels(ipix)%land_percent, land_percent_min, land_percent_max )
      if ( error_present() ) return
      HOBS_min = segment%pixels(ipix)%MASL
      call check_sdata_real ( ipix, 0, 0, 0, 'HOBS', segment%pixels(ipix)%HOBS, HOBS_min, HOBS_max )
      if ( error_present() ) return

      nwl = segment%pixels(ipix)%nwl
      call check_sdata_int ( ipix, 0, 0, 0, 'nwl', nwl, nwl_min, nwl_max )
      if ( error_present() ) return

      iwl = 0
      sza(:) = 0.0
      do iw=1,nwl
        nmeas_type = segment%pixels(ipix)%meas(iw)%NIP
        call check_sdata_int ( ipix, iw, 0, 0, 'nmeas_type', nmeas_type, nmeas_type_min, nmeas_type_max )
        if ( error_present() ) return
        call check_sdata_real ( ipix, iw, 0, 0, 'sza', segment%pixels(ipix)%meas(iw)%sza, sza_min, sza_max )
        if ( error_present() ) return
        do ip=1,nmeas_type
          meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
          nvalid_meas = segment%pixels(ipix)%meas(iw)%NBVM(ip)
          call check_sdata_int ( ipix, iw, ip, 0, 'nvalid_meas', nvalid_meas, nvalid_meas_min, nvalid_meas_max )
          if ( error_present() ) return
          call check_sdata_meas ( ipix, iw, ip, nvalid_meas, meas_type, segment%pixels(ipix) )
          if ( error_present() ) return
        enddo ! ip

! copying sounding zenith angles and and sounding ranges for all lidar wl in one pixel
        if(ANY(segment%pixels(ipix)%meas(iw)%meas_type(1:nmeas_type) .GE. meas_type_lid_beg .AND. &
               segment%pixels(ipix)%meas(iw)%meas_type(1:nmeas_type) .LE. meas_type_lid_end) ) then
          iwl = iwl+1
          sza(iwl) = segment%pixels(ipix)%meas(iw)%sza
        endif
      enddo ! iw
! checking if all sza for lidar are the same in one pixel
! to do make standalone function like chek_sdata_real
      sza(1:iwl) = ABS(sza(1:iwl)-sza(1))
      if(ANY(sza(1:iwl) .GT. 1.0e-9)) then
         write(tmp_message,'(a,i0)') &
         'Sounding angles (SZA) for lidar channels are different in pixel ',ipix
         G_ERROR(trim(tmp_message))
      endif

enddo loop_pix

      return
      end subroutine validator_sdata_values_segment

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine check_sdata_meas ( ipix, iw, ip, nvalid_meas, meas_type, segment_pixel )

        use mod_stop_report
        use mod_sdata_derived_type
        use mod_sdata_meas_type
        use mod_par_OS, only : HMAX_atm

        implicit none
! ----------------------------------------------------------------------
        type(pixel),intent(in) :: segment_pixel
        integer, intent(in) :: ipix, iw, ip
        integer, intent(in) :: nvalid_meas, meas_type
! ----------------------------------------------------------------------
        real, parameter :: p11_min = 1e-6
        real, parameter :: p11_max = 10000.
        real, parameter :: p12_min = -100.
        real, parameter :: p12_max =  100.
        real, parameter :: p22_min = 1e-6
        real, parameter :: p22_max = 10000.
        real, parameter :: p33_min = -100.
        real, parameter :: p33_max =  100.
        real, parameter :: p34_min = -100.
        real, parameter :: p34_max =  100.
        real, parameter :: p44_min = -100.
        real, parameter :: p44_max =  100.
        real, parameter :: p11_rel_ang_min = 1e-6
        real, parameter :: p11_rel_ang_max = 1000.
        real, parameter :: p12_rel_min = -1.
        real, parameter :: p12_rel_max =  1.
        real, parameter :: p11_intd_min = 1e-9
        real, parameter :: p11_intd_max = 1000.

        real, parameter :: aod_min = 1e-6
        real, parameter :: aod_max =  100.
        real, parameter :: tod_min = 1e-6
        real, parameter :: tod_max =  100.
        real, parameter :: htod_min = 1e-6
        real, parameter :: htod_max =  100.
        real, parameter :: aaod_min = 1e-6
        real, parameter :: aaod_max =  100.

        real, parameter :: I_min = 1e-11
        real, parameter :: I_max =  100.
        real, parameter :: Q_min = -100.
        real, parameter :: Q_max =  100.
        real, parameter :: U_min = -100.
        real, parameter :: U_max =  100.
        real, parameter :: I_rel_sum_min = 1e-11
        real, parameter :: I_rel_sum_max = 1.

        real, parameter :: P_min = -100.
        real, parameter :: P_max =  100.
        real, parameter :: P_rel_min = -1.
        real, parameter :: P_rel_max =  1.

        real, parameter :: LS_min = 1e-15
        real, parameter :: LS_max =    1.
        real, parameter :: RL_min = 1e-15
        real, parameter :: RL_max =    1.
        real, parameter :: DP_min = 1e-15
        real, parameter :: DP_max =    100.! unit %

        ! thetav_SvR range is for both satellite and sunphotometer
        ! (almucantar and principle plane) geometries
        real, parameter :: thetav_SvR_min = -180.
        real, parameter :: thetav_SvR_max =  180.
        real, parameter :: thetav_phmx_min =   0.
        real, parameter :: thetav_phmx_max = 180.
        real, parameter :: phi_min = -360.
        real, parameter :: phi_max =  360.
        real, parameter :: alt_min = -100.
        real, parameter :: alt_max = HMAX_atm

        integer :: iv
        real :: meas, thetav, phi, alt, ang, num
        real :: I, U, Q, P
! ----------------------------------------------------------------------

      select case(meas_type)
      case(meas_type_aod)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%aod(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'aod', meas, aod_min, aod_max )
            if ( error_present() ) return
          enddo
      case(meas_type_tod)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%tod(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'tod', meas, tod_min, tod_max )
            if ( error_present() ) return
          enddo
      case(meas_type_htod)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%htod(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'htod', meas, htod_min, htod_max )
            if ( error_present() ) return
      enddo
      case(meas_type_aaod)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%aaod(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'aaod', meas, aaod_min, aaod_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p11)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_phmx_min, thetav_phmx_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%p11(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p11', meas, p11_min, p11_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p11_rel_ang)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_phmx_min, thetav_phmx_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%p11_rel_ang(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p11_rel_ang', meas, p11_rel_ang_min, p11_rel_ang_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p11_intd)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%p11_intd(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p11_intd', meas, p11_intd_min, p11_intd_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p11_intd_cut_off_1)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%p11_intd_cut_off_1(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p11_intd_cut_of_1', meas, p11_intd_min, p11_intd_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p11_intd_cut_off_2)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%p11_intd_cut_off_2(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p11_intd_cut_of_2', meas, p11_intd_min, p11_intd_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p11_intd_cut_off_3)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%p11_intd_cut_off_3(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p11_intd_cut_of_3', meas, p11_intd_min, p11_intd_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p11_intd_cut_off_4)
          do iv=1,nvalid_meas
            meas = segment_pixel%meas(iw)%p11_intd_cut_off_4(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p11_intd_cut_of_4', meas, p11_intd_min, p11_intd_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p12)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_phmx_min, thetav_phmx_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%p12(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p12', meas, p12_min, p12_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p12_rel)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_phmx_min, thetav_phmx_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%p12_rel(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p12_rel', meas, p12_rel_min, p12_rel_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p22)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_phmx_min, thetav_phmx_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%p22(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p22', meas, p22_min, p22_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p33)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_phmx_min, thetav_phmx_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%p33(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p33', meas, p33_min, p33_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p34)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_phmx_min, thetav_phmx_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%p34(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p34', meas, p34_min, p34_max )
            if ( error_present() ) return
          enddo
      case(meas_type_p44)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_phmx_min, thetav_phmx_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%p44(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'p44', meas, p44_min, p44_max )
            if ( error_present() ) return
          enddo
      case(meas_type_LS)
          do iv=1,nvalid_meas
            alt = segment_pixel%HVP(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'HVP', alt, alt_min, alt_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%LS(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'LS', meas, LS_min, LS_max )
            if ( error_present() ) return
          enddo
      case(meas_type_DP)
          do iv=1,nvalid_meas
            alt = segment_pixel%HVP(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'HVP', alt, alt_min, alt_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%DP(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'DP', meas, DP_min, DP_max )
            if ( error_present() ) return
          enddo
      case(meas_type_DPAR)
          do iv=1,nvalid_meas
            alt = segment_pixel%HVP(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'HVP', alt, alt_min, alt_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%DPAR(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'DPAR', meas, DP_min, DP_max )
            if ( error_present() ) return
          enddo
      case(meas_type_DPER)
          do iv=1,nvalid_meas
            alt = segment_pixel%HVP(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'HVP', alt, alt_min, alt_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%DPAR(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'DPER', meas, DP_min, DP_max )
            if ( error_present() ) return
          enddo
      case(meas_type_RL)
          do iv=1,nvalid_meas
            alt = segment_pixel%HVP(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'HVP', alt, alt_min, alt_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%RL(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'RL', meas, RL_min, RL_max )
            if ( error_present() ) return
          enddo
      case(meas_type_VBS)
          do iv=1,nvalid_meas
            alt = segment_pixel%HVP(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'HVP', alt, alt_min, alt_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%VBS(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'VBS', meas, RL_min, RL_max )
            if ( error_present() ) return
          enddo
     case(meas_type_VEXT)
         do iv=1,nvalid_meas
            alt = segment_pixel%HVP(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'HVP', alt, alt_min, alt_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%VEXT(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'VEXT', meas, RL_min, RL_max )
            if ( error_present() ) return
         enddo
      case(meas_type_I)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_SvR_min, thetav_SvR_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%I(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'I', meas, I_min, I_max )
            if ( error_present() ) return
          enddo
      case(meas_type_I_rel_sum)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_SvR_min, thetav_SvR_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%I_rel_sum(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'I_rel_sum', meas, I_rel_sum_min, I_rel_sum_max )
            if ( error_present() ) return
          enddo
      case(meas_type_Q)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_SvR_min, thetav_SvR_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%Q(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'Q', meas, Q_min, Q_max )
            if ( error_present() ) return

            num = segment_pixel%meas(iw)%NBVM(ip-1)
            if(nvalid_meas .eq. num) then
              ang = segment_pixel%meas(iw)%thetav(iv,ip-1)
              if(thetav .eq. ang) then
                ang = segment_pixel%meas(iw)%phi(iv,ip-1)
                if(phi .ne. ang) then
                  write(tmp_message,'(4(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'iv = ',iv,'phi differs for I, Q and U'
                  G_ERROR(trim(tmp_message))
                endif
              else
                write(tmp_message,'(4(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'iv = ',iv,'thetav differs for I, Q and U'
                G_ERROR(trim(tmp_message))
              endif
            else
              write(tmp_message,'(3(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'number of observations differs for I, Q and U '
              G_ERROR(trim(tmp_message)) 
            endif
          enddo
      case(meas_type_U)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_SvR_min, thetav_SvR_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%U(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'I', meas, U_min, U_max )
            if ( error_present() ) return
!
            num = segment_pixel%meas(iw)%NBVM(ip-2)
            if(nvalid_meas .eq. num) then
              ang = segment_pixel%meas(iw)%thetav(iv,ip-2)
                if(thetav .eq. ang) then
                  ang = segment_pixel%meas(iw)%phi(iv,ip-2)
                  if(phi .eq. ang) then
                    I = segment_pixel%meas(iw)%I(iv)
                    U = segment_pixel%meas(iw)%U(iv)
                    Q = segment_pixel%meas(iw)%Q(iv)
                    P = sqrt(Q*Q+U*U)
                    meas = P/I
                    call check_sdata_real ( ipix, iw, ip, iv, 'P/I', meas, 0., P_rel_max )
                    if ( error_present() ) return
                  else
                    write(tmp_message,'(4(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'iv = ',iv,'phi differs for I, Q and U'
                    G_ERROR(trim(tmp_message))
                  endif
                else
                  write(tmp_message,'(4(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'iv = ',iv,'thetav differs for I, Q and U'
                  G_ERROR(trim(tmp_message))
                endif
                else
                  write(tmp_message,'(3(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'number of observations differs for I, Q and U'
                  G_ERROR(trim(tmp_message))                  
            endif
          enddo
      case(meas_type_P)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_SvR_min, thetav_SvR_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%P(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'P', meas, P_min, P_max )
            if ( error_present() ) return
!
            num = segment_pixel%meas(iw)%NBVM(ip-1)
            if(nvalid_meas .eq. num) then
              ang = segment_pixel%meas(iw)%thetav(iv,ip-1)
              if(thetav .eq. ang) then
                ang = segment_pixel%meas(iw)%phi(iv,ip-1)
                if(phi .eq. ang) then
                  I = segment_pixel%meas(iw)%I(iv)
                  P = meas
                  meas = P/I
                  call check_sdata_real ( ipix, iw, ip, iv, 'P/I', meas, P_rel_min, P_rel_max )
                  if ( error_present() ) return
                else
                    write(tmp_message,'(4(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'iv = ',iv,'phi differs for I and P'
                    G_ERROR(trim(tmp_message))
                endif
              else
                write(tmp_message,'(4(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'iv = ',iv,'thetav differs for I and P'
                G_ERROR(trim(tmp_message))
              endif
            else
              write(tmp_message,'(3(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
                          'ip = ',ip,'number of observations differs for I and P'
                G_ERROR(trim(tmp_message)) 
            endif
          enddo
      case(meas_type_P_rel)
          do iv=1,nvalid_meas
            thetav = segment_pixel%meas(iw)%thetav(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'thetav', thetav, thetav_SvR_min, thetav_SvR_max )
            if ( error_present() ) return
            phi = segment_pixel%meas(iw)%phi(iv,ip)
            call check_sdata_real ( ipix, iw, ip, iv, 'phi', phi, phi_min, phi_max )
            if ( error_present() ) return
            meas = segment_pixel%meas(iw)%P_rel(iv)
            call check_sdata_real ( ipix, iw, ip, iv, 'P_rel', meas, P_rel_min, P_rel_max )
            if ( error_present() ) return
          enddo
      case default
          write(tmp_message,'(4(a,i0,3x),a)') 'ipix = ',ipix,'iw = ',iw, &
          'ip = ',ip,'meas_type = ',meas_type,'not valid value'
          G_ERROR(trim(tmp_message))
      end select

      end subroutine check_sdata_meas

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine check_sdata_real ( ipix, iw, ip, iv, str, x, xmin, xmax )

        use mod_stop_report

        implicit none
! ----------------------------------------------------------------------------
        integer, intent(in) :: ipix, iw, ip, iv
        character(*), intent(in) :: str
        real, intent(in) :: x, xmin, xmax
! ----------------------------------------------------------------------------

        if ( .not. ( xmin .le. x .and. xmax .ge. x ) ) then
          if ( iw .eq. 0 ) then
            write(tmp_message,'(a,i0,3x,2a,es11.4,3x,2(a,es11.4),a)') &
            'ipix = ',ipix, &
            NEW_LINE('A'), &
            trim(str)//' = ',x,'is not valid value ( ',xmin,'  - ',xmax,' )'
            G_ERROR(trim(tmp_message))
          elseif ( iw .ne. 0 .and. ip .eq. 0 ) then
            write(tmp_message,'(2(a,i0,3x),2a,es11.4,3x,2(a,es11.4),a)') &
            'ipix = ',ipix,'iw = ',iw, &
            NEW_LINE('A'), &
            trim(str)//' = ',x,'is not valid value ( ',xmin,'  - ',xmax,' )'
            G_ERROR(trim(tmp_message))
          elseif ( iw .ne. 0 .and. ip .ne. 0 .and. iv .eq. 0 ) then
            write(tmp_message,'(3(a,i0,3x),2a,es11.4,3x,2(a,es11.4),a)') &
            'ipix = ',ipix,'iw = ',iw,'ip = ',ip, &
            NEW_LINE('A'), &
            trim(str)//' = ',x,'is not valid value ( ',xmin,'  - ',xmax,' )'
            G_ERROR(trim(tmp_message))
          elseif ( iw .ne. 0 .and. ip .ne. 0 .and. iv .ne. 0 ) then
            write(tmp_message,'(4(a,i0,3x),2a,es11.4,3x,2(a,es11.4),a)') &
            'ipix = ',ipix,'iw = ',iw,'ip = ',ip,'iv = ',iv, &
            NEW_LINE('A'), &
            trim(str)//' = ',x,'is not valid value ( ',xmin,'  - ',xmax,' )'
            G_ERROR(trim(tmp_message))
          endif
        endif

      end subroutine check_sdata_real

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine check_sdata_int ( ipix, iw, ip, iv, str, n, nmin, nmax )

        use mod_stop_report

        implicit none
! ----------------------------------------------------------------------------
        integer, intent(in) :: ipix, iw, ip, iv
        integer, intent(in) :: n, nmin, nmax
        character(*), intent(in) :: str
! ----------------------------------------------------------------------------

        if (.not. ( nmin .le. n .and. nmax .ge. n ) ) then
          if ( iw .eq. 0 ) then
            write(tmp_message,'(a,i0,3x,2a,i0,3x,2(a,i0),a)') &
            'ipix = ',ipix, &
            NEW_LINE('A'), &
            trim(str)//' = ',n,'is not valid value ( ',nmin,' - ',nmax,' )'
            G_ERROR(trim(tmp_message))
          elseif ( iw .ne. 0 .and. ip .eq. 0 ) then
            write(tmp_message,'(2(a,i0,3x),2a,i0,3x,2(a,i0),a)') &
            'ipix = ',ipix,'iw = ',iw, &
            NEW_LINE('A'), &
            trim(str)//' = ',n,'is not valid value ( ',nmin,' - ',nmax,' )'
            G_ERROR(trim(tmp_message))
          elseif ( iw .ne. 0 .and. ip .ne. 0 .and. iv .eq. 0 ) then
            write(tmp_message,'(3(a,i0,3x),2a,i0,3x,2(a,i0),a)') &
            'ipix = ',ipix,'iw = ',iw,'ip = ',ip, &
            NEW_LINE('A'), &
            trim(str)//' = ',n,'is not valid value ( ',nmin,' - ',nmax,' )'
            G_ERROR(trim(tmp_message))
          elseif ( iw .ne. 0 .and. ip .ne. 0 .and. iv .ne. 0 ) then
            write(tmp_message,'(4(a,i0,3x),2a,i0,3x,2(a,i0),a)') &
            'ipix = ',ipix,'iw = ',iw,'ip = ',ip,'iv = ',iv, &
            NEW_LINE('A'), &
            trim(str)//' = ',n,'is not valid value ( ',nmin,' - ',nmax,' )'
            G_ERROR(trim(tmp_message))
          endif
        endif

      end subroutine check_sdata_int

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine validator_settings_sdata ( RIN, segment, lSvR )

      use mod_par_inv, only : KKNOISE, KIP
      use mod_sdata
      use mod_sdata_meas_type, only : meas_type_DP
      use mod_retr_settings_derived_type
      use m_inssor
      use mod_equal_arrays_check
      use mod_stop_report

      implicit none
! ----------------------------------------------------------------------
      type(retr_input_settings),  intent(in) :: RIN
      type(segment_data),         intent(in) :: segment
      logical,                    intent(in) :: lSvR ! =T Stokes parameteres are present as measurements
      !type(stop_report_type),     intent(inout) ::  stop_report

      integer :: IDIM1, i, j, ipix, iwl, ip, n1, n2
      integer :: par_type, meas_type
      logical :: lsurf, lvd, status
      integer, dimension(max(KKNOISE,KIP)) :: MEAS_TYPE_noise
      integer, dimension(max(KKNOISE,KIP)) :: MEAS_TYPE_segment
      character(len=20)  :: cn1, cn2
      character(len=150) :: CFMT
      integer :: nrmax(4)
! ----------------------------------------------------------------------
      lsurf = .false.
      lvd = .false.

      if(lSvR) then
! validate if surface characteristics are present in settings
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if(par_type .gt. par_type_surface_beg .and. par_type .lt. par_type_surface_end) then
            lsurf = .true.
          exit
          endif ! RIN%NDIM%par_type(IDIM1) .eq.
        enddo ! IDIM1
        if(.not. lsurf) then
          write(tmp_message,'(3a)') 'Surface characteristics are not provided for radiative transfer.', &
          NEW_LINE('A'), &
          'Inversion settings and SDATA are inconsistent.'
          G_ERROR(trim(tmp_message))
        endif
! validate if vertical distribution characteristics are present in settings
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if(par_type .gt. par_type_AVP_beg .and. par_type .lt. par_type_AVP_end) then
            lvd = .true.
          exit
          endif ! RIN%NDIM%par_type(IDIM1) .eq.
        enddo ! IDIM1
        if (RIN%use_tmodel) then
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if(par_type .eq. par_type_TM_C) then
            lvd = .true.
          exit
          endif ! RIN%NDIM%par_type(IDIM1) .eq.
        enddo ! IDIM1
        endif
        if(.not. lvd) then
          write(tmp_message,'(3a)') &
          'Vertical distribution characteristics are not provided for radiative transfer.', &
          NEW_LINE('A'), &
          'Inversion settings and SDATA are inconsistent.'
        endif
      endif ! lSvR

! validate a set of measurement types in sdata against a set of measurement types for noises in settings
! measurement types from settings
      MEAS_TYPE_noise(:) = 0
      n1 = 1
      MEAS_TYPE_noise(n1) = RIN%NOISE%MT(1,1)
      do i=1,RIN%NOISE%INOISE
      do j=1,RIN%NOISE%NMT(i)
        meas_type = RIN%NOISE%MT(j,i)
        if ( all(MEAS_TYPE_noise(1:n1) .ne. meas_type) ) then
        n1 = n1 + 1
        MEAS_TYPE_noise(n1) = meas_type
        endif
      enddo
      enddo
      call inssor(MEAS_TYPE_noise(1:n1))
! measurement types from segment data
      MEAS_TYPE_segment(:) = 0
      n2 = 1
      MEAS_TYPE_segment(n2) = segment%pixels(1)%meas(1)%meas_type(1)
      do ipix=1,segment%npixels
      do iwl=1,segment%pixels(ipix)%nwl
      do ip=1,segment%pixels(ipix)%meas(iwl)%nip
        meas_type = segment%pixels(ipix)%meas(iwl)%meas_type(ip)
        if ( all(MEAS_TYPE_segment(1:n2) .ne. meas_type) ) then
        n2 = n2 + 1
        MEAS_TYPE_segment(n2) = meas_type
        endif
      enddo
      enddo
      enddo
      call inssor(MEAS_TYPE_segment(1:n2))
      if ( n1 .ne. n2 ) then
        write(cn1,*) n1
        write(cn2,*) n2
        CFMT = '(3a,i0,4x,a,i0,2a,'//trim(adjustl(cn2))//'i5,2a,'//trim(adjustl(cn1))//'i5)'
        write(tmp_message,trim(CFMT)) &
        'Inconsistency between number of measurement types in sdata (n2) and in settings (n1) (see noises).', &
        NEW_LINE('A'), &
        'n2 = ',n2,'n1 = ',n1, &
        NEW_LINE('A'), &
        'sdata meas_types:    ',MEAS_TYPE_segment(1:n2), &
        NEW_LINE('A'), &
        'settings meas_types: ',MEAS_TYPE_noise(1:n1)
        G_ERROR(trim(tmp_message))
      else
        status = equal_arrays_check(n1, MEAS_TYPE_noise(1:n1), MEAS_TYPE_segment(1:n2))
        if ( .not. status ) then
        write(cn1,*) n1
        write(cn2,*) n2
        CFMT = '(3a,i0,4x,a,i0,2a,'//trim(adjustl(cn2))//'i5,2a,'//trim(adjustl(cn1))//'i5)'
        write(tmp_message,trim(CFMT)) &
        'Inconsistency between measurement type sets in sdata and in settings (see noises).', &
        NEW_LINE('A'), &
        'n2 = ',n2,'n1 = ',n1, &
        NEW_LINE('A'), &
        'sdata meas_types:    ',MEAS_TYPE_segment(1:n2), &
        NEW_LINE('A'), &
        'settings meas_types: ',MEAS_TYPE_noise(1:n1)
        G_ERROR(trim(tmp_message))
        endif
      endif

! validate if volume depolarisation observation (DP) is present in sdata and 
! number of scattering matrix elements is set to 4 in settings
      do ipix=1,segment%npixels
      do iwl=1,segment%pixels(ipix)%nwl
      do ip=1,segment%pixels(ipix)%meas(iwl)%nip
        meas_type = segment%pixels(ipix)%meas(iwl)%meas_type(ip)
        if ( meas_type .eq. meas_type_DP ) then
          if ( RIN%DLSF%keyEL .lt. 4 ) then
          write(tmp_message,'(3a,i0,2a)') &
          'Lidar volume depolarisation observation (DP) is present in sdata then', &
          NEW_LINE('A'), &
          'number of scattering matrix elements in settings = ',RIN%DLSF%keyEL, &
          NEW_LINE('A'), &
          'has to be set to 4 in settings.'
          G_ERROR(trim(tmp_message))
          endif
        endif
      enddo
      enddo
      enddo
! validate if polarized Stoks parameters (Q, U, P, P_rel) are present in sdata and
! number of scattering matrix elements is set to 4 in settings
      do ipix=1,segment%npixels
      do iwl=1,segment%pixels(ipix)%nwl
      do ip=1,segment%pixels(ipix)%meas(iwl)%nip
        meas_type = segment%pixels(ipix)%meas(iwl)%meas_type(ip)
        if ( (meas_type .gt. meas_type_Q .and. meas_type .lt. meas_type_P) .or. &
              meas_type .eq. meas_type_P_rel ) then
          if ( RIN%DLSF%keyEL .lt. 4 ) then
          write(tmp_message,'(3a,i0,2a)') &
          'Polarized Stoks element is present in sdata then', &
          NEW_LINE('A'), &
          'number of scattering matrix elements in settings = ',RIN%DLSF%keyEL, &
          NEW_LINE('A'), &
          'has to be set to 4 in settings.'
          G_ERROR(trim(tmp_message))
          endif
        endif
      enddo
      enddo
      enddo

! validate number of cut off measurements provided in sdata against number
! (RIN%cutoff%nrmax) of maximum radii provided in settings
      nrmax(:) = 0
      loop_pix: do ipix=1,segment%npixels
      do iwl=1,segment%pixels(ipix)%nwl
      do ip=1,segment%pixels(ipix)%meas(iwl)%NIP
        meas_type = segment%pixels(ipix)%meas(iwl)%meas_type(ip)
        select case ( meas_type )
        case ( meas_type_p11_intd_cut_off_1 )
          if ( nrmax(1) .eq. 0 ) nrmax(1) = 1
        case ( meas_type_p11_intd_cut_off_2 )
          if ( nrmax(2) .eq. 0 ) nrmax(2) = 1
        case ( meas_type_p11_intd_cut_off_3 )
          if ( nrmax(3) .eq. 0 ) nrmax(3) = 1
        case ( meas_type_p11_intd_cut_off_4 )
          if ( nrmax(4) .eq. 0 ) nrmax(4) = 1
        end select
      enddo ! ip
      enddo ! iw
      enddo loop_pix
      if ( sum(nrmax(:)) .ne. RIN%cutoff%nrmax ) then
          write(tmp_message,'(2(a,i0,x),a)') &
          'Number of cut off meas types = ',sum(nrmax(:)), &
          'in sdata must be equal to number of rmax = ',RIN%cutoff%nrmax,'in settings.'
          G_ERROR(trim(tmp_message))
      endif

      return
      end subroutine validator_settings_sdata

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      logical function SvR_meas_present ( segment )

      use mod_sdata
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------------------------------
      type(segment_data), intent(in) :: segment

      integer :: ipix, iw, ip, npixels, meas_type, nmeas_type
! -----------------------------------------------------------------------------------------
      SvR_meas_present = .false.

      npixels = segment%npixels 
! search if Stokes parameteres are present as measurements
      loop_pix: do ipix=1,npixels
      do iw=1,segment%pixels(ipix)%nwl
      nmeas_type = segment%pixels(ipix)%meas(iw)%NIP
      do ip=1,nmeas_type
        meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
        if(meas_type .gt. meas_type_SvR_beg .and. meas_type .lt. meas_type_SvR_end) then
            SvR_meas_present = .true.
            return
        endif ! meas_type .gt. meas_type_SvR_beg .and.
      enddo ! ip
      enddo ! iw
      enddo loop_pix

      return
      end function SvR_meas_present

!	ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Validate measurement types and fitted measurements in order 
! to add random noise directly to measurements (sdata simulated measurements)
      subroutine validator_polarization_meas_for_random_noise ( RIN, segment )

      use mod_par_inv, only : KNBVM, KIP, KWM
      use mod_retr_settings_derived_type
      use mod_sdata_meas_type
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------
! IN :
      type(retr_input_settings), intent(in) :: RIN
! -----------------------------------------------------------------
! INOUT :	  	  
      type(segment_data), intent(in) :: segment
! -----------------------------------------------------------------
! LOCAL :
      integer :: ipix, iw, ip
      integer :: npixels, meas_type, nvalid_meas
      real,dimension(KNBVM,KIP,KWM)  ::  meas
! ----------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------
! iPOBS = 
!           1    I, Q, U  or P11,P12,P22,P33,P34,P44                          - absolute_polarization_components
!           2    I, Q/I, U/I  or p11,-P12/P11,P22/p11,P33/P11,P34/P11,P44/P11 - relative_polarization_components
!           3    I, P    or sqrt(Q*Q+U*U)                                     - polarized_reflectance
!           4    I, P/I  or sqrt(Q*Q+U*U)/I                                   - degree_of_polarization
! delete?   5    I, P/I meas or p11,-P12/P11,P22/p11,P33/P11,P34/P11,P44/P11  - relative_phase_matrix_meas

! 21 - p11(angle,wl)  - phase matrix element p11
! 22 - p12(angle,wl)  - phase matrix element p12
! 23 - p22(angle,wl)  - phase matrix element p22
! 24 - p33(angle,wl)  - phase matrix element p33
! 25 - p34(angle,wl)  - phase matrix element p34
! 26 - p44(angle,wl)  - phase matrix element p44
! 27 - p11_rel_ang(angle,wl)  - phase matrix element p11 devided by P11 at given angle
! 28 - p12_rel(angle,wl)  - (-p12(angle,wl)/p11(angle,wl))

! 41 - I(angle,wl)    - Stokes parameter I
! 42 - Q(angle,wl)    - Stokes parameter Q
! 43 - U(angle,wl)    - Stokes parameter U
! 44 - P(angle,wl)    - polarization sqrt(Q*Q+U*U) or P/I(iPOBS=5) 
! 45 - I_rel_sum(angle,wl)    - Stokes parameter I(angle,wl)/sum(I(:,wl) )
! 46 - P_rel(angle,wl)    - P_rel(angle,wl)/I(angle,wl) if set of angles for P is different from one for I
!
      npixels = segment%npixels
LOOP_pixels : do ipix=1,npixels
LOOP_WL : do iw=1,segment%pixels(ipix)%nwl
LOOP_meas_type : do ip=1,segment%pixels(ipix)%meas(iw)%NIP
        meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
        if( meas_type .ge. meas_type_p12 .and. meas_type .le. meas_type_p44 ) then
          if ( RIN%iPOBS .ge. 2 .and. RIN%iPOBS .le. 4 ) then
          write(tmp_message,'(2(a,i0,2x),4a)') &
          'iPOBS = ',RIN%iPOBS,'meas_type = ',meas_type, &
          NEW_LINE('A'), &
          'Polarization measurement types in sdata are different from fitting polarization measurements.', &
          NEW_LINE('A'), &
          'Random noise can not be added to simulated measurements (sdata).'
          G_ERROR(trim(tmp_message))
          endif
        elseif( meas_type .ge. meas_type_Q .and. meas_type .le. meas_type_U ) then
          if ( RIN%iPOBS .ge. 2 .and. RIN%iPOBS .le. 4 ) then
          write(tmp_message,'(2(a,i0,2x),4a)') &
          'iPOBS = ',RIN%iPOBS,'meas_type = ',meas_type, &
          NEW_LINE('A'), &
          'Polarization measurement types in sdata are different from fitting polarization measurements.', &
          NEW_LINE('A'), &
          'Random noise can not be added to simulated measurements (sdata).'
          G_ERROR(trim(tmp_message))
          endif
        endif
enddo LOOP_meas_type
enddo LOOP_WL
enddo LOOP_pixels

      return
      end subroutine validator_polarization_meas_for_random_noise

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss



