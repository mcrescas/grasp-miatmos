! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine set_RIN_fields ( RIN, iu_main_output )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN	  
      integer,                    intent(in)    ::  iu_main_output

! -----------------------------------------------------------------------------------------
! Set number of parameters driving forward model KNSING and 
!     number of retrieved parameters KNSINGF
      call set_RIN_retr_par_number ( RIN, iu_main_output )

! Set difference order for single pixel a priori estimate constraints
      call set_RIN_diff_order_apriori ( RIN, iu_main_output )

! Set radii
      call set_RIN_radii ( RIN, iu_main_output )

! Set surface model flags for RT_OSH 
      call set_RIN_RT_OSH_flags_surf ( RIN, iu_main_output )

! Set flag for edges 
      !call set_RIN_edges_flag ( RIN,iu_main_output )

! Set NDIM plus for characteristics with n-1 retrieved parameters
      call set_RIN_NDIM_plus ( RIN, iu_main_output )

! Set FRETR
      call set_print_RIN_FRETR_spectral ( RIN, iu_main_output )

! Set number of transport model levels
      if ( RIN%use_tmodel ) then
      call set_print_RIN_tmodel_nlev ( RIN, iu_main_output )
      endif

! Set number of TB SD radius bins for p11 cut off meas
      if ( RIN%CUTOFF%nrmax .gt. 0 ) then
      call set_RIN_number_of_tb_for_p11_intd_cut_off ( RIN, iu_main_output )
      endif

      return
      end subroutine set_RIN_fields 

!  sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine set_print_RIN_tmodel_nlev ( RIN,iu_main_output )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN
      integer,                    intent(in)    ::  iu_main_output

      integer :: par_type
      integer :: IDIM1,IDIM2
! -----------------------------------------------------------------------------------------

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if ( par_type .eq. par_type_TM_H ) then
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
          RIN%TMSET%nlev = RIN%NDIM%n3(IDIM2,IDIM1)
          enddo ! IDIM2
        endif
      enddo ! IDIM1

      return
      end subroutine set_print_RIN_tmodel_nlev

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine set_RIN_number_of_tb_for_p11_intd_cut_off ( RIN, iu_main_output )

      use mod_retr_settings_derived_type
      use mod_par_type_aerosol, only : par_type_SD_TB, par_type_SD_LN

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN
      integer,                    intent(in)    ::  iu_main_output

      integer :: i, IDIM1, IDIM2, IDIM3
      integer :: par_type
      real :: radius, dlnr
! -----------------------------------------------------------------------------------------
        RIN%CUTOFF%ntb(:,:) = 0 
        do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)
          if ( par_type .eq. par_type_SD_TB ) then
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
              loop_nrmax_tb: do i=1,RIN%CUTOFF%nrmax
              do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
              if ( RIN%RADIUS1(IDIM3,IDIM2) .le. RIN%CUTOFF%rmax(i) ) then
              RIN%CUTOFF%ntb(IDIM2,i) = IDIM3
              RIN%CUTOFF%rmax_tb(IDIM2,i) = RIN%RADIUS1(IDIM3,IDIM2)
              else
              exit
              endif
              enddo ! IDIM3
              enddo loop_nrmax_tb
            enddo ! IDIM2
          elseif ( par_type .eq. par_type_SD_LN ) then
            do IDIM2=1,RIN%NDIM%n2(IDIM1)
              dlnr = (log(RIN%RADIUS1(2,IDIM2))-log(RIN%RADIUS1(1,IDIM2)))/real(RIN%KNLN(IDIM2)-1)
              loop_nrmax_lb: do i=1,RIN%CUTOFF%nrmax
              do IDIM3=1,RIN%KNLN(IDIM2)
              radius = exp(log(RIN%RADIUS1(1,1))+dlnr*(IDIM3-1))
              if ( radius .le. RIN%CUTOFF%rmax(i) ) then
              RIN%CUTOFF%ntb(IDIM2,i) = IDIM3
              RIN%CUTOFF%rmax_tb(IDIM2,i) = radius
              else
              exit
              endif
              enddo ! IDIM3
              enddo loop_nrmax_lb
            enddo ! IDIM2
          endif
        enddo ! IDIM1

      if(RIN%IPRI_verbose) then
        write (iu_main_output,'(a)') 'in set_RIN_number_of_tb_for_p11_intd_cut_off:'
        write (iu_main_output,'(4x,a,15x,a,10f9.4)') 'in settings:', &
        'rmax(1:nrmax) = ',RIN%CUTOFF%rmax(1:RIN%CUTOFF%nrmax)
        do IDIM2=1,RIN%NSD
        write (iu_main_output,'(4x,a,i0,2x,a,10i9)') 'particle component # ',IDIM2, &
        '    ntb(1:nrmax) = ',RIN%CUTOFF%ntb(IDIM2,1:RIN%CUTOFF%nrmax)
        write (iu_main_output,'(4x,a,i0,2x,a,10f9.4)') 'particle component # ',IDIM2, &
        'rmax_tb(1:nrmax) = ',RIN%CUTOFF%rmax_tb(IDIM2,1:RIN%CUTOFF%nrmax)
        enddo
      endif

      return
      end subroutine set_RIN_number_of_tb_for_p11_intd_cut_off

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine set_print_RIN_FRETR_spectral ( RIN,iu_main_output )

      use mod_retr_settings_derived_type
      use mod_functional_retrieval

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN
      integer,                    intent(in)    ::  iu_main_output

      integer   :: IDIM1, IDIM2
      integer   :: par_type, NDIM3
      integer   :: ibeg, iend
! -----------------------------------------------------------------------------------------
      RIN%FRETR%method(:,:) = 1
      RIN%FRETR%function(:,:) = 0
      RIN%FRETR%nn(:,:) = 0
      RIN%FRETR%ipar(:,:,:) = 0

      if ( RIN%IPRI_additional_info ) then
      write(iu_main_output,'(/,a)') 'LIST OF FUNCTIONAL RETRIEVAL OPTIONS:'
      write(iu_main_output,'(/,a)') '1 - retr_method_fullset'
      write(iu_main_output,'(a)')   '2 - retr_method_subset'

      write(iu_main_output,'(/,a)') '1 - retr_function_const'
      write(iu_main_output,'(a,/)') '2 - retr_function_lns_linear'
      write(iu_main_output,'(a)') 'Assignment of FUNCTIONAL RETRIEVAL OPTIONS:'
      endif

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if ((par_type .gt. par_type_surface_beg .and. par_type .lt. par_type_surface_end) .or. &
            (par_type .eq. par_type_RERI_spect) .or. &
            (par_type .eq. par_type_IMRI_spect)) then
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            if (NDIM3 .eq. RIN%NW) then
              RIN%FRETR%method(IDIM2,IDIM1) = retr_method_fullset
            else
              RIN%FRETR%method(IDIM2,IDIM1) = retr_method_subset
              if (NDIM3 .eq. 1) then
                if (RIN%NW .gt. 1) then
                RIN%FRETR%function(IDIM2,IDIM1) = retr_function_const
                RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1) = 1
                endif
              else
                RIN%FRETR%function(IDIM2,IDIM1) = retr_function_lns_linear
                ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
                iend = ibeg + NDIM3 - 1
                RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1) = RIN%IWW_SINGL(ibeg:iend)
                RIN%IWW_SINGL(ibeg:iend) = 0.0
              endif
              RIN%FRETR%nn(IDIM2,IDIM1) = RIN%NW
!print *, 'IDIM1,IDIM2,method,function,ipar(1:NDIM3): ', &
!IDIM1,IDIM2,RIN%FRETR%method(IDIM2,IDIM1),RIN%FRETR%function(IDIM2,IDIM1),RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1)
            endif
          enddo ! IDIM2
        endif

      enddo ! IDIM1

      if ( any(RIN%FRETR%method(:,:) .ne. retr_method_fullset) ) then
        if ( RIN%IPRI_additional_info ) then
          write(iu_main_output,'(a)') 'in set_print_RIN_FRETR_spectral:'
          write(iu_main_output,'(4x,a)') 'LIST OF FUNCTIONAL RETRIEVAL OPTIONS:'
          write(iu_main_output,'(6x,a)') '1 - retr_method_fullset'
          write(iu_main_output,'(6x,a)') '2 - retr_method_subset'

          write(iu_main_output,'(6x,a)') '1 - retr_function_const'
          write(iu_main_output,'(6x,a)') '2 - retr_function_lns_linear'
          write(iu_main_output,'(4x,a)') 'Assignment of FUNCTIONAL RETRIEVAL OPTIONS:'
          do idim1=1,RIN%NDIM%n1
          do idim2=1,RIN%NDIM%n2(IDIM1)
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            write(iu_main_output,'(4x,5(a,i0,2x))') 'idim1 = ',idim1,'idim2 = ',idim2, &
            'method = ',RIN%FRETR%method(IDIM2,IDIM1), &
            'function = ',RIN%FRETR%function(IDIM2,IDIM1),'total_num_pars = ',RIN%FRETR%nn(IDIM2,IDIM1)
            write(iu_main_output,'(6x,a)') 'iwl involved:'
            write(iu_main_output,'(3x,10i4)') RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1)
            ibeg = RIN%NDIM%ISTARSING(IDIM2,IDIM1)
            iend = ibeg + NDIM3 - 1
            write(iu_main_output,'(6x,a)') 'IWW_SINGL:   '
            write(iu_main_output,'(3x,10i4)') RIN%IWW_SINGL(ibeg:iend)
          enddo
          enddo
          write(iu_main_output,*)
        endif
      endif

      return
      end subroutine set_print_RIN_FRETR_spectral

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_RIN_NDIM_plus ( RIN,iu_main_output )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN	  
      integer,                    intent(in)    ::  iu_main_output

      integer :: par_type, num
      integer :: IDIM1, NDIM2
      logical :: cv_present
! -----------------------------------------------------------------------------------------
      RIN%ndim_plus = RIN%ndim
      RIN%flag_plus = .false.

      if ( .not. RIN%indep_par ) return

      cv_present = .false.
      num = 0

! Check presence of particle conceration in settings
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if ( par_type .eq. par_type_Cv ) then
          cv_present = .true.
        exit
        endif
      enddo ! IDIM1

! Check presence of size distribution (TB, LB, MD) characteristic
      if ( cv_present ) then
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if ( par_type .eq. par_type_SD_TB .or. &
             par_type .eq. par_type_SD_LB .or. &
             par_type .eq. par_type_SD_MD &
           ) then
          RIN%flag_plus = .true.
          NDIM2 = RIN%NDIM_plus%n2(IDIM1)
          RIN%ndim_plus%n3(1:NDIM2,IDIM1) = &
          RIN%ndim_plus%n3(1:NDIM2,IDIM1) + 1
        exit
        endif
      enddo ! IDIM1
      endif

! Check presence of parameters
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        select case ( par_type )
        case ( par_type_SHD_distr )
        ! shape distribution
          RIN%flag_plus = .true.
          NDIM2 = RIN%NDIM_plus%n2(IDIM1)
          RIN%ndim_plus%n3(1:NDIM2,IDIM1) = &
          RIN%ndim_plus%n3(1:NDIM2,IDIM1) + 1
        case ( par_type_AVP_prof )
        ! vertical profile characteristics
          RIN%flag_plus = .true.
          NDIM2 = RIN%NDIM_plus%n2(IDIM1)
          RIN%ndim_plus%n3(1:NDIM2,IDIM1) = &
          RIN%ndim_plus%n3(1:NDIM2,IDIM1) + 1
        case ( par_type_CXRI_nmix )
        ! refractive index practical mixture
          RIN%flag_plus = .true.
          NDIM2 = RIN%NDIM_plus%n2(IDIM1)
          RIN%ndim_plus%n3(1:NDIM2,IDIM1) = &
          RIN%ndim_plus%n3(1:NDIM2,IDIM1) + 1
        end select
      enddo ! IDIM1

      return
      end subroutine set_RIN_NDIM_plus

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine set_RIN_retr_par_number ( RIN,iu_main_output )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN	  
      integer,                    intent(in)    ::  iu_main_output

      integer   :: IDIM1,IDIM2	  
! -----------------------------------------------------------------------------------------

! Calculate KNSING and KNSINGF      
      RIN%KNSING = 0
      do IDIM1=1,RIN%NDIM%n1 
          do IDIM2=1,RIN%NDIM%n2(IDIM1) 
            RIN%KNSING = RIN%KNSING+RIN%NDIM%n3(IDIM2,IDIM1)
          enddo ! IDIM2
      enddo ! IDIM1
	  
      RIN%KNSINGF=0
      do IDIM1=1,RIN%NDIM%n1 
        if(RIN%NDIM%par_retr(IDIM1)) then
          do IDIM2=1,RIN%NDIM%n2(IDIM1) 
            RIN%KNSINGF = RIN%KNSINGF+RIN%NDIM%n3(IDIM2,IDIM1)
          enddo ! IDIM2
        endif ! RIN%NDIM%pat_retr(IDIM1)
      enddo ! IDIM1      

      if(RIN%IPRI_verbose) then
        write (iu_main_output,'(a)') 'in set_RIN_retr_par_number:'
        write (iu_main_output,'(4x,a,i0,a)') 'KNSING  = ',RIN%KNSING,' - number of parameters driving forward model for each pixel'
        write (iu_main_output,'(4x,a,i0,a)') 'KNSINGF = ',RIN%KNSINGF,' - number of retrieved parameters for each pixel'
      endif
      	   	  
      return
      end subroutine set_RIN_retr_par_number
            
!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_RIN_diff_order_apriori ( RIN,iu_main_output )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN	  
      integer,                    intent(in)    ::  iu_main_output
! -----------------------------------------------------------------------------------------
! Difference order for single pixel a priori estimate constraints can be only '0'
      RIN%SPCA%IO(:,:,:) = 0

      return
      end subroutine set_RIN_diff_order_apriori
            
!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_RIN_nsph_package_flags_SD ( RIN,iu_main_output )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN	  
      integer,                    intent(in)    ::  iu_main_output

      integer   :: IDIM1,par_type	  
! -----------------------------------------------------------------------------------------
! The parameters for DLS_bin code
! DLSF (DLS Flags) 
!   IWL=0, key=0 - use original Kernels 22/16 bins or lognormal SD   
!   IWL=1, key=2 - use spectral Kernels for precalculated lognormal bins
!   IWL=0, key=3 - retrieve parameters of lognormal SD
! -----------------------------------------------------------------------------------------

! Spheroid package options
      do IDIM1=1,RIN%NDIM%n1
        if(RIN%NDIM%par_type(IDIM1) .gt. par_type_SD_beg .and. RIN%NDIM%par_type(IDIM1) .lt. par_type_SD_end) then
          par_type = RIN%NDIM%par_type(IDIM1)        
          if(par_type     .eq. par_type_SD_TB) then  ! triangle bins
            RIN%DLSF%IWL = 0
            RIN%DLSF%key = 0        
          elseif(par_type .eq. par_type_SD_LB) then  ! precompued lognormal bins
            RIN%DLSF%IWL = 1
            RIN%DLSF%key = 2                
          elseif(par_type .eq. par_type_SD_LN) then  ! parameters of lognormal SD
            RIN%DLSF%IWL = 0
            RIN%DLSF%key = 3
          endif
        endif ! RIN%NDIM%par_type(IDIM1) .gt. par_type_SD_beg .and.
      enddo ! IDIM1
      if(RIN%IPRI_verbose)  then
        write(iu_main_output,*) 'IWL, key, keyEL :'
        write(iu_main_output,'(3i5,a)') RIN%DLSF%IWL,RIN%DLSF%key,RIN%DLSF%keyEL, &
        '  in set_RIN_nsph_package_flags_SD'
      endif
  
      if(((RIN%DLSF%key .eq. 0  .or.   RIN%DLSF%key .eq. 3) .and. (RIN%DLSF%IWL .ne. 0)) .or.  &
	       ((RIN%DLSF%key .eq. 2) .and. (RIN%DLSF%IWL .ne. 1))) then
        write(*,*) 'key=',RIN%DLSF%key,' IWL=',RIN%DLSF%IWL
        write(*,*) '((key.eq.0/3.and.IWL.ne.0).or.(key.eq.2.and.IWL.ne.1))'
        write(*,*) ' SD options for spheroid package are not correct'
        stop 'stop in set_RIN_nsph_package_flags_SD'
      endif ! key and RIN%DLSF%IWL
      	   	  
      return
      end subroutine set_RIN_nsph_package_flags_SD
      
!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine set_RIN_RT_OSH_flags_surf ( RIN,iu_main_output )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN	  
      integer,                    intent(in)    ::  iu_main_output

      integer   :: IDIM1	  
! -----------------------------------------------------------------------------------------
! The parameters for radiative transfer module 
!   iBRDF    
!   iBPDF 
! -----------------------------------------------------------------------------------------
! Surface model options

      RIN%isurf_land(1:2) = -999
      RIN%isurf_water     = -999
           
      do IDIM1=1,RIN%NDIM%n1
        if(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF1_land_beg .and.     &
           RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF1_land_end) then
                      RIN%isurf_land(1) = RIN%NDIM%par_type(IDIM1)        
        elseif(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF2_land_beg .and. &
               RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF2_land_end) then
                      RIN%isurf_land(2) = RIN%NDIM%par_type(IDIM1)        
        elseif(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF_water_beg .and. &
               RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF_water_end) then
                      RIN%isurf_water   = RIN%NDIM%par_type(IDIM1)        
        endif ! RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF1_land_beg .and.
      enddo ! IDIM1

      if(RIN%IPRI_verbose) then
        write(iu_main_output,'(a)') 'in set_RIN_RT_OSH_flags_surf:'
        write(iu_main_output,'(4x,a)') 'isurf_land(1), isurf_land(2), isurf_water:'
        write(iu_main_output,'(1x,3i8,a)') RIN%isurf_land(1),RIN%isurf_land(2),RIN%isurf_water
      endif
      	   	  
      return
      end subroutine set_RIN_RT_OSH_flags_surf

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine set_RIN_radii ( RIN,iu_main_output )

      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------------------------------
      type(retr_input_settings),  intent(inout) ::  RIN	  
      integer,                    intent(in)    ::  iu_main_output
      integer :: i, n
      integer :: IDIM1, IDIM2, IDIM3, par_type
      real    :: AD
      logical :: icv
! -----------------------------------------------------------------------------------------
      icv = .false.
      do i=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(i)
        if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
          icv = .true.
        exit
        endif ! par_type .gt. par_type_Cv_beg .and.
      enddo ! i

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .eq. par_type_SD_TB .or. par_type .eq. par_type_SD_LN) then
          !if ( par_type .eq. par_type_SD_LN ) RIN%KNLN(1:RIN%NDIM%n2(IDIM1)) = 22 ! delete ?
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            n = RIN%NDIM%n3(IDIM2,IDIM1)
            if ( RIN%indep_par ) then
            if ( par_type .eq. par_type_SD_TB .and. icv ) then
            n = n + 1
            endif
            endif
            if(RIN%IBIN .eq. -1) then
              AD = (LOG(RIN%RMAX(IDIM2))-LOG(RIN%RMIN(IDIM2))) / (n-1)
            else ! RIN%IBIN .EQ. 1                                    
              AD = (RIN%RMAX(IDIM2)-RIN%RMIN(IDIM2)) / (n-1)
            endif
            do IDIM3=1,n
              if(RIN%IBIN .eq. -1) then                                     
                RIN%RADIUS1(IDIM3,IDIM2) = EXP(LOG(RIN%RMIN(IDIM2))+AD*(IDIM3-1))
              else ! RIN%IBIN .EQ. 1                                        
                RIN%RADIUS1(IDIM3,IDIM2) = RIN%RMIN(IDIM2)+AD*(IDIM3-1)
              endif
              if(RIN%IPRI_additional_info) then
                write(iu_main_output,*) RIN%RADIUS1(IDIM3,IDIM2),IDIM2,IDIM3, &
                                    ' - RADIUS in set_RIN_radii'
              endif ! RIN%IPRI_additional_info
            enddo ! IDIM3
          enddo ! IDIM2
        exit
        endif ! par_type .eq. par_type_SD_TB
      enddo ! IDIM1

      return
      end subroutine set_RIN_radii

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss 

      subroutine set_input_settings ( iu_main_output, segment_meas, RIN )
      
      use mod_sdata_derived_type
      use mod_sdata, only : set_RIN_wave_length_array
      use mod_retr_settings_derived_type
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------------------------------
      integer,                    intent(in)    ::  iu_main_output
      type(segment_data),         intent(in)    ::  segment_meas
      type(retr_input_settings),  intent(inout) ::  RIN
! -----------------------------------------------------------------------------------------
! Set RIN fields (radii, surface model flags for RT_OSH, number of retrieved parameters )
      call set_RIN_fields ( RIN, iu_main_output )
      
! Set RIN with wave length array for retrieved parameters (combined array of wavelengths in case  
!     different pixels contain niether diff numbers or diff values of wavelengths)
      call set_RIN_wave_length_array ( iu_main_output, segment_meas, RIN )    

      return
      end subroutine set_input_settings

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss



