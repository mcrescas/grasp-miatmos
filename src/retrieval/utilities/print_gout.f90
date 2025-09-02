! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

#include "../constants_set/mod_globals.inc"
!	mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
module mod_print_array
  use mod_stop_report

contains

   subroutine rprint_array_real(iu, array, element_format, from_element, to_element)

     integer,      intent(in) :: iu
     real,         intent(in) :: array(:)
     character(*), intent(in) :: element_format
     integer,      intent(in) :: from_element
     integer,      intent(in) :: to_element
     integer :: i
     integer :: nelements

     nelements = to_element - from_element + 1
     if(nelements .gt. size(array)) then
        write(tmp_message,'(a,i0,3x,a,i8)') &
        'nelements = ',nelements,'size(array)=',size(array)
        G_ERROR(trim(tmp_message))
     endif

     do i = from_element, to_element
        write(iu,fmt=element_format, advance='no') array(i)
     end do
     write(iu,'("")')

   end subroutine rprint_array_real

end module mod_print_array

!	mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      logical function print_status (iu, RIN_flag, GOUT_flag, product_name)

!      use mod_retr_settings_derived_type
      use iso_c_binding

      implicit none
! ----------------------------------------------------------------	  
      integer, intent(in) :: iu
      logical(kind=C_BOOL), intent(in) :: RIN_flag
      logical(kind=C_BOOL), intent(in) :: GOUT_flag
      character(*), intent(in) :: product_name
! ----------------------------------------------------------------
      character(len=50) :: a1, a2
! ----------------------------------------------------------------	 
      a1 = '!!! WARNING!!! products.'
      a2 = ' is not available to be printed'

      print_status = .false.
      if(RIN_flag .eqv. .true.) then
        if(GOUT_flag .eqv. .true.) then
          print_status = .true.
        else
          write(iu,'(a)') trim(a1)//trim(product_name)//trim(a2)
        endif
      endif

      return
      end function print_status

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine set_GOUT_print_flags (iu, RIN_products, GOUT_products, PRN_products)

      use mod_retr_settings_derived_type
            
      implicit none
! ----------------------------------------------------------------	  
      integer, intent(in) ::  iu
      type(output_segment_products), intent(in)  ::  RIN_products
      type(output_segment_products), intent(in)  ::  GOUT_products
      type(output_segment_products), intent(out) ::  PRN_products
! ----------------------------------------------------------------
      logical :: print_status
      character(len=50) :: product_name
! ----------------------------------------------------------------
! retrieval
      ! final inversion residual
        product_name = 'retrieval.res'
      PRN_products%retrieval%res = &
      print_status(iu, RIN_products%retrieval%res, GOUT_products%retrieval%res, product_name)
      ! retrieved parameters
        product_name = 'retrieval.par'
      PRN_products%retrieval%par = &
      print_status(iu, RIN_products%retrieval%par, GOUT_products%retrieval%par, product_name)
      ! real and fitted measurements
        product_name = 'retrieval.fit'
      PRN_products%retrieval%fit = &
      print_status(iu, RIN_products%retrieval%fit, GOUT_products%retrieval%fit, product_name)

! aerosol
      ! aerosol optical depth and absorption optical depth
        product_name = 'aerosol.opt'
      PRN_products%aerosol%opt = &
      print_status(iu, RIN_products%aerosol%opt, GOUT_products%aerosol%opt, product_name)
      ! complex refractive index
        product_name = 'aerosol.rind'
      PRN_products%aerosol%rind = &
      print_status(iu, RIN_products%aerosol%rind, GOUT_products%aerosol%rind, product_name)
      ! particle chemical fractions if retrieved
        product_name = 'aerosol.chem'
      PRN_products%aerosol%chem = &
      print_status(iu, RIN_products%aerosol%chem, GOUT_products%aerosol%chem, product_name)
      ! lognormal size distribution parameters for theoretical fine and coarse aerosol particles
        product_name = 'aerosol.sd2m_mph'
      PRN_products%aerosol%sd2m_mph = &
      print_status(iu, RIN_products%aerosol%sd2m_mph, GOUT_products%aerosol%sd2m_mph, product_name)
      ! aod for theoretical fine and coarse aerosol particles
        product_name = 'aerosol.sd2m_ext'
      PRN_products%aerosol%sd2m_ext = &
      print_status(iu, RIN_products%aerosol%sd2m_ext, GOUT_products%aerosol%sd2m_ext, product_name)
      ! elements of scattering matrix
        product_name = 'aerosol.phmx'
      PRN_products%aerosol%phmx = &
      print_status(iu, RIN_products%aerosol%phmx, GOUT_products%aerosol%phmx, product_name)
      ! lidar ratio
        product_name = 'aerosol.lidar'
      PRN_products%aerosol%lidar = &
      print_status(iu, RIN_products%aerosol%lidar, GOUT_products%aerosol%lidar, product_name)
      ! particulate matter
        product_name = 'aerosol.pm'
      PRN_products%aerosol%pm = &
      print_status(iu, RIN_products%aerosol%pm, GOUT_products%aerosol%pm, product_name)
      ! aerosol type
        product_name = 'aerosol.types'
      PRN_products%aerosol%types = &
      print_status(iu, RIN_products%aerosol%types, GOUT_products%aerosol%types, product_name)

! gases (hyperspectral)
        product_name = 'gases.concentration'
      PRN_products%gases%concentration = &
      print_status(iu, RIN_products%gases%concentration, GOUT_products%gases%concentration, product_name)
        product_name = 'gases.absorption'
      PRN_products%gases%absorption = &
      print_status(iu, RIN_products%gases%absorption, GOUT_products%gases%absorption, product_name)

! surface
        product_name = 'surface.surf'
      PRN_products%surface%surf = &
      print_status(iu, RIN_products%surface%surf, GOUT_products%surface%surf, product_name)
        product_name = 'surface.bhr_iso'
      PRN_products%surface%bhr_iso = &
      print_status(iu, RIN_products%surface%bhr_iso, GOUT_products%surface%bhr_iso, product_name)

! errest
      ! for retrived parameters
        product_name = 'errest.par'
      PRN_products%errest%par = &
      print_status(iu, RIN_products%errest%par, GOUT_products%errest%par, product_name)
    ! aerosol
      ! for aerosol optical properties
        product_name = 'errest.aerosol.opt'
      PRN_products%errest%aerosol%opt = &
      print_status(iu, RIN_products%errest%aerosol%opt, GOUT_products%errest%aerosol%opt, product_name)
      ! for aerosol lidar ratio
        product_name = 'errest.aerosol.lidar'
      PRN_products%errest%aerosol%lidar = &
      print_status(iu, RIN_products%errest%aerosol%lidar, GOUT_products%errest%aerosol%lidar, product_name)
   !MEH:
      ! for aerosol microphysical properties
      product_name = 'errest.aerosol.mic'
      PRN_products%errest%aerosol%mic = &
      print_status(iu, RIN_products%errest%aerosol%mic, GOUT_products%errest%aerosol%mic, product_name)

! forcing
        product_name = 'forcing.bbflux'
      PRN_products%forcing%bbflux = &
      print_status(iu, RIN_products%forcing%bbflux, GOUT_products%forcing%bbflux, product_name)
        product_name = 'forcing.forcing'
      PRN_products%forcing%forcing = &
      print_status(iu, RIN_products%forcing%forcing, GOUT_products%forcing%forcing, product_name)

      return
      end subroutine set_GOUT_print_flags

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Output results
      
      subroutine print_output_results( iu_main_output, RIN, segment_meas, GOUT )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer, intent(in) :: iu_main_output
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data), intent(inout) :: segment_meas
      type(output_segment_general), intent(in) :: GOUT
!----------------------------------------------------------------------------------------
      type(output_segment_products):: PRN_products
!----------------------------------------------------------------------------------------
! Set print GOUT flags
      call set_GOUT_print_flags ( iu_main_output, RIN%products, GOUT%products, PRN_products )

! Print detailed residual for final iteration
      if(PRN_products%retrieval%res) &
      call print_final_iteration_residuals ( iu_main_output, RIN, segment_meas, GOUT )

! Print final retrieval output
      call print_main_output ( iu_main_output, RIN, segment_meas, GOUT )

! Print general output
      ! Theoretical 2 modes properties
      if(PRN_products%aerosol%sd2m_mph .or. PRN_products%aerosol%sd2m_ext) &
      call print_sd2m ( iu_main_output, RIN, segment_meas, GOUT )
      if ( error_present() ) return

      ! Print particulate matter (PM)
      if(PRN_products%aerosol%PM) &
      call print_aerosol_PM ( iu_main_output, RIN, segment_meas, GOUT )

      ! Print aerosol types
      if(PRN_products%aerosol%types) &
      call print_aerosol_types ( iu_main_output, RIN, segment_meas, GOUT )

      ! Print phase matrix
      if(PRN_products%aerosol%phmx) &
      call print_phmx ( iu_main_output, RIN, segment_meas, GOUT )

      ! Print fitting (FS and FPS (measurements and modeled measurements calculated for retrieved parameters))
      if(PRN_products%retrieval%fit) &
      call print_fitting ( iu_main_output, RIN, segment_meas, GOUT%retrieval%fit%segment_fit )

      ! Print error estimates
      if(PRN_products%errest%par .or. PRN_products%errest%aerosol%opt .or. PRN_products%errest%aerosol%lidar .or. PRN_products%errest%aerosol%mic) &
      call print_error_estimates ( iu_main_output, RIN, segment_meas, GOUT )
      if ( error_present() ) return

      ! Print broadband flux
      if(PRN_products%forcing%bbflux) &
      call print_forcing_bbflux ( iu_main_output, RIN, segment_meas, GOUT )

      ! Print net forcing
      if(PRN_products%forcing%forcing) &
      call print_forcing_forcing ( iu_main_output, RIN, segment_meas, GOUT )

      return
      end subroutine print_output_results 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine print_main_output ( iu, RIN, segment_meas, GOUT )

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE,KVERTM
      use mod_par_DLS_bin, only : NRR
      use mod_par_DLS,     only : KNpar
      use mod_par_OS,      only : KSD	  
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_time_utils
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: KNSING, npixels
      type(output_segment_products)        :: PRN_products
!----------------------------------------------------------------------------------------
! Set print GOUT flags
      call set_GOUT_print_flags(iu, RIN%products, GOUT%products, PRN_products)

      KNSING = RIN%KNSING
      npixels = segment_meas%npixels

! Print a table of retrieved parameters
      call print_date_time(iu,segment_meas)
      call print_coordinates(iu,segment_meas)      
      if(PRN_products%retrieval%par) &
      call print_retrieved_parameters(iu,RIN,segment_meas,GOUT)

! Print detailed parameters            
      write(iu,'(/,a)') '*** DETAILED PARAMETERS ***'
      call print_date_time(iu,segment_meas)
      call print_coordinates(iu,segment_meas)

      ! Size Distribution
      call print_size_distribution(iu,RIN,segment_meas,GOUT)
      if ( error_present() ) return

      ! Aerosol Concentration 
      if ( .not. RIN%use_tmodel ) then
      call print_aerosol_concentration(iu,RIN,segment_meas,GOUT)
      endif
      
      ! Gas Concentration
      if(PRN_products%gases%concentration) &
      call print_gas_concentration(iu,RIN,segment_meas,GOUT)

      ! Sphere fraction or Shape distribution
      call print_shape_distribution(iu,RIN,segment_meas,GOUT)

      ! Aerosol Vertical Profile
      call print_aerosol_profile(iu,RIN,segment_meas,GOUT) 

      ! Calibration coefficient
      call print_lidar_calibration_coeffitient(iu,RIN,segment_meas,GOUT)

      ! Angstrom exponent
      call print_angstrom_exponent(iu,RIN,segment_meas,GOUT)

      ! Aerosol optical depth
      if(PRN_products%aerosol%opt) &
      call print_optical_thickness(iu,RIN,segment_meas,GOUT)

      ! Hyperspectral gas absorption
      if(PRN_products%gases%absorption) &
      call print_gas_absorption(iu,RIN,segment_meas,GOUT)

      ! Single Scattering Albedo
      if(PRN_products%aerosol%opt) &
      call print_single_scattering_albedo(iu,RIN,segment_meas,GOUT)

      ! Absorption optical depth
      if(PRN_products%aerosol%opt) &
      call print_absorption(iu,RIN,segment_meas,GOUT)

      ! Real part of refr.index n(wl)
      if(PRN_products%aerosol%rind) &
      call print_refractive_index_real(iu,RIN,segment_meas,GOUT)

      ! Imaginary part of refr.index k(wl)
      if(PRN_products%aerosol%rind) &
      call print_refractive_index_imaginary(iu,RIN,segment_meas,GOUT)

      ! Particle chemical component fractions
      if(PRN_products%aerosol%chem) then
      call print_relative_humidity(iu,RIN,segment_meas,GOUT)
      call print_chemistry(iu,RIN,segment_meas,GOUT)
      endif

      ! Lidar Ratio  	  
      if(PRN_products%aerosol%lidar) &
      call print_lidar_ratio(iu,RIN,segment_meas,GOUT)

      ! Land percent
      if(PRN_products%surface%surf) &
      call print_surface_land_percent(iu,RIN,segment_meas,GOUT)

      ! Surface ndvi
      if(PRN_products%surface%surf) &
      call print_surface_ndvi(iu,RIN,segment_meas,GOUT)

      ! Surface dhr (Directional Hemispherical Reflectance)
      if(PRN_products%surface%surf) &
      call print_surface_dhr(iu,RIN,segment_meas,GOUT)

      ! Surface BHRiso (isotropic BiHemispherical Reflectance)
      if(PRN_products%surface%bhr_iso) &
      call print_surface_bhr_iso(iu,RIN,segment_meas,GOUT)

      ! Surface parameters 
      if(PRN_products%surface%surf) &
      call print_surface_parameters(iu,RIN,segment_meas,GOUT)

      ! Surface parameters
      if(RIN%use_tmodel) then
      call print_level_H(iu,GOUT)
      call print_level_RH(iu,GOUT)
      call print_level_VP(iu,GOUT)
      endif

      if ( error_present() ) then
        stop 'stop in print_main_output'
      endif

      flush(iu)
      return
      end subroutine print_main_output

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Unpack retrieved parameters from retrieved parameter vector

      subroutine unpack_retr_param_to_print ( RIN,GOUT_retrieval_par,IDIM1,npixels,par )
      
      use mod_par_inv, only : KIDIM3,KIDIM2,KIMAGE
      use mod_retr_settings_derived_type 
      use mod_retr_general_output_derived_type
                 
      implicit none
! ----------------------------------------------------------------	  
      integer,                      intent(in)  ::  IDIM1,npixels
      type(retr_input_settings),    intent(in)  ::  RIN
      type(output_segment_retr_par),intent(in)  ::  GOUT_retrieval_par
      real,dimension(KIDIM3,KIDIM2,KIMAGE),intent(out) ::	par
! ----------------------------------------------------------------	  
      integer  :: NDIM3, IDIM2, ipix, i1, i2, NDIM3_plus
      integer  :: par_type
! ----------------------------------------------------------------
      par_type = RIN%NDIM%par_type(IDIM1)
      par(:,:,:)=0.0
      do ipix=1,npixels
        do IDIM2=1,RIN%NDIM%n2(IDIM1) 
            NDIM3=RIN%NDIM%n3(IDIM2,IDIM1)
            NDIM3_plus = RIN%NDIM_plus%n3(IDIM2,IDIM1)
            i1=RIN%NDIM%ISTARSING(IDIM2,IDIM1)
            i2=i1+NDIM3-1
            par(1:NDIM3,IDIM2,ipix) = GOUT_retrieval_par%pixel(ipix)%par(i1:i2)
            !write(*,*) 'ipix=',ipix,'idim2=',idim2,'ndim3=',ndim3,'par(1:NDIM3,IDIM2,ipix)', par(1:NDIM3,IDIM2,ipix)
        enddo ! IDIM2
      enddo ! ipix
          
      return
      end subroutine unpack_retr_param_to_print 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! OUTPUT print: date&time

      subroutine print_date_time ( iu,segment_meas )
      
      use mod_par_inv, only  : KIMAGE
      use mod_sdata_derived_type
            
      implicit none
! ----------------------------------------------------------------	  
      integer,             intent(in)  ::  iu
      type(segment_data),  intent(in)  ::  segment_meas
! ----------------------------------------------------------------	  
      character(len=12),dimension(KIMAGE)  ::  pdate,ptime
      integer  :: ipix,npixels
! ----------------------------------------------------------------	  
      npixels = segment_meas%npixels 
      call yyyymmdd_hhmmss ( segment_meas,pdate,ptime )      
      write(iu,'(a5,10x,20000(3x,A10,x))')  "Date:",(pdate(ipix),ipix=1,npixels)
      write(iu,'(a5,12x,20000(3x,A8,3x))')  "Time:",(ptime(ipix),ipix=1,npixels)
      
      return
      end subroutine print_date_time 
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! OUTPUT print: coordinates

      subroutine print_coordinates ( iu,segment_meas )
      
      use mod_sdata_derived_type
      use mod_print_array
          
      implicit none
! ----------------------------------------------------------------	  
      integer,                       intent(in)  ::  iu
      type(segment_data),            intent(in)  ::  segment_meas
! ----------------------------------------------------------------	  
      integer  :: npixels
! ----------------------------------------------------------------	  
      npixels = segment_meas%npixels 
      write(iu,'(a10,4x)', advance='no') "Longitude:"
      call rprint_array_real(iu,segment_meas%pixels(:)%x,"(f14.4)", 1, npixels)
      write(iu,'(a10,4x)', advance='no') "Latitude :"
      call rprint_array_real(iu,segment_meas%pixels(:)%y,"(f14.4)", 1, npixels)
      
      return
      end subroutine print_coordinates 
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! OUTPUT print: sd2m structure print

      subroutine print_sd2m ( iu,RIN,segment_meas,GOUT )
      
      use mod_retr_settings_derived_type 
      use mod_retr_general_output_derived_type
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none
! ----------------------------------------------------------------	  
      integer,                       intent(in)  ::  iu
      type(retr_input_settings),     intent(in)  ::  RIN
      type(segment_data),            intent(in)  ::  segment_meas
      type(output_segment_general),  intent(in)  ::  GOUT
! ----------------------------------------------------------------	  
      integer  :: IDIM1
      logical  :: use_models
      integer,parameter  :: par_total  = 0
      integer,parameter  :: par_fine   = 1
      integer,parameter  :: par_coarse = 2      
! ----------------------------------------------------------------

      if(RIN%products%aerosol%sd2m_mph .or. RIN%products%aerosol%sd2m_ext) then
        ! Date and time for npixels
        write(iu,*)
        call print_date_time(iu,segment_meas)
        call print_coordinates(iu,segment_meas)
      endif

      if(RIN%products%aerosol%sd2m_mph) then
        !if(RIN%NSD .eq. 1)  &
        call print_sd2m_mph(iu,RIN,segment_meas,GOUT,par_total )      
        call print_sd2m_mph(iu,RIN,segment_meas,GOUT,par_fine  )      
        call print_sd2m_mph(iu,RIN,segment_meas,GOUT,par_coarse) 
      endif
      if(RIN%products%aerosol%sd2m_ext) then
        use_models = .false.
        do IDIM1=1,RIN%NDIM%n1
          if ( RIN%NDIM%par_type(IDIM1) .eq. par_type_SD_MD) then
            use_models = .true.
            exit
          endif
        enddo
        if(RIN%NSD .eq. 1 .or. use_models ) then
        call print_sd2m_ext(iu,RIN,segment_meas,GOUT,par_fine  )
        if ( error_present() ) return
        call print_sd2m_ext(iu,RIN,segment_meas,GOUT,par_coarse)
        if ( error_present() ) return
        endif
      endif
      
      return
      end subroutine print_sd2m 
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine print_sd2m_mph (iu,RIN,segment_meas,GOUT,index)

      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
            
      implicit none
! ----------------------------------------------------------------	  
      integer,                     intent(in)  ::  iu,index
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
! ----------------------------------------------------------------	  
      integer                 ::  i1,i2,ipix,npixels
      character(len=6)        ::  aaa      
! ----------------------------------------------------------------	 
      npixels = segment_meas%npixels

      select case(index)
      case(0) 
        aaa = 'total'
      case(1) 
        aaa = 'fine'
      case(2) 
        aaa = 'coarse'
      end select 
      
      if(RIN%NSD .eq. 1) then
        write(iu,'(a14)', advance='no') 'cv   '//aaa
        call rprint_array_real(iu,GOUT%aerosol%sd2m%mph%pixel(:)%cv(index),"(e14.5)", 1, npixels)
        write(iu,'(a14)', advance='no') 'rv   '//aaa
        call rprint_array_real(iu,GOUT%aerosol%sd2m%mph%pixel(:)%rm(index),"(e14.5)", 1, npixels)
        write(iu,'(a14)', advance='no') 'std  '//aaa
        call rprint_array_real(iu,GOUT%aerosol%sd2m%mph%pixel(:)%std(index),"(e14.5)", 1, npixels)
      endif ! RIN%NSD .eq. 1
        write(iu,'(a14)', advance='no') 'reff '//aaa
        call rprint_array_real(iu,GOUT%aerosol%sd2m%mph%pixel(:)%reff(index),"(e14.5)", 1, npixels)
      return
      end subroutine print_sd2m_mph 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      
      subroutine print_sd2m_ext (iu,RIN,segment_meas,GOUT,index)

      use mod_retr_general_output_derived_type
      use mod_retr_settings_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none
! ----------------------------------------------------------------	  
      integer,                     intent(in)  ::  iu,index
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
! ----------------------------------------------------------------	  
      integer           ::  iwl, isd, npixels
      character(len=6)  ::  aaa
! ----------------------------------------------------------------
      npixels = segment_meas%npixels
      select case(index)
      case(1) 
        aaa = 'fine'
      case(2) 
        aaa = 'coarse'
      case default
        write(tmp_message,'(a,i0,a)') 'index = ',index,'  - unknown'
        G_ERROR(trim(tmp_message))
      end select
      
      !do isd=1,RIN%NSD
      !write(iu,'(a,i0)') 'Wavelength (um),  aod_'//aaa//' for Particle component ',isd
      write(iu,'(a)') 'Wavelength (um),  aod_'//aaa
      do iwl=1,RIN%NW
        write(iu,'(f14.5)', advance='no') RIN%WAVE(iwl)
        call rprint_array_real(iu,GOUT%aerosol%sd2m%opt%pixel(:)%wl(iwl)%ext(index),"(e14.5)", 1, npixels)
      enddo ! iw
      !enddo ! isd

      return
      end subroutine print_sd2m_ext 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print detailed residual for final iteration

      subroutine print_final_iteration_residuals (iu, RIN, segment_meas, GOUT)

      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
            
      implicit none  
!----------------------------------------------------------------------------------------
      integer,                      intent(in)  ::  iu
      type(retr_input_settings),    intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),           intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
      integer  ::  i, ipix, npixels, INOISE
      character(LEN=20) :: KNC, INOISEC
      character(LEN=150) :: CFMT
!----------------------------------------------------------------------------------------
      npixels  = segment_meas%npixels
      INOISE = RIN%NOISE%INOISE
      write(INOISEC,*) INOISE

      write(iu,'(/,a)') 'Detailed residuals after final iteration'
      CFMT = '(8x,'//trim(adjustl(INOISEC))//'(4x,a))'
      write(iu,trim(CFMT)) ('noise          abs            rel', i=1,INOISE)

      if(RIN%IPFP%INVSING .ge. 2) then

        loop_pixel1: do ipix=1,npixels
            CFMT = '(10x,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),2(6x,a,i0))'
            write(iu,trim(CFMT)) &
            (i,':',GOUT%retrieval%res%pixel(ipix)%resa(i), &
            GOUT%retrieval%res%pixel(ipix)%resr(i)*100.0,' %', &
            i=1,INOISE),'pixel # ', ipix,  &
            'Residual after iteration # ',GOUT%retrieval%res%niter
        enddo loop_pixel1
        CFMT = '(/,f10.5,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),6x,a,i0,a/)'
        write(iu,trim(CFMT)) GOUT%retrieval%res%rest*100.0, &
        (i,':',GOUT%retrieval%res%resat(i), &
        GOUT%retrieval%res%resrt(i)*100.0,' %', &
        i=1,INOISE),'Residual after iteration # ', GOUT%retrieval%res%niter,'  for TOTAL SEGMENT'

      elseif(RIN%IPFP%INVSING .eq. 0 .or. RIN%IPFP%INVSING .eq. 1) then

        loop_pixel2: do ipix=1,npixels
            CFMT = '(f10.5,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5,a),2(6x,a,i0))'
            write(iu,trim(CFMT)) GOUT%retrieval%res%pixel(ipix)%res*100.0, &
            (i,':',GOUT%retrieval%res%pixel(ipix)%resa(i), &
            GOUT%retrieval%res%pixel(ipix)%resr(i)*100.0,' %', &
            i=1,INOISE),'pixel # ', ipix, &
            'Residual after iteration # ', GOUT%retrieval%res%pixel(ipix)%niter
        enddo loop_pixel2

      endif ! INVSING .ge. 2
      write(iu,*)

      return
      end subroutine print_final_iteration_residuals

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print vector of Retreived parameters for segment  	  

      subroutine print_retrieved_parameters (iu,RIN,segment_meas,GOUT)

      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
            
      implicit none  
!----------------------------------------------------------------------------------------
      integer,                      intent(in)  ::  iu
      type(retr_input_settings),    intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),           intent(in)  ::  segment_meas
      integer  ::  i,KNSING,npixels
!----------------------------------------------------------------------------------------
      npixels  = segment_meas%npixels
      KNSING   = RIN%KNSING

      !write(iu,*)
      write(iu,'(a)') 'Parameter #, Vector of retrieved parameters'
      do i=1,KNSING
        write(iu,'(i14)', advance='no') i
        call rprint_array_real (iu,GOUT%retrieval%par%pixel(:)%par(i), '(e14.5)', 1, npixels)
      enddo ! i
            
      return
      end subroutine print_retrieved_parameters 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Size Distribution   	  

      subroutine print_size_distribution (iu,RIN,segment_meas,GOUT)

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(retr_input_settings),    intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),           intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
! LOCAL :
      integer          ::  IDIM1,par_type
!----------------------------------------------------------------------------------------

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if ( par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end ) then
          select case ( par_type )
          case ( par_type_SD_TB )
            call print_size_distribution_TB (iu,RIN,segment_meas,GOUT)
          case ( par_type_SD_LB, par_type_SD_MD )
            call print_size_distribution_LB (iu,RIN,segment_meas,GOUT)
          case ( par_type_SD_LN )
            call print_size_distribution_LN (iu,RIN,segment_meas,GOUT)
            if ( error_present() ) return
          end select
          if ( RIN%IPRI_additional_info ) then
          if ( par_type .ne. par_type_SD_MD ) then
          call print_size_distribution_gout (iu,GOUT)
          endif
          endif
        exit
        endif ! par_type .gt. par_type_SD_beg .and.
      enddo ! IDIM1

      return
      end subroutine print_size_distribution

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Triangle Bin (TB) Size Distribution   	  

      subroutine print_size_distribution_TB (iu,RIN,segment_meas,GOUT)

      use mod_par_inv, only : KIDIM3, KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_meas_type
      use mod_sdata_derived_type

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(retr_input_settings),    intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),           intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,IDIM2,NDIM3
      real,dimension(KIDIM3,KIMAGE)        :: sd
      real,dimension(KIMAGE)               :: cv_aux
      integer                              :: i,ipix,npixels
      real                                 :: DLNR
      integer                              :: par_type
      logical                              :: icv
!----------------------------------------------------------------------------------------
      npixels  = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if( par_type .eq. par_type_SD_TB ) then
        do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
          DLNR = LOG(GOUT%retrieval%information%radius(2,IDIM2)) - &
                 LOG(GOUT%retrieval%information%radius(1,IDIM2))
          NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
          if ( RIN%indep_par) then
          if ( RIN%flag_plus) then
          NDIM3 = RIN%NDIM_plus%n3(IDIM2,IDIM1)
          endif
          endif
          write(iu,'(3(a,1x),i0)') 'Size Distribution dV/dlnr', &
          '(normalized by particle volume concentration) for',  &
                                      'Particle component',IDIM2
          if ( RIN%indep_par ) then
            select case ( RIN%flag_plus )
            case ( .false. )
              do ipix=1,npixels
              sd(1:NDIM3,ipix) = GOUT%retrieval%par%pixel(ipix)%sd(1:NDIM3,IDIM2)
              cv_aux(ipix) = sum(sd(1:NDIM3,IDIM2)) * DLNR
              sd(1:NDIM3,ipix) = sd(i,ipix) / cv_aux(ipix)
              enddo ! ipix
            case ( .true. )
              do ipix=1,npixels
              sd(1:NDIM3,ipix) = GOUT%retrieval%par%pixel(ipix)%sd(1:NDIM3,IDIM2)
              sd(1:NDIM3,ipix) = sd(1:NDIM3,ipix) / DLNR
              enddo ! ipix
            end select
          else
              do ipix=1,npixels
              sd(1:NDIM3,ipix) = GOUT%retrieval%par%pixel(ipix)%sd(1:NDIM3,IDIM2)
              cv_aux(ipix) = sum(sd(1:NDIM3,ipix)) * DLNR
              sd(1:NDIM3,ipix) = sd(1:NDIM3,ipix) / cv_aux(ipix)
              enddo ! ipix
          endif
          do i=1,NDIM3
            write(iu,'(e14.5)',advance='no') GOUT%retrieval%information%radius(i,IDIM2)
            call rprint_array_real (iu,sd(i,:), '(e14.5)', 1, npixels)
          enddo ! i
        enddo  ! IDIM2
        exit
        endif
      enddo ! IDIM1

      return
      end subroutine print_size_distribution_TB 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print precalculated Lognormal Bin (LB) Size Distribution   	  

      subroutine print_size_distribution_LB (iu,RIN,segment_meas,GOUT)

      use mod_par_inv, only : KIDIM2,KIDIM3,KIMAGE
      use mod_par_DLS_bin, only : NRR
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(retr_input_settings),    intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),           intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1, IDIM2, NDIM3, IDIM1_SD
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: SD, CV
      integer                              :: i, ipix, npixels
      integer                              :: par_type, par_type_SD
      real,dimension(KIDIM2,KIMAGE)        :: temp1
      real,dimension(KIMAGE)  :: temp
      logical :: icv
!----------------------------------------------------------------------------------------
! if RIN%indep_par=.false. then NDIM_plus=NDIM
! if RIN%flag_plus=.false. then NDIM_plus=NDIM

      npixels  = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .eq. par_type_SD_LB .or. par_type .eq. par_type_SD_MD ) then
        IDIM1_SD = IDIM1
        par_type_SD = par_type
        call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1_SD,npixels,SD )
        exit
        endif
      enddo ! IDIM1

      if ( RIN%indep_par ) then
          if ( RIN%flag_plus) then
            do IDIM2=1,RIN%NDIM_plus%n2(IDIM1_SD) ! aerosol component loop
            NDIM3 = RIN%NDIM_plus%n3(IDIM2,IDIM1_SD)
            do ipix=1,npixels
            call set_all_bins ( NDIM3, SD(1:NDIM3,IDIM2,ipix) )
            temp1(IDIM2,ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
            enddo ! ipix
            enddo ! IDIM2
          else
            do IDIM2=1,RIN%NDIM%n2(IDIM1_SD)      ! aerosol component loop
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1_SD)
            do ipix=1,npixels
            temp1(IDIM2,ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
            enddo ! ipix
            enddo ! IDIM2
          endif
      else
            do IDIM2=1,RIN%NDIM%n2(IDIM1_SD)      ! aerosol component loop
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1_SD)
            do ipix=1,npixels
            temp1(IDIM2,ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
            enddo ! ipix
            enddo ! IDIM2
      endif

      if ( par_type_SD .eq. par_type_SD_LB ) then
        do IDIM2=1,RIN%NDIM_plus%n2(IDIM1_SD)    ! aerosol component loop
        NDIM3 = RIN%NDIM_plus%n3(IDIM2,IDIM1_SD)
          write(iu,'(3a,i0)') 'Radius (um), Size Distribution dV/dlnr ', &
          '(normalized by particle volume concentration) for ', &
          'Particle component ',IDIM2
          do i=1,GOUT%retrieval%information%ngrid(IDIM2)
            write(iu,'(e14.5)',advance='no') GOUT%retrieval%information%radius(i,IDIM2)
            do ipix=1,npixels
              if (temp1(IDIM2,ipix ) .ne. 0.0) then
              temp(ipix) = GOUT%retrieval%par%pixel(ipix)%sd(i,IDIM2)/temp1(IDIM2,ipix)
              endif
            enddo
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          enddo ! i
          write(iu,'(2a,i0)') 'rv (um), Volume concentration (um^3/um^2 or um^3/um^3) ', &
          ' of precomputed lognormal bins for Particle component ',IDIM2
          do i=1,NDIM3
            write(iu,'(e14.5)',advance='no') RIN%radius1(i,IDIM2)
            call rprint_array_real (iu,SD(i,IDIM2,:), '(e14.5)', 1, npixels)
          enddo ! i
        enddo ! IDIM2
      elseif ( par_type_SD .eq. par_type_SD_MD ) then
        do IDIM2=1,RIN%NDIM_plus%n2(IDIM1_SD)    ! aerosol component loop
        NDIM3 = RIN%NDIM_plus%n3(IDIM2,IDIM1_SD)
          write(iu,'(2a,i0)') '      #, Model fraction in total concentration for', &
          ' Particle component ',IDIM2
          do i=1,NDIM3
            write(iu,'(e14.5)',advance='no') RIN%radius1(i,IDIM2)
            call rprint_array_real (iu,SD(i,IDIM2,:), '(e14.5)', 1, npixels)
          enddo ! i
        enddo ! IDIM2
      endif

      return
      end subroutine print_size_distribution_LB 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Lognormal Size Distribution   	  

      subroutine print_size_distribution_LN (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(retr_input_settings),    intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),           intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              ::  IDIM1,IDIM2,NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) ::  SD, CV
      integer                              ::  i,npixels
      real,dimension(KIMAGE)               ::  temp
      integer                              ::  par_type
      logical                              ::  icv
!----------------------------------------------------------------------------------------
      icv = .false.
      temp(:) = 0.0
      npixels = segment_meas%npixels
      
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
          ! if Aerosol Concentration retreived 
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,CV ) 
          icv = .true.
        exit
        endif ! RIN%NDIM%par_type(i) .gt. par_type_SD_beg .and. 
      enddo ! IDIM1

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .eq. par_type_SD_LN) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,SD )
          do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
              NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
              if ( .not. RIN%use_tmodel) then
              write(iu,'(3a,i0)') 'Radius (um), Size Distribution dV/dlnr ', &
              '(normalized by particle volume concentration) for ', &
              'Particle component ',IDIM2
              else
              write(iu,'(3a,3a)') 'Radius (um), Size Distribution dV/dlnr ', &
              '(normalized by particle volume concentration) for ', &
              'tracer ',RIN%TMSET%trcs(1:3,IDIM2)
              endif
              do i=1,GOUT%retrieval%information%ngrid(IDIM2)
              write(iu,'(f14.5)',advance='no') GOUT%retrieval%information%radius(i,IDIM2)
              temp(1:npixels) = GOUT%retrieval%par%pixel(1:npixels)%sd(i,IDIM2) &
                                / CV(1,IDIM2,1:npixels)
              call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
              enddo ! i                                
              if ( .not. RIN%use_tmodel) then
              write(iu,'(a,i0)') 'Parameters of lognormal SD for Particle component ',IDIM2
              else
              write(iu,'(2a,3a)') 'Parameters of lognormal SD for ', &
              'tracer ',RIN%TMSET%trcs(1:3,IDIM2)
              endif
              do i=1,NDIM3
              if(i .eq. 1) write(iu,'(a14)',advance='no') 'rv (um):'
              if(i .eq. 2) write(iu,'(a14)',advance='no') 'ln(sigma):'
              call rprint_array_real (iu,SD(i,IDIM2,:), '(e14.5)', 1, npixels)
              enddo ! i
          enddo  ! IDIM2
          !IDIM2=0    ! aerosol component loop
          !    write(iu,'(3a,i0)') 'Radius (um), Total Size Distribution dV/dlnr (absolute values)'
          !    do i=1,GOUT%retrieval%information%ngrid(IDIM2)
          !    write(iu,'(f14.5)',advance='no') GOUT%retrieval%information%radius(i,IDIM2)
          !    temp(1:npixels) = GOUT%retrieval%par%pixel(1:npixels)%sd(i,IDIM2)
          !    call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          !    enddo ! i
        exit
        endif ! par_type .gt. par_type_SD_beg .and.
      enddo ! IDIM1

      return
      end subroutine print_size_distribution_LN

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Aerosol concentrations             	  

      subroutine print_aerosol_concentration (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(retr_input_settings),    intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),           intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,IDIM2,NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: CV,SD
      integer                              :: i,ipix,npixels,par_type,par_type_SD
      real,dimension(KIMAGE)               :: temp
      logical                              :: icv = .false.
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels
      
      icv = .false.
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_Cv_beg .and. par_type .lt. par_type_Cv_end) then
        call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,CV )
          icv = .true.
        exit
        endif ! par_type .gt. par_type_Cv_beg .and. 
      enddo ! IDIM1

      !write(iu,*) 

! volume of particles in air column per unit area um^3/um^2 or volume of particles per unit air volume um^3/um^3
      write(iu,'(a)')  'Aerosol volume concentration (um^3/um^2 or um^3/um^3)'

      do IDIM1=1,RIN%NDIM%n1
          par_type = RIN%NDIM%par_type(IDIM1)              
          if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then
            call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,SD )
            par_type_SD = par_type
          exit
          endif ! par_type .gt. par_type_Cv_beg .and. 
      enddo ! IDIM1

      do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)    
        write(iu,'(i14)',advance='no') IDIM2
        if ( icv ) then
          temp(1:npixels) = CV(1,IDIM2,1:npixels)
        else
          do ipix=1,npixels
            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!            WRITE(*,*) 'print_GOUT L1029, concentration = ',ipix,temp(ipix)
!            WRITE(*,*) 'dx=',LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) ! LOG = ln
!            WRITE(*,*) 'SD=',SD(1:NDIM3,IDIM2,ipix)
          enddo ! ipix
          if ( par_type_SD .eq. par_type_SD_TB ) then
            temp(1:npixels) = temp(1:npixels) * ( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
          endif
        endif ! icv
        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
      enddo ! IDIM2        

!!************add by lei on 20160914 to output aerosol chemical elements mass concentration *******
!
!      write(iu,'(a)')  'Aerosol chemical elements mass concentration (mg/m^2)'
!
!      do IDIM1=1,RIN%NDIM%n1
!        par_type = RIN%NDIM%par_type(IDIM1)        
!        if(par_type .gt. par_type_RERI_beg .and. par_type .lt. par_type_RERI_end) then
!
!          if(par_type .eq. par_type_CXRI_nmix) then
!
!      do IDIM2=1,RIN%NDIM%n2(IDIM1)       ! aerosol component loop for chemical elements mass concentration
!        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)    
!          if (IDIM2 .eq. 1) then
!        write(iu,'(a)') 'Mass concentration of BC:'
!        write(iu,'(i14)',advance='no') IDIM2
!          do ipix=1,npixels
!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fsoot(IDIM2)*1.8*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!            if(par_type_SD .eq. par_type_SD_TB)  &
!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!          enddo ! ipi
!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!
!        write(iu,'(a)') 'Mass concentration of BrC:'
!        write(iu,'(i14)',advance='no') IDIM2
!          do ipix=1,npixels
!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fslbl(IDIM2)*1.2*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!            if(par_type_SD .eq. par_type_SD_TB)  &
!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!          enddo ! ipi
!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!
!        write(iu,'(a)') 'Mass concentration of Quartz:'
!        write(iu,'(i14)',advance='no') IDIM2
!          do ipix=1,npixels
!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%finslbl(IDIM2)*2.5*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!            if(par_type_SD .eq. par_type_SD_TB)  &
!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!          enddo ! ipi
!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!
!        write(iu,'(a)') 'Mass concentration of Water:'
!        write(iu,'(i14)',advance='no') IDIM2
!          do ipix=1,npixels
!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fwtr(IDIM2)*1*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!            if(par_type_SD .eq. par_type_SD_TB)  &
!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!          enddo ! ipi
!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!
!         else
!!        write(iu,'(a)') 'Mass concentration of Carbonaceous:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fcarbon(IDIM2)*1.2*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!        write(iu,'(a)') 'Mass concentration of BC:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fsoot(IDIM2)*1.8*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!        write(iu,'(a)') 'Mass concentration of BrC:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fslbl(IDIM2)*1.2*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!
!        write(iu,'(a)') 'Mass concentration of Quartz:'
!        write(iu,'(i14)',advance='no') IDIM2
!          do ipix=1,npixels
!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%finslbl(IDIM2)*2.5*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!            if(par_type_SD .eq. par_type_SD_TB)  &
!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!          enddo ! ipi
!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!
!        write(iu,'(a)') 'Mass concentration of Iron:'
!        write(iu,'(i14)',advance='no') IDIM2
!          do ipix=1,npixels
!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%firon(IDIM2)*4.8*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!            if(par_type_SD .eq. par_type_SD_TB)  &
!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!          enddo ! ipi
!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!
!        write(iu,'(a)') 'Mass concentration of Water:'
!        write(iu,'(i14)',advance='no') IDIM2
!          do ipix=1,npixels
!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fwtr(IDIM2)*1*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!            if(par_type_SD .eq. par_type_SD_TB)  &
!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!          enddo ! ipi
!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!
!           endif
!
!       enddo ! IDIM2
!
!!   elseif(par_type .eq. par_type_CXRI_chem) then
!!
!!      do IDIM2=1,RIN%NDIM%n2(IDIM1)       ! aerosol component loop for chemical elements mass concentration
!!        NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)    
!!     if (IDIM2 .eq. 1) then
!!        write(iu,'(a)') 'Mass concentration of BC:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fsoot(IDIM2)*1.8*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!        write(iu,'(a)') 'Mass concentration of BrC:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fbrc(IDIM2)*1.2*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!        write(iu,'(a)') 'Mass concentration of Quartz:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%finslbl(IDIM2)*2.5*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!        write(iu,'(a)') 'Mass concentration of Soluble:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fslbl(IDIM2)*1.73*1000*sum(SD(1:NDIM3,IDIM2,ipix))  !! NH4NO3
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!
!!        write(iu,'(a)') 'Mass concentration of Water:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fwtr(IDIM2)*1*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!         else
!!        write(iu,'(a)') 'Mass concentration of Carbonaceous:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fcarbon(IDIM2)*1.2*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!        write(iu,'(a)') 'Mass concentration of Quartz:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%finslbl(IDIM2)*2.5*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!        write(iu,'(a)') 'Mass concentration of Iron:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%firon(IDIM2)*4.8*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!        write(iu,'(a)') 'Mass concentration of Soluble:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fslbl(IDIM2)*2.17*1000*sum(SD(1:NDIM3,IDIM2,ipix))   !!sea salt
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!
!!        write(iu,'(a)') 'Mass concentration of Water:'
!!        write(iu,'(i14)',advance='no') IDIM2
!!          do ipix=1,npixels
!!            temp(ipix) = sum(SD(1:NDIM3,IDIM2,ipix))
!!            temp(ipix) = GOUT%aerosol%chem%pixel(ipix)%fwtr(IDIM2)*1*1000*sum(SD(1:NDIM3,IDIM2,ipix))
!!            if(par_type_SD .eq. par_type_SD_TB)  &
!!            temp(ipix) = temp(ipix)*( LOG(RIN%radius1(2,IDIM2))-LOG(RIN%radius1(1,IDIM2)) )
!!          enddo ! ipi
!!        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
!!
!!           endif
!!
!!        enddo ! IDIM2
!!
!      endif
!
!     endif
!
!   enddo  ! IDIM1

!!************add by lei on 20160914 to output aerosol chemical elements mass concentration *******

      return
      end subroutine print_aerosol_concentration 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Gas Concentration

      subroutine print_gas_concentration (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                   intent(in)  ::  iu
      type(retr_input_settings), intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),        intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: C
      real,dimension(KIMAGE)               :: temp
      integer                              :: i,igas,npixels,par_type
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      igas = 0
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if ( par_type .gt. par_type_gases_concentration_beg ) then
        if ( par_type .lt. par_type_gases_concentration_end ) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,C )
          igas = igas + 1
          if ( igas .eq. 1 ) write(iu,'(a)') 'Gas concentration'
          write(iu,'(i14)',advance='no') igas
          temp(1:npixels) = C(1,1,1:npixels)
          call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
        endif
        endif
      enddo ! IDIM1

      return
      end subroutine print_gas_concentration

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Sphericity or Shape Distribution               	  

      subroutine print_shape_distribution (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                   intent(in)  ::  iu
      type(retr_input_settings), intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),        intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,IDIM2,NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: sph
      real,dimension(KIMAGE)               :: temp
      integer                              :: i,ipix,npixels,par_type
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if ( par_type .eq. par_type_SHD_fsph ) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,sph )
          if(RIN%NDIM%n2(IDIM1) .eq. 1 .and. RIN%NDIM%n3(1,IDIM1) .eq. 2) & 
          sph(2,:,:) = sph(1,:,:)     	  
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            if ( .not. RIN%use_tmodel) then
            write(iu,'(a,i0)') '% of spherical particles for Particle component ',IDIM2
            else
            write(iu,'(2a,3a)') '% of spherical particles for ', &
            'tracer ',RIN%TMSET%trcs(1:3,IDIM2)
            endif
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do i=1,NDIM3
              write(iu,'(i14)',advance='no') i
              temp(1:npixels) = sph(i,IDIM2,1:npixels)*100.
              call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
            enddo ! i
          enddo ! IDIM2
        exit
        elseif ( par_type .eq. par_type_SHD_distr ) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,sph )
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            if ( RIN%NDIM%n2(IDIM1) .eq. 1 ) then
            write(iu,'(a)') 'Ratio, Shape distribution'
            else
            if ( .not. RIN%use_tmodel) then
            write(iu,'(a,i0)') 'Ratio, Shape distribution for Particle component ',IDIM2
            else
            write(iu,'(2a,3a)') 'Ratio, Shape distribution for ', &
            'tracer ',RIN%TMSET%trcs(1:3,IDIM2)
            endif
            endif
            if ( .not. RIN%indep_par ) then
              NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            else
              NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1) + 1
              do ipix=1, npixels
              call set_all_bins ( NDIM3, sph(1:NDIM3,IDIM2,ipix) )
              enddo
            endif
            do i=1,NDIM3
              write(iu,'(f14.4)',advance='no') RIN%RATIO1(i,IDIM2)
              temp(1:npixels) = sph(i,IDIM2,1:npixels)
              call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
            enddo ! i
          enddo ! IDIM2
        exit
        endif ! par_type .eq.

      enddo ! IDIM1

      return
      end subroutine print_shape_distribution 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Relative Humidity

      subroutine print_relative_humidity (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type

      implicit none
!----------------------------------------------------------------------------------------
! IN :
      integer,                   intent(in)  ::  iu
      type(retr_input_settings), intent(in)  ::  RIN
      type(output_segment_general), intent(in)  ::  GOUT
      type(segment_data),        intent(in)  ::  segment_meas
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1, IDIM2, NDIM2, NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: rh
      real,dimension(KIMAGE)               :: temp
      integer                              :: i, npixels, par_type
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        NDIM2 = RIN%NDIM%n2(IDIM1)
        par_type = RIN%NDIM%par_type(IDIM1)
        if ( par_type .eq. par_type_RH ) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,rh )
          do IDIM2=1,NDIM2
            write(iu,'(a)') 'Relative Humidity (0 -- 1)'
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do i=1,NDIM3
              write(iu,'(14x)',advance='no')
              temp(1:npixels) = rh(i,IDIM2,1:npixels)
              call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
            enddo ! i
          enddo ! IDIM2
        exit
        endif ! par_type .eq.

      enddo ! IDIM1

      return
      end subroutine print_relative_humidity


! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Aerosol Vertical Profile (if lidar measurements are present) or 
! mean height of the profole                          	  

      subroutine print_aerosol_profile (iu,RIN,segment_meas,GOUT)

      use mod_par_inv, only : KIDIM2, KIDIM3, KIMAGE, KVERTM !,max_NH
      use mod_par_OS,  only : KSD, NBVM, HMAX_atm
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      use mod_sdata, only : get_HVP_lidar
      use MOD_RT_SOS
      use mod_sdata_meas_type      

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,IDIM2,NDIM3,npixels
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: h01,std
      real,dimension(KIDIM3)               :: h01_aux
      integer                              :: NHVP_meas,NHVP_retr
      real,dimension(KIMAGE)               :: HGR_km
      real,dimension(KVERTM,KIMAGE)        :: HVP_meas
      real,dimension(KVERTM,KIMAGE)        :: HVP_retr_km
      integer                              :: i,ipix,par_type,iw
      real                                 :: xnorm
      integer                              :: NH
      real,dimension(KVERTM)               :: H_km
      character(len=20)                    :: distr_type
      real, dimension(KVERTM)              :: prof_temp
      real,dimension(KIDIM2,KIMAGE)        :: h01_HGR
      real                                 :: sza
      logical                              :: pixels_altitudes_different= .false.
!----------------------------------------------------------------------------------------
      npixels = segment_meas%npixels
!      WRITE(*,*) 'IN print_aerosol_profile'
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_AVP_beg  .and. par_type .lt. par_type_AVP_end) then 
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,h01 ) 	  
          if(par_type .eq. par_type_AVP_prof) then
            call get_HVP_lidar (  segment_meas,       & ! IN
                                  NHVP_meas, HVP_meas & ! INOUT
                               )
            distr_type = 'lut'
            do ipix=1,npixels
              HGR_km(ipix) = segment_meas%pixels(ipix)%MASL*0.001
              do IDIM2=1,RIN%NDIM%n2(IDIM1)
                NHVP_retr = RIN%NDIM%n3(IDIM2,IDIM1)
          ! getting sounding zenith angle for lidar measurements
                do iw=1,RIN%NW
                  if(ANY(segment_meas%pixels(ipix)%meas(iw)%meas_type(:) .GE. meas_type_lid_beg .AND. &
                         segment_meas%pixels(ipix)%meas(iw)%meas_type(:) .LE. meas_type_lid_end )) then
                     sza=segment_meas%pixels(ipix)%meas(iw)%sza
                     exit
                  endif
                enddo !iw
                call get_AVP_altitudes_retr ( HGR_km(ipix),                      &
                                              NHVP_meas, HVP_meas(:,ipix)*0.001, &
                                              sza,                               &
                                              NHVP_retr, HVP_retr_km(:,ipix) )
                if ( RIN%indep_par ) then
                call set_vd_all_retr_altitudes (NHVP_retr,                     &
                                                HVP_retr_km(1:NHVP_retr,ipix), &
                                                h01(1:NHVP_retr,IDIM2,ipix) )
                endif
                ! Projecting retrieved altitudes from the inclined
!                HVP_retr_km(1:NHVP_meas,ipix)=HVP_retr_km(1:NHVP_meas,ipix)*cos(sza*0.01745329251)
                ! Altitudes for aerosol concentration profile normalization
                call grid_altitudes_LUT_with_gas ( HGR_km(ipix), HMAX_atm*0.001,               & ! IN
                                          NHVP_retr, HVP_retr_km(1:NHVP_retr,ipix),  &
                                          NH, H_km(1:NHVP_retr+2)               & ! OUT
                                        )
                ! Aerosol concentration profile normalization
                call discrvd_single_atm_component ( distr_type, 0.0, 0.0,                 &
                                                    NHVP_retr, HVP_retr_km(1:NHVP_retr,ipix),  &
                                                    h01(1:NHVP_retr,IDIM2,ipix),          &
                                                    NH, H_km(1:NH),                       &
                                                    xnorm, prof_temp(1:NH)                & ! OUT
                                                  )
                h01(1:NHVP_retr,IDIM2,ipix) = h01(1:NHVP_retr,IDIM2,ipix)/xnorm
                h01(1:NHVP_retr,IDIM2,ipix) = h01(1:NHVP_retr,IDIM2,ipix)*0.001 ! AL 0.001 to make aerosol vertical profile in 1/m
                if(HVP_retr_km(NHVP_retr,ipix) .ne. HGR_km(ipix)) h01_HGR(IDIM2,ipix) = prof_temp(NH)/xnorm*0.001 ! vertical profile at ground level (assumed)
              enddo ! IDIM2
              if(ANY(abs(HVP_retr_km(1:NHVP_retr,ipix)-HVP_retr_km(1:NHVP_retr,1)) .GT. 1e-5) .OR. &
                (abs(HGR_km(ipix)-HGR_km(1)) .GT. 1e-5)) then
                 pixels_altitudes_different=.true.
              endif
            enddo ! ipix
          endif ! par_type .eq. par_type_AVP_prof 

          do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)    
            if ( RIN%indep_par ) then
            NDIM3 = RIN%NDIM_plus%n3(IDIM2,IDIM1)
            endif
            if(par_type .eq. par_type_AVP_par_height) then
              write(iu,'(a)') 'Aerosol profile mean height (m)'
              do i=1,NDIM3
                write(iu,'(i14)',advance='no')  IDIM2
                call rprint_array_real (iu,h01(i,IDIM2,:), '(e14.5)', 1, npixels)
              enddo ! i
            elseif(par_type .eq. par_type_AVP_prof) then
              write(iu,'(a,i0)') 'Aerosol vertical profile [1/m] for Particle component ',IDIM2
              do i=1,NDIM3
                if (pixels_altitudes_different) then
                    write(iu,'(i5)',advance='no') i
                else
                    write(iu,'(i5,f10.2)',advance='no') i,HVP_retr_km(i,1)*1000.
                endif !pixels_altitudes_different
                call rprint_array_real (iu,h01(i,IDIM2,:), '(e14.5)', 1, npixels)
              enddo ! I
              if(HVP_retr_km(NHVP_retr,1) .ne. HGR_km(1)) then
                ! print vertical profile at ground level (assumed)
                if (pixels_altitudes_different) then
                    write(iu,'(a,i3)',advance='no') ' *',NHVP_retr+1
                else
                    write(iu,'(a,i3,f10.2)',advance='no') ' *',NHVP_retr+1,HGR_km(1)*1000.
                endif !pixels_altitudes_different
                call rprint_array_real (iu,h01_HGR(IDIM2,:), '(e14.5)', 1, npixels)
               endif ! HGR_km
            if (pixels_altitudes_different) then
                write(iu,'(a,i0)') 'Aerosol vertical profile altitudes [m] for Particle component ',IDIM2
                do i=1,NDIM3
                  write(iu,'(i5)',advance='no') i
                  call rprint_array_real (iu,HVP_retr_km(i,:), '(e14.5)', 1, npixels)
                enddo ! I

                if(HVP_retr_km(NHVP_retr,1)-HGR_km(1).GT.1e-5) then
                   ! print vertical profile at ground level (assumed)
                     write(iu,'(a,i3)',advance='no') ' *',NHVP_retr+1
                     call rprint_array_real (iu,HGR_km, '(e14.5)', 1, npixels)
!                     do ipix=1,npixels
!                        write(iu,'(f10.2)',advance='no') HGR_km(ipix)*1000.
!                     enddo !ipix
                endif ! HGR_km
            endif ! pixels altitudes different
            endif ! par_type .eq. par_type_AVP_par_height
          enddo ! NDIM2
        elseif(par_type .eq. par_type_AVP_par_std) then
          call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,std ) 	  
          write(iu,'(a)') 'Aerosol profile standard deviation (m)'
          do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)    
            do i=1,NDIM3
              write(iu,'(i14)',advance='no') IDIM2
              call rprint_array_real (iu,std(i,IDIM2,:), '(e14.5)', 1, npixels)
            enddo ! i
          enddo ! NDIM2
        endif ! par_type .gt. par_type_AVP_beg  .and. 
      enddo ! IDIM1
      
      return
      end subroutine print_aerosol_profile 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Lidar Calibration coefficient               	  

      subroutine print_lidar_calibration_coeffitient (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,IDIM2,NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: CL
      real,dimension(KIMAGE)               :: temp
      integer                              :: i,npixels,par_type
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_ADD_beg  .and. par_type .lt. par_type_ADD_end) then    
          if(par_type .eq. par_type_CL) then
            call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,CL ) 	  
            do IDIM2=1,RIN%NDIM%n2(IDIM1)          
              write(iu,'(a)') 'Lidar calibration coefficient'
              NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
              do i=1,NDIM3
                write(iu,'(i14)',advance='no') i
                call rprint_array_real (iu,CL(i,IDIM2,:), '(e14.5)', 1, npixels)
              enddo ! i
            enddo ! IDIM2
          exit
          endif 
        endif ! par_type .gt. par_type_ADD_beg  .and. 
      enddo ! IDIM1

      return
      end subroutine print_lidar_calibration_coeffitient 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Sutface Parameters               	  

      subroutine print_surface_parameters (iu,RIN,segment_meas,GOUT)

      use mod_par_inv, only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      use mod_functional_retrieval, only : retr_method_fullset
      use mod_stop_report

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,IDIM2,IDIM3,NDIM3,ns3
      real,dimension(KIMAGE)               :: SURF
      real,dimension(KIMAGE)               :: temp
      integer                              :: npixels,par_type
      integer                              :: ipix, ibeg_IWW, iend_IWW
      real,dimension(KIDIM3)               :: BRF
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF1_land_end) then
          write(iu,'(a)') 'Wavelength (um), BRDF parameters'
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            write(iu,'(i4)') IDIM2
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do IDIM3=1,RIN%NW
              SURF(1:npixels) = GOUT%retrieval%par%pixel(1:npixels)%BRF(IDIM3,IDIM2)
              if(par_type .eq. par_type_SURF1_land_RPV_BRDF .and. IDIM2 .eq. 3) then
              SURF(1:npixels) = SURF(1:npixels) - 1.0
              endif
              if (RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
              else
                if (any(RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1) .eq. IDIM3)) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
                else
                write(iu,'(5x,a1,f8.5)',advance='no') '*',RIN%wave(IDIM3)
                endif
              endif
              call rprint_array_real (iu,SURF(:), '(e14.5)', 1, npixels)
            enddo ! IDIM3
          enddo ! IDIM2
        elseif(par_type .gt. par_type_SURF2_land_beg .and. par_type .lt. par_type_SURF2_land_end) then
          write(iu,'(a)') 'Wavelength (um), BPDF parameters'
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            write(iu,'(i4)') IDIM2
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do IDIM3=1,RIN%NW
              SURF(1:npixels) = GOUT%retrieval%par%pixel(1:npixels)%BRP(IDIM3,IDIM2)
              if (RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
              else
                if (any(RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1) .eq. IDIM3)) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
                else
                write(iu,'(5x,a1,f8.5)',advance='no') '*',RIN%wave(IDIM3)
                endif
              endif
              call rprint_array_real (iu,SURF(:), '(e14.5)', 1, npixels)
            enddo ! IDIM3
          enddo ! IDIM2
        elseif(par_type .gt. par_type_SURF_water_beg .and. par_type .lt. par_type_SURF_water_end) then
          write(iu,'(a)') 'Wavelength (um), Water surface parameters'
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            write(iu,'(i4)') IDIM2
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do IDIM3=1,RIN%NW
              SURF(1:npixels) = GOUT%retrieval%par%pixel(1:npixels)%BRM(IDIM3,IDIM2)
              if (RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
              else
                if (any(RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1) .eq. IDIM3)) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
                else
                write(iu,'(5x,a1,f8.5)',advance='no') '*',RIN%wave(IDIM3)
                endif
              endif
              call rprint_array_real (iu,SURF(:), '(e14.5)', 1, npixels)
            enddo ! IDIM3
          enddo ! IDIM2
        endif ! par_type .gt. par_type_SURF1_land_beg .and.
      enddo ! IDIM1

      return
      end subroutine print_surface_parameters 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Real part of Refractiv Index

      subroutine print_refractive_index_real (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_functional_retrieval, only : retr_method_fullset
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: IDIM1,IDIM2,IDIM3,NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: RERI
      real,dimension(KIMAGE) :: temp
      integer :: iw,npixels,par_type
!----------------------------------------------------------------------------------------
      !temp(:) = 0.0
      npixels = segment_meas%npixels
      
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .gt. par_type_RERI_beg .and. par_type .lt. par_type_RERI_end) then
          do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
            write(iu,'(a,i0)') &
            'Wavelength (um), REAL Ref. Index for Particle component ',IDIM2
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do IDIM3=1,RIN%NW
              if (RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
              else
                if (any(RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1) .eq. IDIM3)) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
                else
                write(iu,'(5x,a1,f8.5)',advance='no') '*',RIN%wave(IDIM3)
                endif
              endif
              temp(1:npixels) = GOUT%aerosol%rind%pixel(1:npixels)%wl(IDIM3)%mreal(IDIM2)
              call rprint_array_real (iu,temp(:), '(f14.5)', 1, npixels)
            enddo ! IDIM3
          enddo ! IDIM2
        exit
        endif

        if((par_type .eq. par_type_CXRI_chem .or. par_type .eq. par_type_CXRI_nmix) &
            .or. &
           (par_type .eq. par_type_SD_MD)) then
          !if(par_type .ne. par_type_CXRI_chem .and. par_type .ne. par_type_CXRI_nmix) then
            !call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,RERI )
            !write(iu,'(a)') 'Wavelength (um), REAL Ref. Index :'
            !do IDIM2=1,RIN%NDIM%n2(IDIM1)
            !  write(iu,*) IDIM2
            !  NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            !  do IDIM3=1,NDIM3
            !    write(iu,'(e14.5)',advance='no') RIN%wave(IDIM3)
            !    call rprint_array_real (iu,RERI(IDIM3,IDIM2,:), '(e14.5)', 1, npixels)
            !  enddo ! IDIM3
            !enddo ! IDIM2
          !exit
          !endif ! par_type .ne. par_type_CXRI_chem
          do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
            write(iu,'(a,i0)') &
            'Wavelength (um), REAL Ref. Index for Particle component ',IDIM2
            !NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            NDIM3 = RIN%NW
            do IDIM3=1,NDIM3
              write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
              temp(1:npixels) = GOUT%aerosol%rind%pixel(1:npixels)%wl(IDIM3)%mreal(IDIM2)
              call rprint_array_real (iu,temp(:), '(f14.5)', 1, npixels)
            enddo ! IDIM3
          enddo ! IDIM2
        exit
        endif ! par_type .eq. par_type_CXRI_chem .and.

      enddo ! IDIM1

      return
      end subroutine print_refractive_index_real 


! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Imaginary part of Refractiv Index               	  

      subroutine print_refractive_index_imaginary (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIDIM2,KIDIM3,KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_functional_retrieval, only : retr_method_fullset
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: IDIM1,IDIM2,IDIM3,NDIM3
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: IMRI
      real,dimension(KIMAGE) :: temp
      integer :: iw,npixels,par_type
!----------------------------------------------------------------------------------------
      !temp(:) = 0.0
      npixels = segment_meas%npixels
      
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_IMRI_beg .and. par_type .lt. par_type_IMRI_end) then
          do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
            write(iu,'(a,i0)') &
           'Wavelength (um), IMAG Ref. Index for Particle component ',IDIM2
            NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            do IDIM3=1,RIN%NW
              if (RIN%FRETR%method(IDIM2,IDIM1) .eq. retr_method_fullset) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
              else
                if (any(RIN%FRETR%ipar(1:NDIM3,IDIM2,IDIM1) .eq. IDIM3)) then
                write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
                else
                write(iu,'(5x,a1,f8.5)',advance='no') '*',RIN%wave(IDIM3)
                endif
              endif
              temp(1:npixels) = GOUT%aerosol%rind%pixel(1:npixels)%wl(IDIM3)%mimag(IDIM2)
              call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
            enddo ! IDIM3
          enddo ! IDIM2
        endif
        if((par_type .eq. par_type_CXRI_chem .or. par_type .eq. par_type_CXRI_nmix) &
            .or. &
           (par_type .eq. par_type_SD_MD)) then

          !call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,IMRI )
          !write(iu,'(a)') 'Wavelength (um), IMAG Ref. Index :'
          !do IDIM2=1,RIN%NDIM%n2(IDIM1)
            !write(iu,*) IDIM2
            !NDIM3 = RIN%NDIM%n3(IDIM2,IDIM1)
            !do IDIM3=1,NDIM3
              !write(iu,'(e14.5)',advance='no') RIN%wave(IDIM3)
              !call rprint_array_real (iu,IMRI(IDIM3,IDIM2,:), '(e14.5)', 1, npixels)
            !enddo ! IDIM3
          !enddo ! IDIM2
          !exit
          do IDIM2=1,RIN%NDIM%n2(IDIM1)    ! aerosol component loop
           write(iu,'(a,i0)') &
           'Wavelength (um), IMAG Ref. Index for Particle component ',IDIM2
            NDIM3 = RIN%NW
            do IDIM3=1,NDIM3
              write(iu,'(f14.5)',advance='no') RIN%wave(IDIM3)
              temp(1:npixels) = GOUT%aerosol%rind%pixel(1:npixels)%wl(IDIM3)%mimag(IDIM2)
              call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
            enddo ! IDIM3
          enddo ! IDIM2
        exit
        endif ! par_type .gt. par_type_RERI_beg .and.
      enddo ! IDIM1

      return
      end subroutine print_refractive_index_imaginary 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print chemistry              	  

      subroutine print_chemistry (iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer :: IDIM1, IDIM2, IDIM3, NDIM2, NDIM3
      real,dimension(KIMAGE) :: temp, aa
      integer :: iw, npixels, par_type
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels
      
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .eq. par_type_CXRI_nmix) then
          NDIM2 = RIN%NDIM%n2(IDIM1)
          do IDIM2=1,NDIM2    ! aerosol component loop
            write(iu,'(a,i0)') &
            'Volume fractions for chemical species for Particle component ',IDIM2
            NDIM3 = GOUT%retrieval%information%nchem(IDIM2)
            do IDIM3=1,NDIM3
            write(iu,'(i14)',advance='no') IDIM3
            temp(1:npixels) = GOUT%aerosol%chem%pixel(1:npixels)%vfract(IDIM3,IDIM2)
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
            enddo ! IDIM3
          enddo ! IDIM2
        exit
        elseif(par_type .eq. par_type_CXRI_chem) then
          NDIM2 = RIN%NDIM%n2(IDIM1)
          do IDIM2=1,NDIM2    ! aerosol component loop
            write(iu,'(a,i0)') &
            'Volume fractions for chemical species for Particle component ',IDIM2
            NDIM3 = GOUT%retrieval%information%nchem(IDIM2)
            do IDIM3=1,NDIM3
            write(iu,'(i14)',advance='no') IDIM3
            temp(1:npixels) = GOUT%aerosol%chem%pixel(1:npixels)%vfract(IDIM3,IDIM2)
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
            enddo ! IDIM3
            write(iu,'(a)') &
            'Volume fraction of water'
            write(iu,'(14x)',advance='no')
            temp(1:npixels) = GOUT%aerosol%chem%pixel(1:npixels)%fwtr(IDIM2)
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
            write(iu,'(a)') &
            'Volume fraction of soluble chemical species'
            write(iu,'(14x)',advance='no')
            temp(1:npixels) = GOUT%aerosol%chem%pixel(1:npixels)%fslbl(IDIM2)
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          enddo ! IDIM2
        exit
        endif
      enddo ! IDIM1

      return
      end subroutine print_chemistry 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Optical Thickness              	  
      
      subroutine print_optical_thickness(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      real,dimension(KIMAGE) :: temp
      integer                :: iw,isd,npixels
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels
                      
      write(iu,'(a)') 'Wavelength (um), AOD_Total (unitless or 1/um)'
      do iw=1,RIN%nw
        write(iu,'(f14.5)',advance='no') RIN%wave(iw)
        temp(1:npixels) = GOUT%aerosol%opt%pixel(1:npixels)%wl(iw)%extt
        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
      enddo ! iw

      if(RIN%NSD .gt. 1) then
        do isd=1,RIN%NSD    ! aerosol component loop
          write(iu,'(a,i0,a)') &
          'Wavelength (um), AOD_Particle_mode_',isd,' (unitless or 1/um)'
          do iw=1,RIN%nw
            write(iu,'(f14.5)',advance='no') RIN%wave(iw)
            temp(1:npixels) = GOUT%aerosol%opt%pixel(1:npixels)%wl(iw)%ext(isd)
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          enddo ! iw
        enddo ! isd
      endif ! RIN%NSD .gt. 1
  
      return
      end subroutine print_optical_thickness 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Gas absorption
      
      subroutine print_gas_absorption(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      real,dimension(KIMAGE) :: temp
      integer                :: i, iw, ipix, npixels, ngas
!----------------------------------------------------------------------------------------

      temp(:) = 0.0
      npixels = segment_meas%npixels
      ngas = GOUT%retrieval%information%ngas

      write(iu,'(a)') 'Wavelength (um), GABS_Total (unitless)'
      do iw=1,RIN%nw
        write(iu,'(f14.5)',advance='no') RIN%wave(iw)
        do ipix=1,npixels
        temp(ipix) = GOUT%gases%pixel(ipix)%wl(iw)%abs(0)
        enddo
        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
      enddo ! iw
      if ( ngas .gt. 1 ) then
        do i=1,ngas
          write(iu,'(a,i0,1x,a)') 'Wavelength (um), GABS gas # ',i,'(unitless)'
          do iw=1,RIN%nw
            write(iu,'(f14.5)',advance='no') RIN%wave(iw)
            do ipix=1,npixels
            temp(ipix) = GOUT%gases%pixel(ipix)%wl(iw)%abs(i)
            enddo
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          enddo ! iw
        enddo
      endif

      return
      end subroutine print_gas_absorption

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Single Scattering Abledo              	  
      
      subroutine print_single_scattering_albedo(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      real,dimension(KIMAGE) :: temp
      integer                :: iw,isd,npixels
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels
                      
      write(iu,'(a)') 'Wavelength (um), SSA_Total'
      do iw=1,RIN%nw
        write(iu,'(f14.5)',advance='no') RIN%wave(iw)
        temp(1:npixels) = GOUT%aerosol%opt%pixel(1:npixels)%wl(iw)%ssat
        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
      enddo ! iw

      if(RIN%NSD .gt. 1) then
        do isd=1,RIN%NSD    ! aerosol component loop
          write(iu,'(a,i0)') 'Wavelength (um), SSA_Particle_mode_',isd
          do iw=1,RIN%nw
            write(iu,'(f14.5)',advance='no') RIN%wave(iw)
            temp(1:npixels) = GOUT%aerosol%opt%pixel(1:npixels)%wl(iw)%ssa(isd)
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          enddo ! iw
        enddo ! isd
      endif ! RIN%NSD .gt. 1

      return
      end subroutine print_single_scattering_albedo 
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Absorption
      
      subroutine print_absorption(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      real,dimension(KIMAGE) :: temp
      integer                :: iw,isd,npixels
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels
                      
      write(iu,'(a)') 'Wavelength (um), AAOD_Total (unitless or 1/um)'
      do iw=1,RIN%nw
        write(iu,'(f14.5)',advance='no') RIN%wave(iw)
        temp(1:npixels) = GOUT%aerosol%opt%pixel(1:npixels)%wl(iw)%aextt
        call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
      enddo ! iw

      if(RIN%NSD .gt. 1) then
        do isd=1,RIN%NSD    ! aerosol component loop
          write(iu,'(a,i0,a)') &
          'Wavelength (um), AAOD_Particle_mode_',isd,' (unitless or 1/um)'
          do iw=1,RIN%nw
            write(iu,'(f14.5)',advance='no') RIN%wave(iw)
            temp(1:npixels) = GOUT%aerosol%opt%pixel(1:npixels)%wl(iw)%aext(isd)
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          enddo ! iw
        enddo ! isd
      endif ! RIN%NSD .gt. 1
  
      return
      end subroutine print_absorption

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Angstrom exponent              	  
      
      subroutine print_angstrom_exponent(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,par_type
      real,dimension(KIMAGE)               :: temp
      integer                              :: npixels
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF_water_end) then      
          if(any(GOUT%aerosol%opt%pixel(1:npixels)%Aexp .ne. 0.0)) then
            write(iu,'(a,f6.3,a,f6.3,a)') 'Angstrom exp (',RIN%WAVE(RIN%Aexp_iwl(1)),  &
                                                  ' /',RIN%WAVE(RIN%Aexp_iwl(2)),' )'
            write(iu,'(14x)',advance='no') 
            temp(1:npixels) = GOUT%aerosol%opt%pixel(1:npixels)%Aexp
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)                
          endif
        exit
        endif        
      enddo ! IDIM1 

      return
      end subroutine print_angstrom_exponent 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Lidar Ratio              	  
      
      subroutine print_lidar_ratio(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                ::  IDIM1,par_type
      real,dimension(KIMAGE) ::  temp
      integer                ::  iw,isd,npixels
!---------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

!AL      do IDIM1=1,RIN%NDIM%n1
!AL        par_type = RIN%NDIM%par_type(IDIM1)
!AL        if(par_type .eq. par_type_AVP_prof) then      
          write(iu,'(a)') 'Wavelength (um),  Lidar Ratio (Total)'
          do iw=1,RIN%nw
            write(iu,'(f14.5)',advance='no') RIN%wave(iw)
            temp(1:npixels) = GOUT%aerosol%lidar%pixel(1:npixels)%wl(iw)%lrt
            call rprint_array_real (iu,temp(:), '(f14.5)', 1, npixels)
          enddo ! iw
          if(RIN%NSD .gt. 1) then
            do isd=1,RIN%NSD    ! aerosol component loop
              write(iu,'(a,i0,a)') &
              'Wavelength (um), Lidar Ratio (Particle component ',isd,')'
              do iw=1,RIN%nw
                write(iu,'(f14.5)',advance='no') RIN%wave(iw)
                temp(1:npixels) = GOUT%aerosol%lidar%pixel(1:npixels)%wl(iw)%lr(isd)
                call rprint_array_real (iu,temp(:), '(f14.5)', 1, npixels)
              enddo ! iw
            enddo ! isd
          endif ! RIN%NSD .gt. 1
!AL        exit
!AL        endif ! par_type .eq. par_type_CL
!AL      enddo ! IDIM1
  
      return
      end subroutine print_lidar_ratio
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print land percent              	  
      
      subroutine print_surface_land_percent(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,par_type
      real,dimension(KIMAGE)               :: temp
      integer                              :: npixels
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_SURF1_land_beg .and. &
           par_type .lt. par_type_SURF_water_end) then
          write(iu,'(/,a)') '*** SURFACE ***'
          call print_date_time(iu,segment_meas)
          call print_coordinates(iu,segment_meas)
          write(iu,*)
          write(iu,'(a)',advance='no') 'Land percent:'
          temp(1:npixels) = segment_meas%pixels(1:npixels)%land_percent
          call rprint_array_real (iu,temp(:), '(f14.5)', 1, npixels)          
        exit
        endif        
      enddo ! IDIM1 

      return
      end subroutine print_surface_land_percent 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print PM
      
      subroutine print_aerosol_PM(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      real,dimension(KIMAGE)               :: temp
      integer                              :: npixels, ipm
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels
! AL to do: get Pm from GOUT structure not calculate once again!!!!
      do ipm=1,RIN%nPM_diam
        if(RIN%PM_diam(ipm) .gt. 0.0) then
          write(iu,'(a,f4.1)',advance='no') 'PM ',RIN%PM_diam(ipm)
          write(iu,*)
!          call get_PM(RIN%PM_diam(ipm),RIN,GOUT,segment_meas,temp(1:npixels))
          temp(1:npixels) = GOUT%aerosol%pm%pixel(1:npixels)%PM(ipm)
          call rprint_array_real (iu,temp(:), '(f14.5)', 1, npixels)
        endif ! PM_diam(ipm) .gt. 0.0)
      enddo !ipm
!      write(iu,'(a)',advance='no') 'PM10: '
!      write(iu,*) ''
!      call get_PM(10.0,RIN,GOUT,segment_meas,temp(1:npixels))
!      call rprint_array_real (iu,temp(:), '(f14.5)', 1, npixels)
      
      return
      end subroutine print_aerosol_PM


! Print AEROSOL TYPES
      
      subroutine print_aerosol_types(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer,dimension(KIMAGE)            :: temp ! will store indicies
      integer                              :: npixels, ipix
      character(len=12),dimension(0:8)     :: type_name ! strings containing names of aerosol types
!----------------------------------------------------------------------------------------
! initialize dictionaty
      type_name(0)='Complex_mix '
      type_name(1)='Background  '
      type_name(2)='Maritime    '
      type_name(3)='Urban:Poltd '
      type_name(4)='Mixed       '
      type_name(5)='Urban:Clean '
      type_name(6)='Smoke:smold '
      type_name(7)='Smoke:flame '
      type_name(8)='Miner._dust '

      temp(:) = 0
      npixels = segment_meas%npixels
      write(iu,'(a)',advance='no') 'Aerosol type: '
!      write(iu,*)
      do ipix=1,npixels
         write(iu,'(a14)', advance='no') type_name(GOUT%aerosol%types%pixel(ipix)%index)
      enddo
      write(iu,*)
!      write(iu,'(a)',advance='no') 'Aerosol type index:'
!      write(iu,*) ''
!      temp(1:npixels) = GOUT%aerosol%types%pixel(1:npixels)%index
!      call iprint_array_int (iu, temp(:), '(I2)', 1, npixels)
      
      return
      end subroutine print_aerosol_types
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Surface Abledo              	  
      
      subroutine print_surface_dhr(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,par_type
      real,dimension(KIMAGE)               :: temp
      integer                              :: iw,npixels
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF_water_end) then                
          write(iu,'(a)') 'Wavelength (um), Directional Hemispherical Reflectance (dhr)'
          do iw=1,RIN%nw
            write(iu,'(f14.5)',advance='no') RIN%wave(iw)
            temp(1:npixels) = GOUT%surface%pixel(1:npixels)%wl(iw)%dhr
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          enddo ! IW
        exit
        endif        
      enddo ! IDIM1 
      
      return
      end subroutine print_surface_dhr

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Sutface Isotropic BiHemispherical Reflectance
      subroutine print_surface_bhr_iso(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type

      implicit none
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,par_type
      real,dimension(KIMAGE)               :: temp
      integer                              :: iw,npixels
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      !do IDIM1=1,RIN%NDIM%n1
        !par_type = RIN%NDIM%par_type(IDIM1)
        !if(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF_water_end) then
          write(iu,'(a)') 'Wavelength (um), Isotropic BiHemispherical Reflectance (bhr_iso)'
          do iw=1,RIN%nw
            write(iu,'(f14.5)',advance='no') RIN%wave(iw)
            temp(1:npixels) = GOUT%surface%pixel(1:npixels)%wl(iw)%bhr_iso
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
          enddo ! IW
        !exit
        !endif
      !enddo ! IDIM1

      return
      end subroutine print_surface_bhr_iso

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Surface NDVI              	  
      
      subroutine print_surface_ndvi(iu,RIN,segment_meas,GOUT)

      use mod_par_inv,     only : KIMAGE
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
      
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: IDIM1,par_type
      real,dimension(KIMAGE)               :: temp
      integer                              :: npixels
!----------------------------------------------------------------------------------------
      temp(:) = 0.0
      npixels = segment_meas%npixels

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF_water_end) then      
          if(any(GOUT%surface%pixel(1:npixels)%ndvi .ne. 0.0)) then
            write(iu,'(a,f6.3,a,f6.3,a)') 'Surface NDVI (',RIN%WAVE(RIN%ndvi_iwl(1)),  &
                                                  ' /',RIN%WAVE(RIN%ndvi_iwl(2)),' )'

            write(iu,'(14x)',advance='no') 
            temp(1:npixels) = GOUT%surface%pixel(1:npixels)%ndvi
            call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)                
          endif
        exit
        endif        
      enddo ! IDIM1 
      
      return
      end subroutine print_surface_ndvi 

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine print_fitting ( iu_main_output, RIN, segment_meas, segment_fit )
	  
      use mod_par_inv, only : KW, KNBVM, NBVM
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_inversion_utils
      use mod_retr_settings_derived_type
      use mod_sdata, only : get_pixel_wl, get_vert_prof_h
            										      
      implicit none  

!---------------------------------------------------------------------------
! IN :
      integer, intent(in) :: iu_main_output
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data), intent(inout) :: segment_meas
      type(segment_data), intent(in) :: segment_fit

!---------------------------------------------------------------------------
! LOCAL :
      integer ::  nw, npixels
      real, dimension(KW) :: wave
      integer :: ipix, iw, ip, iv
      real, dimension(KNBVM) :: temp_meas, temp_fit, &
                                I_meas, I_fit, Q_meas, Q_fit, &
                                U_meas, U_fit, P11_meas, P11_fit     
      character(len=13) :: meas_type_char
      real :: sza
      real, dimension(NBVM) :: vis,fis                     
      real, dimension(KNBVM) :: arg
      integer :: nmeas_type, meas_type, nvalid_meas 
      logical :: status_funct 
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
! iPOBS = 
!           1    I, Q, U                      
!           2    I, Q/I, U/I                  
!           3    I, P    or sqrt(Q*Q+U*U)
!           4    I, P/I  or sqrt(Q*Q+U*U)/I
!           5    P11_rel_ang
!
!---------------------------------------------------------------------------

      npixels = segment_meas%npixels
      write(iu_main_output,*)
      write(iu_main_output,'(a)') '*** FITTING from sdata structure ***'
loop_pixel: do ipix=1,npixels
      call get_pixel_wl ( segment_meas%pixels(ipix), nw, wave )      
loop_wl: do iw=1,nw
      write(iu_main_output,'(a)') '------------------------------------------------------------------------'
      write(iu_main_output,'(a,i4,a,i3,f10.3,a)') 'pixel # ',ipix,'   wavelength # ',iw,wave(iw),' (um)'
      write(iu_main_output,'(a)') '------------------------------------------------------------------------'
      nmeas_type = segment_meas%pixels(ipix)%meas(iw)%NIP
loop_meas_type: do ip=1,nmeas_type 
      meas_type_char(:) = ' '
      meas_type   = segment_meas%pixels(ipix)%meas(iw)%meas_type(ip)
      if(RIN%iPOBS .ge. 3 .and. meas_type .eq. meas_type_U) &
      exit loop_meas_type
      nvalid_meas = segment_meas%pixels(ipix)%meas(iw)%NBVM(ip)
      if(meas_type .ge. meas_type_LS .and. meas_type .le. meas_type_VBS) then
        call get_vert_prof_h ( iw, ip, segment_meas%pixels(ipix), &
                                    segment_meas%pixels(ipix)%meas(IW)%NBVM(ip), &
                                    arg(:) )
      else
        do iv=1,nvalid_meas
          sza = segment_meas%pixels(ipix)%meas(iw)%sza
          vis(iv) = segment_meas%pixels(ipix)%meas(iw)%thetav(iv,ip)
          fis(iv) = segment_meas%pixels(ipix)%meas(iw)%phi(iv,ip)
          status_funct = geom2scat_angl ( sza,  vis(iv), fis(iv), arg(iv) )
        enddo
      endif
      select case(meas_type)
! Phase matrix
      case(meas_type_tod)
        meas_type_char = 'tod'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%tod(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%tod(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_htod)
        meas_type_char = 'htod'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%htod(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%htod(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_aod)
        meas_type_char = 'aod'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%aod(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%aod(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_aaod)
        meas_type_char = 'aaod'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%aaod(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%aaod(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p11)
        meas_type_char = 'f11'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p11_rel_ang)
        meas_type_char = 'f11_rel'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11_rel_ang(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11_rel_ang(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p11_intd)
        meas_type_char = 'f11_intd'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11_intd(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11_intd(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p11_intd_cut_off_1)
        meas_type_char = 'f11_intd1'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11_intd_cut_off_1(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11_intd_cut_off_1(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p11_intd_cut_off_2)
        meas_type_char = 'f11_intd2'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11_intd_cut_off_2(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11_intd_cut_off_2(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p11_intd_cut_off_3)
        meas_type_char = 'f11_intd3'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11_intd_cut_off_3(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11_intd_cut_off_3(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p11_intd_cut_off_4)
        meas_type_char = 'f11_intd4'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11_intd_cut_off_4(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11_intd_cut_off_4(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p12)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p12(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p12(1:nvalid_meas)
        if(RIN%iPOBS .eq. 1 ) then
          meas_type_char = 'f12'
        elseif(RIN%iPOBS .eq. 5) then
          meas_type_char = '-f12/f11'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = '-f12/f11'
          P11_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          P11_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = - temp_meas(1:nvalid_meas) / P11_meas(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = - temp_fit(1:nvalid_meas) / P11_fit(1:nvalid_meas)
        endif
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p12_rel)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p12_rel(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p12_rel(1:nvalid_meas)
        meas_type_char = '-f12/f11'
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p22)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p22(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p22(1:nvalid_meas)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f22'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = 'f22/f11'
          P11_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          P11_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = temp_meas(1:nvalid_meas) / P11_meas(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = temp_fit(1:nvalid_meas) / P11_fit(1:nvalid_meas)
        elseif(RIN%iPOBS .eq. 5 ) then
          meas_type_char = 'f22/f11'
        endif
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p33)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p33(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p33(1:nvalid_meas)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f33'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = 'f33/f11'
          P11_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          P11_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = temp_meas(1:nvalid_meas) / P11_meas(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = temp_fit(1:nvalid_meas) / P11_fit(1:nvalid_meas)
        elseif(RIN%iPOBS .eq. 5 ) then
          meas_type_char = 'f33/f11'
        endif
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p34)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p34(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p34(1:nvalid_meas)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f34'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = 'f34/f11'
          P11_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          P11_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = temp_meas(1:nvalid_meas) / P11_meas(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = temp_fit(1:nvalid_meas) / P11_fit(1:nvalid_meas)
        elseif(RIN%iPOBS .eq. 5 ) then
          meas_type_char = 'f34/f11'
        endif
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_p44)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p44(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p44(1:nvalid_meas)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f44'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = 'f44/f11'
          P11_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          P11_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%p11(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = temp_meas(1:nvalid_meas) / P11_meas(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = temp_fit(1:nvalid_meas) / P11_fit(1:nvalid_meas)
        elseif(RIN%iPOBS .eq. 5 ) then
          meas_type_char = 'f44/f11'
        endif
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
! Lidar
      case(meas_type_LS)  
        meas_type_char = 'LS'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%LS(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%LS(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_DP)
        meas_type_char = 'DP'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%DP(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%DP(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_DPAR)
        meas_type_char = 'DPAR'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%DPAR(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%DPAR(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_DPER)
        meas_type_char = 'DP'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%DPER(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%DPER(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_RL)
        meas_type_char = 'RL'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%RL(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%RL(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )

      case(meas_type_VBS)
        meas_type_char = 'VBS'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%VBS(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%VBS(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas,          &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )

      case(meas_type_VEXT)
        meas_type_char = 'VExt'
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%VEXT(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%VEXT(1:nvalid_meas)
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )


! Stokes vector
      case(meas_type_I)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
        meas_type_char = 'I'
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_I_rel_sum)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%I_rel_sum(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%I_rel_sum(1:nvalid_meas)
          meas_type_char = 'I/sum'
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_Q)
        select case(RIN%iPOBS)
        case(1)
          meas_type_char = 'Q'
          temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%Q(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%Q(1:nvalid_meas)
        case(2)
          meas_type_char = 'Q/I'
          I_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
          I_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%Q(1:nvalid_meas) / I_meas(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%Q(1:nvalid_meas) / I_fit(1:nvalid_meas)
        case(3)
          meas_type_char = 'P'
          Q_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%Q(1:nvalid_meas)
          Q_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%Q(1:nvalid_meas)
          U_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%U(1:nvalid_meas)
          U_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%U(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = sqrt( Q_meas(1:nvalid_meas)*Q_meas(1:nvalid_meas) + &
                                           U_meas(1:nvalid_meas)*U_meas(1:nvalid_meas) )         
          temp_fit(1:nvalid_meas) = sqrt( Q_fit(1:nvalid_meas)*Q_fit(1:nvalid_meas) + &
                                          U_fit(1:nvalid_meas)*U_fit(1:nvalid_meas) )         
        case(4)
          meas_type_char = 'P/I'
          I_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
          I_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
          Q_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%Q(1:nvalid_meas)
          Q_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%Q(1:nvalid_meas)
          U_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%U(1:nvalid_meas)
          U_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%U(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = sqrt( Q_meas(1:nvalid_meas)*Q_meas(1:nvalid_meas) + &
                                           U_meas(1:nvalid_meas)*U_meas(1:nvalid_meas) ) / I_meas(1:nvalid_meas)         
          temp_fit(1:nvalid_meas) = sqrt( Q_fit(1:nvalid_meas)*Q_fit(1:nvalid_meas) + &
                                          U_fit(1:nvalid_meas)*U_fit(1:nvalid_meas) ) / I_fit(1:nvalid_meas)         
        end select
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_U)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%U(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%U(1:nvalid_meas)
        select case(RIN%iPOBS)
        case(1)
          meas_type_char = 'U'
        case(2)
          meas_type_char = 'U/I'
          I_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
          I_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = temp_meas(1:nvalid_meas) / I_meas(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = temp_fit(1:nvalid_meas) / I_fit(1:nvalid_meas)
        end select
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_P)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%P(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%P(1:nvalid_meas)
        select case(RIN%iPOBS)
        case(3)
          meas_type_char = 'P'
        case(5)
! iPOBS=5 In case when number of angeles or angles for I differe from ones for P
! linear polarization has been divided by I at the time of setting 
! modeled measurements in subroutine set_pixel_Stokes_vec_fit
          meas_type_char = 'P/I'
        case(4)
          meas_type_char = 'P/I'
          I_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
          I_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%I(1:nvalid_meas)
          temp_meas(1:nvalid_meas) = temp_meas(1:nvalid_meas) / I_meas(1:nvalid_meas)
          temp_fit(1:nvalid_meas) = temp_fit(1:nvalid_meas) / I_fit(1:nvalid_meas)
        end select
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      case(meas_type_P_rel)
        temp_meas(1:nvalid_meas) = segment_meas%pixels(ipix)%meas(iw)%P_rel(1:nvalid_meas)
        temp_fit(1:nvalid_meas) = segment_fit%pixels(ipix)%meas(iw)%P_rel(1:nvalid_meas)
        meas_type_char = 'P_rel'
        call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                              sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                              temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
      end select
enddo loop_meas_type
enddo loop_wl
enddo loop_pixel

      end subroutine print_fitting
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine print_fitting_FS ( iu_main_output, RIN, segment_meas, &
                                    segment_vec_meas, segment_vec_fit )
	  
      use mod_par_inv, only : KW, KNBVM, NBVM
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_inversion_utils
      use mod_retr_settings_derived_type
      use mod_sdata, only : get_pixel_wl, get_vert_prof_h
            										      
      implicit none  

!---------------------------------------------------------------------------
! IN :
      integer, intent(in) :: iu_main_output
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data), intent(inout) :: segment_meas
      type(pixel_vector),dimension(KIMAGE), intent(in) :: segment_vec_meas
      type(pixel_vector),dimension(KIMAGE), intent(in) :: segment_vec_fit


!---------------------------------------------------------------------------
! LOCAL :
      integer ::  nw, npixels
      real, dimension(KW) :: wave
      integer :: ipix, iw, ip, iv, IWB, JJS
      real, dimension(KNBVM) :: temp_meas, temp_fit
      character(len=8) :: meas_type_char
      real :: sza
      real, dimension(NBVM) :: vis,fis                     
      real, dimension(KNBVM) :: arg
      integer :: nmeas_type, meas_type, nvalid_meas 
      logical :: status_funct 
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
! iPOBS = 1    I, Q, U
!         2    I, Q/I, U/I
!         3    I, P    or sqrt(Q*Q+U*U)
!         4    I, P/I  or sqrt(Q*Q+U*U)/I
!         5    I, P/I  P/I provided as measurements in input
!
! iIOBS = 1    I
!       = 2    I/sum(I(:))
!---------------------------------------------------------------------------
      npixels = segment_meas%npixels
      write(iu_main_output,*)
      write(iu_main_output,'(a)') '*** FITTING from meas vector ***'
loop_pixel: do ipix=1,npixels
      call get_pixel_wl ( segment_meas%pixels(ipix), nw, wave )      
loop_wl: do iw=1,nw
      write(iu_main_output,'(a)') '--------------------------------------------------------------------------'
      write(iu_main_output,'(a,i4,a,i3,f10.3,a)') 'pixel # ',ipix,'   wavelength # ',iw,wave(iw),' (um)'
      write(iu_main_output,'(a)') '--------------------------------------------------------------------------'
      nmeas_type = segment_meas%pixels(ipix)%meas(iw)%NIP
      IWB=SUM(segment_vec_meas(ipix)%nFS(1:iw))-segment_vec_meas(ipix)%nFS(iw)
      ! IWD - number of elements in FS before current (iw) wavelength
      JJS=IWB
loop_meas_type: do ip=1,nmeas_type
      meas_type_char(:) = ' '
      meas_type   = segment_meas%pixels(ipix)%meas(iw)%meas_type(ip)
      if(RIN%iPOBS .ge. 3 .and. meas_type .eq. meas_type_U) &
      exit loop_meas_type
      nvalid_meas = segment_meas%pixels(ipix)%meas(iw)%NBVM(ip)
      do iv=1,nvalid_meas
        JJS = JJS+1
        temp_meas(iv) = exp(segment_vec_meas(ipix)%FS(JJS))
        temp_fit (iv) = exp(segment_vec_fit (ipix)%FS(JJS))
        if( (meas_type .ge. meas_type_Q   .and. meas_type .le. meas_type_P) .or. &
             (meas_type .eq. meas_type_P_rel) .or.    &
             (meas_type .eq. meas_type_p12) .or.        &
             (meas_type .eq. meas_type_p12_rel) .or.   &
            (meas_type .ge. meas_type_p33 .and. meas_type .le. meas_type_p44) &
          ) then
          temp_meas(iv) = temp_meas(iv) - RIN%SHIFT
          temp_fit (iv) = temp_fit (iv) - RIN%SHIFT
        endif
      enddo  ! iv
      if(meas_type .ge. meas_type_LS .and. meas_type .le. meas_type_VBS) then
        call get_vert_prof_h ( iw, ip, segment_meas%pixels(ipix), &
                                    segment_meas%pixels(ipix)%meas(iw)%NBVM(ip), &
                                    arg(:) )
      else
        do iv=1,nvalid_meas
          sza = segment_meas%pixels(ipix)%meas(iw)%sza
          vis(iv) = segment_meas%pixels(ipix)%meas(iw)%thetav(iv,ip)
          fis(iv) = segment_meas%pixels(ipix)%meas(iw)%phi(iv,ip)
          status_funct = geom2scat_angl ( sza,  vis(iv), fis(iv), arg(iv) )
        enddo
      endif
      select case(meas_type)
! Scattering matrix
      case(meas_type_tod)
        if(meas_type .eq. meas_type_tod) meas_type_char = 'tod'
      case(meas_type_htod)
        if(meas_type .eq. meas_type_htod) meas_type_char = 'htod'
      case(meas_type_aod)
        if(meas_type .eq. meas_type_aod) meas_type_char = 'aod'
      case(meas_type_aaod)
        if(meas_type .eq. meas_type_aaod) meas_type_char = 'aaod'
      case(meas_type_p11)
        meas_type_char = 'f11'
      case(meas_type_p11_rel_ang)
        meas_type_char = 'f11_rel_ang'
      case(meas_type_p11_intd)
        meas_type_char = 'f11_intd'
      case(meas_type_p11_intd_cut_off_1)
        meas_type_char = 'f11_intd_cut_off_1'
      case(meas_type_p11_intd_cut_off_2)
        meas_type_char = 'f11_intd_cut_off_2'
      case(meas_type_p11_intd_cut_off_3)
        meas_type_char = 'f11_intd_cut_off_3'
      case(meas_type_p11_intd_cut_off_4)
        meas_type_char = 'f11_intd_cut_off_4'
      case(meas_type_p12)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f12'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = '-f12/f11'
        endif
      case(meas_type_p12_rel)
          meas_type_char = '-f12/f11'
      case(meas_type_p22)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f22'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = 'f22/f11'
        elseif(RIN%iPOBS .eq. 5) then
          meas_type_char = 'f22/f11'
        endif
      case(meas_type_p33)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f33'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = 'f33/f11'
        elseif(RIN%iPOBS .eq. 5) then
          meas_type_char = 'f33/f11'
        endif
      case(meas_type_p34)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f34'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = 'f34/f11'
        elseif(RIN%iPOBS .eq. 5) then
          meas_type_char = 'f34/f11'
        endif
      case(meas_type_p44)
        if(RIN%iPOBS .eq. 1) then
          meas_type_char = 'f44'
        elseif(RIN%iPOBS .eq. 2) then
          meas_type_char = 'f44/f11'
        elseif(RIN%iPOBS .eq. 5) then
          meas_type_char = 'f44/f11'
        endif
! Lidar
      case(meas_type_LS)  
        meas_type_char = 'LS'
      case(meas_type_RL)
        meas_type_char = 'RL'
      case(meas_type_DPAR)
        meas_type_char = 'DPAR'
      case(meas_type_DPER)
        meas_type_char = 'DPER'
      case(meas_type_DP)
        meas_type_char = 'DP'
      case(meas_type_VBS)
        meas_type_char = 'VBS'
      case(meas_type_VEXT)
        meas_type_char = 'VEXT'
! Stokes vector
      case(meas_type_I)
        meas_type_char = 'I'
      case(meas_type_I_rel_sum)
        meas_type_char = 'I/sum'
      case(meas_type_Q)
        select case(RIN%iPOBS)
        case(1)
          meas_type_char = 'Q'
        case(2)
          meas_type_char = 'Q/I'
        case(3)
          meas_type_char = 'P'
        case(4)
          meas_type_char = 'P/I'
        end select
      case(meas_type_U)
        select case(RIN%iPOBS)
        case(1)
          meas_type_char = 'U'
        case(2)
          meas_type_char = 'U/I'
        end select
      case(meas_type_P)
        select case(RIN%iPOBS)
        case(3)
          meas_type_char = 'P'
        case(5)
! iPOBS=5 In case when number of angeles or angles for I differe from ones for P
! linear polarization has been divided by I at the time of setting 
! modeled measurements in subroutine set_pixel_Stokes_vec_fit
          meas_type_char = 'P/I'
        case(4)
          meas_type_char = 'P/I'
        end select
      case(meas_type_P_rel)
          meas_type_char = 'P_rel'
      end select
      call print_meas_fit ( iu_main_output, meas_type, meas_type_char, nvalid_meas, &
                            sza, vis(1:nvalid_meas), fis(1:nvalid_meas), arg(1:nvalid_meas), &
                            temp_meas(1:nvalid_meas), temp_fit(1:nvalid_meas) )
enddo loop_meas_type
enddo loop_wl
enddo loop_pixel

      end subroutine print_fitting_FS

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine print_meas_fit ( iu_main_output, meas_type, meas_type_char, & 
                                  nvalid_meas, sza, vis, fis, arg, temp_meas, temp_fit )	        										      
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      
      implicit none  
!---------------------------------------------------------------------------
! IN :
      integer, intent(in) :: iu_main_output, nvalid_meas, meas_type
!      character(len=8), intent(in) :: meas_type_char
      character(*), intent(in) :: meas_type_char
      real, intent(in) :: sza
      real, dimension(nvalid_meas), intent(in) :: vis, fis, arg
      real, dimension(nvalid_meas), intent(in) :: temp_meas, temp_fit
!---------------------------------------------------------------------------
! LOCAL :
      integer ::  iv
!---------------------------------------------------------------------------
      if(meas_type .gt. meas_type_tau_beg .and. meas_type .lt. meas_type_tau_end) then
        write(iu_main_output,'(2(7x,a))') &
        'meas_'//trim(meas_type_char),' fit_'//trim(meas_type_char)
        do iv=1,nvalid_meas
          write(iu_main_output,'(2e15.5)') temp_meas(iv),temp_fit(iv)
        enddo ! IV                        
      elseif(meas_type .gt. meas_type_lid_beg .and. meas_type .lt. meas_type_lid_end) then
        write(iu_main_output,'(3x,a,3x,a,7x,a,8x,a)') '#','Range_[m]', &
        'meas_'//trim(meas_type_char),' fit_'//trim(meas_type_char)
        do iv=1,nvalid_meas
          write(iu_main_output,'(i4,f11.2,2e15.5)') iv,arg(iv),temp_meas(iv),temp_fit(iv)
        enddo ! IV
      elseif(meas_type .gt. meas_type_phm_beg .and. meas_type .lt. meas_type_phm_end) then
        write(iu_main_output,'(3x,a,2x,a,2a15)') '#','sca_ang', &
        'meas_'//trim(meas_type_char),' fit_'//trim(meas_type_char)
        do iv=1,nvalid_meas
          write(iu_main_output,'(i4,f9.2,2e15.5)') iv,arg(iv),temp_meas(iv),temp_fit(iv)
        enddo ! IV                  
      elseif(meas_type .gt. meas_type_integrated_beg .and. meas_type .lt. meas_type_integrated_end) then
        write(iu_main_output,'(3x,a,2(2x,a),2x,a,2a15)') '#','ang_min','ang_max', &
        'meas_'//trim(meas_type_char),' fit_'//trim(meas_type_char)
        do iv=1,nvalid_meas
          write(iu_main_output,'(i4,2f9.2,2e15.5)') &
          iv,vis(iv),fis(iv),temp_meas(iv),temp_fit(iv)
        enddo ! IV
      elseif(meas_type .gt. meas_type_SvR_beg .and. meas_type .lt. meas_type_SvR_end) then
        write(iu_main_output,'(3x,a,3(6x,a),2x,a,2a15)') '#','sza','vis','fis','sca_ang', &
        'meas_'//trim(meas_type_char),' fit_'//trim(meas_type_char)
        do iv=1,nvalid_meas
          write(iu_main_output,'(i4,4f9.2,2e15.5)') &
          iv,sza,vis(iv),fis(iv),arg(iv),temp_meas(iv),temp_fit(iv)
        enddo ! IV                        
      endif
      
      end subroutine print_meas_fit

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine print_error_estimates ( iu_main_output, RIN, segment_meas, GOUT )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_time_utils
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none
!	------------------------------------------------------------------------------------------------------
! IN :
      integer, intent(in) :: iu_main_output
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data), intent(in) :: segment_meas
      type(output_segment_general), intent(in) :: GOUT
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      integer  ::  ipix,IW,IOF,ISD
      integer  ::  NW,npixels
      integer  ::  alloc_stat
      real,dimension(:,:,:),allocatable     :: TEMP
      character(len=255)                    :: opt_funct
      character(len=12),dimension(KIMAGE)   :: pdate,ptime


!	------------------------------------------------------------------------------------------------------
! Print error estimates of retrieved parameter logarithms
      if( RIN%products%errest%par ) then
        call print_PAR_ERR_estimates( iu_main_output, RIN, segment_meas, GOUT%errest%par )
        if ( error_present() ) return
      endif

! Print error estimates of retrieved optical characteristic logarithms
      if( RIN%products%errest%aerosol%opt .or. RIN%products%errest%aerosol%lidar) then
        !write(iu_main_output,'(a,i2)') 'INVSING =',RIN%INVSING
        call print_OPT_ERR_estimates ( iu_main_output, RIN, segment_meas, GOUT )
        if ( error_present() ) return
      endif

!MEH:
! Print error estimates of SD_LN in the case it is possible
      if (RIN%products%errest%aerosol%mic) then
        !write(iu_main_output,'(a,i2)') 'INVSING =',RIN%INVSING
        call print_MIC_ERR_estimates ( iu_main_output, RIN, segment_meas, GOUT )
        if ( error_present() ) return
      endif


      return
      end subroutine print_error_estimates

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine print_OPT_ERR_estimates ( iu_main_output, RIN, segment_meas, GOUT )
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_time_utils
      use mod_sdata_derived_type
      use mod_stop_report
      use mod_par_inv, only : KW

      implicit none
!	------------------------------------------------------------------------------------------------------
! IN :
      integer, intent(in) :: iu_main_output
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data), intent(in) :: segment_meas
      type(output_segment_general), intent(in) :: GOUT

! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      integer :: ipix, IW, i, IOF, ISD
      integer :: NW, npixels, INVSING
      integer :: alloc_stat
      real,dimension(:,:,:), allocatable :: TEMP
      character(len=255) :: opt_funct
      character(len=12),dimension(KIMAGE) :: pdate, ptime
      integer :: NSD1, NOF_beg, NOF_end
! NOF  - number of optical functions for error estimates
! NSD1 - number of SD modes + 1(total)
      integer, dimension(4)    :: num_opt_errest_iwl
      integer, dimension(KW,4) :: opt_errest_iwl
!	------------------------------------------------------------------------------------------------------
      npixels = segment_meas%npixels
      NW = RIN%nw
      INVSING = RIN%IPFP%INVSING

      if( (GOUT%products%errest%aerosol%opt .and. RIN%products%errest%aerosol%opt) &
          .and. &
          (GOUT%products%errest%aerosol%lidar .and. RIN%products%errest%aerosol%lidar) ) then
         NOF_beg = 1
         NOF_end = 4 ! ext, ssa, aext, lr
      elseif( (.not. GOUT%products%errest%aerosol%opt .or. .not. RIN%products%errest%aerosol%opt) &
            .and. &
            (GOUT%products%errest%aerosol%lidar .and. RIN%products%errest%aerosol%lidar) ) then
         NOF_beg = 4
         NOF_end = 4 ! lr
      elseif( .not. GOUT%products%errest%aerosol%lidar .and. GOUT%products%errest%aerosol%opt ) then
         NOF_beg = 1
         NOF_end = 3 ! ext, ssa, aext
      endif !
      select case(RIN%NSD)
      case(1)
         NSD1=RIN%NSD
      case(2)
         NSD1=RIN%NSD+1  ! fine, coarse, total    
      end select

      !write(*,*) npixels,NW,NOF,NSD1,'  npixels,NW,NOF,NSD1'
      !write(*,*) NOF_beg, NOF_end,'  NOF_beg, NOF_end'
      !write(*,*) GOUT%products%errest%aerosol%opt, GOUT%products%errest%aerosol%lidar,'  GOUT: opt, lidar'
      !write(*,*) RIN%products%errest%aerosol%opt, RIN%products%errest%aerosol%lidar,'   RIN: opt, lidar'

      num_opt_errest_iwl(1) = RIN%naod_errest_iwl
      num_opt_errest_iwl(2) = RIN%nssa_errest_iwl
      num_opt_errest_iwl(3) = RIN%naext_errest_iwl !MEH: for aext
      num_opt_errest_iwl(4) = RIN%nlidar_errest_iwl
      opt_errest_iwl(1:num_opt_errest_iwl(1),1) = &
                            RIN%aod_errest_iwl(1:num_opt_errest_iwl(1))
      opt_errest_iwl(1:num_opt_errest_iwl(2),2) = &
                            RIN%ssa_errest_iwl(1:num_opt_errest_iwl(2))
! MEH: for aext
      opt_errest_iwl(1:num_opt_errest_iwl(3),3) = &
                            RIN%aext_errest_iwl(1:num_opt_errest_iwl(3))
      opt_errest_iwl(1:num_opt_errest_iwl(4),4) = &
                            RIN%lidar_errest_iwl(1:num_opt_errest_iwl(4))

      call yyyymmdd_hhmmss ( segment_meas,pdate,ptime )
      
      allocate(TEMP(NW,NSD1,npixels),stat=alloc_stat)
      if (alloc_stat /= 0) stop 'error while trying to allocate TEMP in write_OPT_ERR'

! *** STD
      write(iu_main_output,'(/,a,i0)') ' INVSING = ',INVSING
      write(iu_main_output,'(a)')    '--------------------------------------------------------------------------------------------'
      if(RIN%KL .eq. 1) then
         write(iu_main_output,'(a)') 'Total standard deviations of retrieved optical characteristic logarithms (~relative errors) :'
      else
         write(iu_main_output,'(a)') 'Total standard deviations of retrieved optical characteristic  (absolute errors) :'
      endif
      write(iu_main_output,'(a)')    '--------------------------------------------------------------------------------------------'
      write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                  "Date:",(pdate(ipix),ipix=1,npixels)
      write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  & 
                  "Time:",(ptime(ipix),ipix=1,npixels)

      do IOF=NOF_beg,NOF_end
      if(num_opt_errest_iwl(IOF) .eq. 0) cycle
      opt_funct = ''
      select case(IOF)
      case(index_ext)
         opt_funct = 'Wavelength (um), Aerosol Optical Depth (Random)'
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_extt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_ext(ISD)
         enddo ! ISD
         enddo ! ipix
      case(index_ssa)
         opt_funct = 'Wavelength (um), Single Scattering Albedo (Random)'      
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_ssat
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_ssa(ISD)
         enddo ! ISD
         enddo ! ipix
      case(index_aext)
         opt_funct = 'Wavelength (um), Aerosol Absorption Optical Depth (Random)'
         do ipix=1,npixels
        if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_aextt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_aext(ISD)
         enddo ! ISD
         enddo ! ipix
      case(index_lr)
         opt_funct =  'Wavelength (um), Lidar Ratio (Random)'      
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%lidar%pixel(ipix)%wl(1:NW)%TSTD_lrt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%lidar%pixel(ipix)%wl(1:NW)%TSTD_lr(ISD)
         enddo ! ISD
         enddo ! ipix       
      case default
         write(tmp_message,'(a,i0,a)') 'IOF = ',IOF,'value is not valid'
         G_ERROR(trim(tmp_message))
      end select

      if(NSD1 .eq. 3) then
        write(iu_main_output,'(2a)') trim(opt_funct),',   Total '
        do i=1,num_opt_errest_iwl(IOF)
          IW = opt_errest_iwl(i,IOF)
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(IW,NSD1,ipix),ipix=1,npixels)
        enddo ! i
      endif
      do ISD=1,RIN%NSD
        write(iu_main_output,'(2a,i0)') trim(opt_funct),' for Particle component ',ISD
        do i=1,num_opt_errest_iwl(IOF)
          IW = opt_errest_iwl(i,IOF)
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(IW,ISD,ipix),ipix=1,npixels)

        enddo ! i
      enddo ! ISD      

      enddo ! IOF

! *** RANDOM
      write(iu_main_output,'(a)') &
      '--------------------------------------------------------------------------------------'
      if(RIN%KL .eq. 1) then
         write(iu_main_output,'(a)') 'Standard deviations of retrieved optical characteristic logarithms (~relative errors) :'
      else
         write(iu_main_output,'(a)') 'Standard deviations of retrieved optical characteristic  (absolute errors) :'      
      endif
      write(iu_main_output,'(a)') &
      '--------------------------------------------------------------------------------------'
      write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                  "Date:",(pdate(ipix),ipix=1,npixels)
      write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  & 
                  "Time:",(ptime(ipix),ipix=1,npixels)

      do IOF=NOF_beg,NOF_end
      if(num_opt_errest_iwl(IOF) .eq. 0) cycle
      opt_funct = ''
      select case(IOF)
      case(index_ext)
         opt_funct = 'Wavelength (um), Aerosol Optical Depth (Random)'
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%ERR_extt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%ERR_ext(ISD)
         enddo ! ISD
         enddo ! ipix
      case(index_ssa)
         opt_funct = 'Wavelength (um), Single Scattering Albedo (Random)'      
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%ERR_ssat
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%ERR_ssa(ISD)
         enddo ! ISD
         enddo ! ipix
      case(index_aext)
         opt_funct = 'Wavelength (um), Aerosol Absorption Optical Depth (Random)'
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%ERR_aextt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%ERR_aext(ISD)
         enddo ! ISD
         enddo ! ipix
      case(index_lr)
         opt_funct =  'Wavelength (um), Lidar Ratio (Random)'      
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%lidar%pixel(ipix)%wl(1:NW)%ERR_lrt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%lidar%pixel(ipix)%wl(1:NW)%ERR_lr(ISD)
         enddo ! ISD
         enddo ! ipix       
      case default
         write(tmp_message,'(a,i0,a)') 'IOF = ',IOF,'value is not valid'
         G_ERROR(trim(tmp_message))
      end select

      if(NSD1 .eq. 3) then
        write(iu_main_output,'(2a)') trim(opt_funct),',   Total '
        do i=1,num_opt_errest_iwl(IOF)
          IW = opt_errest_iwl(i,IOF)
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(IW,NSD1,ipix),ipix=1,npixels)
        enddo ! i
      endif
      do ISD=1,RIN%NSD
        write(iu_main_output,'(2a,i3)') trim(opt_funct),' for Particle component ',ISD
        do i=1,num_opt_errest_iwl(IOF)
          IW = opt_errest_iwl(i,IOF)
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(IW,ISD,ipix),ipix=1,npixels)

        enddo ! i
      enddo ! ISD      

      enddo ! IOF

! *** BIAS

      write(iu_main_output,'(a)') & 
      '-----------------------------------------------------------------------------------------------'
      if(RIN%KL .eq. 1) then
         write(iu_main_output,'(a)') &
         'BIAS - Standard deviations of systematic errors of retrieved optical characteristic logarithms :'
      else
         write(iu_main_output,'(a)') & 
         'BIAS - Standard deviations of systematic errors of retrieved optical characteristics :'
      endif
      write(iu_main_output,'(a)') &
      '-----------------------------------------------------------------------------------------------'
      write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                  "Date:",(pdate(ipix),ipix=1,npixels)
      write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  & 
                  "Time:",(ptime(ipix),ipix=1,npixels)

      do IOF=NOF_beg,NOF_end
      opt_funct = ''
      select case(IOF)
      case(index_ext)
         opt_funct = 'Wavelength (um), Aerosol Optical Depth (Bias)'
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%BIAS_extt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%BIAS_ext(ISD)
         enddo ! ISD
         enddo ! ipix 
      case(index_ssa)
         opt_funct = 'Wavelength (um), Single Scattering Albedo (Bias)'            
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%BIAS_ssat
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%BIAS_ssa(ISD)
         enddo ! ISD
         enddo ! ipix
      case(index_aext)
         opt_funct = 'Wavelength (um), Aerosol Absorption Optical Depth (Bias)'
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%BIAS_aextt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%BIAS_aext(ISD)
         enddo ! ISD
         enddo ! ipix
      case(index_lr)
         opt_funct =  'Wavelength (um), Lidar Ratio (Bias)'      
         do ipix=1,npixels
         if(NSD1 .eq. 3) then
            TEMP(1:NW,NSD1,ipix) = GOUT%errest%aerosol%lidar%pixel(ipix)%wl(1:NW)%BIAS_lrt
         endif
         do ISD=1,RIN%NSD
            TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%lidar%pixel(ipix)%wl(1:NW)%BIAS_lr(ISD)
         enddo ! ISD
         enddo ! ipix       
      case default
         write(tmp_message,'(a,i0,a)') 'IOF = ',IOF,'value is not valid'
         G_ERROR(trim(tmp_message))
      end select

      if(NSD1 .eq. 3) then
        write(iu_main_output,'(2a)') trim(opt_funct),',   Total '
        do i=1,num_opt_errest_iwl(IOF)
          IW = opt_errest_iwl(i,IOF)
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(IW,NSD1,ipix),ipix=1,npixels)
        enddo ! IW
      endif
      do ISD=1,RIN%NSD
        write(iu_main_output,'(2a,i0)') trim(opt_funct),' for Particle mode ',ISD
        do i=1,num_opt_errest_iwl(IOF)
          IW = opt_errest_iwl(i,IOF)
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(IW,ISD,ipix),ipix=1,npixels)
        enddo ! IW
      enddo ! ISD      

      enddo ! IOF

      deallocate(TEMP,stat=alloc_stat)
      if (alloc_stat /= 0) then
        write(tmp_message,'(a)') 'error while trying to deallocate TEMP'
        G_ERROR(trim(tmp_message))
      endif

      return
      end subroutine print_OPT_ERR_estimates

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! MEH: subroutine to print error estimates of SD_LN (when it is not part of the retrieval)
    subroutine print_MIC_ERR_estimates ( iu_main_output, RIN, segment_meas, GOUT )
    use mod_retr_settings_derived_type
    use mod_retr_general_output_derived_type
    use mod_time_utils
    use mod_sdata_derived_type
    use mod_stop_report
    use mod_par_inv, only : KW

    implicit none
!    ------------------------------------------------------------------------------------------------------
! IN :
    integer, intent(in) :: iu_main_output
    type(retr_input_settings), intent(in) :: RIN
    type(segment_data), intent(in) :: segment_meas
    type(output_segment_general), intent(in) :: GOUT

    ! NOF  - number of optical functions for error estimates
    ! NSD1 - number of SD modes + 1(total)
    !    ------------------------------------------------------------------------------------------------------
! LOCAL :
    integer :: ipix, IW, i, IOF, ISD
    integer :: NW, npixels, INVSING, nrad, par_type
    integer :: alloc_stat
    real,dimension(:,:,:), allocatable :: TEMP_new
    character(len=355) :: opt_funct
    character(len=12),dimension(KIMAGE) :: pdate, ptime
    integer :: NSD1, NOF_beg, NOF_end
    ! NOF  - number of optical functions for error estimates
    ! NSD1 - number of SD modes + 1(total)


!! MEH:
    logical                  :: err_sd_ln
    integer                  :: IDIM1
!    ------------------------------------------------------------------------------------------------------
    npixels = segment_meas%npixels
    INVSING = RIN%IPFP%INVSING
    nrad = GOUT%retrieval%information%ngrid(1)
    par_type = RIN%NDIM%par_type(RIN%NDIM%n1)
    err_sd_ln = .true.

    if (par_type .eq. par_type_SD_LN) then
        if (err_sd_ln) then
            NOF_beg = 1
            NOF_end = 1 ! sd_ln
        endif !
    endif !

! MEH :
    select case(RIN%NSD)
    case(1)
       NSD1=RIN%NSD
    case(2)
       NSD1=RIN%NSD+1  ! fine, coarse, total
    end select



    call yyyymmdd_hhmmss ( segment_meas,pdate,ptime )

    allocate(TEMP_new(nrad,NSD1,npixels),stat=alloc_stat)
    if (alloc_stat /= 0) stop 'error while trying to allocate TEMP in write_OPT_ERR'

! *** STD
    write(iu_main_output,'(/,a,i0)') ' INVSING = ',INVSING
    write(iu_main_output,'(a)')    '--------------------------------------------------------------------------------------------'
    if(RIN%KL .eq. 1) then
       write(iu_main_output,'(a)') 'Total standard deviations of retrieved characteristic logarithms (~relative errors) :'
    else
       write(iu_main_output,'(a)') 'Total standard deviations of retrieved characteristic  (absolute errors) :'
    endif
    write(iu_main_output,'(a)')    '--------------------------------------------------------------------------------------------'
    write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                "Date:",(pdate(ipix),ipix=1,npixels)
    write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  &
                "Time:",(ptime(ipix),ipix=1,npixels)


    do IDIM1=1,RIN%NDIM%n1
      par_type = RIN%NDIM%par_type(IDIM1)
      opt_funct = ''
    select case(par_type)
    case(par_type_SD_LN)
       opt_funct = 'Radius (um), Size Distribution dV/dlnr'
       do ipix=1,npixels
       if(NSD1 .eq. 3) then
          TEMP_new(1:nrad,NSD1,ipix) = GOUT%errest%aerosol%mic%pixel(ipix)%ngrid(1:nrad)%TSTD_sdt
!          write(*,*) 'TEMP(1:nrad,NSD1,ipix)',TEMP(1:nrad,NSD1,ipix)
       endif
       !do ISD=1,RIN%NSD
       !   TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_ext(ISD)
       !enddo ! ISD
       enddo ! ipix
    end select
    exit
    enddo !idim1
    !write(*,*) 'trim(opt_funct)',trim(opt_funct)

    if(NSD1 .eq. 3) then
      write(iu_main_output,'(2a)') trim(opt_funct),',   Total '
      do i=1,nrad
        write(iu_main_output,'(e14.5,1000e14.5)')  &
            GOUT%retrieval%information%radius(i,0),(TEMP_new(i,NSD1,ipix),ipix=1,npixels)
      enddo ! i
    endif


! *** RANDOM
    write(iu_main_output,'(a)') &
    '--------------------------------------------------------------------------------------'
    if(RIN%KL .eq. 1) then
       write(iu_main_output,'(a)') 'Standard deviations of retrieved characteristic logarithms (~relative errors) :'
    else
       write(iu_main_output,'(a)') 'Standard deviations of retrieved characteristic  (absolute errors) :'
    endif
    write(iu_main_output,'(a)') &
    '--------------------------------------------------------------------------------------'
    write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                "Date:",(pdate(ipix),ipix=1,npixels)
    write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  &
                "Time:",(ptime(ipix),ipix=1,npixels)


    do IDIM1=1,RIN%NDIM%n1
      par_type = RIN%NDIM%par_type(IDIM1)
      opt_funct = ''
    select case(par_type)
    case(par_type_SD_LN)
       opt_funct = 'Radius (um), Size Distribution dV/dlnr (Random)'
       do ipix=1,npixels
       if(NSD1 .eq. 3) then
          TEMP_new(1:nrad,NSD1,ipix) = GOUT%errest%aerosol%mic%pixel(ipix)%ngrid(1:nrad)%ERR_sdt
    !          write(*,*) 'TEMP(1:nrad,NSD1,ipix)',TEMP(1:nrad,NSD1,ipix)
       endif
       !do ISD=1,RIN%NSD
       !   TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_ext(ISD)
       !enddo ! ISD
       enddo ! ipix
    end select
    exit
    enddo !idim1
    !write(*,*) 'trim(opt_funct)',trim(opt_funct)

    if(NSD1 .eq. 3) then
      write(iu_main_output,'(2a)') trim(opt_funct),',   Total '
      do i=1,nrad
        write(iu_main_output,'(e14.5,1000e14.5)')  &
            GOUT%retrieval%information%radius(i,0),(TEMP_new(i,NSD1,ipix),ipix=1,npixels)
      enddo ! i
    endif


! *** BIAS

    write(iu_main_output,'(a)') &
    '-----------------------------------------------------------------------------------------------'
    if(RIN%KL .eq. 1) then
       write(iu_main_output,'(a)') &
       'BIAS - Standard deviations of systematic errors of retrieved characteristic logarithms :'
    else
       write(iu_main_output,'(a)') &
       'BIAS - Standard deviations of systematic errors of retrieved characteristics :'
    endif
    write(iu_main_output,'(a)') &
    '-----------------------------------------------------------------------------------------------'
    write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                "Date:",(pdate(ipix),ipix=1,npixels)
    write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  &
                "Time:",(ptime(ipix),ipix=1,npixels)

    do IDIM1=1,RIN%NDIM%n1
      par_type = RIN%NDIM%par_type(IDIM1)
      opt_funct = ''
    select case(par_type)
    case(par_type_SD_LN)
       opt_funct = 'Radius (um), Size Distribution dV/dlnr (BIAS)'
       do ipix=1,npixels
       if(NSD1 .eq. 3) then
          TEMP_new(1:nrad,NSD1,ipix) = GOUT%errest%aerosol%mic%pixel(ipix)%ngrid(1:nrad)%BIAS_sdt
    !          write(*,*) 'TEMP(1:nrad,NSD1,ipix)',TEMP(1:nrad,NSD1,ipix)
       endif
       !do ISD=1,RIN%NSD
       !   TEMP(1:NW,ISD,ipix) = GOUT%errest%aerosol%opt%pixel(ipix)%wl(1:NW)%TSTD_ext(ISD)
       !enddo ! ISD
       enddo ! ipix
    end select
    exit
    enddo !idim1
    
    if(NSD1 .eq. 3) then
      write(iu_main_output,'(2a)') trim(opt_funct),',   Total '
      do i=1,nrad
        write(iu_main_output,'(e14.5,1000e14.5)')  &
            GOUT%retrieval%information%radius(i,0),(TEMP_new(i,NSD1,ipix),ipix=1,npixels)
      enddo ! i
    endif

    deallocate(TEMP_new,stat=alloc_stat)
    if (alloc_stat /= 0) then
      write(tmp_message,'(a)') 'error while trying to deallocate TEMP'
      G_ERROR(trim(tmp_message))
    endif

return
end subroutine print_MIC_ERR_estimates

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


      subroutine print_PAR_ERR_estimates( iu_main_output, RIN, segment_meas, GOUT_errest_par )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_time_utils
      use mod_par_inv, only : KPARS

      implicit none
!	------------------------------------------------------------------------------------------------------
! IN :
      integer, intent(in) :: iu_main_output
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data), intent(in) :: segment_meas
      type(output_segment_err_estim_par), intent(in) :: GOUT_errest_par
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      integer :: npixels, INVSING, KNSINGF, KN
      integer :: ipix, i, idim1, idim2, idim3
      character(len=12), dimension(KIMAGE) :: pdate, ptime
      logical, dimension(KPARS) :: par_errest_mask
!	------------------------------------------------------------------------------------------------------
        npixels = segment_meas%npixels
        INVSING = RIN%IPFP%INVSING
        KNSINGF = RIN%KNSINGF
        if ( .not. RIN%indep_par) then
          KN = RIN%KNSINGF
          par_errest_mask(1:KNSINGF) = RIN%APSERREST(1:KNSINGF)
        else
          KN = 0
          do idim1=1,RIN%NDIM_plus%n1
          if ( .not. RIN%NDIM_plus%par_retr(idim1) ) cycle
          do idim2=1,RIN%NDIM_plus%n2(idim1)
          do idim3=1,RIN%NDIM_plus%n3(idim2,idim1)
            KN = KN + 1
          enddo
          enddo
          enddo
          call set_par_errest_mask_plus(RIN, par_errest_mask)
        endif

        pdate(:) = ' '
        ptime(:) = ' '
        do ipix=1,npixels
          call convert_time_to_string(segment_meas%pixels(ipix)%t, "%F", pdate(ipix))
          call convert_time_to_string(segment_meas%pixels(ipix)%t, "%X", ptime(ipix))
        enddo
        if ( .not. RIN%flag_plus ) then
          write(iu_main_output,'(/,3(a,i0))') &
          ' INVSING = ',INVSING,'  KNSINGF = ',KNSINGF,'  KN = ',KNSINGF
        else
          write(iu_main_output,'(/,3(a,i0))') &
          ' INVSING = ',INVSING,'  KNSINGF = ',KNSINGF,'  KN = ',KNSINGF+(KN-KNSINGF)
        endif
        write(iu_main_output,'(a)')    '-------------------------------------------------------------------------'
        if(RIN%KL .eq. 1) then
          write(iu_main_output,'(a)') 'Total standard deviations of retrieved parameter logarithms (~relative errors) :'
        else
          write(iu_main_output,'(a)') 'Total standard deviations of retrieved parameters  (absolute errors) :'
        endif
        write(iu_main_output,'(a)')    '-------------------------------------------------------------------------'
        write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                     "Date:",(pdate(ipix),ipix=1,npixels)
        write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  &
                     "Time:",(ptime(ipix),ipix=1,npixels)
        do i=1,KN
          if ( .not. par_errest_mask(i) ) cycle
          write(iu_main_output,'(i14,1000e14.5)')  &
                  i,(GOUT_errest_par%pixel(ipix)%TSTDP(i),ipix=1,npixels)
        enddo ! i
        write(iu_main_output,'(a)')    '-------------------------------------------------------------------------'
        if(RIN%KL .eq. 1) then
          write(iu_main_output,'(a)') 'Standard deviations of retrieved parameter logarithms (~relative errors) :'
        else
          write(iu_main_output,'(a)') 'Standard deviations of retrieved parameters  (absolute errors) :'
        endif
        write(iu_main_output,'(a)')    '-------------------------------------------------------------------------'
        write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                     "Date:",(pdate(ipix),ipix=1,npixels)
        write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  &
                     "Time:",(ptime(ipix),ipix=1,npixels)
        do i=1,KN
          if ( .not. par_errest_mask(i) ) cycle
          write(iu_main_output,'(i14,1000e14.5)')  &
                     i,(GOUT_errest_par%pixel(ipix)%ERRP(i),ipix=1,npixels)
        enddo ! i
        write(iu_main_output,'(a)')    '----------------------------------------------------------------------------------'
        if(RIN%KL .eq. 1) then
          write(iu_main_output,'(a)') 'BIAS - Standard deviation of systematic errors of retrieved parameter logarithms :'
        else
          write(iu_main_output,'(a)') 'BIAS - Standard deviation of systematic errors of retrieved parameters :'
        endif
        write(iu_main_output,'(a)')    '----------------------------------------------------------------------------------'
        write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
                     "Date:",(pdate(ipix),ipix=1,npixels)
        write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  &
                     "Time:",(ptime(ipix),ipix=1,npixels)
        do i=1,KN
          if ( .not. par_errest_mask(i) ) cycle
          write(iu_main_output,'(i14,1000e14.5)')  &
                     i,(GOUT_errest_par%pixel(ipix)%BIASP(i),ipix=1,npixels)
        enddo ! i

      return
      end subroutine print_PAR_ERR_estimates

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print phase matrix
      subroutine print_phmx ( iu_main_output, RIN, segment_meas, GOUT )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_time_utils
      use mod_sdata_derived_type
      	  
      implicit none
! -----------------------------------------------------------------------------------
      integer, intent(in) :: iu_main_output
      type(retr_input_settings),    intent(in) :: RIN
      type(output_segment_general), intent(in) :: GOUT
      type(segment_data),           intent(in) :: segment_meas
! -----------------------------------------------------------------------------------
      integer :: II, ipix, IW, ISD, npixels, par_type
      !integer :: IDIM1, NDIM3
      character(LEN=12), dimension(KIMAGE) :: pdate, ptime
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: sph
! -----------------------------------------------------------------------------------
      npixels = segment_meas%npixels

      !do IDIM1=1,RIN%NDIM%n1
      !  par_type = RIN%NDIM%par_type(IDIM1)
      !  if(par_type .gt. par_type_SHD_beg  .and. par_type .lt. par_type_SHD_end) then
      !    call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,sph )
      !    if(RIN%NDIM%n2(IDIM1) .eq. 1 .and. RIN%NDIM%n3(1,IDIM1) .eq. 2) &
      !    sph(2,:,:) = sph(1,:,:)
      !  exit
      !  endif ! par_type .gt. par_type_SHD_beg  .and.
      !enddo ! IDIM1

      write(iu_main_output,*)
      write(iu_main_output,*)	"  Phase Matrix"
      write(iu_main_output,*)

      call yyyymmdd_hhmmss ( segment_meas,pdate,ptime )

      select case (RIN%DLSF%keyEL)
      case(1)
        do ipix=1,segment_meas%npixels
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          write(iu_main_output,'(a,i0,a,a10,a,a8)') 'ipix=',ipix,'  yymmdd = ',pdate(ipix),'  hhmmss = ',ptime(ipix)
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          do IW=1,RIN%nw
            do ISD=1,RIN%NSD
              !NDIM3 = RIN%NDIM%n3(ISD,IDIM1)
              write(iu_main_output,10) &
              'wl =',RIN%wave(IW),'  isd = ',ISD, &
              '  sca =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD)* &
                       GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ext =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ssa =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD), &
              '  n =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mreal(ISD), &
              '  k =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mimag(ISD) !, &
              !'  fsphere=',sph(1:NDIM3,ISD,ipix)
              write(iu_main_output,'(a10,3x,a12,2x,a14)')	'#','angle','p11'
              do II = 1,GOUT%aerosol%phmx%nangle
                write(iu_main_output,11) II,GOUT%aerosol%phmx%angle(II), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph11(II,ISD)
              enddo ! II
            enddo ! ISD
          enddo ! IW
        enddo ! ipix
      case(2)
        do ipix=1,segment_meas%npixels
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          write(iu_main_output,'(a,i0,a,a10,a,a8)') 'ipix=',ipix,'  yymmdd = ',pdate(ipix),'  hhmmss = ',ptime(ipix)
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          do IW=1,RIN%nw
            do ISD=1,RIN%NSD
              !NDIM3 = RIN%NDIM%n3(ISD,IDIM1)
              write(iu_main_output,10) &
              'wl =',RIN%wave(IW),'  isd =',ISD, &
              '  sca =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD)* &
                       GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ext =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ssa =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD), &
              '  n =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mreal(ISD), &
              '  k =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mimag(ISD) !, &
              !'  fsphere=',sph(1:NDIM3,ISD,ipix)
              write(iu_main_output,'(2a)')	"         # 	    angle        ", &
              "p11             p12"
              do II = 1,GOUT%aerosol%phmx%nangle
                write(iu_main_output,11) II,GOUT%aerosol%phmx%angle(II), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph11(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph12(II,ISD)
              enddo ! II
            enddo ! ISD
          enddo ! IW
        enddo ! ipix
      case(3)
        do ipix=1,segment_meas%npixels
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          write(iu_main_output,'(a,i0,a,a10,a,a8)') 'ipix=',ipix,'  yymmdd = ',pdate(ipix),'  hhmmss = ',ptime(ipix)
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          do IW=1,RIN%nw
            do ISD=1,RIN%NSD
              !NDIM3 = RIN%NDIM%n3(ISD,IDIM1)
              write(iu_main_output,10) &
              'wl =',RIN%wave(IW),'  isd = ',ISD, &
              '  sca =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD)* &
                       GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ext =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ssa =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD), &
              '  n =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mreal(ISD), &
              '  k =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mimag(ISD) !, &
              !'  fsphere=',sph(1:NDIM3,ISD,ipix)
              write(iu_main_output,'(2a)')	"         # 	    angle        ", &
              "p11            p12             p22"
              do II = 1,GOUT%aerosol%phmx%nangle
                write(iu_main_output,11) II,GOUT%aerosol%phmx%angle(II),                    &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph11(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph12(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph22(II,ISD)

              enddo ! II
            enddo ! ISD
          enddo ! IW
        enddo ! ipix
      case(4)
        do ipix=1,segment_meas%npixels
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          write(iu_main_output,'(a,i0,a,a10,a,a8)') 'ipix=',ipix,'  yymmdd = ',pdate(ipix),'  hhmmss = ',ptime(ipix)
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          do IW=1,RIN%nw
            do ISD=1,RIN%NSD
              !NDIM3 = RIN%NDIM%n3(ISD,IDIM1)
              write(iu_main_output,10) &
              'wl =',RIN%wave(IW),'  isd = ',ISD, &
              '  sca =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD)* &
                       GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ext =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ssa =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD), &
              '  n =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mreal(ISD), &
              '  k =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mimag(ISD) !, &
              !'  fsphere=',sph(1:NDIM3,ISD,ipix)
              write(iu_main_output,'(2a)')	"         # 	    angle        ", &
              "p11             p12             p22             p33"
              do II = 1,GOUT%aerosol%phmx%nangle
                write(iu_main_output,11) II,GOUT%aerosol%phmx%angle(II),                    &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph11(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph12(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph22(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph33(II,ISD)
              enddo ! II
            enddo ! ISD
          enddo ! IW
        enddo ! ipix
      case(5)
        do ipix=1,segment_meas%npixels
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          write(iu_main_output,'(a,i0,a,a10,a,a8)') 'ipix=',ipix,'  yymmdd = ',pdate(ipix),'  hhmmss = ',ptime(ipix)
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          do IW=1,RIN%nw
            do ISD=1,RIN%NSD
              !NDIM3 = RIN%NDIM%n3(ISD,IDIM1)
              write(iu_main_output,10) &
              'wl =',RIN%wave(IW),'  isd = ',ISD, &
              '  sca =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD)* &
                       GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ext =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ssa =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD), &
              '  n =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mreal(ISD), &
              '  k =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mimag(ISD) !, &
              !'  fsphere=',sph(1:NDIM3,ISD,ipix)
              write(iu_main_output,'(2a)')	"         # 	    angle        ", &
              "p11             p12             p22             p33             p34"
              do II = 1,GOUT%aerosol%phmx%nangle
                write(iu_main_output,11) II,GOUT%aerosol%phmx%angle(II),                    &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph11(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph12(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph22(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph33(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph34(II,ISD)
              enddo ! II
            enddo ! ISD
          enddo ! IW
        enddo ! ipix
      case(6)
        do ipix=1,segment_meas%npixels
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          write(iu_main_output,'(a,i0,a,a10,a,a8)') 'ipix=',ipix,'  yymmdd = ',pdate(ipix),'  hhmmss = ',ptime(ipix)
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          do IW=1,RIN%nw
            do ISD=1,RIN%NSD
              !NDIM3 = RIN%NDIM%n3(ISD,IDIM1)
              write(iu_main_output,10) &
              'wl =',RIN%wave(IW),'  isd = ',ISD, &
              '  sca =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD)* &
                       GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ext =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD), &
              '  ssa =',GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD), &
              '  n =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mreal(ISD), &
              '  k =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mimag(ISD) !, &
              !'  fsphere=',sph(1:NDIM3,ISD,ipix)
              write(iu_main_output,'(2a)')	"         # 	   angle        ", &
              "p11             p12             p22             p33             p34             p44"
              do II = 1,GOUT%aerosol%phmx%nangle
                write(iu_main_output,11) II,GOUT%aerosol%phmx%angle(II),                    &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph11(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph12(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph22(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph33(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph34(II,ISD), &
                                  GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph44(II,ISD)
              enddo ! II
            enddo ! ISD
          enddo ! IW
        enddo ! ipix
      end select

      write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
10    format(a,f9.6,a,i0,5(a,es12.5))
11    format(6x,I4,3x,f12.3,2x,6(es14.5,2x))

      if ( RIN%CUTOFF%nrmax .gt. 0 ) then
        call print_phmx_cut_off ( iu_main_output, RIN, segment_meas, GOUT )
      endif

      return
      end subroutine print_phmx

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print cut off phase matrix
      subroutine print_phmx_cut_off ( iu_main_output, RIN, segment_meas, GOUT )

      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_time_utils
      use mod_sdata_derived_type
      	  
      implicit none
! -----------------------------------------------------------------------------------
      integer, intent(in) :: iu_main_output
      type(retr_input_settings),    intent(in) :: RIN
      type(output_segment_general), intent(in) :: GOUT
      type(segment_data),           intent(in) :: segment_meas
! -----------------------------------------------------------------------------------
      integer :: i, II, ipix, IW, ISD, IDIM1, NDIM3, npixels, par_type
      character(LEN=12), dimension(KIMAGE) :: pdate, ptime
      real,dimension(KIDIM3,KIDIM2,KIMAGE) :: sph
      real,dimension(KMpar,0:KSD,4) :: tmp_p11
      real,dimension(0:KSD,4) :: tmp_sca
      real,dimension(0:KSD,4) :: tmp_ssa
      real,dimension(0:KSD,4) :: tmp_ext
      integer :: nsd
! -----------------------------------------------------------------------------------
      npixels = segment_meas%npixels
      nsd = RIN%NSD
      !do IDIM1=1,RIN%NDIM%n1
      !  par_type = RIN%NDIM%par_type(IDIM1)
      !  if(par_type .gt. par_type_SHD_beg  .and. par_type .lt. par_type_SHD_end) then
      !    call unpack_retr_param_to_print ( RIN,GOUT%retrieval%par,IDIM1,npixels,sph )
      !    if(RIN%NDIM%n2(IDIM1) .eq. 1 .and. RIN%NDIM%n3(1,IDIM1) .eq. 2) &
      !    sph(2,:,:) = sph(1,:,:)
      !  exit
      !  endif ! par_type .gt. par_type_SHD_beg  .and.
      !enddo ! IDIM1

      write(iu_main_output,*)
      write(iu_main_output,*)	"  Phase Matrix CUT OFF"
      write(iu_main_output,*)

      call yyyymmdd_hhmmss ( segment_meas,pdate,ptime )

        do ipix=1,segment_meas%npixels
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          write(iu_main_output,'(a,i0,a,a10,a,a8)') 'ipix=',ipix,'  yymmdd = ',pdate(ipix),'  hhmmss = ',ptime(ipix)
          write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
          do IW=1,RIN%nw
            do i=1,RIN%CUTOFF%nrmax
              tmp_ext(0:nsd,i) = GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext_cut_off(0:nsd,i)
              tmp_sca(0:nsd,i) = GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa_cut_off(0:nsd,i)* &
                                 GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ext_cut_off(0:nsd,i)
              tmp_ssa(0:nsd,i) = GOUT%aerosol%opt%pixel(ipix)%wl(IW)%ssa_cut_off(0:nsd,i)
              tmp_p11(1:GOUT%aerosol%phmx%nangle,0:nsd,i) = &
              GOUT%aerosol%phmx%pixel(ipix)%wl(IW)%ph11_cut_off(1:GOUT%aerosol%phmx%nangle,0:nsd,i)

              write(iu_main_output,'(2(a,f9.5,2x),2(a,es12.5,2x))') &
              'wl =',RIN%wave(IW), &
              'rmax_tb = ',RIN%CUTOFF%rmax_tb(1,i), &
              'n =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mreal(1), &
              'k =',GOUT%aerosol%rind%pixel(ipix)%wl(IW)%mimag(1)
              write(iu_main_output,'(23x,a,10(es14.5,2x))') &
              'sca:',tmp_sca(0:nsd,i)
              write(iu_main_output,'(23x,a,10(es14.5,2x))') &
              'ext:',tmp_ext(0:nsd,i)
              write(iu_main_output,'(23x,a,10(es14.5,2x))') &
              'ssa:',tmp_ssa(0:nsd,i)
              write(iu_main_output,'(a10,3x,a12,2x,a14)')	'#','angle','p11_cut_off:'
              do II = 1,GOUT%aerosol%phmx%nangle
                write(iu_main_output,11) II,GOUT%aerosol%phmx%angle(II), tmp_p11(II,0:nsd,i)
              enddo ! II
            enddo ! i
          enddo ! IW
        enddo ! ipix

      write(iu_main_output,'(a)') 	"--------------------------------------------------------------------------"
11    format(6x,I4,3x,f12.3,2x,6(es14.5,2x))

      return
      end subroutine print_phmx_cut_off

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print broadband flux

      subroutine print_forcing_bbflux (iu,RIN,segment_meas,GOUT)

      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
            
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: npixels, IHLV, NHLV
!----------------------------------------------------------------------------------------
      npixels  = segment_meas%npixels
      NHLV     = GOUT%forcing%bbflux%pixel(1)%NHLV

      write(iu,*) ''
      write(iu,'(a)') 'Upward Broadband Flux without Aerosol (w/m^2):'
      do IHLV=1,NHLV
        write(iu,'(f9.3,a5)', advance='no') GOUT%forcing%bbflux%pixel(1)%HLV(IHLV), ' km  '
        call rprint_array_real (iu,GOUT%forcing%bbflux%pixel(:)%BBUFX0(IHLV), '(e14.5)', 1, npixels)
      enddo ! IHLV

      write(iu,*) ''
      write(iu,'(a)') 'Downward Broadband Flux without Aerosol (w/m^2):'
      do IHLV=1,NHLV
        write(iu,'(f9.3,a5)', advance='no') GOUT%forcing%bbflux%pixel(1)%HLV(IHLV), ' km  '
        call rprint_array_real (iu,GOUT%forcing%bbflux%pixel(:)%BBDFX0(IHLV), '(e14.5)', 1, npixels)
      enddo ! IHLV

      write(iu,*) ''
      write(iu,'(a)') 'Upward Broadband Flux with Aerosol (w/m^2):'
      do IHLV=1,NHLV
        write(iu,'(f9.3,a5)', advance='no') GOUT%forcing%bbflux%pixel(1)%HLV(IHLV), ' km  '
        call rprint_array_real (iu,GOUT%forcing%bbflux%pixel(:)%BBUFXA(IHLV), '(e14.5)', 1, npixels)
      enddo ! IHLV

      write(iu,*) ''
      write(iu,'(a)') 'Downward Broadband Flux with Aerosol (w/m^2):'
      do IHLV=1,NHLV
        write(iu,'(f9.3,a5)', advance='no') GOUT%forcing%bbflux%pixel(1)%HLV(IHLV), ' km  '
        call rprint_array_real (iu,GOUT%forcing%bbflux%pixel(:)%BBDFXA(IHLV), '(e14.5)', 1, npixels)
      enddo ! IHLV

      return
      end subroutine print_forcing_bbflux

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print net aerosol radiative forcing

      subroutine print_forcing_forcing (iu,RIN,segment_meas,GOUT)

      use mod_retr_general_output_derived_type
      use mod_print_array
      use mod_sdata_derived_type
            
      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                     intent(in)  ::  iu
      type(retr_input_settings),   intent(in)  ::  RIN
      type(segment_data),          intent(in)  ::  segment_meas
      type(output_segment_general),intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer                              :: npixels, IHLV, NHLV
!----------------------------------------------------------------------------------------
      npixels  = segment_meas%npixels
      NHLV     = GOUT%forcing%bbflux%pixel(1)%NHLV

      write(iu,*) ''
      write(iu,'(a)') 'Net Aerosol Forcing (w/m^2):'
      do IHLV=1,NHLV
        write(iu,'(f9.3,a5)', advance='no') GOUT%forcing%forcing%pixel(1)%HLV(IHLV), ' km  '
        call rprint_array_real (iu,GOUT%forcing%forcing%pixel(:)%NETFORC(IHLV), '(e14.5)', 1, npixels)
      enddo ! IHLV

      write(iu,*) ''
      write(iu,'(a)') 'Aerosol Forcing Efficiency (w/m^2):'
      do IHLV=1,NHLV
        write(iu,'(f9.3,a5)', advance='no') GOUT%forcing%forcing%pixel(1)%HLV(IHLV), ' km  '
        call rprint_array_real (iu,GOUT%forcing%forcing%pixel(:)%FORCEFF(IHLV), '(e14.5)', 1, npixels)
      enddo ! IHLV

      return
      end subroutine print_forcing_forcing

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print Standardized Size Distribution

      subroutine print_size_distribution_gout (iu, GOUT)

      use mod_retr_general_output_derived_type
      use mod_print_array

      implicit none  
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(output_segment_general), intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer  :: IDIM2
      integer  :: ipix, i
      integer  :: ngrid, npixels, nsd
      real,dimension(KIMAGE)  :: temp
!----------------------------------------------------------------------------------------
      npixels  = GOUT%retrieval%information%npixels
      nsd = GOUT%retrieval%information%nsd

      do IDIM2=1,GOUT%retrieval%information%nsd    ! particle component loop
        ngrid = GOUT%retrieval%information%ngrid(IDIM2)
        write(iu,'(2a,i0)') 'Radius (um), Size Distribution dV/dlnr for', &
                                        ' Particle component ',IDIM2
        do i=1,ngrid
          write(iu,'(e14.5)',advance='no') GOUT%retrieval%information%radius(i,IDIM2)
          do ipix=1,npixels
          temp(ipix) = GOUT%retrieval%par%pixel(ipix)%sd(i,IDIM2)
          enddo
          call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
        enddo ! i_prn
      enddo ! IDIM2

      if(nsd .gt. 1) then
        ngrid = GOUT%retrieval%information%ngrid(0)
        write(iu,'(a)') 'Radius (um), Total Size Distribution dV/dlnr'
        do i=1,ngrid
          write(iu,'(e14.5)',advance='no') GOUT%retrieval%information%radius(i,0)
          do ipix=1,npixels
          temp(ipix) = GOUT%retrieval%par%pixel(ipix)%sd(i,0)
          enddo
          call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
        enddo ! i_prn
      endif

      return
      end subroutine print_size_distribution_gout

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print level Hight

      subroutine print_level_H (iu, GOUT)

      use mod_retr_general_output_derived_type
      use mod_derivative_type_transport_model, only : TM
      use mod_print_array

      implicit none
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(output_segment_general), intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer  :: ipix, i
      integer  :: npixels
      real,dimension(KIMAGE)  :: temp
!----------------------------------------------------------------------------------------
      write(iu,'(/,a)') '...............'
      write(iu,'(a)')   'Transport Model'
      write(iu,'(a,/)') '...............'

      npixels  = GOUT%retrieval%information%npixels

      write(iu,'(2a,i0)') '#, Level Hights (m):'
      do i=1,TM%nlev
          write(iu,'(i14)',advance='no') i
          do ipix=1,npixels
          temp(ipix) = TM%H(i,ipix)
          enddo
          call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
      enddo ! i

      return
      end subroutine print_level_H

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print level Relative Humidity

      subroutine print_level_RH (iu, GOUT)

      use mod_retr_general_output_derived_type
      use mod_derivative_type_transport_model, only : TM
      use mod_print_array

      implicit none
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(output_segment_general), intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer  :: ipix, i
      integer  :: npixels
      real,dimension(KIMAGE)  :: temp
!----------------------------------------------------------------------------------------

      npixels  = GOUT%retrieval%information%npixels

      write(iu,'(2a,i0)') '#, Relative Humidity % :'
      do i=1,TM%nlev
          write(iu,'(i14)',advance='no') i
          do ipix=1,npixels
          temp(ipix) = TM%RH(i,ipix)
          enddo
          call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
      enddo ! i

      return
      end subroutine print_level_RH

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Print level tracer volume Concentration Vertical Profile

      subroutine print_level_VP (iu, GOUT)

      use mod_retr_general_output_derived_type
      use mod_derivative_type_transport_model, only : TM
      use mod_print_array

      implicit none
!----------------------------------------------------------------------------------------
! IN :
      integer,                      intent(in)  ::  iu
      type(output_segment_general), intent(in)  ::  GOUT
!----------------------------------------------------------------------------------------
! LOCAL :
      integer  :: IDIM2
      integer  :: ipix, itrc, i
      integer  :: ngrid, npixels, ntrc
      real,dimension(KIMAGE)  :: temp
!----------------------------------------------------------------------------------------

      npixels  = GOUT%retrieval%information%npixels
      ntrc = TM%ntrc

      do itrc=1,ntrc    ! tracer loop
      write(iu,'(2a,a5)') '#, Volume Concentration Vertical Profile,', &
                                              ' Tracer ',trim(TM%trcs(itrc))
      do i=1,TM%nlev
          write(iu,'(i14)',advance='no') i
          do ipix=1,npixels
          temp(ipix) = TM%vp(i,itrc,ipix)
          enddo
          call rprint_array_real (iu,temp(:), '(e14.5)', 1, npixels)
      enddo ! i
      enddo ! itrc

      return
      end subroutine print_level_VP

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


