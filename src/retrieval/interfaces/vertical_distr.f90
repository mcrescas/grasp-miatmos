        !> @file vertical_distr.f90
        !> Contains routines related to computation of discret vertical
        !> distributions for given number of altitudes to be used in 
        !> in Successive Order of Scattering (SOS) radiative transfer routine
        !>

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!PL the vertical profile with gases is modified.
!PL vertical profile of aerosol can be analitical but can be calculated
!PL in descrite levels spcified by gases vertical profile 

        !> @brief Routine prepares input and calls routine computing discret
        !> @brief verical distributions for all assumed atmospheric components
        !>
        !> @param[in]  RT_SOS_SET - input settings structure for RTSOS
        !> @param[in]  HGR_km - ground height above sea level
        !> @param[in]  HMAX_atm_km - maximum height of atmosphere
        !> @param[in]  NHVP_retr - number of altitudes for retrieved vertical profile
        !> @param[in]  HVP_retr_km - altitudes for retrieved vertical profile
        !> @param[in]  H0 - parameters of vertical distribution or distribution itself
        !> @param[in]  sigma_aerosol - standard deviation for normal distribution
        !> @param[in]  ifgas - defines presence of gas absorption in sdata structure
        !> @param[in]  gaspar - gas absorption value from sdata structure
        !> @param[out] DISCRVD - discret vertical distribution structure

      subroutine RT_discret_vertical_distr_with_gas( aerosol_analyt_prof, 		          &
                                                    gas_abs_line,HGR_km, HMAX_atm_km, &
                                                    N_level, Hight_level_km, H0,   		&
                                                    sigma_aerosol, ifgas, gaspar, 		&
                                                    DISCRVD )

      USE MOD_RT_SOS_SETUP, ONLY : RT_SOS_SET
      use mod_vertical_distr_derived_type
      use mod_par_inv, only : KVERTM
      use mod_par_OS, only : KSD
	    use sub_gas_kd, only : DATA_GAS
	    use mod_bbgas_kd, only : GET_GAS_PROFILE
      use mod_derivative_type_transport_model, only : TM, tracer_average


      implicit none
! -----------------------------------------------------------------------------------------
!IN:
      real,                       intent(in) :: HGR_km, HMAX_atm_km
      integer,                    intent(in) :: N_level
      real,dimension(KVERT_WD),   intent(in) :: Hight_level_km
      real,dimension(KVERTM,KSD), intent(in) :: H0 ! AL contains parameters of aerosol vertical
                                                   ! distribution or distribution itself
      real,dimension(KSD),        intent(in) :: sigma_aerosol
      integer,                    intent(in) :: ifgas
      real,                       intent(in) :: gaspar

      logical,                    intent(in) :: aerosol_analyt_prof,gas_abs_line
! -----------------------------------------------------------------------------------------
!INOUT:
      type(discret_vertical_distribution), intent(inout) :: DISCRVD
! -----------------------------------------------------------------------------------------
!LOCAL:
      character(len=20), dimension(NMM+NMG)    :: distr_type
      real, dimension(NMM+NMG)                 :: hm_km, sigma_km
      integer, dimension(NMM+NMG)              :: nhinp
      real, dimension(KVERT_WD,NMM+NMG)        :: hinp_km
      real, dimension(KVERT_WD,NMM+NMG)        :: vdinp
      integer                                  :: nhinp_max
! -----------------------------------------------------------------------------------------
      integer :: natm, naer, nmol, ngas
      integer :: i, ibeg, iend

! -----------------------------------------------------------------------------------------
      nhinp_max = KVERT_WD

      distr_type(:) = ''
      hm_km(:) = -999.0
      sigma_km(:) = -999.0
      nhinp(:) = -999
      hinp_km(:,:) = -999.0
      vdinp(:,:) = -999.0
      natm = 0
      naer = 0
      nmol = 0
      ngas = 0
! Aerosol
      naer = RT_SOS_SET%NA
      natm = natm + naer
      if(naer .gt. 0) then
        ibeg = 1
        iend = natm
        if(aerosol_analyt_prof) then
          select case(RT_SOS_SET%AER_PRF)
          case(0) ! exponential
          ! exponential
            distr_type(ibeg:iend) = 'exponential'
            hm_km(ibeg:iend) = H0(1,1:naer)*0.001  ! km
          case(1)
          ! gaussian
            distr_type(ibeg:iend) = 'gaussian'
            hm_km(ibeg:iend) = H0(1,1:naer)*0.001  ! km
            sigma_km(ibeg:iend) = sigma_aerosol(1:naer)*0.001 ! km   ! 530.33m -> 0.53033km
          case(2)
          ! threshold
            distr_type(ibeg:iend) = 'threshold'
            hm_km(ibeg:iend) = H0(1,1:naer)*0.001  ! km
          end select
        else
          ! lookup table
          distr_type(ibeg:iend) = 'lut'
          nhinp(ibeg:iend) = N_level
          vdinp(1:N_level,ibeg:iend) = H0(1:N_level,1:naer)
          do i=ibeg,iend
          hinp_km(1:N_level,i) = Hight_level_km(1:N_level)
          enddo
          if ( TM%flag_av_vprof .eq. tracer_average ) then
            naer = N_level
            vdinp(:,:) = 0.0
            do i=1,naer
            vdinp(i,i) = 1.0
            enddo
          endif
!write(*,'(a)') 'hinp_km:   in RT_discret_vertical_distr_with_gas'
!do i=1,N_level
!write(*,'(i0,20es12.4)') i,hinp_km(i,ibeg:iend)
!enddo

!write(*,'(a)') 'vdinp:     in RT_discret_vertical_distr_with_gas'
!do i=1,N_level
!write(*,'(i0,20es12.4)') i,vdinp(i,ibeg:iend)
!enddo

        endif
      endif

! Molecular scattering
      nmol = 1
      natm = natm + nmol
      ibeg = naer + 1
      iend = natm
      select case(RT_SOS_SET%MOL_PRF)
      case(0) ! exponential
        distr_type(ibeg:iend) = 'exponential'
        hm_km(ibeg:iend) = 8.0
      case(1) ! standard atmosphere
        distr_type(ibeg:iend) = 'stdatm'
      end select

!PL  implimentation for gas absorption
      if(gas_abs_line) then
        ngas = 1
        natm = natm + ngas
        ibeg = naer + nmol + 1
        iend = naer + nmol + ngas
        nhinp(1:iend) = N_level
        !gasabs = gaspar
        distr_type(ibeg:iend) = 'lut'
			  hinp_km(1:N_level,iend) = Hight_level_km(1:N_level)
			  !vdinp(1:N_level,ibeg:iend) = H0(1:N_level,1:naer)			
!		 write(*,*) 'vp2',NMG,Hight_level_km(1:N_level)
!		 write(*,*) 	hinp_km(1:N_level,:)
!		 	
      endif

      DISCRVD%natm = natm
      DISCRVD%naer = naer
      DISCRVD%nmol = nmol
      DISCRVD%ngas = ngas

!      write(*,'(5(a,i0,2x))') 'natm = ',natm,'naer = ',naer,'nmol = ',nmol,'ngas = ',ngas

      !if(RT_SOS_SET%IP_VERBOSE) then
      !  if(nhinp_max .lt. maxval(nhinp(1:natm))) then
      !    write(*,'(2(a,i0,x))') 'nhinp_max = ',nhinp_max, &
      !    'must be greater or equal to maxval(nhinp(1:natm)) = ',maxval(nhinp(1:natm))
      !    write(*,'(a)') 'Execution has to be terminated.'
      !    stop 'stop in RT_discret_vertical_distr'
      !  endif
      !endif
      ! nhinp_max = KVERTM
      !write(*,*) 'RT_discret_vertical_distr begin '

      call discret_vertical_distr_with_gas ( HGR_km, HMAX_atm_km, distr_type,  &
                                    hm_km, sigma_km,                  &
                                    nhinp_max, nhinp(:),              &
                                    hinp_km(1:nhinp_max,:),           &
                                    vdinp(1:nhinp_max,:),             &
                                    DISCRVD )

      !write(*,'(a)') 'DISCRVD%h_km(1:DISCRVD%nh) in RT_discret_vertical_distr_with_gas'
      !write(*,'(10es12.4)') DISCRVD%h_km(1:DISCRVD%nh)
      !do i=1,natm
      !write(*,'(a,i0,2x,a,es11.4)') 'component # ',i,'norm =',DISCRVD%norm(i)
      !write(*,'(10es12.4)') DISCRVD%val(1:DISCRVD%nh,i)
      !enddo ! i
      !stop 123
!     write(*,*) 'vp4_0',  nhinp(:)
!     write(*,*) 'vp4_1', hinp_km(1:N_level,1)
!     write(*,*) 'vp4_2', hinp_km(1:N_level,2)
!     write(*,*) 'vp4_3', hinp_km(1:N_level,3)
!     write(*,*) 'vp5', DISCRVD%nh
!     write(*,*) 'vp6', DISCRVD%h_km(1:DISCRVD%nh)
!     write(*,*) 'vp7 _ norm', DISCRVD%norm(:)
!     write(*,*) 'vp8_1', DISCRVD%val(1:DISCRVD%nh,1)
!     write(*,*) 'vp8_2', DISCRVD%val(1:DISCRVD%nh,2)
!     write(*,*) 'vp8_3', DISCRVD%val(1:DISCRVD%nh,3)

      return
      end subroutine RT_discret_vertical_distr_with_gas

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine computes discret verical distributions for
        !> @brief all assumed atmospheric components
        !>
        !> @param[in]  HGR_km - ground height above sea level in km
        !> @param[in]  HMAX_atm_km - maximum height of atmosphere in km
        !> @param[in]  distr_type - extinction vertical distributions
        !> @param[in]  hm_km - mean/scale height for normal distribution
        !> @param[in]  sigma_km - standard deviation for normal distribution
        !> @param[in]  nhinp - number of altitudes for input LUT profile
        !> @param[in]  hinp_km - altitudes for input LUT profile
        !> @param[in]  vdinp - input LUT profile
        !> @param[out] DISCRVD - discret vertical distribution structure

      subroutine discret_vertical_distr_with_gas ( HGR_km, HMAX_atm_km, distr_type,  &
                                          hm_km, sigma_km,                  & ! model (input)
                                          nhinp_max, nhinp, hinp_km, vdinp, & ! lut   (input)
                                          DISCRVD )

      use mod_vertical_distr_derived_type
      use mod_stop_report

      implicit none
!	----------------------------------------------------------------------
      real, intent(in) :: HGR_km, HMAX_atm_km
      character(len=20), dimension(NMM+NMG) :: distr_type
      real, intent(in), dimension(NMM+NMG) :: hm_km, sigma_km
      integer, intent(in) :: nhinp_max
      integer, intent(in), dimension(NMM+NMG) :: nhinp
      real, intent(in), dimension(nhinp_max,NMM+NMG) :: hinp_km
      real, intent(in), dimension(nhinp_max,NMM+NMG) :: vdinp
      type(discret_vertical_distribution), intent(inout) :: DISCRVD
!	----------------------------------------------------------------------
      integer :: i, i1, ilut
      real,dimension(KVERT_WD+2) :: h_km_temp
      integer :: natm
      logical :: lut_present
!	----------------------------------------------------------------------

      lut_present = .false.
! distr_type: const / exponential / gaussian / lut / stdatm
      natm = DISCRVD%natm

!XH   if we have LUT of vertical profile, the computation will be based on the grid points in the LUT

!XH      find the index for the component using the LUT
!XH      search from the end to prevent conflicts when absorbing gas profile and lidar profile coexist
      ilut = -999

      do i1=natm,1,-1

        if(trim(distr_type(i1)) .eq. 'lut') then
          lut_present = .true.
          ilut = i1
          exit
        endif
      end do
      
	  if (lut_present) then
!XH     grid points before regriding
!        write(*,'(2(a,f10.4))') 'HGR_km =',HGR_km,'  HMAX_atm_km =',HMAX_atm_km
!        write(*,'(a)') 'in discret_vertical_distr before grid_altitudes_LUT'
!        write(*,'(a,i0,a,10(i0,2x))') 'natm = ',natm,'  nhinp: ',nhinp(1:natm)
        !do i1=1,natm
        !do i=1,nhinp(i1)
          !write(*,'(2(2x,i0),f12.4,e12.4,2x,a)')  i1, i, hinp_km(i,i1), vdinp(i,i1), &
          !                                     '- i1, i, hinp_km(i,i1), vdinp(i,i1)'
        !end do
        !end do

        ! creating altitude grid from lidar measurements adding additional points at ground
        ! level (HGR_km) and at top pf the atmosphere HMAX_atm_km) km
        if(nhinp(ilut) .gt. KVERT_WD) then
          write(*,'(2(a,i0))') 'Number of altitudes for given vertical distribution lut = ',nhinp(ilut), &
          '  ilut = ',ilut
          write(*,'(a,i0,a)') 'can not be larger than parameter KVERT_WD = ',KVERT_WD, &
                              ' for discret vertical distribution arrays.'
          stop 'stop in discret_vertical_distr'
        endif

		! add top and bottom levels to  LUT profile
        call grid_altitudes_LUT_with_gas ( HGR_km, HMAX_atm_km,             & ! IN
                                  nhinp(ilut), hinp_km(1:nhinp(ilut),ilut), &
                                  DISCRVD%nh,  h_km_temp(1:nhinp(ilut)+2)   & ! OUT
                                )

        if(nhinp(ilut) .eq. KVERT_WD .and. nhinp(ilut) .lt. DISCRVD%nh) then
          write(*,'(2(a,i0))') 'in discret_vertical_distr  nhinp = ',nhinp(ilut), &
                               ' .le. DISCRVD%nh = ',DISCRVD%nh
          write(*,'(a)') 'DISCRVD%h_km() and h_km_temp() array size conflict.'
          stop 'stop in discret_vertical_distr'
        endif


        DISCRVD%h_km(1:DISCRVD%nh) = h_km_temp(1:DISCRVD%nh)

!		write(*,*) 'vp4_1', hinp_km(1:N_level,1)
!	    write(*,*) 'vp4_2', hinp_km(1:N_level,2)
!	    write(*,*) 'vp4_3', hinp_km(1:N_level,3)
!		write(*,*) 'vp4_-1', hinp_km(1:nhinp(ilut),ilut)

      else
         ! creating a full-scale altitude grid from ground level (HGR_km) to the top of
         ! the atmosphere (HMAX_atm_km)

         call grid_altitudes_log ( HGR_km, HMAX_atm_km,        & ! IN
                                   DISCRVD%nh,                 &
                                   DISCRVD%h_km(1:DISCRVD%nh)  & ! OUT
                                 )

      end if
!      write(*,'(a)') 'in discret_vertical_distr altitudes for discret vertical distributions'
!      do i=1,DISCRVD%nh
!        write(*,'(2x,i0,f12.4,2x,a)') i, DISCRVD%h_km(i),'- i, DISCRVD%h_km(i)'
!      end do
!      write(*,*) 'in discret_vertical_distr:'

      do i1=1,natm-DISCRVD%ngas
         ! calculate vertical distributions for each atmospheric component 
         ! depending on distribution type
         ! output vertical distribution is NOT NORMALIZED
         call discrvd_single_atm_component (  distr_type(i1), hm_km(i1), sigma_km(i1),       &
                                              nhinp(i1), hinp_km(1:nhinp(i1),i1), vdinp(1:nhinp(i1),i1), &
                                              DISCRVD%nh, DISCRVD%h_km(1:DISCRVD%nh),        &
                                              DISCRVD%norm(i1), DISCRVD%val(1:DISCRVD%nh,i1) & ! OUT
                                           )
         !write(*,*) i1,DISCRVD%norm(i1),trim(distr_type(i1)),'  - atm component, vd norm, distr type'
         !print *, 'h_km :'
         !write(*,'(10es12.4)') DISCRVD%h_km(1:DISCRVD%nh)
         !print *, 'vp :'
         !write(*,'(10es12.4)') DISCRVD%val(1:DISCRVD%nh,i1)
      end do ! i1

      return
      end subroutine discret_vertical_distr_with_gas

!  sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        SUBROUTINE RT_VERTICAL_DISCRET(RIN,igab,ikdist,IW,         &  !IN
                                       gas_abs_data_forw_im,      &
                                       iFlux,HGR_km,HVP_retr_km,  &
                                       HMAX_atm_km,NHVP_retr,     &
                                       H0,                        &
                                       nsubchannels,              &
                                       sigma_aerosol,             &
                                       ipix,pixel_fit,            &
                                       DISCRVD)                      !OUT

        USE mod_par_inv,   ONLY : KVERTM
        USE mod_abs_kd,    ONLY : DATA_ABS
        USE mod_par_OS,    ONLY : KSD, KVERT_WD
        USE sub_gas_kd,    ONLY : DATA_GAS
        USE mod_vertical_distr_derived_type
        USE mod_retr_settings_derived_type
        USE mod_derivative_type_transport_model, only : TM
        USE mod_sdata_derived_type
        IMPLICIT NONE
! -------------------
! IN:
        TYPE(retr_input_settings),            INTENT(IN)      ::  RIN
        REAL,                                 INTENT(IN)      ::  HGR_km, HMAX_atm_km
        REAL,DIMENSION(KVERTM,KSD),            INTENT(INOUT)    ::  H0
        REAL,DIMENSION(KSD),                   INTENT(IN)      ::  sigma_aerosol
        LOGICAL,                              INTENT(IN)      ::  igab, ikdist
        INTEGER,                              INTENT(IN)      ::  NHVP_retr, nsubchannels
        INTEGER,                              INTENT(IN)      ::  IW, ipix
        REAL,DIMENSION(KVERTM),                INTENT(IN)      ::  HVP_retr_km
! -------------------
! INOUT:
        TYPE (DATA_GAS),                        INTENT(INOUT)  ::  gas_abs_data_forw_im
        LOGICAL,                                INTENT(INOUT)  ::  iFlux
        TYPE(pixel),                            INTENT(INOUT)  ::  pixel_fit
        TYPE(discret_vertical_distribution),    INTENT(INOUT)  ::  DISCRVD
! -------------------
! OUT:

! -------------------
! LOCAL:
        LOGICAL  ::  aerosol_analyt_prof,gas_abs_line
        INTEGER  ::  N_level
        !REAL,DIMENSION(max(KVERTM,KVERT_WD))  ::  Hight_level_km
        REAL,DIMENSION(KVERT_WD)  ::  Hight_level_km
! -------------------

      If (NHVP_retr .eq. 1) then
      ! aerosol at levels for analystica vertical profile function
        aerosol_analyt_prof=.true.
      else
      ! aerosol at LIDAR levels
        aerosol_analyt_prof=.false.
      endif
      if (RIN%use_tmodel) then
      ! Transport model
        aerosol_analyt_prof=.false.
        if (KVERT_WD .lt. KVERTM) then
          write(*,*) 'The value of KVERT_WD = ', KVERT_WD,' should be >=',KVERTM
            stop
        endif
      endif

      if(igab) then
          iFlux = .FALSE.
          if (KVERT_WD .lt. gas_abs_data_forw_im%NLV) then
            write(*,*) 'The value of KVERT_WD = ', KVERT_WD,' should be >=',gas_abs_data_forw_im%NLV
            stop
          endif
!PL            aerosol_analyt_prof=.true.
          gas_abs_line=.true.

!PL LIDAR or gasseus vertical profile
          if (aerosol_analyt_prof) then
            N_level = gas_abs_data_forw_im%NLV
            Hight_level_km (1:N_level) =  gas_abs_data_forw_im%HLV(1:gas_abs_data_forw_im%NLV)
          else
            N_level = NHVP_retr
            Hight_level_km(1:N_level) = HVP_retr_km(1:NHVP_retr)
          endif

      else

!PL            aerosol_analyt_prof=.true.
          gas_abs_line=.false.

!PL LIDAR or analytical aerosol profile
          if (aerosol_analyt_prof) then
            N_level = 1
            Hight_level_km (1) = 1
          else
            N_level = NHVP_retr
            Hight_level_km(1:N_level) = HVP_retr_km(1:NHVP_retr)
          endif
      endif

      if (RIN%use_tmodel) then
        N_level = TM%nlev
        Hight_level_km(1:N_level) = TM%H(1:N_level,ipix) * 0.001
        !H0(1:N_level,1:TM%ntrc) = TM%vp(1:N_level,1:TM%ntrc,ipix)
      endif


      DISCRVD%nh = RIN%NLVLS_GEOM

      call RT_discret_vertical_distr_with_gas( aerosol_analyt_prof,       &
                              gas_abs_line, HGR_km, HMAX_atm_km,          &
                              N_level, Hight_level_km(1:N_level),         &
                              H0, sigma_aerosol,                          &
                              pixel_fit%ifgas, pixel_fit%meas(IW)%gaspar, &
                              DISCRVD )


!            call RT_discret_vertical_distr ( HGR_km, HMAX_atm_km,     &
!            NHVP_retr, HVP_retr_km, H0,   &
!            sigma_aerosol, pixel_fit%ifgas, pixel_fit%meas(IW)%gaspar, &
!            DISCRVD )

        RETURN
        END SUBROUTINE RT_VERTICAL_DISCRET

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine computes verical distributions at discret
        !> @brief altitudes for given single atmospheric component
        !>
        !> @param[in]  distr_type - type of vertical distribution
        !> @param[in]  hm_km - mean/scale height for normal/exponential distribution
        !> @param[in]  sigma_km - standard deviation for normal distribution
        !> @param[in]  nhinp - number of altitudes for input LUT distribution
        !> @param[in]  hinp_km - altitudes for input LUT distribution
        !> @param[in]  vdinp - input LUT distribution
        !> @param[in]  nhout - number of altitudes for discret distribution
        !> @param[in]  hout_km - altitudes for discret distribution
        !> @param[out] xnorm - norm of output distribution
        !> @param[out] vdout - discret vertical distribution


      subroutine discrvd_single_atm_component ( distr_type, hm_km, sigma_km, & ! IN
                                              nhinp, hinp_km, vdinp,    &
                                              nhout, hout_km,           &
                                              xnorm, vdout              & ! OUT
                                             )
      use mod_intrpl_linear
      use mod_molecular_scattering, only: std_atm_density
      use mod_stop_report

      implicit none 
!	------------------------------------------------------------------------------------------------------	  
      real, intent(in) :: hm_km, sigma_km
      character(len=20) :: distr_type
      integer, intent(in) :: nhinp
      real, dimension(nhinp), intent(in) :: hinp_km
      real, dimension(nhinp), intent(in) :: vdinp
      integer, intent(in) :: nhout
      real, dimension(nhout), intent(in) :: hout_km
      real, intent(out) :: xnorm
      real, dimension(nhout), intent(out) :: vdout
!	------------------------------------------------------------------------------------------------------
      double precision, dimension(nhout) :: vdtemp
      double precision, dimension(nhout) :: y
      real, parameter :: tiny = 0.0 !1e-25
      double precision :: ea !, pi
      integer :: i
      logical :: lstop
!	------------------------------------------------------------------------------------------------------
! Calculate vertical distribution (vdout) at nhout given grid altitudes (hout_km)
! NOTE: altitudes are in descending order
!       input : hinp_km   hinp_km(1) > hinp_km(nhinp)
!       output: hout_km   hout_km(1) > hout_km(nhout)
!	------------------------------------------------------------------------------------------------------
! distr_type: const / exponential / gaussian / lut / stdatm
      lstop = .false.

!      pi = dacos(-1e0)
      vdtemp(:) = 0.0 ! vertical distribution initialization

      !write(*,'(2(a,2x),a)') 'in discrvd_single_atm_component', &
      !trim(distr_type),'profile before modification:'
      !do i=1,nhinp
      !   write(*,'(2x,i0,f12.4,e12.4)') i, hinp_km(i), vdinp(i)
      !end do

      select case(trim(distr_type))
      case('const')
          vdtemp(1:nhout) = 1.
      case('gaussian')
          ! p(h) = exp( -(hg-hm)**2/(2.*sigma*sigma) + (hg-h )**2/(2.*sigma*sigma) ) =
          !      = exp( (hg-h)(hg+h-2hm)/(2.*sigma*sigma) )
          !
          !  hg - hout_km(nhout)   ground hight over sea level
          !  h  - hout_km          current hight
          !  hm - hm_km            mean hight

          ea = 2.d0 * sigma_km * sigma_km
!PL

          y(1:nhout) = - (hout_km(1:nhout) - hm_km ) * (hout_km(1:nhout) - hm_km )

          vdtemp(1:nhout) = dexp ( y(1:nhout)/ea )

      case('exponential')
            ! exponential profile is divided by hm_km so the profile will be normalized
            ! vdtemp(1:nhout) = dexp( dble(hout_km(nhout)-hout_km(1:nhout))/hm_km )/dble(hm_km)
            ! /dble(hm_km) can be deleted because the profile will be normalized
            vdtemp(1:nhout) = dexp( dble(hout_km(nhout)-hout_km(1:nhout))/hm_km )

      case('threshold')
           where (hout_km(1:nhout).LE.hm_km)
                 vdtemp = 1 ! AL /hm_km 9actually no need to divide since the profile will be renormalised anyway)
           elsewhere
                 vdtemp = 1.0e-9
           endwhere

      case('lut')
         do i=1,nhout
            if (hout_km(i) .gt. hinp_km(1)) then
!XH            ! points higher than maximum height in LUT are assumed 0
               vdtemp(i) = 0.0
               !write(*,*) i0,hinp_km(i0),vdinp(i0),i,hout_km(i),vdtemp(i),
               !           '  1: i0,hinp_km(i0),vdinp(i0),i,hout_km(i),vdtemp(i)'
            else if(hout_km(i) .lt. hinp_km(nhinp)) then
!XH            ! points lower than minimum height in LUT assumed to be the same
               vdtemp(i) = vdinp(nhinp)
               !write(*,*) i0,hinp_km(i0),vdinp(i0),i,hout_km(i),vdtemp(i),
               !           '  2: i0,hinp_km(i0),vdinp(i0),i,hout_km(i),vdtemp(i)'
            else if(abs(hout_km(i)-hinp_km(nhinp))/hinp_km(nhinp) .lt. 1e-4) then
!XH            ! points equal to minimum height in LUT assumed to be the same
               vdtemp(i) = vdinp(nhinp)
               !write(*,*) i0,hinp_km(i0),vdinp(i0),i,hout_km(i),vdtemp(i),
               !           '  2: i0,hinp_km(i0),vdinp(i0),i,hout_km(i),vdtemp(i)'
            else
!XH            ! otherwise interpolate to LUT grid
               vdtemp(i) = LINEAR(hinp_km(1:nhinp),vdinp(1:nhinp),nhinp,hout_km(i))
               !write(*,*) i0,hinp_km(i0),vdinp(i0),i,hout_km(i),vdtemp(i),  &
               !           '  3: i0,hinp_km(i0),vdinp(i0),i,hout_km(i),vdtemp(i)'
            end if ! hout_km(i .gt. hinp_km(1)
         enddo ! i=1,nhout
      case('stdatm')
          ! used only for molecular scattering vertical distr.
          call std_atm_density(nhout,hout_km,vdtemp)
      case default
          write(*,'(3a)') 'distr_type = ',trim(distr_type),'  - unknown value'
          call print_crash_report()
          lstop = .true.
      end select ! distr_type
      !write(*,'(2(a,2x),a)') 'in discrvd_single_atm_component modified ',trim(distr_type), &
      !                       'discret profile before normalization:'
      !do i=1,nhout
      !   write(*,'(2x,i0,f12.4,e12.4)') i, hout_km(i), vdtemp(i)
      !end do

! calculate the norm of the profile
        xnorm = 0.0
        do i=2,nhout
          xnorm = xnorm + 0.5*(vdtemp(i-1)+vdtemp(i))*(hout_km(i-1)-hout_km(i))
          !write(*,*) '*** i=',i,'  hout_km(i) =',hout_km(i),'  xnorm =',xnorm
        enddo ! i

        if(xnorm .le. tiny) then
          write(*,'(2a,e12.4)') 'Execution has to be terminated.', &
          ' In discrvd_single_atm_component xnorm =', xnorm
          do i=1,nhout
            write(*,'(2x,i0,f12.4,e12.4,2x,a)') i, hout_km(i), vdtemp(i), &
            '- i, hout_km, vdtemp'
          enddo
          do i=1,nhinp
            write(*,'(2x,i0,f12.4,2x,a)') i, hinp_km(i), &
            '- i, hout_km'
          enddo
          call print_crash_report()
          lstop = .true.
        endif

      if(lstop) &
      stop 'stop in discrvd_single_atm_component'

      !vdout(:) = vdtemp(:)/xnorm
      vdout(:) = vdtemp(:) 

      !write(*,'(2(a,2x),a)') 'in discrvd_single_atm_component normalized discret', &
      !                        trim(distr_type),'profile:'
!      write(*,'(a,f10.6)') 'xnorm =',xnorm
      !do i=1,nhout
      !  write(*,'(2x,i0,f12.4,e12.4)') i, hout_km(i), vdout(i)
      !enddo ! i

      return
      end subroutine discrvd_single_atm_component

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine computes given number of equdistant in natural 
        !> @brief logarithm altitudes from top of the atmosphere altitude
        !> @brief to ground height above sea level
        !>
        !> @param[in]  HGR_km - ground height above sea level in km
        !> @param[in]  HMAX_atm_km - top of the atmosphere altitude in km
        !> @param[in]  nhout - number of altitudes
        !> @param[out] hout_km - output altitudes in descending order

!PL Profile clculated relatively ground level (without any "shift")
      subroutine grid_altitudes_log ( HGR_km, HMAX_atm_km, &
                                  nhout, hout_km       &
                                )
      implicit none
!	------------------------------------------------------------------------------------------------------      
      real, intent(in) :: HGR_km, HMAX_atm_km
      integer, intent(in) :: nhout
      real, dimension(nhout), intent(out) :: hout_km  ! hout_km(1) > hout_km(nhout)
!	------------------------------------------------------------------------------------------------------
      real :: shift_km, DH_km
      integer :: i
!PL Boundery level above ground level in km        
      real, parameter:: H_bound=0.2
!	------------------------------------------------------------------------------------------------------
       
!PL relatively ground level
      hout_km(1) = HMAX_atm_km - HGR_km
      hout_km(nhout) = 0
     
!PL The solution to avoid problem with HGR_km=0 or HGR_km < 0.
      If (H_bound .lt. ( HMAX_atm_km-HGR_km) ) then 
         DH_km = (log( HMAX_atm_km-HGR_km ) - log(H_bound) )
      else
         DH_km = (log( HMAX_atm_km-HGR_km ) - log(( HMAX_atm_km-HGR_km )/2. ) )
      endif

 
      DH_km = exp(- DH_km / (nhout-1))
  
      do i=2,nhout-1
        hout_km(i) = hout_km(i-1)*DH_km 
      enddo ! i	

!PL relatively sea level	  
      hout_km(:)=hout_km(:)+HGR_km

 !     write(*,'(a)') 'in grid_altitudes hout_km:'
 !     write(*,*) DH_km, log(H_bound)
 !     write(*,'(10f12.4)') hout_km(1:nhout)
 !     stop

      return
      end subroutine grid_altitudes_log

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine returns altitudes including top of the atmosphere
        !> @brief altitude and ground height above sea level
        !>
        !> @param[in]  HGR_km      - ground height above sea level in km
        !> @param[in]  HMAX_atm_km - top of the atmosphere altitude in km
        !> @param[in]  nhinp       - number of input altitudes
        !> @param[in]  hinp_km     - input altitudes in descending order
        !> @param[out] nhout       - number of output altitudes
        !> @param[out] hout_km     - output altitudes in descending order

      subroutine grid_altitudes_LUT_with_gas ( HGR_km, HMAX_atm_km, nhinp, hinp_km, &
                                              nhout, hout_km  &
                                            )
      implicit none
!	------------------------------------------------------------------------------------------------------      
      real, intent(in) :: HGR_km, HMAX_atm_km
      integer, intent(in) :: nhinp
      real, dimension(nhinp), intent(in) :: hinp_km ! hinp_km(1) > hinp_km(nhinp)
      integer, intent(out) :: nhout
      real, dimension(nhinp+2), intent(out) :: hout_km  ! hout_km(1) > hout_km(nhout)
      integer i_level
!	------------------------------------------------------------------------------------------------------

      nhout = nhinp

      hout_km(1:nhinp) = hinp_km(1:nhinp)

      if(hinp_km(1) .lt. HMAX_atm_km) then
        nhout = nhout+1
        hout_km(2:nhout) = hinp_km(1:nhinp)
        hout_km(1) = HMAX_atm_km
	    else
!PL adding the test for the case when maximal grided altitude is bigger then HMAX_atm_km
!PL test is added for gases absorption profile. The effect on other
!PL appraches should be tested  	  
	     level_loop1: do i_level=2, nhout  				
		 				if (hout_km(i_level) .lt. HMAX_atm_km) then	
		 					hout_km(1) = HMAX_atm_km
		 					nhout=nhinp-i_level+2
		 					hout_km(2:nhout) = hinp_km(i_level:nhinp)
		 					exit level_loop1
						endif
		 			 enddo level_loop1
      endif

      if(hout_km(nhout) .gt. HGR_km) then
!        write(*,*) 'in_grid_altitudes_lut'
!        write(*,*) 'hinp_km(nhinp)=',hinp_km(nhinp)
!        write(*,*) 'HGR_km=', HGR_km
        nhout = nhout+1
        hout_km(nhout) = HGR_km
 	    else
!PL adding the test for the case when minimal grided altitude is smaller then HGR_km
!PL test is added for gases absorption profile. The effect on other
!PL appraches should be tested  	  
	     level_loop: do i_level=(nhout-1),1,-1  				
		 				if (hout_km(i_level) .gt. HGR_km) then	
		 					nhout=i_level+1
		 					hout_km(nhout) = HGR_km
		 					exit level_loop
						endif
		 			 enddo level_loop
	    endif

!      write(*,'(a)') 'in grid_altitudes_LUT hinp_km:'
!      write(*,'(10f12.4)') hinp_km(1:nhinp)
!      write(*,'(a)') 'in grid_altitudes_LUT hout_km:'
!      write(*,'(10f12.4)') hout_km(1:nhout)
!      write(*,*) nhout
! stop
      return
      end subroutine grid_altitudes_LUT_with_gas

!  sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss



