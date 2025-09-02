#include "../../../constants_set/mod_globals.inc"

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine sets transport model structure with setiings and initial guess
        !> 
        !> @param[in]     RIN - settings
        !> @param[in]     AP - vector of parameters driving forward model
        !> @param[in]     npixels - number of pixels in segment
        !> @param[inout]  TM1 - transport model structure
        !>

      subroutine set_transport_model_segment_iguess ( RIN, AP1, npixels, TM1 )

      use mod_globals, only : sp, dp,  GBL_FILE_PATH_LEN
      use mod_retr_settings_derived_type
      use mod_derivative_type_transport_model
      use mod_c_utils, only : cstring2fstring
      use mod_apm_transport_model
      use mod_par_OS, only : HMAX_atm
      use mod_hydroscopic_growth, only : reff_hydroscopic_growth_factor, &
                                         rh_modified_particle_characteristic
      use mod_stop_report

      implicit none
! ..................................................................................
      type(retr_input_settings), intent(in) :: RIN
      real(sp), dimension(KPARS,KIMAGE), intent(in) :: AP1
      integer, intent(in) :: npixels
      type(transport_model), intent(inout) :: TM1
! ..................................................................................
      real(sp), dimension(KPARS,KIMAGE) :: AP
      character(len=GBL_FILE_PATH_LEN) :: path, folder
      integer :: itrc, ilev, ipix
      integer :: IDIM1, IDIM2, IDIM3, II
      integer :: par_type
      real(dp),dimension(KVERTM) :: rh
      character(len=5) :: trc_type
      real(sp) :: scaler, fd
      integer :: ntrc, nlev
      real(sp),dimension(KVERTM) :: hgfactor
      !real(sp), parameter :: tiny_hgfactor = 0.001
! ..................................................................................
      call initialize_transport_model ( TM1 )

      TM1%flag_av_vprof = RIN%TMSET%flag_av_vprof
      TM1%ntrc = RIN%TMSET%ntrc
      do itrc=1,TM1%ntrc
      call cstring2fstring(RIN%TMSET%trcs(:,itrc), TM1%trcs(itrc))
      enddo
      TM1%flag_hphi(1:TM1%ntrc) = RIN%TMSET%flag_hphi(1:TM1%ntrc)
      TM1%density_dry(1:TM1%ntrc) = RIN%TMSET%density(1:TM1%ntrc)
      TM1%nlev = RIN%TMSET%nlev
      ntrc = TM1%ntrc
      nlev = TM1%nlev


! Chin, M., P. Ginoux, S. Kinne, O. Torres, B. N. Holben, B. N. Duncan,
! R. V. Martin, J. A. Logan, A. Higurashi, and T. Nakajima,
! Tropospheric aerosol optical thickness from the GOCART model and
! comparisons with satellite and sunphotometer measurements,
! J. Atmos. Sci., 59, 461-483, 2002.

! Water
      TM1%water%density = 1000.0 ! kg/m3
      TM1%water%nwl = RIN%NW
      TM1%water%wl(1:RIN%NW) = RIN%WAVE(1:RIN%NW)
      TM1%water%reri(:) = 1.33
      TM1%water%imri(:) = 1e-8

      select case(RIN%KL)
      case(0)
        AP(1:RIN%KNSING,1:npixels) = AP1(1:RIN%KNSING,1:npixels)
      case(1)
        AP(1:RIN%KNSING,1:npixels) = EXP(AP1(1:RIN%KNSING,1:npixels))
      end select

      do ipix=1,npixels 

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if ( par_type .eq. par_type_TM_RH ) then
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
          do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            TM1%rh(IDIM3,ipix) = AP(II,ipix)
          enddo
          enddo
        elseif ( par_type .eq. par_type_TM_H ) then
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
          do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            TM1%h(IDIM3,ipix) = AP(II,ipix)
            if ( TM1%h(IDIM3,ipix) .gt. HMAX_atm ) then
              write(tmp_message,'(a,es12.4,a,2x,a,es12.4,2a)') &
              'level height = ',TM1%h(IDIM3,ipix),' .gt. ', &
              'HMAX_atm',HMAX_atm, &
              NEW_LINE('A'), &
              'max of level height in settings must be <= HMAX_atm in mod_par_OS'
              G_ERROR(trim(tmp_message))
            endif
          enddo

          enddo
        elseif ( par_type .eq. par_type_TM_C ) then
          ! concentrations of dry tracers ?
          ! see also routine unpack_parameter_vector_ap()
          ! where TMC unpack for every iteration
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
          do IDIM3=1,RIN%NDIM%n3(IDIM2,IDIM1)
            II = RIN%NDIM%ISTARSING(IDIM2,IDIM1)+IDIM3-1
            TM1%c_dry(IDIM3,IDIM2,ipix) = AP(II,ipix)
          enddo
          enddo
        endif
      enddo ! IDIM1

! Apply relative humidity growth factor to tracer density
      do itrc=1,ntrc
      trc_type = TM%trcs(itrc)
      if ( TM1%flag_hphi(itrc) .eq. hpho ) then
        TM1%hgfactor(1:nlev,itrc,ipix) = 1.0_sp
        TM1%density_rh(1:nlev,itrc,ipix) = TM1%density_dry(itrc)
      else ! hphi
        rh(1:nlev) = TM1%rh(1:nlev,ipix) / 100.0_sp
        do ilev=1,nlev
          if ( rh(ilev)-0.99_dp .gt. 1d-4 ) then
          write(tmp_message,'(2(a,i0,2x),a,es12.6,a)') 'ipix = ',ipix,'ilev = ',ilev, &
          'rh =',rh(ilev),' > 0.99'
          G_ERROR(trim(tmp_message))
          endif
        enddo
        !endif
        call reff_hydroscopic_growth_factor( nlev, rh(1:nlev), &
                                        trc_type(:), hgfactor(1:nlev) )

        TM1%hgfactor(1:nlev,itrc,ipix) = hgfactor(1:nlev)

        do ilev=1,nlev
        if ( hgfactor(ilev) .ge. 1.0_sp ) then ! hydroscopic growth
          !fd = 1.0 ! - volume fraction of dry aerosol
          !fd = ( reff_dry*reff_dry*reff_dry ) / ( reff*reff*reff )
          scaler = hgfactor(ilev) * hgfactor(ilev) * hgfactor(ilev)
          fd = 1.0_sp/scaler
          if ( fd .gt. 1.0_sp ) then
            write(tmp_message,'(a,es11.4,2x,a)') &
            'dry matter fraction =',fd,'valid values <= 1.'
            G_ERROR(trim(tmp_message))
          endif
          TM1%density_rh(ilev,itrc,ipix) = &
          rh_modified_particle_characteristic( TM1%density_dry(itrc), TM1%water%density, fd )
        elseif ( hgfactor(ilev) .lt. 1.0_sp ) then
          write(tmp_message,'(a,f9.4,a,2(2x,a,i0))') &
          'Relative humidity growth factor = ',hgfactor(ilev),'can not be  < 1.', &
          NEW_LINE('A'), &
          'itrc = ',itrc,'ilev = ',ilev
          G_ERROR(trim(tmp_message))
        endif
        enddo ! ilev
      endif
      enddo ! itrc

      enddo ! ipix

!
      call cstring2fstring(RIN%DLSF%external_file_path, TM%tracer_average_phmx_file_path)

      return
      end subroutine set_transport_model_segment_iguess

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

