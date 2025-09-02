#include "../../../constants_set/mod_globals.inc"

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine returns single scattering particle properties based on
        !> @brief transport model

      subroutine forw_phase_matrix_transport_model (              &
                                    iu_main_output,               & ! IN
                                    IPRI_verbose,                 &
                                    IPRI_additional_info,         &
                                    NSD,NBIN,RADIUS,SD,           &
                                    KNLN,                         &
                                    NSHAPE,RATIOS,SHD,            &
                                    IW,WAVE,RREAL,RIMAG,          &
                                    use_models,                   &
                                    tiny_wl_models,               &
                                    NANG,ANGL,ipix,               & ! OUT
                                    GOUT_particles_opt_pixel_wl,  &
                                    GOUT_particles_phmx_pixel_wl, &
                                    ext_norm,                     &
                                    DLSF,KERNELS1,KERNELS2        &
                                                 )

      use mod_globals, only : sp
      use mod_par_inv, only : KSHAPE, KIDIM3, KVERTM
      use mod_par_OS,  only : KSD
      use mod_par_DLS, only : KMpar
      use mod_alloc_kernels
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_derivative_type_transport_model, only : TM, tracer_average
      use mod_forward_model
      use mod_derivative_type_transport_model, only : TM, tracer_average
      use mod_stop_report

      implicit none
! ..................................................................................
! IN:
      integer,                    intent(in)  ::  iu_main_output
      logical,                    intent(in)  ::  IPRI_verbose
      logical,                    intent(in)  ::  IPRI_additional_info
      real(sp),dimension(KIDIM3,KSD), intent(in)  ::  RADIUS, SD
      integer,dimension(KSD),         intent(in)  ::  KNLN
      real(sp),dimension(KSHAPE,KSD), intent(in)  ::  RATIOS, SHD
      integer,dimension(KSD),         intent(in)  ::  NBIN, NSHAPE
      real(sp),dimension(KSD),        intent(in)  ::  RREAL, RIMAG
      integer,                        intent(in)  ::  NSD, ipix, IW
      real(sp),                       intent(in)  ::  WAVE
      type(iP_flags_for_DLS),         intent(in)  ::  DLSF
      logical,                        intent(in)  ::  use_models
      real,                           intent(in)  ::  tiny_wl_models
! ..................................................................................
! OUT:
      integer,                        intent(out) ::  NANG
      real(sp),dimension(KMpar),       intent(out) ::  ANGL
      type(output_pixel_opt_wl),      intent(inout)  ::  GOUT_particles_opt_pixel_wl
      type(output_pixel_ph_matrix_wl),intent(inout)  ::  GOUT_particles_phmx_pixel_wl
      real(sp),dimension(KSD),          intent(out) :: ext_norm
! ..................................................................................
! INOUT:
      type(kernels_triangle_bin), intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),intent(inout)  ::  KERNELS2
! ..................................................................................
! LOCAL:
      real(sp),dimension(KVERTM,KSD) :: ext_lev, ssa_lev, sca_lev
      real(sp),dimension(KMpar,KVERTM,KSD) ::  f11_lev, f12_lev, f22_lev, &
                                             f33_lev, f34_lev, f44_lev  ! sca matrix
! ..................................................................................
      ! compute ext, ssa, sca matrix for every given tracer at each level
      call tracer_level_sca_matrix (                        &
                              iu_main_output,               &
                              IPRI_verbose,                 &
                              IPRI_additional_info,         &
                              NSD,NBIN,RADIUS,SD,           & ! IN
                              KNLN,                         &
                              NSHAPE,RATIOS,SHD,            &
                              IW,WAVE,RREAL,RIMAG,          &
                              use_models,                   &
                              tiny_wl_models,               &
                              NANG,ANGL,ipix,               & ! OUT
                              ext_norm,                     &
                              DLSF,KERNELS1,KERNELS2,       &
                              ext_lev, ssa_lev, sca_lev,    &
                              f11_lev, f12_lev, f22_lev,     &
                              f33_lev, f34_lev, f44_lev      &
                                  )
      ! compute column average scattering matrix
      call column_average_sca_matrix (                           &
                                    IPRI_verbose,                &
                                    DLSF%keyEL, NANG, ipix,      &
                                    ext_lev, ssa_lev, sca_lev,   &
                                    f11_lev, f12_lev, f22_lev,    &
                                    f33_lev, f34_lev, f44_lev,    &
                                    GOUT_particles_opt_pixel_wl, &
                                    GOUT_particles_phmx_pixel_wl &
                                     )

      if ( TM%flag_av_vprof .eq. tracer_average ) then
      ! compute tracer average scattering matrix
      call tracer_average_sca_matrix ( DLSF%keyEL, NANG, ipix,    &
                                      ext_lev, ssa_lev, sca_lev, &
                                      f11_lev, f12_lev, f22_lev,  &
                                      f33_lev, f34_lev, f44_lev   &
                                     )
      if ( IPRI_verbose ) then
      call print_tracer_average_sca_matrix ( DLSF%keyEL, ipix,    &
                                             IW, WAVE,            &
                                             NANG, ANGL(1:NANG) )
      endif
      endif

      return
      end subroutine forw_phase_matrix_transport_model

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine returns single scattering particle properties
        !> @brief for every tracer at each atmosphere level
        !> 

      subroutine tracer_level_sca_matrix (                  & ! IN
                              iu_main_output,               &
                              IPRI_verbose,                 &
                              IPRI_additional_info,         &
                              NSD,NBIN,RADIUS,SD,           &
                              KNLN,                         &
                              NSHAPE,RATIOS,SHD,            &
                              IW,WAVE,RREAL,RIMAG,          &
                              use_models,                   &
                              tiny_wl_models,               &
                              NANG,ANGL,ipix,               & ! OUT
                              ext_norm,                     &
                              DLSF,KERNELS1,KERNELS2,       &
                              ext_lev, ssa_lev, sca_lev,    &
                              f11_lev, f12_lev, f22_lev,    &
                              f33_lev, f34_lev, f44_lev     &
                                        )

      use mod_globals, only : sp, dp, pi_sp, GBL_FILE_PATH_LEN
      use mod_par_inv, only : KSHAPE, KIDIM3, KVERTM
      use mod_par_OS,  only : KSD
      use mod_par_DLS, only : KMpar
      use mod_alloc_kernels
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_derivative_type_transport_model, only : TM, hphi, hpho
      use mod_intrpl_linear, only : linear
      use mod_apm_transport_model
      use mod_forward_model
      use mod_hydroscopic_growth, only : rh_modified_particle_characteristic
      use mod_stop_report

      implicit none
! ..................................................................................
! IN:
      integer,                        intent(in)  ::  iu_main_output
      logical,                        intent(in)  ::  IPRI_verbose
      logical,                        intent(in)  ::  IPRI_additional_info
      real(sp),dimension(KIDIM3,KSD), intent(in)  ::  RADIUS, SD
      integer,dimension(KSD),         intent(in)  ::  KNLN
      real(sp),dimension(KSHAPE,KSD), intent(in)  ::  RATIOS, SHD
      integer,dimension(KSD),         intent(in)  ::  NBIN, NSHAPE
      real(sp),dimension(KSD),        intent(in)  ::  RREAL, RIMAG
      integer,                        intent(in)  ::  NSD, ipix, IW
      real(sp),                       intent(in)  ::  WAVE
      type(iP_flags_for_DLS),         intent(in)  ::  DLSF
      logical,                        intent(in)  ::  use_models
      real,                           intent(in)  ::  tiny_wl_models
      real(sp),dimension(KVERTM,KSD),      intent(out) :: ext_lev, ssa_lev, sca_lev
      real(sp),dimension(KMpar,KVERTM,KSD),intent(out) :: f11_lev, f12_lev, f22_lev, &
                                                       f33_lev, f34_lev, f44_lev  ! sca matrix
! ..................................................................................
! OUT:
      integer,                        intent(out) ::  NANG
      real(sp),dimension(KMpar),      intent(out) ::  ANGL

      real(sp),dimension(KSD),        intent(out) :: ext_norm
! ..................................................................................
! INOUT:
      type(kernels_triangle_bin), intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),intent(inout)  ::  KERNELS2
! ..................................................................................
! LOCAL:
      type(output_pixel_opt_wl)       :: opt_pixel_wl
      type(output_pixel_ph_matrix_wl) :: phmx_pixel_wl
      integer,dimension(KSD) :: NBIN_tmp
      real(sp),dimension(KIDIM3,KSD) :: RADIUS_tmp, SD_tmp
      real(sp),dimension(KSHAPE,KSD) :: SHD_tmp
      real(sp),dimension(KSD) :: RREAL_tmp, RIMAG_tmp
      real(sp),dimension(KSD) :: ext_norm_tmp
      integer,dimension(KSD)  :: KNLN_tmp
      integer :: nm, nwl, nrh, ntrc, nlev, nel
      integer :: i, j, iel, itrc, ilev
      real(dp),dimension(KVERTM) :: rh
      real(sp) :: cv, scaler, fd, reff, reri_wtr, imri_wtr
      integer :: nwl_wtr
      character(len=5) :: trc_type
      real(sp) :: reff_dry
      real(sp) :: sca
      real(sp), parameter :: tiny_hgfactor = 0.001

      real(sp),dimension(KVERTM) :: dh
      integer :: iu
      character(len=GBL_FILE_PATH_LEN) :: path, full_path

      real(sp) :: cn, lnsigma, rn
      
! ..................................................................................

      ext_lev(:,:) = 0._sp
      ssa_lev(:,:) = 0._sp
      sca_lev(:,:) = 0._sp
      f11_lev(:,:,:) = 0._sp
      f12_lev(:,:,:) = 0._sp
      f22_lev(:,:,:) = 0._sp
      f33_lev(:,:,:) = 0._sp
      f34_lev(:,:,:) = 0._sp
      f44_lev(:,:,:) = 0._sp

      nel = DLSF%keyEL

      ntrc = TM%ntrc
      nlev = TM%nlev


! Single scattering properties for hydrophobic tracers
loop_hpho: do itrc=1,ntrc
      do iel=0,nel
      select case ( iel )
      case ( 0 )
      opt_pixel_wl%ext(1) = 0.0_sp
      opt_pixel_wl%aext(1) = 0.0_sp
      opt_pixel_wl%ssa(1) = 0.0_sp
      case ( 1 )
        phmx_pixel_wl%ph11(:,1) = 0.0_sp
      case ( 2 )
        phmx_pixel_wl%ph12(:,1) = 0.0_sp
      case ( 3 )
        phmx_pixel_wl%ph22(:,1) = 0.0_sp
      case ( 4 )
        phmx_pixel_wl%ph33(:,1) = 0.0_sp
      case ( 5 )
        phmx_pixel_wl%ph34(:,1) = 0.0_sp
      case ( 6 )
        phmx_pixel_wl%ph44(:,1) = 0.0_sp
      end select
      enddo ! iel

        if ( TM%flag_hphi(itrc) .eq. hphi ) cycle
        NBIN_tmp(1) = NBIN(itrc)
        RADIUS_tmp(1:NBIN_tmp(1),1) = RADIUS(1:NBIN(itrc),itrc)
        SD_tmp(1:NBIN_tmp(1),1) = SD(1:NBIN(itrc),itrc)
        SD_tmp(3,1) = 1.0_sp
        SHD_tmp(1:NSHAPE(itrc),1) = SHD(1:NSHAPE(itrc),itrc)
        RREAL_tmp(1) = RREAL(itrc)
        RIMAG_tmp(1) = RIMAG(itrc)
        KNLN_tmp(1)  = KNLN(itrc)
!print *, NBIN_tmp(1),KNLN_tmp(1),'  - NBIN_tmp(1),KNLN_tmp(1)'
!print *, 'RADIUS_tmp'
!write(*,'(10es12.4)') RADIUS_tmp(1:NBIN_tmp(1),1)
!print *, 'SD_tmp'
!write(*,'(10es12.4)') SD_tmp(1:3,1)
!print *, 'SHD_tmp'
!write(*,'(10es12.4)') SHD_tmp(1:NSHAPE(itrc),1)
!print *,
!print *, RREAL_tmp(1),RIMAG_tmp(1),'  - RREAL_tmp(1),RIMAG_tmp(1)'

        call forw_phase_matrix (                               & ! IN
                                  iu_main_output,              &
                                  IPRI_verbose,                &
                                  IPRI_additional_info,        &
                                  1, NBIN_tmp, RADIUS_tmp, SD_tmp, &
                                  KNLN_tmp,                     &
                                  NSHAPE, RATIOS, SHD_tmp,      &
                                  IW,WAVE,RREAL_tmp, RIMAG_tmp, &
                                  use_models,                   &
                                  tiny_wl_models,               &
                                  NANG,ANGL,ipix,               & ! OUT
                                  opt_pixel_wl,                 &
                                  phmx_pixel_wl,                &
                                  ext_norm_tmp,                 &
                                  DLSF,KERNELS1,KERNELS2        &
                               )
        nm = NANG

        do iel=0,nel
        select case ( iel )
        case ( 0 )
          ext_lev(1:nlev,itrc) = opt_pixel_wl%ext(1) * TM%vp(1:nlev,itrc,ipix)
          ssa_lev(1:nlev,itrc) = opt_pixel_wl%ssa(1)
          sca_lev(1:nlev,itrc) = ext_lev(1:nlev,itrc) * ssa_lev(1:nlev,itrc)
        case ( 1 )
          do ilev=1,nlev
          f11_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph11(1:nm,1) * TM%vp(ilev,itrc,ipix)
          enddo
        case ( 2 )
          do ilev=1,nlev
          f12_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph12(1:nm,1) * TM%vp(ilev,itrc,ipix)
          enddo
        case ( 3 )
          do ilev=1,nlev
          f22_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph22(1:nm,1) * TM%vp(ilev,itrc,ipix)
          enddo
        case ( 4 )
          do ilev=1,nlev
          f33_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph33(1:nm,1) * TM%vp(ilev,itrc,ipix)
          enddo
        case ( 5 )
          do ilev=1,nlev
          f34_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph34(1:nm,1) * TM%vp(ilev,itrc,ipix)
          enddo
        case ( 6 )
          do ilev=1,nlev
          f44_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph44(1:nm,1) * TM%vp(ilev,itrc,ipix)
          enddo
        end select
        enddo ! iel

goto 1
! print into external files for testing single scattering properties
if (IW .eq. 4) then
if( itrc .ge. 10 .and. itrc .le. 14 ) then
path = '/Users/lapionak/MyCodes/grasp/home/test-transport/Christian-S7-MERRA/pixels-11/'// &
'pixels_aerosol_cases_v1/output-2021-06-28-phmx-dry-ss-new/'
full_path = trim(path)//'phmx-'//trim(TM%trcs(itrc))//'.txt'
!write(*,*) 'full_path = ',trim(full_path)
open (newunit=iu,file=trim(full_path),status='replace')
!write(*,*) 'full_path = ',trim(full_path)
  write(iu,'(2a,2(2x,a,es11.4),2x,a,i0)') 'tracer = ',trim(TM%trcs(itrc)), &
  'rmin =',RADIUS_tmp(1,1), &
  'rmax =',RADIUS_tmp(NBIN_tmp(1),1), &
  'nbins = ',KNLN_tmp(1)
  write(iu,'(a,3es12.4)') 'dv/dlnr: rv, lnsigma, cv - ', SD_tmp(1:3,1)
  write(iu,'(a,2es12.4)') 'ratios:  ', ratios(1:NSHAPE(itrc),1)
  write(iu,'(a,2es12.4)') 'shd   :  ',SHD_tmp(1:NSHAPE(itrc),1)
  write(iu,'(2(a,f8.4,2x),a,es11.4)') 'WL =',WAVE,'n =',RREAL_tmp(1),'k =',RIMAG_tmp(1)
  sca = ext_lev(1,itrc) * ssa_lev(1,itrc)
  write(iu,'(3(a,es11.4,2x))') 'ext =',ext_lev(1,itrc),'sca =',sca,'ssa =',ssa_lev(1,itrc)
  write(iu,'(a)') 'Phase matrix: '
  write(iu,'(a3,a10,4a12)') '#','sca_ang','ph11','ph12','ph22','ph33'
  do i=1,nm
  write(iu,'(i3,f10.2,4es12.4)') i, ANGL(i), &
  f11_lev(i,1,itrc)/sca, f12_lev(i,1,itrc)/sca, &
  f22_lev(i,1,itrc)/sca, f33_lev(i,1,itrc)/sca
  enddo
close(iu)
endif
endif
1 continue

enddo loop_hpho
!if (iw .eq. 4) stop 'print phase matrix in tracer_level_sca_matrix'

! Single scattering properties for hydrophilic tracers
loop_hphi: do itrc=1,ntrc
        if ( TM%flag_hphi(itrc) .eq. hpho ) cycle

        trc_type = TM%trcs(itrc)
        reff_dry = reff_lognormal_sd( 3, exp(SD(2,itrc)), SD(1,itrc) ) ! id_sd, sigma, rm
!print *
!print *,'itrc =',itrc,'  SD(2,itrc) =',SD(2,itrc),'  SD(1,itrc) =',SD(1,itrc)

        do ilev=1,nlev
          NBIN_tmp(1) = NBIN(itrc)
          RADIUS_tmp(1,1) = RADIUS(1,itrc)
          RADIUS_tmp(NBIN_tmp(1),1) = RADIUS(NBIN(itrc),itrc)
          SD_tmp(2,1) = SD(2,itrc)
          SD_tmp(3,1) = 1.0_sp
          SHD_tmp(1:NSHAPE(itrc),1) = SHD(1:NSHAPE(itrc),itrc)
          reff = reff_dry * TM%hgfactor(ilev,itrc,ipix)
          SD_tmp(1,1) = rmean_lognormal_sd( 3, exp(SD(2,itrc)), reff )
!print *,'ilev =',ilev,'  TM%hgfactor(ilev,itrc,ipix) =',TM%hgfactor(ilev,itrc,ipix), &
!'  TM%rh(ilev,ipix) =',TM%rh(ilev,ipix)
!print *,'itrc =',itrc,'  SD(2,itrc) =',SD_tmp(2,1),'  SD(1,itrc) =',SD_tmp(1,1), &
!'  SD(3,itrc) =',SD_tmp(3,1),'  reff_wet =',reff,'  reff_dry =',reff_dry

          nwl_wtr = TM%water%nwl
          reri_wtr = linear(TM%water%wl(1:nwl_wtr), TM%water%reri(1:nwl_wtr), nwl_wtr, wave)
          imri_wtr = linear(TM%water%wl(1:nwl_wtr), TM%water%imri(1:nwl_wtr), nwl_wtr, wave)
          !fd = 1.0 ! - volume fraction of dry aerosol
          !fd = ( reff_dry*reff_dry*reff_dry ) / ( reff*reff*reff )
          scaler = TM%hgfactor(ilev,itrc,ipix) * TM%hgfactor(ilev,itrc,ipix) * TM%hgfactor(ilev,itrc,ipix)
          fd = 1.0_sp/scaler
          if ( fd .gt. 1.0_sp ) fd = 1.0_sp
          RREAL_tmp(1) = rh_modified_particle_characteristic( RREAL(itrc), reri_wtr, fd )
          RIMAG_tmp(1) = rh_modified_particle_characteristic( RIMAG(itrc), imri_wtr, fd )
          RADIUS_tmp(NBIN_tmp(1),1) = RADIUS(NBIN(itrc),itrc) * TM%hgfactor(ilev,itrc,ipix)
          !if ( RADIUS_tmp(NBIN_tmp(1),1) .gt. 33.88 ) then
          if ( RADIUS_tmp(NBIN_tmp(1),1)/WAVE .gt. 33.88_sp/0.34_sp ) then
          write(*,'(2(a,i0,2x),4(a,es11.4,2x))') 'itrc = ',itrc,'ilev = ',ilev, &
          'wl =',WAVE, &
          'rmax_dry =',RADIUS(NBIN(itrc),itrc), &
          'rmax_wet =',RADIUS_tmp(NBIN_tmp(1),1), &
          'rh =',TM%rh(ilev,ipix), &
          'hgfactor =',TM%hgfactor(ilev,itrc,ipix)
          stop 'stop in column_average_wl'
          endif

          KNLN_tmp(1) = KNLN(itrc)

          call forw_phase_matrix (                           &
                                    iu_main_output,          & ! IN
                                    IPRI_verbose,            &
                                    IPRI_additional_info,    &
                                    1, NBIN_tmp, RADIUS_tmp, SD_tmp, & ! IN
                                    KNLN_tmp,                     &
                                    NSHAPE, RATIOS, SHD_tmp,      &
                                    IW,WAVE,RREAL_tmp, RIMAG_tmp, &
                                    use_models,                   &
                                    tiny_wl_models,               &
                                    NANG,ANGL,ipix,               & ! OUT
                                    opt_pixel_wl,                 &
                                    phmx_pixel_wl,                &
                                    ext_norm_tmp,                 &
                                    DLSF,KERNELS1,KERNELS2        &
                                )
          nm = NANG

goto 2
if (iw .eq. 4 ) then
goto 3
if (itrc .eq. 15 .and. ilev .eq. 68 ) then
    call convert_lognormal_sd_parameters( 3, 1., SD_tmp(2,1), SD_tmp(1,1), &
                                          0, cn, lnsigma, rn )
write(*,'(a,2x,2(a,i0,2x),a,es11.4)') TM%trcs(itrc),'itrc = ',itrc,'IW = ',iw,'WAVE =',WAVE

write(*,'(a,i0,2(2x,a,es11.4))') 'ilev = ',ilev,'rh =',TM%rh(ilev,ipix), &
                                 'hgf =',TM%hgfactor(ilev,itrc,ipix)

write(*,'(3(a,es11.4,2x))') 'fd =',fd,'reff_dry =',reff_dry,'reff_wet =',reff

write(iu,'(a,2es12.4)') 'ratios:  ',ratios(1:NSHAPE(itrc),itrc)
write(iu,'(a,2es12.4)') 'shd   :  ',SHD_tmp(1:NSHAPE(itrc),1)

write(*,'(a,i0)') 'KNLN_tmp = ',KNLN_tmp(1)
write(*,'(2(a,es11.4,2x))') 'rmin_dry =',RADIUS(1,itrc), &
                            'rmax_dry =',RADIUS(NBIN_tmp(1),itrc)
write(*,'(2(a,es11.4,2x))') 'rmin_wet =',RADIUS_tmp(1,1), &
                            'rmax_wet =',RADIUS_tmp(NBIN_tmp(1),1)
write(*,'(2(a,es11.4,2x))') 'RREAL_dry(itrc)  =',RREAL(itrc), 'RIMAG_dry(itrc)  =',RIMAG(itrc)
write(*,'(2(a,es11.4,2x))') 'RREAL_wet(1) =',RREAL_tmp(1),'RIMAG_wet(1) =',RIMAG_tmp(1)

write(*,'(a)') 'SD_wet (number)'
write(*,'(3es12.4,a)') rn,lnsigma,cn,'  - rn_wet, ln(sigma), cn'
write(*,'(a)') 'SD_wet(1:3,1)'
write(*,'(3es12.4,a)') SD_tmp(1:3,1),'  - rv_wet, ln(sigma), cv'

write(*,'(a)') 'SD_dry(1:3,itrc)'
write(*,'(3es12.4,a)') SD(1:3,itrc),'  - rv_dry, ln(sigma), cv'
write(*,'(2(a,es11.4,2x))') 'mixing ratio =',TM%c_dry(ilev,itrc,ipix), &
                            'cv =',TM%vp(ilev,itrc,ipix)

write(*,'(2(a,es11.4,2x))') 'cv=1: ext_wet =',opt_pixel_wl%ext(1)*1e-12, &
                            'ssa_wet =',opt_pixel_wl%ssa(1)

write(*,'(a,es11.4)') 'cn=1: ext_wet =',opt_pixel_wl%ext(1)/cn*1e-12


stop 'stop test in column_average_wl'
endif
3 continue


if (itrc .eq. 15 ) then
    call convert_lognormal_sd_parameters( 3, 1., SD_tmp(2,1), SD_tmp(1,1), &
                                          0, cn, lnsigma, rn )

if (ilev .eq. 1) then
write(*,'(a5,6a12)') 'ilev','Height','RH','hgfactor','CV','ext_cv=1','ext_cn=1'
endif
write(*,'(i5,6es12.4)') ilev,TM%h(ilev,ipix),TM%rh(ilev,ipix), &
             TM%hgfactor(ilev,itrc,ipix), &
             TM%vp(ilev,itrc,ipix), &
              opt_pixel_wl%ext(1)*1e-12, &
              opt_pixel_wl%ext(1)/cn*1e-12
endif ! itrc .eq. 15

endif ! iw .eq. 4
2 continue

          !TM%vp = TM%c_dry * hgfactor^3 / TM%density_dry(itrc) - done in unpack_TMC routine (unpack_par_vect_ap.f90)
          !TM%vp = TM%vp * 1e+9 - convertion of units done in unpack_TMC routine (unpack_par_vect_ap.f90)
          cv = TM%vp(ilev,itrc,ipix)

          do iel=0,nel
            select case ( iel )
            case ( 0 )
              ext_lev(ilev,itrc) = opt_pixel_wl%ext(1) * cv
              sca_lev(ilev,itrc) = opt_pixel_wl%ext(1) * &
                                   opt_pixel_wl%ssa(1) * cv
!write(*,*) itrc,ilev, cv,GOUT_particles_opt_pixel_wl_tmp%ext(1),GOUT_particles_opt_pixel_wl_tmp%ssa(1), &
!'  - itrc,ilev, cv,GOUT_particles_opt_pixel_wl_tmp%ext(1),GOUT_particles_opt_pixel_wl_tmp%ssa(1)'
!if (itrc .eq. 10. .and. ilev .eq. 60 ) stop 222
            case ( 1 )
              f11_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph11(1:nm,1) * cv
            case ( 2 )
              f12_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph12(1:nm,1) * cv
            case ( 3 )
              f22_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph22(1:nm,1) * cv
            case ( 4 )
              f33_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph33(1:nm,1) * cv
            case ( 5 )
              f34_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph34(1:nm,1) * cv
            case ( 6 )
              f44_lev(1:nm,ilev,itrc) = phmx_pixel_wl%ph44(1:nm,1) * cv
            end select
          enddo ! iel
        enddo ! ilev

enddo loop_hphi


!write(*,'(a)') 'ext in column_average_wl:'
!do itrc=1,ntrc
!write(*,'(2x,2i5,es12.4)') iw,itrc,GOUT_particles_opt_pixel_wl%ext(itrc)
!enddo
!write(*,'(a)') 'ssa in column_average_wl:'
!do itrc=1,ntrc
!write(*,'(2x,2i5,es12.4)') iw,itrc,GOUT_particles_opt_pixel_wl%ssa(itrc)
!enddo
!stop 777

!if (iw .eq. 4) stop 'print phase matrix in column_average_wl'

      return
      end subroutine tracer_level_sca_matrix

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine returns column average single scattering properties
        !> @brief for every tracer
        !> 

      subroutine column_average_sca_matrix (                       &
                                    IPRI_verbose,                 &
                                    nel, nang, ipix,              &
                                    ext_lev, ssa_lev, sca_lev,    &
                                    f11_lev, f12_lev, f22_lev,     &
                                    f33_lev, f34_lev, f44_lev,     &
                                    GOUT_particles_opt_pixel_wl,  &
                                    GOUT_particles_phmx_pixel_wl  &
                                          )

      use mod_globals, only : sp
      use mod_par_inv, only : KVERTM
      use mod_par_OS,  only : KSD
      use mod_par_DLS, only : KMpar
      use mod_retr_general_output_derived_type
      use mod_derivative_type_transport_model, only : TM
      use mod_stop_report

      implicit none
! ..................................................................................
! IN:
      logical, intent(in) :: IPRI_verbose
      integer, intent(in) :: nel, nang, ipix
      real(sp),dimension(KVERTM,KSD), intent(in) :: ext_lev, ssa_lev, sca_lev
      real(sp),dimension(KMpar,KVERTM,KSD), intent(in) :: f11_lev, f12_lev, &
                                                          f22_lev, f33_lev, &
                                                          f34_lev, f44_lev  ! sca matrix
! ..................................................................................
! OUT:
      type(output_pixel_opt_wl),      intent(inout)  ::  GOUT_particles_opt_pixel_wl
      type(output_pixel_ph_matrix_wl),intent(inout)  ::  GOUT_particles_phmx_pixel_wl
! ..................................................................................
! LOCAL:
      integer :: ntrc, nlev
      integer :: iel, itrc, ilev
      real(sp),dimension(KVERTM) :: dh
      real(sp) :: sca
! ..................................................................................

      ntrc = TM%ntrc
      nlev = TM%nlev

      do iel=0,nel
      select case ( iel )
      case ( 0 )
      GOUT_particles_opt_pixel_wl%ext(:) = 0.0_sp
      GOUT_particles_opt_pixel_wl%aext(:) = 0.0_sp
      GOUT_particles_opt_pixel_wl%ssa(:) = 0.0_sp
      case ( 1 )
        GOUT_particles_phmx_pixel_wl%ph11(:,:) = 0.0_sp
      case ( 2 )
        GOUT_particles_phmx_pixel_wl%ph12(:,:) = 0.0_sp
      case ( 3 )
        GOUT_particles_phmx_pixel_wl%ph22(:,:) = 0.0_sp
      case ( 4 )
        GOUT_particles_phmx_pixel_wl%ph33(:,:) = 0.0_sp
      case ( 5 )
        GOUT_particles_phmx_pixel_wl%ph34(:,:) = 0.0_sp
      case ( 6 )
        GOUT_particles_phmx_pixel_wl%ph44(:,:) = 0.0_sp
      end select
      enddo ! iel
      do ilev=2,nlev
        dh(ilev) = (TM%h(ilev-1,ipix)-TM%h(ilev,ipix)) * 0.001_sp ! 1e-3 convert m to km
      enddo ! ilev

loop_tracers: do itrc=1,ntrc

      do iel=0,nel
        select case ( iel )
        case ( 0 )
            sca = 0.0_sp
            do ilev=2,nlev
              GOUT_particles_opt_pixel_wl%ext(itrc) = &
              GOUT_particles_opt_pixel_wl%ext(itrc) + &
              0.5*dh(ilev)*(ext_lev(ilev-1,itrc)+ext_lev(ilev,itrc))
              sca = sca + &
              0.5_sp*dh(ilev)*(sca_lev(ilev-1,itrc)+sca_lev(ilev,itrc))
            enddo ! ilev
            GOUT_particles_opt_pixel_wl%aext(itrc) = &
            GOUT_particles_opt_pixel_wl%ext(itrc) - sca
            GOUT_particles_opt_pixel_wl%ssa(itrc) = &
            sca / GOUT_particles_opt_pixel_wl%ext(itrc)
        case ( 1 )
            do ilev=2,nlev
              GOUT_particles_phmx_pixel_wl%ph11(1:nang,itrc) = &
              GOUT_particles_phmx_pixel_wl%ph11(1:nang,itrc) + &
              0.5_sp*dh(ilev)*(f11_lev(1:nang,ilev-1,itrc)+f11_lev(1:nang,ilev,itrc))
            enddo ! ilev
        case ( 2 )
            do ilev=2,nlev
              GOUT_particles_phmx_pixel_wl%ph12(1:nang,itrc) = &
              GOUT_particles_phmx_pixel_wl%ph12(1:nang,itrc) + &
              0.5_sp*dh(ilev)*(f12_lev(1:nang,ilev-1,itrc)+f12_lev(1:nang,ilev,itrc))
            enddo ! ilev
        case ( 3 )
            do ilev=2,nlev
              GOUT_particles_phmx_pixel_wl%ph22(1:nang,itrc) = &
              GOUT_particles_phmx_pixel_wl%ph22(1:nang,itrc) + &
              0.5_sp*dh(ilev)*(f22_lev(1:nang,ilev-1,itrc)+f22_lev(1:nang,ilev,itrc))
            enddo ! ilev
        case ( 4 )
            do ilev=2,nlev
              GOUT_particles_phmx_pixel_wl%ph33(1:nang,itrc) = &
              GOUT_particles_phmx_pixel_wl%ph33(1:nang,itrc) + &
              0.5_sp*dh(ilev)*(f33_lev(1:nang,ilev-1,itrc)+f33_lev(1:nang,ilev,itrc))
            enddo ! ilev
        case ( 5 )
            do ilev=2,nlev
              GOUT_particles_phmx_pixel_wl%ph34(1:nang,itrc) = &
              GOUT_particles_phmx_pixel_wl%ph34(1:nang,itrc) + &
              0.5_sp*dh(ilev)*(f34_lev(1:nang,ilev-1,itrc)+f34_lev(1:nang,ilev,itrc))
            enddo ! ilev
        case ( 6 )
            do ilev=2,nlev
              GOUT_particles_phmx_pixel_wl%ph44(1:nang,itrc) = &
              GOUT_particles_phmx_pixel_wl%ph44(1:nang,itrc) + &
              0.5_sp*dh(ilev)*(f44_lev(1:nang,ilev-1,itrc)+f44_lev(1:nang,ilev,itrc))
            enddo ! ilev
        end select
      enddo ! iel 
enddo loop_tracers

      return
      end subroutine column_average_sca_matrix

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine tracer_average_sca_matrix (  nel, nang, ipix,           &
                                             ext_lev, ssa_lev, sca_lev, &
                                             f11_lev, f12_lev, f22_lev,  &
                                             f33_lev, f34_lev, f44_lev )

      use mod_globals, only : sp
      use mod_par_inv, only : KVERTM
      use mod_par_DLS, only : KMpar
      use mod_par_OS,  only : KSD
      use mod_derivative_type_transport_model, only : TM
      use mod_derivative_type_tracer_average, only : TRCAVG, initialize_tracer_average_sca_matrix
      use mod_stop_report

      implicit none
! ..................................................................................
! IN:
      integer, intent(in) :: nel, nang, ipix
      real(sp),dimension(KVERTM,KSD), intent(in) :: ext_lev, ssa_lev, sca_lev
      real(sp),dimension(KMpar,KVERTM,KSD), intent(in) :: f11_lev, f12_lev, &
                                                       f22_lev, f33_lev, &
                                                       f34_lev, f44_lev  ! sca matrix
! ..................................................................................
! LOCAL:
      integer :: nlev, ntrc
      integer :: j, iel, ilev, itrc
! ..................................................................................

      ntrc = TM%ntrc
      nlev = TM%nlev

! sum tracer single scattering characteristics at every level
      call initialize_tracer_average_sca_matrix ( TRCAVG )

      do iel=0,nel
        select case ( iel )
        case ( 0 )
          TRCAVG%ext(1) = sum( ext_lev(1,1:ntrc) ) * 0.5_sp*( TM%h(1,ipix)-TM%h(2,ipix) ) * 0.001_sp ! 0.001_sp converts m -> km 
          TRCAVG%sca(1) = sum( sca_lev(1,1:ntrc) ) * 0.5_sp*( TM%h(1,ipix)-TM%h(2,ipix) ) * 0.001_sp
          TRCAVG%ssa(1) = TRCAVG%sca(1) / TRCAVG%ext(1)
          do ilev=2,nlev-1
          TRCAVG%ext(ilev) = sum( ext_lev(ilev,1:ntrc) ) * 0.5_sp*( TM%h(ilev-1,ipix)-TM%h(ilev+1,ipix) ) * 0.001_sp
          TRCAVG%sca(ilev) = sum( sca_lev(ilev,1:ntrc) ) * 0.5_sp*( TM%h(ilev-1,ipix)-TM%h(ilev+1,ipix) ) * 0.001_sp
          TRCAVG%ssa(ilev) = TRCAVG%sca(ilev) / TRCAVG%ext(ilev)
          enddo
          TRCAVG%ext(nlev) = sum( ext_lev(nlev,1:ntrc) ) * 0.5_sp*( TM%h(nlev-1,ipix)-TM%h(nlev,ipix) ) * 0.001_sp
          TRCAVG%sca(nlev) = sum( sca_lev(nlev,1:ntrc) ) * 0.5_sp*( TM%h(nlev-1,ipix)-TM%h(nlev,ipix) ) * 0.001_sp
          TRCAVG%ssa(nlev) = TRCAVG%sca(nlev) / TRCAVG%ext(nlev)
        case ( 1 )
          do j=1,nang
          TRCAVG%ph11(j,1) = sum( f11_lev(j,1,1:ntrc) ) * 0.5_sp*( TM%h(1,ipix)-TM%h(2,ipix) ) * 0.001_sp
          enddo
          do ilev=2,nlev-1
          do j=1,nang
          TRCAVG%ph11(j,ilev) = sum( f11_lev(j,ilev,1:ntrc) ) * 0.5_sp*( TM%h(ilev-1,ipix)-TM%h(ilev+1,ipix) ) * 0.001_sp
          enddo
          enddo
          do j=1,nang
          TRCAVG%ph11(j,nlev) = sum( f11_lev(j,nlev,1:ntrc) ) * 0.5_sp*( TM%h(nlev-1,ipix)-TM%h(nlev,ipix) ) * 0.001_sp
          enddo
        case ( 2 )
          do j=1,nang
          TRCAVG%ph12(j,1) = sum( f12_lev(j,1,1:ntrc) ) * 0.5_sp*( TM%h(1,ipix)-TM%h(2,ipix) ) * 0.001_sp
          enddo
          do ilev=2,nlev-1
          do j=1,nang
          TRCAVG%ph12(j,ilev) = sum( f12_lev(j,ilev,1:ntrc) ) * 0.5_sp*( TM%h(ilev-1,ipix)-TM%h(ilev+1,ipix) ) * 0.001_sp
          enddo
          enddo
          do j=1,nang
          TRCAVG%ph12(j,nlev) = sum( f12_lev(j,nlev,1:ntrc) ) * 0.5_sp*( TM%h(nlev-1,ipix)-TM%h(nlev,ipix) ) * 0.001_sp
          enddo
        case ( 3 )
          do j=1,nang
          TRCAVG%ph22(j,1) = sum( f22_lev(j,1,1:ntrc) ) * 0.5_sp*( TM%h(1,ipix)-TM%h(2,ipix) ) * 0.001_sp
          enddo
          do ilev=2,nlev-1
          do j=1,nang
          TRCAVG%ph22(j,ilev) = sum( f22_lev(j,ilev,1:ntrc) ) * 0.5_sp*( TM%h(ilev-1,ipix)-TM%h(ilev+1,ipix) ) * 0.001_sp
          enddo
          enddo
          do j=1,nang
          TRCAVG%ph22(j,nlev) = sum( f22_lev(j,nlev,1:ntrc) ) * 0.5_sp*( TM%h(nlev-1,ipix)-TM%h(nlev,ipix) ) * 0.001_sp
          enddo
        case ( 4 )
          do j=1,nang
          TRCAVG%ph33(j,1) = sum( f33_lev(j,1,1:ntrc) ) * 0.5_sp*( TM%h(1,ipix)-TM%h(2,ipix) ) * 0.001_sp
          enddo
          do ilev=2,nlev-1
          do j=1,nang
          TRCAVG%ph33(j,ilev) = sum( f33_lev(j,ilev,1:ntrc) ) * 0.5_sp*( TM%h(ilev-1,ipix)-TM%h(ilev+1,ipix) ) * 0.001_sp
          enddo
          enddo
          do j=1,nang
          TRCAVG%ph33(j,nlev) = sum( f33_lev(j,nlev,1:ntrc) ) * 0.5_sp*( TM%h(nlev-1,ipix)-TM%h(nlev,ipix) ) * 0.001_sp
          enddo
        case ( 5 )
          do j=1,nang
          TRCAVG%ph34(j,1) = sum( f34_lev(j,1,1:ntrc) ) * 0.5_sp*( TM%h(1,ipix)-TM%h(2,ipix) ) * 0.001_sp
          enddo
          do ilev=2,nlev-1
          do j=1,nang
          TRCAVG%ph34(j,ilev) = sum( f34_lev(j,ilev,1:ntrc) ) * 0.5_sp*( TM%h(ilev-1,ipix)-TM%h(ilev+1,ipix) ) * 0.001_sp
          enddo
          enddo
          do j=1,nang
          TRCAVG%ph34(j,nlev) = sum( f34_lev(j,nlev,1:ntrc) ) * 0.5_sp*( TM%h(nlev-1,ipix)-TM%h(nlev,ipix) ) * 0.001_sp
          enddo
        case ( 6 )
          do j=1,nang
          TRCAVG%ph44(j,1) = sum( f44_lev(j,1,1:ntrc) ) * 0.5_sp*( TM%h(1,ipix)-TM%h(2,ipix) ) * 0.001_sp
          enddo
          do ilev=2,nlev-1
          do j=1,nang
          TRCAVG%ph44(j,ilev) = sum( f44_lev(j,ilev,1:ntrc) ) * 0.5_sp*( TM%h(ilev-1,ipix)-TM%h(ilev+1,ipix) ) * 0.001_sp
          enddo
          enddo
          do j=1,nang
          TRCAVG%ph44(j,nlev) = sum( f44_lev(j,nlev,1:ntrc) ) * 0.5_sp*( TM%h(nlev-1,ipix)-TM%h(nlev,ipix) ) * 0.001_sp
          enddo
        end select
      enddo ! iel

      TRCAVG%nlev = nlev

      return
      end subroutine tracer_average_sca_matrix

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine print_tracer_average_sca_matrix ( nel, ipix,  &
                                                  iw, wl,     &
                                                  nang, angles )

      use mod_globals, only : sp, GBL_FILE_PATH_LEN
      use mod_derivative_type_transport_model, only : TM
      use mod_derivative_type_tracer_average, only : TRCAVG
      use mod_stop_report

      implicit none
! ..................................................................................
      integer, intent(in) :: nel, ipix, nang, iw
      real(sp), intent(in) :: wl
      real(sp), dimension(nang), intent(in) :: angles
! ..................................................................................
      character(len=GBL_FILE_PATH_LEN) :: path, full_path
      integer :: iu
      integer :: j, ilev
      logical :: phmx_dump = .false.
! ..................................................................................
      path = trim(TM%tracer_average_phmx_file_path)

      if ( phmx_dump ) then
        full_path = trim(path)//'phmx-tracer-average'//'.txt'
        !write(*,*) 'full_path = ',trim(full_path)
        if ( ipix .eq. 1 .and. iw .eq. 1 ) then
        open (newunit=iu,file=trim(full_path),status='replace')
        else
        open (newunit=iu,file=trim(full_path),access='append',status='old')
        endif
        write(iu,'(2(a,i0,2x),a,f8.4)') 'ipix = ',ipix,'iw = ',iw,'wl =',wl
        do ilev=1,TRCAVG%nlev
        write(iu,'(a,i0,2x,a,es11.4)') 'ilev = ',ilev,'h_m =',TM%h(ilev,ipix)
        write(iu,'(3(a,es11.4,2x))') 'ext =',TRCAVG%ext(ilev), &
                                  'sca =',TRCAVG%sca(ilev), &
                                  'ssa =',TRCAVG%ssa(ilev)
        write(iu,'(a)') 'Phase matrix: '
        write(iu,'(a3,a10,4a12)') '#','sca_ang','ph11','ph12','ph22','ph33'
        do j=1,nang
        write(iu,'(i3,f10.2,4es12.4)') j, angles(j), &
                                  TRCAVG%ph11(j,ilev)/TRCAVG%sca(ilev), &
                                  TRCAVG%ph12(j,ilev)/TRCAVG%sca(ilev), &
                                  TRCAVG%ph22(j,ilev)/TRCAVG%sca(ilev), &
                                  TRCAVG%ph33(j,ilev)/TRCAVG%sca(ilev)
        enddo ! j
        enddo ! ilev
        close ( iu )
      endif

      if ( phmx_dump ) then
        full_path = trim(path)//'phmx-tracer-average-test.txt'
        write(*,*) 'full_path = ',trim(full_path)
        if ( ipix .eq. 1 .and. iw .eq. 1 ) then
        open (newunit=iu,file=trim(full_path),status='replace')
        else
        open (newunit=iu,file=trim(full_path),access='append',status='old')
        endif
        write(iu,'(2(a,i0,2x),a,f8.4)') 'ipix = ',ipix,'iw = ',iw,'wl =',wl
        write(iu,'(a4,11(a12))') 'ilev','h_m','rh%','ext','sca', &
        'p11_0.0','p11_30.0','p11_60.0','p11_90.0','p11_120.0','p11_150.0','p11_180.0'
        do ilev=1,TRCAVG%nlev
        write(iu,'(i4,11(es12.4))') ilev,TM%h(ilev,ipix),TM%rh(ilev,ipix), &
                                      TRCAVG%ext(ilev),TRCAVG%sca(ilev), &
                                      TRCAVG%ph11(1,ilev)/TRCAVG%sca(ilev), &
                                      TRCAVG%ph11(31,ilev)/TRCAVG%sca(ilev)!, &
                                      !TRCAVG%ph11(61,ilev)/TRCAVG%sca(ilev), &
                                      !TRCAVG%ph11(91,ilev)/TRCAVG%sca(ilev), &
                                      !TRCAVG%ph11(121,ilev)/TRCAVG%sca(ilev), &
                                      !TRCAVG%ph11(151,ilev)/TRCAVG%sca(ilev), &
                                      !TRCAVG%ph11(181,ilev)/TRCAVG%sca(ilev)
        enddo
        close ( iu )
      endif

      return
      end subroutine print_tracer_average_sca_matrix

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
