! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **
        !> @file forw_lidar_garrlic.f90
        !> File contains soubroutine lidar_garrlic to simulate different types of lidar measurements
        !>
#include "../../../constants_set/mod_globals.inc"
      subroutine lidar_garrlic (                &
                                 HOBS_km,       & !> @param[in] HOBS_km – altitude of obserwation above sea level in km (not in actual use)
                                 HGR_km,        & !> @param[in] HGR_km – ground level altitude above sea level in km
                                 HMAX_atm_km,   & !> @param[in] HMAX_atm_km – maximum atmosphere altitude above sea level in km
                                 NHVP_fit,      & !> @param[in] NHVP_fit - Number of Heights in lidar Vertical Profile
                                 HVP_fit_km,    & !> @param[in] HVP_fit_km(KVERTM) – altitudes of layers of lidar vertical profiles in descending order
                                 NHVP_retr,     & !> @param[in] NHVP_retr - Number of Heights in retrieved aerosol Vertical Profile
                                 HVP_retr_km,   & !> @param[in] HVP_retr_km(KVERTM) – altitudes of layers of retrieved aerosol vertical profiles in descending order
                                 AVP_retr,      & !> @param[in] APV_retr(KSD,KVERTM) - retrieved Aerosol Vertival Profile
                                 CL,            & !> @param[in] CL – calibration coefficient for given wavelength
                                 MDPR_wl,       & !> @param[in] MDPR_wl – molecular depolarization ratio for current channel, modified by Qiaoyun HU, 2017/03/13
                                 NSD,           & !> @param[in] NSD - number of aerosol fractions
                                 EXTA,          & !> @param[in] EXTA(KSD) — aerosol extinction of aerosol fractions at given wavelenght in 1/km
                                 LRA,           & !> @param[in] LRA(KSD) - Lidar Ratio of Aerosol fractions at given wavelenth in Sr
                                 EXTM,          & !> @param[in] EXTM - molecular extinction for given vawelength in 1/km
                                 MPROF,         & !> @param[in] MPROF molecular vertical profile
                                 mol_prof_type, & !> @param[in] mol_prof_type — selector of the shape of molecular profile
                                 aer_prof_type, & !> @param[in] aer_prof_type — selector defining the type of aerosol profile (LUT, exponential, gaussian etc)
                                 meas_type,     & !> @param[in] meas_type — type of the lidar measurement (elastic, raman, polarised, etc.)
                                 EXTA_I,        & !> @param[in] EXTA_I(KSD) — aerosol extinction of aerosol fractions at initial wavelenght lidar pulse before RAMAN shift in 1/km (RAMAN lidars only)
                                 EXTM_I,        & !> @param[in] EXTM_I — molecular extinction at initial wavelenght lidar pulse before RAMAN shift in 1/km (RAMAN lidars only)
                                 DR_PAR,        & !> @param[in] DR_PAR(KSD) — lidar ratio for aerosol fractions for parallel polarisation (in relation to initial pulse) at given wavelenght (polarisation lidars only)
                                 DR_PER,        & !> @param[in] DR_PER(KSD) — lidar ratio for aerosol fractions for perpendicular polarisation (in relation to initial pulse) at given wavelenght (polarisation lidars only)
                                 MU,            & !> @param[in] MU – crosstalk between receivers of parallel and perpendicular polarized channels (polarisation lidars only)
                                 meas           & !> @param[out] meas(KVERTM) - ouput vector containing lidar measurement calculaions
                                )

!> @brief Forward lidar problem for Single pixel and Single wavelength for different types of lidars
!> @brief with Molecular correction (extinction and backscatter)
!> @brief function performs all necessary normalizations and data preparations
!> @brief and selects soubroutine for lidar signal calculation corresponding to the given type
!>

      use mod_par_inv, only : KVERTM, KNBVM ! KVERTM = KVERTM + 2
      use mod_par_OS,  only : KSD, NBVM
      use mod_sdata,   only : meas_type_LS, meas_type_RL, meas_type_DPAR, meas_type_DPER, meas_type_DP, meas_type_VEXT, meas_type_VBS
      use mod_molecular_scattering, only : std_atm_density
      USE MOD_RT_SOS
      use mod_intrpl_linear
      use mod_stop_report

      implicit none
!-----------------------------------------------------------------------------------------
! IN
      real,                       intent(in)    :: HOBS_km
      real,                       intent(in)    :: HGR_km
      real,                       intent(in)    :: HMAX_atm_km
      integer,                    intent(in)    :: NHVP_fit
      integer,                    intent(in)    :: NHVP_retr
      real,dimension(KVERTM),     intent(in)    :: HVP_fit_km
      real,dimension(KVERTM),     intent(in)    :: MPROF
      real,dimension(KVERTM),     intent(in)    :: HVP_retr_km
      real,dimension(KVERTM,KSD), intent(in)    :: AVP_retr
      real,                       intent(in)    :: CL
      real,                       intent(in)    :: MDPR_wl ! modified by Qiaoyun HU, 2017-03-13
!      real,dimension(KSD),        intent(in)    :: A_conc ! added by QiaoyunHU, 2017-01-31
      real,                       intent(in)    :: EXTM
      real,                       intent(in)    :: EXTM_I
      real,                       intent(in)    :: MU
      real,dimension(KSD),        intent(in)    :: EXTA
      real,dimension(KSD),        intent(in)    :: EXTA_I
      real,dimension(KSD),        intent(in)    :: LRA
      real,dimension(KSD),        intent(in)    :: DR_PER
      real,dimension(KSD),        intent(in)    :: DR_PAR
      integer,                    intent(in)    :: NSD
      integer,                    intent(in)    :: meas_type
      integer,                    intent(in)    :: mol_prof_type
      integer,                    intent(in)    :: aer_prof_type
!-----------------------------------------------------------------------------------------
! OUT
      real,dimension(KVERTM),     intent(out)   :: meas
!-----------------------------------------------------------------------------------------
! LOCAL

      integer                    :: isd
      integer                    :: ivert
      integer                    :: NH
      real,dimension(KVERTM)     :: H_km
      real,dimension(KVERTM)     :: prof_temp
      real                       :: xnorm
      integer                    :: NH_temp
      real,dimension(KVERTM)     :: HVP_temp_dis_km  !QY, distance + ground point
      real,dimension(KVERTM)     :: HVP_temp_alt_km ! QY, altitude + ground point
      real,dimension(KVERTM,KSD) :: AVP_temp_norm
      integer                    :: IFMP1
      real,dimension(KVERTM)     :: MVP_norm
      real                       :: hm_km
      real                       :: sigma
      character(len=20)          :: distr_type
      logical                    :: lstop
      real,dimension(KVERTM)     :: meas_par
      integer                    :: IDIM2,IDIM3



!---------------
! NOTE: Altitude arrays are in descending order
!      write(*,*) 'in forw_lidar_garrlic:'
!      write(*,*) 'CL=', CL
!      write(*,*) 'EXTA=', EXTA
!      write(*,*) 'EXTA_I=', EXTA_I
!      write(*,*) 'EXTA=', EXTM
!      write(*,*) 'EXTA_I=', EXTM_I
!      write(*,*) 'LRA=', LRA
!      write(*,*) 'meas_type=', meas_type
!      write(*,*) 'NHVP_fit, NHVP_retr', NHVP_fit, NHVP_retr
!      write(*,*) 'NSD=',NSD
!      do isd=1,NSD
!         do ivert=1, NHVP_retr
!         write(*,*) 'isd, ivert, AVP_retr', isd, ivert, AVP_retr(ivert,isd)
!         enddo
!      enddo
!      do ivert=1, NHVP_retr
!         write(*,*) 'ivert, HVP_retr', ivert, HVP_retr_km(ivert)
!      enddo
!      stop


      if(NHVP_retr .lt. NHVP_fit) then
         write(tmp_message,'(2(a,i0),a)') &
         'NHVP_retr = ',NHVP_retr,' .lt. NHVP_fit = ',NHVP_fit,'  not supported'
         G_ERROR(trim(tmp_message))
      endif

      NH = 0
      H_km(:) = 0.0
      prof_temp(:) = 0.0
      xnorm = 0.0
      NH_temp = 0
      HVP_temp_dis_km(:) = 0.0
      HVP_temp_alt_km(:) = 0.0
      AVP_temp_norm(:,:) = 0.0
      hm_km = 0.0
      sigma = 0.0
      distr_type = ''

      !   Number of altitudes and altitude values for lidar signal computation
      !   include HGR_km altitude in array HVP_temp_km of altitudes for 
      !   normalization of vertical distribution (if it is not present
      !   in HVP_fit_km)
!      WRITE(*,*),'L144 forw_lidar_garrlic.f90, NHVP_fit, NHVP_retr=',NHVP_fit,NHVP_retr
!      WRITE(*,*),'L145 forw_lidar_garrlic.f90, HVP_fit_km(NHVP_fit), HGR_km=',HVP_fit_km(NHVP_fit),HGR_km
      NH_temp = NHVP_fit
      HVP_temp_dis_km(1:NHVP_fit) = HVP_fit_km(1:NHVP_fit)
      HVP_temp_alt_km(1:NHVP_fit) = HVP_retr_km(1:NHVP_fit)! QY test
!      if(HVP_fit_km(NHVP_fit) .ne. HGR_km) then
!AL   condition changed since direct comparison of real values isn't always correct, threshold is 0.5m
      if( ABS(HVP_fit_km(NHVP_fit)-HGR_km) .GT. 0.0005) then
        NH_temp = NH_temp + 1
        HVP_temp_dis_km(NHVP_fit+1) = 0
        HVP_temp_alt_km(NHVP_fit+1) = HGR_km ! QY test
      endif
!      write (*,*) 'in forw_lidar_garrlic.f90'
!      do ivert=1, NHVP_retr
!        WRITE(*,*)'ivert, HVP_fit, HVP_retr=',ivert, HVP_fit_km(ivert), HVP_retr_km(ivert)
!      enddo
!      stop
! Aerosol vertical distribution
      !   include HGR_km and HMAX_atm_km altitudes in array H_km of
      !   altitudes for normalization of vertical distribution (if they 
      !   are not present in HVP_fit_km)
 !     WRITE(*,*),'HGR_km=',HGR_km,'HVP_retr_km(NHVP_retr)=',HVP_retr_km(NHVP_retr),'HMAX_atm_km=',HMAX_atm_km
      call grid_altitudes_LUT_with_gas (                   &
                                HGR_km,                   &
                                HMAX_atm_km,              & ! IN
                                NHVP_retr,                &
                                HVP_retr_km(1:NHVP_retr), &
                                NH,                       & ! OUT
                                H_km(1:NHVP_retr+2)       &
                              )
      !   normalization of vertical distribution from ground (HGR_km) level
      !   to top of atmosphere (HMAX_atm_km)
!    write(*,*) 'NHVP_retr=', NHVP_retr
!      WRITE(*,*),'L168 forw_lidar_garrlic.f90, NH, N_km=',NH,H_km
      distr_type = 'lut'
      do isd=1,NSD
        call discrvd_single_atm_component (                            &
                                            distr_type,                &
                                            hm_km,                     &
                                            sigma,                     & ! IN
                                            NHVP_retr,                 &
                                            HVP_retr_km(1:NHVP_retr),  &
                                            AVP_retr(1:NHVP_retr,isd), &
                                            NH,                        &
                                            H_km(1:NH),                &
                                            xnorm,                     &
                                            prof_temp(1:NH)            & ! OUT
                                           )
        if ( error_present() ) return
        prof_temp(1:NH) = prof_temp(1:NH)/xnorm
!AL        prof_temp(1:NH) = prof_temp(1:NH)*EXTA(isd)/xnorm ! for testing purposes only
        !   normalized aerosol vertical distribution at altidudes of
        !   lidar signal measurements + at HGR_km

        !write(*,'(a,i0,a)') 'isd = ',isd,'  H_km:'
        !write(*,'(10e14.5)') H_km(1:NH)
        !write(*,'(a,i0,a)') 'isd = ',isd,'  prof_temp:'
        !write(*,'(10e14.5)') prof_temp(1:NH)

        do ivert=1,NH_temp

          AVP_temp_norm(ivert,isd) = LINEAR (                   &
                                              H_km(1:NH),       &
                                              prof_temp(1:NH),  &
                                              NH,               &
                                              HVP_temp_alt_km(ivert)&!!HVP_temp_km(ivert)
                                            )
!AL        write(*,*) HVP_temp_dis_km(ivert), H_km (ivert), prof_temp(ivert), AVP_temp_norm(ivert,isd)
        enddo ! ivert
        !write(*,*) isd,xnorm,'  - isd, xnorm'
        !write(*,'(a,i0,a)') 'isd = ',isd,'  HVP_temp_km:'
        !write(*,'(10f14.5)') HVP_temp_dis_km(1:NH_temp)
        !write(*,'(a,i0,a)') 'isd = ',isd,'  AVP_norm:'
        !write(*,'(10e14.5)') AVP_temp_norm(1:NH_temp,isd)
      enddo ! isd

! Molecular vertical distribution/profile
      !IFMP1 = IFMP
      !   TODO for IFMP1 = IFMP :
      !AL add extrapolation at ground level altitude of molecular profile loaded from SData
      !AL change discr_vertical_distribution_single to have a switch between nearest neighbour (hardcoded now)
      !AL and other extrapolation methods, for example linear
      IFMP1 = 0

      select case(IFMP1)
      case(1) ! from SDATA file
      ! NOTE: Development needed
        !   linear extrapolation of molecular backscatter profile provided
        !   with lidar signal measurements
        !MPROF(NHVP_fit+1) = MPROF(NHVP_fit) +   &
        !                    (HVP_retr_km(NHVP_fit+1)-HVP_retr_km(NHVP_fit)) /  &
        !                    (HVP_retr_km(NHVP_fit-1)-HVP_retr_km(NHVP_fit)) *  &
        !                    (MPROF(NHVP_fit-1)-MPROF(NHVP_fit))
      case(0) ! molecular scattering vertical distribution
        !   include HGR_km and HMAX_atm_km altitudes in array H_km of
        !   altitudes for normalization of vertical distribution (if they
        !   are not present in HVP_temp_km)
        call grid_altitudes_LUT_with_gas (                &
                                  HGR_km,                 &
                                  HMAX_atm_km,            & ! IN
                                  NHVP_retr,                &
                                  HVP_retr_km(1:NHVP_retr), &
                                  NH,                     &
                                  H_km(1:NH_temp+2)       & ! OUT
                                ) 
        select case(mol_prof_type)
        case(0)
          distr_type = 'exponential'
          hm_km = 8.0
        case(1)
          distr_type = 'stdatm'
        end select
        !   vertical distribution from ground (HGR_km) level
        !   to top of atmosphere (HMAX_atm_km)
        call discrvd_single_atm_component (                  &
                                            distr_type,      &
                                            hm_km,           &
                                            sigma,           & ! IN
                                            NH,              &
                                            H_km(1:NH),      &
                                            prof_temp(1:NH), &
                                            NH,              &
                                            H_km(1:NH),      &
                                            xnorm,           &
                                            prof_temp(1:NH)  & ! OUT
                                           )
        if ( error_present() ) return
        prof_temp(1:NH) = prof_temp(1:NH)/xnorm
        do ivert=1,NH_temp
          MVP_norm(ivert) = &
          LINEAR ( H_km(1:NH), prof_temp(1:NH), NH, HVP_temp_alt_km(ivert) )! HVP_temp_km(ivert)
        enddo ! ivert
        !write(*,*) 'xnorm =',xnorm
        !write(*,'(a)') 'HVP_temp_alt_km:'
        !write(*,'(10f14.5)') HVP_temp_alt_km(1:NH_temp)
        !write(*,'(a,i0,a)') 'MVP_norm:'
        !write(*,'(10e14.5)') MVP_norm(1:NH_temp)
      end select
!        WRITE(*,*),'meas_type=',meas_type,'meas_type_DP=',meas_type_DP
! Lidar signals computation
      meas(:)=0.0
      select case(meas_type)
      case(meas_type_LS)
        call lidar_signal_elastic (                &
                                    HOBS_km,       &
                                    HGR_km,        &
                                    NH_temp,       &
                                    HVP_temp_dis_km,&
                                    NSD,           &
                                    AVP_temp_norm, &
                                    EXTA,          &
                                    LRA,           &
                                    MVP_norm,      &
                                    EXTM,          &
                                    meas(:)        &
                                   )
        meas(1:NHVP_fit) = meas(1:NHVP_fit)*CL*0.001 ! units coefficient 0.001 meters to kilometers
        !do ivert=1,NHVP_fit
        !  write(*,*) ivert,HVP_fit_km(ivert),meas(ivert),'  ivert,HVP_fit_km, meas'
        !enddo

      case(meas_type_RL)
        call lidar_signal_raman (                &
                                  HOBS_km,       &
                                  HGR_km,        &
                                  NH_temp,       &
                                  HVP_temp_dis_km,   &
                                  NSD,           &
                                  AVP_temp_norm, &
                                  EXTA_I,        &
                                  EXTA,          &
                                  MVP_norm,      &
                                  EXTM_I,        &
                                  EXTM,          &
                                  meas(:)        &
                                )
        meas(1:NHVP_fit) = meas(1:NHVP_fit)*CL*0.001 ! units coefficient 0.001 meters to kilometers
!        write(*,*) 'in forw_lidar_garrlic: after_lidar_signal_raman'
!        do ivert=1,NHVP_fit
!          write(*,*) 'NH_temp(ivert),RL(ivert)', HVP_temp_km(ivert),meas(ivert)
!        enddo
!        stop
      case(meas_type_DPAR)
        call lidar_signal_elastic_parallel (                &
                                             HOBS_km,       &
                                             HGR_km,        &
                                             NH_temp,       &
                                             HVP_temp_dis_km,   &
                                             NSD,           &
                                             AVP_temp_norm, &
                                             EXTA,          &
                                             DR_PAR,        &
                                             MVP_norm,      &
                                             EXTM,          &
                                             meas(:)        &
                                            )

        meas(1:NHVP_fit) = meas(1:NHVP_fit)*CL*0.001 ! units coefficient 0.001 meters to kilometers

      case(meas_type_DPER)
        call lidar_signal_elastic_perpendicular (                &
                                                  HOBS_km,       &
                                                  HGR_km,        &
                                                  NH_temp,       &
                                                  HVP_temp_dis_km,   &
                                                  NSD,           &
                                                  AVP_temp_norm, &
                                                  EXTA,          &
                                                  DR_PAR,        &
                                                  DR_PER,        &
                                                  MU,            &
                                                  MVP_norm,      &
                                                  EXTM,          &
                                                  meas(:)        &
                                                )

        meas(1:NHVP_fit) = meas(1:NHVP_fit)*CL*0.001 ! units coefficient 0.001 meters to kilometers

      case(meas_type_DP)
!            call lidar_signal_elastic_DP(                   &
!                                            NH_temp,        &
!                                            HVP_temp_km,    &
!                                            NSD,            &
!                                            AVP_temp_norm,  &
!                                            A_conc,         &
!                                            DR_PAR,         &
!                                            DR_PER,         &
!                                            MVP_norm,       &
!                                            EXTM,           &
!                                            meas(:)         &
!                                            )
            call lidar_signal_elastic_VLDP(                 &
                                            NH_temp,        &
                                            HVP_temp_dis_km,&
                                            NSD,            &
                                            AVP_temp_norm,  &
                                            MDPR_wl,        &
                                            EXTA,           &
                                            LRA,            &
                                            DR_PAR,         &
                                            DR_PER,         &
                                            MVP_norm,       &
                                            EXTM,           &
                                            meas(:)         &
                                            )


 !       meas(1:NHVP_fit)=meas_par(1:NHVP_fit)/meas_par(1:NHVP_fit)*CL

      case(meas_type_VEXT)
        call vertical_extinction (                 &
  ! AL                                  HOBS_km,       &
                                    NH_temp,       &
                                    HVP_temp_dis_km,&
                                    NSD,           &
                                    AVP_temp_norm, &
                                    EXTA,          &
                                    meas(:)        &
                                  )

        meas(1:NHVP_fit) = meas(1:NHVP_fit)*0.001 ! units coefficient 0.001 meters to kilometers
      case(meas_type_VBS)
        call vertical_backscatter (                &
                                    NH_temp,       &
                                    HVP_temp_dis_km,   &
                                    NSD,           &
                                    AVP_temp_norm, &
                                    EXTA,          &
                                    LRA,           &
                                    meas(:)        &
                                  )

          if ( error_present() ) return
          meas(1:NHVP_fit) = meas(1:NHVP_fit)*0.001

      case default
         write(*,*) 'Error in lidar_garrlic: lidar measurement type unknown'
         write(*,*) 'meas_type=', meas_type
      end select
      return
      end subroutine lidar_garrlic

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss



