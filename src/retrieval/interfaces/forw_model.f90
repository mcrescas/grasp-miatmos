! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! file contains :
   
! subroutine forward_model_pixel_wl
! subroutine forward_model_pixel
! subroutine forw_phase_matrix
! subroutine forw_lidar_signal
! subroutine forw_radiative_transfer
! subroutine forward_model_pixel_PHMX
#include "../constants_set/mod_globals.inc"
      module mod_forward_model

      use mod_stop_report

      contains

      subroutine forward_model_pixel_wl (                           &
                                           iu_main_output,          & ! IN
                                           RIN,ipix,                &
                                           igab,ihyper,             &
                                           ikdist,ifilter,istdat,   &
                                           ATMOS_EMIS, SOLAR_EMIS,  &
                                           IW,WAVE,ind_wl,lresult,  &
                                           NBIN,RADIUS,SD,          &
                                           NSHAPE,RATIOS,SHD,       &
                                           RREAL,RIMAG,             &
                                           OSHP,                    &
                                           iBRF_land,iBPF_land,iBRM_water, &
                                           BRF_land,BRP_land,BRM_water,tau_mol, &
                                           HOBS_km,HGR_km,HMAX_atm_km, &
                                           NHVP_fit,HVP_fit_km,     &
                                           NHVP_retr,HVP_retr_km,   &
                                           H0,sigma_aerosol,CL,     &
                                           abs_data_forw_im,        &
                                           gas_abs_data_forw_im,    &
                                           Nsubchannels,            &
                                           WL_Subchannels,          &
                                           WL_Planck,               &
                                           bandwidth,               &
                                           filters_trans,           &
                                           RREAL_Subchannels,RIMAG_Subchannels,&
                                           GAS_C_REF,NSPECIES,      &
                                           NGAS, CGAS,              &
                                           pixel_fit,               & ! INOUT
                                           GOUT_aerosol,            &
                                           GOUT_gases,              &
                                           GOUT_surface,            &
                                           MU, tau_mol_I, ind_wl_i, & ! AL
                                           MDPR_wl,                 & ! modified by Qiaoyun HU

                                           GOUT_bbflux_pixel,       &
                                           NANG,ANGL,               &  ! OUT
                                           KERNELS1,KERNELS2        &
                                        )

      use mod_par_DLS,      only : KMpar
      use mod_par_inv,      only : KPARS,KSHAPE,KBF,KIDIM3,KVERTM,KIP,KNBVM
      use mod_par_OS,       only : NMG,KSD,NBVM,NMM,KNT,NG0T,KVERT_WD, N_SUB_CHANNEL_MAX,N_WL_CHANNEL_MAX
      USE MOD_RT_SOS_SETUP, ONLY : RT_SOS_RES
!XH   module related to gas absorption
      use mod_abs_kd

      use sub_gas_kd, only : DATA_GAS,NLEVEL_GAS

      use mod_index_cloud
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_alloc_kernels
      use mod_alloc_gas_lut, only: LUT_GASES,ATM_PROF,filter_channel_type, filter_transmission

	  	  
      IMPLICIT NONE
!	------------------------------------------------------------------------------------------------------
! IN:
! IN:
      integer,                    intent(in)  ::  iu_main_output
      type(retr_input_settings),  intent(in)  ::  RIN
      integer,                    intent(in)  ::  ipix,IW,ind_wl, ind_wl_i
      real,                       intent(in)  ::  HOBS_km,HGR_km,HMAX_atm_km                            
      integer,                    intent(in)  ::  NHVP_fit,NHVP_retr
      real,dimension(KVERTM),     intent(in)  ::  HVP_fit_km,HVP_retr_km
      real,                       intent(in)  ::  WAVE
      real,DIMENSION(N_SUB_CHANNEL_MAX), intent(in)  ::  bandwidth,WL_Subchannels
      REAL,DIMENSION(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX), intent(inout) ::    WL_Planck
      integer,dimension(KSD),     intent(in)  ::  NSHAPE
      integer,                    intent(in)  ::  iBRF_land,iBPF_land,iBRM_water
      real,dimension(KBF),        intent(in)  ::  BRF_land,BRP_land,BRM_water
      type(OSH_par),              intent(in)  ::  OSHP		
      real,                       intent(in)  ::  tau_mol, tau_mol_I
      real,                       intent(in)  ::  CL, MU
      real,                       intent(in)  ::  MDPR_wl
      real,dimension(KVERTM,KSD), intent(inout) ::  H0 ! AL contains parameters of aerosol vertical
                                                       ! distribution or distribution itself
      real,dimension(KSD),        intent(inout) ::  RREAL, RIMAG
      real,dimension(KSD),        intent(in)    ::  sigma_aerosol
!XH   data of gas absorbing band
      type (DATA_ABS),            intent(in)    ::  abs_data_forw_im

      REAL,DIMENSION(NMG),        intent(in)    ::  GAS_C_REF
      type (DATA_GAS),            intent(inout) ::  gas_abs_data_forw_im

      integer,dimension(KSD),     intent(in)  ::  NBIN
      real,dimension(KIDIM3,KSD), intent(in)  ::  RADIUS, SD
      real,dimension(KSHAPE,KSD), intent(in)  ::  RATIOS, SHD

      logical,                    intent(in)  ::  lresult
      integer,                    intent(in)  ::  NSPECIES, NSubchannels
      integer,                    intent(in)  ::  NGAS
      real,dimension(NMG),        intent(in)  ::  CGAS
      logical,                    intent(in)  ::  ihyper, igab, ikdist, ifilter, istdat, ATMOS_EMIS, SOLAR_EMIS
      type(filter_transmission),  intent(in)  ::  filters_trans
      real,dimension(KSD,N_SUB_CHANNEL_MAX),intent(inout)  ::  RREAL_Subchannels,RIMAG_Subchannels
!	------------------------------------------------------------------------------------------------------
! INOUT:
      type(output_segment_particles),  intent(inout)  ::  GOUT_aerosol
      type(output_segment_gases),      intent(inout)  ::  GOUT_gases
      type(output_segment_surface),    intent(inout)  ::  GOUT_surface
      type(output_pixel_bbflux),       intent(inout), optional  ::  GOUT_bbflux_pixel
      type(pixel),                     intent(inout)  ::  pixel_fit
      type(kernels_triangle_bin),      intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),     intent(inout)  ::  KERNELS2
!	------------------------------------------------------------------------------------------------------
! OUT:
      integer,                        intent(out)           ::  NANG
      real,dimension(KMpar),          intent(out)           ::  ANGL
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      integer,dimension(KSD)         ::  NBIN_tmp
      real,dimension(KIDIM3,KSD)     ::  RADIUS_tmp
      type(output_segment_particles) ::  GOUT_aerosol_tmp
      integer                       ::  I, ISD, IP, IAN
      real, dimension(KNBVM)        ::  meas
      logical                       ::  laerosol, lsurface
      integer                       ::  nmeas_type, meas_type
      real                          ::  sca
      integer                       ::  NBV_comb
      real, dimension(2*NBVM)       ::  SQout_comb,SUout_comb,SLPout_comb,SLout_comb
      logical                       ::  SvR_first
!      real,dimension(KSD)           ::  A_conc      ! Aerosol concentration, added by Qiaoyun HU, 2017-02-01
      integer                       ::   IDIM2,IDIM3, ISUB
!XH   switch for broadband flux calculation
      logical                       ::  iFlux
      real,dimension(KSD)           ::  ext_norm, ext_norm_tmp
      type(output_segment_particles)::  GOUT_aerosol_Subchannels
!	------------------------------------------------------------------------------------------------------
! SAVE:
      integer,                    save  ::  ipix_save = 0
      real,                       save  ::  WAVE_save = 0.0
      real,dimension(KIDIM3,KSD), save  ::  SD_save  = 0.0
      real,dimension(KSHAPE,KSD), save  ::  SHD_save = 0.0
      real,dimension(KVERTM,KSD), save  ::  H0_save = 0.0
      real,dimension(KSD),        save  ::  RREAL_save = 0.0 	
      real,dimension(KSD),        save  ::  RIMAG_save = 0.0
      real,dimension(KBF),        save  ::  BRF_land_save  = 0.0
      real,dimension(KBF),        save  ::  BRP_land_save  = 0.0
      real,dimension(KBF),        save  ::  BRM_water_save = 0.0
      real,dimension(KSD),        save  ::  sigma_aerosol_save = 0.0
      integer,                    save  ::  IMSC_save = -1
      real,dimension(NMG),        save  ::  CGAS_save = 0.0
!	------------------------------------------------------------------------------------------------------
!     IW     - index of wave length in array of wave length for current pixel
!     WAVE   - value of wave length for index IW from array of wave length for current pixel
!     ind_wl - index of wave length in general array of wave length for inversion
!     ind_wl_i – index of initial wavelength in general array of wavelenght of inversion, corresponding 
!                to index of shifted measurement (e.g. RAMAN)
!     write(*,*) 'in forward_model_pixel_wl: iw,wl,ind_wl,ind_wl_i',iw,WAVE,ind_wl,ind_wl_i
!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------

! To reset saved values to "0" at the beginning of inversion (for grasp run in CLOUD project)
      if(.NOT. RT_SOS_RES%IGQ_F) then
        WAVE_save = 0.0
        SD_save(:,:)  = 0.0
        SHD_save(:,:) = 0.0
        H0_save(:,:) = 0.0
        RREAL_save(:) = 0.0
        RIMAG_save(:) = 0.0
        BRF_land_save(:)  = 0.0
        BRP_land_save(:)  = 0.0
        BRM_water_save(:) = 0.0
        sigma_aerosol_save(:) = 0.0
        IMSC_save = -1
        CGAS_save(:) = 0.0
      endif

! lresult=.true. only single scattering properties are calculated
      if(.not. lresult) then
        laerosol = .false. ! if laerosol true we need to recalculate its properties
        lsurface = .false.
        if (ipix .ne. ipix_save) then ! recalculating properties if pixel is different from previous
            laerosol = .true.
            lsurface = .true.
        else
            if (WAVE .ne. WAVE_save) then ! recalculating properties if new wl differs from previous
               laerosol = .true.
               lsurface = .true.
            else
               if (OSHP%IMSC .ne. IMSC_save) then
                  laerosol = .true.
                  lsurface = .true.
               endif !imsc

! checking if any of the model params changed and recalculate properties if change appeared
! scipping calculations if everuthing's the same to save time on derivatives calculation
! aerosol
          if (.not. laerosol) then
            if(ANY(SD(:,:)  .ne. SD_save(:,:)))   &
            laerosol = .true.
          endif
          if (.not. laerosol) then
            if(ANY(SHD(:,:) .ne. SHD_save(:,:)))  &
            laerosol = .true.
          endif
          if (.not. laerosol) then
            if(ANY(H0(:,:)  .ne. H0_save(:,:)))   &
            laerosol = .true.
          endif
          !if ((.not. laerosol) .and. (.not. RIN%use_models)) then
          if ( .not. laerosol ) then
            if(ANY(RREAL(:) .ne. RREAL_save(:)))  &
            laerosol = .true.
          endif
          !if ((.not. laerosol) .and. (.not. RIN%use_models)) then
          if ( .not. laerosol ) then
            if(ANY(RIMAG(:) .ne. RIMAG_save(:)))  &
            laerosol = .true.
          endif
          if (.not. laerosol) then
            if(ANY(sigma_aerosol(:) .ne. sigma_aerosol_save(:)))  &
            laerosol = .true.
          endif
          if (.not. laerosol) then
            if(ANY(CGAS(:) .ne. CGAS_save(:)))  &
            laerosol = .true.
          endif
! surface
          if (.not. lsurface) then
            if(ANY(BRF_land(:)  .ne. BRF_land_save(:)))  &
            lsurface = .true.
          endif
          if (.not. lsurface) then
            if(ANY(BRP_land(:)  .ne. BRP_land_save(:)))  &
            lsurface = .true.
          endif
          if (.not. lsurface) then
            if(ANY(BRM_water(:) .ne. BRM_water_save(:))) &
            lsurface = .true.
          endif
        endif ! WAVE .eq. WAVE_save
        endif ! ipix
        if (laerosol) then
          SD_save(:,:)  = SD(:,:)
          SHD_save(:,:) = SHD(:,:)
          H0_save(:,:)  = H0(:,:)
          RREAL_save(:) = RREAL(:)
          RIMAG_save(:) = RIMAG(:)
          sigma_aerosol_save(:) = sigma_aerosol(:)
          CGAS_save(:) = CGAS(:)
        endif
        if (lsurface) then
          BRF_land_save(:)  = BRF_land(:)
          BRP_land_save(:)  = BRP_land(:)
          BRM_water_save(:) = BRM_water(:)
        endif
        !laerosol=.true.
        !lsurface=.true.
!       write(*,*) 'laerosol=',laerosol,'  lsurface=',lsurface
        ipix_save = ipix
        WAVE_save = WAVE
        IMSC_save = OSHP%IMSC
      else
        !only single scattering properties are calculated
        laerosol = .true.
        lsurface = .true.
      endif ! lresult

!	------------------------------------------------------------------------------------------------------
!  Aerosol phase matrix, extinction and single scattering albedo
      if(laerosol) then
      if(RIN%NSD .gt. 0) then
! forw_single_scattering_particle_properties output:
! - cross sections ext sca abs (1/um)
! - scattering matrix elements ph11, p12, ..., p44 (unitless)
!write(*,*)'NSD', RIN%NSD
!write(*,*)'NBIN', NBIN
!write(*,*)'RADIUS', RADIUS

!write(*,*)'SD', SD
!write(*,*)'NSHAPE', NSHAPE
!write(*,*)'RATIOS', RATIOS
!write(*,*)'SHD', SHD
!write(*,*)'RREAL', RREAL
!write(*,*)'RIMAG', RIMAG
!
!write(*,*)'NANG', NANG
!write(*,*)'ANGL', ANGL

        call forw_single_scattering_particle_properties ( iu_main_output,             & ! IN
                                                          RIN, RIN%NSD,               &
                                                          NBIN, RADIUS, SD, RIN%KNLN, &
                                                          NSHAPE, RATIOS, SHD,        &
                                                          ind_wl, WAVE,               &
                                                          RREAL,                      &
                                                          RIMAG,                      &
                                                          NANG, ANGL, ipix,           & ! OUT
                                                          GOUT_aerosol,               &
                                                          ext_norm,                   &
                                                          KERNELS1, KERNELS2          &
                                                        )
            
        if(Nsubchannels .gt. 1) then
            do ISUB=1,Nsubchannels
                !MH Calculation of aerosol single scattering properties in each subchannel
                call forw_single_scattering_particle_properties ( iu_main_output,              & ! IN
                                                                  RIN, RIN%NSD,                &
                                                                  NBIN, RADIUS, SD, RIN%KNLN,  &
                                                                  NSHAPE, RATIOS, SHD,         &
                                                                  ISUB, WL_Subchannels(ISUB),  &
                                                                  RREAL_Subchannels(:,ISUB),   &
                                                                  RIMAG_Subchannels(:,ISUB),   &
                                                                  NANG, ANGL, ipix,            & ! OUT
                                                                  GOUT_aerosol_Subchannels,    &
                                                                  ext_norm,                    &
                                                                  KERNELS1, KERNELS2           &
                                                                )

            end do
            !MH Integration of aerosol single scattering properties in full channel
            call ss_particle_properties_subch_int(Nsubchannels,                        &  !IN
                                                  ipix,IW,RIN%NSD,NANG,                &
                                                  GOUT_aerosol_Subchannels,            &
                                                  RREAL_Subchannels,RIMAG_Subchannels, &
                                                  bandwidth,                           &
                                                  RREAL, RIMAG,                        &
                                                  GOUT_aerosol)                  !OUT

        else

            GOUT_aerosol_Subchannels%opt%pixel(ipix)%wl(1)   = GOUT_aerosol%opt%pixel(ipix)%wl(ind_wl)
            GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl(1)  = GOUT_aerosol%phmx%pixel(ipix)%wl(ind_wl)
        
        end if
        if ( error_present() ) return

        if(RIN%IPRI_additional_info .and. OSHP%IMSC .eq. -2) then
          do ISD=1,RIN%NSD
          write(*,*) RREAL(ISD),RIMAG(ISD),ISD,IW,WAVE,ind_wl,'  RREAL RIMAG ISD IW WAVE ind_wl in forw_IMAGE_I_IW'
          write(*,*) ISD,GOUT_aerosol%opt%pixel(ipix)%wl(ind_wl)%ssa(ISD),  &
                         GOUT_aerosol%opt%pixel(ipix)%wl(ind_wl)%ext(ISD),'  ISD SSA EXT - aerosol'
          if(RIN%DLSF%keyEL .gt. 0) then
            do IAN=1,NANG
            write(*,'(i4,5e14.5)') ISD,ANGL(IAN),  &
            GOUT_aerosol%phmx%pixel(ipix)%wl(ind_wl)%ph11(IAN,ISD), &
            GOUT_aerosol%phmx%pixel(ipix)%wl(ind_wl)%ph12(IAN,ISD), &
            GOUT_aerosol%phmx%pixel(ipix)%wl(ind_wl)%ph22(IAN,ISD), &
            GOUT_aerosol%phmx%pixel(ipix)%wl(ind_wl)%ph33(IAN,ISD)
            enddo ! IAN
          endif
          enddo ! ISD
       endif ! IPRI

      ! Single scattering matrix for cut off particle size
      if ( RIN%cutoff%nrmax .gt. 0 ) then
! work only for triangle bin (TB) size particle model
! Calculate single scattering properties for cut off TB size distributions with
! given in settings rmax
!
! forw_single_scattering_particle_properties output:
! - cross sections ext sca abs (1/um)
! - scattering matrix elements ph11, p12, ..., p44 (unitless)
        do i=1,RIN%cutoff%nrmax
          NBIN_tmp(:) = 0
          RADIUS_tmp(:,:) = 0.0
          if ( all(NBIN(1:RIN%NSD) .eq. 2) ) then
          ! for LN SD
            NBIN_tmp(1:RIN%NSD) = 2
            RADIUS_tmp(1,1:RIN%NSD) = RADIUS(1,1:RIN%NSD)
            RADIUS_tmp(2,1:RIN%NSD) = RIN%CUTOFF%rmax_tb(1:RIN%NSD,i)
          else
          ! for TB SD
            do ISD=1,RIN%NSD
            NBIN_tmp(ISD) = RIN%cutoff%ntb(ISD,i)
            RADIUS_tmp(1:NBIN_tmp(ISD),ISD) = RADIUS(1:NBIN_tmp(ISD),ISD)
            enddo
          endif
          call forw_single_scattering_particle_properties ( iu_main_output,             & ! IN
                                                            RIN, RIN%NSD,               &
                                                            NBIN_tmp, RADIUS_tmp, SD,   &
                                                            RIN%cutoff%ntb(:,i),        &
                                                            NSHAPE, RATIOS, SHD,        &
                                                            ind_wl, WAVE, RREAL, RIMAG, &
                                                            NANG, ANGL, ipix,           & ! OUT
                                                            GOUT_aerosol_tmp,           &
                                                            ext_norm_tmp,               &
                                                            KERNELS1, KERNELS2          &
                                                          )
          if ( error_present() ) return
          ! set cut off single scattering properties to related fields of general output structure
          call set_gout_cutoff_single_scattering_properties ( ipix, ind_wl, i, RIN%NSD, NANG, &
                                                              GOUT_aerosol_tmp, GOUT_aerosol)
        enddo ! i
      endif ! RIN%cutoff%nrmax .gt. 0

      endif ! RIN%NSD .gt. 0
      endif ! laerosol
!write(*,*) GOUT_aerosol%opt%pixel(1)%wl(1)%ssa(1),GOUT_aerosol%opt%pixel(1)%wl(1)%ext(1)
! gases if applied

!      GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(:) = 0.0
!write(*,*) 'ihyper', ihyper, 'indx_WL', ind_wl
!      if(igab .and. ihyper) then
!        !MH Vertical, spectral and concentration normalization
!        call GET_GAS_TOT_EXT(gas_abs_data_forw_im,ikdist,bandwidth,NGAS,GAS_C_REF,CGAS,TOT_EXT_GAS)
!!        GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(1:NGAS) = TOT_EXT_GAS(1:NGAS)
!!        write(*,*) 'GOD', TOT_EXT_GAS, 'indx_WL', ind_wl
!        GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(:) = 0.0
!        GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(1) = TOT_EXT_GAS(1)
!
!!        write(*,*) 'GOD', GOUT_gases%pixel(ipix)%wl(ind_wl)%abs, 'indx_WL', ind_wl
!      endif

!	------------------------------------------------------------------------------------------------------
      
      if (present(GOUT_bbflux_pixel) .and. (.not. igab)) then
!XH      for broadband flux calculation
         iFlux = .true.
         call forw_radiative_transfer (   iFlux,igab,ikdist,ifilter,                &  ! IN
                                          ATMOS_EMIS,SOLAR_EMIS,                    &
                                          RIN,                                      &
                                          IW,WAVE,ind_wl,IP,                        &
                                          OSHP,                                     &
                                          iBRF_land,iBPF_land,iBRM_water,           &
                                          BRF_land,BRP_land,BRM_water,tau_mol,      &
                                          HOBS_km,HGR_km,HMAX_atm_km,               &
                                          NHVP_retr,HVP_retr_km,H0,sigma_aerosol,   &
                                          laerosol,lsurface,                        &
                                          NANG,ANGL,                                &
                                          RIN%NSD,NBIN,SD,ext_norm,                 &
                                          abs_data_forw_im,                         &
                                          gas_abs_data_forw_im,                     &
                                          RIN%gases%nsubchannels(IW),               &
                                          WL_Planck,                                &
                                          bandwidth,                                &
                                          filters_trans,                            &
                                          NGAS,CGAS,                                &
                                          ipix,pixel_fit,                           &
                                          GOUT_aerosol_Subchannels%opt%pixel(ipix)%wl,  & ! INOUT
                                          GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl, &
                                          GOUT_surface%pixel(ipix)%wl(ind_wl),      &
                                          GOUT_bbflux_pixel,                        &

                                          NBV_comb,                                 & ! OUT
                                          SLout_comb,SQout_comb,                    &
                                          SUout_comb,SLPout_comb                    &
                                      )
         return
      end if
!	------------------------------------------------------------------------------------------------------
      if (.not. lresult) then
         nmeas_type = pixel_fit%meas(IW)%NIP
         !write(*,*) 'nmeas_type =',nmeas_type
         SvR_first = .true.
         LOOP_meas_type: do IP=1,nmeas_type
            meas_type = pixel_fit%meas(IW)%meas_type(IP)
            !write(*,*) 'ip =',ip,'  meas_type =',meas_type
!	------------------------------------------------------------------------------------------------------
!           Single scattering optical properties
            if ( (meas_type .gt. meas_type_tau_beg .and. &
                 meas_type .lt. meas_type_phm_end) .or. &
                 (meas_type .gt. meas_type_integrated_beg .and. &
                  meas_type .lt. meas_type_integrated_end) )  then
               call set_pixel_phase_matr_fit (                                            &
                                                RIN%NSD, NGAS, tau_mol,                   &
                                                NANG, ANGL, RIN%sca_ang_norm_p11,         &
                                                GOUT_aerosol%opt%pixel(ipix)%wl(ind_wl),  &
                                                GOUT_aerosol%phmx%pixel(ipix)%wl(ind_wl), &
                                                GOUT_gases%pixel(ipix)%wl(ind_wl),        &
                                                meas_type, IW, IP, RIN%iPOBS,             &
                                                RIN%CUTOFF%cutoff_meas_diff,              &
                                                pixel_fit                                 &
                                             )
              if ( error_present() ) return
!	------------------------------------------------------------------------------------------------------
!           Lidar signal and if developped depolarization
!           This part has to be developed for depolarization and Roman lidar
            else if (meas_type .gt. meas_type_lid_beg .and. meas_type .lt. meas_type_lid_end) then
!WRITE(*,*),'L274 forw_model.f90 : RIN%NDIM%n1 =',RIN%NDIM%n1
!WRITE(*,*),'L275 forw_model.f90 : RIN%NDIM%n2 =',RIN%NDIM%n2
!WRITE(*,*),'L276 forw_model.f90 : RIN%NDIM%n3 =',RIN%NDIM%n3
!WRITE(*,*),'L277 forw_model.f90 : RIN%NDIM%par_type =',RIN%NDIM%par_type
!WRITE(*,*),'L278 forw_model.f90 : RIN%NDIM%par_type =',RIN%NDIM%par_retr

!!  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!                  Added by Qiaoyun HU, 2017-01-31
!!  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!! compute the aersosol concentration from RADIUS, SD 

!    DO IDIM2= 1,RIN%NSD         ! check with Anton, if the RIN%NSD == RIN%NDIM%n2(1)
!        IDIM3= RIN%NDIM%n3(IDIM2,1)
!        A_conc(IDIM2) = SUM(SD(1:IDIM3,IDIM2))*(LOG(RADIUS(2,IDIM2))-LOG(RADIUS(1,IDIM2)))
!    END DO
!!  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
               call forw_lidar_signal (                                                     &
                                         HOBS_km,HGR_km,HMAX_atm_km,                        & ! IN
                                         NHVP_fit,HVP_fit_km,                               &
                                         NHVP_retr,HVP_retr_km,                             &
                                         H0,CL,                                             &
                                         MDPR_wl,                                           & ! MDPR for current wavelength index
                                         RIN%NSD,                                           &
                                         GOUT_aerosol%opt%pixel(ipix)%wl(ind_wl)%ext(:),    &
                                         GOUT_aerosol%lidar%pixel(ipix)%wl(ind_wl)%lr(:),   &
                                         tau_mol,                                           &
                                         pixel_fit%meas(IW)%MPROF(:,IP),                    &
                                         RIN%mol_prof_type,                                 &
                                         RIN%aer_prof_type,                                 &
                                         GOUT_aerosol%opt%pixel(ipix)%wl(ind_wl_i)%ext(:),  &
                                         tau_mol_I,                                         &
                                         ! to do put drpar and drper calculations in a proper place
                                         GOUT_aerosol%lidar%pixel(ipix)%wl(ind_wl)%ldpar(:),&
                                         GOUT_aerosol%lidar%pixel(ipix)%wl(ind_wl)%ldper(:),&
                                         pixel_fit%meas(IW)%MU(1), meas_type,               & ! AL TO DO check size of MU!!!!
                                         meas(:)                                & ! INOUT

                                      )
               if ( error_present() ) return
               call set_pixel_lidar_signal_fit ( NHVP_fit,meas(1:NHVP_fit), &
                                                 meas_type,IW,IP,  &
                                                 pixel_fit         &
                                               )
!	------------------------------------------------------------------------------------------------------
!           Radiative transfer accounting for multiple scattering

            else if(meas_type .gt. meas_type_SvR_beg .and. meas_type .lt. meas_type_SvR_end) then
               if (SvR_first) then
                  ! Radiative transfer routine is called only once for first Stokes vector measurement
                  ! type because its output is a Stokes vector (I,Q,U) in one routine call.
                  iFlux = .false.
                  call forw_radiative_transfer (   iFlux,igab,ikdist,ifilter,ATMOS_EMIS,SOLAR_EMIS,  & ! IN
                                                   RIN,                                      &
                                                   IW,WAVE,ind_wl,IP,                        &
                                                   OSHP,                                     &
                                                   iBRF_land,iBPF_land,iBRM_water,           &
                                                   BRF_land,BRP_land,BRM_water,tau_mol,      &
                                                   HOBS_km,HGR_km,HMAX_atm_km,               &
                                                   NHVP_retr,HVP_retr_km,H0,sigma_aerosol,   &
                                                   laerosol,lsurface,                        &
                                                   NANG,ANGL,                                &
                                                   RIN%NSD,NBIN,SD,ext_norm,                 &
                                                   abs_data_forw_im,                         &
                                                   gas_abs_data_forw_im,                     &
                                                   RIN%gases%nsubchannels(IW),               &
                                                   WL_Planck,                                &
                                                   bandwidth,                                &
                                                   filters_trans,                            &
                                                   NGAS,CGAS,                                &
                                                   ipix,pixel_fit,                           &
                                                   GOUT_aerosol_Subchannels%opt%pixel(ipix)%wl,  & ! INOUT
                                                   GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl, &
                                                   GOUT_surface%pixel(ipix)%wl(ind_wl),      &
                                                   GOUT_bbflux_pixel,                        &

                                                   NBV_comb,                                 & ! OUT
                                                   SLout_comb,SQout_comb,                    &
                                                   SUout_comb,SLPout_comb                    &
                                               )

                  if ( error_present() ) return

                  call set_pixel_Stokes_vec_fit (  IW,IP,nmeas_type,RIN%iPOBS, &
                                                   NBV_comb,                   &
                                                   SLout_comb,SQout_comb,      &
                                                   SUout_comb,SLPout_comb,     &
                                                   pixel_fit                   &
                                                )


                  if ( error_present() ) return
                  SvR_first = .false.
               end if
            end if ! meas_type .gt. meas_type_tau_beg .and.
!	------------------------------------------------------------------------------------------------------
         end do LOOP_meas_type
      endif ! lresult
!
      RETURN
      END SUBROUTINE forward_model_pixel_wl

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! forw_phase_matrix
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine forw_phase_matrix (                         &
                                    iu_main_output,          & ! IN
                                    IPRI_verbose,            &
                                    IPRI_additional_info,    &
                                    NSD,NBIN,RADIUS,SD,      &
                                    KNLN,                    &
                                    NSHAPE,RATIOS,SHD,       &
                                    ind_wl,WAVE,RREAL,RIMAG, &
                                    use_models,              &
                                    tiny_wvl_models,         &
                                    NANG,ANGL,ipix,          & ! OUT
                                    GOUT_particles_opt_pixel_wl,  &
                                    GOUT_particles_phmx_pixel_wl, &
                                    ext_norm,                &
                                    DLSF,KERNELS1,KERNELS2   & 
                                   )
                                   
      use mod_par_inv, only : KSHAPE,KIDIM3
      use mod_par_OS,  only : KSD
      use mod_alloc_kernels
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
            
      implicit none
! -----------------------------------------------------------------------
! IN:
      integer,                    intent(in)  ::  iu_main_output
      logical,                    intent(in)  ::  IPRI_verbose
      logical,                    intent(in)  ::  IPRI_additional_info
      real,dimension(KIDIM3,KSD), intent(in)  ::  RADIUS,SD
      real,dimension(KSHAPE,KSD), intent(in)  ::  RATIOS,SHD
      integer,dimension(KSD),     intent(in)  ::  NBIN,NSHAPE
      integer,dimension(KSD),     intent(in)  ::  KNLN
      real,dimension(KSD),        intent(inout)  ::  RREAL,RIMAG
      real,                       intent(in)  ::  WAVE
      real,                       intent(in)  ::  tiny_wvl_models
      integer,                    intent(in)  ::  NSD,ind_wl,ipix
      type(iP_flags_for_DLS),     intent(in)  ::  DLSF
      logical,                    intent(in)  ::  use_models
! -----------------------------------------------------------------------
! OUT:
      integer,                        intent(out) ::  NANG
      real,dimension(KMpar),          intent(out) ::  ANGL
      type(output_pixel_opt_wl),      intent(inout)  ::  GOUT_particles_opt_pixel_wl
      type(output_pixel_ph_matrix_wl),intent(inout)  ::  GOUT_particles_phmx_pixel_wl
      real,dimension(KSD),            intent(out) :: ext_norm
!	----------------------------------------------------------------------
! INOUT:
      type(kernels_triangle_bin), intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),intent(inout)  ::  KERNELS2
! -----------------------------------------------------------------------

#if defined(SPHEROID)
!      if(.not. present(KERNELS1) .or. .not. present(KERNELS2)) then
!         write(*,*) 'KERNELS1,KERNELS2 are not present in forw_phase_matrix'
!         stop 'stop in forw_phase_matrix'
!      endif

! spheroid_package output:
! - cross sections ext sca abs (1/um)
! - scattering matrix elements ph11=f11, ph12=f12, ..., ph44=f44 (1/um)
      call spheroid_package (                          &
                              iu_main_output,          &
                              IPRI_verbose,            &
                              IPRI_additional_info,    & ! IN
                              NSD,NBIN,RADIUS,SD,      &
                              KNLN,                    &
                              NSHAPE,RATIOS,SHD,       &
                              ind_wl,WAVE,RREAL,RIMAG, &
                              use_models,              &
                              tiny_wvl_models,         &
                              NANG,ANGL,               & ! OUT
                              GOUT_particles_opt_pixel_wl,  &
                              GOUT_particles_phmx_pixel_wl, &
                              ext_norm,                &
                              DLSF,KERNELS1,KERNELS2   &
                            )
!print *, nsd,1,GOUT_particles_opt_pixel_wl%ext(1:nsd),'  - nsd,itrc, ext_tmp; in forw_phase_matrix'
      if ( error_present() ) return

!#elif defined(ANOTHER)
!      call another_model()
#else
#error No PHASE_MATRIX configured!
#endif

      return
      end subroutine forw_phase_matrix
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! forw_lidar_signal
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine forw_lidar_signal (                                &
                                    HOBS_km,HGR_km,HMAX_atm_km,     & ! IN
                                    NHVP_fit,HVP_fit_km,            &
                                    NHVP_retr,HVP_retr_km,          &
                                    H0,CL,                          &
                                    MDPR_wl,                        &   ! modified by Qiaoyun HU
                                    NSD,EXTA,LRA,                   &
                                    tau_mol,MPROF,                  &
                                    mol_prof_type, aer_prof_type,   & !AL
                                    EXTA_I, tau_mol_I,              &
                                    DR_PAR, DR_PER, MU,             &
                                    meas_type,                      &
                                    meas                            & ! INOUT
                                   )

      use mod_par_inv,   only : KVERTM
      use mod_par_OS,    only : KSD  
	  	  
      implicit none
!	------------------------------------------------------------------------------------------------------
! IN:
      integer,                    intent(in)  ::  NSD !  ,IFMP AL: seems not to be used
      real,                       intent(in)  ::  HOBS_km,HGR_km,HMAX_atm_km                            
      integer,                    intent(in)  ::  NHVP_fit,NHVP_retr
      real,dimension(KVERTM),     intent(in)  ::  HVP_fit_km,HVP_retr_km 
      real,                       intent(in)  ::  CL,tau_mol, tau_mol_I, MU, MDPR_wl  ! 'MDPR_wl' modified by Qiaoyun HU
      real,dimension(KSD),        intent(in)  ::  EXTA,LRA,EXTA_I,DR_PAR,DR_PER
      real,dimension(KVERTM,KSD), intent(in)  ::  H0 ! contains parameters of aerosol vertical 
                                                     ! distribution or distribution itself
!      real,dimension(KSD),        intent(in)  ::  A_conc ! added by QiaoyunHU, 2017-01-31
      real,dimension(KVERTM),     intent(in)  ::  MPROF
      integer,                    intent(in)  ::  meas_type
      integer,                    intent(in)  ::  mol_prof_type, aer_prof_type
!	------------------------------------------------------------------------------------------------------
! INOUT:
      real,dimension(KVERTM),     intent(out) ::  meas
!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------

#if defined(GARRLIC)
      call lidar_garrlic (                                  &
                            HOBS_km,HGR_km,HMAX_atm_km,     & ! IN
                            NHVP_fit,HVP_fit_km,            &
                            NHVP_retr,HVP_retr_km,          &
                            H0,CL,                          &
                            MDPR_wl,                        &
!                            A_conc,                         & ! added by QiaoyunHU, 2017-01-31, Aerosol concentration
                            NSD,EXTA,LRA,                   &
                            tau_mol, MPROF,                 &
                            mol_prof_type, aer_prof_type,   &
                            meas_type,                      &
                            EXTA_I, tau_mol_I,              &
                            DR_PAR, DR_PER, MU,             &
                            meas                            & ! INOUT
                         )      

      if ( error_present() ) return
!#elif defined(ANOTHER)
!      call another_model()
#else
!#error No LIDAR configured!
#warning No LIDAR configured!
#endif

      return
      end subroutine forw_lidar_signal

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! forw_radiative_transfer
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine forw_radiative_transfer (                                          &
                                             iFlux,igab,ikdist,ifilter,ATMOS_EMIS,SOLAR_EMIS,&
                                             RIN,                                   & ! IN
                                             IW,WAVE,ind_wl, IP,                    &
                                             OSHP,                                  &
                                             iBRF_land,iBPF_land,iBRM_water,        &
                                             BRF_land,BRP_land,BRM_water,tau_mol,   &
                                             HOBS_km,HGR_km,HMAX_atm_km,            &
                                             NHVP_retr,HVP_retr_km,H0,sigma_aerosol,&
                                             laerosol,lsurface,                     &
                                             NANG,ANGL,                             &
                                             NSD,NBIN,SD,ext_norm,                  &
                                             abs_data_forw_im,                      &
                                             gas_abs_data_forw_im,                  &
                                             nsubchannels,                          &
                                             WL_Planck,                             &
                                             bandwidth,                             &
                                             filters_trans,                         &
                                             NGAS,CGAS,                             &
                                             ipix,pixel_fit,                        &
                                             GOUT_aerosol_opt_pixel_wl,             & ! INOUT
                                             GOUT_aerosol_phmx_pixel_wl,            &
                                             GOUT_surface_pixel_wl,                 &
                                             GOUT_bbflux_pixel,                     &

                                             NBV_comb,                              & ! OUT
                                             SLout_comb,SQout_comb,                 &
                                             SUout_comb,SLPout_comb                 &
                                         )
 
      use mod_globals, only  : GBL_FILE_PATH_LEN
      USE MOD_RT_SOS
      use mod_rt
      use mod_par_DLS,   only : KMpar
      use mod_par_inv,   only : KPARS,KSHAPE,KBF,KIDIM3,KVERTM,KIP,KNBVM,KW
      use mod_par_OS,    only : NMG,KSD,NBVM,NMM,KNT,NG0T,KVERT_WD,N_SUB_CHANNEL_MAX,N_WL_CHANNEL_MAX
!XH   module related to gas absorption
      use mod_abs_kd,   only  :  DATA_ABS

      use sub_gas_kd, only : DATA_GAS,MAXKD,NWL_GAS,NLEVEL_GAS
      use mod_bbgas_kd,   only : RT_GAS_PROFILE,INT_KD,INT_LINES,INT_SC
      use mod_alloc_gas_lut, only: ATM_PROF,filter_channel_type, filter_transmission
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_alloc_kernels
      
      use mod_vertical_distr_derived_type
      use mod_derivative_type_transport_model, only : TM, tracer_average
      use mod_c_utils

      IMPLICIT NONE
!	------------------------------------------------------------------------------------------------------
! IN:
!XH   switch for broadband flux calculation
      logical,                    intent(inout)  ::  iFlux
      logical,                    intent(in)  ::  igab,ikdist,ifilter,ATMOS_EMIS,SOLAR_EMIS
      type(retr_input_settings),  intent(in)  ::  RIN
      integer,                    intent(in)  ::  ipix,IW,ind_wl,IP
      real,                       intent(in)  ::  HOBS_km,HGR_km,HMAX_atm_km                            
      integer,                    intent(in)  ::  NHVP_retr
      real,dimension(KVERTM),     intent(in)  ::  HVP_retr_km
      real,                       intent(in)  ::  WAVE
      integer,                    intent(in)  ::  iBRF_land,iBPF_land,iBRM_water
      real,dimension(KBF),        intent(in)  ::  BRF_land,BRP_land,BRM_water
      type(OSH_par),              intent(in)  ::  OSHP		
      real,                       intent(in)  ::  tau_mol
      real,dimension(KVERTM,KSD), intent(inout)  ::  H0 ! AL contains parameters of aerosol vertical
                                                        ! distribution or distribution itself
      real,dimension(KSD),        intent(in)     ::  sigma_aerosol
      logical,                    intent(in) ::  laerosol, lsurface
!XH   data of o2 absorption, do not delete
      type (DATA_ABS),            intent(in)  ::  abs_data_forw_im

      type (DATA_GAS),            intent(inout) ::  gas_abs_data_forw_im

      integer,                    intent(in)    ::  NANG
      real,dimension(KMpar),      intent(in)    ::  ANGL
      integer,                    intent(inout) ::  NBV_comb
      real, dimension(2*NBVM),    intent(inout) ::  SQout_comb,SUout_comb, &
                                                    SLPout_comb,SLout_comb

      integer,                    intent(in)  ::  NSD
      integer,dimension(KSD),     intent(in)  ::  NBIN
      real,dimension(KIDIM3,KSD), intent(in)  ::  SD
      real,dimension(KSD),        intent(in)  ::  ext_norm

      integer,                     intent(in) :: NGAS, nsubchannels
      real,dimension(N_SUB_CHANNEL_MAX),intent(in) :: bandwidth
      real,dimension(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX),intent(inout) :: WL_Planck
      type(filter_transmission),       intent(in) :: filters_trans
      real,dimension(NMG),         intent(in) :: CGAS

!	------------------------------------------------------------------------------------------------------
! INOUT:
      type(output_pixel_opt_wl),dimension(KW),      intent(inout)  ::  GOUT_aerosol_opt_pixel_wl
      type(output_pixel_ph_matrix_wl),dimension(KW),intent(inout)  ::  GOUT_aerosol_phmx_pixel_wl
      type(output_pixel_surface_wl),  intent(inout)  ::  GOUT_surface_pixel_wl
      type(pixel),                    intent(inout)  ::  pixel_fit
      type(output_pixel_bbflux),      intent(inout)  ::  GOUT_bbflux_pixel
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      integer,dimension(2)                ::  surf_land_par_num
      real, dimension(2*KBF)              ::  surf_land_par_vect
      integer                             ::  surf_water_par_num, IWG, ISUB,I, NSubCH
      real, dimension(KBF)                ::  surf_water_par_vect
      integer                             ::  IDIM1, par_type, ns3, n_gas_lines
      integer                             ::  INDEX_GAS_PROFILE_BIN,INDEX_GAS_PROFILE_SC
      type(discret_vertical_distribution) ::  DISCRVD, DISCRVD_line
      character (len=GBL_FILE_PATH_LEN)   ::  external_file_path
      real                                ::  EXT_gas
      logical                             ::  gas_abs_line
      real, dimension(NWL_GAS,2*NBVM)     ::  SLout_SC = 0.0
      real, dimension(N_SUB_CHANNEL_MAX,2*NBVM)     ::  SLout_int
      REAL,DIMENSION(NLEVEL_GAS)          ::  T_profile_RT
      logical                             ::  igab_line
      real, dimension(N_SUB_CHANNEL_MAX)  ::  delta_WL

      type(output_pixel_opt_wl),dimension(1)       :: opt_pixel_wl
      type(output_pixel_ph_matrix_wl),dimension(1) :: phmx_pixel_wl

!	------------------------------------------------------------------------------------------------------
      call cstring2fstring(RIN%DLSF%external_file_path, external_file_path)

#if defined(OSH)

      call set_surface_parameter_vectors( RIN, &
                                          BRF_land, BRP_land, BRM_water, &
                                          surf_land_par_num, surf_land_par_vect, &
                                          surf_water_par_num, surf_water_par_vect )


      call RT_VERTICAL_DISCRET(RIN,igab,ikdist,IW,gas_abs_data_forw_im,     & !IN
                                    iFlux,HGR_km,HVP_retr_km,               &
                                    HMAX_atm_km,NHVP_retr,H0,               &
                                    nsubchannels,                           &
                                    sigma_aerosol,                          &
                                    ipix,pixel_fit,                         &
                                    DISCRVD)                                  !INOUT
      if ( error_present() ) return

    !write(*,'(a)')  'alt:   in forw_radiative_transfer after RT_VERTICAL_DISCRET'
    !write(*,'(10es12.4)') DISCRVD%h_km(1:DISCRVD%NH)
    !write(*,'(a)')  'aer1:   in forw_radiative_transfer after RT_VERTICAL_DISCRET'
    !write(*,'(10es12.4)') DISCRVD%val(1:DISCRVD%NH,1)
    !write(*,'(a)')  'aer2:   in forw_radiative_transfer after RT_VERTICAL_DISCRET'
    !write(*,'(10es12.4)') DISCRVD%val(1:DISCRVD%NH,2)
    !write(*,'(a)')  'mol:   in forw_radiative_transfer after RT_VERTICAL_DISCRET'
    !write(*,'(10es12.4)') DISCRVD%val(1:DISCRVD%NH,3)
!    write(*,*)  'gas:'
!    write(*,'(10es12.4)') DISCRVD%val(1:DISCRVD%NH,4)

      !if ( RIN%use_tmodel) then ! delete ???
      !! normalize vertical profile for transport model
        !do i=1,DISCRVD%natm
        !DISCRVD%val(1:DISCRVD%nh,i) = DISCRVD%val(1:DISCRVD%nh,i) / DISCRVD%norm(i)
        !enddo
      !endif

!stop
 
      IF(igab) THEN
         NSubCH = gas_abs_data_forw_im%NSubCH
      ELSE
         NSubCH = 1
      END IF

      DO ISUB =1,NSubCH !MH Loop over all subchannels in each cannel

          IF(igab) THEN
             IF(ikdist) then
                IF(gas_abs_data_forw_im%DATA_KD_Channel(1)%NEXP(ISUB) .LT. 1)THEN !MHG This situation corresponds when there is no gas especies selected in one channel and KD is used
                  igab_line = .False.
                  n_gas_lines = 1
                ELSE
                  igab_line = igab
                  n_gas_lines = gas_abs_data_forw_im%DATA_KD_Channel(1)%NEXP(ISUB)
                END IF

                WL_Planck = WAVE !MH in future the k-distribution will account for the change in planck also
                
             ELSE

                n_gas_lines = gas_abs_data_forw_im%NWL(ISUB)
                igab_line = igab

             END IF

          ELSE
                n_gas_lines = 1
                WL_Planck = WAVE
                igab_line = igab
          END IF

          DO IWG=1,n_gas_lines !MH Loop over all gas lines/kd-bins for a constant aerosol value, or subchannels WL
                
                DISCRVD_line = DISCRVD
                      
                  IF (n_gas_lines .GT. 1) THEN
                        ! gas segment profile
                        CALL RT_GAS_PROFILE(gas_abs_data_forw_im,ikdist, &       !IN
                                            IWG,ISUB,                    &
                                            NGAS,                        &
                                            DISCRVD,                     &       !OUT
                                            EXT_gas                      &
                                           )

                                          !  write(*,*)IWG, 'RT_GAS_PROFILE', EXT_gas, 'EXT_gas'
                        !MH For avoiding problems with lines/bins without any gas extinction we avoid gas related calculations in them.
                        IF(EXT_gas .LE. 0.0)THEN
                            igab_line = .False.
                        ELSE
                            DISCRVD_line = DISCRVD
                        END IF

                  ENDIF

                  !MHG Once all checks to know if gas is going to be accounted in this line we take out the component if necessary
                  IF(igab .AND. (.NOT. igab_line))THEN
                        DISCRVD_line%natm = DISCRVD_line%natm-1
                  END IF

                 if(ATMOS_EMIS) then
                    !MH Interpolation of the temperature profile from the atmospheric file to the aerosol/gas grid
                    call get_temperature_profile(DISCRVD,ATM_PROF,T_profile_RT)
                 endif

                 opt_pixel_wl(1)  = GOUT_aerosol_opt_pixel_wl(ISUB)
                 phmx_pixel_wl(1) = GOUT_aerosol_phmx_pixel_wl(ISUB)

                ! Set tracer average single scattering matrix
                 if ( RIN%use_tmodel) then
                 if ( TM%flag_av_vprof .eq. tracer_average ) then
                    call set_tracer_average_sca_matrix ( RIN%DLSF%keyEL, NANG, &
                                                        opt_pixel_wl(1),  & ! OUT
                                                        phmx_pixel_wl(1)  &
                                                      )
                 endif
                 endif
                 
                  CALL forw_SOS_RT (iFlux,igab_line,ATMOS_EMIS,SOLAR_EMIS,     & ! IN
                                      IW, WAVE, ind_wl, IWG,  IP,              &
                                      OSHP,                                    &
                                      iBRF_land,iBPF_land,iBRM_water,          &
                                      surf_land_par_num, surf_land_par_vect,   &
                                      surf_water_par_num, surf_water_par_vect, &
                                      tau_mol,EXT_gas,                         &
                                      HOBS_km, HGR_km, HMAX_atm_km,            &
                                      DISCRVD_line,                            &
                                      laerosol, lsurface,                      &
                                      NANG, ANGL,                              &
                                      SD,ext_norm,                             &
                                      abs_data_forw_im,                        &
                                      pixel_fit,                               &
                                      WL_Planck(IWG,ISUB),                     &  !MH This should be homogenize with wave for full consistency for fluxes and everything else
                                      T_profile_RT,ATM_PROF%STEMP,             &

                                      opt_pixel_wl(1),                         & ! INOUT
                                      phmx_pixel_wl(1),                        &
                                      GOUT_surface_pixel_wl,                   &
                                      GOUT_bbflux_pixel,                       &

                                      NBV_comb,                                & ! OUT
                                      SLout_SC(IWG,:), SQout_comb,             &
                                      SUout_comb, SLPout_comb,                 &
                                      external_file_path                       &
                                    )

                    !MHG When flux calculation is done the radiance comming from the single scattering which are not used may cause problems
                    If((SLout_SC(IWG,1) .le. 0) .and. (.not. iflux))then

                        write(*,*) 'ERROR: Negative SLout_kd in forw_SOS_RT forw_model.f90', SLout_SC(IWG,1)
                        stop
                    endif

                  igab_line = igab

          END DO !n_gas_lines
 
          IF (n_gas_lines .GT. 1) THEN

            if(ikdist) then
                !MH k-distribution integration of each subchanel
                  
                call INT_KD(ifilter,                &
                            filters_trans,          &
                            SLout_SC,               &
                            gas_abs_data_forw_im,   &
                            NBV_comb,               &
                            ISUB,                   &
                            IW,                     &
                            n_gas_lines,            &
                            SLout_int(ISUB,:)       &
                            )
            else

                call INT_LINES(ikdist,ifilter,           &
                               SLout_SC,                 &
                               bandwidth(ISUB),          &
                               filters_trans,            &
                               gas_abs_data_forw_im,     &
                               NBV_comb,                 &
                               ISUB,                     &
                               n_gas_lines,              &
                               delta_WL(ISUB),           & !MH This variable is to minimize the loss of accuracy in the filter integration if subchannel division is present
                               SLout_int(ISUB,:)         &
                               )

            end if

        ELSE

            SLout_int(1,1:NBV_comb) = SLout_SC(1,1:NBV_comb)

        END IF


      END DO ! NSubCH

      IF(gas_abs_data_forw_im%NSubCH .GT. 1)THEN

          !MH Integration over all subchannels

          IF(ikdist)THEN
            delta_WL = bandwidth
          END IF

          call INT_SC(SLout_int,                   &
                      delta_WL,                    &
                      NBV_comb,                    &
                      gas_abs_data_forw_im%NSubCH, &
                      SLout_comb                   &
                      )

      ELSE

          SLout_comb(1:NBV_comb) = SLout_int(1,1:NBV_comb)

      END IF


! write(*,*)'radiance',SLout_comb(1:NBV_comb), WAVE
! stop

!#elif defined(ANOTHER)
!      call another_model()
#else
!#error No RAD_TRANSF configured!
#warning No RAD_TRANSF configured!
#endif

      return
      end subroutine forw_radiative_transfer

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      SUBROUTINE forward_model_pixel (  iu_main_output,     & ! IN
                                        RIN,OSHP,ipix,      &
                                        IWb,IWe,IWW,lresult,&
                                        tau_mol,            &
                                        NHVP_meas,HVP_meas, &
                                        nwl_pix,wl_pix,ind_wl,AP, & ! INOUT
                                        pixel_fit,          &
                                        pixel_vec_fit,      &
                                        GOUT_aerosol,       &
                                        GOUT_gases,         &
                                        GOUT_surface,       &
                                        GOUT_retrieval,     &
                                        ind_wl_i,           & !AL
                                        MDPR,               & ! added by Qiaoyun HU, molecular depolarization
                                        NANG,ANGL,          & ! OUT
                                        KERNELS1,KERNELS2   &
                                     )
      USE MOD_RT_SOS
      use mod_par_DLS, only : KMpar
      use mod_par_inv, only : KW,KMESS,KPARS,KSHAPE,KBF,  &
                              KIDIM2,KIDIM3,KVERTM
      use mod_par_OS, only : NMG,KSD,NBVM,NMM,KNT,NG0,HMAX_atm,KVERT_WD,N_WL_GLUT_MAX,N_SUB_CHANNEL_MAX,N_WL_CHANNEL_MAX
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_sdata, only : set_pixel_meas_vector_fs
      use mod_alloc_kernels
!XH   modules related to gas absorption
      use mod_abs_kd

      use mod_c_utils,  only: cstring2fstring

      use mod_bbgas_kd, only: GET_GAS_DATA,get_absorption,get_absorption_flags
      use sub_gas_kd,   only: DATA_GAS,NLEVEL_GAS, DATATM_GAS
      use mod_globals,  only: GBL_FILE_PATH_LEN
      use mod_forward_model_characteristics
      use mod_alloc_gas_lut, only: LUT_GASES,ATM_PROF,filter_channel_type, filter_transmission
      use mod_derivative_type_transport_model, only : TM

      implicit none
!	------------------------------------------------------------------------------------------------------
! IN :
      integer,                    intent(in)  ::  iu_main_output
      type(retr_input_settings),  intent(in)  ::  RIN
      type(OSH_par),              intent(in)  ::  OSHP
      integer,                    intent(in)  ::  IWW,IWb,IWe,ipix		                                            
      real,dimension(KW),         intent(in)  ::  tau_mol
      integer,                    intent(in)  ::  NHVP_meas ! number of heights for lidar measurements
      real,dimension(KVERTM),     intent(in)  ::  HVP_meas  ! heights for lidar measurements
      integer,                    intent(in)  ::  nwl_pix
      real,dimension(KW),         intent(in)  ::  wl_pix
      integer,dimension(KW),      intent(in)  ::  ind_wl, ind_wl_i
      real,dimension(KPARS),      intent(in)  ::  AP
!     lresult=.true. only single scattering properties are calculated for parameters retrieved at all wavelengths
      logical,                          intent(in)     :: lresult
      real,dimension(KW),               intent(in)     :: MDPR  ! molecular depolariztion ratio, added by Qiaoyun HU
      !logical,                          intent(in)     :: igab, ikdist

!    ------------------------------------------------------------------------------------------------------
! INOUT :
      type(output_segment_particles),   intent(inout)  :: GOUT_aerosol
      type(output_segment_gases),       intent(inout)  :: GOUT_gases
      type(output_segment_surface),     intent(inout)  :: GOUT_surface
      type(pixel),                      intent(inout)  :: pixel_fit
      type(kernels_triangle_bin),       intent(inout)  :: KERNELS1
      type(kernels_lognormal_bin),      intent(inout)  :: KERNELS2
      type(pixel_vector),               intent(inout)  :: pixel_vec_fit
      type(output_segment_retrieval),   intent(inout)  :: GOUT_retrieval
!	------------------------------------------------------------------------------------------------------
! OUT :
      integer,                    intent(out)  ::  NANG
      real,dimension(KMpar),      intent(out)  ::  ANGL
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      real                                             :: HOBS_km      ! height of observations
      real                                             :: HGR_km       ! height above sea level
      real                                             :: HMAX_atm_km  ! top of atmosphere
      real                                             :: inclination_angle  ! angle of the inclination of HVP_meas
      real,dimension(KVERTM)                           :: HVP_meas_km
      real,dimension(KPARS)                            :: APSING
      character (len=GBL_FILE_PATH_LEN)                :: internal_file_path
!	------------------------------------------------------------------------------------------------------
      type(forward_model_characteristics_particles)    :: forw_aerosol
      type(forward_model_characteristics_gases)        :: forw_gases
      type(forward_model_characteristics_surface)      :: forw_surface
!	------------------------------------------------------------------------------------------------------
      integer	                                         :: II,ISD,IW
      integer                     	                   :: IDIM1
!	------------------------------------------------------------------------------------------------------
      integer                      ::  iBRF_land,iBRP_land,iBRM_water
      real,dimension(KBF)	       ::  BRF_land, BRP_land, BRM_water
!	------------------------------------------------------------------------------------------------------
!      real (selected_real_kind(p=15)),dimension(RIN%NW) :: RREALMM,RIMAGMM
!      real (selected_real_kind(p=15)),dimension(RIN%NW) :: WAVELM
!      real (selected_real_kind(p=15))	::	rh_mxtr       ! Valid range: 0--1
!      real (selected_real_kind(p=15))	::	fract_inslbl  ! Volume fraction of insoluble inclusions
!      real (selected_real_kind(p=15))	::	fract_inslbl1 ! Volume fraction of insoluble inclusions 
!      real (selected_real_kind(p=15))	::	fract_soot    ! Volume fraction of mixture that is composed of soot (aka BC).
!      real (selected_real_kind(p=15))	::	fract_iron    ! Volume fraction of mixture that is composed of iron (aka Hematite).
!      real (selected_real_kind(p=15))	::	fract_slbl    ! Volume fraction of soluble inclusions 
!      real (selected_real_kind(p=15))	::	fract_wtr     ! Volume fraction of mixture that is composed of water.
!      character(12)                    :: instrument
!	------------------------------------------------------------------------------------------------------
      integer	                                       :: IB
!	------------------------------------------------------------------------------------------------------
      real,dimension(KSD)	                           :: RREAL, RIMAG
!	------------------------------------------------------------------------------------------------------
      logical		                                     :: status_funct
      integer, parameter                                 :: KANG=2*NG0+1
!	------------------------------------------------------------------------------------------------------
!     variables related to gas absorption
      type(DATA_ABS)                                   :: abs_data_forw_im
      type(DATA_GAS)                                   :: gas_abs_data_forw_im


      integer                                          :: K, IS, IC

      real, dimension(KW,N_SUB_CHANNEL_MAX)            :: bandwidth
      real,dimension(KW,N_SUB_CHANNEL_MAX)             :: wl_Subchannels
      integer,dimension(KW)                            :: NSubchannels
      logical                                          :: ISTDAT,ATMOS_EMIS,SOLAR_EMIS
      logical, dimension(KW)                           :: ihyper   !MH marks if the actual wl has to be treated as hyperspectral or it can be described with one line
      logical, dimension(KW)                           :: ifilter  !MH marks if the actual wl has a specific filter function taken from a file or just a squared filter
      character(GBL_FILE_PATH_LEN)                     :: VTP_PATH, KDIST_PATH
      real, dimension(NMG)                             :: GAS_C_REF
      logical                                          :: igab, ikdist
      
      type(filter_transmission),dimension(KW)          :: filters_trans
      real,dimension(KSD,N_SUB_CHANNEL_MAX)            :: RREAL_Subchannels,RIMAG_Subchannels
      REAL,dimension(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX)::    WL_Planck
      LOGICAL,dimension(KW,NMG)                        :: ISPECIE
      INTEGER,dimension(KW)                            :: NGAS_channel
!	------------------------------------------------------------------------------------------------------

      call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)

!	------------------------------------------------------------------------------------------------------
      ANGL(:)     = 0.0
!	------------------------------------------------------------------------------------------------------
      HOBS_km     = pixel_fit%HOBS * 0.001
      HGR_km      = pixel_fit%MASL * 0.001
      HMAX_atm_km = HMAX_atm       * 0.001
      do IW=1,RIN%NW
         if(ANY((pixel_fit%meas(IW)%meas_type(1:pixel_fit%meas(IW)%NIP) .GE. meas_type_lid_beg) .AND.&
                (pixel_fit%meas(IW)%meas_type(1:pixel_fit%meas(IW)%NIP) .LE. meas_type_lid_end))) then
            inclination_angle=pixel_fit%meas(IW)%SZA
            exit
         endif
      enddo !IW
      if (NHVP_meas .gt. 1) then
         HVP_meas_km(1:NHVP_meas) = HVP_meas(1:NHVP_meas) * 0.001
      endif
!	------------------------------------------------------------------------------------------------------
      select case(RIN%KL) 
      case(1)
        APSING(1:RIN%KNSING) = EXP(AP(1:RIN%KNSING))
      case(0) 
        APSING(1:RIN%KNSING) =     AP(1:RIN%KNSING)
      end select
!tl      DO II=1,RIN%KNSING
!tl      IF(APSING(II) .LT. RIN%APSMIN(II))  APSING(II) = RIN%APSMIN(II)
!tl      IF(APSING(II) .GT. RIN%APSMAX(II))  APSING(II) = RIN%APSMAX(II)
!tl      ENDDO ! II
	   			   
!           write(*,*) ' 3: forward_model_pixel APSMIN: '
!           write(*,'(10e13.4)') RIN%APSMIN(1:RIN%KNSING)
!           write(*,*) ' 3: forward_model_pixel APSMAX: '
!           write(*,'(10e13.4)') RIN%APSMAX(1:RIN%KNSING)

!al      write(*,*) 'in forward_model_pixel AP,APSING: '
!al      do ii=1,RIN%KNSING
!al      write(*,*) ii,exp(AP(ii)),APSING(ii)
!al      enddo 

 ! Unpack parameter vector AP (driving forward model)

      
      call unpack_parameter_vector_ap(  RIN, APSING, &
                                        forw_aerosol, &
                                        forw_gases, &
                                        forw_surface, &
                                        ipix, pixel_fit=pixel_fit, &      
                                        HGR_km=HGR_km, NHVP_meas=NHVP_meas,&
                                        HVP_meas_km=HVP_meas_km,&
                                        inclination_angle=inclination_angle )

      call set_gout_particles_parameters( RIN, forw_aerosol, &
                                          GOUT_retrieval%par%pixel(ipix))

goto 111
! Particles
write(*,*) 'Values after unpack_parameter_vector_ap '

write(*,*) '******** in forward_model_pixel'
do isd=1,RIN%NSD
  write(*,*) 'RADIUS: isd=',isd,'  C0=',forw_aerosol%C0(isd)
  write(*,'(10e14.6)') forw_aerosol%RADIUS(1:forw_aerosol%NBIN(isd),isd)
enddo
write(*,*) '******** in forward_model_pixel'
do isd=1,RIN%NSD
  write(*,*) 'SD:     isd=',isd
  write(*,'(10e14.6)') forw_aerosol%SD(1:forw_aerosol%NBIN(isd),isd)
enddo
write(*,*) '******** in forward_model_pixel'
do isd=1,RIN%NSD
  write(*,*) 'NSHAPE: isd=',isd
  write(*,'(2i5)') forw_aerosol%NSHAPE(isd)
enddo
write(*,*) '******** in forward_model_pixel'
do isd=1,RIN%NSD
  write(*,*) 'RATIO:     isd=',isd
  write(*,'(10e14.6)') forw_aerosol%RATIO(1:forw_aerosol%NSHAPE(isd),isd)
enddo
write(*,*) '******** in forward_model_pixel'
do isd=1,RIN%NSD
  write(*,*) 'SHD:     isd=',isd
  write(*,'(10e14.6)') forw_aerosol%SHD(1:forw_aerosol%NSHAPE(isd),isd)
enddo
write(*,*) '******** in forward_model_pixel'
if(RIN%use_tmodel) then
  ! transport model
  write(*,*) 'h_m:'
  write(*,'(10e14.6)') TM%h(1:TM%nlev,ipix)
do isd=1,RIN%NSD
  write(*,*) 'H0: isd=',isd,'  nlev=',RIN%TMSET%nlev
  write(*,'(10e14.6)') forw_aerosol%H0(1:TM%nlev,isd)
enddo
else
  write(*,*) 'HVP_retr_km:'
  write(*,'(10e14.6)') forw_aerosol%HVP_retr_km(1:forw_aerosol%NHVP_retr)
do isd=1,RIN%NSD
  write(*,*) 'H0: isd=',isd,'  NHVP_retr=',forw_aerosol%NHVP_retr
  write(*,'(10e14.6)') forw_aerosol%H0(1:forw_aerosol%NHVP_retr,isd)
enddo
endif ! RIN%use_tmodel
write(*,*) '******** in forward_model_pixel'
do isd=1,RIN%NSD
  write(*,*) 'profile std: isd=',isd
  write(*,'(10e14.6)') forw_aerosol%sigma(isd)
enddo
write(*,*) '******** in forward_model_pixel'
do isd=1,RIN%NSD
  write(*,*) 'RREALL: isd=',isd
  write(*,'(10e14.6)') forw_aerosol%RREAL(1:RIN%NW,isd)
enddo
write(*,*) '******** in forward_model_pixel'
do isd=1,RIN%NSD
  write(*,*) 'RIMAGL: isd=',isd
  write(*,'(10e14.6)') forw_aerosol%RIMAG(1:RIN%NW,isd)
enddo
stop 'test stop 1 in forward_model_pixel'
111 continue

goto 222
! Gases
write(*,*) '******** in forward_model_pixel'
  write(*,*) 'gas C:'
  write(*,'(10e14.6)') forw_gases%C(1:forw_gases%n)
stop 'test stop 2'
222 continue

goto 333
! Surface
write(*,*) '******** in forward_model_pixel'
do ii=1,4
  write(*,'(a,i0,2x,a)') 'i = ',ii,'BRF1_land:'
  write(*,'(10e14.6)') forw_surface%BRF_land(1:RIN%NW,ii)
enddo
write(*,*) '******** in forward_model_pixel'
do ii=1,4
  write(*,'(a,i0,2x,a)') 'i = ',ii,'BRP1_land:'
  write(*,'(10e14.6)') forw_surface%BRP_land(1:RIN%NW,ii)
enddo
do ii=1,4
  write(*,'(a,i0,2x,a)') 'i = ',ii,'BRM1_water:'
  write(*,'(10e14.6)') forw_surface%BRM_water(1:RIN%NW,ii)
enddo
stop 'test stop 3'
333 continue

goto 444
!Relative humidity
write(*,*) '******** in forward_model_pixel'
  write(*,'(a,e14.6)') 'RH =',forw_aerosol%RH
444 continue

!XH   get surface model type
   call get_radiative_transfer_SOS_flags_surf ( RIN,iBRF_land,iBRP_land,iBRM_water )

!MH These two subroutines need to be moved upward (inversion.f90) in future for computation eficiency

    call get_absorption_flags(RIN,                    & !IN
                              igab,                   & !OUT
                              ISTDAT,                 &
                              ikdist,                 &
                              VTP_PATH,               &
                              KDIST_PATH              &
                              )

    call get_absorption(RIN,                          &
                        igab,ihyper,                  &
                        ISTDAT,ikdist,                &
                        IWb,IWe,wl_pix,bandwidth,     &
                        VTP_PATH,                     &
                        Nsubchannels,                 &
                        WL_Subchannels,               &
                        ISPECIE,                      &
                        NGAS_channel,                 &
                        ifilter,                      &
                        KDIST_PATH                    &
                        )
                        
!     Calculate Optical Characteristics
      LOOP_WL:  DO IW=IWb,IWe

!MH Check if the actual wavelength is in the spectral range where emission is considered

    if(RIN%emission%threshold_for_starting_wavelength < wl_pix(IW)) then

            ATMOS_EMIS = RIN%emission%planck
            SOLAR_EMIS = RIN%emission%solar_irradiance

    else
            ATMOS_EMIS = .False.
            SOLAR_EMIS = .False.

    end if

    IF(igab .AND. IHYPER(IW) ) THEN

        !MH   Reads and format the information relative to gas absorption/kdistribution, including filters
        CALL GET_GAS_DATA(RIN,                            &
                          ipix,ind_wl(IW),IW,wl_pix(IW),  &
                          ikdist,IHYPER(IW),              &
                          ifilter(IW),                    &
                          bandwidth(IW,:),                &
                          Nsubchannels(IW),               &
                          WL_Subchannels(IW,:),           &
                          ISPECIE(IW,:),                  &
                          NGAS_channel(IW),               &
                          forw_gases%n,forw_gases%C,      &
                          gas_abs_data_forw_im,           &
                          GOUT_gases,                     &
                          filters_trans(IW),              &
                          WL_Planck                       &
                          )
                          

    END IF

!write(*,*)'OUT',gas_abs_data_forw_im%ABS_GS_WL(1:3)%NWL
!tl      IF(IWW .NE. 0 .AND. IWW .NE. ind_wl(IW)) CYCLE LOOP_WL
! Surface parameters at IW-th wave length 

!tl      IF(IWW .NE. 0 .AND. IWW .NE. ind_wl(IW)) CYCLE LOOP_WL 
! Surface parameters at IW-th wave length
      call get_SURF_wl( RIN,IW,ind_wl(IW),      &
                        forw_surface%BRF_land,  &
                        forw_surface%BRP_land,  &
                        forw_surface%BRM_water, &
                        BRF_land,BRP_land,BRM_water )

    
! Complex refractive index at IW-th wave length
      call set_gout_surface_parameters( ind_wl(IW), BRF_land, BRP_land, BRM_water, &
                                       GOUT_retrieval%par%pixel(ipix) )

!MH Get Complex refractive index at IW-th wave length from settings or Chemistry
      if(RIN%NSD .ne. 0) &
      call get_REFI_wl( RIN,                                        & !IN
                        IW,ind_wl(IW),                              &
                        Nsubchannels(IW),                           &
                        forw_aerosol%RH,                            &
                        bandwidth(IW,:),                            &
                        WL_Subchannels(IW,:),                       &
                        GOUT_aerosol%chem%pixel(ipix),              & !INOUT
                        forw_aerosol%RREAL,forw_aerosol%RIMAG,      &
                        RREAL_Subchannels,RIMAG_Subchannels,        & !OUT
                        RREAL,RIMAG)
!write(*,*)'RREAL out', 1, RREAL_Subchannels(:,1)
!write(*,*)'RIMAG out',  1, RIMAG_Subchannels(:,1)
!      if(RIN%NSD_clouds .ne. 0) &
!      call get_REFI_wl( RIN,GOUT_clouds%chem%pixel(ipix),IW,ind_wl(IW),ind_wl(1),  &
!                        forw_clouds%RREAL,forw_clouds%RIMAG,RREAL_clouds,RIMAG_clouds)
!write(*,*) 'in forward_model_pixel: iw     =',iw,'  ind_wl(IW)      =',ind_wl(IW)
!write(*,*) 'in forward_model_pixel: iw     =',iw,'  ind_wl_i(IW)      =',ind_wl_i(IW)
!write(*,*) 'in forward_model_pixel: iw     =',iw,'  WAVE(IW)    =',RIN%WAVE(ind_wl(IW))
!write(*,*) 'in forward_model_pixel: iw     =',iw,'  ind_wl(ind_wli(IW))      =',ind_wl(ind_wl_i(IW))
!write(*,*) 'in forward_model_pixel: iw     =',iw,'  wl(IW)      =',wl(IW)
!write(*,*) 'in forward_model_pixel: RREAL:',RREAL(1:RIN%NSD)
!write(*,*) 'in forward_model_pixel: RIMAG:',RIMAG(1:RIN%NSD)

! forward_model_pixel_wl output:
! - cross sections ext sca abs (1/um)
! - scattering matrix elements ph11, p12, ..., p44 (unitless)

        CALL forward_model_pixel_wl (                                                          &
                                      iu_main_output,                                          & ! IN
                                      RIN,ipix,                                                &
                                      igab,ihyper(IW),                                         &
                                      ikdist,ifilter(IW),                                      &
                                      istdat,ATMOS_EMIS,SOLAR_EMIS,                            &
                                      IW,wl_pix(IW),ind_wl(IW),lresult,                        &
                                      forw_aerosol%NBIN,forw_aerosol%RADIUS,forw_aerosol%SD,   &
                                      forw_aerosol%NSHAPE,forw_aerosol%RATIO,forw_aerosol%SHD, &
                                      RREAL,RIMAG,                                             &
                                      OSHP,                                                    &
                                      iBRF_land,iBRP_land,iBRM_water,                          &
                                      BRF_land,BRP_land,BRM_water,tau_mol(ind_wl(IW)),         &
                                      HOBS_km,HGR_km,HMAX_atm_km,                              &
                                      NHVP_meas,HVP_meas_km,                                   &
                                      forw_aerosol%NHVP_retr,forw_aerosol%HVP_retr_km,         &
                                      forw_aerosol%H0,forw_aerosol%sigma,forw_aerosol%CL(IW),  &
                                      abs_data_forw_im,                                        &
                                      gas_abs_data_forw_im,                                    &
                                      Nsubchannels(IW),                                        &
                                      WL_Subchannels(IW,:),                                    &
                                      WL_Planck,                                               &
                                      bandwidth(IW,:),                                         &
                                      filters_trans(IW),                                       &
                                      RREAL_Subchannels,RIMAG_Subchannels,                     &
                                      GAS_C_REF,LUT_GASES%NSPECIES,                            &
                                      forw_gases%n, forw_gases%C,                              &
                                      pixel_fit,                                               & ! INOUT
                                      GOUT_aerosol,                                            &
                                      GOUT_gases,                                              &
                                      GOUT_surface,                                            &
                                      pixel_fit%meas(IW)%MU(1),                                &
                                      tau_mol(ind_wl_i(IW)),                                   &
                                      ind_wl_i(IW),                                            &
                                      MDPR(IW),                                                &  ! modified by Qiaoyun HU
                                      NANG=NANG,ANGL=ANGL,                                     &  ! OUT
                                      KERNELS1=KERNELS1,                                       &
                                      KERNELS2=KERNELS2                                        &
                                      )

        if ( error_present() ) return
!write(*,*) 'after  forward_model_pixel: iw =',iw,'  iwb =',iwb,'  iwe =',iwe
      ENDDO LOOP_WL  ! IW

!	-------------------------------------------------------------------------------------
      if (.not. lresult)  &
         CALL set_pixel_meas_vector_FS (  RIN,IWb,IWe,ipix, & ! IN
                                          pixel_fit,        & ! INOUT
                                          pixel_vec_fit     &
                                       )
		 
!      IB=SUM(pixel_vec_fit%nFS(1:IWb))-pixel_vec_fit%nFS(IWb) ! number of elements in FPS before IWb-th wavelength 
!      II=SUM(pixel_vec_fit%nFS(IWb:IWe))     ! number of elements in FPS for wavelengths from IWb to IWe
!      do iw=1,pixel_vec_fit%KMIMAGE
!         write(*,*) iw,FPS(iw),'  - iw,pixel_vec_fit%FS(iw)'
!      enddo ! iw
!      write(*,*) IB,II,pixel_vec_fit%KMIMAGE,'  IB,II,KMIMAGE'

!      status_funct = check_nan(II,pixel_vec_fit%FS(IB+1:IB+II))
!      if(.not. status_funct) then
!		   write(*,*) 'NaN: after set_pixel_meas_vector_FS in forward_model_pixel'
!		   stop 'stop in forward_model_pixel'
!      endif
!      stop 'stop test after check_nan in forward_model_pixel_wl'

!	-------------------------------------------------------------------------------------

!tl      IF(RIN%KL .EQ. 1) AP(1:RIN%KNSING) = LOG(APSING(1:RIN%KNSING))
!tl      IF(RIN%KL .EQ. 0) AP(1:RIN%KNSING) =     APSING(1:RIN%KNSING)
!
      RETURN
      END SUBROUTINE forward_model_pixel

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine forw_single_scattering_particle_properties (                       &
                                                              iu_main_output,       &
                                                              RIN,NSD,NBIN,RADIUS,SD, & ! IN
                                                              KNLN,                 &
                                                              NSHAPE,RATIOS,SHD,    &
                                                              IW,WAVE,RREAL,RIMAG,  &
                                                              NANG,ANGL,ipix,       & ! OUT
                                                              GOUT_particles,       &
                                                              ext_norm,             &
                                                              KERNELS1,KERNELS2     &
                                                            )
      use mod_par_DLS,   only : KMpar
      use mod_par_inv,   only : KSHAPE,KIDIM3 
      use mod_par_OS,    only : KSD
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_alloc_arrays
      use mod_stop_report

      implicit none
!	------------------------------------------------------------------------------------------------------
      integer,                    intent(in)  ::  iu_main_output
      type(retr_input_settings),  intent(in)  ::  RIN
      integer,                    intent(in)  ::  ipix,IW,NSD
      integer,dimension(KSD),     intent(in)  ::  KNLN
      real,                       intent(in)  ::  WAVE
      integer,dimension(KSD),     intent(in)  ::  NSHAPE
      real,dimension(KSD),        intent(inout)  ::  RREAL,RIMAG
      real,dimension(KIDIM3,KSD), intent(in)  ::  RADIUS,SD	  
      real,dimension(KSHAPE,KSD), intent(in)  ::  RATIOS,SHD
      integer,dimension(KSD),     intent(in)  ::  NBIN
!	------------------------------------------------------------------------------------------------------
      type(output_segment_particles),intent(inout)  ::  GOUT_particles
      type(kernels_triangle_bin),    intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),   intent(inout)  ::  KERNELS2
      real,dimension(KSD),           intent(out)    ::  ext_norm
!	------------------------------------------------------------------------------------------------------
      integer,                    intent(out)  ::  NANG
      real,dimension(KMpar),      intent(out)  ::  ANGL      
!	------------------------------------------------------------------------------------------------------
      integer	                      ::	ISD, IDIM1, par_type
      real                          ::  sca, tiny_wl_models
      logical                       ::  use_models
      logical                       ::  IPRI_verbose
      logical                       ::  IPRI_additional_info
!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------
      use_models = .false.
      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)        
        if(par_type .gt. par_type_SD_beg .and. par_type .lt. par_type_SD_end) then
          if ( par_type .eq. par_type_SD_MD) then
            use_models = .true.
            exit
          endif
        endif
      enddo
      IPRI_verbose = RIN%IPRI_verbose
      IPRI_additional_info = RIN%IPRI_additional_info

      tiny_wl_models = RIN%tiny_wvl_models

      if ( .not. RIN%use_tmodel ) then
! forw_phase_matrix output:
! - cross sections ext sca abs (1/um)
! - scattering matrix elements ph11=f11, ph12=f12, ..., ph44=f44 (1/um)
        call forw_phase_matrix (                               &
                                  iu_main_output,              & ! IN
                                  IPRI_verbose,                &
                                  IPRI_additional_info,        &
                                  NSD,NBIN,RADIUS,SD,KNLN,     &
                                  NSHAPE,RATIOS,SHD,           &
                                  IW,WAVE,RREAL,RIMAG,         &
                                  use_models,                  &
                                  tiny_wl_models,              &
                                  NANG,ANGL,ipix,              & ! OUT
                                  GOUT_particles%opt%pixel(ipix)%wl(IW),  &
                                  GOUT_particles%phmx%pixel(ipix)%wl(IW), &
                                  ext_norm,                    &
                                  RIN%DLSF,KERNELS1,KERNELS2   &
                               )
                               
        if ( error_present() ) return
      else
!commit 723fa21a4ec31cf48eec7abad0260b734755a920
!Author: Tatsiana Lapionak <tatsiana.lapionak@univ-lille.fr>
!Date:   Wed Jun 30 10:35:29 2021 +0200
!
!    Transport model: UoL rh growth factor table. 
!
!The commit contains development (not finished) of single scattering property 
!calculation from external single scattering data base. 

          call forw_phase_matrix_transport_model (             &
                                  iu_main_output,              & ! IN
                                  IPRI_verbose,                &
                                  IPRI_additional_info,        &
                                  NSD,NBIN,RADIUS,SD,          &
                                  KNLN,                        &
                                  NSHAPE,RATIOS,SHD,           &
                                  IW,WAVE,RREAL,RIMAG,         &
                                  use_models,                  &
                                  tiny_wl_models,              &
                                  NANG,ANGL,ipix,              & ! OUT
                                  GOUT_particles%opt%pixel(ipix)%wl(IW),  &
                                  GOUT_particles%phmx%pixel(ipix)%wl(IW), &
                                  ext_norm,                    &
                                  RIN%DLSF,KERNELS1,KERNELS2   &
                                                )
          if ( error_present() ) return
      endif

      ! Set optical properties for output
      do ISD=1,NSD
        GOUT_particles%rind%pixel(ipix)%wl(IW)%mreal(ISD) = RREAL(ISD)
        GOUT_particles%rind%pixel(ipix)%wl(IW)%mimag(ISD) = RIMAG(ISD)

        sca = GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(ISD) * &
              GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(ISD)
        GOUT_particles%opt%pixel(ipix)%wl(IW)%aext(ISD) = &
                    GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(ISD) * &
                    (1.-GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(ISD))
        if(GOUT_particles%opt%pixel(ipix)%wl(IW)%aext(ISD) .lt. 0.0) then
          write(tmp_message,'(a,i0,3x,a,es11.4,3x,a)') &
          'isd = ',isd,'aext =',GOUT_particles%opt%pixel(ipix)%wl(IW)%aext(ISD), &
          'absorption can not be negative.'
          G_ERROR(trim(tmp_message))
        endif
! Phase matrix elements for output (P11, P12, ..., P44 (unitless))

        if(RIN%DLSF%keyEL .gt. 0) GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph11(1:NANG,ISD) =  &
                                 GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph11(1:NANG,ISD)/sca
        if(RIN%DLSF%keyEL .gt. 1) GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph12(1:NANG,ISD) =  & 
                                 GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph12(1:NANG,ISD)/sca
        if(RIN%DLSF%keyEL .gt. 2) GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(1:NANG,ISD) =  & 
                                 GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(1:NANG,ISD)/sca
        if(RIN%DLSF%keyEL .gt. 3) GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph33(1:NANG,ISD) =  & 
                                 GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph33(1:NANG,ISD)/sca
        if(RIN%DLSF%keyEL .gt. 4) GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph34(1:NANG,ISD) =  & 
                                 GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph34(1:NANG,ISD)/sca
        if(RIN%DLSF%keyEL .gt. 5) GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph44(1:NANG,ISD) =  & 
                                 GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph44(1:NANG,ISD)/sca
        !GOUT_particles%rind%pixel(ipix)%wl(IW)%mreal(ISD) = RREAL(ISD)
        !GOUT_particles%rind%pixel(ipix)%wl(IW)%mimag(ISD) = RIMAG(ISD)
      enddo ! ISD
      if(RIN%DLSF%keyEL .gt. 0) &
        GOUT_particles%lidar%pixel(ipix)%wl(IW)%lr(1:NSD)= 4.*3.14159265/  &
        ( GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(1:NSD)*  &
          GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph11(NANG,1:NSD) )
      if(RIN%DLSF%keyEL .gt. 2)  then
!        GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldpar(1:NSD) =          & ! AL TO DO assign correct values to LDPAR/LDPER
!        (1.-GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,1:NSD))/  &
!        (1.+GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,1:NSD))*100.
!        GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldper(1:NSD) =          &
!        (1.-GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,1:NSD))/  &
!        (1.+GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,1:NSD))*100.

!!!!******************************************************************************************************
!!!                 modified my Qiaoyun HU, ldpar --> P11,  ldper -->P22
!!!                 particle depoalrization ratio = (P11 - P22)/(P11 + P22)
!!!*******************************************************************************************************

        GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldpar(1:NSD)=GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph11(NANG,1:NSD)*    &
                            GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(1:NSD)*GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(1:NSD)
        GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldper(1:NSD)=GOUT_particles%phmx%pixel(ipix)%wl(IW)%ph22(NANG,1:NSD)*    &
                            GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(1:NSD)*GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(1:NSD)
      endif
!        WRITE(*,*),'----Forw_single_scattering_particles_properties-----'
!        WRITE(*,*),'sca=',GOUT_particles%opt%pixel(ipix)%wl(IW)%ssa(1:NSD)*GOUT_particles%opt%pixel(ipix)%wl(IW)%ext(1:NSD)
!        WRITE(*,*),'ldpar, ldper = '
!        WRITE(*,*),GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldpar(1:ISD),GOUT_particles%lidar%pixel(ipix)%wl(IW)%ldper(1:ISD)
!        WRITE(*,*),'                                                    '
      return
      end subroutine forw_single_scattering_particle_properties

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
    subroutine set_surface_parameter_vectors( RIN, &
                                              BRF_land, BRP_land, BRM_water, &
                                              surf_land_par_num, surf_land_par_vect, &
                                              surf_water_par_num, surf_water_par_vect )

      use mod_retr_settings_derived_type
      use mod_par_inv, only : KBF

      implicit none
! ............................................................................
      type(retr_input_settings),  intent(in)  ::  RIN
      real,dimension(KBF),    intent(in)  ::  BRF_land, BRP_land, BRM_water
      integer,dimension(2),   intent(out) ::  surf_land_par_num
      real, dimension(2*KBF), intent(out) ::  surf_land_par_vect
      integer,                intent(out) ::  surf_water_par_num
      real, dimension(KBF),   intent(out) ::  surf_water_par_vect
! ............................................................................
      integer ::  ns3
      integer :: IDIM1, par_type
! ............................................................................
      ! Vector of surface parametrs
      ! surf_par_num(1) - total number of surface parameters (BRDF+BPDF)
      ! surf_par_num(2) - number of SURF1 (BRDF) surface parameters
! ask PL
!tl Temporary solution if water parameters are not provided.
!tl if surf_water_par_num = 0 or surf_land_par_num = 0, there is a memory problem
!tl in call developpe_ocean_land: x_vect_water(1:n_par_water)
!tl                               x_vect_land(1:n_par_land)
!tl      surf_land_par_num(:)   = 0
      surf_land_par_num(:)   = 1
      surf_land_par_vect(:)  = 0.0
!tl      surf_water_par_num     = 0
      surf_water_par_num     = 1
      surf_water_par_vect(:) = 0.0

      do IDIM1=1,RIN%NDIM%n1
         par_type = RIN%NDIM%par_type(IDIM1)
         if (par_type .gt. par_type_SURF1_land_beg .and. par_type .lt. par_type_SURF1_land_end) then
            surf_land_par_num(1:2) = RIN%NDIM%n2(IDIM1)
            surf_land_par_vect(1:RIN%NDIM%n2(IDIM1)) = BRF_land(1:RIN%NDIM%n2(IDIM1))
          if(par_type .eq. par_type_SURF1_land_RPV_BRDF) then
            ns3 = 3
            surf_land_par_vect(ns3) = surf_land_par_vect(ns3) - 1.0
          endif
         else if (par_type .gt. par_type_SURF2_land_beg .and. par_type .lt. par_type_SURF2_land_end) then
            surf_land_par_vect(surf_land_par_num(1)+1:surf_land_par_num(1)+RIN%NDIM%n2(IDIM1)) = BRP_land(1:RIN%NDIM%n2(IDIM1))
            surf_land_par_num(1) = surf_land_par_num(1) + RIN%NDIM%n2(IDIM1)
         else if (par_type .gt. par_type_SURF_water_beg .and. par_type .lt. par_type_SURF_water_end) then
            surf_water_par_num = RIN%NDIM%n2(IDIM1)
            surf_water_par_vect(1:RIN%NDIM%n2(IDIM1)) = BRM_water(1:RIN%NDIM%n2(IDIM1))
         end if ! par_type .gt.
      end do ! IDIM1
      !write(*,*) 'surf_model:    ',surf_model(1:2)
      !write(*,*) 'surf_par_vect: ',surf_par_vect(1:sum(surf_par_num(1:2)))

    return
    end subroutine set_surface_parameter_vectors


      subroutine get_temperature_profile(DISCRVD,   &      !IN
                                         ATM_PROF,  &
                                         T_profile  &      !OUT
                                         )

       use sub_gas_kd, only : NLEVEL_GAS
       use mod_vertical_distr_derived_type
       use mod_intrpl_linear
       use mod_alloc_gas_lut, only: ATM_ATP_PROF
       IMPLICIT NONE

       type(discret_vertical_distribution), intent(IN)  ::  DISCRVD
       type(ATM_ATP_PROF),                  intent(IN)  ::  ATM_PROF
       real, dimension(NLEVEL_GAS),         intent(OUT) ::  T_profile


      integer :: i


        do i=1,DISCRVD%NH
            T_profile(i) = LINEAR( ATM_PROF%ALT(1:ATM_PROF%NLV), ATM_PROF%T(1:ATM_PROF%NLV), ATM_PROF%NLV, DISCRVD%h_km(i) )
        end do

     end subroutine get_temperature_profile


    subroutine ss_particle_properties_subch_int(Nsubchannels,                        &  !IN
                                                ipix,IW,NSD,NANG,                    &
                                                GOUT_aerosol_Subchannels,            &
                                                RREAL_Subchannels,RIMAG_Subchannels, &
                                                bandwidth,                           &
                                                RREAL, RIMAG,                        &
                                                GOUT_aerosol)               !OUT

    use mod_retr_general_output_derived_type
    use mod_par_OS,       only : N_SUB_CHANNEL_MAX

    implicit NONE

    integer,                           intent(in)     ::  Nsubchannels, ipix, IW, NSD, NANG
    real,DIMENSION(N_SUB_CHANNEL_MAX), intent(in)     ::  bandwidth
    type(output_segment_particles),    intent(in)     ::  GOUT_aerosol_Subchannels
    real,dimension(KSD,N_SUB_CHANNEL_MAX),intent(inout)  ::  RREAL_Subchannels,RIMAG_Subchannels
    
    real,dimension(KSD),               intent(inout)  ::  RREAL, RIMAG
    type(output_segment_particles),    intent(inout)  ::  GOUT_aerosol


    integer :: ISUB, ISD

    GOUT_aerosol%opt%pixel(ipix)%wl(IW)%ssa(:) = 0.0
    GOUT_aerosol%opt%pixel(ipix)%wl(IW)%ext(:) = 0.0
    GOUT_aerosol%opt%pixel(ipix)%wl(IW)%aext(:) = 0.0
    GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph11(1:NANG,:) = 0.0
    GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph12(1:NANG,:) = 0.0
    GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph22(1:NANG,:) = 0.0
    GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph33(1:NANG,:) = 0.0
    GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph34(1:NANG,:) = 0.0
    GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph44(1:NANG,:) = 0.0

    GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%lr(1:NSD) = 0.0
    GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%ldpar(1:NSD) = 0.0
    GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%ldper(1:NSD) = 0.0

    RREAL(1:NSD) = 0.0
    RIMAG(1:NSD) = 0.0

    DO ISD=1,NSD

        DO ISUB=1,Nsubchannels

            RREAL(ISD) = RREAL(ISD) + (RREAL_Subchannels(ISD,ISUB)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            RIMAG(ISD) = RIMAG(ISD) + (RREAL_Subchannels(ISD,ISUB)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))

            GOUT_aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD) = GOUT_aerosol%opt%pixel(ipix)%wl(IW)%ssa(ISD) + (GOUT_aerosol_Subchannels%opt%pixel(ipix)%wl(ISUB)%ssa(ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))

            GOUT_aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD) = GOUT_aerosol%opt%pixel(ipix)%wl(IW)%ext(ISD) + (GOUT_aerosol_Subchannels%opt%pixel(ipix)%wl(ISUB)%ext(ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%opt%pixel(ipix)%wl(IW)%aext(ISD) = GOUT_aerosol%opt%pixel(ipix)%wl(IW)%aext(ISD) + (GOUT_aerosol_Subchannels%opt%pixel(ipix)%wl(ISUB)%aext(ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph11(1:NANG,ISD) = GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph11(1:NANG,ISD) + (GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl(ISUB)%ph11(1:NANG,ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph12(1:NANG,ISD) = GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph12(1:NANG,ISD) + (GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl(ISUB)%ph12(1:NANG,ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph22(1:NANG,ISD) = GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph22(1:NANG,ISD) + (GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl(ISUB)%ph22(1:NANG,ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph33(1:NANG,ISD) = GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph33(1:NANG,ISD) + (GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl(ISUB)%ph33(1:NANG,ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph34(1:NANG,ISD) = GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph34(1:NANG,ISD) + (GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl(ISUB)%ph34(1:NANG,ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph44(1:NANG,ISD) = GOUT_aerosol%phmx%pixel(ipix)%wl(IW)%ph44(1:NANG,ISD) + (GOUT_aerosol_Subchannels%phmx%pixel(ipix)%wl(ISUB)%ph44(1:NANG,ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))

            GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%lr(ISD) = GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%lr(ISD) + (GOUT_aerosol_Subchannels%lidar%pixel(ipix)%wl(ISUB)%lr(ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%ldpar(ISD) = GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%ldpar(ISD) + (GOUT_aerosol_Subchannels%lidar%pixel(ipix)%wl(ISUB)%ldpar(ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%ldper(ISD) = GOUT_aerosol%lidar%pixel(ipix)%wl(IW)%ldper(ISD) + (GOUT_aerosol_Subchannels%lidar%pixel(ipix)%wl(ISUB)%ldper(ISD)*bandwidth(ISUB)/SUM(bandwidth(1:Nsubchannels)))
            
        END DO

    END DO



     end subroutine ss_particle_properties_subch_int


    end module mod_forward_model

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
		subroutine bhr_iso_segment( RIN, segment, GOUT )

      use mod_par_inv, only : KPARS, KW, KBF
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_sdata_meas_type
      use mod_forward_model_characteristics
      use mod_forward_model, only : set_surface_parameter_vectors
      use Mod_BRM, only : BHRiso
      use mod_stop_report

      implicit none
! ............................................................................
      type(retr_input_settings),    intent(in)    ::  RIN
      type(segment_data),           intent(in)    ::  segment
      type(output_segment_general), intent(inout) ::  GOUT
! ............................................................................
      integer :: iBRF_land, iBRP_land, iBRM_water
      integer, dimension(2)  ::  surf_land_par_num
      real, dimension(2*KBF) ::  surf_land_par_vect
      integer                ::  surf_water_par_num
      real, dimension(KBF)   ::  surf_water_par_vect

      integer :: IDIM1, ipix, iw
      integer :: nwl, par_type
      real, dimension(KPARS) :: APSING
      real,dimension(KBF) :: BRF_land, BRP_land, BRM_water
      type(forward_model_characteristics_surface) :: forw_surface
      real :: BHR_iso
      real(8) :: land_percent
! ............................................................................
      nwl = RIN%nw

      call get_radiative_transfer_SOS_flags_surf ( RIN, iBRF_land, iBRP_land, iBRM_water )

loop_pixel: do ipix=1,segment%npixels

      call initialize_forward_model_characteristics_surface(forw_surface)
      APSING(1:RIN%KNSING) = GOUT%retrieval%par%pixel(ipix)%par(1:RIN%KNSING)
      land_percent = dble(segment%pixels(ipix)%land_percent)

      do IDIM1=1,RIN%NDIM%n1
        par_type = RIN%NDIM%par_type(IDIM1)
        if(par_type .gt. par_type_surface_beg .and. par_type .lt. par_type_surface_end) then
            call unpack_spectral_SURF (RIN, IDIM1, APSING, forw_surface)
        endif
      enddo ! IDIM1

loop_wl: do iw=1,nwl

      call get_SURF_wl( RIN, IW, IW,            &
                        forw_surface%BRF_land,  &
                        forw_surface%BRP_land,  &
                        forw_surface%BRM_water, &
                        BRF_land, BRP_land, BRM_water )
      if ( error_present() ) return
      call set_surface_parameter_vectors( RIN, &
                                          BRF_land, BRP_land, BRM_water, &
                                          surf_land_par_num,  surf_land_par_vect, &
                                          surf_water_par_num, surf_water_par_vect )

      call BHRiso(iBRF_land, iBRP_land, iBRM_water, land_percent, &
                  surf_land_par_num(1), surf_land_par_num(2), &
                  dble(surf_land_par_vect(1:surf_land_par_num(1))), &
                  surf_water_par_num, &
                  dble(surf_water_par_vect(1:surf_water_par_num)), &
                  dble(RIN%WAVE(iw)), &
                  BHR_iso )
      GOUT%surface%pixel(ipix)%wl(iw)%bhr_iso = BHR_iso
enddo loop_wl

enddo loop_pixel

    return
    end subroutine bhr_iso_segment

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine set_tracer_average_sca_matrix ( nel, nang,    &
                                                opt_pixel_wl, &
                                                phmx_pixel_wl &
                                              )

      use mod_globals, only : sp
      use mod_par_DLS, only : KMpar
      use mod_par_inv, only :
      use mod_retr_general_output_derived_type

      use mod_derivative_type_transport_model, only : TM, tracer_average
      use mod_derivative_type_tracer_average, only : TRCAVG

      implicit none
!...................................................................................
      integer, intent(in) :: nel, nang
      type(output_pixel_opt_wl), dimension(1), intent(out) ::  opt_pixel_wl
      type(output_pixel_ph_matrix_wl), dimension(1), intent(out) ::  phmx_pixel_wl

!...................................................................................
      integer :: ntrc ! ntrc = nlev
      integer :: j, iel, itrc
!...................................................................................

      ntrc = TRCAVG%nlev

      do iel=0,nel
        select case ( iel )
        case ( 0 )
          opt_pixel_wl(1)%ext(:) = 0.0_sp
          opt_pixel_wl(1)%aext(:) = 0.0_sp
          opt_pixel_wl(1)%ssa(:) = 0.0_sp
          do itrc=1,ntrc
          opt_pixel_wl(1)%ext(itrc) = TRCAVG%ext(itrc)
          opt_pixel_wl(1)%aext(itrc) = TRCAVG%ext(itrc) - TRCAVG%sca(itrc)
          opt_pixel_wl(1)%ssa(itrc) = TRCAVG%ssa(itrc)
          enddo
        case ( 1 )
          phmx_pixel_wl(1)%ph11(:,:) = 0.0_sp
          do itrc=1,ntrc
          phmx_pixel_wl(1)%ph11(1:nang,itrc) = TRCAVG%ph11(1:nang,itrc)
          enddo
        case ( 2 )
          phmx_pixel_wl(1)%ph12(:,:) = 0.0_sp
          do itrc=1,ntrc
          phmx_pixel_wl(1)%ph12(1:nang,itrc) = TRCAVG%ph12(1:nang,itrc)
          enddo
        case ( 3 )
          phmx_pixel_wl(1)%ph22(:,:) = 0.0_sp
          do itrc=1,ntrc
          phmx_pixel_wl(1)%ph22(1:nang,itrc) = TRCAVG%ph22(1:nang,itrc)
          enddo
        case ( 4 )
          phmx_pixel_wl(1)%ph33(:,:) = 0.0_sp
          do itrc=1,ntrc
          phmx_pixel_wl(1)%ph33(1:nang,itrc) = TRCAVG%ph33(1:nang,itrc)
          enddo
        case ( 5 )
          phmx_pixel_wl(1)%ph34(:,:) = 0.0_sp
          do itrc=1,ntrc
          phmx_pixel_wl(1)%ph34(1:nang,itrc) = TRCAVG%ph34(1:nang,itrc)
          enddo
        case ( 6 )
          phmx_pixel_wl(1)%ph44(:,:) = 0.0_sp
          do itrc=1,ntrc
          phmx_pixel_wl(1)%ph44(1:nang,itrc) = TRCAVG%ph44(1:nang,itrc)
          enddo
        end select
      enddo ! iel

      return
      end subroutine set_tracer_average_sca_matrix
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss




