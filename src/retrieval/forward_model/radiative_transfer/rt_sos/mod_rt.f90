! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **
#include "../../../constants_set/mod_globals.inc"
      module mod_rt

      use mod_stop_report

      contains
!
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      SUBROUTINE forw_SOS_RT (  iFlux,igab,ATMOS_EMIS, SOLAR_EMIS,       & ! IN
                                IW, WAVE, ind_wl, IWLGAS, IP,            &
                                OSHP,                                    &
                                iBRF_land,iBPF_land,iBRM_water,          &
                                surf_land_par_num, surf_land_par_vect,   &
                                surf_water_par_num, surf_water_par_vect, &
                                tau_mol,EXT_gas,                         &
                                HOBS_km, HGR_km, HMAX_atm_km,            &
                                DISCRVD,                                 &
                                laerosol, lsurface,                      &
                                NANG, ANGL,                              &
                                SD,ext_norm,                             &
                                abs_data_forw_im,                        &
								
                                pixel_fit,                               &
                                WL_Subchannels,                          &
                                T_profile,STEMP,                         &
                                 
                                GOUT_aerosol_opt_pixel_wl,               & ! INOUT
                                GOUT_aerosol_phmx_pixel_wl,              &
                                GOUT_surface_pixel_wl,                   &
                                GOUT_bbflux_pixel,                       &

                                NBV_comb,                                & ! OUT
                                SLout_comb, SQout_comb,                  &
                                SUout_comb, SLPout_comb,                 &
                                external_file_path                       &
                              )
      use mod_globals, only  : GBL_FILE_PATH_LEN
      USE MOD_RT_SOS, only : rad_sp, radiative_transfer_SOS
      USE MOD_RT_SOS_SETUP, ONLY : RT_SOS_CTL, RT_SOS_SET
      use mod_par_DLS,   only : KMpar
      use mod_par_inv,   only : KSHAPE, KBF, KIP, KIDIM3
      use mod_par_OS,    only : NMG, KSD, NBVM, NMM, KNT, NG0T, KVERT_WD
      use mod_vertical_distr_derived_type
!XH   modules related to gas absorption
      use mod_abs_kd

      use mod_retr_general_output_derived_type
      use mod_sdata
      use mod_inversion_utils, only : check_nan
      use mod_intrpl_linear
      use mod_alloc_kernels
      use mod_vertical_distr_derived_type

      use sub_gas_kd, only : NLEVEL_GAS

      implicit none
!	------------------------------------------------------------------------------------------------------
! IN:
!XH   switch for broadband flux calculation
      logical,                    intent(in)  ::  iFlux, igab,ATMOS_EMIS, SOLAR_EMIS
      integer,                    intent(in)  ::  IW,ind_wl,IP
      integer,                    intent(in)  ::  IWLGAS  !MH   Current gas wavelength
      real,                       intent(in)  ::  HOBS_km,HGR_km,HMAX_atm_km                            
      real,                       intent(in)  ::  WAVE, WL_Subchannels
      type(OSH_par),              intent(in)  ::  OSHP
      real,                       intent(in)  ::  tau_mol,EXT_gas
      type(discret_vertical_distribution), intent(inout) :: DISCRVD
!XH   data of gas absorption
      type (DATA_ABS),            intent(in)  ::  abs_data_forw_im
      type(pixel),                intent(in)  ::  pixel_fit
      integer,                    intent(in)  ::  NANG
      real,dimension(KMpar),      intent(in)  ::  ANGL
      integer,                    intent(in)  ::  iBRF_land,iBPF_land,iBRM_water
      integer,dimension(2),       intent(in)  ::  surf_land_par_num
      real, dimension(2*KBF),     intent(in)  ::  surf_land_par_vect
      integer,                    intent(in)  ::  surf_water_par_num
      real, dimension(KBF),       intent(in)  ::  surf_water_par_vect

      real,dimension(KIDIM3,KSD), intent(in)  ::  SD
      real,dimension(KSD),        intent(in)  ::  ext_norm
      character (len=GBL_FILE_PATH_LEN), intent(in)  ::  external_file_path
      real,dimension(NLEVEL_GAS), intent(in)  ::  T_profile
      real,                       intent(in)  ::  STEMP
!	------------------------------------------------------------------------------------------------------
! INOUT:
      type(output_pixel_opt_wl),      intent(inout)  ::  GOUT_aerosol_opt_pixel_wl
      type(output_pixel_ph_matrix_wl),intent(inout)  ::  GOUT_aerosol_phmx_pixel_wl
      type(output_pixel_surface_wl),  intent(inout)  ::  GOUT_surface_pixel_wl
      type(output_pixel_bbflux),      intent(inout)  ::  GOUT_bbflux_pixel
!	------------------------------------------------------------------------------------------------------
! OUT:
      integer,                 intent(out) ::  NBV_comb
      real, dimension(2*NBVM), intent(out) ::  SLout_comb, SQout_comb, SUout_comb, SLPout_comb
!	------------------------------------------------------------------------------------------------------
! LOCAL :
      integer	                    ::  ISD, IV, i1, i2, i3, ii
      real,dimension(2*NBVM)      ::  ASCAT
!	------------------------------------------------------------------------------------------------------
      logical                       ::	status_funct 
!	------------------------------------------------------------------------------------------------------
      integer                       ::  meas_type
      integer                       ::  NBV
      real, dimension(NBVM,KIP)     ::  vis,fiv
      real, dimension(NMM+NMG)      ::  EXT_os, SSA_os
      integer                       ::  NQDR, NT1(1:2)
      real                          ::  sza, gteta, dhr
!	------------------------------------------------------------------------------------------------------   
      integer                         ::  natm, naer, nmol
      integer, dimension(NMM+NMG)     ::  nhinp
      real, dimension(KVERT_WD,NMM+NMG) ::  hinp_km
      real, dimension(KVERT_WD,NMM+NMG) ::  vdinp
      integer :: nhinp_max
      logical ::  laerosol, lsurface,lsurface_in
      integer ::  IDIM1,par_type
!	------------------------------------------------------------------------------------------------------
! gas absorption
      integer                       ::  ngas
      integer                       ::  curr_wl_index
      integer,dimension(NCORPS)     ::  ns
      real,dimension(KNT)           ::  UFX_tmp, DFX_tmp, UFX0_tmp, DFX0_tmp
      character(len=20), dimension(NMM+NMG) :: distr_type
!	------------------------------------------------------------------------------------------------------
      real,dimension(2*NBVM)        ::  SQout_tmp,SUout_tmp,SLPout_tmp,SLout_tmp
      real, dimension(2*NBVM)       ::  vis_comb,fiv_comb  
      logical                       ::  lexist
      character(LEN=15) :: npar_land_str, npar_water_str
      character(LEN=GBL_FILE_PATH_LEN) :: cfmt
!	------------------------------------------------------------------------------------------------------
! timer
      real,save                     ::  xtime=0.0
      real                          ::  time_start,time_finish
      logical,save					        ::  IDWN_save


      logical 						          ::  combined_up_down=.true.
!	------------------------------------------------------------------------------------------------------
!     IW     - index of wave length in array of wave length for current pixel
!     WAVE   - value of wave length for index IW from array of wave length for current pixel
!     ind_wl - index of wave length in general array of wave length for inversion
!     write(*,*) 'in forw_IMAGE_I_IW: iw =',iw,WAVE,ind_wl
!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------

      nhinp_max = KVERT_WD

      distr_type(:) = 'lut'

      natm = DISCRVD%natm
      naer = DISCRVD%naer
      nmol = DISCRVD%nmol

      EXT_os(:) = 0.0
      SSA_os(:) = 0.0
!	------------------------------------------------------------------------------------------------------
! Extinction and single scattering albedo as an input for SOS_RT
!     aerosol
      do ISD=1,naer
        if(RT_SOS_SET%IP_VERBOSE) then
        if(GOUT_aerosol_opt_pixel_wl%EXT(ISD) .ge. 70.0) then
          write(*,'(a)') '!!! AOD is too high before call radiative_transfer_SOS !!!'
          write(*,'(a,i0,a,f11.4)') 'isd = ',isd,'  aod = ',GOUT_aerosol_opt_pixel_wl%EXT(ISD)
          call print_crash_report()
          !stop 'stop in forw_SOS_RT'
        endif
        endif
        EXT_os(ISD) = GOUT_aerosol_opt_pixel_wl%EXT(ISD)
        SSA_os(ISD) = GOUT_aerosol_opt_pixel_wl%SSA(ISD)
      enddo ! ISD
!      write(*,*) 'before OS_H: EXT_os(1:naer)  ',EXT_os(1:naer)
!      write(*,*) 'before OS_H: SSA_os(1:naer)  ',SSA_os(1:naer)
!      print *, wave, sum(ext_os(1:naer))
!	------------------------------------------------------------------------------------------------------
!     molecular
      EXT_os(naer+nmol) = tau_mol
      SSA_os(naer+nmol) = 1.0
!	------------------------------------------------------------------------------------------------------
      if (iFlux) then
!XH      checking if we need absorption calculation for this wavelength
         curr_wl_index = 0
         do ii = 1, abs_data_forw_im%NWL
            if (abs_data_forw_im%ABS_GS_WL(ii)%WVL .eq. WAVE) then
!XH            considering gas absorption for broadband flux calculation
              ngas = 1
              RT_SOS_CTL%IFLX = .TRUE.
              if (ngas .gt. NMG) then
                write(*,'(2(a,i0))') 'ngas = ',ngas,'  NMG = ',NMG
                write(*,'(a)') 'Number of gas component > NMG parameter.'
                stop 'stop in forw_SOS_RT'
              end if ! ngas .gt. NMG
              curr_wl_index = ii
            exit
            end if
         end do ! ii
      else
         ngas = 0
         RT_SOS_CTL%IFLX = .FALSE.
      end if
!	------------------------------------------------------------------------------------------------------
      if (igab) then
        EXT_os(natm) = EXT_gas     !MH Gas extintction is sum up here to run RT over ordinary route
        SSA_os(natm) = 0.
      else
        !  gas provided in sdata structure
        if((.not. RT_SOS_CTL%IFLX))  then
        if (pixel_fit%IFGAS .eq. 1) then
          SSA_os(1) = EXT_os(1)*SSA_os(1)
          EXT_os(1) = EXT_os(1)+pixel_fit%meas(IW)%gaspar
          SSA_os(1) = SSA_os(1)/EXT_os(1)
        end if !IFGAS
        endif !IFLX,
      endif

      NT1(1:2)=RT_SOS_SET%NLYR(1:2)+1

      if (iFlux) then
         sza = abs_data_forw_im%SZA
         NBV_comb = 1
         vis_comb(1) = 0.0
         fiv_comb(1) = 0.0
      else
!MH		Just variable assigment from  pixel_fit

         call get_pixel_geom ( IW, IP, pixel_fit, NBV, sza, vis(:,IP), fiv(:,IP) )
         NBV_comb = NBV
!MH		 Masurement Zenith angle
         vis_comb(1:NBV) = vis(1:NBV,IP)
!MH		 Masurement Azimuth angle
         fiv_comb(1:NBV) = fiv(1:NBV,IP)
		 
         meas_type = pixel_fit%meas(IW)%meas_type(IP+1)
         if (meas_type .gt. meas_type_SvR_beg .and. meas_type .lt. meas_type_SvR_end) then
!XH         if there are polarized measurements
            call get_pixel_geom (IW,IP+1,      &
                                 pixel_fit,    &
                                 NBV,          &
                                 sza,          &
                                 vis(:,IP+1),  &
                                 fiv(:,IP+1)   &
                                )
            if ( (NBV .ne. NBV_comb) .or.   &
                 (NBV .eq. NBV_comb .and.   &
                 (any(vis(1:NBV,IP+1) .ne. vis_comb(1:NBV)) .or.   &
                  any(fiv(1:NBV,IP+1) .ne. fiv_comb(1:NBV))) ) ) then
               vis_comb(NBV_comb+1:NBV_comb+NBV) = vis(1:NBV,IP+1)
               fiv_comb(NBV_comb+1:NBV_comb+NBV) = fiv(1:NBV,IP+1)
               NBV_comb = NBV_comb+NBV
            end if ! NBV .ne. NBV_comb .or.
         end if ! meas_type .gt.

         if (any(cos(vis_comb(1:NBV_comb)*rad_sp) .lt. 0.)) then
!       Ground based observations (SunPhotometer)
          IDWN_save = .TRUE.
         else
!       Airborne observations
          IDWN_save = .FALSE.		  
         end if

      end if ! iFlux

      RT_SOS_CTL%IDWN = IDWN_save

!PL Now RT_SOS_CTL%IDWN is defined from the last "vis" value in a pixel.
!PL if pixel contain "vis" value both in the ranges 0-90 and 90-180

!PL further code modifications are necessary 	  

!MH   Selects number of quadrature points
!MH   These lines are commented beacause they come from a conflict with dev merging and I am not sure if this is the correct solution
! 	  if (RT_SOS_CTL%IDWN) then
!      NQDR = NG0T

!PL further code modifications are necessary
	  if (RT_SOS_CTL%ICMB) then
         NQDR = NG0T
!		 if (RT_SOS_CTL%IDWN) then
!		   	lsurface_in=lsurface
!			lsurface=.false.
!		 endif

	  else
	  	if (RT_SOS_CTL%IDWN) then
        NQDR = NG0T
	  	else
     		NQDR = OSHP%NG
		endif
	  endif
	  
!MH	  ISGL: Only single scattering calculation
!MH	  IJCB: Jacobian calculation approach	  
!MH	  ISRF: Take into account surface in RT
!MH	  IWUT: Look Up Table approach

      SELECT CASE (OSHP%IMSC)
      CASE (0)
          RT_SOS_CTL%ISGL = .FALSE.
          RT_SOS_CTL%IJCB = .FALSE.
      CASE (1)
          RT_SOS_CTL%ISGL = .TRUE.
          RT_SOS_CTL%IJCB = .FALSE.
      CASE (2)
          RT_SOS_CTL%ISGL = .FALSE.
          RT_SOS_CTL%IJCB = .TRUE.
      END SELECT ! OSHP%IMSC

      IF (RT_SOS_SET%IWUT) THEN
          RT_SOS_CTL%ISRF = .FALSE.
      ELSE
          RT_SOS_CTL%ISRF = .TRUE.
      END IF

!!XH   test without surface
!      RT_SOS_CTL%ISRF = .FALSE.

      RT_SOS_CTL%IVEC = RT_SOS_SET%IVEC .AND. .NOT. RT_SOS_CTL%IFLX

!      write(*,*) 'sza=',sza
!      write(*,*) '1: vis:'
!      write(*,'(10f16.4)') vis_comb(1:NBV_comb)
!      write(*,*) '1: fiv:'
!      write(*,'(10f16.4)') fiv_comb(1:NBV_comb)

!      write(*,'(a)') 'Vertical distributions:'
!      do iv=1,DISTRVD%nh
!         write(*,'(a,i0,f12.4,10e12.4)') 'iv, h_km(iv), VD(iv,1:natm)  ', &
!         iv,DISTRVD%h_km(iv),DISTRVD%val(iv,1:natm)
!      end do ! iv

      call CPU_time(time_start)
      if (RT_SOS_CTL%IFLX) then
!XH      k distribution computation
         ns = abs_data_forw_im%ABS_GS_WL(curr_wl_index)%NEXP
         SLout_tmp  = 0.0
         SQout_tmp  = 0.0
         SUout_tmp  = 0.0
         UFX_tmp = 0.0
         DFX_tmp = 0.0
         UFX0_tmp= 0.0
         DFX0_tmp= 0.0
      else
         ns = 1
      end if ! RT_SOS_CTL%IFLX

      DO i1=1,ns(1)
      DO i2=1,ns(2)
      DO i3=1,ns(3)
         IF(RT_SOS_SET%IP_ADDITION)  THEN
             !WRITE(*,*) 'MOD_RT: Print function is up to date!'
             !WRITE(*,*) 'MOD_RT: IW =',IW,'  IMSC =',OSHP%IMSC 
             CALL write_rad_transf_input (                                    &
                                          IW, OSHP%IMSC,                      &
                                          OSHP%NG, OSHP%NN, OSHP%NF,          &
                                          iBRF_land, iBPF_land, iBRM_water,   &
                                          pixel_fit%land_percent,             &
                                          NQDR, NT1,                          &
                                          sza, NBV_comb, vis_comb, fiv_comb,  &
                                          WAVE,                               &
                                          surf_land_par_num(1),               &
                                          surf_land_par_num(2),               &
                                          surf_land_par_vect,                 &
                                          surf_water_par_num,                 &
                                          surf_water_par_vect,                &
                                          EXT_os, SSA_os,                     &
                                          NANG, ANGL,                         &
                                          SD, ext_norm,                       &
                                          HOBS_km, HGR_km, HMAX_atm_km,       &
                                          DISCRVD,                            &
                                          laerosol, lsurface,                 &
                                          GOUT_aerosol_phmx_pixel_wl%ph11,    &
                                          GOUT_aerosol_phmx_pixel_wl%ph12,    &
                                          GOUT_aerosol_phmx_pixel_wl%ph22,    &
                                          GOUT_aerosol_phmx_pixel_wl%ph33,    &
                                          external_file_path                  &
                                         )
         ENDIF ! RT_SOS_SET%IP_ADDITION



         if (RT_SOS_CTL%IFLX) then
            nhinp(1:natm) = DISCRVD%nh
            do isd=1,natm
              hinp_km(:,isd) = DISCRVD%h_km(:)
              vdinp(:,isd) = DISCRVD%val(:,isd)
            enddo
            ! gas segment profile
            call ABS_GAS_PROFILE (  abs_data_forw_im,                   & ! IN
                                    curr_wl_index,                      &
                                    i1, i2, i3,                         &
                                    nhinp_max,                          &
                                    nhinp(natm+ngas),                   &  ! OUT
                                    hinp_km(1:nhinp_max,natm+ngas),     &
                                    vdinp(1:nhinp_max,natm+ngas),       &
                                    EXT_os(natm+ngas)                   &
                                 )
!XH         do not consider atmospheric profile for zero optical thickness
            if (EXT_os(natm+ngas) .eq. 0.0) then
              ngas = 0
            endif

            DISCRVD%natm = natm + ngas
            DISCRVD%ngas = 0

            ! Recompute all discret profiles of atmospheric components at altitudes for fluxes
            ! above nhinp_max = KVERT_WD
            ! above nhinp_max = DISCRVD%nh

            call discret_vertical_distr_with_gas ( HGR_km, HMAX_atm_km, distr_type, &   !IN
                                                  0.0, 0.0, nhinp_max, nhinp(:),    &
                                                  hinp_km(1:nhinp_max,:),           &
                                                  vdinp(1:nhinp_max,:),             &
                                                  DISCRVD )                             !INOUT

            DISCRVD%ngas = ngas

!XH         calculate broadband flux without aerosol
            RT_SOS_CTL%IAER = .FALSE.
            CALL radiative_transfer_SOS (  ATMOS_EMIS, SOLAR_EMIS,                                       & ! IN
                                           IW,OSHP%NG,OSHP%NN,OSHP%NF,                                   &
                                           iBRF_land,iBPF_land,iBRM_water,pixel_fit%land_percent,        &
                                           NQDR, NT1,                                                    &
                                           sza, NBV_comb, vis_comb(:), fiv_comb(:),                      &
                                           WAVE,                                               &
                                           surf_land_par_num(1),surf_land_par_num(2),surf_land_par_vect, &
                                           surf_water_par_num,surf_water_par_vect,                       &
                                           EXT_os, SSA_os,                                               &
                                           NANG, ANGL,                                                   &
                                           SD,ext_norm,                                                  &
                                           HOBS_km, HGR_km, HMAX_atm_km, DISCRVD,                        &
                                           T_profile, STEMP,                                             &
                                           laerosol,lsurface,                                            &
                                           GOUT_aerosol_phmx_pixel_wl%ph11(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph12(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph22(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph33(:,:),                         &
                                           ASCAT,                                                        &
                                           SLout_comb,SQout_comb,                                        & ! OUT
                                           SUout_comb,SLPout_comb,                                       &
                                           dhr,                                                      &
                                           GOUT_bbflux_pixel%NHLV,                                       &
                                           GOUT_bbflux_pixel%HLV,                                        &
                                           GOUT_bbflux_pixel%BBUFX0,                                     &
                                           GOUT_bbflux_pixel%BBDFX0                                      &
                                        )

            if ( error_present() ) return
            !print *, slout_comb(1:nbv_comb)

!XH         calculate broadband flux with aerosol
            RT_SOS_CTL%IAER = .TRUE.
            CALL radiative_transfer_SOS (  ATMOS_EMIS, SOLAR_EMIS,                                       & ! IN
                                           IW,OSHP%NG,OSHP%NN,OSHP%NF,                                   &
                                           iBRF_land,iBPF_land,iBRM_water,pixel_fit%land_percent,        &
                                           NQDR, NT1,                                                    &
                                           sza, NBV_comb, vis_comb(:), fiv_comb(:),                      &
                                           WAVE,                                                         &
                                           surf_land_par_num(1),surf_land_par_num(2),surf_land_par_vect, &
                                           surf_water_par_num,surf_water_par_vect,                       &
                                           EXT_os, SSA_os,                                               &
                                           NANG, ANGL,                                                   &
                                           SD,ext_norm,                                                  &
                                           HOBS_km, HGR_km, HMAX_atm_km, DISCRVD,                        &
                                           T_profile,STEMP,                                              &
                                           laerosol,lsurface,                                            &
                                           GOUT_aerosol_phmx_pixel_wl%ph11(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph12(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph22(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph33(:,:),                         &
                                           ASCAT,                                                        &
                                           SLout_comb,SQout_comb,                                        & ! OUT
                                           SUout_comb,SLPout_comb,                                       &
                                           dhr,                                                      &
                                           GOUT_bbflux_pixel%NHLV,                                       &
                                           GOUT_bbflux_pixel%HLV,                                        &
                                           GOUT_bbflux_pixel%BBUFXA,                                     &
                                           GOUT_bbflux_pixel%BBDFXA                                      &
                                        )

            if ( error_present() ) return
            !print *, slout_comb(1:nbv_comb)

!XH         integration over the absorbing band to get the radiance with K-distribution for vector case
            call ABS_GAS_INT(                                           &
                               curr_wl_index,i1,i2,i3,                  & ! IN
                               abs_data_forw_im,                        &
                               NBV=NBV_comb,                            &
                               SL=SLout_comb,                           &
                               SQ=SQout_comb,                           &
                               SU=SUout_comb,                           &
                               SL_TMP=SLout_tmp,                        &
                               SQ_TMP=SQout_tmp,                        &
                               SU_TMP=SUout_tmp,                        &
                               NLV=GOUT_bbflux_pixel%NHLV,              &
                               UFX0=GOUT_bbflux_pixel%BBUFX0,           &
                               DFX0=GOUT_bbflux_pixel%BBDFX0,           &
                               UFX =GOUT_bbflux_pixel%BBUFXA,           &
                               DFX =GOUT_bbflux_pixel%BBDFXA,           &

                               UFX0_TMP=UFX0_tmp,                       &
                               DFX0_TMP=DFX0_tmp,                       &
                               UFX_TMP =UFX_tmp,                        &
                               DFX_TMP =DFX_tmp                         & ! OUT
                            )
!XH         set number of gas components back
            ngas = 1



            else
!MH         This part of the subroutine is always called at least once, by inversion_forward_model in inversion.f90
!MH         for kernel and pixel calculations. Gas extintction option is also run over here.

!XH         without flux calculation
            RT_SOS_CTL%IAER = .TRUE.

            CALL radiative_transfer_SOS (  ATMOS_EMIS, SOLAR_EMIS,                                       & ! IN
                                           IW,OSHP%NG,OSHP%NN,OSHP%NF,                                   &
                                           iBRF_land,iBPF_land,iBRM_water,pixel_fit%land_percent,        &
                                           NQDR, NT1,                                                    &
                                           sza, NBV_comb, vis_comb(:), fiv_comb(:),                      &
                                           WL_Subchannels,                                               &
                                           surf_land_par_num(1),surf_land_par_num(2),surf_land_par_vect, &
                                           surf_water_par_num,surf_water_par_vect,                       &
                                           EXT_os, SSA_os,                                               &
                                           NANG, ANGL,                                                   &
                                           SD,ext_norm,                                                  &
                                           HOBS_km, HGR_km, HMAX_atm_km, DISCRVD,                        &
                                           T_profile,STEMP,                                              &
                                           laerosol,lsurface,                                            &
                                           GOUT_aerosol_phmx_pixel_wl%ph11(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph12(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph22(:,:),                         &
                                           GOUT_aerosol_phmx_pixel_wl%ph33(:,:),                         &
                                           ASCAT,                                                        &
                                           SLout_comb,SQout_comb,                                        & ! OUT
                                           SUout_comb,SLPout_comb,                                       &
                                           dhr                                                       &
                                        )
										

            if ( error_present() ) return
         end if !RT_SOS_CTL%IFLX
      END DO ! i3
      END DO ! i2
      END DO ! i1

      if (RT_SOS_CTL%IFLX) then
         SLout_comb(1:NBV_comb)  = SLout_tmp(1:NBV_comb)
         SQout_comb(1:NBV_comb)  = SQout_tmp(1:NBV_comb)
         SUout_comb(1:NBV_comb)  = SUout_tmp(1:NBV_comb)
         SLPout_comb(1:NBV_comb) = SQRT( SQout_comb(1:NBV_comb)**2  &
                                        +SUout_comb(1:NBV_comb)**2)
         GOUT_bbflux_pixel%BBUFX0=UFX0_tmp
         GOUT_bbflux_pixel%BBDFX0=DFX0_tmp
         GOUT_bbflux_pixel%BBUFXA=UFX_tmp
         GOUT_bbflux_pixel%BBDFXA=DFX_tmp
      end if ! RT_SOS_CTL%IFLX

      call CPU_time(time_finish)
      xtime=xtime+time_finish-time_start

      GOUT_surface_pixel_wl%dhr = 0.
      IF (.NOT. RT_SOS_CTL%ISGL) THEN
         GOUT_surface_pixel_wl%dhr = dhr
      END IF ! .NOT. RT_SOS_CTL%ISGL

	  IF (.not. iFlux) then
      	DO IV=1,NBV_comb
          status_funct = check_nan(1,SLout_comb(IV:IV))
          if (SLout_comb(IV) .LE. 0. .OR. (.not. status_funct)) then
              write(npar_land_str,'(i2)') surf_land_par_num(1)
              write(npar_water_str,'(i2)') surf_water_par_num
              cfmt = '(f9.4,es12.4,2a, i5,4es12.4,2a,'//adjustl(trim(npar_land_str))//'es12.4,2a,'//adjustl(trim(npar_water_str))//'es12.4,a)'
              write(tmp_message,trim(cfmt)) &
              WAVE,dhr,'  - WAVE, dhr', &
              NEW_LINE('A'), &
              IV, vis_comb(IV), fiv_comb(IV), sza, SLout_comb(IV), &
              '  - IV, vis_comb, fiv_comb, sza, SLout_comb', &
              NEW_LINE('A'), &
              surf_land_par_vect(1:surf_land_par_num(1)),' - land parameters', &
              NEW_LINE('A'), &
              surf_water_par_vect(1:surf_water_par_num),' - water parameters'
              G_ERROR(trim(tmp_message))

          end if
          status_funct = check_nan(1,SQout_comb(IV:IV))
          if (.not. status_funct) then
              write(tmp_message,'(i5,es12.4,a)') IV,SQout_comb(IV),'  - IV,SQout_comb(IV)'
              G_ERROR(trim(tmp_message))
          end if
          status_funct = check_nan(1,SUout_comb(IV:IV))
          if (.not. status_funct) then
              write(tmp_message,'(i5,es12.4,a)') IV,SUout_comb(IV),'  - IV,SUout_comb(IV)'
              G_ERROR(trim(tmp_message))
          end if
          IF(RT_SOS_SET%IP_VERBOSE .and. (.not. status_funct)) THEN
            write(*,'(4a9,5a12)') 'thd','vis','phi','sza','plh','pqh','puh','pph','wl'
            write(*,'(4f9.3,4es12.4,f12.4)') ASCAT(iv),vis_comb(iv),fiv_comb(iv),sza, &
                SLout_comb(IV),SQout_comb(IV),SUout_comb(IV),SLPout_comb(IV)/SLout_comb(IV),WAVE
          END IF
      	END DO ! IV
	  endif

!	  if (combined_up_down) then
!		 lsurface=lsurface_in
!	  endif
	
!
      RETURN
      END SUBROUTINE forw_SOS_RT
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      end module mod_rt
