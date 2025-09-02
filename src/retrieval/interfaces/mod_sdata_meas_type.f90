! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **
! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
module mod_sdata_meas_type
      implicit none 	
!	------------------------------------------------------------------------------------------------------
#ifdef WARN_DRY
#warning "__MEAS_TYPE__ duplicated"
#endif   
      integer,parameter ::  meas_type_tau_beg = 10
      integer,parameter ::	meas_type_tod  = 11   ! tod(wl) = aer+mol+gas - Total Optical Depth
      integer,parameter ::	meas_type_aod  = 12   ! aod(wl) - Aerosol Optical Depth
      integer,parameter ::	meas_type_aaod = 13   ! aaod(wl) - Aerosol Absorption Optical Depth
      integer,parameter ::	meas_type_htod  = 14  ! htod(wl) - Hyperspectral Total Optical Depth
      integer,parameter ::  meas_type_tau_end = 20

      integer,parameter ::  meas_type_phm_beg = 20
      integer,parameter ::	meas_type_p11 = 21   ! p11(angle,wl)  - scattering matrix element P11
      integer,parameter ::	meas_type_p12 = 22   ! p12(angle,wl)  - scattering matrix element P12
      integer,parameter ::	meas_type_p22 = 23   ! p22(angle,wl)  - scattering matrix element P22
      integer,parameter ::	meas_type_p33 = 24   ! p33(angle,wl)  - scattering matrix element P33
      integer,parameter ::	meas_type_p34 = 25   ! p34(angle,wl)  - scattering matrix element P34
      integer,parameter ::	meas_type_p44 = 26   ! p44(angle,wl)  - scattering matrix element P44
      integer,parameter ::	meas_type_p11_rel_ang = 27  ! p11(angle,wl)/p11(given_angle,wl)  - relative scattering matrix element P11
      integer,parameter ::	meas_type_p12_rel = 28  ! -p12(angle,wl)/p11(angle,wl)  - relative scattering matrix element P12
      integer,parameter ::  meas_type_phm_end = 30
      
      integer,parameter ::  meas_type_lid_beg = 30
      integer,parameter ::	meas_type_LS   = 31   ! LS(height,wl)     - Lidar Signal
      integer,parameter ::	meas_type_RL   = 32   ! RL(height,wl)     - Raman Lidar signal
      integer,parameter ::	meas_type_DPAR = 33   ! LS_PAR(height,wl) - polarized lidar signal (parallel to sounding beam)
      integer,parameter ::	meas_type_DPER = 34   ! LS_PER(height,wl) - polarized lidar signal (perpendicular to sounding beam)
      integer,parameter ::	meas_type_DP   = 35   ! DP(height,wl)     - DePolarization ratio
      integer,parameter ::	meas_type_VEXT = 36   ! VEXT(height,wl)   - Vertical EXTinctin profile
      integer,parameter ::	meas_type_VBS  = 39   ! VBS(height,wl)    - Vertical BackScatter profile (e.g. from sondes)
      integer,parameter ::  meas_type_lid_end = 40
      
      integer,parameter ::  meas_type_SvR_beg = 40
      integer,parameter ::	meas_type_I   = 41  ! I(angle,wl)    - Stokes parameter I
      integer,parameter ::	meas_type_Q   = 42  ! Q(angle,wl)    - Stokes parameter Q
      integer,parameter ::	meas_type_U   = 43  ! U(angle,wl)    - Stokes parameter U
      integer,parameter ::	meas_type_P   = 44  ! P(angle,wl)    - linear Polarization sqrt(Q*Q+U*U)
      integer,parameter ::	meas_type_I_rel_sum = 45  ! I(angle,wl)/sum(I(1:nmeas,wl)) - Relative Stokes parameter I
      integer,parameter ::	meas_type_P_rel     = 46  ! P(angle,wl)/I(angle,wl) - degree of linear Polarization sqrt(Q*Q+U*U)/I
      integer,parameter ::  meas_type_SvR_end = 50

      integer,parameter ::  meas_type_integrated_beg = 50
      integer,parameter ::	meas_type_p11_intd = 51  ! p11_intd(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file
      integer,parameter ::	meas_type_p11_intd_cut_off_1 = 52  ! p11_intd_off_1(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file for rmax1 given in settings
      integer,parameter ::	meas_type_p11_intd_cut_off_2 = 53  ! p11_intd_off_2(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file for rmax2 given in settings
      integer,parameter ::	meas_type_p11_intd_cut_off_3 = 54  ! p11_intd_off_3(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file for rmax3 given in settings
      integer,parameter ::	meas_type_p11_intd_cut_off_4 = 55  ! p11_intd_off_4(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file for rmax4 given in settings
      integer,parameter ::  meas_type_integrated_end = 60

!	------------------------------------------------------------------------------------------------------
      contains
      
      subroutine dummy_subroutine_for_avoiding_warnings_in_an_empty_module()
      end subroutine dummy_subroutine_for_avoiding_warnings_in_an_empty_module                

end module mod_sdata_meas_type

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
