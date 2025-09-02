! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

module mod_par_type_aerosol

  implicit none
  
#ifdef WARN_DRY
#warning "__CHARACTERISTIC_TYPE__ duplicated"
#endif
        
  integer,parameter ::	par_type_aerosol_beg = 10000

! Size Distribution
      integer,parameter ::	par_type_SD_beg = 10100
        integer,parameter :: par_type_SD_TB = 10101   ! Size distribution dV/dlnr at "Triangle" Bins
        integer,parameter :: par_type_SD_LB = 10102   ! Size distribution dV/dlnr for precompued Lognormal Bins
        integer,parameter :: par_type_SD_LN = 10103   ! Parameters of LogNormal size distribution dV/dlnr
        integer,parameter :: par_type_SD_MD = 10121   ! aerosol_model_concentration
      integer,parameter ::	par_type_SD_end = 10200
! Real part of complex Refractive Index or complex refractive index for Chemistry and mixture
      integer,parameter ::	par_type_RERI_beg = 10200
        integer,parameter :: par_type_RERI_spect = 10201  ! Spectral dependent real part of complex refractive index
        integer,parameter :: par_type_RERI_const = 10202  ! Real part of complex refractive index is constant
        integer,parameter :: par_type_CXRI_nmix  = 10203  ! Complex refractive index is mixture.
        integer,parameter :: par_type_CXRI_chem  = 10204  ! Complex refractive index. Chemistry: rh,finslbl,fsoot,firon
      integer,parameter ::	par_type_RERI_end = 10300
! Imaginary part of complex Refractive Index
      integer,parameter ::	par_type_IMRI_beg = 10300
        integer,parameter :: par_type_IMRI_spect = 10301  ! Spectral dependent imaginary part of complex refractive index
        integer,parameter :: par_type_IMRI_const = 10302  ! Imaginary part of complex refractive index is constant
      integer,parameter ::	par_type_IMRI_end = 10400
! Particles shape (nonsphericity)
      integer,parameter ::	par_type_SHD_beg = 10400
        integer,parameter ::	 par_type_SHD_fsph  = 10401     ! Fraction of spherical particles
        integer,parameter ::	 par_type_SHD_distr = 10402     ! Axis Ratio Distribution
        !integer,parameter ::	par_type_SHD_prof  = 10403     ! Sphericity profile
      integer,parameter ::	par_type_SHD_end = 10500
! Aerosol profile
      integer,parameter ::	par_type_AVP_beg = 10500
        integer,parameter :: par_type_AVP_par_height = 10501  ! Parameter of Aerosol Vertical Profile (...)
        integer,parameter :: par_type_AVP_prof       = 10502  ! Normalized Aerosol Vertical Profile
      integer,parameter ::	par_type_AVP_end = 10600
! Aerosol concentration
      integer,parameter ::par_type_Cv_beg = 10600
        integer,parameter :: par_type_Cv = 10601     ! Aerosol concentration
        integer,parameter :: par_type_TM_C  = 10602 ! Transport model concentrations
      integer,parameter ::par_type_Cv_end = 10700
! Additional parameters
      integer,parameter ::	par_type_ADD_beg = 10700
        integer,parameter :: par_type_CL          = 10701  ! Calibration coefficient for lidar
        integer,parameter :: par_type_AVP_par_std = 10702  ! Standard deviation for vertical profile
        integer,parameter :: par_type_TM_H        = 10703  !  Transport model level heights
      integer,parameter ::	par_type_ADD_end = 10800
! Relative humidity
      integer,parameter ::par_type_RH_beg = 10800
        integer,parameter :: par_type_RH     = 10801  ! Relative humidity
        integer,parameter :: par_type_TM_RH  = 10802  ! Transport model: Values of vertically dependent relative humidity
       integer,parameter ::par_type_RH_end = 10900

  integer,parameter ::	par_type_aerosol_end = 20000

! Gas concentration parameters
    integer,parameter ::par_type_gases_concentration_beg = 30000
      integer,parameter :: par_type_gas_concentration_1   = 30101
      integer,parameter :: par_type_gas_concentration_2   = 30201
      integer,parameter :: par_type_gas_concentration_3   = 30301
      integer,parameter :: par_type_gas_concentration_4   = 30401
      integer,parameter :: par_type_gas_concentration_5   = 30501
      integer,parameter :: par_type_gas_concentration_6   = 30601
      integer,parameter :: par_type_gas_concentration_7   = 30701
      integer,parameter :: par_type_gas_concentration_8   = 30801
      integer,parameter :: par_type_gas_concentration_9   = 30901
      integer,parameter :: par_type_gas_concentration_10  = 31001
    integer,parameter ::par_type_gases_concentration_end = 40000


! User Defined parametres. They allow to hack the code and let some space for random numbers
  integer,parameter ::	par_type_USER_defined_beg = 90000
        integer,parameter :: par_type_USER_defined_1 = 90100
        integer,parameter :: par_type_USER_defined_2 = 90200
        integer,parameter :: par_type_USER_defined_3 = 90300
  integer,parameter ::	par_type_USER_defined_end = 100000

  
      contains
      
      subroutine for_avoiding_warnings_empty_module_settings_par_type_derived()
      end subroutine for_avoiding_warnings_empty_module_settings_par_type_derived     
      
end module mod_par_type_aerosol

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
