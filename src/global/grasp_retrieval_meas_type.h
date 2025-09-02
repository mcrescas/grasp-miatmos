/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* 
 * File:   grasp_retrieval_meas_type.h
 * Author: fuertes
 *
 * Created on 28 de octubre de 2013, 10:34
 */

#ifndef GRASP_RETRIEVAL_MEAS_TYPE_H
#define	GRASP_RETRIEVAL_MEAS_TYPE_H

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef WARN_DRY
#warning "__MEAS_TYPE__ duplicated"
#endif    
    
enum {
  MEAS_TYPE_UNKNOWN = 0,
  // tau beginning
  MEAS_TYPE_TOD = 11, // tod(wl) = aer+mol+gas - total optical depth
  MEAS_TYPE_AOD = 12, // aod(wl) - aerosol optical depth
  MEAS_TYPE_ABS = 13, // aaod(wl) - aerosol absorption
  MEAS_TYPE_HTOD = 14, // hyperspectral(wl) - hyperspectral relative irradiance
  // tau end
  // phase matrix beginning
  MEAS_TYPE_P11 = 21,
  MEAS_TYPE_P12 = 22,
  MEAS_TYPE_P22 = 23,
  MEAS_TYPE_P33 = 24,
  MEAS_TYPE_P34 = 25,
  MEAS_TYPE_P44 = 26,
  MEAS_TYPE_P11_rel_ang = 27,  // p11(angle,wl)/p11(given_angle,wl)  - relative scattering matrix element P11
  MEAS_TYPE_P12_rel = 28,  // -p12(angle,wl)/p11(angle,wl)  - relative scattering matrix element P12

  // phase matrix end
  // lidar beginning
  MEAS_TYPE_LS   = 31,
  MEAS_TYPE_RL   = 32,
  MEAS_TYPE_DPAR = 33,
  MEAS_TYPE_DPER = 34,
  MEAS_TYPE_DP   = 35,
  MEAS_TYPE_VEXT = 36,
  MEAS_TYPE_VBS  = 39,
  // lidar end
  // SvR beginning
  MEAS_TYPE_I   = 41,
  MEAS_TYPE_Q   = 42,
  MEAS_TYPE_U   = 43,
  MEAS_TYPE_P   = 44, //P(angle,wl)    - linear Polarization sqrt(Q*Q+U*U)
  MEAS_TYPE_I_rel_sum = 45,  // I(angle,wl)/sum(I(1:nmeas,wl)) - Relative Stokes parameter I
  MEAS_TYPE_P_rel     = 46,  // P(angle,wl)/I(angle,wl) - degree of linear Polarization sqrt(Q*Q+U*U)/I
  // SvR end
  // p11 integrated beginning
  MEAS_TYPE_P11_intd = 51,  // p11_intd(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file
  MEAS_TYPE_P11_intd_cut_off_1 = 52,  // p11_intd(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file for rmax[0] given in settings
  MEAS_TYPE_P11_intd_cut_off_2 = 53,  // p11_intd(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file for rmax[1] given in settings
  MEAS_TYPE_P11_intd_cut_off_3 = 54,  // p11_intd(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file for rmax[2] given in settings
  MEAS_TYPE_P11_intd_cut_off_4 = 55,  // p11_intd(wl) phase matrix element integrated in scattering angle range [ang1,ang2] defined as [thetav, phi] in the sdata file for rmax[3] given in settings
  // p11 integrated end
};

#define GRASP_NMEAS_TYPES 29

#ifdef	__cplusplus
}
#endif

#endif	/* GRASP_RETRIEVAL_MEAS_TYPE_H */

