/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */


/* NGO: number of terms in phase matrix expansion */
#define _NG0 41

/* NNO: number of terms in directional integration */
#define _NN0 20
 
/*
!PLTL NG0T = NG0*4 if ground based observations are present
!PLTL NG0T = NG0   for satellite observations
      INTEGER, PARAMETER ::   NG0T = _NG0T  ! ground based observations are present
!      INTEGER, PARAMETER ::   NG0T = NG0   ! for satellite observations  
 */
#define _NG0T (_NG0*4)
 
/*
!PL NF0 is maximum number of Fourier expansion.
!PL In general NF0<=2*NG0-2 but for aerosol we don't need such accuracy.
!PL Therefore, we take:		
 */
#define _NF0 _NN0 

/* KSD: number of aerosol components */
#define _KSD 2

/* KNT: number of atmospheric layers */
#define _KNT 51 

/* NMM: number of atmospheric component in OS */
#define _NMM (_KSD+1)

/* NBVM: number of observation angles for each measure */
#define _NBVM 300 

/* NMG: number of gas component with profile */
#define _NMG 1 

/* NG_SPEC_RANGE: number of MAXIMUM spectral ranges per gas*/
#define _NG_SPEC_RANGE_MAX 1

/* N_P_GLUT_MAX: number of MAXIMUM PRESSURE VALUES IN GAS LUT */
#define _N_P_GLUT_MAX 1

/* N_T_GLUT_MAX: number of MAXIMUM TEMPERATURE VALUES IN GAS LUT */
#define _N_T_GLUT_MAX 1

/* N_WL_GLUT_MAX: number of MAXIMUM WL VALUES IN GAS LUT */
#define _N_WL_GLUT_MAX 1

/* N_WL_CHANNEL_MAX: number of MAXIMUM WL PER HYPERSPECTRAL CHANNEL */
#define _N_WL_CHANNEL_MAX 1

/* N_WL_CHANNEL_MAX: number of MAXIMUM WL PER HYPERSPECTRAL CHANNEL */
#define _N_SUB_CHANNEL_MAX 1

/* N_FILTERS_MAX: MAXIMUM number of FILTER TYPES */
#define _N_FILTERS_MAX 1
      
/* N_CHEM_MAX: MAXIMUM number of chemical species for each mode */
#define _N_CHEM_MAX 10

/* N_RR_MAX: MAXIMUM number of refractive index wl per chemical specie*/
#define _N_RR_MAX 180
      
/* HMAX_atm (m) : atmosphere max height */
#define _HMAX_atm 40000.0 

/* KVERT_WD: number of profile grid heights */
#define _KVERT_WD 100 
