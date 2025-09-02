/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/**
 * @file grasp_input_segment.h
 * @author David Fuertes
 * @date 3 Oct 2014
 * @brief This file contains segment definition
 *
 * Segment definition is retrieval (fortran code) segment definition binded plus
 * extra information added by the framework to this structure (iguess in same structure).
 * In addition this file contains the function to set sdata information with limit checking
 */

#ifndef GRASP_INPUT_SEGMENT_H
#define	GRASP_INPUT_SEGMENT_H

#ifdef	__cplusplus
extern "C" {
#endif

#include <inttypes.h>
#include "mod_par_inv.inc"
#include "mod_par_OS.inc"
    
#ifdef WARN_DRY
#warning "WARN_DRY: __CHARACTERISTIC_TYPE__ duplicated (binded)"
#endif
   
/**
 * This structure represents the information inside a specific wavelength of a pixel
 */    
typedef struct data_wl_t_{
    int    meas_type[_KIP];  /**< @brief Type of measure of each kind of measure  */   
    float  wl;               /**< @brief Current wavelength  */   
    int    ind_wl;           /**< @brief Index of wavelength in the list of wavelengths
                              * 
                              * This information is set inside retrieval code and it is used
                              * in internal calculations. It is not necessary to be set outside retrieval code
                              */   
    float  sza;                  /**< @brief Solar zenit angle of current wavelength in degrees */
    float  thetav [_KIP][_NBVM]; /**< @brief View zenit angle (thetav) for each measure in this wavelength in degrees  */   
    float  phi[_KIP][_NBVM];     /**< @brief relative azimuth angle (phi) for each measure in this wavelength in degrees  */   
    int    nsurf;                /**< @brief Skip it: Number of surface. Currently not used  */
    float  groundpar[_KSURF];    /**< @brief Skip it: Value of surface for each surface. Currently not used  */
    float  gaspar;               /**< @brief Total gas absorption for this wavelength (tau gases)  */
    
    int    nbvm[_KIP];        /**< @brief Number of measurements for each measure type  */
    int    nip;               /**< @brief Number of measure types in this wavelength    */
    
    float  tod[_NBVM];  /**< @brief total optical depth; meas_type = 11  */  
    float  aod[_NBVM];  /**< @brief aerosol optical depth; meas_type = 12  */  
    float  aaod[_NBVM]; /**< @brief absorption aerosol optical depth; meas_type = 13  */  
    float  htod[_NBVM]; /**< @brief hyperspectral relative irradiance; meas_type = 14  */  
    float  p11[_NBVM];  /**< @brief Values of p11 phase matrix element */
    float  p12[_NBVM];  /**< @brief Values of p12 phase matrix element */
    float  p22[_NBVM];  /**< @brief Values of p22 phase matrix element */
    float  p33[_NBVM];  /**< @brief Values of p33 phase matrix element */
    float  p34[_NBVM];  /**< @brief Values of p34 phase matrix element */
    float  p44[_NBVM];  /**< @brief Values of p44 phase matrix element */
    float  p11_rel_ang[_NBVM];  /**< @brief p11/p11(given_angle) phase matrix element; meas_type = 27 */
    float  p12_rel[_NBVM];  /**< @brief -p12/p11 phase matrix element;             meas_type = 28 */
    float  p11_intd[_NBVM]; /* p11 phase matrix element integrated in scattering angle range [ang1,ang2]; meas_type = 51 defined as [thetav, phi] */
    float  p11_intd_cut_off_1[_NBVM]; /* p11 phase matrix element integrated in scattering angle range [ang1,ang2] for rmax1 given in settings; meas_type = 52 defined as [thetav, phi] */
    float  p11_intd_cut_off_2[_NBVM]; /* p11 phase matrix element integrated in scattering angle range [ang1,ang2] for rmax2 given in settings; meas_type = 53 defined as [thetav, phi] */
    float  p11_intd_cut_off_3[_NBVM]; /* p11 phase matrix element integrated in scattering angle range [ang1,ang2] for rmax3 given in settings; meas_type = 54 defined as [thetav, phi] */
    float  p11_intd_cut_off_4[_NBVM]; /* p11 phase matrix element integrated in scattering angle range [ang1,ang2] for rmax4 given in settings; meas_type = 55 defined as [thetav, phi] */

    float  ls[_KVERTM];   /**< @brief Values of lidar signal                          */
    float  rl[_KVERTM];   /**< @brief Values of Raman lidar signal                    */
    float  dpar[_KVERTM]; /**< @brief Values of parallel polarized lidar              */
    float  dper[_KVERTM]; /**< @brief Values of cross (perpendicular) polarized lidar */
    float  dp[_KVERTM];   /**< @brief Values of depolarization ratio                  */
    float  vext[_KVERTM]; /**< @brief Values of vertical extinction profile           */
    float  vbs[_KVERTM];  /**< @brief  Raman lidar signal;          meas_type = 39    */

    float  i[_NBVM];    /**< @brief I Stokes parameter         */ 
    float  q[_NBVM];    /**< @brief Q Stokes parameter         */ 
    float  u[_NBVM];    /**< @brief U Stokes parameter         */ 
    float  p[_NBVM];    /**< @brief Polarization sqrt(Q*Q+U*U) */ 
    
    float  i_rel_sum[_NBVM];    /**< @brief Relative Stokes parameter I/sum(I(1:NBVM)); meas_type = 45 */ 
    float  p_rel[_NBVM];    /**< @brief linear polarization sqrt(Q*Q+U*U)/I;        meas_type = 46 */ 
    
    float cmtrx[_KIP][_KNBVM];  /**< @brief Diagonal of covariance matrix (also known as OMEGA) */
    float mprof[_KIP][_KVERTM]; /**< @brief Vertical profile of Rayleigh backscatter (beta_m) */
    int   ifcov[_KIP];          /**< @brief 0/1 Presence of covariance matrix  */
    int   ifmp[_KIP];           /**< @brief  0/1 Presence of molecular profile */
    
    float mu[_KIP]; /**< @brief value of crosstalk between par/per lidar channels */
    int   ind_wl_i; /**< @brief index of initial wl in list of wavelenghts corresponding to an index of a shifted measurement (e.q. Raman lidar) */
} data_wl_t;

/**
 * This structure contains the data for be retrieved of each pixel
 */
typedef struct pixel_t_{
    float hobs;  /**< @brief Height of observation in meters */
    int nwl;     /**< @brief Number of wavelengths */
    int cloudy;  /**< @brief If pixel is cloudy or not. 0=cloud and 1=clean */
    float x;     /**< @brief Longitude */
    float y;     /**< @brief Latitude  */
    int64_t t;   /**< @brief Datetime (Unix timestamp) */
    int ix;      /**< @brief Internal X index of pixel inside the segment starting in 1 */
    int iy;      /**< @brief Internal Y index of pixel inside the segment starting in 1 */
    int it;      /**< @brief Internal T index of pixel inside the segment starting in 1 */
    int out_x;   /**< @brief Index of X index in output grid. 
                  * 
                  * The index of output grid is an information which will not used inside retrieval code,
                  * however this information will allow framework to organize output tile */
    int out_y;   /**< @brief Index of Y index in output grid. 
                  * 
                  * The index of output grid is an information which will not used inside retrieval code,
                  * however this information will allow framework to organize output tile */
    int out_t;   /**< @brief Index of T index in output grid. 
                  * 
                  * The index of output grid is an information which will not used inside retrieval code,
                  * however this information will allow framework to organize output tile */
    float masl;  /**< @brief Metres Above Sea Level */
    float land_percent;  /**< @brief Metres Above Sea Level */
    // FIXME: have 'float snow_percent;' ?
    int irow;     /**< @brief Index for row (y) in input grid. 
                   * 
                   * This index will not be used neither in framework nor in retrieval. 
                   * This is only for traceability of input information
                   */
    int icol;     /**< @brief Index for col (x) in input grid. 
                   * 
                   * This index will not be used neither in framework nor in retrieval. 
                   * This is only for traceability of input information
                   */
    int file_index;/**< @brief Reference to used_files array (in tile)
                   * 
                   * This index is the position of input file in the framework input file list
                   */
    int ifgas;     /**< @brief 1 if the gas correction will be applied to this pixel */
    data_wl_t meas[_KWM]; /**< @brief Array of measures for each wavelength of current pixel */
    float hvp[_KVERTM]; /**< @brief Height for vertical profile  */
}pixel_t;

/**
 * Sdata structure binded with src/retrieval/src/mod_sdata_ut.f90
 */
typedef struct sensor_data_t_{
    /**
     * Number of total pixels in the segment
     */
    int npixels;
    /**
     * Number of elements in T dimension
     */
    int nt;
    /**
     * Maximum number of elements in X dimension
     */
    int nx;
    /**
     * Maximum number of elements in Y dimension
     */
    int ny;
    /**
     * identification of current segment inside the tile
     */
    int id_inversion;
    /**
     * Data for each pixel
     */
    pixel_t pixel[_KIMAGE];
}sensor_data_t;


// For edges system

    /** groups (1 - before, 2 - after) contain */
    typedef struct edges_group_x_{
        int nx; /**< @brief nx - size of X edges with data  */  
        int ny; /**< @brief ny - size of Y edges with data  */ 
        int nt; /**< @brief nt - size of T edges with data     */   
        int icloud[_KITIME][_KIY][_KIEDGE]; /**< @brief icloud(_KIEDGE,_KIY,_KITIME) - cloud present indicator  */ 
        float x[_KITIME][_KIY][_KIEDGE]; /**< @brief x(_KIEDGE,_KIY,_KITIME) - pixel longitude (X)  */ 
        float y[_KITIME][_KIY][_KIEDGE]; /**< @brief y(_KIEDGE,_KIY,_KITIME) - pixel latitude  (Y)  */ 
        int64_t t[_KITIME]; /**< @brief t(_KITIME)            - pixel time      (T)  */ 
        int out_x[_KITIME][_KIY][_KIEDGE]; /**< @brief x(_KIEDGE,_KIY,_KITIME) - absolute position of pixel inside the tile in x dimension  */ 
        int out_y[_KITIME][_KIY][_KIEDGE]; /**< @brief y(_KIEDGE,_KIY,_KITIME) - absolute position of pixel inside the tile in y dimension  */ 
        int out_t[_KITIME]; /**< @brief t(_KITIME)            -                  absolute position of pixel inside the tile in t dimension  */ 
        int it[_KITIME]; /**< @brief it(_KITIME) - array contains time indices for time with data  */ 
        float AP[_KITIME][_KIY][_KIEDGE][_KPARS];  /**< @brief  AP(KPARS,_KIEDGE,_KIY,_KITIME) - parameters in edge data     */      
    }edges_group_x;

    /** Y groups (1 - before, 2 - after) contain  */ 
    typedef struct edges_group_y_{
        int nx;                             /**< @brief nx - size of X edges with data  */
        int ny;                             /**< @brief ny - size of Y edges with data  */
        int nt;                             /**< @brief nt - size of T edges with data  */
        int icloud[_KITIME][_KIEDGE][_KIX]; /**< @brief icloud(_KIX,_KIEDGE,_KITIME) - cloud present index  */ 
        float x[_KITIME][_KIEDGE][_KIX];    /**< @brief x(_KIX,_KIEDGE,_KITIME) - pixel longitude (X)  */
        float y[_KITIME][_KIEDGE][_KIX];    /**< @brief y(_KIX,_KIEDGE,_KITIME) - pixel latitude  (Y)  */
        int64_t t[_KITIME];                 /**< @brief t(_KITIME)              - pixel time      (T)  */
        int out_x[_KITIME][_KIEDGE][_KIX];  /**< @brief x(_KIX,_KIEDGE,_KITIME) - absolute position of pixel inside the tile in x dimension  */
        int out_y[_KITIME][_KIEDGE][_KIX];  /**< @brief y(_KIX,_KIEDGE,_KITIME) - absolute position of pixel inside the tile in y dimension  */
        int out_t[_KITIME];                 /**< @brief t(_KITIME) - absolute position of pixel inside the tile in t dimension  */
        int it[_KITIME];                    /**< @brief it(_KITIME) - array contains time indices  */
        float AP[_KITIME][_KIEDGE][_KIX][_KPARS]; /**< @brief AP(KPARS,_KIX,_KIEDGE,_KITIME) - parameters in edge data  */ 
    }edges_group_y;
    
    /** T groups (1 - before, 2 - after) contain  */ 
    typedef struct edges_group_t_{

        int nx; /**< @brief nx - size of X edges with data  */ 
        int ny; /**< @brief ny - size of Y edges with data  */ 
        int nt; /**< @brief nt - size of T edges with data  */ 
        int icloud[_KIEDGE][_KIY][_KIX];       /**< @brief icloud(_KIX,_KIY,_KIEDGE) - cloud present index  */
        float x[_KIEDGE][_KIY][_KIX];          /**< @brief x(_KIX,_KIY,_KIEDGE) - pixel longitude (X)  */
        float y[_KIEDGE][_KIY][_KIX];          /**< @brief y(_KIX,_KIY,_KIEDGE) - pixel latitude  (Y)  */
        int64_t t[_KIEDGE];                    /**< @brief t(_KIEDGE)         - pixel time      (T)  */
        int out_x[_KIEDGE][_KIY][_KIX];        /**< @brief x(_KIX,_KIY,_KIEDGE) - absolute position of pixel inside the tile in x dimension  */
        int out_y[_KIEDGE][_KIY][_KIX];        /**< @brief y(_KIX,_KIY,_KIEDGE) - absolute position of pixel inside the tile in y dimension  */
        int out_t[_KIEDGE];                    /**< @brief t(_KIEDGE)                       - absolute position of pixel inside the tile in t dimension  */
        int it[_KIEDGE];                       /**< @brief it(_KIEDGE) - array contains time indices  */
        float AP[_KIEDGE][_KIY][_KIX][_KPARS]; /**< @brief AP(KPARS,_KIX,_KIY,_KIEDGE) - parameters in edge data  */ 
    }edges_group_t;
    
    /** segment edge structure contain  */ 
    typedef struct  segment_edges_{
        int N_I_EDGE;             /**< @brief N_I_EDGE   - number of edge groups with data (maximum 6: X before/after, Y before/after, T before/after)  */
        int I_EDGE[6];            /**< @brief I_EDGE(6)  - present group index array I_EDGE(1:N_I_EDGE)  */
        edges_group_x group_x[2]; /**< @brief group_X(2) - array of present group indices (group(1:N_I_EDGE))  */ 
        edges_group_y group_y[2]; /**< @brief group_Y(2) - array of present group indices (group(1:N_I_EDGE))  */ 
        edges_group_t group_t[2]; /**< @brief group_T(2) - array of present group indices (group(1:N_I_EDGE))  */ 
    } segment_edges;  
    
    


/**
 * Framework segment structure contains segment structure (binded with retrieval part)
 * and initial guess for each pixel. If a value of initial guess is -999, it will be ignored and
 * the value in setting file will be used like initial guess.
 */
typedef struct grasp_segment_t_{
    /**
     * Sensor data which it will run inside retrieval code.
     * It is binded with Fortran file.
     */
    sensor_data_t sdata;
    /**
     * Initial guess for each pixel. This initial guess will be used instead of
     * values in settings file if the values here are different than -999. Otherwise
     * values from settings file will be used
     */
    float iguess[_KIMAGE][_KPARS];     
    /**
     * Segment edges has information of neighbors 
     */
    segment_edges edges;
}grasp_segment_t;


// Functions to manage a segment

/**
 * Inialize a segment
 * @param segment to be initialized. It has to be already allocated
 */
void grasp_segment_initialize(grasp_segment_t* segment);

/**
 * Return 0 if the segment is valid, otherwise return error code.
 * @param segment
 * @return status code
 */
int grasp_segment_validate(grasp_segment_t* segment);

/**
 * Return 0 if the sdata is valid, otherwise return error code.
 * @param sdata
 * @return status code
 */
 int grasp_sdata_validate(sensor_data_t* sdata);

/**
 * This function will remove ipixel from sdata. NOTE: This function does not support edges yet.
 * @param sdata I/O argument, sdata where want to have one pixel less
 * @param ipixel Index of the pixel to be removed
 */
void grasp_segment_remove_pixel(grasp_segment_t *segment, int ipixel);

/**
 * This function will remove a list of pixels from an sdata. NOTE: This function does not support edges yet.
 * @param sdata I/O argument, sdata where want to have one pixel less
 * @param npixel Number of pixels in array of indexes
 * @param pixels Array of pixels to be removed. NOTE: THE INDEXES HAS TO HAVE INCREASE MONOTONIC ORDER. 
 */
void grasp_segment_remove_pixels(grasp_segment_t *segment, int npixels, int *pixels);




// The following functions help you to set segment fields without do memory leaks.
// This functions does not has the objective of validate the data. The objective of
// following functions is to prevent memory leaks. Use this functions to set segment
// information. Then, the segment will be validated in scientific point of view.

// ===== Set functions ===

/** @brief Set number of t dimensions in segment data */
void grasp_segment_set_nt(sensor_data_t *sdata, int nt);

/** @brief Set number of x dimensions in segment data */
void grasp_segment_set_nx(sensor_data_t *sdata, int nx);

/** @brief Set number of y dimensions in segment data */
void grasp_segment_set_ny(sensor_data_t *sdata, int ny);

/** @brief Set number of pixels dimensions in segment data. It has to be after set nt, nx and ny */
void grasp_segment_set_npixels(sensor_data_t *sdata, int npixels);

/** @brief Set sdata->pixel[ipixel].ix */
void grasp_segment_set_pixel_ix(sensor_data_t *sdata,int ipixel,int it);

/** @brief Set sdata->pixel[ipixel].ix */
void grasp_segment_set_pixel_iy(sensor_data_t *sdata,int ipixel,int ix);

/** @brief Set sdata->pixel[ipixel].iy */
void grasp_segment_set_pixel_it(sensor_data_t *sdata,int ipixel,int iy);

/** @brief Set sdata->pixel[ipixel].cloudy */
void grasp_segment_set_pixel_cloudy(sensor_data_t *sdata,int ipixel,int cloudy);

/** @brief Set sdata->pixel[ipixel].irow */
void grasp_segment_set_pixel_irow(sensor_data_t *sdata,int ipixel,int irow);

/** @brief Set sdata->pixel[ipixel].icol */
void grasp_segment_set_pixel_icol(sensor_data_t *sdata,int ipixel,int icol);

/** @brief Set sdata->pixel[ipixel].file_index */
void grasp_segment_set_pixel_file_index(sensor_data_t *sdata,int ipixel,int file_index);

/** @brief Set sdata->pixel[ipixel].x (longitude) */
void grasp_segment_set_pixel_x(sensor_data_t *sdata,int ipixel,float x);

/** @brief Set sdata->pixel[ipixel].y (latitude) */
void grasp_segment_set_pixel_y(sensor_data_t *sdata,int ipixel,float y);

/** @brief Set sdata->pixel[ipixel].t (time) */
void grasp_segment_set_pixel_t(sensor_data_t *sdata,int ipixel,int64_t t);

/** @brief Set sdata->pixel[ipixel].out_t */
void grasp_segment_set_pixel_out_t(sensor_data_t *sdata,int ipixel,int out_t);

/** @brief Set sdata->pixel[ipixel].out_x */
void grasp_segment_set_pixel_out_x(sensor_data_t *sdata,int ipixel,int out_x);

/** @brief Set sdata->pixel[ipixel].out_y */
void grasp_segment_set_pixel_out_y(sensor_data_t *sdata,int ipixel,int out_y);

/** @brief Set sdata->pixel[ipixel].MASL */
void grasp_segment_set_pixel_masl(sensor_data_t *sdata,int ipixel,float masl);

/** @brief Set sdata->pixel[ipixel].HOBS */
void grasp_segment_set_pixel_hobs(sensor_data_t *sdata,int ipixel,float hobs);

/** @brief Set sdata->pixel[ipixel].land_percent */
void grasp_segment_set_pixel_land_percent(sensor_data_t *sdata,int ipixel,float land_percent);

/** @brief Set sdata->pixel[ipixel].nwl */
void grasp_segment_set_pixel_nwl(sensor_data_t *sdata,int ipixel,int nwl);

/** @brief Set sdata->pixel[ipixel].ifgas */
void grasp_segment_set_pixel_ifgas(sensor_data_t *sdata,int ipixel,int ifgas);

/** @brief Set sdata->pixel[ipixel].HVP[ivm] */
void grasp_segment_set_pixel_hvp(sensor_data_t *sdata,int ipixel, int ivm, float hvp); 

/** @brief Set sdata->pixel[ipixel].meas[iwl].wl */
void grasp_segment_set_pixel_meas_wl(sensor_data_t *sdata,int ipixel, int iwl, float wl); 

/** @brief Set sdata->pixel[ipixel].meas[iwl].ind_wl */
void grasp_segment_set_pixel_meas_ind_wl(sensor_data_t *sdata,int ipixel, int iwl, float ind_wl);

/** @brief Set sdata->pixel[ipixel].meas[iwl].Nsurf */
void grasp_segment_set_pixel_meas_nsurf(sensor_data_t *sdata,int ipixel, int iwl, int nsurf);

/** @brief Set sdata->pixel[ipixel].meas[iwl].gaspar */
void grasp_segment_set_pixel_meas_gaspar(sensor_data_t *sdata,int ipixel, int iwl, float gaspar);

/** @brief Set sdata->pixel[ipixel].meas[iwl].sza */
void grasp_segment_set_pixel_meas_sza(sensor_data_t *sdata,int ipixel, int iwl, float sza);

/** @brief Set sdata->pixel[ipixel].meas[iwl].groundpar[isurf] */
void grasp_segment_set_pixel_meas_groundpar(sensor_data_t *sdata,int ipixel, int iwl, int isurf, float groundpar);

/** @brief Set sdata->pixel[ipixel].meas[iwl].nip */
void grasp_segment_set_pixel_meas_nip(sensor_data_t *sdata,int ipixel, int iwl, int nip);

/** @brief Set sdata->pixel[ipixel].meas[iwl].meas_type[ip] */
void grasp_segment_set_pixel_meas_meas_type(sensor_data_t *sdata,int ipixel, int iwl, int ip, int meas_type);

/** @brief Set sdata->pixel[ipixel].meas[iwl].nbvm[ip]  */
void grasp_segment_set_pixel_meas_nbvm(sensor_data_t *sdata,int ipixel, int iwl, int ip, int nbvm);

/** @brief Set sdata->pixel[ipixel].meas[iwl].IFCOV[ip] */
void grasp_segment_set_pixel_meas_ifcov(sensor_data_t *sdata,int ipixel, int iwl, int ip, int ifcov);

/** @brief Set sdata->pixel[ipixel].meas[iwl].IFMP[ip] */
void grasp_segment_set_pixel_meas_ifmp(sensor_data_t *sdata,int ipixel, int iwl, int ip, int ifmp);

/** @brief Set sdata->pixel[ipixel].meas[iwl].thetav[ip][ivalidmeas] */
void grasp_segment_set_pixel_meas_thetav(sensor_data_t *sdata,int ipixel, int iwl, int ip, int ivalidmeas, float thetav);

/** @brief Set sdata->pixel[ipixel].meas[iwl].phi[ip][ivalidmeas] */
void grasp_segment_set_pixel_meas_phi(sensor_data_t *sdata,int ipixel, int iwl, int ip, int ivalidmeas, float phi);
                    
/** @brief Set sdata->pixel[ipixel].meas[iwl].CMTRX[ip][ivalidmeas] */
void grasp_segment_set_pixel_meas_cmtrx(sensor_data_t *sdata,int ipixel, int iwl, int ip, int ivalidmeas, float cmtrx);

/** @brief Set sdata->pixel[ipixel].meas[iwl].MPROF[ip][ivalidmea] */
void grasp_segment_set_pixel_meas_mprof(sensor_data_t *sdata,int ipixel, int iwl, int ip, int ivalidmeas, float mprof);

/** @brief Set sdata->pixel[ipixel].meas[iwl].tod[ivalidmeas] */
void grasp_segment_set_pixel_meas_aod(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float tod);

/** @brief Set sdata->pixel[ipixel].meas[iwl].aod[ivalidmeas] */
void grasp_segment_set_pixel_meas_tod(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float aod);

/** @brief Set sdata->pixel[ipixel].meas[iwl].aaod[ivalidmeas] */
void grasp_segment_set_pixel_meas_aaod(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float aaod);

/** @brief Set sdata->pixel[ipixel].meas[iwl].htod[ivalidmeas] */
void grasp_segment_set_pixel_meas_htod(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float htod);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p11[ivalidmeas] */
void grasp_segment_set_pixel_meas_p11(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p11);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p12[ivalidmeas] */
void grasp_segment_set_pixel_meas_p12(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p12);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p22[ivalidmeas] */
void grasp_segment_set_pixel_meas_p22(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p22);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p33[ivalidmeas] */
void grasp_segment_set_pixel_meas_p33(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p33);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p34[ivalidmeas] */
void grasp_segment_set_pixel_meas_p34(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p34);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p44[ivalidmeas] */
void grasp_segment_set_pixel_meas_p44(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p44);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p11_rel_ang[ivalidmeas] */
void grasp_segment_set_pixel_meas_p11_rel_ang(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p11_rel_ang);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p12_rel[ivalidmeas] */
void grasp_segment_set_pixel_meas_p12_rel(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p12_rel);

/** @brief Set sdata->pixel[ipixel].meas[iwl].I[ivalidmeas] */
void grasp_segment_set_pixel_meas_i(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float i);

/** @brief Set sdata->pixel[ipixel].meas[iwl].Q[ivalidmeas] */
void grasp_segment_set_pixel_meas_q(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float q);

/** @brief Set sdata->pixel[ipixel].meas[iwl].U[ivalidmeas] */
void grasp_segment_set_pixel_meas_u(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float u);

/** @brief Set sdata->pixel[ipixel].meas[iwl].P[ivalidmeas] */
void grasp_segment_set_pixel_meas_p(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p);

/** @brief Set sdata->pixel[ipixel].meas[iwl].P11_intd[ivalidmeas] */
void grasp_segment_set_pixel_meas_p11_intd(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd);

/** @brief Set sdata->pixel[ipixel].meas[iwl].P11_intd_cut_off_1[ivalidmeas] */
void grasp_segment_set_pixel_meas_p11_intd_cut_off_1(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd_cut_off_1);

/** @brief Set sdata->pixel[ipixel].meas[iwl].P11_intd_cut_off_2[ivalidmeas] */
void grasp_segment_set_pixel_meas_p11_intd_cut_off_2(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd_cut_off_2);

/** @brief Set sdata->pixel[ipixel].meas[iwl].P11_intd_cut_off_3[ivalidmeas] */
void grasp_segment_set_pixel_meas_p11_intd_cut_off_3(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd_cut_off_3);

/** @brief Set sdata->pixel[ipixel].meas[iwl].P11_intd_cut_off_4[ivalidmeas] */
void grasp_segment_set_pixel_meas_p11_intd_cut_off_4(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd_cut_off_4);

/** @brief Set sdata->pixel[ipixel].meas[iwl].i_rel_sum[ivalidmeas] */
void grasp_segment_set_pixel_meas_i_rel_sum(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float i_rel_sum);

/** @brief Set sdata->pixel[ipixel].meas[iwl].p_rel[ivalidmeas] */
void grasp_segment_set_pixel_meas_p_rel(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float p_rel);

/** @brief Set sdata->pixel[ipixel].meas[iwl].LS[ivalidmeas] */
void grasp_segment_set_pixel_meas_ls(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float ls);

/** @brief Set sdata->pixel[ipixel].meas[iwl].DP[ivalidmeas] */
void grasp_segment_set_pixel_meas_dp(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float dp);

/** @brief Set sdata->pixel[ipixel].meas[iwl].DPAR[ivalidmeas] */
void grasp_segment_set_pixel_meas_dpar(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float dpar);

/** @brief Set sdata->pixel[ipixel].meas[iwl].DPER[ivalidmeas] */
void grasp_segment_set_pixel_meas_dper(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float dper);

/** @brief Set sdata->pixel[ipixel].meas[iwl].RL[ivalidmeas] */
void grasp_segment_set_pixel_meas_rl(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float rl);

/** @brief Set sdata->pixel[ipixel].meas[iwl].VBS[ivalidmeas] */
void grasp_segment_set_pixel_meas_vbs(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float vbs);

/** @brief Set sdata->pixel[ipixel].meas[iwl].VEXT[ivalidmeas] */
void grasp_segment_set_pixel_meas_vext(sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas, float vext);



// =========get functions =======================================================

int grasp_segment_get_nt(const sensor_data_t *sdata);

int grasp_segment_get_nx(const sensor_data_t *sdata);

int grasp_segment_get_ny(const sensor_data_t *sdata);

int grasp_segment_get_npixels(const sensor_data_t *sdata);

int grasp_segment_get_pixel_it(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_ix(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_iy(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_cloudy(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_irow(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_icol(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_file_index(const sensor_data_t *sdata,int ipixel);

float grasp_segment_get_pixel_x(const sensor_data_t *sdata,int ipixel);

float grasp_segment_get_pixel_y(const sensor_data_t *sdata,int ipixel);

int64_t grasp_segment_get_pixel_t(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_out_t(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_out_x(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_out_y(const sensor_data_t *sdata,int ipixel);

float grasp_segment_get_pixel_masl(const sensor_data_t *sdata,int ipixel);

float grasp_segment_get_pixel_hobs(const sensor_data_t *sdata,int ipixel);

float grasp_segment_get_pixel_land_percent(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_nwl(const sensor_data_t *sdata,int ipixel);

int grasp_segment_get_pixel_ifgas(const sensor_data_t *sdata,int ipixel);

float grasp_segment_get_pixel_hvp(const sensor_data_t *sdata,int ipixel, int ivm);

float grasp_segment_get_pixel_meas_wl(const sensor_data_t *sdata,int ipixel, int iwl);

float grasp_segment_get_pixel_meas_ind_wl(const sensor_data_t *sdata,int ipixel, int iwl);

int grasp_segment_get_pixel_meas_nsurf(const sensor_data_t *sdata,int ipixel, int iwl);

float grasp_segment_get_pixel_meas_gaspar(const sensor_data_t *sdata,int ipixel, int iwl);

float grasp_segment_get_pixel_meas_sza(const sensor_data_t *sdata,int ipixel, int iwl);

float grasp_segment_get_pixel_meas_groundpar(const sensor_data_t *sdata,int ipixel, int iwl, int isurf);

int grasp_segment_get_pixel_meas_nip(const sensor_data_t *sdata,int ipixel, int iwl);

int grasp_segment_get_pixel_meas_meas_type(const sensor_data_t *sdata,int ipixel, int iwl, int ip);

int grasp_segment_get_pixel_meas_nbvm(const sensor_data_t *sdata,int ipixel, int iwl, int ip);

int grasp_segment_get_pixel_meas_ifcov(const sensor_data_t *sdata,int ipixel, int iwl, int ip);

int grasp_segment_get_pixel_meas_ifmp(const sensor_data_t *sdata,int ipixel, int iwl, int ip);

float grasp_segment_get_pixel_meas_thetav(const sensor_data_t *sdata,int ipixel, int iwl, int ip, int ivalidmeas);

float grasp_segment_get_pixel_meas_phi(const sensor_data_t *sdata,int ipixel, int iwl, int ip, int ivalidmeas);

float grasp_segment_get_pixel_meas_cmtrx(const sensor_data_t *sdata,int ipixel, int iwl, int ip, int ivalidmeas);

float grasp_segment_get_pixel_meas_mprof(const sensor_data_t *sdata,int ipixel, int iwl, int ip, int ivalidmeas);

float grasp_segment_get_pixel_meas_tod(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_aod(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_aaod(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_htod(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p11(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p12(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p22(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p33(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p34(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p44(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p11_rel_ang(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p12_rel(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_i(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_q(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_u(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_i_rel_sum(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_p_rel(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_ls(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_dp(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);

float grasp_segment_get_pixel_meas_rl(const sensor_data_t *sdata,int ipixel, int iwl, int ivalidmeas);


//===== Setters for pixel =====


void grasp_segment_pixel_set_pixel_it(pixel_t *pixel ,int it);

void grasp_segment_pixel_set_pixel_ix(pixel_t *pixel ,int ix);

void grasp_segment_pixel_set_pixel_iy(pixel_t *pixel ,int iy);

void grasp_segment_pixel_set_pixel_cloudy(pixel_t *pixel ,int cloudy);

void grasp_segment_pixel_set_pixel_irow(pixel_t *pixel ,int irow);

void grasp_segment_pixel_set_pixel_icol(pixel_t *pixel ,int icol);

void grasp_segment_pixel_set_pixel_file_index(pixel_t *pixel ,int file_index);

void grasp_segment_pixel_set_pixel_x(pixel_t *pixel ,float x);

void grasp_segment_pixel_set_pixel_y(pixel_t *pixel ,float y);

void grasp_segment_pixel_set_pixel_t(pixel_t *pixel ,int64_t t);

void grasp_segment_pixel_set_pixel_out_t(pixel_t *pixel ,int out_t);

void grasp_segment_pixel_set_pixel_out_x(pixel_t *pixel ,int out_x);

void grasp_segment_pixel_set_pixel_out_y(pixel_t *pixel ,int out_y);

void grasp_segment_pixel_set_pixel_masl(pixel_t *pixel ,float masl);

void grasp_segment_pixel_set_pixel_hobs(pixel_t *pixel ,float hobs);

void grasp_segment_pixel_set_pixel_land_percent(pixel_t *pixel ,float land_percent);

void grasp_segment_pixel_set_pixel_nwl(pixel_t *pixel ,int nwl);

void grasp_segment_pixel_set_pixel_ifgas(pixel_t *pixel ,int ifgas);

void grasp_segment_pixel_set_pixel_hvp(pixel_t *pixel , int ivm, float hvp);

void grasp_segment_pixel_set_pixel_meas_wl(pixel_t *pixel , int iwl, float wl);

void grasp_segment_pixel_set_pixel_meas_ind_wl(pixel_t *pixel , int iwl, float ind_wl);

void grasp_segment_pixel_set_pixel_meas_nsurf(pixel_t *pixel , int iwl, int nsurf);

void grasp_segment_pixel_set_pixel_meas_gaspar(pixel_t *pixel , int iwl, float gaspar);

void grasp_segment_pixel_set_pixel_meas_sza(pixel_t *pixel , int iwl, float sza);

void grasp_segment_pixel_set_pixel_meas_groundpar(pixel_t *pixel , int iwl, int isurf, float groundpar);

void grasp_segment_pixel_set_pixel_meas_nip(pixel_t *pixel , int iwl, int nip);

void grasp_segment_pixel_set_pixel_meas_meas_type(pixel_t *pixel , int iwl, int ip, int meas_type);

void grasp_segment_pixel_set_pixel_meas_nbvm(pixel_t *pixel , int iwl, int ip, int nbvm);

void grasp_segment_pixel_set_pixel_meas_ifcov(pixel_t *pixel , int iwl, int ip, int ifcov);

void grasp_segment_pixel_set_pixel_meas_ifmp(pixel_t *pixel , int iwl, int ip, int ifmp);

void grasp_segment_pixel_set_pixel_meas_thetav(pixel_t *pixel , int iwl, int ip, int ivalidmeas, float thetav);

void grasp_segment_pixel_set_pixel_meas_phi(pixel_t *pixel , int iwl, int ip, int ivalidmeas, float phi);

void grasp_segment_pixel_set_pixel_meas_cmtrx(pixel_t *pixel , int iwl, int ip, int ivalidmeas, float cmtrx);

void grasp_segment_pixel_set_pixel_meas_mprof(pixel_t *pixel , int iwl, int ip, int ivalidmeas, float mprof);

void grasp_segment_pixel_set_pixel_meas_tod(pixel_t *pixel , int iwl, int ivalidmeas, float tod);

void grasp_segment_pixel_set_pixel_meas_aod(pixel_t *pixel , int iwl, int ivalidmeas, float aod);

void grasp_segment_pixel_set_pixel_meas_aaod(pixel_t *pixel , int iwl, int ivalidmeas, float aaod);

void grasp_segment_pixel_set_pixel_meas_htod(pixel_t *pixel, int iwl, int ivalidmeas, float htod);

void grasp_segment_pixel_set_pixel_meas_p11(pixel_t *pixel , int iwl, int ivalidmeas, float p11);

void grasp_segment_pixel_set_pixel_meas_p12(pixel_t *pixel , int iwl, int ivalidmeas, float p12);

void grasp_segment_pixel_set_pixel_meas_p22(pixel_t *pixel , int iwl, int ivalidmeas, float p22);

void grasp_segment_pixel_set_pixel_meas_p33(pixel_t *pixel , int iwl, int ivalidmeas, float p33);

void grasp_segment_pixel_set_pixel_meas_p34(pixel_t *pixel , int iwl, int ivalidmeas, float p34);

void grasp_segment_pixel_set_pixel_meas_p44(pixel_t *pixel , int iwl, int ivalidmeas, float p44);

void grasp_segment_pixel_set_pixel_meas_p11_rel_ang(pixel_t *pixel , int iwl, int ivalidmeas, float p11_rel_ang);

void grasp_segment_pixel_set_pixel_meas_p12_rel(pixel_t *pixel , int iwl, int ivalidmeas, float p12_rel);

void grasp_segment_pixel_set_pixel_meas_i(pixel_t *pixel , int iwl, int ivalidmeas, float i);

void grasp_segment_pixel_set_pixel_meas_q(pixel_t *pixel , int iwl, int ivalidmeas, float q);

void grasp_segment_pixel_set_pixel_meas_u(pixel_t *pixel , int iwl, int ivalidmeas, float u);

void grasp_segment_pixel_set_pixel_meas_p(pixel_t *pixel , int iwl, int ivalidmeas, float p);

void grasp_segment_pixel_set_pixel_meas_i_rel_sum(pixel_t *pixel , int iwl, int ivalidmeas, float i_rel_sum);

void grasp_segment_pixel_set_pixel_meas_p_rel(pixel_t *pixel , int iwl, int ivalidmeas, float p_rel);

void grasp_segment_pixel_set_pixel_meas_ls(pixel_t *pixel , int iwl, int ivalidmeas, float ls);

void grasp_segment_pixel_set_pixel_meas_dp(pixel_t *pixel , int iwl, int ivalidmeas, float dp);

void grasp_segment_pixel_set_pixel_meas_rl(pixel_t *pixel , int iwl, int ivalidmeas, float rl);



//===== Getters for pixel =====

int grasp_segment_pixel_get_pixel_it(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_ix(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_iy(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_cloudy(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_irow(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_icol(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_file_index(const pixel_t *pixel );

float grasp_segment_pixel_get_pixel_x(const pixel_t *pixel );

float grasp_segment_pixel_get_pixel_y(const pixel_t *pixel );

int64_t grasp_segment_pixel_get_pixel_t(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_out_t(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_out_x(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_out_y(const pixel_t *pixel );

float grasp_segment_pixel_get_pixel_masl(const pixel_t *pixel );

float grasp_segment_pixel_get_pixel_hobs(const pixel_t *pixel );

float grasp_segment_pixel_get_pixel_land_percent(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_nwl(const pixel_t *pixel );

int grasp_segment_pixel_get_pixel_ifgas(const pixel_t *pixel );

float grasp_segment_pixel_get_pixel_hvp(const pixel_t *pixel , int ivm);

float grasp_segment_pixel_get_pixel_meas_wl(const pixel_t *pixel , int iwl);

float grasp_segment_pixel_get_pixel_meas_ind_wl(const pixel_t *pixel , int iwl);

int grasp_segment_pixel_get_pixel_meas_nsurf(const pixel_t *pixel , int iwl);

float grasp_segment_pixel_get_pixel_meas_gaspar(const pixel_t *pixel , int iwl);

float grasp_segment_pixel_get_pixel_meas_sza(const pixel_t *pixel , int iwl);

float grasp_segment_pixel_get_pixel_meas_groundpar(const pixel_t *pixel , int iwl, int isurf);

int grasp_segment_pixel_get_pixel_meas_nip(const pixel_t *pixel , int iwl);

int grasp_segment_pixel_get_pixel_meas_meas_type(const pixel_t *pixel , int iwl, int ip);

int grasp_segment_pixel_get_pixel_meas_nbvm(const pixel_t *pixel , int iwl, int ip);

int grasp_segment_pixel_get_pixel_meas_ifcov(const pixel_t *pixel , int iwl, int ip);

int grasp_segment_pixel_get_pixel_meas_ifmp(const pixel_t *pixel , int iwl, int ip);

float grasp_segment_pixel_get_pixel_meas_thetav(const pixel_t *pixel , int iwl, int ip, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_phi(const pixel_t *pixel , int iwl, int ip, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_cmtrx(const pixel_t *pixel , int iwl, int ip, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_mprof(const pixel_t *pixel , int iwl, int ip, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_tod(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_aod(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_aaod(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_htod(const pixel_t *pixel, int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p11(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p12(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p22(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p33(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p34(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p44(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p11_rel_ang(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p12_rel(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_i(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_q(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_u(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_i_rel_sum(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_p_rel(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_ls(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_dp(const pixel_t *pixel , int iwl, int ivalidmeas);

float grasp_segment_pixel_get_pixel_meas_rl(const pixel_t *pixel , int iwl, int ivalidmeas);


#ifdef	__cplusplus
}
#endif

#endif	/* GRASP_INPUT_SEGMENT_H */

