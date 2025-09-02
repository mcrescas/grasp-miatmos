/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* 
 * File:   grasp_output_segment_result.h
 * Author: fuertes
 *
 * Created on 3 de octubre de 2014, 14:56
 */

#ifndef GRASP_OUTPUT_SEGMENT_RESULT_H
#define	GRASP_OUTPUT_SEGMENT_RESULT_H

#ifdef	__cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include "mod_par_inv.inc"
#include "mod_par_OS.inc"
#include "mod_par_DLS.inc"
#include "mod_par_DLS_bin.inc"
#include "../settings/grasp_products.h"
#include "../input/grasp_input_segment.h"
#include "../global/grasp_parameters.h"
    
#ifdef WARN_DRY
#warning "__RETRIEVAL_OUTPUT_DEFINITION__ binded"
#endif          
    
#define index_ext 1
#define index_ssa 2
#define index_aext  3
#define index_lr  4


// Retrieval output
// Structure contains retrieval output data :
// res       - single pixel total residual (meas + smothness constrain )
// resa,resr - detailed absolut and relative measurement residuals for single pixel 
// niter       - number of iterations
// rest        - total residual for multi-pixel retrieval
// resat,resrt - detailed absolut and relative measurement residuals for segment
// par         - retrieved aerosol and surface reflectance parameters
// ngrid       - number of grid radii for SD (saved to print output)
// radius      - grid radii (saved to print output)
// SDL         - if applicable, precalculated lognormal bins SD (saved to print output)
   
    typedef struct output_pixel_residual_{
         int   niter;
         float res;
         float resa[_KKNOISE];
         float resr[_KKNOISE];
    } output_pixel_residual;
    
    typedef struct output_segment_residual_{
         int   niter;
         float rest;  
         float resat[_KKNOISE];
         float resrt[_KKNOISE];
         output_pixel_residual pixel[_KIMAGE];  
    } output_segment_residual;
    
    typedef struct output_pixel_retr_par_{
         float par[_KPARS];
//         ! Retrieved SD values
         float sd[_KSD+1][4*_KIDIM3]; // ! index=0 for total SD
//         ! Cv values
         float Cv[_KSD][_KIDIM3];
//         ! ShD values
         float  ShD[_KSD][_KIDIM3];
//         ! normalized VD values
         float VD[_KSD][_KIDIM3];
//         ! Values of surface parameters
         float BRF[_KBF][_KW]; // ! land parameters
         float BRP[_KBF][_KW]; //  ! land parameters
         float BRM[_KBF][_KW]; // ! water parameters
    } output_pixel_retr_par;
        
    typedef struct output_segment_retr_par_{
         output_pixel_retr_par pixel[_KIMAGE];  
    } output_segment_retr_par;
    
     typedef struct output_segment_info_{
         int npixels;
         int nsd;       // KSD
         int nnoises;   // KKNOISE
         int nbins;     // NRC
         int ngas;
         int nwl;       // KW
         float wl[_KW];
         par_number_NDIM ndim;
         int   ngrid[_KSD+1];           // number of grid radii
         float radius[_KSD+1][4*_KIDIM3];     // grid radius values (um)
         float sd_lb[_KCpar][_NRR];   // values of SD (cv=1) for each precalculated lognormal bin
         int nhlv; // number of levels for forcing and flux
         float hlv[_KNT]; // values of levels (in meters!) for forcing and flux
         float delta_ut; // Real time in seconds
         float delta_ct; // User time in seconds
         bool flag_plus;
         par_number_NDIM ndim_plus;
         int nchem[_KSD]; // N_CHEM_MAX
         int nbrf;       // KBF
         int nbpf;       // KBF
         int nbrm;       // KBF
     } output_segment_info;

    typedef struct output_pixel_coordinates_ { // position of the pixel
        float 	 x_lon;   
        float    y_lat;
        int64_t  t_masl;
    }output_pixel_coordinates;
     
    typedef struct output_segment_coordinates_{
         output_pixel_coordinates  pixel[_KIMAGE];
    }output_segment_coordinates;    
     
    typedef struct output_segment_fitting_{
        sensor_data_t segment_fit;
    }output_segment_fitting;
    
    typedef struct output_segment_retrieval_ {
         output_segment_residual res;  
         output_segment_retr_par par;  
         output_segment_fitting fit;
         output_segment_info   information;
    } output_segment_retrieval;

    // Optical characteristics      

// ext,ssa,aext   - spectral ext, sca and aext for each aerosol component
// Aexp      - Angstrom exponent (wl(4)/wl(5))
      typedef struct output_pixel_opt_wl_ {
         float extt;
         float ssat;
         float aextt;
         float ext[_KSD];
         float ssa[_KSD];
         float aext[_KSD];
         float ext_cut_off[4][_KSD+1];
         float ssa_cut_off[4][_KSD+1];
      } output_pixel_opt_wl;
      
      typedef struct output_pixel_opt_ {
         float Aexp;
         output_pixel_opt_wl wl[_KW];
      } output_pixel_opt;
      
      typedef struct output_segment_opt_ {
         output_pixel_opt pixel[_KIMAGE];
      } output_segment_opt;
      
// Refractive index
      typedef struct output_pixel_rindex_wl_ {
         float mreal[_KSD];
         float mimag[_KSD];
      } output_pixel_rindex_wl;
      
      typedef struct output_pixel_rindex_ {
         output_pixel_rindex_wl wl[_KW];
      } output_pixel_rindex;
      
      typedef struct output_segment_rindex_ {
         output_pixel_rindex pixel[_KIMAGE];
      } output_segment_rindex;

// Phase matrix :
      typedef struct output_pixel_ph_matrix_wl_ {
         float ph11[_KSD][_KMpar];
         float ph12[_KSD][_KMpar];
         float ph22[_KSD][_KMpar];
         float ph33[_KSD][_KMpar];
         float ph34[_KSD][_KMpar];
         float ph44[_KSD][_KMpar];
         
         float pht11[_KMpar];
         float pht12[_KMpar];
         float pht22[_KMpar];
         float pht33[_KMpar];
         float pht34[_KMpar];
         float pht44[_KMpar];         

         float ph11_cut_off[4][_KSD+1][_KMpar];
         float ph12_cut_off[4][_KSD+1][_KMpar];
         float ph22_cut_off[4][_KSD+1][_KMpar];
         float ph33_cut_off[4][_KSD+1][_KMpar];
         float ph34_cut_off[4][_KSD+1][_KMpar];
         float ph44_cut_off[4][_KSD+1][_KMpar];
      } output_pixel_ph_matrix_wl;
      
      typedef struct output_pixel_ph_matrix_ {   
         output_pixel_ph_matrix_wl wl[_KW];
      }output_pixel_ph_matrix;
      
      typedef struct output_segment_ph_matrix_ {
         int  nangle;
         float angle[_KMpar];
         output_pixel_ph_matrix pixel[_KIMAGE];
      } output_segment_ph_matrix;

//Lidar and depolarization ratios :
      
      typedef struct output_pixel_lidar_ratio_wl_ {
         float lr[_KSD];
         float ldpar[_KSD];
         float ldper[_KSD];
         float lrt;
         float ldprt;
      } output_pixel_lidar_ratio_wl;
      
      typedef struct output_pixel_lidar_ratio_ {
         output_pixel_lidar_ratio_wl wl[_KW];
      } output_pixel_lidar_ratio;
      
      typedef struct output_segment_lidar_ratio_ {
         output_pixel_lidar_ratio pixel[_KIMAGE];
      } output_segment_lidar_ratio;

// Chemistry parameters
      typedef struct output_pixel_chemistry_ {
         float rh[_KSD];
         float fwtr[_KSD];
         float fslbl[_KSD];
         float vfract[_KSD][_N_CHEM_MAX];
      } output_pixel_chemistry;
              
      typedef struct output_segment_chemistry_ { 
         output_pixel_chemistry pixel[_KIMAGE];
      } output_segment_chemistry;

// Two mode aerosol characteristics
// Structure contains output data (microphysical parameters):
// 0 - total, 1 - fine mode, 2 - coarse mode 
// reff        - volume median radius
// std         - standard deviation
// cv          - concentration 
// rm          - median radius 
// ext         - ext each aerosol component
      typedef struct output_pixel_sd2m_mph_ {
         float cv[3];
         float std[3];
         float rm[3];
         float reff[3];
      } output_pixel_sd2m_mph; 
                 
      typedef struct output_segment_sd2m_mph_ {
         output_pixel_sd2m_mph pixel[_KIMAGE];
      }output_segment_sd2m_mph;

      typedef struct output_pixel_sd2m_opt_wl_ {	
         float ext[2];
      } output_pixel_sd2m_opt_wl;
      
      typedef struct output_pixel_sd2m_opt_ {
         output_pixel_sd2m_opt_wl wl[_KW];
      } output_pixel_sd2m_opt;
      
      typedef struct output_segment_sd2m_opt_ {
         output_pixel_sd2m_opt pixel[_KIMAGE];
      } output_segment_sd2m_opt;

      typedef struct output_segment_sd2m_ {
         output_segment_sd2m_mph mph;
         output_segment_sd2m_opt opt;
      } output_segment_sd2m;
//-----------------------------------------------------------------------------------------
// AL Particulate Matter
      typedef struct output_pixel_PM_ {
         float  pm[2];
      } output_pixel_PM;
      typedef struct output_segment_PM_ {
         output_pixel_PM   pixel[_KIMAGE];
      } output_segment_PM;
// AL aerosol or cloud type
// index value — Aerosol type
// 0 – Complex mixture
// 1 – Background Aerosol
// 2 – Water/Maritime
// 3 — Urban Polluted
// 4 – Mixed aerosol
// 5 – Urban Clean
// 6 – Smoke Smoldering
// 7 – Smoke flaming
// 8 – Mineral dust
      typedef struct output_pixel_types_ {
         int  index;
      } output_pixel_types;
      typedef struct output_segment_types_ {
         output_pixel_types   pixel[_KIMAGE];
      } output_segment_types;
 
//-----------------------------------------------------------------------------------------
    // Gases
    typedef struct output_pixel_gases_wl_ {
             float abs[_NMG+1];
    }output_pixel_gases_wl;

    typedef struct output_pixel_gases_ {
             output_pixel_gases_wl  wl[_KW];
    } output_pixel_gases;

    typedef struct output_segment_gases_ {
             output_pixel_gases pixel[_KIMAGE];
    }output_segment_gases;
//-----------------------------------------------------------------------------------------

// Surface      
      
// Surface albedo 
      
      typedef struct output_pixel_surface_wl_{
         float dhr;
         float bhr_iso;
      } output_pixel_surface_wl;
      
      typedef struct output_pixel_surface_{
         float ndvi;
         output_pixel_surface_wl wl[_KW];
      }output_pixel_surface;
      
      typedef struct output_segment_surface_ {
         output_pixel_surface pixel[_KIMAGE];
      } output_segment_surface;

// Radiative forcing

// Radiative broad band flux and forcing
      typedef struct output_pixel_bbflux_ {
         int nhlv; // Repeated in output_segment_info structure
         float bbufx0[_KNT];
         float bbdfx0[_KNT];
         float bbufxa[_KNT];
         float bbdfxa[_KNT];
         float hlv[_KNT]; // In Km!! but in info structure is in meters
      } output_pixel_bbflux;
  
      typedef struct output_segment_bbflux_ {
         output_pixel_bbflux pixel[_KIMAGE];
      } output_segment_bbflux;

      typedef struct output_pixel_forcing_ {
         int   nhlv; // Repeated in output_segment_info structure
         float netforc[_KNT];
         float forceff[_KNT];
         float hlv[_KNT]; // In Km!! but in info structure is in meters
      } output_pixel_forcing;
      
      typedef struct output_segment_forcing_ {
         output_pixel_forcing pixel[_KIMAGE];
      } output_segment_forcing;

      typedef struct output_segment_rad_forcing_ {
         output_segment_bbflux  bbflux;
         output_segment_forcing forcing;
      } output_segment_rad_forcing;

// Error estimations

// ERRP - Standard deviations of retrieved parameter logarithms (~relative errors)
// BIASP - Standard deviation of systematic errors of retrieved parameter logarithms
// TSTDP - Standard deviation sqrt(ERRP*ERRP+BIASP*BIASP)
// ERR_ - Standard deviations of retrieved optical characteristic logarithms (~relative errors)
// BIAS_ - Standard deviations of systematic errors of retrieved optical characteristic logarithms
// TSTD_ - Standard deviations sqrt(ERR_*ERR_+BIAS_*BIAS_)
// structure par      contains BIAS & ERR for all retrieved (of aerosol, clouds and surface)
// structure aerosol1 contains BIAS & ERR for ext & ssa - optical thickness and single scattering albedo of aerosol
// structure aerosol2 contains BIAS & ERR for lr        - lidar ratio of aerosol
// structure cloud1   contains BIAS & ERR for ext & ssa - optical thickness and single scattering albedo of clouds
// structure cloud2   contains BIAS & ERR for lr        - lidar ratio of clouds
      typedef struct output_pixel_err_estim_par_ {
         float ERRP[_KPARS];
         float BIASP[_KPARS];
         float TSTDP[_KPARS];
         float sd_err[_KSD+1][4*_KIDIM3]; // index=0 for total SD errors
                                        // can't be filled if number of particle components > 1
         
         bool use_mask;
         int num;
         int idim1[3];
         int par_type[3];
         bool KNSINGF_mask[_KPARS];
      } output_pixel_err_estim_par;
  
      typedef struct output_segment_err_estim_par_ {
         output_pixel_err_estim_par pixel[_KIMAGE];
      } output_segment_err_estim_par;

      typedef struct output_pixel_err_estim_particles_opt_wl_ {
         float ERR_ext[_KSD];
         float BIAS_ext[_KSD];
         float TSTD_ext[_KSD];
         float ERR_extt;
         float BIAS_extt;
         float TSTD_extt;
         float ERR_ssa[_KSD];
         float BIAS_ssa[_KSD];
         float TSTD_ssa[_KSD];
         float ERR_ssat;
         float BIAS_ssat;
         float TSTD_ssat;
         float ERR_aext[_KSD];
         float BIAS_aext[_KSD];
         float TSTD_aext[_KSD];
         float ERR_aextt;
         float BIAS_aextt;
         float TSTD_aextt;
      } output_pixel_err_estim_particles_opt_wl;
      
      typedef struct output_pixel_err_estim_particles_opt_ {
         output_pixel_err_estim_particles_opt_wl wl[_KW];
      } output_pixel_err_estim_particles_opt;
      
      typedef struct output_segment_err_estim_particles_opt_ {
         output_pixel_err_estim_particles_opt pixel[_KIMAGE];
      } output_segment_err_estim_particles_opt;
      
      typedef struct output_pixel_err_estim_particles_lidar_wl_ {
         float ERR_lr[_KSD];
         float BIAS_lr[_KSD];     
         float TSTD_lr[_KSD];
         float ERR_lrt;
         float BIAS_lrt;
         float TSTD_lrt;
      } output_pixel_err_estim_particles_lidar_wl;
      
      typedef struct output_pixel_err_estim_particles_lidar_ {
         output_pixel_err_estim_particles_lidar_wl wl[_KW];
      } output_pixel_err_estim_particles_lidar;
      
      typedef struct output_segment_err_estim_particles_lidar_ {
         output_pixel_err_estim_particles_lidar pixel[_KIMAGE];
      } output_segment_err_estim_particles_lidar;

//MEH: structure for SD error estimates when it is not part of the retrieval
      typedef struct output_pixel_err_estim_particles_mic_rad_ {
        float ERR_sdt;
        float BIAS_sdt;
        float TSTD_sdt;
      } output_pixel_err_estim_particles_mic_rad;

      typedef struct output_pixel_err_estim_particles_mic_ {
        output_pixel_err_estim_particles_mic_rad grid[4*_KIDIM3];
      } output_pixel_err_estim_particles_mic;

     typedef struct output_segment_err_estim_particles_mic_ {
      output_pixel_err_estim_particles_mic pixel[_KIMAGE];
     } output_segment_err_estim_particles_mic;

//****

      typedef struct output_segment_err_estim_particles_ { 
         output_segment_err_estim_particles_opt    opt;
         output_segment_err_estim_particles_lidar  lidar;
         output_segment_err_estim_particles_mic    mic;
      } output_segment_err_estim_particles;

      typedef struct output_segment_err_estim_ { 
         output_segment_err_estim_par      par;
         output_segment_err_estim_particles aerosol ;
//         output_segment_err_estim_mic_sd    sd;
      } output_segment_err_estim;

      
// Aerosol
      
      typedef struct output_segment_particles_ {
         output_segment_opt        opt;
         output_segment_rindex     rind;
         output_segment_ph_matrix  phmx;
         output_segment_lidar_ratio lidar;
         output_segment_sd2m       sd2m;
         output_segment_chemistry  chem;
         output_segment_PM         pm;
         output_segment_types      types;
      } output_segment_particles;


// General output
      typedef struct output_segment_general_ {
          output_segment_products     products;
          output_segment_coordinates  coord;
          output_segment_retrieval    retrieval;
          output_segment_particles    aerosol;
          output_segment_gases        gases;
          output_segment_surface      surface;
          output_segment_err_estim    errest;
          output_segment_rad_forcing  forcing;
      } output_segment_general;
    
      
      
/**
 * Return array of retrieved parameters. It can be used within grasp_parameters 
 * library to extract easily information in retrieved parameters 
 * @param output Output of retrieval from a segment
 * @param ipix Index of pixel
 * @return array of retrieved parameters
 */      
const float *grasp_output_segment_parameters(const output_segment_general *output, int ipix);

/**
 * Return true if output retrieval residuals is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */        
bool grasp_output_segment_products_retrieval_res (const output_segment_general *output);

/**
 * Return true if output retrieval parameters is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_retrieval_par (const output_segment_general *output);

/**
 * Return true if output retrieval fitting is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_retrieval_fit (const output_segment_general *output);

/**
 * Return true if output for aerosol optical products is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_opt (const output_segment_general *output);

/**
 * Return true if output for aerosol refractive index is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_rind (const output_segment_general *output);

/**
 * Return true if output of aerosol chemistry is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_chem (const output_segment_general *output);

/**
 * Return true if output of aerosol phase matrix is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_phmx (const output_segment_general *output);

/**
 * Return true if output for aerosol lidar products is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_lidar (const output_segment_general *output);

/**
 * Return true if output products for simulated two modes is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_sd2m_mph (const output_segment_general *output);

/**
 * Return true if output extinction for aerosol two modes simulated is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_sd2m_ext (const output_segment_general *output);

/**
 * Return true if output aerosol particular matter is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_pm (const output_segment_general *output);

/**
 * Return true if output for aerosol types is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_aerosol_types (const output_segment_general *output);

/**
 * If surface products is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_surface_surf (const output_segment_general *output);

/**
 * If surface bhr_iso product is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_surface_bhr_iso (const output_segment_general *output);

/**
 * Return true if output error estimation for parameters is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_errest_par (const output_segment_general *output);

/**
 * Return true if output error estimation for aerosol optical products is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_errest_aerosol_opt (const output_segment_general *output);

/**
 * Return true if output error estimation for aerosol lidar signal is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_errest_aerosol_lidar (const output_segment_general *output);

//MEH:
/**
 * Return true if output error estimation for sd when it is not part of the retrieved parameters
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_errest_aerosol_mic (const output_segment_general *output);

/**
 * Return true if output for forcing flux is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_forcing_bbflux (const output_segment_general *output);

/**
 * Return true if output for forcing is available
 * @param output Output of retrieval from a segment
 * @return if it is available
 */
bool grasp_output_segment_products_forcing_forcing (const output_segment_general *output);

/**
 * Return longitude of the pixel
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return longitude of the pixel
 */
float grasp_output_segment_coord_pixel_x (const output_segment_general *output,  int ipix);

/**
 * Return latitude of the pixel
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return latitude of the pixel
 */
float grasp_output_segment_coord_pixel_y (const output_segment_general *output,  int ipix);

/**
 * Return time of the pixel
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return time of the pixel
 */
int64_t grasp_output_segment_coord_pixel_t (const output_segment_general *output,  int ipix);

/**
 * Return number of iterations in multipixel scenario
 * @param output Output of retrieval from a segment
 * @return number of iterations in multipixel scenario
 */
int grasp_output_segment_retrieval_res_niter (const output_segment_general *output);

/**
 * Return total residual for multi-pixel retrieval
 * @param output Output of retrieval from a segment
 * @return total residual for multi-pixel retrieval
 */
float grasp_output_segment_retrieval_res_rest (const output_segment_general *output);

/**
 * Return detailed absolute measurement residuals for segment
 * @param output Output of retrieval from a segment 
 * @param inoise Number of noise
 * @return detailed absolute measurement residuals for segment
 */
float grasp_output_segment_retrieval_res_resat (const output_segment_general *output, int inoise);

/**
 * Return detailed relative measurement residuals for segment
 * @param output Output of retrieval from a segment
 * @param inoise Number of noise
 * @return detailed relative and relative measurement residuals for segment
 */
float grasp_output_segment_retrieval_res_resrt (const output_segment_general *output, int inoise);

/**
 * Return number of iteration in single pixel scenario
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return number of iteration in single pixel scenario
 */
int grasp_output_segment_retrieval_res_pixel_niter (const output_segment_general *output, int ipix);

/**
 * Return single pixel total residual (meas + smothness constrain )
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return single pixel total residual (meas + smothness constrain )
 */
float grasp_output_segment_retrieval_res_pixel_res (const output_segment_general *output, int ipix);

/**
 * Return detailed absolute measurement residuals for single pixel 
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param inoise Number of noise
 * @return detailed absolute measurement residuals for single pixel 
 */
float grasp_output_segment_retrieval_res_pixel_resa (const output_segment_general *output, int ipix, int inoise);

/**
 * Return detailed relative measurement residuals for single pixel 
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param inoise Number of noise
 * @return detailed relative measurement residuals for single pixel 
 */
float grasp_output_segment_retrieval_res_pixel_resr (const output_segment_general *output, int ipix, int inoise);

/**
 * Return number of grid radii for SD (saved to print output)
 * @param output Output of retrieval from a segment
 * @return number of grid radii for SD (saved to print output)
 */
int grasp_output_segment_retrieval_par_ngrid (const output_segment_general *output);

/**
 * Return grid radii (saved to print output)
 * @param output Output of retrieval from a segment
 * @param irr
 * @return grid radii (saved to print output)
 */
float grasp_output_segment_retrieval_par_radius (const output_segment_general *output, int irr);

/**
 * Return if applicable, precalculated lognormal bins SD (saved to print output)
 * @param output Output of retrieval from a segment
 * @param irr
 * @param irc
 * @return if applicable, precalculated lognormal bins SD (saved to print output)
 */
float grasp_output_segment_retrieval_par_sdl (const output_segment_general *output, int irr, int irc);

/**
 * Return retrieved aerosol and surface reflectance parameters
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param ipar
 * @return retrieved aerosol and surface reflectance parameters
 */
float grasp_output_segment_retrieval_par_parameters (const output_segment_general *output, int ipix, int ipar);

/**
 * Return retrieved size distribution simplified for easy interpretation
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param isd 0 if it is total size distriubtion or the number of the mode
 * @param bin value of the bin
 * @return value of the bin of the SD
 */
float grasp_output_segment_retrieval_par_sd (const output_segment_general *output, int ipix, int isd, int ibin);

/**
 * Return segment fit
 * @param output Output of retrieval from a segment
 * @return Fit segment. It is like sdata structure but filled with fitting information
 */
const sensor_data_t *grasp_output_segment_retrieval_fit_segment_fit (const output_segment_general *output);

/**
 * Return fit for specific pixel
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return Fit pixel. It is like sdata structure but filled with fitting information
 */
const pixel_t *grasp_output_segment_retrieval_fit_pixel_fit (const output_segment_general *output, int ipix);

/**
 * Return number of pixels of current segment
 */
int grasp_output_segment_retrieval_information_npixels (const output_segment_general *output);

/**
 * Return number of modes of current segment
 */
int grasp_output_segment_retrieval_information_nsd (const output_segment_general *output);

/**
 * Return number of noises of current segment
 */
int grasp_output_segment_retrieval_information_nnoises (const output_segment_general *output);

/**
 * Return number of bins of current segment
 */
int grasp_output_segment_retrieval_information_nbins (const output_segment_general *output);

 /**
 * Return number of wavelenghts of current segment
 */
int grasp_output_segment_retrieval_information_nwl (const output_segment_general *output);

/**
 * Return the value of specific weavelenght in nanometers
 */
float grasp_output_segment_retrieval_information_wl (const output_segment_general *output, int iwl);

/**
 * Return NDIM struct (that can be used to know the size and shape of parameters array)
 * @param output
 * @return 
 */
par_number_NDIM grasp_output_segment_retrieval_information_ndim (const output_segment_general *output);

/**
 * Return number of grid radii points
 * @param output
 * @param isd
 * @return 
 */
int grasp_output_segment_retrieval_information_ngrid (const output_segment_general *output, int isd);

/**
 * Return values of grid radius points (X axis of SD plot)
 * @param output
 * @param isd
 * @param ipar
 * @return 
 */
float grasp_output_segment_retrieval_information_radius (const output_segment_general *output, int isd, int ipar);

/**
 * Return values of grid values of the bings in case of lognormal bbins sd (X axis of SD plot)
 * @param output
 * @param irr
 * @param irc
 * @return 
 */
float grasp_output_segment_retrieval_information_sd_lb (const output_segment_general *output, int irr, int irc);


/**
 * Return number of levels for forcing and flux
 */
int grasp_output_segment_retrieval_information_nhlv (const output_segment_general *output);

/**
 * Return the height of levels for forcing and flux in meters
 */
float grasp_output_segment_retrieval_information_hlv (const output_segment_general *output, int ihlv);


/**
 * Return real time for segment processing
 * @param output segment results
 * @return total number of seconds (real time) of segment processing 
 */
float grasp_output_segment_retrieval_information_delta_ut (const output_segment_general *output);

/**
 * Return real time for segment processing
 * @param output segment results
 * @return total number of seconds (user time) of segment processing 
 */
float grasp_output_segment_retrieval_information_delta_ct (const output_segment_general *output);

/**
 * Return real time for segment processing
 * @param output segment results
 * @param isd indes of mode
 * @return total number of seconds (user time) of segment processing 
 */
float grasp_output_segment_retrieval_information_nchem (const output_segment_general *output, int isd);


///////////////

/**
 * Return angstrom exponent for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return angstrom exponent for aerosol optical properties
 */
float grasp_output_segment_aerosol_opt_aexp (const output_segment_general *output, int ipix);

/**
 * Return total extinction for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return total extinction for aerosol optical properties
 */
float grasp_output_segment_aerosol_opt_extt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return total single scattering albedo for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return total single scattering albedo for aerosol optical properties
 */
float grasp_output_segment_aerosol_opt_ssat (const output_segment_general *output, int ipix, int iwl);

/**
 * Return total absorption extinction for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return total absorption extinction for aerosol optical properties
 */
float grasp_output_segment_aerosol_opt_aextt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return extinction for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return extinction for aerosol optical properties
 */
float grasp_output_segment_aerosol_opt_ext (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return single scattering albedo for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return single scattering albedo for aerosol optical properties
 */
float grasp_output_segment_aerosol_opt_ssa (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return absorption extinction for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return absorption extinction for aerosol optical properties
 */
float grasp_output_segment_aerosol_opt_aext (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return real part refractive index for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return real part refractive index for aerosol optical properties
 */
float grasp_output_segment_aerosol_rind_mreal (const output_segment_general *output, int ipix, int iwl, int isd);


/**
 * Return imaginary part refractive index for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return imaginary part refractive index for aerosol optical properties
 */
float grasp_output_segment_aerosol_rind_mimag (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return number of angles of phase matrix for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @return number of angles of phase matrix for aerosol optical properties
 */
int grasp_output_segment_aerosol_phmx_nangle (const output_segment_general *output);

/**
 * Return angles of the phase matrix for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param impar
 * @return angles of the phase matrix for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_angle (const output_segment_general *output, int impar);

/**
 * Return p11 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @return p11 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_ph11 (const output_segment_general *output, int ipix, int iwl, int impar, int isd);

/**
 * Return p12 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @return p12 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_ph12 (const output_segment_general *output, int ipix, int iwl, int impar, int isd);

/**
 * Return p22 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @return p22 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_ph22 (const output_segment_general *output, int ipix, int iwl, int impar, int isd);

/**
 * Return p33 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @return p33 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_ph33 (const output_segment_general *output, int ipix, int iwl, int impar, int isd);

/**
 * Return p34 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @return p34 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_ph34 (const output_segment_general *output, int ipix, int iwl, int impar, int isd);

/**
 * Return p44 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @return p44 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_ph44 (const output_segment_general *output, int ipix, int iwl, int impar, int isd);

/**
 * Return total p11 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @return total p11 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_pht11 (const output_segment_general *output, int ipix, int iwl, int impar);

/**
 * Return total p12 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @return total p12 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_pht12 (const output_segment_general *output, int ipix, int iwl, int impar);

/**
 * Return total p22 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @return total p22 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_pht22 (const output_segment_general *output, int ipix, int iwl, int impar);

/**
 * Return total p33 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @return total p33 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_pht33 (const output_segment_general *output, int ipix, int iwl, int impar);

/**
 * Return total p34 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @return total p34 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_pht34 (const output_segment_general *output, int ipix, int iwl, int impar);

/**
 * Return total p44 for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param impar
 * @return total p44 for aerosol optical properties
 */
float grasp_output_segment_aerosol_phmx_pht44 (const output_segment_general *output, int ipix, int iwl, int impar);

/**
 * Return lidar ratio for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return lidar ratio for aerosol optical properties
 */
float grasp_output_segment_aerosol_lidar_lr (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return lidar depolarization ratio (parallel component) N.B. reserved for future developments
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return lidar lidar depolarization ratio (parallel component) N.B. reserved for future developments
 */
float grasp_output_segment_aerosol_lidar_ldpar (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return lidar depolarization ratio (perpendicular component) N.B. reserved for future developments
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return lidar depolarization ratio (perpendicular component) N.B. reserved for future developments
 */
float grasp_output_segment_aerosol_lidar_ldper (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return total lidar ratio for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return total lidar ratio for aerosol optical properties
 */
float grasp_output_segment_aerosol_lidar_lrt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return total lidar depolarization for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return total lidar depolarization for aerosol optical properties
 */
float grasp_output_segment_aerosol_lidar_ldprt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return concentration for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param i mode. 0 for fine and 1 for coarse
 * @return concentration for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_cv (const output_segment_general *output, int ipix, int i);

/**
 * Return standard deviation for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param i mode. 0 for fine and 1 for coarse
 * @return standard deviation for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_std (const output_segment_general *output, int ipix, int i);

/**
 * Return XXX for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param i mode. 0 for fine and 1 for coarse
 * @return XXX for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_rm (const output_segment_general *output, int ipix, int i);

/**
 * Return refractive index for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param i mode. 0 for fine and 1 for coarse
 * @return refractive index for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_reff (const output_segment_general *output, int ipix, int i);

/**
 * Return extinction for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param i mode. 0 for fine and 1 for coarse
 * @return extinction for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_opt_ext (const output_segment_general *output, int ipix, int iwl, int i);

/**
 * Return concentration for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_mph_cv with i = 0
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return concentration for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_cv_fine_mode (const output_segment_general *output, int ipix);

/**
 * Return standard deviation for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_mph_std with i = 0
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return standard deviation for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_std_fine_mode (const output_segment_general *output, int ipix);

/**
 * Return XXX for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_mph_rm with i = 0
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return XXX for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_rm_fine_mode (const output_segment_general *output, int ipix);

/**
 * Return refractive index for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_mph_reff with i = 0
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return refractive index for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_reff_fine_mode (const output_segment_general *output, int ipix);

/**
 * Return extinction for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_opt_ext with i = 0
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return extinction for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_opt_ext_fine_mode (const output_segment_general *output, int ipix, int iwl);

/**
 * Return concentration for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_mph_cv with i = 1
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return concentration for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_cv_coarse_mode (const output_segment_general *output, int ipix);

/**
 * Return standard deviation for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_mph_std with i = 1
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return standard deviation for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_std_coarse_mode (const output_segment_general *output, int ipix);

/**
 * Return XXX for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_mph_rm with i = 1
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return XXX for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_rm_coarse_mode (const output_segment_general *output, int ipix);

/**
 * Return refractive index for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_mph_reff with i = 1
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return refractive index for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_mph_reff_coarse_mode (const output_segment_general *output, int ipix);

/**
 * Return extinction for two simulated modes for aerosol optical properties. Alias to grasp_output_segment_aerosol_sd2m_opt_ext with i = 1
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return extinction for two simulated modes for aerosol optical properties
 */
float grasp_output_segment_aerosol_sd2m_opt_ext_coarse_mode (const output_segment_general *output, int ipix, int iwl);

/**
 * Return relative humidity for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param isd Index of aerosol component
 * @return relative humidity for aerosol optical properties
 */
float grasp_output_segment_aerosol_chem_rh (const output_segment_general *output, int ipix, int isd);

/**
 * Return water fraction for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param isd Index of aerosol component
 * @return water fraction for aerosol optical properties
 */
float grasp_output_segment_aerosol_chem_fwtr (const output_segment_general *output, int ipix, int isd);

/**
 * Return soluble fraction for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param isd Index of aerosol component
 * @return soluble fraction for aerosol optical properties
 */
float grasp_output_segment_aerosol_chem_fslbl (const output_segment_general *output, int ipix, int isd);

/**
 * Return chemical fractions for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param isd Index of aerosol component
 * @param nchem Index of chemical components
 * @return  insoluble fraction for aerosol optical properties
 */
float grasp_output_segment_aerosol_chem_vfract (const output_segment_general *output, int ipix, int isd, int nchem);



/**
 * Return particular matter for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param i
 * @return particular matter for aerosol optical properties
 */
float grasp_output_segment_aerosol_pm_pm (const output_segment_general *output, int ipix, int i);

/**
 * Return aerosol type for aerosol optical properties
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return aerosol type: 0 – Complex mixture; 1 – Background Aerosol; 2 – Water/Maritime; 3 — Urban Polluted; 4 – Mixed aerosol;  5 – Urban Clean;  6 – Smoke Smoldering;  7 – Smoke flaming;  8 – Mineral dust
 */
int grasp_output_segment_aerosol_types_index (const output_segment_general *output, int ipix);

/**
 * Return gas absorption. Index 0 for total
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return 
 */
int grasp_output_segment_gases_absorption (const output_segment_general *output, int ipix, int iwl, int ngas);

///////////////

/**
 * Return surface ndvi
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return surface ndvi
 */
float grasp_output_segment_surface_ndvi (const output_segment_general *output, int ipix);

/**
 * Return surface albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return surface albedo
 */
float grasp_output_segment_surface_dhr (const output_segment_general *output, int ipix, int iwl);

/**
 * Return surface bhr_iso
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return surface albedo
 */
float grasp_output_segment_surface_bhr_iso (const output_segment_general *output, int ipix, int iwl);

/////////////////

/**
 * Return error estimation for parameter
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param ipar
 * @return error estimation for parameter
 */
float grasp_output_segment_errest_par_errp (const output_segment_general *output, int ipix, int ipar);

/**
 * Return bias of error estimation of parameters
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param ipar
 * @return bias of error estimation of parameters
 */
float grasp_output_segment_errest_par_biasp (const output_segment_general *output, int ipix, int ipar);

/**
 * Return total error estimation for parameter
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param ipar
 * @return error estimation for parameter
 */
float grasp_output_segment_errest_par_tstdp (const output_segment_general *output, int ipix, int ipar);

/**
 * Return error estimation for sd
 * +/- err(:,1:nsd) = exp(ln(GOUT%retrieval%par%pixel(ipix)%sd(:,1:nsd)) +/- GOUT%errest%par%pixel(ipix)%sd_err(:,1:nsd))
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param ipar
 * @return error estimation for sd bin
 */
float grasp_output_segment_errest_par_sd_err (const output_segment_general *output, int ipix, int isd, int ibin);

/**
 * Return error estimation of extinction
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return error estimation of extinction
 */
float grasp_output_segment_errest_aerosol_opt_err_ext (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return bias of error estimation of extinction
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return bias of error estimation of extinction
 */
float grasp_output_segment_errest_aerosol_opt_bias_ext (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return total error estimation of extinction
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return error estimation of extinction
 */
float grasp_output_segment_errest_aerosol_opt_tstd_ext (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return error estimation of total extinction
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return error estimation of total extinction
 */
float grasp_output_segment_errest_aerosol_opt_err_extt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return bias of error estimation of total extinction
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return bias of error estimation of total extinction
 */
float grasp_output_segment_errest_aerosol_opt_bias_extt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return total error estimation of total extinction
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return error estimation of total extinction
 */
float grasp_output_segment_errest_aerosol_opt_tstd_extt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return error estimation of single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return error estimation of single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_err_ssa (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return bias of error estimation of single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return bias of error estimation of single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_bias_ssa (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return total error estimation of single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return error estimation of single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_tstd_ssa (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return error estimation of total single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return error estimation of total single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_err_ssat (const output_segment_general *output, int ipix, int iwl);

/**
 * Return bias of error estimation of total single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return bias of error estimation of total single scattering albedo 
 */
float grasp_output_segment_errest_aerosol_opt_bias_ssat (const output_segment_general *output, int ipix, int iwl);

/**
 * Return total error estimation of total single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return error estimation of total single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_tstd_ssat (const output_segment_general *output, int ipix, int iwl);

/**
 * Return error estimation of single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return error estimation of single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_err_aext (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return bias of error estimation of single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return bias of error estimation of single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_bias_aext (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return total error estimation of single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return error estimation of single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_tstd_aext (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return error estimation of total single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return error estimation of total single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_err_aextt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return bias of error estimation of total single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return bias of error estimation of total single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_bias_aextt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return total error estimation of total single scattering albedo
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return error estimation of total single scattering albedo
 */
float grasp_output_segment_errest_aerosol_opt_tstd_aextt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return error estimation of lidar ration
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return error estimation of lidar ration
 */
float grasp_output_segment_errest_aerosol_lidar_err_lr (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return bias of error estimation of lidar ratio
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return bias of error estimation of lidar ratio
 */
float grasp_output_segment_errest_aerosol_lidar_bias_lr (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return total error estimation of lidar ration
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @return error estimation of lidar ration
 */
float grasp_output_segment_errest_aerosol_lidar_tstd_lr (const output_segment_general *output, int ipix, int iwl, int isd);

/**
 * Return error estimation of total lidar ratio
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return error estimation of total lidar ratio
 */
float grasp_output_segment_errest_aerosol_lidar_err_lrt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return bias of error estimation of total lidar ratio
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return bias of error estimation of total lidar ratio
 */
float grasp_output_segment_errest_aerosol_lidar_bias_lrt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return total error estimation of total lidar ratio
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iwl Index of the wavelength
 * @return error estimation of total lidar ratio
 */
float grasp_output_segment_errest_aerosol_lidar_tstd_lrt (const output_segment_general *output, int ipix, int iwl);

/**
 * Return error estimation of total size distribution (when it is not part of the retrieved parameters)
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param ibin Index of grid radius values
 * @return error estimation of total lidar ratio
 */
float grasp_output_segment_errest_aerosol_mic_err_sd (const output_segment_general *output, int ipix, int ibin);

/**
 * Return bias of error estimation of total size distribution (when it is not part of the retrieved parameters)
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param ibin Index of grid radius values
 * @return bias of error estimation of total lidar ratio
 */
float grasp_output_segment_errest_aerosol_mic_bias_sd (const output_segment_general *output, int ipix, int ibin);

/**
 * Return total error estimation of total size distribution (when it is not part of the retrieved parameters)
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param ibin Index of grid radius values
 * @return error estimation of total lidar ratio
 */
float grasp_output_segment_errest_aerosol_mic_tstd_sd (const output_segment_general *output, int ipix, int ibin);

///////////////

/**
 * Return number of heights of forcing fluxes
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return number of heights of forcing fluxes
 */
int grasp_output_segment_forcing_bbflux_nhlv (const output_segment_general *output, int ipix);

/**
 * Return broad band up-ward flux without aerosol at each height
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iknt
 * @return broad band up-ward flux without aerosol at each height
 */
float grasp_output_segment_forcing_bbflux_bbufx0 (const output_segment_general *output, int ipix, int iknt);

/**
 * Return broad band down-ward flux without aerosol at each height
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iknt
 * @return broad band down-ward flux without aerosol at each height
 */
float grasp_output_segment_forcing_bbflux_bbdfx0 (const output_segment_general *output, int ipix, int iknt);

/**
 * Return broad band up-ward flux with aerosol at each height
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iknt
 * @return broad band up-ward flux with aerosol at each height
 */
float grasp_output_segment_forcing_bbflux_bbufxa (const output_segment_general *output, int ipix, int iknt);

/**
 * Return broad band down-ward flux with aerosol at each height
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iknt
 * @return broad band down-ward flux with aerosol at each height
 */
float grasp_output_segment_forcing_bbflux_bbdfxa (const output_segment_general *output, int ipix, int iknt);

/**
 * Return heights of forcing fluxes
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iknt
 * @return heights of forcing fluxes
 */
float grasp_output_segment_forcing_bbflux_hlv (const output_segment_general *output, int ipix, int iknt);

/**
 * Return number of heights of forcing calculations
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @return number of heights of forcing calculations
 */
int grasp_output_segment_forcing_forcing_nhlv (const output_segment_general *output, int ipix);

/**
 * Return net forcing
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iknt
 * @return net forcing
 */
float grasp_output_segment_forcing_forcing_netforc (const output_segment_general *output, int ipix, int iknt);

/**
 * Return forcing efficiency
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iknt
 * @return forcing efficiency
 */
float grasp_output_segment_forcing_forcing_forceff (const output_segment_general *output, int ipix, int iknt);

/**
 * Return heights of forcing calculations
 * @param output Output of retrieval from a segment
 * @param ipix Number of pixel inside the segment
 * @param iknt
 * @return heights of forcing calculations
 */
float grasp_output_segment_forcing_forcing_hlv (const output_segment_general *output, int ipix, int iknt);

      
      
#ifdef	__cplusplus
}
#endif

#endif	/* GRASP_OUTPUT_SEGMENT_RESULT_H */

