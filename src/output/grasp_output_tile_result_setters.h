/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/*
 * File:   grasp_output_tile_result_setters.h
 * Author: lbindreiter
 *
 * Created on 30.08.2018
 */

#ifndef GRASP_GRASP_OUTPUT_TILE_RESULT_SETTERS_H
#define GRASP_GRASP_OUTPUT_TILE_RESULT_SETTERS_H

#ifdef  __cplusplus
extern "C" {
#endif

#include <inttypes.h>
//#include "mod_par_inv.inc"
//#include "mod_par_OS.inc"
#include "mod_par_DLS.inc"
#include "mod_par_DLS_bin.inc"
#include "../settings/grasp_products.h"
#include "../input/grasp_input_segment.h"
#include "../global/grasp_parameters.h"

#include "grasp_output_tile_result.h"

/**
 * Set if output retrieval residuals is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_retrieval_res (grasp_results_t *output, bool value);

/**
 * Set if output retrieval parameters is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_retrieval_par (grasp_results_t *output, bool value);

/**
 * Set if output retrieval fitting is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_retrieval_fit (grasp_results_t *output, bool value);

/**
 * Set if output for aerosol optical products is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_opt (grasp_results_t *output, bool value);

/**
 * Set if output for aerosol refractive index is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_rind (grasp_results_t *output, bool value);

/**
 * Set if output of aerosol chemistry is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_chem (grasp_results_t *output, bool value);

/**
 * Set if output of aerosol phase matrix is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_phmx (grasp_results_t *output, bool value);

/**
 * Set if output for aerosol lidar products is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_lidar (grasp_results_t *output, bool value);

/**
 * Set if output products for simulated two modes is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_sd2m_mph (grasp_results_t *output, bool value);

/**
 * Set if output extinction for aerosol two modes simulated is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_sd2m_ext (grasp_results_t *output, bool value);

/**
 * Set if output aerosol particular matter is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_pm (grasp_results_t *output, bool value);

/**
 * Set if output for aerosol types is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_aerosol_types (grasp_results_t *output, bool value);

/**
 * Set if output for cloud optical properties is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_opt (grasp_results_t *output, bool value);

/**
 * Set if output of cloud refractive index is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_rind (grasp_results_t *output, bool value);

/**
 * Set if output cloud chemistry is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_chem (grasp_results_t *output, bool value);

/**
 * Set if output cloud phase matrix is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_phmx (grasp_results_t *output, bool value);

/**
 * Set if output cloud lidar is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_lidar (grasp_results_t *output, bool value);

/**
 * Set if output clouds in two simulated modes is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_sd2m_mph (grasp_results_t *output, bool value);

/**
 * Set if output extinction for clouds in two simulated modes is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_sd2m_ext (grasp_results_t *output, bool value);

/**
 * Set if output for cloud particular matter is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_pm (grasp_results_t *output, bool value);

/**
 * Set if cloud type is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_clouds_types (grasp_results_t *output, bool value);

/**
 * Set if surface products is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_surface_surf (grasp_results_t *output, bool value);

/**
 * Set if output error estimation for parameters is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_errest_par (grasp_results_t *output, bool value);

/**
 * Set if output error estimation for aerosol optical products is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_errest_aerosol_opt (grasp_results_t *output, bool value);

/**
 * Set if output error estimation for aerosol lidar signal is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_errest_aerosol_lidar (grasp_results_t *output, bool value);

// MEH:
/**
 * Set if output error estimation for size distribution, when it is not part of the retrieved parameters, is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_errest_aerosol_mic (grasp_results_t *output, bool value);


/**
 * Set if output for error estimation of clouds optical properties is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_errest_clouds_opt (grasp_results_t *output, bool value);

/**
 * Set if output for cloud lidar signal is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_errest_clouds_lidar (grasp_results_t *output, bool value);

/**
 * Set if output for forcing flux is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_forcing_bbflux (grasp_results_t *output, bool value);

/**
 * Set if output for forcing is available
 * @param output Output of retrieval from a tile
 * @param value if it is available
 */
void set_grasp_output_tile_products_forcing_forcing (grasp_results_t *output, bool value);

/**
 * Set total number of pixels availables in output
 * @param output Output of retrieval from a tile
 * @param value total number of pixels availables in output
 */
void set_grasp_output_tile_information_tile_npixels(grasp_results_t *output, int value);

/**
 * Set total number of retrieved gases
 * @param output Output of retrieval from a tile
 * @param value total number of pixels availables in output
 */
void set_grasp_output_tile_information_tile_ngases(grasp_results_t *output, int value);

/**
 * Set total number of times in the tile
 * @param output Output of retrieval from a tile
 * @param value total number of times in the tile
 */
void set_grasp_output_tile_information_tile_npixels_t(grasp_results_t *output, int value);

/**
 * Set width of the tile
 * @param output Output of retrieval from a tile
 * @param value width of the tile
 */
void set_grasp_output_tile_information_tile_npixels_x(grasp_results_t *output, int value);

/**
 * Set height of the tile
 * @param output Output of retrieval from a tile
 * @param value height of the tile
 */
void set_grasp_output_tile_information_tile_npixels_y(grasp_results_t *output, int value);

/**
 * Set number of total retrieved parameter for each pixel
 * @param output Output of retrieval from a tile
 * @param value umber of total retrieved parameter for each pixel
 */
void set_grasp_output_tile_information_npars(grasp_results_t *output, int value);

/**
 * Set
 * @param output Output of retrieval from a tile
 * @param value
 */
void set_grasp_output_tile_information_nrr(grasp_results_t *output, int value);

/**
 * Set
 * @param output Output of retrieval from a tile
 * @param value
 */
void set_grasp_output_tile_information_nrc(grasp_results_t *output, int value);

/**
 * Set number of valid angles in output phase matrix
 * @param output Output of retrieval from a tile
 * @param value number of valid angles in output phase matrix
 */
void set_grasp_output_tile_information_nmpar(grasp_results_t *output, int value);

/**
 * Set
 * @param output Output of retrieval from a tile
 * @param value
 */
void set_grasp_output_tile_information_nsd(grasp_results_t *output, int value);

/**
 * Set
 * @param output Output of retrieval from a tile
 * @param value
 */
void set_grasp_output_tile_information_retrieval_par_radius(grasp_results_t *output, int irr, float value);

/**
 * Set
 * @param output Output of retrieval from a tile
 * @param irr Index of rr of SDL
 * @param irc Inxed of rc of SDL
 * @param value
 */
void set_grasp_output_tile_information_retrieval_par_SDL(grasp_results_t *output, int irr, int irc, float value);

/**
 *
 * @param output Output of retrieval from a tile
 * @param value
 */
void set_grasp_output_tile_information_npm_diam(grasp_results_t *output, int value);

/**
 * Set a specific angle (indexed by iangle) of output phase function
 * @param iangle Index of phase matrix angle
 * @param output Output of retrieval from a tile
 * @param value a specific angle (indexed by iangle) of output phase function
 */
void set_grasp_output_tile_information_phmx_angle(grasp_results_t *output, int iangle, int value);

/**
 * Set
 * @param output Output of retrieval from a tile
 * @param value
 */
void set_grasp_output_tile_information_nnoises(grasp_results_t *output, int value);

/**
 * Set the wavelenght of specific index
 * @param output Output of retrieval from a tile
 * @param iwl Index of the wavelenght
 * @param value Value of the wavelenght in nanometers
 */
void set_grasp_output_tile_information_wl(grasp_results_t *output, int iwl, float value);

//////////


/**
 * Set position of the segment, dimension T
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value position of the segment, dimension T
 */
void set_grasp_output_tile_pixel_information_segment_time(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set position of the segment, dimension X
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value position of the segment, dimension X
 */
void set_grasp_output_tile_pixel_information_segment_col(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set position of the segment, dimension Y
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value position of the segment, dimension Y
 */
void set_grasp_output_tile_pixel_information_segment_row(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set Internal T index of pixel inside the segment starting in 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Internal T index of pixel inside the segment starting in 1
 */
void set_grasp_output_tile_pixel_information_it(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set Internal X index of pixel inside the segment starting in 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Internal X index of pixel inside the segment starting in 1
 */
void set_grasp_output_tile_pixel_information_ix(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set Internal Y index of pixel inside the segment starting in 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Internal Y index of pixel inside the segment starting in 1
 */
void set_grasp_output_tile_pixel_information_iy(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set Index of X index in output grid.
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Index of X index in output grid.
 */
void set_grasp_output_tile_pixel_information_out_x(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set Index of Y index in output grid.
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Index of Y index in output grid.
 */
void set_grasp_output_tile_pixel_information_out_y(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set Index of T index in output grid.
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Index of T index in output grid.
 */
void set_grasp_output_tile_pixel_information_out_t(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set latitude of the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value latitude of the pixel
 */
void set_grasp_output_tile_pixel_information_latitude(grasp_results_t *output,int it, int ix, int iy, float value);

/**
 * Set longitude of the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value longitude of the pixel
 */
void set_grasp_output_tile_pixel_information_longitude(grasp_results_t *output,int it, int ix, int iy, float value);

/**
 * Set index in input grid for cols
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value index in input grid for cols
 */
void set_grasp_output_tile_pixel_information_grid_col(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set index in input grid for rows
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value index in input grid for rows
 */
void set_grasp_output_tile_pixel_information_grid_row(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set index in input grid for time
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value index in input grid for time
 */
void set_grasp_output_tile_pixel_information_time(grasp_results_t *output,int it, int ix, int iy, int64_t value);

/**
 * Set process real time of the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value seconds taken to process the pixel
 */
void set_grasp_output_tile_pixel_information_real_time(grasp_results_t *output,int it, int ix, int iy, float value);

/**
 * Set process user time of the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value seconds taken to process the pixel
 */
void set_grasp_output_tile_pixel_information_user_time(grasp_results_t *output,int it, int ix, int iy, float value);

/**
 * Set number of wavelengths for the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value number of wavelengths for the pixel
 */
void set_grasp_output_tile_pixel_information_nwl(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set cloud flag of the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value cloud flag of the pixel
 */
void set_grasp_output_tile_pixel_information_cloud_flag(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set land percent information of the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value land percent information of the pixel
 */
void set_grasp_output_tile_pixel_information_land_percent(grasp_results_t *output,int it, int ix, int iy, float value);

/**
 * Set file index (origin) of the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value file index (origin) of the pixel
 */
void set_grasp_output_tile_pixel_information_file_index(grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set meters above sea-level of the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value meters above sea-leve of the pixel
 */
void set_grasp_output_tile_pixel_information_masl(grasp_results_t *output,int it, int ix, int iy, float value);

/**
 * Set Solar zenit angle of current wavelength in degrees
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Solar zenit angle of current wavelength in degrees
 */
void set_grasp_output_tile_pixel_information_sza(grasp_results_t *output,int it, int ix, int iy, int iwl, float value);

/**
 * Set number of vertical points in the pixel
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Number of vertical points in vertical profile
 */
void set_grasp_output_tile_pixel_information_nhvp(const grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * If the pixel has lidar signal this array contains the vertical altitudes.
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param ihvp Vertical profile index
 * @param value Value in meter of vertical range
 */
void set_grasp_output_tile_pixel_information_hvp(const grasp_results_t *output,int it, int ix, int iy, int ihvp, float value);

/**
 * Set NDIM struct (that can be used to know the size and shape of parameters array)
 * @param output Output of retrieval from a tile
 * @param value current NDIM struct
 */
void set_grasp_output_tile_information_ndim (grasp_results_t *output, par_number_NDIM value);

/**
 * Set number of grid radii points
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value radii points for specific mode (0=total)
 */
void set_grasp_output_tile_information_ngrid(grasp_results_t *output, int isd, int value);

/**
 * Set values of grid radius points (X axis of SD plot)
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd
 * @param ipar
 * @param value values of grid radius points (X axis of SD plot)
 */
void set_grasp_output_tile_information_radius(grasp_results_t *output, int isd, int ipar, float value);


/**
 * Set values of grid values of the bings in case of lognormal bbins sd (X axis of SD plot)
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param irr
 * @param irc
 * @param value values of grid radius points (X axis of SD plot)
 */
void set_grasp_output_tile_information_retrieval_sd_lb(grasp_results_t *output, int irc, int irr, float value);

/**
 * Set the number of valid pixels in this pixel's segment
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param nrCol number of columns of segments in tile
 * @param nrRow number if rows of segments in tile
 * @param value the number of valid pixels in this pixel's segment
 */
void set_grasp_output_tile_pixel_segment_npixels(grasp_results_t *output, int it, int ix, int iy, int nrCol, int nrRow, int value);

//////////////

/**
 * Set number of iterations in multipixel scenario
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value number of iterations in multipixel scenario
 */
void set_grasp_output_tile_retrieval_res_niter (grasp_results_t *output,int it, int ix, int iy, int value);

/**
 * Set total residual for multi-pixel retrieval
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value total residual for multi-pixel retrieval
 */
void set_grasp_output_tile_retrieval_res_rest (grasp_results_t *output,int it, int ix, int iy, float value);

/**
 * Set detailed absolute measurement residuals for tile
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param inoise Number of noise
 * @param value detailed absolute measurement residuals for tile
 */
void set_grasp_output_tile_retrieval_res_resa (grasp_results_t *output,int it, int ix, int iy, int inoise, float value);

/**
 * Set detailed relative measurement residuals for tile
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param inoise Number of noise
 * @param value detailed relative and relative measurement residuals for tile
 */
void set_grasp_output_tile_retrieval_res_resr (grasp_results_t *output,int it, int ix, int iy, int inoise, float value);


/**
 * Set retrieved aerosol and surface reflectance parameters
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param ipar
 * @param value retrieved aerosol and surface reflectance parameters
 */
void set_grasp_output_tile_retrieval_par_parameters (grasp_results_t *output, int it, int ix, int iy, int ipar, float value);

/**
 * Set retrieved size distribution simplified for easy interpretation
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd 0 if it is total size distriubtion or the number of the mode
 * @param bin value of the bin
 * @param value retrieved value of the bin of SD
 */
void set_grasp_output_tile_retrieval_par_sd (grasp_results_t *output, int it, int ix, int iy, int isd, int bin, float value);

/**
 * Set original pixel data
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Original pixel data. It is the pixel in sdata input information
 */
void set_grasp_output_tile_retrieval_fit_pixel_original (grasp_results_t *output, int it, int ix, int iy, pixel_t*);

/**
 * Set fit pixel data
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value Fit pixel. It is like sdata pixel but filled with fitting information
 */
void set_grasp_output_tile_retrieval_fit_pixel_fit (grasp_results_t *output, int it, int ix, int iy, pixel_t*);

/**
 * Set segment fitting data
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value It is like sdata but filled with fitting information
 */
void set_grasp_output_tile_retrieval_fit_segment_fit (grasp_results_t *output, int isegment, sensor_data_t*);

/**
 * Set segment input data
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value It is the sdata input information
 */
void set_grasp_output_tile_retrieval_fit_segment_original (grasp_results_t *output, int isegment, sensor_data_t*);


/**
 * Set angstrom exponent for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value angstrom exponent for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_opt_aexp (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set total extinction for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total extinction for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_opt_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set total single scattering albedo for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total single scattering albedo for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_opt_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set total absorption extinction for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total absorption extinction for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_opt_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set extinction for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value extinction for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_opt_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set single scattering albedo for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value single scattering albedo for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_opt_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set absorption extinction for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value absorption extinction for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_opt_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set real part refractive index
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value real part refractive index
 */
void set_grasp_output_tile_aerosol_rind_mreal (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set real part refractive index
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value real part refractive index
 */
void set_grasp_output_tile_aerosol_rind_mimag (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

//////

/**
 * Set p11 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @param value p11 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_ph11 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p12 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @param value p12 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_ph12 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p22 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @param value p22 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_ph22 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p33 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @param value p33 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_ph33 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p34 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @param value p34 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_ph34 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p44 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of aerosol component
 * @param value p44 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_ph44 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set total p11 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p11 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_pht11 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p12 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p12 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_pht12 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p22 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p22 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_pht22 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p33 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p33 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_pht33 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p34 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p34 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_pht34 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p44 for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p44 for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_phmx_pht44 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/////

/**
 * Set lidar ratio for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value lidar ratio for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_lidar_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set lidar lidar depolarization ratio (parallel component) N.B. reserved for future developments
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value lidar lidar depolarization ratio (parallel component) N.B. reserved for future developments
 */
void set_grasp_output_tile_aerosol_lidar_ldpar (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set lidar lidar depolarization ratio (perpendicular component) N.B. reserved for future developments
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value lidar lidar depolarization ratio (perpendicular component) N.B. reserved for future developments
 */
void set_grasp_output_tile_aerosol_lidar_ldper (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set total lidar ratio for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total lidar ratio for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_lidar_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set total lidar depolarization for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total lidar depolarization for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_lidar_ldprt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set concentration for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i mode. 0 for fine and 1 for coarse
 * @param value concentration for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_cv (grasp_results_t *output, int it, int ix, int iy, int i, float value);

/**
 * Set standard deviation for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i mode. 0 for fine and 1 for coarse
 * @param value standard deviation for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_std (grasp_results_t *output, int it, int ix, int iy, int i, float value);

/**
 * Set XXX for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i mode. 0 for fine and 1 for coarse
 * @param value XXX for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_rm (grasp_results_t *output, int it, int ix, int iy, int i, float value);

/**
 * Set refractive index for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i
 * @param value refractive index for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_reff (grasp_results_t *output, int it, int ix, int iy, int i, float value);


/**
 * Set extinction for two simulated modes for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param i mode. 0 for fine and 1 for coarse
 * @param value extinction for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_opt_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int i, float value);

/**
 * Set concentration for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_mph_cv with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value concentration for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_cv_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set standard deviation for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_mph_std with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value standard deviation for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_std_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set XXX for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_mph_rm with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value XXX for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_rm_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set refractive index for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_mph_reff with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value refractive index for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_reff_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value);


/**
 * Set extinction for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_opt_ext with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value extinction for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_opt_ext_fine_mode (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);


/**
 * Set concentration for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_mph_cv with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value concentration for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_cv_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set standard deviation for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_mph_std with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value standard deviation for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_std_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set XXX for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_mph_rm with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value XXX for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_rm_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set refractive index for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_mph_reff with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value refractive index for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_mph_reff_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value);


/**
 * Set extinction for two simulated modes for aerosol optical properties. Alias void set_grasp_output_tile_aerosol_sd2m_opt_ext with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value extinction for two simulated modes for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_sd2m_opt_ext_coarse_mode (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);


/**
 * Set relative humidity for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of aerosol component
 * @param value relative humidity for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_chem_rh (grasp_results_t *output, int it, int ix, int iy, int isd, float value);

/**
 * Set water fraction for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of aerosol component
 * @param value water fraction for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_chem_fwtr (grasp_results_t *output, int it, int ix, int iy, int isd, float value);

/**
 * Set soluble fraction for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of aerosol component
 * @param value soluble fraction for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_chem_fslbl (grasp_results_t *output, int it, int ix, int iy, int isd, float value);

/**
 * Set insoluble fraction for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of aerosol component
 * @param ichem Index of chemical component
 * @param value  insoluble fraction for aerosol optical properties
 */
void set_grasp_output_tile_aerosol_chem_vfract (grasp_results_t *output, int it, int ix, int iy, int isd, int ichem, float value);



/**
 * Set particular matter for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i
 * @param value particular matter for aerosol optical properties
 */

void set_grasp_output_tile_aerosol_pm_pm (grasp_results_t *output, int it, int ix, int iy, int i, float value);

/**
 * Set aerosol type for aerosol optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value aerosol type: 0 – Complex mixture; 1 – Background Aerosol; 2 – Water/Maritime; 3 — Urban Polluted; 4 – Mixed aerosol;  5 – Urban Clean;  6 – Smoke Smoldering;  7 – Smoke flaming;  8 – Mineral dust
 */
void set_grasp_output_tile_aerosol_types_index (grasp_results_t *output, int it, int ix, int iy, int value);

/**
 * Set gas absorption
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param igas number of gas, 0 for total
 * @param 
 */
void set_grasp_output_tile_gases_absorption(grasp_results_t *output, int it, int ix, int iy, int iwl, int igas, int value);

/////////

//////


/**
 * Set angstrom exponent for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value angstrom exponent for clouds optical properties
 */
void set_grasp_output_tile_clouds_opt_aexp (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set total extinction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total extinction for clouds optical properties
 */
void set_grasp_output_tile_clouds_opt_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set total single scattering albedo for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total single scattering albedo for clouds optical properties
 */
void set_grasp_output_tile_clouds_opt_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set total absorption extinction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total absorption extinction for clouds optical properties
 */
void set_grasp_output_tile_clouds_opt_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set extinction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value extinction for clouds optical properties
 */
void set_grasp_output_tile_clouds_opt_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set single scattering albedo for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value single scattering albedo for clouds optical properties
 */
void set_grasp_output_tile_clouds_opt_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set absorption extinction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value absorption extinction for clouds optical properties
 */
void set_grasp_output_tile_clouds_opt_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set real part refractive index
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value real part refractive index
 */
void set_grasp_output_tile_clouds_rind_mreal (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set real part refractive index
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value real part refractive index
 */
void set_grasp_output_tile_clouds_rind_mimag (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

//////

/**
 * Set p11 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of clouds component
 * @param value p11 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_ph11 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p12 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of clouds component
 * @param value p12 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_ph12 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p22 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of clouds component
 * @param value p22 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_ph22 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p33 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of clouds component
 * @param value p33 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_ph33 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p34 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of clouds component
 * @param value p34 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_ph34 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set p44 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param isd Index of clouds component
 * @param value p44 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_ph44 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value);

/**
 * Set total p11 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p11 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_pht11 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p12 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p12 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_pht12 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p22 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p22 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_pht22 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p33 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p33 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_pht33 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p34 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p34 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_pht34 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/**
 * Set total p44 for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param impar
 * @param value total p44 for clouds optical properties
 */
void set_grasp_output_tile_clouds_phmx_pht44 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value);

/////

/**
 * Set lidar ratio for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value lidar ratio for clouds optical properties
 */
void set_grasp_output_tile_clouds_lidar_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set lidar depolarization profile for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value lidar depolarization profile for clouds optical properties
 */
void set_grasp_output_tile_clouds_lidar_ldpr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set total lidar ratio for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total lidar ratio for clouds optical properties
 */
void set_grasp_output_tile_clouds_lidar_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set total lidar depolarization for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value total lidar depolarization for clouds optical properties
 */
void set_grasp_output_tile_clouds_lidar_ldprt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);


/**
 * Set concentration for two simulated modes for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i mode. 0 for fine and 1 for coarse
 * @param value concentration for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_cv (grasp_results_t *output, int it, int ix, int iy, int i, float value);

/**
 * Set standard deviation for two simulated modes for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i mode. 0 for fine and 1 for coarse
 * @param value standard deviation for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_std (grasp_results_t *output, int it, int ix, int iy, int i, float value);

/**
 * Set XXX for two simulated modes for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i mode. 0 for fine and 1 for coarse
 * @param value XXX for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_rm (grasp_results_t *output, int it, int ix, int iy, int i, float value);

/**
 * Set refractive index for two simulated modes for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i mode. 0 for fine and 1 for coarse
 * @param value refractive index for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_reff (grasp_results_t *output, int it, int ix, int iy, int i, float value);


/**
 * Set extinction for two simulated modes for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param i mode. 0 for fine and 1 for coarse
 * @param value extinction for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_opt_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int i, float value);


/**
 * Set concentration for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_mph_cv with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value concentration for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_cv_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set standard deviation for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_mph_std with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value standard deviation for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_std_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set XXX for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_mph_rm with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value XXX for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_rm_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set refractive index for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_mph_reff with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value refractive index for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_reff_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value);


/**
 * Set extinction for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_opt_ext with i = 0
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value extinction for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_opt_ext_fine_mode (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);


/**
 * Set concentration for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_mph_cv with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value concentration for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_cv_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set standard deviation for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_mph_std with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value standard deviation for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_std_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set XXX for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_mph_rm with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value XXX for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_rm_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set refractive index for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_mph_reff with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value refractive index for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_mph_reff_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value);


/**
 * Set extinction for two simulated modes for clouds optical properties. Alias void set_grasp_output_tile_clouds_sd2m_opt_ext with i = 1
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value extinction for two simulated modes for clouds optical properties
 */
void set_grasp_output_tile_clouds_sd2m_opt_ext_coarse_mode (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);


/**
 * Set relative humidity for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of clouds component
 * @param value relative humidity for clouds optical properties
 */
void set_grasp_output_tile_clouds_chem_rh (grasp_results_t *output, int it, int ix, int iy, int isd, float value);

/**
 * Set water fraction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of clouds component
 * @param value water fraction for clouds optical properties
 */
void set_grasp_output_tile_clouds_chem_fwtr (grasp_results_t *output, int it, int ix, int iy, int isd, float value);

/**
 * Set soluble fraction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of clouds component
 * @param value soluble fraction for clouds optical properties
 */
void set_grasp_output_tile_clouds_chem_fslbl (grasp_results_t *output, int it, int ix, int iy, int isd, float value);

/**
 * Set insoluble fraction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of clouds component
 * @param value  insoluble fraction for clouds optical properties
 */
void set_grasp_output_tile_clouds_chem_finslbl (grasp_results_t *output, int it, int ix, int iy, int isd, float value);

/**
 * Set soot fraction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of clouds component
 * @param value soot fraction for clouds optical properties
 */
void set_grasp_output_tile_clouds_chem_fsoot (grasp_results_t *output, int it, int ix, int iy, int isd, float value);

/**
 * Set iron fraction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of clouds component
 * @param value iron fraction for clouds optical properties
 */
void set_grasp_output_tile_clouds_chem_firon (grasp_results_t *output, int it, int ix, int iy, int isd, float value);


/**
 * Set brc fraction for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd Index of clouds component
 * @param value iron fraction for clouds optical properties
 */
void set_grasp_output_tile_clouds_chem_fbrc (grasp_results_t *output, int it, int ix, int iy, int isd, float value);



/**
 * Set particular matter for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param i
 * @param value particular matter for clouds optical properties
 */
void set_grasp_output_tile_clouds_pm_pm (grasp_results_t *output, int it, int ix, int iy, int i, float value);

/**
 * Set clouds type for clouds optical properties
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value clouds type: 0 – Complex mixture; 1 – Background Aerosol; 2 – Water/Maritime; 3 — Urban Polluted; 4 – Mixed clouds;  5 – Urban Clean;  6 – Smoke Smoldering;  7 – Smoke flaming;  8 – Mineral dust
 */
void set_grasp_output_tile_clouds_types_index (grasp_results_t *output, int it, int ix, int iy, int value);


////////

/**
 * Set surface ndvi
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value surface ndvi
 */
void set_grasp_output_tile_surface_ndvi (grasp_results_t *output, int it, int ix, int iy, float value);

/**
 * Set surface dhr
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value surface albedo
 */
void set_grasp_output_tile_surface_dhr (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set surface bhr_iso
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value surface albedo
 */
void set_grasp_output_tile_surface_bhr_iso (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/////////////////

/**
 * Set error estimation for parameter
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param ipar
 * @param value error estimation for parameter
 */
void set_grasp_output_tile_errest_par_errp (grasp_results_t *output, int it, int ix, int iy, int ipar, float value);

/**
 * Set bias of error estimation of parameters
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param ipar
 * @param value bias of error estimation of parameters
 */
void set_grasp_output_tile_errest_par_biasp (grasp_results_t *output, int it, int ix, int iy, int ipar, float value);

/**
 * Set total standard deviation
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param ipar
 * @param value bias of error estimation of parameters
 */
void set_grasp_output_tile_errest_par_tstdp (grasp_results_t *output, int it, int ix, int iy, int ipar, float value);

/*
 * Set error of retrieved size distribution simplified for easy interpretation
 * +/- err(:,1:nsd) = exp(ln(GOUT%retrieval%par%pixel(ipix)%sd(:,1:nsd)) +/- GOUT%errest%par%pixel(ipix)%sd_err(:,1:nsd))
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param isd 0 if it is total size distriubtion or the number of the mode
 * @param bin value of the bin
 * @param value retrieved value of the bin of SD
 */
void set_grasp_output_tile_errest_par_sd_err (grasp_results_t *output, int it, int ix, int iy, int isd, int ibin, float value);

/**
 * Set error estimation of extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value error estimation of extinction
 */
void set_grasp_output_tile_errest_aerosol_opt_err_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set bias of error estimation of extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value bias of error estimation of extinction
 */
void set_grasp_output_tile_errest_aerosol_opt_bias_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);


/**
 * Set total error estimation of extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value error estimation of extinction
 */
void set_grasp_output_tile_errest_aerosol_opt_tstd_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set error estimation of total extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total extinction
 */
void set_grasp_output_tile_errest_aerosol_opt_err_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set bias of error estimation of total extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value bias of error estimation of total extinction
 */
void set_grasp_output_tile_errest_aerosol_opt_bias_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);


/**
 * Set total error estimation of total extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total extinction
 */
void set_grasp_output_tile_errest_aerosol_opt_tstd_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set error estimation of single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value error estimation of single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_err_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set bias of error estimation of single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value bias of error estimation of single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_bias_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set total error estimation of single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value error estimation of single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_tstd_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set error estimation of total single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_err_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set bias of error estimation of total single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value bias of error estimation of total single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_bias_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set total error estimation of total single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_tstd_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set error estimation of single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value error estimation of single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_err_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set bias of error estimation of single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value bias of error estimation of single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_bias_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set total error estimation of single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value error estimation of single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_tstd_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set error estimation of total single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_err_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set bias of error estimation of total single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value bias of error estimation of total single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_bias_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set total error estimation of total single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total single scattering albedo
 */
void set_grasp_output_tile_errest_aerosol_opt_tstd_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set error estimation of lidar ration
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value error estimation of lidar ration
 */
void set_grasp_output_tile_errest_aerosol_lidar_err_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set bias of error estimation of lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value bias of error estimation of lidar ratio
 */
void set_grasp_output_tile_errest_aerosol_lidar_bias_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set total error estimation of lidar ration
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of aerosol component
 * @param value error estimation of lidar ration
 */
void set_grasp_output_tile_errest_aerosol_lidar_tstd_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set error estimation of total lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total lidar ratio
 */
void set_grasp_output_tile_errest_aerosol_lidar_err_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set bias of error estimation of total lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value bias of error estimation of total lidar ratio
 */
void set_grasp_output_tile_errest_aerosol_lidar_bias_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set error estimation of total lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total lidar ratio
 */
void set_grasp_output_tile_errest_aerosol_lidar_tstd_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set error estimation of total lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total lidar ratio
 */
void set_grasp_output_tile_errest_aerosol_mic_err_sd (grasp_results_t *output, int it, int ix, int iy, int ibin, float value);

/**
 * Set bias of error estimation of total lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value bias of error estimation of total lidar ratio
 */
void set_grasp_output_tile_errest_aerosol_mic_bias_sd (grasp_results_t *output, int it, int ix, int iy, int ibin, float value);

/**
 * Set error estimation of total lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total lidar ratio
 */
void set_grasp_output_tile_errest_aerosol_mic_tstd_sd (grasp_results_t *output, int it, int ix, int iy, int ibin, float value);

/**
 * Set error estimation of extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value error estimation of extinction
 */
void set_grasp_output_tile_errest_clouds_opt_err_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set bias of error estimation of extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value bias of error estimation of extinction
 */
void set_grasp_output_tile_errest_clouds_opt_bias_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set error estimation of total extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total extinction
 */
void set_grasp_output_tile_errest_clouds_opt_err_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set bias of error estimation of total extinction
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value bias of error estimation of total extinction
 */
void set_grasp_output_tile_errest_clouds_opt_bias_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set error estimation of single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value error estimation of single scattering albedo
 */
void set_grasp_output_tile_errest_clouds_opt_err_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set bias of error estimation of single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value bias of error estimation of single scattering albedo
 */
void set_grasp_output_tile_errest_clouds_opt_bias_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set error estimation of total single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total single scattering albedo
 */
void set_grasp_output_tile_errest_clouds_opt_err_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set bias of error estimation of total single scattering albedo
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value bias of error estimation of total single scattering albedo
 */
void set_grasp_output_tile_errest_clouds_opt_bias_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set error estimation of lidar ration
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value error estimation of lidar ration
 */
void set_grasp_output_tile_errest_clouds_lidar_err_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set bias of error estimation of lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param isd Index of clouds component
 * @param value bias of error estimation of lidar ratio
 */
void set_grasp_output_tile_errest_clouds_lidar_bias_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value);

/**
 * Set error estimation of total lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value error estimation of total lidar ratio
 */
void set_grasp_output_tile_errest_clouds_lidar_err_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

/**
 * Set bias of error estimation of total lidar ratio
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iwl Index of the wavelength
 * @param value bias of error estimation of total lidar ratio
 */
void set_grasp_output_tile_errest_clouds_lidar_bias_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value);

///////////////

/**
 * Set number of heights of forcing fluxes
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value number of heights of forcing fluxes
 */
void set_grasp_output_tile_forcing_bbflux_nhlv (grasp_results_t *output, int it, int ix, int iy, int value);

/**
 * Set broad band up-ward flux without aerosol at each height
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iknt
 * @param value broad band up-ward flux without aerosol at each height
 */
void set_grasp_output_tile_forcing_bbflux_bbufx0 (grasp_results_t *output, int it, int ix, int iy, int iknt, float value);

/**
 * Set broad band down-ward flux without aerosol at each height
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iknt
 * @param value broad band down-ward flux without aerosol at each height
 */
void set_grasp_output_tile_forcing_bbflux_bbdfx0 (grasp_results_t *output, int it, int ix, int iy, int iknt, float value);

/**
 * Set broad band up-ward flux with aerosol at each height
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iknt
 * @param value broad band up-ward flux with aerosol at each height
 */
void set_grasp_output_tile_forcing_bbflux_bbufxa (grasp_results_t *output, int it, int ix, int iy, int iknt, float value);

/**
 * Set broad band down-ward flux with aerosol at each height
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iknt
 * @param value broad band down-ward flux with aerosol at each height
 */
void set_grasp_output_tile_forcing_bbflux_bbdfxa (grasp_results_t *output, int it, int ix, int iy, int iknt, float value);

/**
 * Set heights of forcing fluxes
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iknt
 * @param value heights of forcing fluxes
 */
void set_grasp_output_tile_forcing_bbflux_hlv (grasp_results_t *output, int it, int ix, int iy, int iknt, float value);

/**
 * Set number of heights of forcing calculations
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param value number of heights of forcing calculations
 */
void set_grasp_output_tile_forcing_forcing_nhlv (grasp_results_t *output, int it, int ix, int iy, int value);

/**
 * Set net forcing
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iknt
 * @param value net forcing
 */
void set_grasp_output_tile_forcing_forcing_netforc (grasp_results_t *output, int it, int ix, int iy, int iknt, float value);

/**
 * Set forcing efficiency
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iknt
 * @param value forcing efficiency
 */
void set_grasp_output_tile_forcing_forcing_forceff (grasp_results_t *output, int it, int ix, int iy, int iknt, float value);

/**
 * Set heights of forcing calculations
 * @param output Output of retrieval from a tile
 * @param it Time index of the pixel in the tile
 * @param ix X index of the pixel in the tile
 * @param iy Y index of the pixel in the tile
 * @param iknt
 * @param value heights of forcing calculations
 */
void set_grasp_output_tile_forcing_forcing_hlv (grasp_results_t *output, int it, int ix, int iy, int iknt, float value);


#ifdef  __cplusplus
}
#endif

#endif //GRASP_GRASP_OUTPUT_TILE_RESULT_SETTERS_H
