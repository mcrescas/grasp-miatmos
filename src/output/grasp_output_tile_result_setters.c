/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "grasp_output_tile_result.h"
#include <grasp/utils.h>

#define ipixel_ index3D(it,ix,iy,output->information.tile_npixels_x,output->information.tile_npixels_y)
#define NSD_ output->information.nsd
#define NMPAR_ output->information.nmpar
#define NWL_ output->information.nwl

void set_grasp_output_tile_products_retrieval_res (grasp_results_t *output, bool value) {
  output->products.retrieval.res = value;
}

void set_grasp_output_tile_products_retrieval_par (grasp_results_t *output, bool value) {
  output->products.retrieval.par = value;
}

void set_grasp_output_tile_products_retrieval_fit (grasp_results_t *output, bool value) {
  output->products.retrieval.fit = value;
}

void set_grasp_output_tile_products_aerosol_opt (grasp_results_t *output, bool value) {
  output->products.aerosol.opt = value;
}

void set_grasp_output_tile_products_aerosol_rind (grasp_results_t *output, bool value) {
  output->products.aerosol.rind = value;
}

void set_grasp_output_tile_products_aerosol_chem (grasp_results_t *output, bool value) {
  output->products.aerosol.chem = value;
}

void set_grasp_output_tile_products_aerosol_phmx (grasp_results_t *output, bool value) {
  output->products.aerosol.phmx = value;
}

void set_grasp_output_tile_products_aerosol_lidar (grasp_results_t *output, bool value) {
  output->products.aerosol.lidar = value;
}

void set_grasp_output_tile_products_aerosol_sd2m_mph (grasp_results_t *output, bool value) {
  output->products.aerosol.sd2m_mph = value;
}

void set_grasp_output_tile_products_aerosol_sd2m_ext (grasp_results_t *output, bool value) {
  output->products.aerosol.sd2m_ext = value;
}

void set_grasp_output_tile_products_aerosol_pm (grasp_results_t *output, bool value) {
  output->products.aerosol.pm = value;
}

void set_grasp_output_tile_products_aerosol_types (grasp_results_t *output, bool value) {
  output->products.aerosol.types = value;
}

void set_grasp_output_tile_products_surface_surf (grasp_results_t *output, bool value) {
  output->products.surface.surf = value;
}

void set_grasp_output_tile_products_errest_par (grasp_results_t *output, bool value) {
  output->products.errest.par = value;
}

void set_grasp_output_tile_products_errest_aerosol_opt (grasp_results_t *output, bool value) {
  output->products.errest.aerosol.opt = value;
}

void set_grasp_output_tile_products_errest_aerosol_lidar (grasp_results_t *output, bool value) {
  output->products.errest.aerosol.lidar = value;
}

//MEH:
void set_grasp_output_tile_products_errest_aerosol_mic (grasp_results_t *output, bool value) {
  output->products.errest.aerosol.mic = value;
}

void set_grasp_output_tile_products_forcing_bbflux (grasp_results_t *output, bool value) {
  output->products.forcing.bbflux = value;
}

void set_grasp_output_tile_products_forcing_forcing (grasp_results_t *output, bool value) {
  output->products.forcing.forcing = value;
}

void set_grasp_output_tile_information_tile_npixels(grasp_results_t *output, int value){
  output->information.tile_npixels = value;
}

void set_grasp_output_tile_information_tile_ngases(grasp_results_t *output, int value){
  output->information.ngases = value;
}

void set_grasp_output_tile_information_tile_npixels_t(grasp_results_t *output, int value){
  output->information.tile_npixels_t = value;
}

void set_grasp_output_tile_information_tile_npixels_x(grasp_results_t *output, int value){
  output->information.tile_npixels_x = value;
}

void set_grasp_output_tile_information_tile_npixels_y(grasp_results_t *output, int value){
  output->information.tile_npixels_y = value;
}

void set_grasp_output_tile_information_npars(grasp_results_t *output, int value){
  output->information.npars = value;
}

void set_grasp_output_tile_information_nrc(grasp_results_t *output, int value){
  output->information.nrc = value;
}

void set_grasp_output_tile_information_nmpar(grasp_results_t *output, int value){
  output->information.nmpar = value;
}

void set_grasp_output_tile_information_nsd(grasp_results_t *output, int value){
  output->information.nsd = value;
}

void set_grasp_output_tile_information_npm_diam(grasp_results_t *output, int value){
  output->information.nPM_diam = value;
}

void set_grasp_output_tile_information_phmx_angle(grasp_results_t *output, int iangle, int value){
  output->information.phmx_angle[iangle] = value;
}

void set_grasp_output_tile_information_nnoises(grasp_results_t *output, int value){
  output->information.nnoises = value;
}

void set_grasp_output_tile_information_wl(grasp_results_t *output, int iwl, float value){
  output->information.wl[iwl] = value;
}

void set_grasp_output_tile_pixel_information_segment_time(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.segment_time = value;
}

void set_grasp_output_tile_pixel_information_segment_col(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.segment_col = value;
}

void set_grasp_output_tile_pixel_information_segment_row(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.segment_row = value;
}

void set_grasp_output_tile_pixel_information_it(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.it = value;
}

void set_grasp_output_tile_pixel_information_ix(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.ix = value;
}

void set_grasp_output_tile_pixel_information_iy(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.iy = value;
}

void set_grasp_output_tile_pixel_information_out_x(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.out_x = value;
}

void set_grasp_output_tile_pixel_information_out_y(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.out_y = value;
}

void set_grasp_output_tile_pixel_information_out_t(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.out_t = value;
}

void set_grasp_output_tile_pixel_information_latitude(grasp_results_t *output,int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->information.latitude = value;
}

void set_grasp_output_tile_pixel_information_longitude(grasp_results_t *output,int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->information.longitude = value;
}

void set_grasp_output_tile_pixel_information_grid_col(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.grid_col = value;
}

void set_grasp_output_tile_pixel_information_grid_row(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.grid_row = value;
}

void set_grasp_output_tile_pixel_information_time(grasp_results_t *output,int it, int ix, int iy, int64_t value){
  output->tile_result_map[ipixel_]->information.time = value;
}

void set_grasp_output_tile_pixel_information_real_time(grasp_results_t *output,int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->information.real_time = value;
}

void set_grasp_output_tile_pixel_information_user_time(grasp_results_t *output,int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->information.user_time = value;
}

// DEPRECATED:
// THIS FUNCTION EXIST JUST TO SUPPORT CHANGES IN INTERNAL STRUCTURES BUT IT SHOULD NOT BE USED!
void set_grasp_output_tile_pixel_information_nwl(grasp_results_t *output,int it, int ix, int iy, int value){
  output->information.nwl = value;
}

void set_grasp_output_tile_pixel_information_cloud_flag(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.cloud_flag = value;
}

void set_grasp_output_tile_pixel_information_land_percent(grasp_results_t *output,int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->information.land_percent = value;
}

void set_grasp_output_tile_pixel_information_file_index(grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->information.file_index = value;
}

void set_grasp_output_tile_pixel_information_masl(grasp_results_t *output,int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->information.masl = value;
}

void set_grasp_output_tile_pixel_information_sza(grasp_results_t *output,int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->information.sza[iwl] = value;
}

// DEPRECATED: used instead: int grasp_output_tile_information_nhvp(const grasp_results_t *output,int it, int ix, int iy); // no setter function.
void set_grasp_output_tile_pixel_information_nhvp(grasp_results_t *output,int it, int ix, int iy, int value) {
  output->information.nhvp = value;
}

void set_grasp_output_tile_pixel_information_hvp(const grasp_results_t *output,int it, int ix, int iy, int ihvp, float value){
  output->tile_result_map[ipixel_]->information.hvp[ihvp] = value;
}

void set_grasp_output_tile_information_ndim (grasp_results_t *output, par_number_NDIM value) {
  output->information.ndim = value;
}

void set_grasp_output_tile_information_ngrid(grasp_results_t *output, int isd, int value){
  output->information.ngrid[isd] = value;
}

void set_grasp_output_tile_information_radius(grasp_results_t *output, int isd, int igrid, float value){
  output->information.radius[isd][igrid] = value;
}

void set_grasp_output_tile_information_retrieval_sd_lb(grasp_results_t *output, int irc, int irr, float value){
  output->information.sd_lb[irc][irr] = value;
}

void set_grasp_output_tile_pixel_segment_npixels(grasp_results_t *output, int it, int ix, int iy, int nrCol, int nrRow, int value){
  int index = (output->tile_result_map[ipixel_]->information.segment_time * nrCol*nrRow +
               output->tile_result_map[ipixel_]->information.segment_col * nrRow +
               output->tile_result_map[ipixel_]->information.segment_row);
  output->segment_result[index].npixel = value;
}

void set_grasp_output_tile_retrieval_res_niter (grasp_results_t *output,int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->retrieval_res->niter = value;
}

void set_grasp_output_tile_retrieval_res_rest (grasp_results_t *output,int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->retrieval_res->rest = value;
}

void set_grasp_output_tile_retrieval_res_resa (grasp_results_t *output,int it, int ix, int iy, int inoise, float value){
  output->tile_result_map[ipixel_]->retrieval_res->resa[inoise] = value;
}

void set_grasp_output_tile_retrieval_res_resr (grasp_results_t *output,int it, int ix, int iy, int inoise, float value){
  output->tile_result_map[ipixel_]->retrieval_res->resr[inoise] = value;
}

void set_grasp_output_tile_retrieval_par_parameters (grasp_results_t *output, int it, int ix, int iy, int ipar, float value){
  output->tile_result_map[ipixel_]->retrieval_par->parameters[ipar] = value;
}

void set_grasp_output_tile_retrieval_par_sd (grasp_results_t *output, int it, int ix, int iy, int isd, int igrid, float value){
  output->tile_result_map[ipixel_]->retrieval_par->sd[index2D(isd,igrid,4*_KIDIM3)] = value;
}

void set_grasp_output_tile_retrieval_fit_pixel_original (grasp_results_t *output, int it, int ix, int iy, pixel_t* value){
  output->tile_result_map[ipixel_]->pixel_original = value;
}

void set_grasp_output_tile_retrieval_fit_pixel_fit (grasp_results_t *output, int it, int ix, int iy, pixel_t* value){
  output->tile_result_map[ipixel_]->pixel_fit = value;
}

void set_grasp_output_tile_retrieval_fit_segment_fit (grasp_results_t *output, int isegment, sensor_data_t* value){
  output->segment_fit[isegment] = value;
}

void set_grasp_output_tile_retrieval_fit_segment_original (grasp_results_t *output, int isegment, sensor_data_t* value){
  output->segment_original[isegment] = value;
}

void set_grasp_output_tile_aerosol_opt_aexp (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_opt->Aexp = value;
}

void set_grasp_output_tile_aerosol_opt_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->aerosol_opt->extt[iwl] = value;
}

void set_grasp_output_tile_aerosol_opt_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->aerosol_opt->ssat[iwl] = value;
}

void set_grasp_output_tile_aerosol_opt_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->aerosol_opt->aext[iwl] = value;
}

void set_grasp_output_tile_aerosol_opt_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_opt->ext[index2D(iwl,isd,NSD_)] = value;
}

void set_grasp_output_tile_aerosol_opt_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_opt->ssa[index2D(iwl,isd,NSD_)] = value;
}

void set_grasp_output_tile_aerosol_opt_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_opt->aext[index2D(iwl,isd,NSD_)] = value;
}

void set_grasp_output_tile_aerosol_rind_mreal (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_rind->mreal[index2D(iwl,isd,NSD_)] = value;
}

void set_grasp_output_tile_aerosol_rind_mimag (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_rind->mimag[index2D(iwl,isd,NSD_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_ph11 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->ph11[index3D(iwl,isd,impar,NSD_,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_ph12 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->ph12[index3D(iwl,isd,impar,NSD_,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_ph22 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->ph22[index3D(iwl,isd,impar,NSD_,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_ph33 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->ph33[index3D(iwl,isd,impar,NSD_,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_ph34 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->ph34[index3D(iwl,isd,impar,NSD_,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_ph44 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->ph44[index3D(iwl,isd,impar,NSD_,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_pht11 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->pht11[index2D(iwl,impar,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_pht12 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->pht12[index2D(iwl,impar,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_pht22 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->pht22[index2D(iwl,impar,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_pht33 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->pht33[index2D(iwl,impar,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_pht34 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->pht34[index2D(iwl,impar,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_phmx_pht44 (grasp_results_t *output, int it, int ix, int iy, int iwl, int impar, float value){
  output->tile_result_map[ipixel_]->aerosol_phmx->pht44[index2D(iwl,impar,NMPAR_)] = value;
}

void set_grasp_output_tile_aerosol_lidar_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_lidar->lr[index2D(iwl,isd,NSD_)] = value;
}

void set_grasp_output_tile_aerosol_lidar_ldpar (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_lidar->ldpar[index2D(iwl,isd,NSD_)] = value;
}

void set_grasp_output_tile_aerosol_lidar_ldper (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_lidar->ldper[index2D(iwl,isd,NSD_)] = value;
}

void set_grasp_output_tile_aerosol_lidar_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->aerosol_lidar->lrt[iwl] = value;
}

void set_grasp_output_tile_aerosol_lidar_ldprt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->aerosol_lidar->ldprt[iwl] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_cv (grasp_results_t *output, int it, int ix, int iy, int i, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->cv[i] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_std (grasp_results_t *output, int it, int ix, int iy, int i, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->std[i] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_rm (grasp_results_t *output, int it, int ix, int iy, int i, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->rm[i] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_reff (grasp_results_t *output, int it, int ix, int iy, int i, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->reff[i] = value;
}

void set_grasp_output_tile_aerosol_sd2m_opt_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int i, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_ext->ext[index2D(iwl,i,2)] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_cv_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->cv[0] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_std_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->std[0] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_rm_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->rm[0] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_reff_fine_mode (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->reff[0] = value;
}

void set_grasp_output_tile_aerosol_sd2m_opt_ext_fine_mode (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_ext->ext[index2D(iwl,0,2)] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_cv_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->cv[1] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_std_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->std[1] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_rm_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->rm[1] = value;
}

void set_grasp_output_tile_aerosol_sd2m_mph_reff_coarse_mode (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_mph->reff[1] = value;
}

void set_grasp_output_tile_aerosol_sd2m_opt_ext_coarse_mode (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->aerosol_sd2m_ext->ext[index2D(iwl,1,2)] = value;
}

void set_grasp_output_tile_aerosol_chem_rh (grasp_results_t *output, int it, int ix, int iy, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_chem->rh[isd] = value;
}

void set_grasp_output_tile_aerosol_chem_fwtr (grasp_results_t *output, int it, int ix, int iy, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_chem->fwrt[isd] = value;
}

void set_grasp_output_tile_aerosol_chem_fslbl (grasp_results_t *output, int it, int ix, int iy, int isd, float value){
  output->tile_result_map[ipixel_]->aerosol_chem->fslbl[isd] = value;
}

void set_grasp_output_tile_aerosol_chem_vfract (grasp_results_t *output, int it, int ix, int iy, int isd, int ichem, float value){
  output->tile_result_map[ipixel_]->aerosol_chem->vfract[index2D(isd,ichem,_N_CHEM_MAX)] = value;
}

/* modify by lei on 15/11/2016 */


void set_grasp_output_tile_aerosol_pm_pm (grasp_results_t *output, int it, int ix, int iy, int i, float value){
  output->tile_result_map[ipixel_]->aerosol_pm->pm[i] = value;
}

void set_grasp_output_tile_aerosol_types_index (grasp_results_t *output, int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->aerosol_types->index = value;
}


void set_grasp_output_tile_gases_absorption(grasp_results_t *output, int it, int ix, int iy, int iwl, int igas, int value){
  output->tile_result_map[ipixel_]->gases->absorption[index2D(iwl,igas,output->information.ngases)] = value;
}

/////////

void set_grasp_output_tile_surface_ndvi (grasp_results_t *output, int it, int ix, int iy, float value){
  output->tile_result_map[ipixel_]->surface_surf->ndvi = value;
}

void set_grasp_output_tile_surface_dhr (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->surface_surf->dhr[iwl] = value;
}

void set_grasp_output_tile_surface_bhr_iso (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->surface_surf->bhr_iso[iwl] = value;
}

void set_grasp_output_tile_errest_par_errp (grasp_results_t *output, int it, int ix, int iy, int ipar, float value){
  output->tile_result_map[ipixel_]->errest_par->ERRP[ipar] = value;
}

void set_grasp_output_tile_errest_par_biasp (grasp_results_t *output, int it, int ix, int iy, int ipar, float value){
  output->tile_result_map[ipixel_]->errest_par->BIASP[ipar] = value;
}

void set_grasp_output_tile_errest_par_tstdp (grasp_results_t *output, int it, int ix, int iy, int ipar, float value){
  output->tile_result_map[ipixel_]->errest_par->TSTDP[ipar] = value;
}

void set_grasp_output_tile_errest_par_sd_err (grasp_results_t *output, int it, int ix, int iy, int isd, int ibin, float value){
  output->tile_result_map[ipixel_]->errest_par->sd_err[index2D(isd,ibin,4*_KIDIM3)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_err_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->ERR_ext[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_bias_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->BIAS_ext[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_tstd_ext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->TSTD_ext[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_err_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->ERR_extt[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_bias_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->BIAS_extt[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_tstd_extt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->TSTD_extt[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_err_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->ERR_ssa[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_bias_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->BIAS_ssa[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_tstd_ssa (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->TSTD_ssa[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_err_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->ERR_ssat[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_bias_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->BIAS_ssat[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_tstd_ssat (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->TSTD_ssat[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_err_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->ERR_aext[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_bias_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->BIAS_aext[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_tstd_aext (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->TSTD_aext[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_err_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->ERR_aextt[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_bias_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->BIAS_aextt[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_opt_tstd_aextt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_opt->TSTD_aextt[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_lidar_err_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_lidar->ERR_lr[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_lidar_bias_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_lidar->BIAS_lr[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_lidar_tstd_lr (grasp_results_t *output, int it, int ix, int iy, int iwl, int isd, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_lidar->TSTD_lr[index2D(isd,iwl,NWL_)] = value;
}

void set_grasp_output_tile_errest_aerosol_lidar_err_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_lidar->ERR_lrt[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_lidar_bias_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_lidar->BIAS_lrt[iwl] = value;
}

void set_grasp_output_tile_errest_aerosol_lidar_tstd_lrt (grasp_results_t *output, int it, int ix, int iy, int iwl, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_lidar->TSTD_lrt[iwl] = value;
}

// MEH:
void set_grasp_output_tile_errest_aerosol_mic_err_sd (grasp_results_t *output, int it, int ix, int iy, int ibin, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_mic->ERR_sdt[ibin] = value;
}

void set_grasp_output_tile_errest_aerosol_mic_bias_sd (grasp_results_t *output, int it, int ix, int iy, int ibin, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_mic->BIAS_sdt[ibin] = value;
}

void set_grasp_output_tile_errest_aerosol_mic_tstd_sd (grasp_results_t *output, int it, int ix, int iy, int ibin, float value){
  output->tile_result_map[ipixel_]->errest_aerosol_mic->TSTD_sdt[ibin] = value;
}
///////////////

void set_grasp_output_tile_forcing_bbflux_nhlv (grasp_results_t *output, int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->forcing_bbflux->nhlv = value;
}

void set_grasp_output_tile_forcing_bbflux_bbufx0 (grasp_results_t *output, int it, int ix, int iy, int iknt, float value){
  output->tile_result_map[ipixel_]->forcing_bbflux->bbufx0[iknt] = value;
}

void set_grasp_output_tile_forcing_bbflux_bbdfx0 (grasp_results_t *output, int it, int ix, int iy, int iknt, float value){
  output->tile_result_map[ipixel_]->forcing_bbflux->bbdfx0[iknt] = value;
}

void set_grasp_output_tile_forcing_bbflux_bbufxa (grasp_results_t *output, int it, int ix, int iy, int iknt, float value){
  output->tile_result_map[ipixel_]->forcing_bbflux->bbufxa[iknt] = value;
}

void set_grasp_output_tile_forcing_bbflux_bbdfxa (grasp_results_t *output, int it, int ix, int iy, int iknt, float value){
  output->tile_result_map[ipixel_]->forcing_bbflux->bbdfxa[iknt] = value;
}

void set_grasp_output_tile_forcing_bbflux_hlv (grasp_results_t *output, int it, int ix, int iy, int iknt, float value){
  output->tile_result_map[ipixel_]->forcing_bbflux->hlv[iknt] = value;
}

void set_grasp_output_tile_forcing_forcing_nhlv (grasp_results_t *output, int it, int ix, int iy, int value){
  output->tile_result_map[ipixel_]->forcing_forcing->nhlv = value;
}

void set_grasp_output_tile_forcing_forcing_netforc (grasp_results_t *output, int it, int ix, int iy, int iknt, float value){
  output->tile_result_map[ipixel_]->forcing_forcing->netforc[iknt] = value;
}

void set_grasp_output_tile_forcing_forcing_forceff (grasp_results_t *output, int it, int ix, int iy, int iknt, float value){
  output->tile_result_map[ipixel_]->forcing_forcing->forceff[iknt] = value;
}

void set_grasp_output_tile_forcing_forcing_hlv (grasp_results_t *output, int it, int ix, int iy, int iknt, float value){
  output->tile_result_map[ipixel_]->forcing_forcing->hlv[iknt] = value;
}
