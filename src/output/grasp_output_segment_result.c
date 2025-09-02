/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <grasp/utils.h>
#include "grasp_output_segment_result.h"


/// NOTE: A developer can take advantage of the this private access to fields
///       to implement assertion verifications (values inside valid ranges of arrays)

const float *grasp_output_segment_parameters(const output_segment_general *output, int ipix){
    return output->retrieval.par.pixel[ipix].par;
}

bool grasp_output_segment_products_retrieval_res (const output_segment_general *output) {
    return output->products.retrieval.res;
}

bool grasp_output_segment_products_retrieval_par (const output_segment_general *output) {
    return output->products.retrieval.par;
}

bool grasp_output_segment_products_retrieval_fit (const output_segment_general *output) {
    return output->products.retrieval.fit;
}

bool grasp_output_segment_products_aerosol_opt (const output_segment_general *output) {
    return output->products.aerosol.opt;
}

bool grasp_output_segment_products_aerosol_rind (const output_segment_general *output) {
    return output->products.aerosol.rind;
}

bool grasp_output_segment_products_aerosol_chem (const output_segment_general *output) {
    return output->products.aerosol.chem;
}

bool grasp_output_segment_products_aerosol_phmx (const output_segment_general *output) {
    return output->products.aerosol.phmx;
}

bool grasp_output_segment_products_aerosol_lidar (const output_segment_general *output) {
    return output->products.aerosol.lidar;
}

bool grasp_output_segment_products_aerosol_sd2m_mph (const output_segment_general *output) {
    return output->products.aerosol.sd2m_mph;
}

bool grasp_output_segment_products_aerosol_sd2m_ext (const output_segment_general *output) {
    return output->products.aerosol.sd2m_ext;
}

bool grasp_output_segment_products_aerosol_pm (const output_segment_general *output) {
    return output->products.aerosol.pm;
}

bool grasp_output_segment_products_aerosol_types (const output_segment_general *output) {
    return output->products.aerosol.types;
}

bool grasp_output_segment_products_surface_surf (const output_segment_general *output) {
    return output->products.surface.surf;
}

bool grasp_output_segment_products_surface_bhr_iso (const output_segment_general *output) {
    return output->products.surface.bhr_iso;
}

bool grasp_output_segment_products_errest_par (const output_segment_general *output) {
    return output->products.errest.par;
}

bool grasp_output_segment_products_errest_aerosol_opt (const output_segment_general *output) {
    return output->products.errest.aerosol.opt;
}

// MEH:
bool grasp_output_segment_products_errest_aerosol_mic (const output_segment_general *output) {
    return output->products.errest.aerosol.mic;
}

bool grasp_output_segment_products_errest_aerosol_lidar (const output_segment_general *output) {
    return output->products.errest.aerosol.lidar;
}

bool grasp_output_segment_products_forcing_bbflux (const output_segment_general *output) {
    return output->products.forcing.bbflux;
}

bool grasp_output_segment_products_forcing_forcing (const output_segment_general *output) {
    return output->products.forcing.forcing;
}

float grasp_output_segment_coord_pixel_x (const output_segment_general *output,  int ipix) {
    return output->coord.pixel[ipix].x_lon;
}

float grasp_output_segment_coord_pixel_y (const output_segment_general *output,  int ipix) {
    return output->coord.pixel[ipix].y_lat;
}

int64_t grasp_output_segment_coord_pixel_t (const output_segment_general *output,  int ipix) {
    return output->coord.pixel[ipix].t_masl;
}

///////////////////

int grasp_output_segment_retrieval_res_niter (const output_segment_general *output) {
    return output->retrieval.res.niter;
}

float grasp_output_segment_retrieval_res_rest (const output_segment_general *output) {
    return output->retrieval.res.rest;
}

float grasp_output_segment_retrieval_res_resat (const output_segment_general *output, int inoise) {
    return output->retrieval.res.resat[inoise];
}

float grasp_output_segment_retrieval_res_resrt (const output_segment_general *output, int inoise) {
    return output->retrieval.res.resrt[inoise];
}

int grasp_output_segment_retrieval_res_pixel_niter (const output_segment_general *output, int ipix) {
    return output->retrieval.res.pixel[ipix].niter;
}

float grasp_output_segment_retrieval_res_pixel_res (const output_segment_general *output, int ipix) {
    return output->retrieval.res.pixel[ipix].res;
}

float grasp_output_segment_retrieval_res_pixel_resa (const output_segment_general *output, int ipix, int inoise) {
    return output->retrieval.res.pixel[ipix].resa[inoise];
}

float grasp_output_segment_retrieval_res_pixel_resr (const output_segment_general *output, int ipix, int inoise) {
    return output->retrieval.res.pixel[ipix].resr[inoise];
}

float grasp_output_segment_retrieval_par_parameters (const output_segment_general *output, int ipix, int ipar) {
    return output->retrieval.par.pixel[ipix].par[ipar];
}

float grasp_output_segment_retrieval_par_sd (const output_segment_general *output, int ipix, int isd, int ibin) {
    return output->retrieval.par.pixel[ipix].sd[isd][ibin];
}

const sensor_data_t *grasp_output_segment_retrieval_fit_segment_fit (const output_segment_general *output) {
    return &output->retrieval.fit.segment_fit;
}

const pixel_t *grasp_output_segment_retrieval_fit_pixel_fit (const output_segment_general *output, int ipix){
    return &output->retrieval.fit.segment_fit.pixel[ipix];
}


int grasp_output_segment_retrieval_information_npixels (const output_segment_general *output){
    return output->retrieval.information.npixels;
}

int grasp_output_segment_retrieval_information_nsd (const output_segment_general *output){
    return output->retrieval.information.nsd;
}

int grasp_output_segment_retrieval_information_nnoises (const output_segment_general *output){
    return output->retrieval.information.nnoises;
}

int grasp_output_segment_retrieval_information_nbins (const output_segment_general *output){
    return output->retrieval.information.nbins;
}

int grasp_output_segment_retrieval_information_nwl (const output_segment_general *output){
    return output->retrieval.information.nwl;
}

float grasp_output_segment_retrieval_information_wl (const output_segment_general *output, int iwl){
    return output->retrieval.information.wl[iwl];
}

par_number_NDIM grasp_output_segment_retrieval_information_ndim (const output_segment_general *output) {
    return output->retrieval.information.ndim;
}

int grasp_output_segment_retrieval_information_ngrid (const output_segment_general *output, int isd) {
    return output->retrieval.information.ngrid[isd];
}

float grasp_output_segment_retrieval_information_radius (const output_segment_general *output, int isd, int ipar) {
    return output->retrieval.information.radius[isd][ipar];
}

float grasp_output_segment_retrieval_information_sd_lb (const output_segment_general *output, int irr, int irc) {
    return output->retrieval.information.sd_lb[irc][irr];
}

int grasp_output_segment_retrieval_information_nhlv (const output_segment_general *output){
    return output->retrieval.information.nhlv;
}

float grasp_output_segment_retrieval_information_hlv (const output_segment_general *output, int ihlv){
    return output->retrieval.information.hlv[ihlv];
}
 
float grasp_output_segment_retrieval_information_delta_ut (const output_segment_general *output) {
    return output->retrieval.information.delta_ut;
}
 
float grasp_output_segment_retrieval_information_delta_ct (const output_segment_general *output) {
    return output->retrieval.information.delta_ct;
}

float grasp_output_segment_retrieval_information_nchem (const output_segment_general *output, int isd) {
    return output->retrieval.information.nchem[isd];
}


///////////////

float grasp_output_segment_aerosol_opt_aexp (const output_segment_general *output, int ipix) {
    return output->aerosol.opt.pixel[ipix].Aexp;
}

float grasp_output_segment_aerosol_opt_extt (const output_segment_general *output, int ipix, int iwl) {
    return output->aerosol.opt.pixel[ipix].wl[iwl].extt;
}

float grasp_output_segment_aerosol_opt_ssat (const output_segment_general *output, int ipix, int iwl) {
    return output->aerosol.opt.pixel[ipix].wl[iwl].ssat;
}

float grasp_output_segment_aerosol_opt_aextt (const output_segment_general *output, int ipix, int iwl) {
    return output->aerosol.opt.pixel[ipix].wl[iwl].aextt;
}

float grasp_output_segment_aerosol_opt_ext (const output_segment_general *output, int ipix, int iwl, int isd) {
    return output->aerosol.opt.pixel[ipix].wl[iwl].ext[isd];
}

float grasp_output_segment_aerosol_opt_ssa (const output_segment_general *output, int ipix, int iwl, int isd) {
    return output->aerosol.opt.pixel[ipix].wl[iwl].ssa[isd];
}

float grasp_output_segment_aerosol_opt_aext (const output_segment_general *output, int ipix, int iwl, int isd) {
    return output->aerosol.opt.pixel[ipix].wl[iwl].aext[isd];
}

float grasp_output_segment_aerosol_rind_mreal (const output_segment_general *output, int ipix, int iwl, int isd) {
    return output->aerosol.rind.pixel[ipix].wl[iwl].mreal[isd];
}

float grasp_output_segment_aerosol_rind_mimag (const output_segment_general *output, int ipix, int iwl, int isd) {
    return output->aerosol.rind.pixel[ipix].wl[iwl].mimag[isd];
}

int grasp_output_segment_aerosol_phmx_nangle (const output_segment_general *output){
    return output->aerosol.phmx.nangle;
}

float grasp_output_segment_aerosol_phmx_angle (const output_segment_general *output, int impar){
    return output->aerosol.phmx.angle[impar];
}

float grasp_output_segment_aerosol_phmx_ph11 (const output_segment_general *output, int ipix, int iwl, int impar, int isd){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].ph11[isd][impar];
}

float grasp_output_segment_aerosol_phmx_ph12 (const output_segment_general *output, int ipix, int iwl, int impar, int isd){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].ph12[isd][impar];
}

float grasp_output_segment_aerosol_phmx_ph22 (const output_segment_general *output, int ipix, int iwl, int impar, int isd){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].ph22[isd][impar];
}

float grasp_output_segment_aerosol_phmx_ph33 (const output_segment_general *output, int ipix, int iwl, int impar, int isd){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].ph33[isd][impar];
}

float grasp_output_segment_aerosol_phmx_ph34 (const output_segment_general *output, int ipix, int iwl, int impar, int isd){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].ph34[isd][impar];
}

float grasp_output_segment_aerosol_phmx_ph44 (const output_segment_general *output, int ipix, int iwl, int impar, int isd){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].ph44[isd][impar];
}

float grasp_output_segment_aerosol_phmx_pht11 (const output_segment_general *output, int ipix, int iwl, int impar){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].pht11[impar];
}

float grasp_output_segment_aerosol_phmx_pht12 (const output_segment_general *output, int ipix, int iwl, int impar){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].pht12[impar];
}

float grasp_output_segment_aerosol_phmx_pht22 (const output_segment_general *output, int ipix, int iwl, int impar){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].pht22[impar];
}

float grasp_output_segment_aerosol_phmx_pht33 (const output_segment_general *output, int ipix, int iwl, int impar){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].pht33[impar];
}

float grasp_output_segment_aerosol_phmx_pht34 (const output_segment_general *output, int ipix, int iwl, int impar){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].pht34[impar];
}

float grasp_output_segment_aerosol_phmx_pht44 (const output_segment_general *output, int ipix, int iwl, int impar){
    return output->aerosol.phmx.pixel[ipix].wl[iwl].pht44[impar];
}

float grasp_output_segment_aerosol_lidar_lr (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->aerosol.lidar.pixel[ipix].wl[iwl].lr[isd];
}

float grasp_output_segment_aerosol_lidar_ldpar (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->aerosol.lidar.pixel[ipix].wl[iwl].ldpar[isd];
}

float grasp_output_segment_aerosol_lidar_ldper (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->aerosol.lidar.pixel[ipix].wl[iwl].ldper[isd];
}

float grasp_output_segment_aerosol_lidar_lrt (const output_segment_general *output, int ipix, int iwl){
    return output->aerosol.lidar.pixel[ipix].wl[iwl].lrt;
}

float grasp_output_segment_aerosol_lidar_ldprt (const output_segment_general *output, int ipix, int iwl){
    return output->aerosol.lidar.pixel[ipix].wl[iwl].ldprt;
}

float grasp_output_segment_aerosol_sd2m_mph_cv (const output_segment_general *output, int ipix, int i){
    return output->aerosol.sd2m.mph.pixel[ipix].cv[i];
}

float grasp_output_segment_aerosol_sd2m_mph_std (const output_segment_general *output, int ipix, int i){
    return output->aerosol.sd2m.mph.pixel[ipix].std[i];
}

float grasp_output_segment_aerosol_sd2m_mph_rm (const output_segment_general *output, int ipix, int i){
    return output->aerosol.sd2m.mph.pixel[ipix].rm[i];
}

float grasp_output_segment_aerosol_sd2m_mph_reff (const output_segment_general *output, int ipix, int i){
    return output->aerosol.sd2m.mph.pixel[ipix].reff[i];
}

float grasp_output_segment_aerosol_sd2m_opt_ext (const output_segment_general *output, int ipix, int iwl, int i){
    return output->aerosol.sd2m.opt.pixel[ipix].wl[iwl].ext[i];
}

float grasp_output_segment_aerosol_sd2m_mph_cv_fine_mode (const output_segment_general *output, int ipix){
    return output->aerosol.sd2m.mph.pixel[ipix].cv[0];
}

float grasp_output_segment_aerosol_sd2m_mph_std_fine_mode (const output_segment_general *output, int ipix){
    return output->aerosol.sd2m.mph.pixel[ipix].std[0];
}

float grasp_output_segment_aerosol_sd2m_mph_rm_fine_mode (const output_segment_general *output, int ipix){
    return output->aerosol.sd2m.mph.pixel[ipix].rm[0];
}

float grasp_output_segment_aerosol_sd2m_mph_reff_fine_mode (const output_segment_general *output, int ipix){
    return output->aerosol.sd2m.mph.pixel[ipix].reff[0];
}

float grasp_output_segment_aerosol_sd2m_opt_ext_fine_mode (const output_segment_general *output, int ipix, int iwl){
    return output->aerosol.sd2m.opt.pixel[ipix].wl[iwl].ext[0];
}

float grasp_output_segment_aerosol_sd2m_mph_cv_coarse_mode (const output_segment_general *output, int ipix){
    return output->aerosol.sd2m.mph.pixel[ipix].cv[1];
}

float grasp_output_segment_aerosol_sd2m_mph_std_coarse_mode (const output_segment_general *output, int ipix){
    return output->aerosol.sd2m.mph.pixel[ipix].std[1];
}

float grasp_output_segment_aerosol_sd2m_mph_rm_coarse_mode (const output_segment_general *output, int ipix){
    return output->aerosol.sd2m.mph.pixel[ipix].rm[1];
}

float grasp_output_segment_aerosol_sd2m_mph_reff_coarse_mode (const output_segment_general *output, int ipix){
    return output->aerosol.sd2m.mph.pixel[ipix].reff[1];
}

float grasp_output_segment_aerosol_sd2m_opt_ext_coarse_mode (const output_segment_general *output, int ipix, int iwl){
    return output->aerosol.sd2m.opt.pixel[ipix].wl[iwl].ext[1];
}

float grasp_output_segment_aerosol_chem_rh (const output_segment_general *output, int ipix, int isd){
    return output->aerosol.chem.pixel[ipix].rh[isd];
}

float grasp_output_segment_aerosol_chem_fwtr (const output_segment_general *output, int ipix, int isd){
    return output->aerosol.chem.pixel[ipix].fwtr[isd];
}

float grasp_output_segment_aerosol_chem_fslbl (const output_segment_general *output, int ipix, int isd){
    return output->aerosol.chem.pixel[ipix].fslbl[isd];
}

float grasp_output_segment_aerosol_chem_vfract (const output_segment_general *output, int ipix, int isd, int nchem){
    return output->aerosol.chem.pixel[ipix].vfract[isd][nchem];
}

float grasp_output_segment_aerosol_pm_pm (const output_segment_general *output, int ipix, int i){
    return output->aerosol.pm.pixel[ipix].pm[i];
}

int grasp_output_segment_aerosol_types_index (const output_segment_general *output, int ipix){
    return output->aerosol.types.pixel[ipix].index;
}

int grasp_output_segment_gases_absorption (const output_segment_general *output, int ipix, int iwl, int ngas){
    return output->gases.pixel[ipix].wl[iwl].abs[ngas];
}

///////////////

float grasp_output_segment_surface_ndvi (const output_segment_general *output, int ipix){
    return output->surface.pixel[ipix].ndvi;
}

float grasp_output_segment_surface_dhr (const output_segment_general *output, int ipix, int iwl){
    return output->surface.pixel[ipix].wl[iwl].dhr;
}

float grasp_output_segment_surface_bhr_iso (const output_segment_general *output, int ipix, int iwl){
    return output->surface.pixel[ipix].wl[iwl].bhr_iso;
}

/////////////////

float grasp_output_segment_errest_par_errp (const output_segment_general *output, int ipix, int ipar){
    return output->errest.par.pixel[ipix].ERRP[ipar];
}

float grasp_output_segment_errest_par_biasp (const output_segment_general *output, int ipix, int ipar){
    return output->errest.par.pixel[ipix].BIASP[ipar];
}

float grasp_output_segment_errest_par_tstdp (const output_segment_general *output, int ipix, int ipar){
    return output->errest.par.pixel[ipix].TSTDP[ipar];
}

float grasp_output_segment_errest_par_sd_err (const output_segment_general *output, int ipix, int isd, int ibin){
    assert(isd>=0 && isd<_KSD+1);
    assert(ibin>=0 && ibin<4*_KIDIM3);
    return output->errest.par.pixel[ipix].sd_err[isd][ibin];
}

float grasp_output_segment_errest_aerosol_opt_err_ext (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].ERR_ext[isd];
}

float grasp_output_segment_errest_aerosol_opt_bias_ext (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].BIAS_ext[isd];
}

float grasp_output_segment_errest_aerosol_opt_tstd_ext (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].TSTD_ext[isd];
}

float grasp_output_segment_errest_aerosol_opt_err_extt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].ERR_extt;
}

float grasp_output_segment_errest_aerosol_opt_bias_extt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].BIAS_extt;
}

float grasp_output_segment_errest_aerosol_opt_tstd_extt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].TSTD_extt;
}

float grasp_output_segment_errest_aerosol_opt_err_ssa (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].ERR_ssa[isd];
}

float grasp_output_segment_errest_aerosol_opt_bias_ssa (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].BIAS_ssa[isd];
}

float grasp_output_segment_errest_aerosol_opt_tstd_ssa (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].TSTD_ssa[isd];
}

float grasp_output_segment_errest_aerosol_opt_err_ssat (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].ERR_ssat;
}

float grasp_output_segment_errest_aerosol_opt_bias_ssat (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].BIAS_ssat;
}

float grasp_output_segment_errest_aerosol_opt_tstd_ssat (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].TSTD_ssat;
}

float grasp_output_segment_errest_aerosol_opt_err_aext (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].ERR_aext[isd];
}

float grasp_output_segment_errest_aerosol_opt_bias_aext (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].BIAS_aext[isd];
}

float grasp_output_segment_errest_aerosol_opt_tstd_aext (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].TSTD_aext[isd];
}

float grasp_output_segment_errest_aerosol_opt_err_aextt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].ERR_aextt;
}

float grasp_output_segment_errest_aerosol_opt_bias_aextt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].BIAS_aextt;
}

float grasp_output_segment_errest_aerosol_opt_tstd_aextt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.opt.pixel[ipix].wl[iwl].TSTD_aextt;
}

float grasp_output_segment_errest_aerosol_lidar_err_lr (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.lidar.pixel[ipix].wl[iwl].ERR_lr[isd];
}

float grasp_output_segment_errest_aerosol_lidar_bias_lr (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.lidar.pixel[ipix].wl[iwl].BIAS_lr[isd];
}

float grasp_output_segment_errest_aerosol_lidar_tstd_lr (const output_segment_general *output, int ipix, int iwl, int isd){
    return output->errest.aerosol.lidar.pixel[ipix].wl[iwl].TSTD_lr[isd];
}

float grasp_output_segment_errest_aerosol_lidar_err_lrt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.lidar.pixel[ipix].wl[iwl].ERR_lrt;
}

float grasp_output_segment_errest_aerosol_lidar_bias_lrt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.lidar.pixel[ipix].wl[iwl].BIAS_lrt;
}

float grasp_output_segment_errest_aerosol_lidar_tstd_lrt (const output_segment_general *output, int ipix, int iwl){
    return output->errest.aerosol.lidar.pixel[ipix].wl[iwl].TSTD_lrt;
}

// MEH: error for sd when it is not part of the retrieved parameters
float grasp_output_segment_errest_aerosol_mic_err_sd (const output_segment_general *output, int ipix, int ibin){
    assert(ibin>=0 && ibin<4*_KIDIM3);
    return output->errest.aerosol.mic.pixel[ipix].grid[ibin].ERR_sdt;
}

float grasp_output_segment_errest_aerosol_mic_bias_sd (const output_segment_general *output, int ipix, int ibin){
    assert(ibin>=0 && ibin<4*_KIDIM3);
    return output->errest.aerosol.mic.pixel[ipix].grid[ibin].BIAS_sdt;
}

float grasp_output_segment_errest_aerosol_mic_tstd_sd (const output_segment_general *output, int ipix, int ibin){
    assert(ibin>=0 && ibin<4*_KIDIM3);
    return output->errest.aerosol.mic.pixel[ipix].grid[ibin].TSTD_sdt;
}


///////////////

int grasp_output_segment_forcing_bbflux_nhlv (const output_segment_general *output, int ipix){
    return output->forcing.bbflux.pixel[ipix].nhlv;
}

float grasp_output_segment_forcing_bbflux_bbufx0 (const output_segment_general *output, int ipix, int iknt){
    return output->forcing.bbflux.pixel[ipix].bbufx0[iknt];
}

float grasp_output_segment_forcing_bbflux_bbdfx0 (const output_segment_general *output, int ipix, int iknt){
    return output->forcing.bbflux.pixel[ipix].bbdfx0[iknt];
}

float grasp_output_segment_forcing_bbflux_bbufxa (const output_segment_general *output, int ipix, int iknt){
    return output->forcing.bbflux.pixel[ipix].bbufxa[iknt];
}

float grasp_output_segment_forcing_bbflux_bbdfxa (const output_segment_general *output, int ipix, int iknt){
    return output->forcing.bbflux.pixel[ipix].bbdfxa[iknt];
}

float grasp_output_segment_forcing_bbflux_hlv (const output_segment_general *output, int ipix, int iknt){
    return output->forcing.bbflux.pixel[ipix].hlv[iknt];
}

int grasp_output_segment_forcing_forcing_nhlv (const output_segment_general *output, int ipix){
    return output->forcing.forcing.pixel[ipix].nhlv;
}

float grasp_output_segment_forcing_forcing_netforc (const output_segment_general *output, int ipix, int iknt){
    return output->forcing.forcing.pixel[ipix].netforc[iknt];
}

float grasp_output_segment_forcing_forcing_forceff (const output_segment_general *output, int ipix, int iknt){
    return output->forcing.forcing.pixel[ipix].forceff[iknt];
}

float grasp_output_segment_forcing_forcing_hlv (const output_segment_general *output, int ipix, int iknt){
    return output->forcing.forcing.pixel[ipix].hlv[iknt];
}

