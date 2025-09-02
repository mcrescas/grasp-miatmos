/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include "grasp_output.h"
#include <string.h>
#include <stdlib.h>
#include <grasp/utils.h>
#include "mod_par_OS.inc"
#include "../retrieval/constants_set/mod_globals.inc"
#include <inttypes.h>
#include "../output/grasp_output_stream.h"
#include "../global/grasp_retrieval_characteristic_type.h"
//#include "regridding/AverageResampler.h"
#include <math.h>
#include "grasp_output_tile_result.h"
#include "../global/grasp_error_codes.h"


/**
 * This function extract output from segment and set it in tile output.
 * It follows these steps:
 * 1. Set available products as && operator between settings (what the user wants) and the output (what the retrieval can offer)
 * 2. Allocate memory for product blocks 
 * 3. 
 * @param settings
 * @param segment
 * @param output
 * @param tile_description
 * @param results
 * @param icol
 * @param irow
 * @param itime
 */
void grasp_output_process_output(grasp_settings *settings,grasp_segment_t *segment,output_segment_general *output, const grasp_tile_description_t *tile_description, grasp_results_t *results ,int icol,int irow,int itime){
    pixel_result_t *output_data;
    int ipixel;
    int i,j,k;
    int isegment;
    
    if (segment->sdata.npixels>0){
        isegment=index3D(itime,icol,irow,tile_description->dimensions.segment_ncols,tile_description->dimensions.segment_nrows);
        assert(isegment < tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_ntimes);
        // Allocating pixel result
        results->segment_result[isegment].pixel_result = (pixel_result_t *) trackmem_malloc(sizeof (pixel_result_t) * segment->sdata.npixels);
        assert(results->segment_result[isegment].pixel_result!=NULL);       
        // Number of pixels
        results->segment_result[isegment].npixel=segment->sdata.npixels; 

        // 1. Set available products as && operator between settings (what the user wants) and the output (what the retrieval can offer)
        results->products.retrieval.par = output->products.retrieval.par && settings->retrieval.products.retrieval.par;
        results->products.retrieval.res = output->products.retrieval.res && settings->retrieval.products.retrieval.res;
        results->products.retrieval.fit = output->products.retrieval.fit && settings->retrieval.products.retrieval.fit;

        results->products.aerosol.opt = output->products.aerosol.opt && settings->retrieval.products.aerosol.opt;
        results->products.aerosol.rind = output->products.aerosol.rind && settings->retrieval.products.aerosol.rind;
        results->products.aerosol.phmx = output->products.aerosol.phmx && settings->retrieval.products.aerosol.phmx;
        results->products.aerosol.lidar = output->products.aerosol.lidar && settings->retrieval.products.aerosol.lidar;
        results->products.aerosol.sd2m_mph = output->products.aerosol.sd2m_mph && settings->retrieval.products.aerosol.sd2m_mph;
        results->products.aerosol.sd2m_ext = output->products.aerosol.sd2m_ext && settings->retrieval.products.aerosol.sd2m_ext;
        results->products.aerosol.chem = output->products.aerosol.chem && settings->retrieval.products.aerosol.chem;
        results->products.aerosol.pm = output->products.aerosol.pm && settings->retrieval.products.aerosol.pm; 
        results->products.aerosol.types = output->products.aerosol.pm && settings->retrieval.products.aerosol.types; 

        results->products.surface.surf = output->products.surface.surf && settings->retrieval.products.surface.surf;    
        results->products.surface.bhr_iso = output->products.surface.bhr_iso && settings->retrieval.products.surface.bhr_iso;    

        results->products.errest.par = output->products.errest.par && settings->retrieval.products.errest.par;
        results->products.errest.aerosol.opt = output->products.errest.aerosol.opt && settings->retrieval.products.errest.aerosol.opt;
        results->products.errest.aerosol.lidar = output->products.errest.aerosol.lidar && settings->retrieval.products.errest.aerosol.lidar;
//MEH:
        results->products.errest.aerosol.mic = output->products.errest.aerosol.mic && settings->retrieval.products.errest.aerosol.mic;

        results->products.forcing.bbflux = output->products.forcing.bbflux && settings->retrieval.products.forcing.bbflux;
        results->products.forcing.forcing = output->products.forcing.forcing && settings->retrieval.products.forcing.forcing;   
        
        results->products.gases.absorption = output->products.gases.absorption && settings->retrieval.products.gases.absorption;   
        results->products.gases.concentration = output->products.gases.concentration && settings->retrieval.products.gases.concentration;   

        // Copying data   
        results->information.tile_npixels+=segment->sdata.npixels;    
        results->information.tile_npixels_t=tile_description->dimensions.tile_nt;
        results->information.tile_npixels_x=tile_description->dimensions.tile_nx;
        results->information.tile_npixels_y=tile_description->dimensions.tile_ny;
        
        results->information.ngases=0; // it is updated later

        results->information.npars=settings->retrieval.KNSING;
        
        memcpy(&results->information.ndim, &output->retrieval.information.ndim, sizeof(par_number_NDIM));
        
        // I compute NRC which represents number of precomputed lognormal bins
        results->information.nrc=0;
        j=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_SD_LB);
        if(j>=0){
            for (i = 0; i < settings->retrieval.NSD; i++) {
                results->information.nrc+=settings->retrieval.NDIM.n3[j][i];
            }
        }
        results->information.nmpar=output->aerosol.phmx.nangle;
        results->information.nsd=settings->retrieval.NSD;
        assert(results->information.nrc<=_KCpar);
        assert(results->information.nmpar<=_KMpar);
        
        for (i = 0; i < settings->retrieval.NSD+1; i++) {
            results->information.ngrid[i]=output->retrieval.information.ngrid[i];
            for (j = 0; j < results->information.ngrid[i] ; j++) {
                results->information.radius[i][j]=output->retrieval.information.radius[i][j];
            }
        }

        if(/*lognormal_bins*/ results->information.nrc>0){
            for (i = 0; i < results->information.nrc; i++) {
                for (j = 0; j < results->information.ngrid[0]; j++) {
                    results->information.sd_lb[i][j]=output->retrieval.information.sd_lb[i][j];
                }
            }
        }else{
            for (i = 0; i < _KCpar; i++) {
                for (j = 0; j < _NRR; j++) {
                    results->information.sd_lb[i][j]=0;
                }
            }
        }
        
        results->information.nhlv=output->retrieval.information.nhlv;
        for (i = 0; i < results->information.nhlv; i++) {
            results->information.hlv[i]=output->retrieval.information.hlv[i];
        }
        
        k=grasp_parameters_characteristic_code_present_of_kind_of_parameter(&(settings->retrieval.NDIM),par_type_AVP_beg,par_type_AVP_end);
        if(k>0){
            results->information.nhvp=grasp_parameters_number_of_elements_of_parameter(&(settings->retrieval.NDIM), k, 1);
        }else{
            results->information.nhvp=0;
        }
        
        for (i = 0; i < results->information.nmpar; i++) {
            results->information.phmx_angle[i]=output->aerosol.phmx.angle[i];
        }
        results->information.nPM_diam=settings->retrieval.nPM_diam;
        results->information.nnoises=settings->retrieval.NOISE.INOISE;
        
        results->information.nwl=settings->retrieval.NW;
        for (i = 0; i < output->retrieval.information.nwl; i++) {
            results->information.wl[i]=output->retrieval.information.wl[i];
        }
        
        for (i=0; i<results->information.nsd; i++){
            results->information.nchem[i] = output->retrieval.information.nchem[i];
        }
        

        // Segment fit is an excetion that we will address before the rest of parameters that are save in a loop for each pixel
        if(grasp_output_tile_products_retrieval_fit(results)){
            if(results->segment_fit==NULL){
               results->segment_fit = (sensor_data_t **) trackmem_malloc (sizeof(sensor_data_t *)*tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ntimes);
               for (i = 0; i < tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ntimes; i++) {
                   results->segment_fit[i]=NULL;
                }
               results->segment_original = (sensor_data_t **) trackmem_malloc (sizeof(sensor_data_t *)*tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ntimes);
               for (i = 0; i < tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ntimes; i++) {
                   results->segment_original[i]=NULL;
                }
            }
            results->segment_fit[isegment]=(sensor_data_t *)trackmem_malloc(sizeof(sensor_data_t));
            memcpy(results->segment_fit[isegment],&output->retrieval.fit,sizeof(sensor_data_t));   
            results->segment_original[isegment]=(sensor_data_t *)trackmem_malloc(sizeof(sensor_data_t));
            memcpy(results->segment_original[isegment],&segment->sdata,sizeof(sensor_data_t));   
        }
                
        for(ipixel=0;ipixel<segment->sdata.npixels;ipixel++){
            // 2. Allocate and copy memory for product blocks 
            results->segment_result[isegment].pixel_result[ipixel].information.segment_time=itime;
            results->segment_result[isegment].pixel_result[ipixel].information.segment_col=icol;
            results->segment_result[isegment].pixel_result[ipixel].information.segment_row=irow;
            results->segment_result[isegment].pixel_result[ipixel].information.it=segment->sdata.pixel[ipixel].it;
            results->segment_result[isegment].pixel_result[ipixel].information.ix=segment->sdata.pixel[ipixel].ix;
            results->segment_result[isegment].pixel_result[ipixel].information.iy=segment->sdata.pixel[ipixel].iy;
            results->segment_result[isegment].pixel_result[ipixel].information.out_x=segment->sdata.pixel[ipixel].out_x;
            results->segment_result[isegment].pixel_result[ipixel].information.out_y=segment->sdata.pixel[ipixel].out_y;        
            results->segment_result[isegment].pixel_result[ipixel].information.out_t=segment->sdata.pixel[ipixel].out_t;
            results->segment_result[isegment].pixel_result[ipixel].information.latitude=segment->sdata.pixel[ipixel].y;
            results->segment_result[isegment].pixel_result[ipixel].information.longitude=segment->sdata.pixel[ipixel].x;
            results->segment_result[isegment].pixel_result[ipixel].information.grid_col=segment->sdata.pixel[ipixel].icol;
            results->segment_result[isegment].pixel_result[ipixel].information.grid_row=segment->sdata.pixel[ipixel].irow;        
            results->segment_result[isegment].pixel_result[ipixel].information.time=segment->sdata.pixel[ipixel].t;
            
            results->segment_result[isegment].pixel_result[ipixel].information.real_time=output->retrieval.information.delta_ut/segment->sdata.npixels;
            results->segment_result[isegment].pixel_result[ipixel].information.user_time=output->retrieval.information.delta_ct/segment->sdata.npixels;;

            results->segment_result[isegment].pixel_result[ipixel].information.cloud_flag=!(segment->sdata.pixel[ipixel].cloudy);
            results->segment_result[isegment].pixel_result[ipixel].information.land_percent=segment->sdata.pixel[ipixel].land_percent;
            results->segment_result[isegment].pixel_result[ipixel].information.file_index=segment->sdata.pixel[ipixel].file_index;        
            results->segment_result[isegment].pixel_result[ipixel].information.masl=segment->sdata.pixel[ipixel].masl;

            results->segment_result[isegment].pixel_result[ipixel].information.sza    = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW);
            assert(results->segment_result[isegment].pixel_result[ipixel].information.sza!=NULL);
            for (i = 0; i < settings->retrieval.NW; i++) {
                results->segment_result[isegment].pixel_result[ipixel].information.sza[i] = segment->sdata.pixel[ipixel].meas[i].sza;
            }
            
            if(results->information.nhvp>0){
                results->segment_result[isegment].pixel_result[ipixel].information.hvp    = (float *) trackmem_malloc(sizeof(float)*results->information.nhvp);
                assert(results->segment_result[isegment].pixel_result[ipixel].information.hvp!=NULL);
                for (i = 0; i < results->information.nhvp; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].information.hvp[i] = segment->sdata.pixel[ipixel].hvp[i];
                }
            }else{
                results->segment_result[isegment].pixel_result[ipixel].information.hvp=NULL;
            }
            
            // Initializing pixel results
            results->segment_result[isegment].pixel_result[ipixel].retrieval_res=NULL;
            results->segment_result[isegment].pixel_result[ipixel].retrieval_par=NULL;
            results->segment_result[isegment].pixel_result[ipixel].pixel_fit=NULL;
            results->segment_result[isegment].pixel_result[ipixel].pixel_original=NULL;

            results->segment_result[isegment].pixel_result[ipixel].aerosol_opt=NULL;
            results->segment_result[isegment].pixel_result[ipixel].aerosol_rind=NULL;
            results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx=NULL;
            results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar=NULL;
            results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_mph=NULL;
            results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_ext=NULL;
            results->segment_result[isegment].pixel_result[ipixel].aerosol_chem=NULL;
            results->segment_result[isegment].pixel_result[ipixel].aerosol_pm=NULL;
            results->segment_result[isegment].pixel_result[ipixel].aerosol_types=NULL;

            results->segment_result[isegment].pixel_result[ipixel].surface_surf=NULL;

            results->segment_result[isegment].pixel_result[ipixel].errest_par=NULL;
            results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt=NULL;
            results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar=NULL;
// MEH:
            results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic=NULL;
//
            results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux=NULL;
            results->segment_result[isegment].pixel_result[ipixel].forcing_forcing=NULL;

            if(grasp_output_tile_products_retrieval_res(results)){
                results->segment_result[isegment].pixel_result[ipixel].retrieval_res = (grasp_output_tile_retrieval_res *) trackmem_malloc(sizeof (grasp_output_tile_retrieval_res)*1);
                assert(results->segment_result[isegment].pixel_result[ipixel].retrieval_res!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].retrieval_res->resa = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NOISE.INOISE);
                assert(results->segment_result[isegment].pixel_result[ipixel].retrieval_res->resa!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].retrieval_res->resr = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NOISE.INOISE);
                assert(results->segment_result[isegment].pixel_result[ipixel].retrieval_res->resr!=NULL);

                if(settings->retrieval.IPFP.INVSING==2){ //if multipixel
                    results->segment_result[isegment].pixel_result[ipixel].retrieval_res->niter=output->retrieval.res.niter;
                    results->segment_result[isegment].pixel_result[ipixel].retrieval_res->rest=output->retrieval.res.rest;
                }else{ // We do not cover the case of single-pixel followed by multipixel
                    results->segment_result[isegment].pixel_result[ipixel].retrieval_res->niter=output->retrieval.res.pixel[ipixel].niter;
                    results->segment_result[isegment].pixel_result[ipixel].retrieval_res->rest=output->retrieval.res.pixel[ipixel].res;
                }
                for (i = 0; i < settings->retrieval.NOISE.INOISE; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].retrieval_res->resa[i] = output->retrieval.res.pixel[ipixel].resa[i];
                    results->segment_result[isegment].pixel_result[ipixel].retrieval_res->resr[i] = output->retrieval.res.pixel[ipixel].resr[i];
                }

            }

            if(grasp_output_tile_products_retrieval_par(results)){
                results->segment_result[isegment].pixel_result[ipixel].retrieval_par = (grasp_output_tile_retrieval_par *) trackmem_malloc(sizeof (grasp_output_tile_retrieval_par)*1);

                results->segment_result[isegment].pixel_result[ipixel].retrieval_par->parameters = (float *) trackmem_malloc(sizeof (float)*settings->retrieval.KNSING);
                results->segment_result[isegment].pixel_result[ipixel].retrieval_par->sd = (float *) trackmem_malloc(sizeof (float)*((_KSD+1)*(4*_KIDIM3)));
                assert(results->segment_result[isegment].pixel_result[ipixel].retrieval_par->parameters!=NULL);
                for (i = 0; i < settings->retrieval.KNSING; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].retrieval_par->parameters[i]=output->retrieval.par.pixel[ipixel].par[i];
                }   
                for (i = 0; i < results->information.nsd+1; i++) {
                    for (j = 0; j < _KIDIM3*4; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].retrieval_par->sd[index2D(i,j,_KIDIM3*4)]=output->retrieval.par.pixel[ipixel].sd[i][j];
                    }
                }
            }
            if(grasp_output_tile_products_retrieval_fit(results)){
                // It is just point to the already allocated pixel
                results->segment_result[isegment].pixel_result[ipixel].pixel_fit = &results->segment_fit[isegment]->pixel[ipixel];
                results->segment_result[isegment].pixel_result[ipixel].pixel_original = &results->segment_original[isegment]->pixel[ipixel];
            }

            if(grasp_output_tile_products_aerosol_opt(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_opt = (grasp_output_tile_aerosol_opt *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_opt)*1);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->Aexp=output->aerosol.opt.pixel[ipixel].Aexp;

                results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->extt    = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->extt!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ssat    = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ssat!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->aextt    = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->aextt!=NULL);        

                results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ext    = (float *) trackmem_malloc(sizeof(float*)*settings->retrieval.NW*results->information.nsd);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ext!=NULL);


                results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ssa    = (float *) trackmem_malloc(sizeof(float*)*settings->retrieval.NW*results->information.nsd);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ssa!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->aext    = (float *) trackmem_malloc(sizeof(float*)*settings->retrieval.NW*results->information.nsd);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->aext!=NULL);

                for (i = 0; i < settings->retrieval.NW; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->extt[i]   = output->aerosol.opt.pixel[ipixel].wl[i].extt;
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ssat[i]   = output->aerosol.opt.pixel[ipixel].wl[i].ssat;
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->aextt[i]   = output->aerosol.opt.pixel[ipixel].wl[i].aextt;    
                    for (j = 0; j < results->information.nsd; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ext[index2D(i,j,results->information.nsd)]   = output->aerosol.opt.pixel[ipixel].wl[i].ext[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->ssa[index2D(i,j,results->information.nsd)]   = output->aerosol.opt.pixel[ipixel].wl[i].ssa[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_opt->aext[index2D(i,j,results->information.nsd)]   = output->aerosol.opt.pixel[ipixel].wl[i].aext[j];                       
                    }
                }  
            }

            if(grasp_output_tile_products_aerosol_rind(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_rind = (grasp_output_tile_aerosol_rind *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_rind)*1);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_rind->mreal    = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW*results->information.nsd);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_rind->mreal!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_rind->mimag    = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW*results->information.nsd);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_rind->mimag!=NULL);

                for (i = 0; i < settings->retrieval.NW; i++) {   
                    for (j = 0; j < results->information.nsd; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_rind->mreal[index2D(i,j,results->information.nsd)]   = output->aerosol.rind.pixel[ipixel].wl[i].mreal[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_rind->mimag[index2D(i,j,results->information.nsd)]   = output->aerosol.rind.pixel[ipixel].wl[i].mimag[j];                       
                    }
                }  
            }
            if(grasp_output_tile_products_aerosol_phmx(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx = (grasp_output_tile_aerosol_phmx *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_phmx)*1);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph11   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nsd*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph11!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph12   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nsd*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph12!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph22   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nsd*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph22!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph33   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nsd*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph33!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph34   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nsd*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph34!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph44   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nsd*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph44!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht11   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht11!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht12   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht12!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht22   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht22!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht33   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht33!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht34   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht34!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht44   = (float *) trackmem_malloc(sizeof(float)*results->information.nwl*results->information.nmpar);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht44!=NULL);

                for (i = 0; i < results->information.nwl; i++) {
                    for (j = 0; j < results->information.nsd; j++) {
                        for (k = 0; k < results->information.nmpar; k++) {
                            results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph11[index3D(i,j,k,results->information.nsd,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].ph11[j][k];
                            results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph12[index3D(i,j,k,results->information.nsd,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].ph12[j][k];
                            results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph22[index3D(i,j,k,results->information.nsd,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].ph22[j][k];
                            results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph33[index3D(i,j,k,results->information.nsd,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].ph33[j][k];
                            results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph34[index3D(i,j,k,results->information.nsd,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].ph34[j][k];
                            results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->ph44[index3D(i,j,k,results->information.nsd,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].ph44[j][k];
                        }
                    }
                }
                for (i = 0; i < results->information.nwl; i++) {
                    for (j = 0; j < results->information.nmpar; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht11[index2D(i,j,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].pht11[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht12[index2D(i,j,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].pht12[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht22[index2D(i,j,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].pht22[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht33[index2D(i,j,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].pht33[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht34[index2D(i,j,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].pht34[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_phmx->pht44[index2D(i,j,results->information.nmpar)]=output->aerosol.phmx.pixel[ipixel].wl[i].pht44[j];
                    }
                }

            }
            if(grasp_output_tile_products_aerosol_lidar(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar = (grasp_output_tile_aerosol_lidar *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_lidar)*1);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->lrt = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->lrt!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldprt = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldprt!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->lr = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW*results->information.nsd);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->lr!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldpar = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW*results->information.nsd);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldpar!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldper = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW*results->information.nsd);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldper!=NULL);
                for (i = 0; i < settings->retrieval.NW; i++) {
                     results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->lrt[i] = output->aerosol.lidar.pixel[ipixel].wl[i].lrt;  
                     results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldprt[i] = output->aerosol.lidar.pixel[ipixel].wl[i].ldprt;  
                     for (j = 0; j < results->information.nsd; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->lr[index2D(i,j,results->information.nsd)] = output->aerosol.lidar.pixel[ipixel].wl[i].lr[j];  
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldpar[index2D(i,j,results->information.nsd)] = output->aerosol.lidar.pixel[ipixel].wl[i].ldpar[j];
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_lidar->ldper[index2D(i,j,results->information.nsd)] = output->aerosol.lidar.pixel[ipixel].wl[i].ldper[j];
                    }
                }   
            }
            if(grasp_output_tile_products_aerosol_sd2m_mph(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_mph = (grasp_output_tile_aerosol_sd2m_mph *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_sd2m_mph)*1);

                for (i = 0; i < 3; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_mph->cv[i]=output->aerosol.sd2m.mph.pixel[ipixel].cv[i];
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_mph->std[i]=output->aerosol.sd2m.mph.pixel[ipixel].std[i];
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_mph->rm[i]=output->aerosol.sd2m.mph.pixel[ipixel].rm[i];
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_mph->reff[i]=output->aerosol.sd2m.mph.pixel[ipixel].reff[i];
                }
            }
            if(grasp_output_tile_products_aerosol_sd2m_ext(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_ext = (grasp_output_tile_aerosol_sd2m_ext *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_sd2m_ext)*1);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_ext->ext = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW*2);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_ext->ext!=NULL);

                for (i = 0; i < settings->retrieval.NW; i++) {
                    for (j = 0; j < 2; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_sd2m_ext->ext[index2D(i,j,2)] = output->aerosol.sd2m.opt.pixel[ipixel].wl[i].ext[j];
                    }    
                } 
            }
            
            if(grasp_output_tile_products_aerosol_pm(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_pm = (grasp_output_tile_aerosol_pm *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_pm)*1);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_pm->pm = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.nPM_diam);
                assert(results->segment_result[isegment].pixel_result[ipixel].aerosol_pm->pm != NULL);

                for (i=0; i<settings->retrieval.nPM_diam; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_pm->pm[i] = output->aerosol.pm.pixel[ipixel].pm[i];
                }
            }
        
            if(grasp_output_tile_products_aerosol_chem(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_chem = (grasp_output_tile_aerosol_chem *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_chem)*1);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_chem->rh = (float *) trackmem_malloc(sizeof(float)*results->information.nsd);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_chem->fwrt = (float *) trackmem_malloc(sizeof(float)*results->information.nsd);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_chem->fslbl = (float *) trackmem_malloc(sizeof(float)*results->information.nsd);
                results->segment_result[isegment].pixel_result[ipixel].aerosol_chem->vfract = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*_N_CHEM_MAX);  /* add by lei on 15/11/2016 */

                for (i = 0; i < results->information.nsd; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_chem->rh[i]=output->aerosol.chem.pixel[ipixel].rh[i];
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_chem->fwrt[i]=output->aerosol.chem.pixel[ipixel].fwtr[i];
                    results->segment_result[isegment].pixel_result[ipixel].aerosol_chem->fslbl[i]=output->aerosol.chem.pixel[ipixel].fslbl[i];
                    for (j=0; j<_N_CHEM_MAX; j++){
                        results->segment_result[isegment].pixel_result[ipixel].aerosol_chem->vfract[index2D(i,j,_N_CHEM_MAX)]=output->aerosol.chem.pixel[ipixel].vfract[i][j];  /* add by lei on 15/11/2016 */
                    }
                }
            }
            if(grasp_output_tile_products_aerosol_types(results)){
                results->segment_result[isegment].pixel_result[ipixel].aerosol_types = (grasp_output_tile_aerosol_types *) trackmem_malloc(sizeof (grasp_output_tile_aerosol_types)*1);

                results->segment_result[isegment].pixel_result[ipixel].aerosol_types->index=output->aerosol.types.pixel[ipixel].index;
            }
            
            if(grasp_output_tile_products_gases_absorption(results)){
                results->segment_result[isegment].pixel_result[ipixel].gases = (grasp_output_tile_gases *) trackmem_malloc(sizeof (grasp_output_tile_gases)*1);

                results->information.ngases = settings->retrieval.gases.nlut_name+1;
                results->segment_result[isegment].pixel_result[ipixel].gases->absorption = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW*(settings->retrieval.gases.nlut_name+1));
                for (i = 0; i < settings->retrieval.NW; i++) {
                    for (j = 0; j< results->information.ngases; j++){
                        results->segment_result[isegment].pixel_result[ipixel].gases->absorption[index2D(i,j,results->information.ngases)]=output->gases.pixel[ipixel].wl[i].abs[j];
                    }
                }  
               
            }

    //////////        
            
            if(grasp_output_tile_products_surface_surf(results) || grasp_output_tile_products_surface_bhr_iso(results)){
                results->segment_result[isegment].pixel_result[ipixel].surface_surf = (grasp_output_tile_surface_surf *) trackmem_malloc(sizeof (grasp_output_tile_surface_surf)*1);

                if(grasp_output_tile_products_surface_surf(results)){
                    results->segment_result[isegment].pixel_result[ipixel].surface_surf->ndvi=output->surface.pixel[ipixel].ndvi;

                    results->segment_result[isegment].pixel_result[ipixel].surface_surf->dhr = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW);
                    assert(results->segment_result[isegment].pixel_result[ipixel].surface_surf->dhr!=NULL);

                    for (i = 0; i < settings->retrieval.NW; i++) {
                        results->segment_result[isegment].pixel_result[ipixel].surface_surf->dhr[i]= output->surface.pixel[ipixel].wl[i].dhr;          
                    }  
                }
                if(grasp_output_tile_products_surface_bhr_iso(results)){
                    results->segment_result[isegment].pixel_result[ipixel].surface_surf->bhr_iso = (float *) trackmem_malloc(sizeof(float)*settings->retrieval.NW);
                    assert(results->segment_result[isegment].pixel_result[ipixel].surface_surf->bhr_iso!=NULL);

                    for (i = 0; i < settings->retrieval.NW; i++) {
                        results->segment_result[isegment].pixel_result[ipixel].surface_surf->bhr_iso[i]= output->surface.pixel[ipixel].wl[i].bhr_iso;          
                    }  
                }
            }
            

            if(grasp_output_tile_products_errest_par(results)){
                results->segment_result[isegment].pixel_result[ipixel].errest_par = (grasp_output_tile_errest_par *) trackmem_malloc(sizeof (grasp_output_tile_errest_par)*1);

                results->segment_result[isegment].pixel_result[ipixel].errest_par->ERRP = (float *) trackmem_malloc(sizeof(float)*results->information.npars);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_par->ERRP!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].errest_par->BIASP = (float *) trackmem_malloc(sizeof(float)*results->information.npars);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_par->BIASP!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].errest_par->TSTDP = (float *) trackmem_malloc(sizeof(float)*results->information.npars);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_par->TSTDP!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].errest_par->sd_err = (float *) trackmem_malloc(sizeof (float)*((_KSD+1)*(4*_KIDIM3)));
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_par->sd_err!=NULL);

                for (i = 0; i < results->information.npars; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].errest_par->ERRP[i]=output->errest.par.pixel[ipixel].ERRP[i];
                    results->segment_result[isegment].pixel_result[ipixel].errest_par->BIASP[i]=output->errest.par.pixel[ipixel].BIASP[i];
                    results->segment_result[isegment].pixel_result[ipixel].errest_par->TSTDP[i]=output->errest.par.pixel[ipixel].TSTDP[i];
                }
                for (i = 0; i < results->information.nsd+1; i++) {
                    for (j = 0; j < _KIDIM3*4; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].errest_par->sd_err[index2D(i,j,_KIDIM3*4)]=output->errest.par.pixel[ipixel].sd_err[i][j];
                    }
                }
            }
            if(grasp_output_tile_products_errest_aerosol_opt(results)){
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt = (grasp_output_tile_errest_aerosol_opt *) trackmem_malloc(sizeof (grasp_output_tile_errest_aerosol_opt)*1);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ext = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ext!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ext = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ext!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ext = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ext!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_extt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_extt!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_extt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_extt!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_extt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_extt!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ssa = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ssa!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ssa = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ssa!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ssa = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ssa!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ssat = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ssat!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ssat = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ssat!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ssat = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ssat!=NULL);
//MEH: for aext
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_aext = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_aext!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_aext = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_aext!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_aext = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_aext!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_aextt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_aextt!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_aextt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_aextt!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_aextt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_aextt!=NULL);

                for (i = 0; i < results->information.nwl; i++) {
                    for (j = 0; j < results->information.nsd; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ext[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].ERR_ext[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ext[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].BIAS_ext[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ext[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].TSTD_ext[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ssa[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].ERR_ssa[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ssa[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].BIAS_ssa[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ssa[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].TSTD_ssa[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_aext[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].ERR_aext[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_aext[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].BIAS_aext[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_aext[index2D(i,j,results->information.nsd)]=output->errest.aerosol.opt.pixel[ipixel].wl[i].TSTD_aext[j];

                    }
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_extt[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].ERR_extt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_extt[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].BIAS_extt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_extt[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].TSTD_extt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_ssat[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].ERR_ssat;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_ssat[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].BIAS_ssat;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_ssat[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].TSTD_ssat;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->ERR_aextt[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].ERR_aextt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->BIAS_aextt[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].BIAS_aextt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_opt->TSTD_aextt[i]=output->errest.aerosol.opt.pixel[ipixel].wl[i].TSTD_aextt;
                }
            }
            if(grasp_output_tile_products_errest_aerosol_lidar(results)){
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar = (grasp_output_tile_errest_aerosol_lidar *) trackmem_malloc(sizeof (grasp_output_tile_errest_aerosol_lidar)*1);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->ERR_lr = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->ERR_lr!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->BIAS_lr = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->BIAS_lr!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->TSTD_lr = (float *) trackmem_malloc(sizeof(float)*results->information.nsd*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->TSTD_lr!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->ERR_lrt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->ERR_lrt!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->BIAS_lrt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->BIAS_lrt!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->TSTD_lrt = (float *) trackmem_malloc(sizeof(float)*results->information.nwl);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->TSTD_lrt!=NULL);

                for (i = 0; i < results->information.nwl; i++) {
                    for (j = 0; j < results->information.nsd; j++) {
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->ERR_lr[index2D(i,j,results->information.nsd)]=output->errest.aerosol.lidar.pixel[ipixel].wl[i].ERR_lr[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->BIAS_lr[index2D(i,j,results->information.nsd)]=output->errest.aerosol.lidar.pixel[ipixel].wl[i].BIAS_lr[j];
                        results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->TSTD_lr[index2D(i,j,results->information.nsd)]=output->errest.aerosol.lidar.pixel[ipixel].wl[i].TSTD_lr[j];
                    }
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->ERR_lrt[i]=output->errest.aerosol.lidar.pixel[ipixel].wl[i].ERR_lrt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->BIAS_lrt[i]=output->errest.aerosol.lidar.pixel[ipixel].wl[i].BIAS_lrt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_lidar->TSTD_lrt[i]=output->errest.aerosol.lidar.pixel[ipixel].wl[i].TSTD_lrt;
                }
            }
            if(grasp_output_tile_products_errest_aerosol_mic(results)){
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic = (grasp_output_tile_errest_aerosol_mic *) trackmem_malloc(sizeof (grasp_output_tile_errest_aerosol_mic)*1);

                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->ERR_sdt = (float *) trackmem_malloc(sizeof(float)*results->information.ngrid[0]);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->ERR_sdt!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->BIAS_sdt = (float *) trackmem_malloc(sizeof(float)*results->information.ngrid[0]);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->BIAS_sdt!=NULL);
                
                results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->TSTD_sdt = (float *) trackmem_malloc(sizeof(float)*results->information.ngrid[0]);
                assert(results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->TSTD_sdt!=NULL);

                for (i = 0; i < results->information.ngrid[0]; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->ERR_sdt[i]=output->errest.aerosol.mic.pixel[ipixel].grid[i].ERR_sdt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->BIAS_sdt[i]=output->errest.aerosol.mic.pixel[ipixel].grid[i].BIAS_sdt;
                    results->segment_result[isegment].pixel_result[ipixel].errest_aerosol_mic->TSTD_sdt[i]=output->errest.aerosol.mic.pixel[ipixel].grid[i].TSTD_sdt;
                }
            }
            if(grasp_output_tile_products_forcing_bbflux(results)){
                results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux = (grasp_output_tile_forcing_bbflux *) trackmem_malloc(sizeof (grasp_output_tile_forcing_bbflux)*1);

                results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbufx0 = (float *) trackmem_malloc(sizeof(float)*output->forcing.bbflux.pixel[ipixel].nhlv);
                assert(results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbufx0!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbdfx0 = (float *) trackmem_malloc(sizeof(float)*output->forcing.bbflux.pixel[ipixel].nhlv);
                assert(results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbdfx0!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbufxa = (float *) trackmem_malloc(sizeof(float)*output->forcing.bbflux.pixel[ipixel].nhlv);
                assert(results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbufxa!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbdfxa = (float *) trackmem_malloc(sizeof(float)*output->forcing.bbflux.pixel[ipixel].nhlv);
                assert(results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbdfxa!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->hlv = (float *) trackmem_malloc(sizeof(float)*output->forcing.bbflux.pixel[ipixel].nhlv);
                assert(results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->hlv!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->nhlv=output->forcing.bbflux.pixel[ipixel].nhlv;
                for (i = 0; i < output->forcing.bbflux.pixel[ipixel].nhlv; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbufx0[i]=output->forcing.bbflux.pixel[ipixel].bbufx0[i];
                    results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbdfx0[i]=output->forcing.bbflux.pixel[ipixel].bbdfx0[i];
                    results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbufxa[i]=output->forcing.bbflux.pixel[ipixel].bbufxa[i];
                    results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->bbdfxa[i]=output->forcing.bbflux.pixel[ipixel].bbdfxa[i];
                    results->segment_result[isegment].pixel_result[ipixel].forcing_bbflux->hlv[i]=output->forcing.bbflux.pixel[ipixel].hlv[i];
                }
            }
            if(grasp_output_tile_products_forcing_forcing(results)){
                results->segment_result[isegment].pixel_result[ipixel].forcing_forcing = (grasp_output_tile_forcing_forcing *) trackmem_malloc(sizeof (grasp_output_tile_forcing_forcing)*1);

                results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->netforc=(float *) trackmem_malloc(sizeof(float)*output->forcing.forcing.pixel[ipixel].nhlv);
                assert(results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->netforc!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->forceff=(float *) trackmem_malloc(sizeof(float)*output->forcing.forcing.pixel[ipixel].nhlv);
                assert(results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->forceff!=NULL);
                results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->hlv    =(float *) trackmem_malloc(sizeof(float)*output->forcing.forcing.pixel[ipixel].nhlv);
                assert(results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->hlv!=NULL);

                results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->nhlv=output->forcing.forcing.pixel[ipixel].nhlv;
                for (i = 0; i < output->forcing.forcing.pixel[ipixel].nhlv; i++) {
                    results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->netforc[i] = output->forcing.forcing.pixel[ipixel].netforc[i];
                    results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->forceff[i] = output->forcing.forcing.pixel[ipixel].forceff[i];
                    results->segment_result[isegment].pixel_result[ipixel].forcing_forcing->hlv[i]     = output->forcing.forcing.pixel[ipixel].hlv[i];
                }
            }

        }

        // Reindex the data to a pixel tile instead of a segment->sdata tile
        for(ipixel=0;ipixel<segment->sdata.npixels;ipixel++){
            output_data=&(results->segment_result[isegment].pixel_result[ipixel]);
            i=index3D(output_data->information.out_t,output_data->information.out_x,output_data->information.out_y, tile_description->dimensions.tile_nx,tile_description->dimensions.tile_ny);
            assert(i < tile_description->dimensions.tile_nt*tile_description->dimensions.tile_nx*tile_description->dimensions.tile_ny);
            results->tile_result_map[i]=output_data;
        }
    }
}



int grasp_output_initialize_results(grasp_settings *settings, const grasp_tile_description_t *tile_description, grasp_results_t *results){
    int i, icol, irow, itime;
        
    results->information.tile_npixels=0;
    results->segment_result=NULL;
    results->segment_fit=NULL;
    results->segment_original=NULL;
    results->information.ngases=0;
         
    if (tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_ntimes<=0){
        return GRASP_ERROR_CONTROLLER_EMPTY_TILE;
    }
    // Allocate results pointer
    results->segment_result = (grasp_output_tile_segment_result_t *) trackmem_malloc(sizeof (grasp_output_tile_segment_result_t)*tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_ntimes);
    assert(results->segment_result!=NULL);
    for (i = 0; i < tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ntimes; i++) {
        results->segment_result[i].npixel=0;
        results->segment_result[i].pixel_result=NULL;
    }
    
    // Reindex output, same than obove but refering pixels instead of segments.
    results->tile_result_map = (pixel_result_t **) trackmem_malloc(sizeof (pixel_result_t *)*tile_description->dimensions.tile_nt*tile_description->dimensions.tile_nx*tile_description->dimensions.tile_ny);
    assert(results->tile_result_map!=NULL);
    // Initialize map
    for (itime = 0; itime < tile_description->dimensions.tile_nt; itime++) {
        for (icol = 0; icol < tile_description->dimensions.tile_nx; icol++) {
            for (irow = 0; irow < tile_description->dimensions.tile_ny; irow++) {
                results->tile_result_map[index3D(itime,icol,irow,tile_description->dimensions.tile_nx,tile_description->dimensions.tile_ny)]=NULL;
            }
        }
    }    
    
    return 0;
}

void grasp_output_destroy_result(const grasp_tile_description_t *tile_description, grasp_results_t *results){
    int i,itime,ix,iy,ipixel;
        
    if(results->segment_result!=NULL){
        for (itime = 0; itime < tile_description->dimensions.tile_nt; itime++) {
            for (ix = 0; ix < tile_description->dimensions.tile_nx; ix++) {
                for (iy = 0; iy < tile_description->dimensions.tile_ny; iy++) {
                    ipixel=index3D(itime,ix,iy,tile_description->dimensions.tile_nx,tile_description->dimensions.tile_ny);
                    
                    if( results->tile_result_map[ipixel] != NULL ){
                        trackmem_free(results->tile_result_map[ipixel]->information.sza);
                        
                        if(results->tile_result_map[ipixel]->information.hvp != NULL){
                            trackmem_free(results->tile_result_map[ipixel]->information.hvp);
                        }
                        
                        if(grasp_output_tile_products_retrieval_res(results)){
                            trackmem_free(results->tile_result_map[ipixel]->retrieval_res->resa);
                            trackmem_free(results->tile_result_map[ipixel]->retrieval_res->resr);
                            
                            trackmem_free(results->tile_result_map[ipixel]->retrieval_res);
                        }
                        if(grasp_output_tile_products_retrieval_par(results)){
                            trackmem_free(results->tile_result_map[ipixel]->retrieval_par->parameters);
                            trackmem_free(results->tile_result_map[ipixel]->retrieval_par->sd);
                            
                            trackmem_free(results->tile_result_map[ipixel]->retrieval_par);
                        }
                        if(grasp_output_tile_products_aerosol_opt(results)){
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_opt->extt);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_opt->ssat);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_opt->aextt);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_opt->ext);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_opt->ssa);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_opt->aext);
                            
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_opt);
                        }
                        if(grasp_output_tile_products_aerosol_rind(results)){
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_rind->mreal);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_rind->mimag);
                            
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_rind);
                        }
                        if(grasp_output_tile_products_aerosol_phmx(results)){
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->ph11);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->ph12);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->ph22);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->ph33);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->ph34);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->ph44);
                            
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->pht11);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->pht12);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->pht22);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->pht33);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->pht34);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx->pht44);
                                           
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_phmx);
                        }
                        if(grasp_output_tile_products_aerosol_lidar(results)){
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_lidar->lrt);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_lidar->ldprt);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_lidar->lr);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_lidar->ldpar);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_lidar->ldper);
                            
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_lidar);
                        }
                        if(grasp_output_tile_products_aerosol_sd2m_mph(results)){
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_sd2m_mph);
                        }
                        if(grasp_output_tile_products_aerosol_sd2m_ext(results)){    
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_sd2m_ext->ext);
                            
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_sd2m_ext);
                        }
                        if(grasp_output_tile_products_aerosol_chem(results)){
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_chem->rh);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_chem->fwrt);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_chem->fslbl);
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_chem->vfract);

                            trackmem_free(results->tile_result_map[ipixel]->aerosol_chem);
                        }
                        if(grasp_output_tile_products_aerosol_pm(results)){
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_pm->pm);
                            
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_pm);
                        }
                        if(grasp_output_tile_products_aerosol_types(results)){
                            trackmem_free(results->tile_result_map[ipixel]->aerosol_types);
                        }
                        if(grasp_output_tile_products_gases_absorption(results)){
                            trackmem_free(results->tile_result_map[ipixel]->gases->absorption);
                            
                            trackmem_free(results->tile_result_map[ipixel]->gases);
                        }
                        
                        /////
                        
                        if(grasp_output_tile_products_surface_surf(results) || grasp_output_tile_products_surface_bhr_iso(results)){
                            if(grasp_output_tile_products_surface_surf(results)){
                                trackmem_free(results->tile_result_map[ipixel]->surface_surf->dhr);
                            }
                            if(grasp_output_tile_products_surface_bhr_iso(results)){
                                trackmem_free(results->tile_result_map[ipixel]->surface_surf->bhr_iso);
                            }
                            
                            trackmem_free(results->tile_result_map[ipixel]->surface_surf);
                        }
                        if(grasp_output_tile_products_errest_par(results)){
                            trackmem_free(results->tile_result_map[ipixel]->errest_par->ERRP);
                            trackmem_free(results->tile_result_map[ipixel]->errest_par->BIASP);
                            trackmem_free(results->tile_result_map[ipixel]->errest_par->TSTDP);
                            trackmem_free(results->tile_result_map[ipixel]->errest_par->sd_err);
                            
                            trackmem_free(results->tile_result_map[ipixel]->errest_par);
                        }
                        if(grasp_output_tile_products_errest_aerosol_opt(results)){
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->ERR_ext);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->BIAS_ext);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->ERR_extt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->BIAS_extt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->ERR_ssa);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->BIAS_ssa);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->ERR_ssat);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->BIAS_ssat);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->ERR_aext);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->BIAS_aext);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->ERR_aextt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->BIAS_aextt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->TSTD_ext);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->TSTD_extt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->TSTD_ssa);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->TSTD_ssat);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->TSTD_aext);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt->TSTD_aextt);
                            
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_opt);
                        }
                        if(grasp_output_tile_products_errest_aerosol_lidar(results)){
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_lidar->ERR_lr);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_lidar->BIAS_lr);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_lidar->ERR_lrt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_lidar->BIAS_lrt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_lidar->TSTD_lr);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_lidar->TSTD_lrt);
                            
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_lidar);
                        }
                        if(grasp_output_tile_products_errest_aerosol_mic(results)){
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_mic->BIAS_sdt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_mic->TSTD_sdt);
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_mic->TSTD_sdt);
                            
                            trackmem_free(results->tile_result_map[ipixel]->errest_aerosol_mic);
                        }
                        if(grasp_output_tile_products_forcing_bbflux(results)){
                            trackmem_free(results->tile_result_map[ipixel]->forcing_bbflux->bbufx0);
                            trackmem_free(results->tile_result_map[ipixel]->forcing_bbflux->bbdfx0);
                            trackmem_free(results->tile_result_map[ipixel]->forcing_bbflux->bbufxa);
                            trackmem_free(results->tile_result_map[ipixel]->forcing_bbflux->bbdfxa);
                            trackmem_free(results->tile_result_map[ipixel]->forcing_bbflux->hlv);
                            
                            trackmem_free(results->tile_result_map[ipixel]->forcing_bbflux);
                        }
                        if(grasp_output_tile_products_forcing_forcing(results)){
                            trackmem_free(results->tile_result_map[ipixel]->forcing_forcing->netforc);
                            trackmem_free(results->tile_result_map[ipixel]->forcing_forcing->forceff);
                            trackmem_free(results->tile_result_map[ipixel]->forcing_forcing->hlv);
                            
                            trackmem_free(results->tile_result_map[ipixel]->forcing_forcing);
                        }
                        
                    }
                }
            }
        }
        
        for (i = 0; i < tile_description->dimensions.segment_ncols*tile_description->dimensions.segment_nrows*tile_description->dimensions.segment_ntimes; i++) {
            if( results->segment_result[i].npixel > 0 ){
                if(results->segment_result[i].pixel_result!=NULL){
                    trackmem_free(results->segment_result[i].pixel_result);
                }
                if(grasp_output_tile_products_retrieval_fit(results)){
                    if(results->segment_fit!=NULL && results->segment_fit[i]!=NULL){
                        trackmem_free(results->segment_fit[i]);
                        trackmem_free(results->segment_original[i]);
                    } 
                }
            }
        }
        
        trackmem_free(results->tile_result_map);
        trackmem_free(results->segment_result);
        if(results->segment_fit!=NULL){
            trackmem_free(results->segment_fit);
            trackmem_free(results->segment_original);
        }   
    }
}



/*
if(grasp_output_tile_products_retrieval_res(results)){

}
if(grasp_output_tile_products_retrieval_par(results)){

}
if(grasp_output_tile_products_retrieval_fit(results)){

}
if(grasp_output_tile_products_aerosol_opt(results)){

}
if(grasp_output_tile_products_aerosol_rind(results)){

}
if(grasp_output_tile_products_aerosol_phmx(results)){

}
if(grasp_output_tile_products_aerosol_lidar(results)){

}
if(grasp_output_tile_products_aerosol_sd2m_mph(results)){

}
if(grasp_output_tile_products_aerosol_sd2m_ext(results)){

}
if(grasp_output_tile_products_aerosol_chem(results)){

}
if(grasp_output_tile_products_aerosol_pm(results)){

}
if(grasp_output_tile_products_aerosol_types(results)){

}
if(grasp_output_tile_products_clouds_opt(results)){

}
if(grasp_output_tile_products_clouds_rind(results)){

}
if(grasp_output_tile_products_clouds_phmx(results)){

}
if(grasp_output_tile_products_clouds_lidar(results)){

}
if(grasp_output_tile_products_clouds_sd2m_mph(results)){

}
if(grasp_output_tile_products_clouds_sd2m_ext(results)){

}
if(grasp_output_tile_products_clouds_chem(results)){

}
if(grasp_output_tile_products_clouds_pm(results)){

}
if(grasp_output_tile_products_clouds_types(results)){

}
if(grasp_output_tile_products_surface_surf(results)){

}
if(grasp_output_tile_products_errest_aerosol_opt(results)){

}
if(grasp_output_tile_products_errest_aerosol_lidar(results)){

}
if(grasp_output_tile_products_errest_aerosol_mic(results)){

}

if(grasp_output_tile_products_errest_clouds_opt(results)){

}
if(grasp_output_tile_products_errest_clouds_lidar(results)){

}
if(grasp_output_tile_products_errest_par(results)){

}
if(grasp_output_tile_products_forcing_bbflux(results)){

}
if(grasp_output_tile_products_forcing_forcing(results)){

}
*/
