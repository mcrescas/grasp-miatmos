/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include <string.h>
#include <stdlib.h>
#include "yamlsettings/yamlsettings_input_yaml.h"
#include "yamlsettings/yamlsettings_data_types.h"
#include <grasp/utils.h>
#include "math.h"
#include "yamlsettings/yamlsettings_dictionary.h"
#include "mod_par_inv.inc"
#include "grasp_settings.h"
#include "../global/grasp_retrieval_characteristic_type.h"

void grasp_settings_set_deducted_nwl(yamlsettings_dictionary_t *dictionary){
    grasp_settings *settings;
    
    settings=(grasp_settings *)dictionary->settings;
    
    settings->retrieval.NW=grasp_settings_deduct_nwl(dictionary);
}

void grasp_settings_calculate_ndim_part(yamlsettings_dictionary_t *dictionary){
    int idim1,idim2,idim3;
    int tmp, tmpf, nsd, index, sd_index;
    grasp_settings *settings;
    char error[512];
    
    settings=(grasp_settings *)dictionary->settings;    
    
    // Initialize NDIM part
    settings->retrieval.NDIM.n1=0;    
    for(idim1=0;idim1<_KIDIM1; idim1++){
        settings->retrieval.NDIM.n2[idim1]=0;
        for(idim2=0;idim2<_KIDIM2; idim2++){
            settings->retrieval.NDIM.n3[idim1][idim2]=0;
            settings->retrieval.NDIM.ISTARSING[idim1][idim2]=0;
        }
    }   
    // Initialize other arrays based in KPARS length
    for (index = 0; index < _KPARS; index++) {
        settings->retrieval.APSING[index]=0.0;
        settings->retrieval.APSMIN[index]=0.0;
        settings->retrieval.APSMAX[index]=0.0;
        settings->retrieval.APSERREST[index]=true;
        settings->retrieval.IWW_SINGL[index]=0;
    }
    
    // Finding maximums for NDIM
    for (idim1 = 0; idim1 < _KIDIM1; idim1++) {
        for (idim2 = 0; idim2 < _KIDIM2; idim2++) {
            for (idim3 = 0; idim3 < _KIDIM3; idim3++) {
                if(settings->tmp.TAPSING[idim1][idim2][idim3]>FLT_MIN){
                    settings->retrieval.NDIM.n3[idim1][idim2]++;
                }else{
                    break;
                }
            }
            if(settings->tmp.TAPSING[idim1][idim2][0]>FLT_MIN){
                settings->retrieval.NDIM.n2[idim1]++;
            }else{
                break;
            }
        }
        if(settings->tmp.TAPSING[idim1][0][0]>FLT_MIN){
            settings->retrieval.NDIM.n1++;
        }else{
            break;
        }
    }
    
    // Set other parameters
    tmp=1; 
    tmpf=1;
    nsd=0;   
    settings->retrieval.KNSING=0;
    index=0;
    for(idim1=0;idim1<settings->retrieval.NDIM.n1; idim1++){
        if(settings->retrieval.NDIM.n2[idim1]>_KIDIM2){
            sprintf(error,"Error allocating NDIM parameters. They are more than constants allows (KIDIM2[%d]=%d)",idim1,settings->retrieval.NDIM.n2[idim1]);
            yamlsettings_error_add_parse_error(&(dictionary->status), error, YAMLSETTINGS_FILE_UNKNOWN, YS_ERROR); 
            break;
        }   
        
        for(idim2=0; idim2<settings->retrieval.NDIM.n2[idim1]; idim2++){
            if(settings->retrieval.NDIM.n3[idim1][idim2]>_KIDIM3){
                sprintf(error,"Error allocating NDIM parameters. There are more values than constants allows (KIDIM3[%d][%d]=%d)",idim1,idim2,settings->retrieval.NDIM.n3[idim1][idim2]);
                yamlsettings_error_add_parse_error(&(dictionary->status), error, YAMLSETTINGS_FILE_UNKNOWN, YS_ERROR); 
                break;
            }    
            settings->retrieval.NDIM.ISTARSING[idim1][idim2]=tmp;
            tmp=tmp+settings->retrieval.NDIM.n3[idim1][idim2];
            if(settings->retrieval.NDIM.par_retr[idim1]==true){
                tmpf=tmpf+settings->retrieval.NDIM.n3[idim1][idim2];
            }
                
            if(idim1==0) nsd++;
            for (idim3 = 0; idim3 < settings->retrieval.NDIM.n3[idim1][idim2]; idim3++) {
                // Settings kndim parameters
                if(index>=_KPARS){
                    sprintf(error,"Error allocating NDIM parameters. The constant _KPARS is not big enough to allocate all parameters (index=%d; _KPARS=%d)",index, _KPARS);
                    yamlsettings_error_add_parse_error(&(dictionary->status), error, YAMLSETTINGS_FILE_UNKNOWN, YS_ERROR); 
                    break;
                }
                settings->retrieval.APSING[index]=settings->tmp.TAPSING[idim1][idim2][idim3];
                settings->retrieval.APSMIN[index]=settings->tmp.TAPSMIN[idim1][idim2][idim3];
                settings->retrieval.APSMAX[index]=settings->tmp.TAPSMAX[idim1][idim2][idim3];
                settings->retrieval.APSERREST[index]=settings->tmp.TAPSERREST[idim1][idim2][idim3];
                settings->retrieval.IWW_SINGL[index]=settings->tmp.TIWW_SINGL[idim1][idim2][idim3];
                index++;
            }            
        }
    }
    settings->retrieval.KNSING=tmp-1;
    settings->retrieval.KNSINGF=tmpf-1;
    sd_index=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_SD_beg, par_type_SD_end);
    if (sd_index>=0){
        settings->retrieval.NSD=grasp_parameters_number_of_modes_of_parameter(&settings->retrieval.NDIM, settings->retrieval.NDIM.par_type[sd_index]);
    }else{
        // It means there is an error in settings definition but here
        // we are not going to trigger any action. Other validator will
        // show the error
        settings->retrieval.NSD=1; 
    }
    
    // For transport models it is needed to copy NSD to ntrc
    if (settings->retrieval.use_tmodel==true){
        settings->retrieval.TMSET.ntrc=settings->retrieval.NSD;
    }
}

void grasp_settings_single_pixel_smoothness_constraints(yamlsettings_dictionary_t *dictionary){
    grasp_settings *settings;
    int i,j,k;
    
    settings=(grasp_settings *)dictionary->settings; 
    
    // settings%SPS%IO(:,:) = settings%SPCS%IO(:,:)
    for (i=0; i<_KIDIM1; i++){
        for (j=0; j<_KIDIM2; j++){
            settings->retrieval.SMS.IO[i][j] = settings->retrieval.SPCS.IO[i][j];
            
            // if ( settings%SPS%IO(i,j) = 0 ) { settings%SPS%EST(:,i,j) = 0.0; settings%SPS%COV(:,i,j) = 0.0}
            if (settings->retrieval.SMS.IO[i][j] == 0){
                for (k=0; k<_KIDIM3; k++) {
                    settings->retrieval.SMS.EST[i][j][k]=0;
                    settings->retrieval.SMS.WGT[i][j][k]=1;
                }
            }
        }
    }
}


void grasp_settings_calculate_iwl_and_key(yamlsettings_dictionary_t *dictionary){
    int idim1;
    grasp_settings *settings;
    
    settings=(grasp_settings *)dictionary->settings;
    
      for(idim1=0;idim1<settings->retrieval.NDIM.n1;idim1++){
        if(settings->retrieval.NDIM.par_type[idim1] > par_type_SD_beg && settings->retrieval.NDIM.par_type[idim1] < par_type_SD_end){      
          if(settings->retrieval.NDIM.par_type[idim1] == par_type_SD_TB) { // triangle  bins
            settings->retrieval.DLSF.IWL = 0;
            settings->retrieval.DLSF.key = 0;       
          }else if(settings->retrieval.NDIM.par_type[idim1] == par_type_SD_LB || settings->retrieval.NDIM.par_type[idim1] == par_type_SD_MD) { // precalculated lognormal bins      
            settings->retrieval.DLSF.IWL = 1;
            settings->retrieval.DLSF.key = 2;               
          }else if(settings->retrieval.NDIM.par_type[idim1] == par_type_SD_LN){ // parameters of bi-modal lognormal SD
            settings->retrieval.DLSF.IWL = 0;
            settings->retrieval.DLSF.key = 3;
          }
        }
    }    
}

void grasp_settings_input_method(yamlsettings_dictionary_t *dictionary){
    int center_latitude_index, center_longitude_index, corner_row_index, corner_col_index;
    
    center_latitude_index=yamlsettings_dictionary_find_parameter_by_name("input.center.latitude", dictionary);
    center_longitude_index=yamlsettings_dictionary_find_parameter_by_name("input.center.latitude", dictionary);
    corner_row_index=yamlsettings_dictionary_find_parameter_by_name("input.corner.row", dictionary);
    corner_col_index=yamlsettings_dictionary_find_parameter_by_name("input.corner.column", dictionary);
    
    assert(center_latitude_index>=0);
    assert(center_longitude_index>=0);
    assert(corner_row_index>=0);
    assert(corner_col_index>=0);
    
    if(yamlsettings_parameter_is_unset(center_latitude_index, dictionary)==false && yamlsettings_parameter_is_unset(center_longitude_index, dictionary)==false){
        strcpy(((grasp_settings *)dictionary->settings)->input.coordinates_refence,"center");
        strcpy(((grasp_settings *)dictionary->settings)->input.coordinates_type,"latlon");
    }
    if(yamlsettings_parameter_is_unset(corner_row_index, dictionary)==false && yamlsettings_parameter_is_unset(corner_col_index, dictionary)==false){
        strcpy(((grasp_settings *)dictionary->settings)->input.coordinates_refence,"corner");
        strcpy(((grasp_settings *)dictionary->settings)->input.coordinates_type,"rowcol");
    }        
}


//void grasp_settings_simulated_sdata(yamlsettings_dictionary_t *dictionary){
//    int sim;
//
//    sim=yamlsettings_dictionary_find_parameter_by_name("retrieval.debug.simulated_sdata_file", dictionary);        
//    assert(sim>=0);
//
//    if(yamlsettings_parameter_is_unset(sim, dictionary)==false){ // If sim sdata is activated (set, no default value), istop is set to true 
//        ((grasp_settings *)(dictionary->settings))->retrieval.ISTOP=true;
//    }else{ // If the parameter is not set, it is set to empty  string
//        strcpy(((grasp_settings *)(dictionary->settings))->retrieval.sdata_sim_file, "");
//    }
//}

void grasp_settings_controller_perform_retrieval(yamlsettings_dictionary_t *dictionary){
    grasp_settings *settings;
    
    settings=(grasp_settings *)dictionary->settings; 
    if(settings->controller.perform_retrieval==false){
        settings->output.ncurrent_output_function=0;
        settings->output.nsegment_output_function=0;
        settings->output.ntile_output_function=0;
    }
}

void grasp_settings_default_arrays_from_one_to_max_wl(yamlsettings_dictionary_t *dictionary){
    int iwl,iparam;
    grasp_settings *settings;
    
    settings=(grasp_settings *)dictionary->settings; 
    
    // for aod
    iparam=yamlsettings_dictionary_find_parameter_by_name("retrieval.products.configuration.wavelenght_indices_for_aod_error_estimation", dictionary);
    if(yamlsettings_parameter_is_unset(iparam, dictionary)){
        settings->retrieval.naod_errest_iwl=grasp_settings_deduct_nwl(dictionary);
        for (iwl = 0; iwl < settings->retrieval.naod_errest_iwl; iwl++) {
            settings->retrieval.aod_errest_iwl[iwl]=iwl+1;
        } 
    }
    
    // for ssa
    iparam=yamlsettings_dictionary_find_parameter_by_name("retrieval.products.configuration.wavelenght_indices_for_ssa_error_estimation", dictionary);
    if(yamlsettings_parameter_is_unset(iparam, dictionary)){
        settings->retrieval.nssa_errest_iwl=grasp_settings_deduct_nwl(dictionary);
        for (iwl = 0; iwl < settings->retrieval.nssa_errest_iwl; iwl++) {
            settings->retrieval.ssa_errest_iwl[iwl]=iwl+1;
        } 
    }
    
    // MEH: for aext
    iparam=yamlsettings_dictionary_find_parameter_by_name("retrieval.products.configuration.wavelenght_indices_for_aext_error_estimation", dictionary);
    if(yamlsettings_parameter_is_unset(iparam, dictionary)){
        settings->retrieval.naext_errest_iwl=grasp_settings_deduct_nwl(dictionary);
        for (iwl = 0; iwl < settings->retrieval.naext_errest_iwl; iwl++) {
            settings->retrieval.aext_errest_iwl[iwl]=iwl+1;
        }
    }
    
    // for lidar
    iparam=yamlsettings_dictionary_find_parameter_by_name("retrieval.products.configuration.wavelenght_indices_for_lidar_error_estimation", dictionary);
    if(yamlsettings_parameter_is_unset(iparam, dictionary)){
        settings->retrieval.nlidar_errest_iwl=grasp_settings_deduct_nwl(dictionary);
        for (iwl = 0; iwl < settings->retrieval.nlidar_errest_iwl; iwl++) {
            settings->retrieval.lidar_errest_iwl[iwl]=iwl+1;
        } 
    }
}


