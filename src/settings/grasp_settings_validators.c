/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include <dirent.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "grasp_settings_validators.h"
#include <grasp/utils.h>
#include "yamlsettings/yamlsettings_dictionary.h"
#include "grasp_settings.h"
#include "../output/grasp_output_stream.h"
#include "../global/grasp_compilation_information.h"
#include "../global/grasp_retrieval_characteristic_type.h"
#include "../global/grasp_retrieval_meas_type.h"

int grasp_settings_validator_directory_fortran(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    char *path,*value;
    DIR *dir;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    
    // Retrieve parameter
    param=&(dictionary->parameters[param_index]);    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){ // it is set
        // Get pointer to first char
        value=(char *)param->mem_pos;
        // Convert string to c string
        path=fstr2c(value,param->maxlength);
        
        dir= opendir(path);

        if (dir) { //Directory exists.
            closedir(dir);
        } else {
            sprintf(error,"%s is not a existing directory in %s", path,param->name);
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
            nerror--;
        } 

        trackmem_free(path);
    }
    
    return nerror;
}

int graspsettings_validator_indexes_of_wavelengths(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    int *value,min,max,i;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    param=&(dictionary->parameters[param_index]);
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        value=(int *)param->mem_pos;
        min=atoi(arguments[0]);
        max=grasp_settings_deduct_nwl(dictionary);
        
        // Now we check all values read from settings
        for(i=0;i<yamlsettings_parameter_number_of_elements(param);i++){
            if(!(value[i]>=min && value[i]<=max)){
                sprintf(error,"%s have to be defined like a value between 1 and maximum of wavelengths defined (%d) (value read: %d)",param->name, max,value[i]);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), error); 
                nerror--;
            }
        }
    }
    
    return nerror;
}

int graspsettings_validator_all_wavelengths_defined(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    int value1,nwl,index2,i,indexes[YAMLSETTINGS_MAX_PARAMETER_DIMENSIONS];
    yamlsettings_parameter *param;
    char error[2048];  
    int nerror=0;
    grasp_settings *settings; 
    
    settings=(grasp_settings *)dictionary->settings; 
    
    param=&(dictionary->parameters[param_index]);    
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        value1 = yamlsettings_parameter_nelements_set(param_index, dictionary);
        nwl = settings->retrieval.NW;
        
        if(value1>0 && value1!=nwl){
            sprintf(error,"%s (%d elements) must have the same number of elements than the wavelength in the settings (%d elements)."
                    ,param->name,value1/**(param->counter_mem_pos)*/, 
                    nwl /**(dictionary->parameters[index2].counter_mem_pos)*/);
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), error); 
            nerror--;
        }
    }
    
    return nerror;    
}


int graspsettings_validator_cut_off(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    int i,j;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    grasp_settings *settings; 
    settings=(grasp_settings *)dictionary->settings; 
    bool found;
    
    param=&(dictionary->parameters[param_index]);
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==true){
        found = false;
        
        for(i=0; i<_KKNOISE; i++){
            for(j=0;j<_KIP;j++){
                if (settings->retrieval.NOISE.MT[i][j] >= MEAS_TYPE_P11_intd_cut_off_1 && settings->retrieval.NOISE.MT[i][j] <= MEAS_TYPE_P11_intd_cut_off_4) {
                    found = true;
                }
            }
        }
        if(found==true){
            sprintf(error,"In noise section there are measurements of type 'cut-off' but retrieval.inversion.measurement_fitting.integrated_p11_measurements_cut_off is missing");
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), error); 
            nerror--;
        }
    }
    
    return nerror;
}

int grasp_settings_validator_stream(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    char *value;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    int i,j;
    DIR *dir;
    char *path;
    bool validable_path;

    param=&(dictionary->parameters[param_index]);  
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        for(i=0;i<yamlsettings_parameter_number_of_elements(param);i++){
            value=(char *)param->mem_pos;
            value=value+(param->maxlength*i); // move the pointer to current position (it could be an array)
            
            // Checking if the stream is wll formed
            if(grasp_output_stream_filename_validation(value)==false){
                sprintf(error,"%s is not a well-formed stream. Please, check documentation (value: %s)",param->name,value);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--;
            }
            
            // Checking if the directory exist
            if(strcmp(value,"screen")!=0){                
                path=pathoffile(value);
                
                // I can only check the path if it does not contain wildcards in the path part (excluding filename) and if is not an empty string (relative path no explicit)
                validable_path=true;
                for (j = 0; j < strlen(path); j++) {
                    if(path[j]=='{' || path[j]=='}'){
                        validable_path=false;
                        break;
                    }
                }
                if(strlen(path)==0){
                    validable_path=false;
                }

                if(validable_path==true){ 
                    dir= opendir(path);

                    if (dir) { //Directory exists.
                        closedir(dir);
                    } else {
                        sprintf(error,"%s=%s must be an existing directory. %s does not exist", param->name,value, path);
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;
                    } 
                }
                
                trackmem_free(path);
            }
            
        }
    }        
    
    return nerror;
}

int grasp_settings_validator_input_driver(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    char *value;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    int i;

    char *rest, *driver, *ptr;
    char *drivers;
    char *drivers_for_print;
    int found=0;

    //drivers=grasp_compilation_information_input_drivers();
    drivers_for_print=grasp_compilation_information_input_drivers();
    drivers = (char *) trackmem_malloc(sizeof (char)*(strlen(drivers_for_print)+1));   
    assert( drivers!= NULL);
    param=&(dictionary->parameters[param_index]);  
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        for(i=0;i<yamlsettings_parameter_number_of_elements(param);i++){
            strcpy(drivers, drivers_for_print);
            value=(char *)param->mem_pos;
            value=value+(param->maxlength*i); // move the pointer to current position (it could be an array)
            
            ptr=drivers;
            found=0;
            while((driver = strtok_r(ptr, " ", &rest))) {
                if(strcmp(driver, value)==0){
                    found=1;
                    break;
                }
                ptr = rest;
            }
            
            if(found==0){
                sprintf(error,"Driver %s does not exist. The system is compiled with following drivers: %s)",value,drivers_for_print);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--;
            }
        }
    }        
    
    trackmem_free(drivers);
    trackmem_free(drivers_for_print);
    
    return nerror;
}

int graspsettings_validator_divisible(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    int *value1,*value2,index2;
    yamlsettings_parameter *param;
    char error[2048];  
    int nerror=0;
    
    param=&(dictionary->parameters[param_index]);    
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        value1=(int *)param->mem_pos;
        index2=yamlsettings_dictionary_find_parameter_by_name(arguments[0],dictionary);
        assert(index2>=0);
        value2=dictionary->parameters[index2].mem_pos;
        
        if(*value1!=0 && *value2!=0 && *value1%*value2!=0){
            sprintf(error,"%s=%d have to be a number divisible by %s=%d"
                    ,param->name, *value1 
                    ,dictionary->parameters[index2].name, *value2);
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
            nerror--;
        }
              
    }
    
    return nerror;    
}

int grasp_settings_validator_simulated_file(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    int index2;
    char *value1;
    bool *value2;
    yamlsettings_parameter *param;
    char error[2048];  
    int nerror=0;
    
    param=&(dictionary->parameters[param_index]);    
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        value1=(char *)param->mem_pos;
        index2=yamlsettings_dictionary_find_parameter_by_name("retrieval.products.retrieval.fitting",dictionary);
        assert(index2>=0);
        value2=(bool *)dictionary->parameters[index2].mem_pos;
        if(strcmp(value1, "none") != 0 || strcmp(value1, "null") != 0 || strcmp(value1, "false") != 0 || strcmp(value1, "f") != 0 || strcmp(value1, "0") == 0){
            if(*value2==false){
                sprintf(error,"It is not possible to dump simulated data if retrieval.products.retrieval.fitting=false. Please, set output.sdata.simulated_file to 'none' or retrieval.products.retrieval.fitting=true");
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--;
            }
        }
              
    }
    
    return nerror;    
}

int graspsettings_validator_bins (int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    bool isset_bins, isset_min, isset_max;
    int index,i;
    int nerror=0;
    
    isset_bins=!yamlsettings_parameter_is_unset(param_index,dictionary);    
    
    index=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].min",dictionary);
    assert(index>=0);
    isset_min=!yamlsettings_parameter_is_unset(index,dictionary); 
    
    index=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].max",dictionary);
    assert(index>=0);
    isset_max=!yamlsettings_parameter_is_unset(index,dictionary); 
    
    if(!isset_bins && !isset_min && !isset_max){
        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), 
                        "The bins have to be defined mandatorily using retrieval.forward_model.phase_matrix.radius.mode[].bins or retrieval.forward_model.phase_matrix.radius.mode[].min and retrieval.forward_model.phase_matrix.radius.mode[].max"); 
        nerror--;     
    }else{
        if(isset_bins && (isset_min || isset_max)){
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), 
                        "You can not define bins in modes by retrieval.forward_model.phase_matrix.radius.mode[].bins and retrieval.forward_model.phase_matrix.radius.mode[].min and retrieval.forward_model.phase_matrix.radius.mode[].max at same time. Only one way is accepted"); 
                nerror--;        
        }else{    
            if(isset_min && !isset_max){
                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), 
                            "If you define the bins using retrieval.forward_model.phase_matrix.radius.mode[].min also retrieval.forward_model.phase_matrix.radius.mode[].max is mandatory"); 
                    nerror--;             
            }
            if(isset_max && !isset_min){
                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), 
                            "If you define the bins using retrieval.forward_model.phase_matrix.radius.mode[].max also retrieval.forward_model.phase_matrix.radius.mode[].min is mandatory"); 
                    nerror--;             
            }    
        }
    }
    
    grasp_settings *settings; 
    settings=(grasp_settings *)dictionary->settings;  
    // The number of bins of the SD has to be equal to the number of bins of noises
    // We only check if no other errors
    
    index=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_SD_beg, par_type_SD_end);
    if (nerror == 0){
        if (index>=0){
            if (isset_bins) { 
                i=0; while (i<_KSD && settings->retrieval.NBIN[i] != 0){ i++;  } // Find NBINS            
                if (i != settings->retrieval.NDIM.n2[index]){
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), 
                                "The number of modes defined in retrieval.forward_model.phase_matrix.radius.mode[] has to be the same than the number of modes in size distribution"); 
                        nerror--;   
                }
            }else{
                if (settings->tmp.NRMIN != settings->retrieval.NDIM.n2[index]){
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), 
                                "The number of modes defined in retrieval.forward_model.phase_matrix.radius.mode[] has to be the same than the number of modes in size distribution"); 
                        nerror--; 
                }
            }
        }
    }
        
//    grasp_settings *settings; 
//    settings=(grasp_settings *)dictionary->settings;  
//    if (settings->retrieval.NBIN != settings->retrieval.NDIM.n2[0]){
//                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), 
//                            "xxx"); 
//                    nerror--;    
//    }

    return nerror;
}

int grasp_settings_validator_output_segment_function(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    char *value;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    int i;

    char *rest, *function, *ptr;
    char *functions;
    char *functions_for_print;
    int found=0;

    functions_for_print=grasp_compilation_information_output_segment_functions();
    functions = (char *) trackmem_malloc(sizeof (char)*(strlen(functions_for_print)+1));
    assert( functions!= NULL);
    param=&(dictionary->parameters[param_index]);  
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        for(i=0;i<yamlsettings_parameter_number_of_elements(param);i++){
            strcpy(functions,functions_for_print);
            value=(char *)param->mem_pos;
            value=value+(param->maxlength*i); // move the pointer to current position (it could be an array)
            
            ptr=functions;
            found=0;
            while((function = strtok_r(ptr, " ", &rest))) {
                if(strcmp(function, value)==0){
                    found=1;
                    break;
                }
                ptr = rest;
            }

            if(found==0){
                sprintf(error,"Output segment function %s does not exist. The system is compiled with following functions: %s",value,functions_for_print);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--;
            }
        }
    }        
    
    trackmem_free(functions);
    trackmem_free(functions_for_print);
    
    return nerror;
}

int grasp_settings_validator_output_tile_function(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    char *value;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    int i;

    char *rest, *function, *ptr;
    char *functions;
    char *functions_for_print;
    int found=0;

    functions_for_print=grasp_compilation_information_output_tile_functions();
    functions = (char *) trackmem_malloc(sizeof (char)*(strlen(functions_for_print)+1));
    assert( functions!= NULL);
    param=&(dictionary->parameters[param_index]);  
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        for(i=0;i<yamlsettings_parameter_number_of_elements(param);i++){
            strcpy(functions,functions_for_print);
            value=(char *)param->mem_pos;
            value=value+(param->maxlength*i); // move the pointer to current position (it could be an array)
            
            ptr=functions;
            found=0;
            while((function = strtok_r(ptr, " ", &rest))) {
                if(strcmp(function, value)==0){
                    found=1;
                    break;
                }
                ptr = rest;
            }

            if(found==0){
                sprintf(error,"Output tile function %s does not exist. The system is compiled with following functions: %s",value,functions_for_print);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--;
            }
        }
    }        
    
    trackmem_free(functions);
    trackmem_free(functions_for_print);
    
    return nerror;
}

int grasp_settings_validator_output_current_function(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    char *value;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    int i;

    char *rest, *function, *ptr;
    char *functions;
    char *functions_for_print;
    int found=0;

    functions_for_print=grasp_compilation_information_output_current_functions();
    functions = (char *) trackmem_malloc(sizeof (char)*(strlen(functions_for_print)+1));
    assert( functions!= NULL);
    param=&(dictionary->parameters[param_index]);  
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        for(i=0;i<yamlsettings_parameter_number_of_elements(param);i++){
            strcpy(functions,functions_for_print);
            value=(char *)param->mem_pos;
            value=value+(param->maxlength*i); // move the pointer to current position (it could be an array)
            
            ptr=functions;
            found=0;
            while((function = strtok_r(ptr, " ", &rest))) {
                if(strcmp(function, value)==0){
                    found=1;
                    break;
                }
                ptr = rest;
            }

            if(found==0){
                sprintf(error,"Output current function %s does not exist. The system is compiled with following functions: %s",value,functions_for_print);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--;
            }
        }
    }        
    
    trackmem_free(functions);
    trackmem_free(functions_for_print);
    
    return nerror;
}


int grasp_settings_validator_input_transformer(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    char *value;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    int i;

    char *rest, *function, *ptr;
    char *transformers;
    char *transformers_for_print;
    int found=0;

    transformers_for_print=grasp_compilation_information_input_transformers();
    transformers = (char *) trackmem_malloc(sizeof (char)*(strlen(transformers_for_print)+1));
    assert( transformers!= NULL);
    param=&(dictionary->parameters[param_index]);  
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        for(i=0;i<yamlsettings_parameter_number_of_elements(param);i++){
            strcpy(transformers,transformers_for_print);
            value=(char *)param->mem_pos;
            value=value+(param->maxlength*i); // move the pointer to current position (it could be an array)
            
            ptr=transformers;
            found=0;
            while((function = strtok_r(ptr, " ", &rest))) {
                if(strcmp(function, value)==0){
                    found=1;
                    break;
                }
                ptr = rest;
            }

            if(found==0){
                sprintf(error,"Input transformer %s does not exist. The system is compiled with following transformers: %s)",value,transformers_for_print);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--;
            }
        }
    }        
    
    trackmem_free(transformers);
    trackmem_free(transformers_for_print);
    
    return nerror;
}


int graspsettings_validator_characteristic_retrieved(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    int i;
    bool *value;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    char *unset=dictionary->files[YAMLSETTINGS_FILE_UNSET];

    param=&(dictionary->parameters[param_index]);
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        value=(bool *)param->mem_pos;
        for(i=0;i<yamlsettings_parameter_number_of_elements(param)-1;i++){
            if(value[i]==false && value[i+1]==true && param->settings_file[i]!=unset && param->settings_file[i+1]!=unset){
                sprintf(error,"%s . All not retrieved characteristics have to be at the end. Problem found in characteristic %d",param->name,i+1);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--;
                break;
            }
        }
    }
    
    return nerror;    
}

int graspsettings_validator_characteristic_type(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    int icharacteristic,imode,ipos;
    int icharacteristic2;
    int *value;
    yamlsettings_parameter *param;
    char error[2048];
    char *tmp;
    int nerror=0;
    char *unset=dictionary->files[YAMLSETTINGS_FILE_UNSET];
    int index_required, char_type;
    grasp_settings *settings;
    int number_of_modes;
    int number_of_element_per_mode; // this value should be the same for all modes.
    int indexes[YAMLSETTINGS_MAX_PARAMETER_DIMENSIONS];
    settings=(grasp_settings *)dictionary->settings;
    bool SD_MD=false;
    int ngases=0;
    
    param=&(dictionary->parameters[param_index]);
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        value=(int *)param->mem_pos;
        for(icharacteristic=0;icharacteristic<yamlsettings_parameter_number_of_elements(param);icharacteristic++){
            if(param->settings_file[icharacteristic]!=unset){ // If the parameter is set
                for(imode=0;imode<icharacteristic;imode++){ // Checking if there is another characteristic defined of the same group
                    if(param->settings_file[icharacteristic]!=unset && param->settings_file[imode]!=unset){ // If the parameter is set                
                        if(abs(value[icharacteristic]-value[imode])<10){ //Two defined characteristics in same block
                            sprintf(error,"In %s parameter. Cannot be defined in a retrieval two characteristics of same kind of characteristics %d and %d are incompatible",param->name,icharacteristic+1,imode+1);
                            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                            nerror--;
                            break;
                        }
                    }
                }
                
                // Values have to be between minimum and maximum
                number_of_modes=settings->retrieval.NDIM.n2[icharacteristic];
                for (imode = 0; imode < number_of_modes; imode++) {
                    number_of_element_per_mode=settings->retrieval.NDIM.n3[icharacteristic][imode];
                    for (ipos = 0; ipos < number_of_element_per_mode; ipos++) {
                        if(settings->retrieval.APSING[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]<settings->retrieval.APSMIN[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]){
                            indexes[0]=icharacteristic;
                            indexes[1]=imode;
                            indexes[2]=ipos;
                            tmp=yamlsettings_data_type_get_string(param->data_type_get, param->mem_pos, indexND(param->dimensions.ndimensions, indexes ,param->dimensions.dimension_size), param->maxlength, false);
                            sprintf(error,"The minimum value of characteristic type %s has to be bigger than the value of initial guess (characteristic[%d], mode[%d], position[%d]: %f has to be bigger than %f)",
                                    tmp,
                                    icharacteristic+1,imode+1,ipos+1,settings->retrieval.APSING[settings->retrieval.NDIM.ISTARSING[icharacteristic][0]+ipos-1], settings->retrieval.APSMIN[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]);
                            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                            free(tmp);
                            nerror--;  
                        }
                        if(settings->retrieval.APSMAX[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]<settings->retrieval.APSING[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]){
                            indexes[0]=icharacteristic;
                            indexes[1]=imode;
                            indexes[2]=ipos;
                            tmp=yamlsettings_data_type_get_string(param->data_type_get, param->mem_pos, indexND(param->dimensions.ndimensions, indexes ,param->dimensions.dimension_size), param->maxlength, false);
                            sprintf(error,"The maximum value of characteristic type %s has to be bigger than the value of initial guess (characteristic[%d], mode[%d], position[%d]: %f has to be lower than %f)",
                                    tmp,
                                    icharacteristic+1,imode+1,ipos+1,settings->retrieval.APSING[settings->retrieval.NDIM.ISTARSING[icharacteristic][0]+ipos-1], settings->retrieval.APSMAX[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]);
                            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                            free(tmp);
                            nerror--;  
                        }                        
                    }
                }
                
                // Checking requirements by type
                if(value[icharacteristic]==par_type_SD_LB){
                    index_required=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].bins",dictionary);
                    
                    if(yamlsettings_parameter_is_unset(index_required,dictionary)==true){
                        sprintf(error,"In %s parameter. size_distribution_precalculated_lognormal require set retrieval.forward_model.phase_matrix.radius.mode[].bins",param->name);
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;                        
                    }
                }
                
                if(value[icharacteristic]==par_type_SD_LN){          
                    
                    index_required=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].min",dictionary);   
                    if(yamlsettings_parameter_is_unset(index_required,dictionary)==true){
                        sprintf(error,"In %s parameter. size_distribution_lognormal require set retrieval.forward_model.phase_matrix.radius.mode[].min",param->name);
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;                        
                    }                    
                    
                    index_required=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].max",dictionary);
                    if(yamlsettings_parameter_is_unset(index_required,dictionary)==true){
                        sprintf(error,"In %s parameter. size_distribution_lognormal require set retrieval.forward_model.phase_matrix.radius.mode[].max",param->name);
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;                        
                    }     
                    
                    number_of_modes=settings->retrieval.NDIM.n2[icharacteristic];
                    for (imode = 0; imode < number_of_modes; imode++) {
                        number_of_element_per_mode=settings->retrieval.NDIM.n3[icharacteristic][imode];
                        if(number_of_element_per_mode!=2){
                            sprintf(error,"Characteristic type size_distribution_lognormal has to be described by two elements (%d read)", number_of_element_per_mode);
                            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                            nerror--;                                   
                        }
                    }
                    
                    icharacteristic2=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_Cv_beg, par_type_Cv_end);
                    if (icharacteristic2>=0){
                        // If SD_LN co2ncentration has to be provided
                    }else{
                        sprintf(error,"If size distribution characteristic is lognormal (size_distribution_lognormal) concentration characteristic have to be provided in settings");
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;  
                    }
                }
                
                if(value[icharacteristic]>par_type_gas_concentration_beg && value[icharacteristic]<par_type_gas_concentration_end){ 
                    ngases++; // counting gases
                }
                
                if(value[icharacteristic]>par_type_gas_concentration_1 && value[icharacteristic]<par_type_gas_concentration_end){          
                    indexes[0]=icharacteristic;
                    indexes[1]=imode;
                    indexes[2]=ipos;
                    icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, value[icharacteristic]-100);
                    if (icharacteristic2<0){
                        tmp=yamlsettings_data_type_get_string(param->data_type_get, param->mem_pos, indexND(param->dimensions.ndimensions, indexes ,param->dimensions.dimension_size), param->maxlength, false);
                        sprintf(error,"If gas_concentration_X is defined, gas_concentration_X-1 must be present too. %s was found in characteristic number %d but not the previous gas", tmp, icharacteristic+1);
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        free(tmp);
                        nerror--;  
                    }
                }   
                
                if(value[icharacteristic]==par_type_SD_TB){
                    index_required=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].min",dictionary);
                            
                    if(yamlsettings_parameter_is_unset(index_required,dictionary)==true){
                        sprintf(error,"In %s parameter. size_distribution_triangle_bins require set retrieval.forward_model.phase_matrix.radius.mode[].min",param->name);
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;                        
                    }
                    
                    index_required=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].max",dictionary);
                    if(yamlsettings_parameter_is_unset(index_required,dictionary)==true){
                        sprintf(error,"In %s parameter. size_distribution_triangle_bins require set retrieval.forward_model.phase_matrix.radius.mode[].max",param->name);
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;                        
                    }   
                    
                } 
                
                if(value[icharacteristic]==par_type_SD_MD){
                    if(grasp_compilation_information_models_present()==false){
                        sprintf(error, "GRASP models module is not compiled. Compile it or another kind of size distribution model");
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error);         
                        nerror--;  
                    }
                    // I enable de flag to check later special conditions of models approach.
                    SD_MD=true;
                }
                
                if(value[icharacteristic] == par_type_CXRI_chem){
                    icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_RH);
                    if(icharacteristic2<0){
                        sprintf(error,"If particle_component_fractions_chemical_mixture is present, relative_humidity characteristic has to be present too");
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--; 
                    }
                }
                if(value[icharacteristic] == par_type_RH){
                    icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_CXRI_chem);
                    if(icharacteristic2<0){
                        sprintf(error,"If relative_humidity is present, particle_component_fractions_chemical_mixture characteristic has to be present too");
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--; 
                    }
                }
                
//                // For N-1 bins. If concentration is present the number of elements in par_type_SD_LB and par_type_SD_MD has to be N-1
                
                if(value[icharacteristic]==par_type_Cv){
                    if (settings->retrieval.indep_par==true){
                        // If radius are explicitly defined
                        index_required=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].bins",dictionary);
                        assert(index_required>=0);
                        if(yamlsettings_parameter_is_unset(index_required,dictionary)==false){
                            
                            icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_SD_LB);
                            if (icharacteristic2<0){
                                icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_SD_MD);
                            }
                            
                            if (icharacteristic2>=0){
                                for(imode = 0; imode<settings->retrieval.NDIM.n2[icharacteristic2]; imode++){
                                    if(settings->retrieval.NDIM.n3[icharacteristic2][imode]!=settings->retrieval.NBIN[imode]-1){
                                        sprintf(error,"If concentration is present and independent_concentration_and_shape is true the number of bins of SD has to be retrieval.forward_model.phase_matrix.radius.mode[].bins - 1 (bins of SD=%d; number of radius=%d; mode=%d)",settings->retrieval.NDIM.n3[icharacteristic2][imode], settings->retrieval.NBIN[imode], imode );
                                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                        nerror--; 
                                    }
                                    if(settings->retrieval.NDIM.n3[icharacteristic2][imode]<2){
                                        sprintf(error,"If concentration is present and independent_concentration_and_shape is true at least 2 values are needed in the SD (bins of SD=%d; number of radius=%d; mode=%d)",settings->retrieval.NDIM.n3[icharacteristic2][imode], settings->retrieval.NBIN[imode], imode );
                                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                        nerror--; 
                                    }
                                }
                            }
                            icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_SHD_distr);
                            if (icharacteristic2>0){
                                for(imode = 0; imode<settings->retrieval.NDIM.n2[icharacteristic2]-1; imode++){
                                    if(settings->retrieval.NDIM.n3[icharacteristic2][imode]!=settings->retrieval.NBIN[imode]-1){
                                        sprintf(error,"If concentration is present and independent_concentration_and_shape is true the number of bins of aspect_ratio_distribution has to be retrieval.forward_model.phase_matrix.radius.mode[].bins - 1 (bins of aspect_ratio_distribution=%d; number of radius=%d; mode=%d)",settings->retrieval.NDIM.n3[icharacteristic2][imode], settings->retrieval.NBIN[imode], imode );
                                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                        nerror--; 
                                    }
                                    if(settings->retrieval.NDIM.n3[icharacteristic2][imode]<2){
                                        sprintf(error,"If concentration is present and independent_concentration_and_shape is true at least 2 values are needed in the aspect_ratio_distribution (bins of aspect_ratio_distribution=%d; number of radius=%d; mode=%d)",settings->retrieval.NDIM.n3[icharacteristic2][imode], settings->retrieval.NBIN[imode], imode );
                                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                        nerror--; 
                                    }
                                }
                            }
                        }
                    }else{
                        // If radius are explicitly defined
                        index_required=yamlsettings_dictionary_find_parameter_by_name("retrieval.forward_model.phase_matrix.radius.mode[].bins",dictionary);
                        assert(index_required>=0);
                        if(yamlsettings_parameter_is_unset(index_required,dictionary)==false){
                            
                            icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_SD_LB);
                            if (icharacteristic2<0){
                                icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_SD_MD);
                            }
                            
                            if (icharacteristic2>=0){
                                for(imode = 0; imode<settings->retrieval.NDIM.n2[icharacteristic2]; imode++){
                                    if(settings->retrieval.NDIM.n3[icharacteristic2][imode]!=settings->retrieval.NBIN[imode]){
                                        sprintf(error,"If concentration is present and independent_concentration_and_shape is false the number of bins of SD has to be retrieval.forward_model.phase_matrix.radius.mode[].bins (bins of SD=%d; number of radius=%d; mode=%d)",settings->retrieval.NDIM.n3[icharacteristic2][imode], settings->retrieval.NBIN[imode], imode );
                                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                        nerror--; 
                                    }
                                }
                            }
                            icharacteristic2=grasp_parameters_index_of_parameter_type(&settings->retrieval.NDIM, par_type_SHD_distr);
                            if (icharacteristic2>0){
                                for(imode = 0; imode<settings->retrieval.NDIM.n2[icharacteristic2]; imode++){
                                    if(settings->retrieval.NDIM.n3[icharacteristic2][imode]-1!=settings->retrieval.NBIN[imode]){
                                        sprintf(error,"If concentration is present and independent_concentration_and_shape is false the number of bins of aspect_ratio_distribution has to be retrieval.forward_model.phase_matrix.radius.mode[].bins (bins of SD=%d; number of radius=%d; mode=%d)",settings->retrieval.NDIM.n3[icharacteristic2][imode], settings->retrieval.NBIN[imode], imode );
                                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                        nerror--; 
                                    }
                                }
                            }
                        }
                    }
                }
                
                
                if(value[icharacteristic]==par_type_AVP_prof){ // All modes have to have same values
                    number_of_modes=settings->retrieval.NDIM.n2[icharacteristic];
                    number_of_element_per_mode=settings->retrieval.NDIM.n3[icharacteristic][0];
                    for (imode = 1; imode < number_of_modes; imode++) {
                        if(settings->retrieval.NDIM.n3[icharacteristic][imode]!=number_of_element_per_mode){
                            sprintf(error,"Error analyzing vertical_profile_normalized characteristic. All modes has to have same number of elements but mode 1 has %d elements and mode %d has %d elements", number_of_element_per_mode, imode+1,settings->retrieval.NDIM.n3[icharacteristic][imode]);
                            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                            nerror--;                             
                        }else{                      
                            for (ipos = 0; ipos < number_of_element_per_mode; ipos++) {
                                if(settings->retrieval.APSMIN[settings->retrieval.NDIM.ISTARSING[icharacteristic][0]+ipos-1]!=settings->retrieval.APSMIN[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]){
                                    sprintf(error,"Error analyzing vertical_profile_normalized characteristic because all modes have to have same minimum value for initial guess but element %d is different in mode %d (it has the value %f in mode 1 and %f in mode %d)", ipos+1, imode+1, settings->retrieval.APSMIN[settings->retrieval.NDIM.ISTARSING[icharacteristic][0]+ipos-1], settings->retrieval.APSMIN[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1], ipos+1);
                                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                    nerror--;                                   
                                }
                            }
                            for (ipos = 0; ipos < number_of_element_per_mode; ipos++) {
                                if(settings->retrieval.APSMAX[settings->retrieval.NDIM.ISTARSING[icharacteristic][0]+ipos-1]!=settings->retrieval.APSMAX[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]){
                                    sprintf(error,"Error analyzing vertical_profile_normalized characteristic because all modes have to have same maximum value for initial guess but element %d is different in mode %d (it has the value %f in mode 1 and %f in mode %d)", ipos+1, imode+1, settings->retrieval.APSMAX[settings->retrieval.NDIM.ISTARSING[icharacteristic][0]+ipos-1], settings->retrieval.APSMAX[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1], ipos+1);
                                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                    nerror--;                                   
                                }
                            }
                            for (ipos = 0; ipos < number_of_element_per_mode; ipos++) {
                                if(settings->retrieval.IWW_SINGL[settings->retrieval.NDIM.ISTARSING[icharacteristic][0]+ipos-1]!=settings->retrieval.IWW_SINGL[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]){
                                    sprintf(error,"Error analyzing vertical_profile_normalized characteristic because all modes have to have same wavelength involved value for initial guess but element %d is different in mode %d (it has the value %d in mode 1 and %d in mode %d)", ipos+1, imode+1, settings->retrieval.IWW_SINGL[settings->retrieval.NDIM.ISTARSING[icharacteristic][0]+ipos-1], settings->retrieval.IWW_SINGL[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1], ipos+1);
                                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                    nerror--;                                   
                                }
                            }                            
                        }
                    }
                } 
                
                if(value[icharacteristic]==par_type_SHD_distr){   
                    if(settings->retrieval.indep_par == true){
                        number_of_modes=settings->retrieval.NDIM.n2[icharacteristic];

                        for (ipos = 0; ipos < number_of_modes; ipos++) {
                            if(settings->retrieval.NDIM.n3[icharacteristic][ipos]!=settings->tmp.NRATIO1[ipos]-1){
                                sprintf(error,"Definition of the characteristic type aspect_ratio_distribution does not match with retrieval.forward_model.phase_matrix.ratio.mode[%d].value. Number of elemens should be one element less than the number of ratios %d!=%d",ipos, settings->retrieval.NDIM.n3[icharacteristic][ipos], settings->tmp.NRATIO1[ipos]-1);
                                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error);
                                nerror--; 
                            }
                        }
                    }else{
                        number_of_modes=settings->retrieval.NDIM.n2[icharacteristic];

                        for (ipos = 0; ipos < number_of_modes; ipos++) {
                            if(settings->retrieval.NDIM.n3[icharacteristic][ipos]!=settings->tmp.NRATIO1[ipos]){
                                sprintf(error,"Definition of the characteristic type aspect_ratio_distribution does not match with retrieval.forward_model.phase_matrix.ratio.mode[%d].value. Number of elemens should be the same than the number of ratios %d!=%d",ipos, settings->retrieval.NDIM.n3[icharacteristic][ipos], settings->tmp.NRATIO1[ipos]-1);
                                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error);
                                nerror--; 
                            }
                        }
                    }
                }
                
                if(value[icharacteristic]==par_type_AVP_par_std){
                    number_of_modes=settings->retrieval.NDIM.n2[icharacteristic];
                    for (imode = 0; imode < number_of_modes; imode++) {
                        number_of_element_per_mode=settings->retrieval.NDIM.n3[icharacteristic][imode];
                        if(number_of_element_per_mode!=1){
                            sprintf(error,"Characteristic type vertical_profile_parameter_standard_deviation can have only one element per mode");
                            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                            nerror--;                                   
                        }
                    
                        for (ipos = 0; ipos < number_of_element_per_mode; ipos++) {
                            if(settings->retrieval.APSING[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]<130){
                                sprintf(error,"The value of characteristic type vertical_profile_parameter_standard_deviation is too low (less than 130 meters)");
                                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                nerror--;  
                            }
                            if(settings->retrieval.APSMIN[settings->retrieval.NDIM.ISTARSING[icharacteristic][imode]+ipos-1]<130){
                                sprintf(error,"The minimum value of characteristic type vertical_profile_parameter_standard_deviation is too low (less than 130 meters)");
                                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                                nerror--;  
                            }
                        }
                    }
                    // If std is present and aerosol vertical profile is exponential this characteristic can not be retrieved (retrieved has to be false)
                    if(settings->retrieval.aer_prof_type==0 && settings->retrieval.NDIM.par_retr[icharacteristic]==true){ // 0 means exponential
                        sprintf(error,"If retrieval.radiative_transfer.aerosol_profile_vertical_type is exponential the characteristic type vertical_profile_parameter_standard_deviation can not be retrieved. Set to false retrieval.constraints.characteristic[%d].retrieved", icharacteristic+1);
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;                              
                    }
                }
                
                if(value[icharacteristic]==par_type_USER_defined_1 && settings->retrieval.NDIM.par_retr[icharacteristic]==true){
                    sprintf(error,"user_defined_1 characteristic type can not be retrieved. Set to false retrieval.constraints.characteristic[%d].retrieved", icharacteristic+1);
                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                    nerror--;   
                }
                
                if(value[icharacteristic]==par_type_USER_defined_2 && settings->retrieval.NDIM.par_retr[icharacteristic]==true){
                    sprintf(error,"user_defined_2 characteristic type can not be retrieved. Set to false retrieval.constraints.characteristic[%d].retrieved", icharacteristic+1);
                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                    nerror--;   
                }
                
                if(value[icharacteristic]==par_type_USER_defined_3 && settings->retrieval.NDIM.par_retr[icharacteristic]==true){
                    sprintf(error,"user_defined_3 characteristic type can not be retrieved. Set to false retrieval.constraints.characteristic[%d].retrieved", icharacteristic+1);
                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                    nerror--;   
                }
            }
        }
    }
    
    icharacteristic=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_SD_beg, par_type_SD_end);
    if (icharacteristic>=0){
        // To have defined a size distribution is mandatory. It is ok
    }else{
        sprintf(error,"There is no size distribution defined in initial guess (retrieval.constraints.characteristic[].type parameters). It is mandatory");
        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
        nerror--;  
    }
    
    // For transport models
    if(settings->retrieval.use_tmodel==true && settings->retrieval.NDIM.par_type[icharacteristic]!=par_type_SD_LN){
        sprintf(error,"If use_transport_model==true, the size distribution must be represented as lognormal bins");
        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
        nerror--;  
    }

    
    icharacteristic=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_Cv_beg, par_type_Cv_end);
    if(settings->retrieval.indep_par == true){
        if (icharacteristic>=0){
            // To have defined concentration is mandatory if indep_par==true
        }else{
            sprintf(error,"Concentration characteristic must be definedd if retrieval.inversion.independent_concentration_and_shape=true");
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
            nerror--;  
        }
    }
    
    // For transport models
    if(settings->retrieval.use_tmodel==true && settings->retrieval.NDIM.par_type[icharacteristic]!=par_type_TM_C){
        sprintf(error,"If use_transport_model==true, aerosol concentration must be represented as tracer_level_concentration (instead of aerosol_concentration)");
        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
        nerror--;  
    }
    
    
    icharacteristic=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_SHD_beg, par_type_SHD_end);
    if (icharacteristic>=0){
        if(SD_MD){ // We check sphericity only if SD models is not present
            sprintf(error,"Sphericity is defined in initial guess (retrieval.constraints.characteristic[].type parameters) but it is not compatible with SD defined as 'models'");
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
            nerror--;  
        }else{
            // To have defined a sphericity is mandatory. It is ok   
        }
    }else{
        if(SD_MD==false){ // We check sphericity only if SD models is not present
            sprintf(error,"There is no sphericity defined in initial guess (retrieval.constraints.characteristic[].type parameters). It is mandatory (except if SD is defined as 'models')");
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
            nerror--;  
        }
    }    
    
    if(settings->retrieval.gases.integration_method<1 && settings->retrieval.gases.nlut_name < ngases){
        sprintf(error,"The number of look-up-tables defined for gases (%d) must be equal or greater than the number of gases (%d) defined in the retrieval", settings->retrieval.gases.nlut_name, ngases);
        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
        nerror--; 
    }

    icharacteristic=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_RERI_beg, par_type_RERI_end);
    if (icharacteristic>=0){
        if(SD_MD){ // We check sphericity only if SD models is not present
            sprintf(error,"Refractive index real part is defined in initial guess (retrieval.constraints.characteristic[].type parameters) but it is not compatible with SD defined as 'models'");
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
            nerror--; 
        }else{
            // To have defined a size distribution is mandatory. It is ok
            char_type=settings->retrieval.NDIM.par_type[icharacteristic];
            icharacteristic=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_IMRI_beg, par_type_IMRI_end);
            if(SD_MD){
                if(icharacteristic>=0){
                    sprintf(error,"Refractive index imaginary part is defined in initial guess (retrieval.constraints.characteristic[].type parameters) but it is not compatible with SD defined as 'models'");
                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                    nerror--; 
                }
            }else{
                if (char_type==par_type_RERI_spect || char_type==par_type_RERI_const){
                    if (icharacteristic>=0){
                        // If refractive index real part is "const" or "spect", imaginary part of refractive index has to be defined
                    }else{
                        sprintf(error,"If characteristic real_part_of_refractive_index_spectral_dependent or real_part_of_refractive_index_constant is present then is mandatory to define imaginary part of refractive index");
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;  
                    }        
                }else{
                    if (icharacteristic>=0){
                        sprintf(error,"If characteristic particle_component_volume_fractions_linear_mixture or particle_component_fractions_chemical_mixture is present then is forbidden to define imaginary part of refractive index");
                        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                        nerror--;
                    }else{
                        // Imaginary part of refractive index can not be set if particle_component_fractions_chemical_mixture or particle_component_fractions_chemical_mixture is used
                    } 
                }
            }
        }
    }else{
        if(SD_MD==false){ // We check sphericity only if SD models is not present
            sprintf(error,"There is no refractive index defined in initial guess (retrieval.constraints.characteristic[].type parameters). It is mandatory (except if SD is defined as 'models')");
            yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
            nerror--;  
        }else{
            icharacteristic=grasp_parameters_index_of_parameter_type_by_kind_of_parameter(&settings->retrieval.NDIM, par_type_IMRI_beg, par_type_IMRI_end);
            if(icharacteristic>=0){
                sprintf(error,"Refractive index imaginary part is defined in initial guess (retrieval.constraints.characteristic[].type parameters) but it is not compatible with SD defined as 'models'");
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index,dictionary), error); 
                nerror--; 
            }
        }
    }   

    
    return nerror;    
    
}


int graspsettings_validator_same_nelements_or_zero(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    int *value1,*value2,index2,i,indexes[YAMLSETTINGS_MAX_PARAMETER_DIMENSIONS];
    yamlsettings_parameter *param;
    char error[2048], *param_name1, *param_name2;  
    int nerror=0;
    
    param=&(dictionary->parameters[param_index]);    
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        value1=(int *)param->counter_mem_pos;
        index2=yamlsettings_dictionary_find_parameter_by_name(arguments[0],dictionary);
        assert(index2>=0);
        value2=dictionary->parameters[index2].counter_mem_pos;
        for (i = 0; i < yamlsettings_parameter_counter_number_of_elements(param); i++) {
            if(value1[i]>0 && value1[i]!=value2[i]){
                indexesofND(i,indexes,yamlsettings_parameter_number_of_dimensions_in_string(param),param->dimensions.dimension_size);
                param_name1=yamlsettings_parameter_get_with_indexes(param->name, yamlsettings_parameter_number_of_dimensions_in_string(param), indexes);
                param_name2=yamlsettings_parameter_get_with_indexes(dictionary->parameters[index2].name, yamlsettings_parameter_number_of_dimensions_in_string(param), indexes);
                sprintf(error,"%s (%d elements) must be an array the same number of elements that %s (%d elements)."
                        ,param_name1,value1[i]/**(param->counter_mem_pos)*/, 
                        param_name2, value2[i] /**(dictionary->parameters[index2].counter_mem_pos)*/);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), error); 
                nerror--;
                free(param_name1);
                free(param_name2);
            }
        }        
    }
    
    return nerror;    
}

int graspsettings_validator_estimates_and_weights(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    // They have to be characteristic[].mode[].initial_guess.value minus characteristic[].mode[].single_pixel.smoothness_constraints.difference_order
    // or zero 
    int *value1,*value2,index2,i,indexes[YAMLSETTINGS_MAX_PARAMETER_DIMENSIONS];
    int *value3, index3;
    yamlsettings_parameter *param;
    char error[2048], *param_name1, *param_name2;  
    int nerror=0;
    
    param=&(dictionary->parameters[param_index]);    
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        value1=(int *)param->counter_mem_pos;
        index2=yamlsettings_dictionary_find_parameter_by_name("retrieval.constraints.characteristic[].mode[].initial_guess.value",dictionary);
        assert(index2>=0);
        value2=(int *)dictionary->parameters[index2].counter_mem_pos;
        
        index3=yamlsettings_dictionary_find_parameter_by_name("retrieval.constraints.characteristic[].mode[].single_pixel.smoothness_constraints.difference_order",dictionary);
        assert(index3>=0);
        value3=(int *)dictionary->parameters[index3].mem_pos;
        
        
        for (i = 0; i < yamlsettings_parameter_counter_number_of_elements(param); i++) {
            if(value1[i]>0 && value2[i]>0 && value1[i]!=value2[i]-value3[i]){
                indexesofND(i,indexes,yamlsettings_parameter_number_of_dimensions_in_string(param),param->dimensions.dimension_size);
                param_name1=yamlsettings_parameter_get_with_indexes(param->name, yamlsettings_parameter_number_of_dimensions_in_string(param), indexes);
                param_name2=yamlsettings_parameter_get_with_indexes(dictionary->parameters[index2].name, yamlsettings_parameter_number_of_dimensions_in_string(param), indexes);
                sprintf(error,"%s has %d elements instead of the expected %d. The array must have the same number of elements that %s - difference_order (currently read %d elements - %d difference order == %d != %d)."
                        ,param_name1,value1[i], value2[i]-value3[i],/**(param->counter_mem_pos)*/ 
                        param_name2, value2[i], value3[i], value1[i], value2[i]-value3[i]  /**(dictionary->parameters[index2].counter_mem_pos)*/);
                yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), error); 
                nerror--;
                free(param_name1);
                free(param_name2);
            }
        }        
    }
    
    return nerror;    
}

int graspsettings_validator_kernelpath(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    char *kernel_folder;
    DIR *dir;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    char path[2048];
    char kernel_folder_c[2048];
    int i;
    char *internal_files;
        
    // Retrieve parameter
    param=&(dictionary->parameters[param_index]);    
        
    internal_files=((grasp_settings *)(dictionary->settings))->retrieval.DLSF.internal_file_path;
    
    // Search the index of last character in internal files
    i=_GBL_FILE_PATH_LEN-1;
    while(internal_files[i]==' ') i--;
    
    strncpy(path,internal_files,i+1);
    path[i+1]='\0';
    
    // Get pointer to first char
    i=_GBL_FILE_PATH_LEN-1;
    kernel_folder=(char *)param->mem_pos;
    while(kernel_folder[i]==' ') i--;
    strncpy(kernel_folder_c,kernel_folder,i+1);
    kernel_folder_c[i+1]='\0';
    
    strcat(path,kernel_folder_c);
    
    dir= opendir(path);

    if (dir) { //Directory exists.
        closedir(dir);
    } else {
        sprintf(error,"%s must be an existing directory. Kernels %s does not exists or is not installed in %s. Please, install them correctly\n", path, kernel_folder_c, path);
        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), error); 
        nerror--;
    } 
    
    
    return nerror;
}

//int graspsettings_validator_rnoise(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
//    grasp_settings *settings; 
//    char error[2048];
//    int nerror=0;
//    
//    settings=(grasp_settings *)dictionary->settings; 
//    
//    if(settings->retrieval.ISTOP==false && settings->retrieval.NOISE.sim_meas_rnoise!=0){ // inversion
//        sprintf(error,"If retrieval.mode=inversion then retrieval.inversion.noises.add_random_noise has to be 'disable'");
//        yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), error); 
//        nerror--;
//    }
//    
//    return nerror;
//}

int graspsettings_validator_int_not_repeated_or_one(int param_index,yamlsettings_dictionary_t *dictionary, char arguments[YAMLSETTINGS_VALIDATOR_MAX_ARGUMENTS][YAMLSETTINGS_VALIDATOR_MAX_ARGUMENT_SIZE]){
    /* MH This validator has not into account that the one could be repeated. 
     * The one is the default value, so we are blind to it */
    int *value,counter,i,j;
    yamlsettings_parameter *param;
    char error[2048];
    int nerror=0;
    
    param=&(dictionary->parameters[param_index]);
    
    if(yamlsettings_parameter_is_unset(param_index, dictionary)==false){
        counter=yamlsettings_parameter_number_of_elements(param);
        value=(int *)param->mem_pos;
        for (i = 0; i < counter; i++) {
            for (j = 0; j < counter; j++) {
                if(i>j && value[i]!=1 && value[i]==value[j]){
                    sprintf(error,"%s has the value %d repeated",param->name, value[i]);
                    yamlsettings_error_add_validation_error(&(dictionary->status), yamlsettings_parameter_file_index(param_index, dictionary), error); 
                    nerror--;
                }
            }
        }
    }
    
    return nerror;    
}