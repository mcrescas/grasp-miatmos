/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include "yamlsettings/yamlsettings_data_types.h"
#include "grasp_settings_data_types.h"
#include <grasp/utils.h>
#include "../input/grasp_input_settings.h"
#include "../global/grasp_retrieval_meas_type.h"
#include "../global/grasp_retrieval_characteristic_type.h"


int grasp_data_type_retrieval_mode_set(yamlsettings_status *status,void *mem_pos, int nelements,  const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file){
    char *c;
    bool *d;
    GString *error;
    
    d = (bool *) mem_pos;

    c = strtolower(data);

    if (strcmp(c, "inversion") == 0 || strcmp(c, "invert") == 0 || strcmp(c, "i") == 0) {
        d[position] = false;
    } else if (strcmp(c, "forward") == 0 || strcmp(c, "simulation") == 0 || strcmp(c, "simulate") == 0) {
        d[position] = true;
    } else {  
        error=g_string_new(NULL);
        g_string_printf (error,
                 "Data can not be assigned. Value unrecognised in parameter %s. Expected inversion or forward (read: %s)",
                 name, c);
        yamlsettings_error_add_parse_error(status, (char *)error->str, file_index, YS_ERROR);        
        free(c);
        g_string_free(error,true);
        return -1;
    }
    free(c);
    
    return 0;
}
char *grasp_data_type_retrieval_mode_get(void *mem_pos,int position, int maxlength, bool escape){
    char *result;
    
    result = (char *) malloc(sizeof (char)*YAMLSETTINGS_VAR_VALUE_MAX_LENGTH);
    assert(result!=NULL);
    strcpy(result,"");
    if(mem_pos!=NULL){
        if (((bool *) mem_pos)[position] == true) {
            strcpy(result, "forward");
        } else {
            strcpy(result, "inversion");
        }    
    }
    
    return result;
}

int grasp_data_type_fortran_string_set(yamlsettings_status *status, void *mem_pos, int nelements,  const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file){
    char *c;
    int max,i;
    char error[512];
    c = (char *) mem_pos;
    c = &c[(position*maxlength)+0];
    max = strlen(data);
    if (max <= maxlength) {
        // Copy the string in fortran format
        for (i = 0; i < max; i++) {
            c[i] = data[i];
        }
        for (i = max; i < maxlength; i++) {
            c[i] = ' ';
        }
    } else {
        sprintf(error,"String too long in parameter %s\n", name);
        yamlsettings_error_add_parse_error(status, error, file_index, YS_ERROR); 
        return -1;
    }

    return 0;
}
char *grasp_data_type_fortran_string_get(void *mem_pos,  int position, int maxlength, bool escape){
    int nvalues;
    char *c,*result;
    
    result = (char *) trackmem_malloc(sizeof (char)*maxlength+5);
    assert( result!= NULL);
    strcpy(result,"");
    
    if(mem_pos!=NULL){
        nvalues = strfortranlen(mem_pos, maxlength);
        c = (char *) trackmem_malloc(sizeof (char)*nvalues + 5);
        assert( c!= NULL);
        strncpy(c, mem_pos, nvalues);
        c[nvalues] = '\0';
        if(escape){
            sprintf(result, "\"%s\"", c);
        }else{
            strcpy(result, c);
        }
        trackmem_free(c);
    }
    
    return result;
}

int grasp_data_type_fortran_file_path_set(yamlsettings_status *status, void *mem_pos, int nelements,  const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file){
    char *c;
    int max_data,i,max_base=0;
    char *base_path;
    char error[512];
    
    base_path=pathoffile(settings_file);
    c = (char *) mem_pos;
    c = &c[(position*maxlength)+0];
    max_data = strlen(data);
    
    if(isabsolute(data)==false){
        max_base=strlen(base_path);
    }

    if (max_base+max_data <= maxlength) {
        // Copy the string in fortran format
        for (i = 0; i < max_base; i++) { // Copy base if its necessary (if not max_base is equal to 0)
            c[i] = base_path[i];
        }
        for (i = 0; i < max_data; i++) { // Copy data
            c[max_base+i] = data[i];
        }
        for (i = 0; i < maxlength-(max_base+max_data); i++) { // Filling with zeros
            c[max_base+max_data+i] = ' ';
        }
        
    } else {
        printf("String too long in parameter %s\n", name);
        yamlsettings_error_add_parse_error(status, error, file_index, YS_ERROR);
        trackmem_free(base_path);
        return -1;
    }

    trackmem_free(base_path);
    return 0;
}
char *grasp_data_type_fortran_file_path_get(void *mem_pos,  int position, int maxlength, bool escape){
    int nvalues;
    char *c,*result;
    
    result = (char *) trackmem_malloc(sizeof (char)*maxlength+5);
    assert( result!= NULL);
    strcpy(result,"");
    
    if(mem_pos!=NULL){
        nvalues = strfortranlen(mem_pos, maxlength);
        c = (char *) trackmem_malloc(sizeof (char)*nvalues + 5);
        assert( c!= NULL);
        strncpy(c, mem_pos, nvalues);
        c[nvalues] = '\0';
        if(escape){
            sprintf(result, "\"%s\"", c);
        }else{
            strcpy(result, c);
        }
        trackmem_free(c);  
    }
    
    return result;
}

int grasp_data_type_fortran_folder_path_set(yamlsettings_status *status, void *mem_pos, int nelements,  const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file){
    char *c;
    int max_data,i,max_base=0;
    char *base_path;
    bool add_slash=false;
    char error[512];
    
    base_path=pathoffile(settings_file);
    c = (char *) mem_pos;
    c = &c[(position*maxlength)+0];
    max_data = strlen(data);
    
    if(data[max_data-1]!='/'){
        max_data+=1;
        add_slash=true;
    }
    
    if(isabsolute(data)==false){
        max_base=strlen(base_path);
    }

    if (max_base+max_data <= maxlength) {
        // Copy the string in fortran format
        for (i = 0; i < max_base; i++) { // Copy base if its necessary (if not max_base is equal to 0)
            c[i] = base_path[i];
        }
        for (i = 0; i < max_data; i++) { // Copy data
            c[max_base+i] = data[i];
        }
        for (i = 0; i < maxlength-(max_base+max_data); i++) { // Filling with zeros
            c[max_base+max_data+i] = ' ';
        }
        if(add_slash==true){
            c[max_base+max_data-1]='/';
        }        
    } else {
        printf("String too long in parameter %s\n", name);
        yamlsettings_error_add_parse_error(status, error, file_index, YS_ERROR);
        trackmem_free(base_path);
        return -1;
    }

    trackmem_free(base_path);
    return 0;
}
char *grasp_data_type_fortran_folder_path_get(void *mem_pos,  int position, int maxlength, bool escape){
    int nvalues;
    char *c,*result;
    
    result = (char *) trackmem_malloc(sizeof (char)*maxlength+5);
    assert( result!= NULL);
    strcpy(result,"");
    
    if(mem_pos!=NULL){
        nvalues = strfortranlen(mem_pos, maxlength);
        c = (char *) trackmem_malloc(sizeof (char)*nvalues + 5);
        assert( c!= NULL);
        strncpy(c, mem_pos, nvalues);
        c[nvalues] = '\0';
        if(escape){
            sprintf(result, "\"%s\"", c);
        }else{
            strcpy(result, c);
        }
        trackmem_free(c);  
    }
    
    return result;
}

int grasp_data_type_stream_set(yamlsettings_status *status, void *mem_pos, int nelements,  const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file){
    char *c, *d;
    
    c = strtolower(data);
    
    // calculating position
    d = (char *) mem_pos;
    d = &d[(position*maxlength)+0];
    
    if (strcmp(c, "screen") == 0 || strcmp(c, "stdout") == 0 || strcmp(c, "true") == 0|| strcmp(c, "t") == 0 || strcmp(c, "1") == 0 || strcmp(c, "") == 0) {
        assert(maxlength>7);
        strcpy(d,"screen");
        trackmem_free(c);
        return 0;
    } else if (strcmp(c, "none") == 0 || strcmp(c, "null") == 0|| strcmp(c, "false") == 0|| strcmp(c, "f") == 0 || strcmp(c, "0") == 0) {
        assert(maxlength>5);
        strcpy(d,"none");
        trackmem_free(c);
        return 0;
    } else if (strcmp(c, "stderr") == 0 || strcmp(c, "-1") == 0) {
        assert(maxlength>7);
        strcpy(d,"stderr");
        trackmem_free(c);
        return 0;
    }else{
        if((strlen(c)>5 && c[0]=='{' && c[1]=='p' && c[2]=='w' && c[3]=='d' && c[4]=='}') ||  (strlen(c)>5 && c[0]=='{' && c[1]=='y' && c[2]=='m' && c[3]=='l' && c[4]=='}')){
            trackmem_free(c);
            return yamlsettings_data_type_string_set(status, mem_pos, nelements,  name, data, position, maxlength, file_index, settings_file);
        }else{
            trackmem_free(c);
            return yamlsettings_data_type_file_path_set(status, mem_pos, nelements,  name, data, position, maxlength, file_index, settings_file);
        }
    }
}
char *grasp_data_type_stream_get(void *mem_pos,  int position, int maxlength, bool escape){
    return yamlsettings_data_type_file_path_get(mem_pos, position, maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_minimization={"minimization", 2,
        {
            {"absolute", "abs", 0},
            {"logarithm", "log", 1}
        }};
int grasp_data_type_minimization_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_minimization, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_minimization_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_minimization,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_ipplane={"ipplane", 2,
        {
            {"principal_plane", "pp", 0},
            {"meridian", "mer", 1}
        }};
int grasp_data_type_ipplane_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_ipplane, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_ipplane_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_ipplane,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_int_method={"integration_method", 3,
        {
            {"no-absorption", "no", -1},
            {"line-by-line", "lbl", 0},
            {"k-distribution", "kdist", 1}
        }};
int grasp_data_type_int_method_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_int_method, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_int_method_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_int_method,mem_pos,position,maxlength, escape);
}


yamlsettings_enumeration_definition grasp_data_type_stdat_name={"stdat_name", 7,
        {
            {"from_file",          "ff", -1},
            {"us_standard",         "us", 0},
            {"mid_latitude_summer", "ms", 1},
            {"mid_latitude_winter", "mw", 2},
            {"surbartic_summer",    "ss", 3},
            {"subartic_winter",     "sw", 4},
            {"tropical_atmosphere", "tr", 5},
        }};
int grasp_data_type_stdat_name_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_stdat_name, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_stdat_name_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_stdat_name,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_pfitting={"polarization_fitting", 4,
        {
            {"absolute_polarization_components", "abs_pol", 1},
            {"relative_polarization_components", "rel_pol", 2},
            {"polarized_reflectance", "pol_ref", 3},
            {"degree_of_polarization", "deg_pol", 4},
            // {"relative_phase_matrix_meas", "rel_phmx_meas", 5} // Deprecated: do not use any more.
        }};
int grasp_data_type_pfitting_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_pfitting, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_pfitting_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_pfitting,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_ifitting={"irradiance_fitting", 2,
        {
            {"radiances", "rad", 1},
            {"relative_radiances", "rel_rad", 2}
        }};
int grasp_data_type_ifitting_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_ifitting, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_ifitting_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_ifitting,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_bin={"bin", 2,
        {
            {"absolute", "abs", 1},
            {"logarithm", "log", -1}
        }};
int grasp_data_type_bin_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_bin, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_bin_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_bin,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_invsing={"inversion_regime", 3,
        {
            {"single_pixel", "single", 0},
            {"multi_pixel_followed_by_single_pixel", "mp_then_sp", 1},
            {"multi_pixel", "multi", 2}
        }};
int grasp_data_type_invsing_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_invsing, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_invsing_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_invsing,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_oshf_imsc={"oshf_imsc", 2,
        {
            {"multiple_scattering", "multiple", 0},
            {"single_scattering", "single", 1}
        }};
int grasp_data_type_oshf_imsc_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_oshf_imsc, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_oshf_imsc_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_oshf_imsc,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_oshd_imsc={"oshd_imsc", 2,
        {
            {"single_scattering", "single", 1},
            {"multiple_scattering","multiple_scattering",2}
        }};
int grasp_data_type_oshd_imsc_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_oshd_imsc, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_oshd_imsc_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_oshd_imsc,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_error={"error", 2,
        {
            {"absolute", "abs", 1},
            {"relative", "rel", 0}
        }};
int grasp_data_type_error_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_error, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_error_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_error,mem_pos,position,maxlength, escape);
}

#ifdef WARN_DRY
#warning "__MEAS_TYPE__ duplicated"
#endif   




yamlsettings_enumeration_definition grasp_data_type_measuretypes={"measuretypes", 30,
        {
            {"tod", "total_optical_depth",   MEAS_TYPE_TOD},
            {"aod", "aerosol_optical_depth", MEAS_TYPE_AOD},
            {"aaod", "aerosol_absorption",   MEAS_TYPE_ABS},
            {"htod", "hyperspectral_total_optical_depth",MEAS_TYPE_HTOD},
            {"p11", "p11",                   MEAS_TYPE_P11},
            {"p12", "p12",                   MEAS_TYPE_P12},
            {"p22", "p22",                   MEAS_TYPE_P22},
            {"p33", "p33",                   MEAS_TYPE_P33},
            {"p34", "p34",                   MEAS_TYPE_P34},
            {"p44", "p44",                   MEAS_TYPE_P44},
            {"p11_rel", "p11_rel_ang",       MEAS_TYPE_P11_rel_ang},
            {"p12_rel", "p12_rel",           MEAS_TYPE_P12_rel},
            {"ls", "ls",                     MEAS_TYPE_LS},
            {"rl", "rl",                     MEAS_TYPE_RL},
            {"dpar", "dpar",                 MEAS_TYPE_DPAR},
            {"dper", "dper",                 MEAS_TYPE_DPER},
            {"dp", "dp",                     MEAS_TYPE_DP},
            {"vext", "vext",                 MEAS_TYPE_VEXT},
            {"vbs", "vbs",                   MEAS_TYPE_VBS},
            {"i", "i",                       MEAS_TYPE_I},
            {"q", "q",                       MEAS_TYPE_Q},
            {"u", "u",                       MEAS_TYPE_U},
            {"p", "p",                       MEAS_TYPE_P},
            {"i_rel", "i_rel_sum",           MEAS_TYPE_I_rel_sum},
            {"p_rel", "p_rel",               MEAS_TYPE_P_rel},
            {"p11_intd", "p11_integrated",   MEAS_TYPE_P11_intd},
            {"p11_intd_cut_off_1", "p11_integrated_cut_off_1",   MEAS_TYPE_P11_intd_cut_off_1},
            {"p11_intd_cut_off_2", "p11_integrated_cut_off_2",   MEAS_TYPE_P11_intd_cut_off_2},
            {"p11_intd_cut_off_3", "p11_integrated_cut_off_3",   MEAS_TYPE_P11_intd_cut_off_3},
            {"p11_intd_cut_off_4", "p11_integrated_cut_off_4",   MEAS_TYPE_P11_intd_cut_off_4},
        }};
int grasp_data_type_measuretypes_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_measuretypes, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_measuretypes_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_measuretypes,mem_pos,position,maxlength, escape);
}


yamlsettings_enumeration_definition grasp_data_type_interpolation={"phase_matrix_interpolation", 2,
        {
            {"linear", "linear", 1},
            {"spline", "spline", 2},
        }};
int grasp_data_type_interpolation_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_interpolation, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_interpolation_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_interpolation,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_rnoise={"added_noise", 3,
        {
            {"disable", "false", 0},
            {"measurement_fitting", "true", 1},
            {"sdata", "fitting", 2},
        }};
int grasp_data_type_rnoise_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_rnoise, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_rnoise_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_rnoise,mem_pos,position,maxlength, escape);
}


yamlsettings_enumeration_definition grasp_data_type_imq={"normal_system_solver", 3,
        {
            {"simple_linear_iterations", "sli", 1},
            {"singular_value_decomposition", "svd", 2},
            {"sparse_matrix_solver", "sms", 3}
        }};
int grasp_data_type_imq_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_imq, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_imq_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_imq,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_imode_lut={"lut_mode", 3,
        {
            {"disable", "dis", 0},
            {"generate", "gen", 1},
            {"use", "use", 2}
        }};
int grasp_data_type_imode_lut_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_imode_lut, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_imode_lut_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_imode_lut,mem_pos,position,maxlength,escape);
}


#ifdef WARN_DRY
#warning "__CHARACTERISTIC_TYPE__ duplicated"
#endif  

yamlsettings_enumeration_definition grasp_data_type_characteristic={"characteristic_type", 45,
        {
            {"size_distribution_triangle_bins",                       "SizeDistrTriangBin",    par_type_SD_TB},
            {"size_distribution_precalculated_lognormal",             "SizeDistrLogNormBin",   par_type_SD_LB},
            {"size_distribution_lognormal",                           "SizeDistrLogNorm",      par_type_SD_LN},
            {"aerosol_model_concentration",                           "AeroModelCon",          par_type_SD_MD},
            {"real_part_of_refractive_index_spectral_dependent",      "RealRefIndSpect",       par_type_RERI_spect},
            {"real_part_of_refractive_index_constant",                "RealRefIndConst",       par_type_RERI_const},
            {"particle_component_volume_fractions_linear_mixture",    "VolFractLinearMix",     par_type_CXRI_nmix},
            {"particle_component_fractions_chemical_mixture",         "FractChemMix",          par_type_CXRI_chem},
            {"imaginary_part_of_refractive_index_spectral_dependent", "ImagRefIndSpect",       par_type_IMRI_spect},
            {"imaginary_part_of_refractive_index_constant",           "ImagRefIndConst",       par_type_IMRI_const},
            {"sphere_fraction",                                       "SphereFraction",        par_type_SHD_fsph},
            {"aspect_ratio_distribution",                             "AspectRatioDistrib",    par_type_SHD_distr},
            {"vertical_profile_parameter_height",                     "VertProfileHeight",     par_type_AVP_par_height},
            {"vertical_profile_normalized",                           "VertProfileNormalized", par_type_AVP_prof},
            {"aerosol_concentration",                                 "AerosolConcentration",  par_type_Cv},                    
            {"lidar_calibration_coefficient",                         "LidarCalibrCoeff",      par_type_CL},  
            {"vertical_profile_parameter_standard_deviation",         "VertProfileStdDev",     par_type_AVP_par_std},   
            {"relative_humidity",                                     "RH",                    par_type_RH},         
            {"surface_land_brdf_ross_li",                             "LandBRDFRossLi",        par_type_SURF1_land_Ross_Li_BRDF},
            {"surface_land_brdf_rpv",                                 "LandBRDFRPV",           par_type_SURF1_land_RPV_BRDF},
            {"surface_land_litvinov",                                 "LandBRMLitvinov",       par_type_SURF1_land_Litvinov},
            {"surface_land_litvinov_fast",                            "LandBRDFfastLitvinov",  par_type_SURF1_land_Litvinov_fast},
            {"surface_land_polarized_maignan_breon",                  "LandBPDFMaignanBreon",  par_type_SURF2_land_Maignan_Breon},
            {"surface_land_polarized_litvinov",                       "LandBPDFLitvinov",      par_type_SURF2_land_Litvinov},                      
            {"surface_water_cox_munk_iso",                            "WaterBRMCoxMunkIso",    par_type_SURF_water_Cox_Munk_iso},
            {"surface_water_cox_munk_ani",                            "WaterBRMCoxMunkAni",    par_type_SURF_water_Cox_Munk_ani},   
            {"surface_water_litvinov",                                "WaterBRMLitvinov",      par_type_SURF_water_Litvinov},
            {"surface_water_cox_munk_iso_2par",                       "WaterBRMCoxMunkIso2par",par_type_SURF_water_Cox_Munk_iso_2par},
            {"surface_water_morel",                                   "WaterBRMMorel",         par_type_SURF_water_Morel},
            {"gas_concentration_1",                                   "GasConcentration1",     par_type_gas_concentration_1},
            {"gas_concentration_2",                                   "GasConcentration2",     par_type_gas_concentration_2},
            {"gas_concentration_3",                                   "GasConcentration3",     par_type_gas_concentration_3},
            {"gas_concentration_4",                                   "GasConcentration4",     par_type_gas_concentration_4},
            {"gas_concentration_5",                                   "GasConcentration5",     par_type_gas_concentration_5},
            {"gas_concentration_6",                                   "GasConcentration6",     par_type_gas_concentration_6},
            {"gas_concentration_7",                                   "GasConcentration7",     par_type_gas_concentration_7},
            {"gas_concentration_8",                                   "GasConcentration8",     par_type_gas_concentration_8},
            {"gas_concentration_9",                                   "GasConcentration9",     par_type_gas_concentration_9},                    
            {"gas_concentration_10",                                  "GasConcentration10",    par_type_gas_concentration_10},
            {"level_relative_humidity",                               "LevRH",                 par_type_TM_RH},
            {"level_height",                                          "LevHeight",             par_type_TM_H},
            {"tracer_level_concentration",                            "TrLevC",                par_type_TM_C},
            {"user_defined_1",                                        "UsrDef1",               par_type_USER_defined_1},
            {"user_defined_2",                                        "UsrDef2",               par_type_USER_defined_2},
            {"user_defined_3",                                        "UsrDef3",               par_type_USER_defined_3},
        }};

int grasp_data_type_characteristic_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {    
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_characteristic, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_characteristic_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_characteristic,mem_pos,position,maxlength, escape);
}


yamlsettings_enumeration_definition grasp_data_type_surface={"surface", 2,
        {
            {"ocean", "o", 1},
            {"land", "l", 2}
        }};
int grasp_data_type_surface_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_surface, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_surface_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_surface,mem_pos,position,maxlength, escape);
}


yamlsettings_enumeration_definition grasp_data_type_molecular_profile_vertical_type={"molecular_profile_vertical_type", 2,
        {
            {"exponential", "exp", 0},
            {"standard_atmosphere", "std_atm", 1},
        }};
int grasp_data_type_molecular_profile_vertical_type_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_molecular_profile_vertical_type, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_molecular_profile_vertical_type_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_molecular_profile_vertical_type,mem_pos,position,maxlength, escape);
}


yamlsettings_enumeration_definition grasp_data_type_aerosol_profile_vertical_type={"aerosol_profile_vertical_type", 3,
        {
            {"exponential", "exp", 0},
            {"gaussian", "gauss", 1},
            {"threshold", "thresh", 2},
        }};
int grasp_data_type_aerosol_profile_vertical_type_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_aerosol_profile_vertical_type, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_aerosol_profile_vertical_type_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_aerosol_profile_vertical_type,mem_pos,position,maxlength, escape);
}



yamlsettings_enumeration_definition grasp_data_type_aerosol_transport_average_vertical_profile_type={"transport_model_vertical_type", 2,
        {
            {"column_average", "column", 1},
            {"tracer_average", "tracer", 2},
        }};
int grasp_data_type_trm_vertica_average_type_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_aerosol_transport_average_vertical_profile_type, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_trm_vertica_average_type_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_aerosol_transport_average_vertical_profile_type,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_fretr_method_type={"functional_retrieval_method_type", 3,
        {
            {"full_set", "none", 1},
            {"subset", "subset", 2},
            {"coefficients", "coefficients", 3},
        }};
int grasp_data_type_fretr_method_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_fretr_method_type, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_fretr_method_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_fretr_method_type,mem_pos,position,maxlength, escape);
}

yamlsettings_enumeration_definition grasp_data_type_fretr_function_type={"functional_retrieval_function_type", 2,
        {
            {"constant", "linear", 1},
            {"linear_logarithmic", "linear_logarithmic", 2},
        }};
int grasp_data_type_fretr_function_set(yamlsettings_status *status, void *mem_pos, int nelements, const char *name, const char *data, int position, int maxlength, int file_index, const char *settings_file) {
    return yamlsettings_data_type_generic_enumeration_set(yamlsettings_data_type_generic_enumeration_save_integer, &grasp_data_type_fretr_function_type, status, mem_pos,nelements,name,data,position,maxlength, file_index, settings_file);
}
char *grasp_data_type_fretr_function_get(void *mem_pos,int position, int maxlength, bool escape){
    return yamlsettings_data_type_generic_enumeration_get(yamlsettings_data_type_generic_enumeration_retrieve_integer, &grasp_data_type_fretr_function_type,mem_pos,position,maxlength, escape);
}

