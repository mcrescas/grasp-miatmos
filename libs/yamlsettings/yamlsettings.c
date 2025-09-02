#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <grasp/utils.h>
#include "yamlsettings_dictionary.h"
#include "yamlsettings_data_types.h"
#include "yamlsettings_input_yaml.h"
#include "yamlsettings_process_yaml.h"
#include "yamlsettings_assign_data.h"
//#include "../grasp_settings_assign_data.h"
#include "yamlsettings.h"


int yamlsettings_read_file(yamlsettings_dictionary_t *dictionary, int nparameters, char const *parameters[], yamlsettings_preprocess preprocess, yamlsettings_postprocess postprocess, yamlsettings_parser_settings_file_mode settings_file_mode){
    GNode *root;
    
    // Initialize dictionary and set default values
    yamlsettings_initialize_dictionary(dictionary,parameters[0],settings_file_mode);
    yamlsettings_dictionary_set_defaults(dictionary);
    
    yamlsettings_input_yaml_read(&(dictionary->status), &root, nparameters, parameters, dictionary->files, dictionary->file_modes, settings_file_mode);
   
    // Pre-process
    if(preprocess!=NULL){
        preprocess(root,dictionary);
    }
    
    // Save tree
    g_node_traverse(root, G_PRE_ORDER, G_TRAVERSE_LEAVES, -1, yamlsettings_assign_node, (gpointer)dictionary);
    
    // Closing tree
    yamlsettings_destroy_tree(root);
    
    // Post-process
    if(postprocess!=NULL){
          postprocess(dictionary);
    }
    
    // Validate input
    yamlsettings_dictionary_validate(dictionary);
     
    // Return number of error in negative
    return -(dictionary->status.parse_errors->len + dictionary->status.validation_errors->len);
}

void yamlsettings_debug_dictionary(FILE *stream, yamlsettings_dictionary_t *dictionary, const char *filter){
    int i;
    char *string;
    int matching=0;
    
    assert(stream!=NULL);
    
    if(strcmp(filter,"")==0){
        fprintf(stream,"The following values will be used like input data (* means data were not set):\n");
    }else{
        fprintf(stream,"The following values will be used like input data matching with \"%s\"(* means data were not set):\n", filter);
    }    
    for (i=0; i<dictionary->nparameters; i++){ 
        if(strstr(dictionary->parameters[i].name,filter)!=NULL){
            // Printed if it has default value or no

            if(yamlsettings_parameter_is_unset(i, dictionary)==true){
                fprintf(stream, "*%3d ",i);
            }else{
                fprintf(stream, " %3d ",i);
            }

            string=dictionary->parameters[i].debug_function(&(dictionary->parameters[i]));

            fprintf(stream, "%s\n",string);

            free(string);
            
            matching++;
        }
    }   
    if(matching==0){
        fprintf(stream,"There is not matching parameters with %s\n", filter);
    }else{
        fprintf(stream, "%d parameter(s) found\n", matching);
    }    
}

void yamlsettings_print_help_information(FILE *stream, yamlsettings_dictionary_t *dictionary, const char *filter){
    int i;
    char *tmp;
    int matching=0;
    
    assert(stream!=NULL);
    
    // Print help information
    if(strcmp(filter,"")==0){
        fprintf(stream,"The following options are accepted:\n");
    }else{
        fprintf(stream,"The following options are accepted matching with \"%s\":\n", filter);
    }
    for (i=0; i<dictionary->nparameters; i++){ 
        if(strstr(dictionary->parameters[i].name,filter)!=NULL){
            fprintf(stream,"  %s: %s",dictionary->parameters[i].name, dictionary->parameters[i].description);
            tmp=dictionary->parameters[i].data_type_get(NULL,0,0,false);
            if(strcmp(tmp,"")!=0){
                fprintf(stream," (values: [%s])",tmp);
            }
            free(tmp);
            matching++;
            fprintf(stream,"\n");
        }
    }
    if(matching==0){
        fprintf(stream,"There is not matching parameters with %s\n", filter);
    }else{
        fprintf(stream, "%d parameter(s) found\n", matching);
    }
}


// from https://stackoverflow.com/questions/32496497/standard-function-to-replace-character-or-substring-in-a-char-array
static char* replace_char(const char* str, char find, char replace){
    char *result;
    
    result=(char *) malloc (sizeof(char) * (strlen(str) + 1));
    
    strcpy(result, str);
    
    char *current_pos = strchr(result,find);
    while (current_pos){
        *current_pos = replace;
        current_pos = strchr(current_pos,find);
    }
    return result;
}

#define __cat__( str ) length+=strlen(str); if(length>json_length){ return -2; }else{ strcat(json,str); }

int yamlsettings_dictionary_description_json(yamlsettings_dictionary_t *dictionary, char *json, int json_length){
    int i,j,length=0;
    char str_tmp[100];
    char* description_sanitized;
    
    assert(json_length>0);
    
    length+=3; // Some buffer
    
    __cat__("[\n")
    
    for (i=0; i<dictionary->nparameters; i++){
        __cat__("  {\n    \"name\":\"");
        __cat__(dictionary->parameters[i].name);
        __cat__("\",\n    \"description\":\"");
        description_sanitized = replace_char(dictionary->parameters[i].description, '\"', '\'');
        __cat__(description_sanitized);
        free(description_sanitized);
        //__cat__(dictionary->parameters[i].description);
        __cat__("\",\n    \"dimensions\":[");
        for (j = 0; j < dictionary->parameters[i].dimensions.ndimensions; j++) {
            if(j!=0){
                __cat__(",");
            }
            sprintf(str_tmp,"%d",dictionary->parameters[i].dimensions.dimension_size[j]);
            __cat__(str_tmp);
        }
        __cat__("]\n");
        //__cat__("],\n");
        
        //__cat__("    \"datatype\":\"");
        //dictionary->parameters[i].data_type_get      
        //__cat__("\"\n");
        
        if(i!=dictionary->nparameters-1){
            __cat__("  },\n");
        }else{
            __cat__("  }\n");
        }
    }
    
    __cat__("]");
    
    return 0;
}

void yamlsettings_simulate_default_values(yamlsettings_dictionary_t *dictionary, yamlsettings_dictionary_t *reference){
    int ipar,ielement;
    char *default_value, *original_value;
    
    
    for (ipar = 0; ipar < dictionary->nparameters; ipar++) {
        for (ielement = 0; ielement < yamlsettings_parameter_number_of_elements(&dictionary->parameters[ipar]); ielement++) {
            default_value=dictionary->parameters[ipar].data_type_get(dictionary->parameters[ipar].mem_pos, ielement, dictionary->parameters[ipar].maxlength,false);
            original_value=reference->parameters[ipar].data_type_get(reference->parameters[ipar].mem_pos, ielement, dictionary->parameters[ipar].maxlength,false);
            
            if(strcmp(default_value,original_value)!=0){
                dictionary->parameters[ipar].settings_file[ielement]=NULL;
            }
            
            free(default_value);
            free(original_value);
        }
    }
}

int yamlsettings_dump(char *r, yamlsettings_dictionary_t *dictionary, bool print_defaults, yamlsettings_dictionary_t *reference, bool force_arrays){
    int i;
    int indexes[YAMLSETTINGS_MAX_PARAMETER_DIMENSIONS];
    // Preparing indexes
    for (i = 0; i < YAMLSETTINGS_MAX_PARAMETER_DIMENSIONS; i++) {
        indexes[i]=0;
    }
    yamlsettings_simulate_default_values(dictionary, reference);
    // Calling recursive function with initial values
    yamlsettings_dictionary_dump_recursive(r, dictionary,0,0,indexes,dictionary->nparameters, print_defaults, force_arrays);
        
    return 0;
}

void yamlsettings_copy_parameter(yamlsettings_parameter *origin, yamlsettings_parameter *dest){
    int i;
        
    dest->name=origin->name;
    dest->mem_pos=origin->mem_pos;
    dest->additional_information=origin->additional_information;
    dest->data_type_set=origin->data_type_set;
    dest->data_type_get=origin->data_type_get;
    dest->maxlength=origin->maxlength;       
    dest->default_value=origin->default_value;
    dest->dimensions=origin->dimensions;
    dest->allow_array=origin->allow_array;
    dest->description=origin->description;
    for(i=0;i<YAMLSETTINGS_MAX_VALIDATORS;i++){
             dest->validators[i]=origin->validators[i];
    }      
    dest->counter_mem_pos=origin->counter_mem_pos;
    dest->input_function=origin->input_function;
    dest->debug_function=origin->debug_function;
    dest->settings_file=origin->settings_file;  
    dest->name_deallocatable=origin->name_deallocatable;  
}