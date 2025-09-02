/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include <stdio.h>
#include <stdlib.h>
#include "../settings/grasp_settings.h"
#include "../input/grasp_input.h"
#include "grasp_controller.h"
#include "grasp_mpi_engine.h"
#include "yamlsettings/yamlsettings.h"
#include "../output/grasp_output_stream.h"
#include "../output/grasp_output.h"
#include "../retrieval/inversion/iteration_callback.h"
#include "../input/grasp_input_tile_description.h"
#include "grasp_controller_iteration_callback.h"

const grasp_settings           *global_settings;
const grasp_segment_t          *global_segment;
output_segment_general         *global_output;
const grasp_tile_description_t *global_tile_description;
int                            global_icol;
int                            global_irow;
int                            global_itime;
grasp_processing_functions_t   *global_functions;


void grasp_controller_iteration_callback_initialize(const grasp_settings *settings, const grasp_segment_t *segment, output_segment_general *output, const grasp_tile_description_t *tile_description, grasp_processing_functions_t *functions){
    global_settings=settings;          
    global_segment=segment;
    global_output=output;
    global_tile_description=tile_description; 
    global_functions=functions;
    
    set_callback_function(grasp_controller_iteration_callback);
}

void grasp_controller_iteration_callback_setup(int icol,int irow,int itime){
    global_icol=icol;
    global_irow=irow;
    global_itime=itime;  
}

void grasp_controller_iteration_callback(){
    int i /*, status*/;
    grasp_output_stream stream;
    
    for (i = 0; i < global_settings->output.niteration_output_function; i++) {
        grasp_output_stream_initialize(global_settings->output.iteration_stream[i], &stream);
        /*status=*/global_functions->iteration_output_functions[i].function(&stream, (grasp_settings *)global_settings,  (grasp_segment_t *)global_segment, global_output, (grasp_tile_description_t *) global_tile_description, global_icol, global_irow, global_itime);
        grasp_output_stream_destroy(&stream);
    }
}

