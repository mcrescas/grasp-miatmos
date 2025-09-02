/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   grasp_controller_iteration_callback.h
 * Author: david
 *
 * Created on October 25, 2017, 5:33 AM
 */

#ifndef GRASP_CONTROLLER_ITERATION_CALLBACK_H
#define GRASP_CONTROLLER_ITERATION_CALLBACK_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../settings/grasp_settings_t.h"
#include "../input/grasp_input_segment.h"
#include "../output/grasp_output_segment_result.h"
    
/**
 * This function set up global variables that will allow retrieval code to perform callbacks
 * @param settings
 * @param segment
 * @param output
 * @param tile_description
 */
void grasp_controller_iteration_callback_initialize(const grasp_settings *settings, const grasp_segment_t *segment, output_segment_general *output, const grasp_tile_description_t *tile_description, grasp_processing_functions_t *functions);

/**
 * This function set up global variables for each segment retrieved that will allow the retrieval code to perform callbacks
 * @param icol
 * @param irow
 * @param itime
 */
void grasp_controller_iteration_callback_setup(int icol,int irow,int itime);

/**
 * This function is called by retrieval code as callback
 */
void grasp_controller_iteration_callback();

#ifdef __cplusplus
}
#endif

#endif /* GRASP_CONTROLLER_ITERATION_CALLBACK_H */

