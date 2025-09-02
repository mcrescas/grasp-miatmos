/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/**
 * @file grasp_main.h
 * @author David Fuertes
 * @date 29 Sep 2013
 * @brief Start point of GRASP code
 */

#ifndef GRASP_MAIN_H
#define	GRASP_MAIN_H

#ifdef	__cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include "../input/grasp_input_tile_description.h"
#include "../output/grasp_output_tile_result.h"
#include "../output/grasp_output_segment_result.h"
#include "grasp_controller.h"    


/**
 * Starting point of GRASP from external library
 * @param argc Number of input command line arguments
 * @param argv Input command line arguments
 * @param settings_file_mode how settings are provided (inline, none, mandatory, ...)
 * @param results_description Access to results
 * @param input_segment In case a segment is provided, it will loaded instead of calling a driver
 * @param input_sdata In case a sdata is provided, it will loaded instead of calling a driver
 * @return 0 if the process finished properly, otherwise error code
 */
int main_sequential(int argc, char** argv, yamlsettings_parser_settings_file_mode settings_file_mode, grasp_results_description *results_description, grasp_segment_t *input_segment, sensor_data_t *input_sdata);

#ifdef USE_MPI

int main_mpi(int argc, char** argv, yamlsettings_parser_settings_file_mode settings_file_mode, grasp_results_description *results_description, grasp_segment_t *input_segment, sensor_data_t *input_sdata);

#endif /* USE_MPI */


#ifdef	__cplusplus
}
#endif

#endif	/* GRASP_MAIN_H */

