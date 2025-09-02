/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* 
 * File:   grasp_output_segment_print_csv.h
 * Author: fuertes
 *
 * Created on January 25, 2018, 12:49 PM
 */

#ifndef GRASP_OUTPUT_SEGMENT_PRINT_CSV_H
#define	GRASP_OUTPUT_SEGMENT_PRINT_CSV_H

#ifdef	__cplusplus
extern "C" {
#endif

#include "../../grasp_output.h"
    
grasp_output_segment_function_t grasp_output_segment_function_csv();

int grasp_output_segment_function_csv_init(grasp_settings *settings, grasp_tile_description_t *input_information);

int grasp_output_segment_function_csv_close(void);

int grasp_output_segment_function_csv_process(grasp_output_stream *stream, grasp_settings *settings, grasp_segment_t *segment, output_segment_general *output, grasp_tile_description_t *tile_description,int icol,int irow,int itime);

grasp_settings_parameter_array *grasp_output_segment_function_settings_csv(grasp_settings *settings);

#ifdef	__cplusplus
}
#endif

#endif	/* GRASP_OUTPUT_SEGMENT_PRINT_CSV_H */

