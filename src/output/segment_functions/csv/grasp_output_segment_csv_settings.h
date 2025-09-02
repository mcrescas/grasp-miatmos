/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */


#ifndef GRASP_OUTPUT_SEGMENT_CSV_SETTINGS_H
#define GRASP_OUTPUT_SEGMENT_CSV_SETTINGS_H

#ifdef __cplusplus
extern "C" {
#endif

    typedef struct grasp_output_segment_function_settings_csv_t_ {
        char delimiter;
        bool compression;
        bool show_timing;
    }grasp_output_segment_function_settings_csv_t;


#ifdef __cplusplus
}
#endif

#endif /* GRASP_OUTPUT_SEGMENT_CSV_SETTINGS_H */

