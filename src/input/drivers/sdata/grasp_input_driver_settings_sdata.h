/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* 
 * File:   grasp_input_bridge_sdata_settings.h
 * Author: david
 *
 * Created on 3 de septiembre de 2014, 12:42
 */

#ifndef GRASP_INPUT_BRIDGE_SDATA_SETTINGS_H
#define	GRASP_INPUT_BRIDGE_SDATA_SETTINGS_H

#ifdef	__cplusplus
extern "C" {
#endif
    
#include "../../grasp_input_segment.h"

    typedef struct grasp_input_driver_settings_sdata_t_ {
        // Activate debug mode in sdata driver
        bool debug;
        // if a reference to input segment is provided, instead of read a file, this is returned
        // to know if the reference is provided, input file has to have the special name "."
        // This hack help python to provide a segment already loaded
        grasp_segment_t *input_segment;
        sensor_data_t   *input_sdata;
    }grasp_input_driver_settings_sdata_t;


#ifdef	__cplusplus
}
#endif

#endif	/* GRASP_INPUT_BRIDGE_SDATA_SETTINGS_H */

