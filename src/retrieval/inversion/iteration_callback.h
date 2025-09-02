/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* 
 * File:   iteration_callback.h
 * Author: david
 *
 * Created on October 25, 2017, 6:28 AM
 * 
 * This file helps to make callbacks to the C code from Fortran code.
 * The callback is performed after each iteration to get partial results
 */

#ifndef ITERATION_CALLBACK_H
#define ITERATION_CALLBACK_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * The function that will be called. It has to be passed as pointer to a 
 * function because it will be dinamically linked (no dependency at compilation time)
 * @param func Function will be called after each iteration
 */
void set_callback_function(void (*func)());


/**
 * This will make a call to the function set up if it is not null.
 */
void iteration_callback();


#ifdef __cplusplus
}
#endif

#endif /* ITERATION_CALLBACK_H */

