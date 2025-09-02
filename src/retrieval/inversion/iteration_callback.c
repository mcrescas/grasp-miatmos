/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* 
 * File:   interation_callback.c
 * Author: david
 *
 * Created on October 25, 2017, 3:38 AM
 */

#include <stdio.h>
#include <stdlib.h>


void (*callback_function)(); 

void set_callback_function(void (*func)()){
    callback_function=func;
}

void iteration_callback(){
    if(callback_function!=NULL){
        (*callback_function)();
    }
}
