/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */


/* 
 * File:   grasp_app.c
 * Author: David Fuertes
 *
 * Created on 15 May 2020, 16:38
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grasp_main.h"
#include "yamlsettings/yamlsettings.h"
#include "../input/grasp_input.h"
#include "../output/grasp_output.h"
#include "../settings/grasp_settings.h"
#include <grasp/utils.h>
#include "grasp_controller.h"
#include "grasp_mpi_engine.h"
#include "mo_grasp_controller.h"
#include "../global/grasp_runtime_information.h"

/**
 * Entry point to the application from terminal.
 * @param argc Number of input command line arguments
 * @param argv Input command line arguments
 * @return 0 if the process finished properly
 */ 
int main(int argc, char** argv) {
  int status;
  grasp_results_description *results_description;
  
  results_description = (grasp_results_description *) trackmem_malloc(sizeof (grasp_results_description));

#ifdef USE_MPI
  status = main_mpi(argc, argv, YS_PARSE_SETTINGS_FILE_MANDATORY, results_description, NULL,NULL);
#else
  status = main_sequential(argc, argv, YS_PARSE_SETTINGS_FILE_MANDATORY, results_description, NULL,NULL);
#endif
 
  grasp_controller_clean_memory(NULL, NULL, NULL, NULL, results_description);
  trackmem_free(results_description);
  
  // Some unix systems only allow return codes in the range (-127,128) and GRASP
  // uses integer values in the range of (min_int,max_int). If error code want
  // to be properly printed following code has to be uncommented or compile with debug:
#ifdef DEBUG
  if(status!=0){
      fprintf(stderr, "Exit with error code %d\n", status);
  }
#endif
  // Return code will be converted to a range (-127,128) so the value obtained
  // can be unexpected. Istead of "return status;" will just return if status 
  // code is positive, negative or 0 (no error).
  if (status<0) return -1;
  else if (status > 0 ) return 1;
  else return 0;
}


