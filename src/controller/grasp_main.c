/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* 
 * File:   main.c
 * Author: David Fuertes
 *
 * Created on 29 de septiembre de 2013, 22:27
 * Adapted for MPI by Fabrice Ducos, last change 8 october 2014
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

#ifdef USE_MPI
#include "mpi.h"
#endif

typedef enum benchmark_enum_ {
  BENCHMARK_SEQ,
  BENCHMARK_MPI
} benchmark_enum;

static void print_benchmark_information(const grasp_results_description *results_description, const benchmark_t *benchmark, benchmark_enum benchmark_output) {
  grasp_output_stream *stream;
  assert(results_description != NULL);
  assert(results_description->results != NULL);
  stream=grasp_controller_get_stream();
  if(grasp_output_tile_information_tile_npixels(results_description->results)>0){
    gos_fprintf(stream, "Total Time: %d pixels processed in %f seconds (cpu time: %f). Average per pixel: %f (cpu time: %f)\n",        grasp_output_tile_information_tile_npixels(results_description->results), benchmark->delta_ct,                                      benchmark->delta_ut,                                     benchmark->delta_ct/grasp_output_tile_information_tile_npixels(results_description->results),                                       benchmark->delta_ut/grasp_output_tile_information_tile_npixels(results_description->results));
    gos_fprintf(stream, "Algorithm Time: %d pixels processed in %f seconds (cpu time: %f). Average per pixel: %f (cpu time: %f)\n",    grasp_output_tile_information_tile_npixels(results_description->results), grasp_controller_get_algorithm_ct(),                     grasp_controller_get_algorithm_ut(),                    grasp_controller_get_algorithm_ct()/grasp_output_tile_information_tile_npixels(results_description->results),                      grasp_controller_get_algorithm_ut()/grasp_output_tile_information_tile_npixels(results_description->results));

    if (benchmark_output == BENCHMARK_SEQ) {
        gos_fprintf(stream, "Control Unit Time: %d pixels processed in %f seconds (cpu time: %f). Average per pixel: %f (cpu time: %f)\n", grasp_output_tile_information_tile_npixels(results_description->results), benchmark->delta_ct-grasp_controller_get_algorithm_ct(),  benchmark->delta_ut-grasp_controller_get_algorithm_ut(), (benchmark->delta_ct-grasp_controller_get_algorithm_ct())/grasp_output_tile_information_tile_npixels(results_description->results), (benchmark->delta_ct-grasp_controller_get_algorithm_ut())/grasp_output_tile_information_tile_npixels(results_description->results));
    }
  }
  if(grasp_controller_get_nerror_segment()>0){
    gos_fprintf(stream, "RETRIEVAL ERROR: During the retrieval process %d segments (%d pixels) were ignored because retrieval code returned an error\n", grasp_controller_get_nerror_segment(), grasp_controller_get_nerror_pixel());
  }
  grasp_output_stream_destroy(stream);
}

int main_sequential(int argc, char** argv, yamlsettings_parser_settings_file_mode settings_file_mode, grasp_results_description *results_description, grasp_segment_t *input_segment, sensor_data_t *input_sdata) {
  grasp_settings *settings=NULL;
  grasp_processing_functions_t functions;
  grasp_tile_description_t* tile_description = NULL;
  grasp_results_t* results = NULL;
  benchmark_t benchmark;
  int status;
  
  benchmark_start(&benchmark);

  // 0) Init output
  results_description->results = NULL;
  results_description->description = NULL;
  results_description->n_retrieved_pixels=0;
  results_description->n_error_pixels=0;

  // 1) Read settings and process controller options
  status=grasp_controller_read_settings(argc, argv, &settings, settings_file_mode);
  if(status<0){
      grasp_controller_clean_memory(settings,NULL, NULL,NULL,results_description);
      return status;
  }
  
  // If input_segment is provided we ignore the input driver and use segment_pointer driver
  if(input_segment!=NULL || input_sdata!=NULL){
      strcpy(settings->input.driver_name,"sdata");
      settings->input.nfiles=1;
      strcpy(settings->input.files[0],".");
      if(input_segment!=NULL){
        settings->input.driver.sdata.input_segment=input_segment;
        settings->input.driver.sdata.input_sdata=&(input_segment->sdata);
      }else{
        settings->input.driver.sdata.input_segment=NULL;
        settings->input.driver.sdata.input_sdata=input_sdata;
      }
  }

  // 2) Get tile information
  results = (grasp_results_t*) malloc(sizeof(grasp_results_t));
  tile_description = (grasp_tile_description_t*) malloc(sizeof(grasp_tile_description_t));
  results_description->results = results;
  results_description->description = tile_description;
  status=grasp_controller_initialize_inversion(settings, tile_description, &functions, results);
  if(status<0){
      grasp_controller_clean_memory(settings,NULL, NULL,&functions,results_description);
      return status;
  }

  // 3) Invert tile data
  status=grasp_controller_invert_tile(settings, tile_description, results,  &functions);
  if(status<0){
      grasp_controller_clean_memory(settings,NULL, NULL,&functions,results_description);
      return status;
  }

   results_description->n_retrieved_pixels=grasp_output_tile_information_tile_npixels(results);
   results_description->n_error_pixels=grasp_controller_get_nerror_pixel();

  // 4) Manage output
  status=grasp_controller_manage_tile(settings, tile_description, results, &functions);
  if(status<0){
      grasp_controller_clean_memory(settings,NULL, NULL,&functions,results_description);
      return status;
  }

  benchmark_stop(&benchmark);
  
  // Print benchmark information in case there was not error at retrieval time.
  if(status==0){
    print_benchmark_information(results_description, &benchmark, BENCHMARK_SEQ);
  }
  
  // 5) Finalizing
  grasp_controller_clean_memory(settings,NULL, NULL,&functions,NULL); // We do not free python_results because it is the output to python
  
  return status;
}


#ifdef USE_MPI

void cleanup() {
  MPI_Finalize();
}

int main_mpi(int argc, char** argv, yamlsettings_parser_settings_file_mode settings_file_mode, grasp_results_description *results_description, grasp_segment_t *input_segment, sensor_data_t *input_sdata) {
  grasp_settings *settings=NULL;
  grasp_processing_functions_t functions;
  grasp_tile_description_t *tile_description = NULL;
  grasp_results_t *results = NULL;
  benchmark_t benchmark;
  int status;
  
  atexit(cleanup);
  MPI_Init(&argc, &argv);

  benchmark_start(&benchmark);
 
  // 0) Init output
  results_description->results = NULL;
  results_description->description = NULL;
  results_description->n_retrieved_pixels=0;
  results_description->n_error_pixels=0;
 
  // 1) Read settings and process controller options
  status=grasp_controller_read_settings(argc, argv, &settings, settings_file_mode);
  if(status<0) {
    grasp_controller_clean_memory(settings, NULL, NULL, NULL, results_description);
    return status;
  }  

    /* in MPI mode, only the master has to load the tile */
  if (grasp_mpi_engine_is_master()) {
    // 2) Get Tile information

    results = (grasp_results_t*) malloc(sizeof(grasp_results_t));
    tile_description = (grasp_tile_description_t*) malloc(sizeof(grasp_tile_description_t));
    results_description->results = results;
    results_description->description = tile_description;

    status = grasp_controller_initialize_inversion(settings, tile_description, &functions, results);
    if (status < 0) {
      grasp_controller_clean_memory(settings,NULL, NULL,&functions,results_description);
      return status;
    } 

    // 3) Invert tile data
    status = grasp_controller_invert_tile(settings, tile_description, results, &functions);
    if (status < 0) {
      grasp_controller_clean_memory(settings,NULL, NULL,&functions,results_description);
      return status;
    }

   results_description->n_retrieved_pixels=grasp_output_tile_information_tile_npixels(results);
   results_description->n_error_pixels=grasp_controller_get_nerror_pixel();

    // 4) Manage output
    status = grasp_controller_manage_tile(settings,tile_description, results, &functions);
    if (status < 0) {
      grasp_controller_clean_memory(settings,NULL, NULL,&functions,results_description);
      return status;
    }
 
  }
  else { /* no tile is loaded for the workers, but they have to start the processing too */
    /* There are three reasons for which the workers do not (and MUST not) call grasp_controller_initialize_inversion():
     * - it would make the startup with large tiles even slower than before, because the same data would have to be loaded by each process (masters and workers). Especially, there could be concurrent access on the same files, and that could dramatically impact the performance for network mounted filesystems.
     * - it would be a considerable waste of memory (in production, huge 3D tiles would be allocated for each worker); it would prevent us to run as many tiles in parallel as we did in the past
     * - each worker could actually initialize the tile, the output functions and the results, but they would make no use of them.
     */
    
    status = grasp_controller_invert_tile(settings, NULL, NULL, NULL);
  } /* if (grasp_mpi_engine_is_master()) */

  benchmark_stop(&benchmark);
  // Print benchmark information in case there was not error at retrieval time.
  if(status==0 && grasp_mpi_engine_is_master()){
    print_benchmark_information(results_description, &benchmark, BENCHMARK_MPI);
  }
 
  // 5) Finalizing
  grasp_controller_clean_memory(settings,NULL, NULL,&functions,NULL); // We do not free python_results because it is the output to python
 
  return status;
}
#endif /* USE_MPI */


