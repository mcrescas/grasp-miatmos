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

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "python_grasp.h"
#include "yamlsettings/yamlsettings.h"
#include "../input/grasp_input.h"
#include "../output/grasp_output.h"
#include "../settings/grasp_settings.h"
#include <grasp/utils.h>
#include "../controller/grasp_main.h"
#include "../controller/grasp_controller.h"
#include "../controller/grasp_mpi_engine.h"
#include "../controller/mo_grasp_controller.h"
#include "../global/grasp_runtime_information.h"
#include "../global/grasp_retrieval_characteristic_type.h"
#include "../input/drivers/sdata/grasp_input_driver_sdata.h"
#include "../input/drivers/sdata/grasp_input_driver_sdata_error_codes.h"
#include "../output/grasp_output_segment_result.h"
#include "../input/grasp_input_segment.h"
#include "../global/grasp_error_codes.h"
#include "../output/grasp_output_tile_result_setters.h"
#include "mod_par_OS.inc"

/*
 * Values from the constants set that should be accessible from python
 * Follow the naming convention CONSTANTS_name, otherwise python won't know
 * what the variables are called.
 */
int CONSTANTS_KIMAGE=_KIMAGE;
int CONSTANTS_KPARS=_KPARS;
int CONSTANTS_KIEDGE=_KIEDGE;
int CONSTANTS_KIP=_KIP;
int CONSTANTS_NBVM=_NBVM;
int CONSTANTS_KSURF=_KSURF;
int CONSTANTS_KVERTM=_KVERTM;
int CONSTANTS_KNBVM=_KNBVM;
int CONSTANTS_KWM=_KWM;
int CONSTANTS_KIX=_KIX;
int CONSTANTS_KIY=_KIY;
int CONSTANTS_KITIME=_KITIME;
int CONSTANTS_KMESS=_KMESS;
int CONSTANTS_KMpar=_KMpar;
int CONSTANTS_KW=_KW;
int CONSTANTS_KSD=_KSD;
int CONSTANTS_KNT=_KNT;
int CONSTANTS_KNpar=_KNpar;
int CONSTANTS_KCpar=_KCpar;
int CONSTANTS_NRR=_NRR;
int CONSTANTS_KIDIM1=_KIDIM1;
int CONSTANTS_KIDIM2=_KIDIM2;
int CONSTANTS_KIDIM3=_KIDIM3;
int CONSTANTS_N_CHEM_MAX=_N_CHEM_MAX;
int CONSTANTS_NMG = _NMG;
int CONSTANTS_KVERT_WD = _KVERT_WD;
int CONSTANTS_KBF = _KBF;
int CONSTANTS_NMM = _NMM;

void this_is_just_here_so_that_setters_are_included_in_compiled_library() {
  set_grasp_output_tile_errest_par_errp(NULL, 0, 0, 0, 0, 0.);
}

int python_sequential(int argc, char **argv, grasp_results_description *python_result, grasp_segment_t *input_segment,
                      sensor_data_t *input_sdata) {
  // terminate on ctrl+c
  void* python_sigint_handler = signal(SIGINT, SIG_DFL);
  if(python_sigint_handler == SIG_ERR) {
    fprintf(stderr, "failed to set SIGINT handler\n");
    return 1;
  }
  int status = main_sequential(argc, argv, YS_PARSE_SETTINGS_INLINE, python_result, input_segment, input_sdata);
  // restore python's SIGINT handler
  signal(SIGINT, python_sigint_handler);
  return status;
}


void initialize_sdata(sensor_data_t *sdata) {
  memset(sdata, 0, sizeof(sensor_data_t));
}

int python_read_segment(char *file_name, grasp_segment_t *segment) {
  grasp_segment_initialize(segment);
  return python_read_sdata(file_name, &(segment->sdata));
}

int python_read_sdata(char *file_name, sensor_data_t *sdata) {
  int status;
  grasp_tile_description_t tile_description;
  char input_files[GRASP_INPUT_MAX_FILES][_GBL_FILE_PATH_LEN];
  grasp_segment_t *seg=NULL;

  initialize_sdata(sdata);
  seg=(grasp_segment_t *) trackmem_malloc(sizeof(grasp_segment_t));
  grasp_segment_initialize(seg);

  strcpy(input_files[0], file_name);
  status=grasp_input_initialize_tile_description(1, input_files, &tile_description);
  // initialize tile description already checks if the given file name exists
  // if it does not, it removes the file from the list of input files and returns
  // GRASP_ERROR_INPUT_NO_INPUT_FILES
  if (status<0) {
    grasp_controller_clean_memory(NULL, &tile_description, NULL, NULL, NULL);
    trackmem_free(seg);
    return GRASP_ERROR_SDATA_DRIVER_FILE_CANNOT_OPEN;
  }

  // We are goint to bypass the driver so we have to simulate the settings it needs to make the proper calls.
  status=grasp_input_driver_sdata_init(NULL, &tile_description);
  if (status<0) {
    grasp_controller_clean_memory(NULL, &tile_description, NULL, NULL, NULL);
    trackmem_free(seg);
    return status;
  }
  status=grasp_input_driver_sdata_get_segment(NULL, seg, 0, 0, 0);
  if (status<0) {
    grasp_controller_clean_memory(NULL, &tile_description, NULL, NULL, NULL);
    trackmem_free(seg);
    return status;
  }
  status=grasp_input_driver_sdata_close();
  if (status<0) {
    grasp_controller_clean_memory(NULL, &tile_description, NULL, NULL, NULL);
    trackmem_free(seg);
    return status;
  }

  memcpy(sdata, &(seg->sdata), sizeof(sensor_data_t)); // Copying results

  grasp_controller_clean_memory(NULL, &tile_description, NULL, NULL, NULL);
  trackmem_free(seg);
  return status;
}

int python_write_sdata(char *path, sensor_data_t *sdata) {
  FILE *file=fopen(path, "w");
  if (file==NULL) return -1;
  grasp_input_dump_sdata(file, sdata);

  fclose(file);

  return 0;
}

/**
    Return the memory index of the pixel [t, x, y]
    /!\ index t, x and y start at 1
    return -1 if the pixel doesn't exist
*/
int python_get_index(grasp_segment_t *seg, int t, int x, int y) {
  int len=grasp_segment_get_npixels(&seg->sdata);

  int i;
  for (i=0; i<len; i++) {
    if (grasp_segment_get_pixel_it(&seg->sdata, i)==t&&
        grasp_segment_get_pixel_ix(&seg->sdata, i)==x&&
        grasp_segment_get_pixel_iy(&seg->sdata, i)==y)
      return i;
  }

  return -1;
}


sensor_data_t *get_sensor() {
  sensor_data_t *sensor=(sensor_data_t *) malloc(sizeof(sensor_data_t));

  return sensor;
}


void free_ptr(void *ptr) {
  free(ptr);
}

/* DEPRECATED FUNCTION
int python_new_results_description(char *settings_text, grasp_results_description *results_description, int tile_nt,
                                   int tile_nx, int tile_ny) {
  int i, x, y, t;

  results_description->description=python_get_tile_description(tile_nt, tile_nx, tile_ny);
  if (results_description->description==NULL) {
    free(results_description);
    return -1;
  }
  results_description->results=python_get_new_results(settings_text, results_description->description);
  if (results_description->results==NULL) {
    free(results_description->description);
    free(results_description);
    return -1;
  }
  results_description->n_retrieved_pixels=tile_nt*tile_nx*tile_ny;
  results_description->n_error_pixels=tile_nt*tile_nx*tile_ny;

  // Set the internal index of each pixel
  i=0;
  for (t=0; t<tile_nt; t++) {
    for (x=0; x<tile_nx; x++) {
      for (y=0; y<tile_ny; y++) {
        results_description->results->segment_result[0].pixel_result[i].information.out_t=t;
        results_description->results->segment_result[0].pixel_result[i].information.out_x=x;
        results_description->results->segment_result[0].pixel_result[i].information.out_y=y;
        i++;
      }
    }
  }
  results_description->results->segment_result[0].npixel=i;


  // Reindex the results (based on previous indexes) in order to have access to pixel values (instead of segment values)
  python_reindex_results(results_description);

  return 0;
}
 */

grasp_settings *python_read_settings(char *settings_text) {
  grasp_settings *settings=NULL;
  char *argv[]={"/grasp", NULL, NULL};
  int status;

  argv[1]=settings_text;

  status=grasp_controller_read_settings(2, argv, &settings, YS_PARSE_SETTINGS_INLINE);

  // We close the streams opened by previous function in order to free full memory.
  grasp_output_stream_destroy(grasp_controller_get_trackmem_stream());
  grasp_output_stream_destroy(grasp_controller_get_stream());

  if (status<0) {
    trackmem_free(settings);
    return NULL;
  }


  return settings;
}

//nsegments=tile_description.dimensions.segment_ntimes*tile_description.dimensions.segment_nx*tile_description.dimensions.segment_ny
void python_reindex_results(grasp_results_description *result_description) {
  pixel_result_t *output_data;
  int i, ipixel, isegment;
  // Reindex the data to a pixel tile instead of a segment->sdata tile
  for (isegment=0; isegment<result_description->description->dimensions.segment_ntimes*
                            result_description->description->dimensions.segment_ncols*
                            result_description->description->dimensions.segment_nrows; isegment++) {
    for (ipixel=0; ipixel<result_description->results->segment_result[isegment].npixel; ipixel++) {
      output_data=&(result_description->results->segment_result[isegment].pixel_result[ipixel]);
      i=index3D(output_data->information.out_t, output_data->information.out_x, output_data->information.out_y,
                result_description->results->information.tile_npixels_x,
                result_description->results->information.tile_npixels_y);
      assert(i<result_description->results->information.tile_npixels_t*
               result_description->results->information.tile_npixels_x*
               result_description->results->information.tile_npixels_y);
      result_description->results->tile_result_map[i]=output_data;
    }
  }
}

grasp_tile_description_t *python_get_tile_description(int tile_nt, int tile_nx, int tile_ny) {
  grasp_tile_description_t *tile_description=malloc(sizeof(grasp_tile_description_t));

  //hack to avoid the call to grasp_input_initialize_tile_description(0,NULL, tile_description). I initialize the tile_description manually
  memset(tile_description, 0, sizeof(grasp_tile_description_t));
  tile_description->dimensions.segment_ntimes=1;
  tile_description->dimensions.segment_ncols=1;
  tile_description->dimensions.segment_nrows=1;
  tile_description->dimensions.tile_nt=tile_nt;
  tile_description->dimensions.tile_nx=tile_nx;
  tile_description->dimensions.tile_ny=tile_ny;

  return tile_description;
}



int python_debug_settings(int argc, char **argv, char **settings_debug, char **error_string){
    int i, errors;
    yamlsettings_dictionary_t *dictionary;
    grasp_settings *settings;

    char* error_buffer = NULL;
    size_t error_buffer_size = 0;
    char* settings_buffer = NULL;
    size_t settings_buffer_size = 0;


    FILE* settings_stream = open_memstream(&settings_buffer, &settings_buffer_size);
    FILE* error_stream = open_memstream(&error_buffer, &error_buffer_size);


    // Allocate settings
    settings=(grasp_settings *)trackmem_malloc(sizeof(grasp_settings));
    memset(settings, 0, sizeof(grasp_settings));
    assert(settings!=NULL);

    // Get dictionary for allocated settings
    dictionary=grasp_settings_dictionary_get(settings);

    // Read file
    errors=yamlsettings_read_file(dictionary, argc,(const char **)argv, NULL, grasp_settings_postprocess_function, YS_PARSE_SETTINGS_INLINE);


    // Print parse errors
    for (i = 0; i < dictionary->status.parse_errors->len; i++) {
        if(((yamlsettings_parse_error *)(dictionary->status.parse_errors->pdata[i]))->type==YS_ERROR){
            fprintf(error_stream, "ERROR: ");
        }else if(((yamlsettings_parse_error *)(dictionary->status.parse_errors->pdata[i]))->type==YS_FATAL_ERROR){
            fprintf(error_stream, "ERROR: ");
        }else if(((yamlsettings_parse_error *)(dictionary->status.parse_errors->pdata[i]))->type==YS_WARNING){
            fprintf(error_stream, "WARNING: ");
        }else if(((yamlsettings_parse_error *)(dictionary->status.parse_errors->pdata[i]))->type==YS_INFO){
            fprintf(error_stream, "INFO: ");
        }
        fprintf(error_stream, "%s\n", ((yamlsettings_parse_error *)(dictionary->status.parse_errors->pdata[i]))->message );
    }

    // Print validator erros if there are not parse errors
    for (i = 0; i < dictionary->status.validation_errors->len; i++) {
        fprintf(error_stream, "ERROR: %s\n", ((yamlsettings_validation_error *)(dictionary->status.validation_errors->pdata[i]))->message );
    }

    // Print error summary
    if((yamlsettings_error_number_parse_error(&dictionary->status, YS_ERROR) + yamlsettings_error_number_parse_error(&dictionary->status, YS_WARNING) + yamlsettings_error_number_validation_error(&dictionary->status) )>0){
        fprintf(error_stream, "Summarizing settings problems: %d parse warnings, %d parse errors, %d validation errors\n",
                yamlsettings_error_number_parse_error(&dictionary->status, YS_WARNING),
                yamlsettings_error_number_parse_error(&dictionary->status, YS_ERROR),
                yamlsettings_error_number_validation_error(&dictionary->status));
    }



    yamlsettings_debug_dictionary(settings_stream, dictionary, "");


    fclose(error_stream);
    fclose(settings_stream);
    yamlsettings_dictionary_complete_destroy(dictionary);

    *settings_debug=settings_buffer;
    *error_string=error_buffer;
    //printf("%s", settings_buffer);
    return -errors;
}


size_t sanity_check_segment_size() {
    return sizeof(grasp_segment_t);
}