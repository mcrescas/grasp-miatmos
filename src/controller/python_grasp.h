#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "../input/grasp_input_tile_description.h"
#include "../output/grasp_output_tile_result.h"
#include "../output/grasp_output_segment_result.h"
#include "../controller/grasp_main.h"


/**
 * Entry point to GRASP. Used by grasp.code.run(...)
 */
int python_sequential(int argc, char **argv, grasp_results_description *python_result, grasp_segment_t *input_segment,
                      sensor_data_t *sdata);

/**
 * Load the sdata file with the given name from disk.
 * Used by grasp.code.SDATA('file.sdat')
 */
int python_read_segment(char *file_name, grasp_segment_t *segment);

int python_read_sdata(char *file_name, sensor_data_t *sdata);

int python_write_sdata(char *path, sensor_data_t *sdata);

int python_get_index(grasp_segment_t *seg, int t, int x, int y);

sensor_data_t *get_sensor();

// General functions/tools required by Python interface

void free_ptr(void *ptr);

// Functions generated to first attemp of persist results. 
// For the moment this solution is temporal
// Current limitation: currently everything is considered as a single segment with all pixels.

// public functions


/**
 * This function need to be called if results structure is modified
 * @param results modified structure
 * @param tile_description it defined the dimensions of the results
 */
void python_reindex_results(grasp_results_description *results_description);

// internal functions

/**
 * This function return a tile_description structure from the sizes of the tile
 * @param segment_ntimes Number of segments it contains in T dimension
 * @param segment_ncols Number of segments it contains in X dimension 
 * @param segment_nrows Number of segments it contains in Y dimension
 * @param tile_nt Number of pixels it contains in T dimension
 * @param tile_nx Number of pixels it contains in X dimension
 * @param tile_ny Number of pixels it contains in Y dimension
 * @return allocated tile_description structure or NULL in case of error
 */
grasp_tile_description_t *python_get_tile_description(int tile_nt, int tile_nx, int tile_ny);

/**
 * Read functions and return the structure. 
 * @param settings_text Settings in 'inline' way
 * @return allocated settins strucuture or NULL in case of error
 */
grasp_settings *python_read_settings(char *settings_text);

/**
 * 
 * @param argc number of arguments as argc
 * @param argv arguments first has to be settings in inline format
 * @param settings_debug information of debug settings into screen
 * @param error_string text of errors
 * @return 0 if everything works, otherwise number of errors
 */
int python_debug_settings(int argc, char **argv, char **settings_debug, char **error_string);


/**
 * @brief Returns byte of the the segment structure.
 * Useful as a quick sanity check that the constants set loaded by the grasp
 * python library is the same one as the one compiled in the binary.
 * 
 * @return sizeof(grasp_segment_t) 
 */
size_t sanity_check_segment_size();