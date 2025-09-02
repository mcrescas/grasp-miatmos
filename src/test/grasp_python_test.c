/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   test.c
 * Author: david
 *
 * Created on 31 de julio de 2017, 12:35
 */

#include <stdio.h>
#include <stdlib.h>

#include "../input/grasp_input_segment.h"
#include "../output/grasp_output_segment_result.h"
#include "../output/grasp_output_tile_result.h"
//#include "../output/tile_functions/python/grasp_output_tile_python.h"
#include "../settings/grasp_settings.h"
#include "../controller/python_grasp.h"
#include "../controller/grasp_main.h"

#include "../global/grasp_compilation_information.h"

int test1ph(int argc, char** argv){
    grasp_results_description r;
    char *settings=grasp_settings_to_string("examples/sunphotometer/settings_example_sunphotometer_inversion.yml");
    char* dummy_args[] = { "/grasp", settings, "output.segment.function=none" ,"retrieval.convergence.stop_before_performing_retrieval", NULL };
    //char* dummy_args[] = { "/grasp", settings, NULL };
    int status;
    
    argv = dummy_args;
    
            
    status = python_sequential(4, argv, &r, NULL, NULL); //change 2 by the number of arguments
    
    free(settings);
    
    grasp_controller_free_results_description(&r);
    
    return status;
}

int test1phSDATA(int argc, char** argv){
    grasp_results_description r;
    char *settings=grasp_settings_to_string("examples/sunphotometer/settings_example_sunphotometer_inversion.yml");
    char* dummy_args[] = { "/grasp", settings, "output.segment.function=none" ,"retrieval.convergence.stop_before_performing_retrieval", NULL };
    //char* dummy_args[] = { "/grasp", settings, NULL };
    int status;
    grasp_segment_t* seg = malloc(sizeof(grasp_segment_t));
    
    argv = dummy_args;
    
    python_read_segment("examples/sunphotometer/example_sunphotometer.sdat", seg);
    //python_read_segment("examples/polder/example_polder.sdat", seg);

    status=grasp_segment_validate(seg);
    if(status<0){
        abort();
    }
    status = python_sequential(4, argv, &r, seg, NULL); //change 2 by the number of arguments
    
    free(settings);
    free(seg);
    grasp_controller_free_results_description(&r);

    // now after one retrieval is done, trying to read another sdata file produces a segfault
    sensor_data_t* sd = malloc(sizeof(sensor_data_t));
    status = python_read_sdata("examples/sunphotometer/example_sunphotometer.sdat", sd);
    free (sd);
            
    return status;
}

int testSettingsJson(int argc, char** argv){
    char *text;
    int status;
    
    text = (char *) malloc(sizeof (char)*100000);
    
    status=grasp_settings_description_json(text, 100000);
    
    printf("%d\n%s",status,text);
    
    free(text);
    
    return 0;
}

int test1polder(int argc, char** argv){
    grasp_results_description r;
    char *settings=grasp_settings_to_string("examples/polder/settings_example_polder_inversion.yml");
    //char* dummy_args[] = { "/grasp", settings, "output.segment.function=none" ,"retrieval.convergence.stop_before_performing_retrieval", NULL };
    char* dummy_args[] = { "/grasp", settings,  NULL };
    int status;
    
    argv = dummy_args;
    
    status = python_sequential(2, argv, &r, NULL, NULL); //change 2 by the number of arguments
    
    free(settings);
    
    grasp_controller_free_results_description(&r);
    
    return status;
}

int test2(int argc, char** argv){
    grasp_segment_t *segment = (grasp_segment_t *) malloc(sizeof (grasp_segment_t)*1);
    
    python_read_segment("examples/polder/example_polder.sdat", segment);
    
    //fprintf(stderr, "%s:%d: debug: %d\n", __FILE__, __LINE__, segment->sdata.pixel[0].nwl);
    fprintf(stderr, "%s:%d: debug: %d\n", __FILE__, __LINE__, grasp_segment_get_npixels(&segment->sdata));
    
    return 0;
}


int test5(int argc, char** argv){
    grasp_results_description r = {NULL, NULL};
    
    char *settings=grasp_settings_to_string("examples/sunphotometer/settings_example_sunphotometer_inversion.yml");
    
    char* dummy_args[] = { "/grasp", settings,
                            "retrieval.convergence.stop_before_performing_retrieval", NULL };
                            
    int status;
    
    argv = dummy_args;
    
    status = python_sequential(3, argv, &r, NULL, NULL);
    
    //printf(">>> %f\n", grasp_output_tile_aerosol_sd2m_opt_ext_fine_mode(r.res, 4, 1, 1, 0));
    
    free(settings);
    
    return status;
}


int test6(int argc, char** argv){
    grasp_results_description r;
    
    char *settings=grasp_settings_to_string("examples/sunphotometer/settings_example_sunphotometer_inversion.yml");
    
    char* dummy_args[] = { "/grasp", ///Users/david/grasp/
                            settings,
                            "output.segment.function=none",
                            NULL };
    int status;
    
    argv = dummy_args;
    
    status = python_sequential(3, argv, &r, NULL, NULL);
    
    free(settings);
    
    return status;
}


// Fit for tiles
int test7b(int argc, char** argv){
    grasp_results_description r;
    
    char *settings=grasp_settings_to_string("examples/sunphotometer/settings_example_sunphotometer_inversion.yml");
    
    char* dummy_args[] = { "/grasp", ///Users/david/grasp/
                            settings,
                            "output.segment.function=none",
                            NULL };
    int status;
    
    argv = dummy_args;
    
    status = python_sequential(3, argv, &r, NULL, NULL);
    
    // The way to access to fitting information has changed so following lines got deprecated
    //fprintf(stderr, "%s:%d: debug: segment: %f ~= %f \n", __FILE__, __LINE__, grasp_segment_get_pixel_meas_i(&r.input_segments[0].sdata, 0, 0, 0), grasp_segment_get_pixel_meas_i(grasp_output_tile_retrieval_fit_segment_fit(r.res,0), 0, 0, 0));
    //fprintf(stderr, "%s:%d: debug: pixel:   %f ~= %f \n", __FILE__, __LINE__, grasp_segment_pixel_get_pixel_meas_i(&r.input_segments[0].sdata.pixel[0], 0, 0), grasp_segment_pixel_get_pixel_meas_i(grasp_output_tile_retrieval_fit_pixel_fit(r.res,0,0,0), 0, 0));

    free(settings);
    
    return status;
}

int test8(int argc, char** argv){
    grasp_segment_t* s = (grasp_segment_t *) malloc(sizeof (grasp_segment_t)*1);
    grasp_segment_initialize(s);
    python_read_segment("examples/polder/example_polder.sdat", s);
    python_write_sdata("lala.sdat", &(s->sdata));
    
    return 0;
}

int test9(int argc, char** argv){
    grasp_segment_t* s = (grasp_segment_t *) malloc(sizeof (grasp_segment_t)*1);
    python_read_segment("examples/polder/example_polder.sdat", s);
//    printf(">>> %f\n", s->iguess[1][0]);
    
    return 0;
}


/*
 * sdata [get/set]_pixel_meas_mprof
 */
void test12(){
	grasp_segment_t* sdata = (grasp_segment_t *) malloc(sizeof (grasp_segment_t)*1);
    python_read_segment("examples/polder/example_polder.sdat", sdata);

	printf("grasp_segment_get_pixel_meas_mprof\n");

	printf(">>> %f\n", grasp_segment_get_pixel_meas_mprof(&sdata->sdata, 0, 0, 0, 0));

	grasp_segment_set_pixel_meas_mprof(&sdata->sdata, 0, 0, 0, 0, 1234.);

	printf(">>> %f\n", grasp_segment_get_pixel_meas_mprof(&sdata->sdata, 0, 0, 0, 0));
}

/**
 * [Tile]
 * Test: longitude
 */
int test14(){
    grasp_results_description r;
    char *settings=grasp_settings_to_string("examples/polder/settings_example_polder_inversion.yml");
    char* dummy_args[] = { "/grasp", settings, "retrieval.convergence.stop_before_performing_retrieval", NULL };
    int status;

    status = python_sequential(3, dummy_args , &r, NULL, NULL);

    free(settings);


    printf(">>> index 0,0 : %f\n", grasp_output_tile_pixel_information_longitude(r.results, 0, 0, 0));
    printf(">>> index 0,1 : %f\n", grasp_output_tile_pixel_information_longitude(r.results, 0, 0, 1));
    printf(">>> index 1,0 : %f\n", grasp_output_tile_pixel_information_longitude(r.results, 0, 1, 0));
    printf(">>> index 1,1 : %f\n", grasp_output_tile_pixel_information_longitude(r.results, 0, 1, 1));


    grasp_controller_free_results_description(&r);

    return status;
}

/**
 * [Tile]
 * Test: aerosol_opt_ssat
 */
int test15(){
    grasp_results_description r;
    char *settings=grasp_settings_to_string("examples/polder/settings_example_polder_inversion.yml");
    char* dummy_args[] = { "/grasp", settings, "retrieval.convergence.stop_before_performing_retrieval", NULL };
    int status;

    status = python_sequential(3, dummy_args , &r, NULL, NULL);

    free(settings);

    int i;

    for(i = 0; i < 6; i++){
    	printf("========== wl %d ==========\n", i);
		printf(">>> index 0,0 : %f\n", grasp_output_tile_aerosol_opt_ssat(r.results, 0, 0, 0, i));
		printf(">>> index 0,1 : %f\n", grasp_output_tile_aerosol_opt_ssat(r.results, 0, 0, 1, i));
		printf(">>> index 1,0 : %f\n", grasp_output_tile_aerosol_opt_ssat(r.results, 0, 1, 0, i));
		printf(">>> index 1,1 : %f\n", grasp_output_tile_aerosol_opt_ssat(r.results, 0, 1, 1, i));
    }

    grasp_controller_free_results_description(&r);

    return status;
}

/*
 * sdata grasp_segment_get_pixel_hvp
 */
void test16(){
	grasp_segment_t* sdata = (grasp_segment_t *) malloc(sizeof (grasp_segment_t)*1);
    python_read_segment("examples/polder/example_polder.sdat", sdata);

	printf("grasp_segment_get_pixel_hvp\n");

	printf(">>> 0: %f\n", grasp_segment_get_pixel_hvp(&sdata->sdata, 0, 0));
	printf(">>> 1: %f\n", grasp_segment_get_pixel_hvp(&sdata->sdata, 0, 1));
	printf(">>> 2: %f\n", grasp_segment_get_pixel_hvp(&sdata->sdata, 0, 2));
	printf(">>> 3: %f\n", grasp_segment_get_pixel_hvp(&sdata->sdata, 0, 3));

	// grasp_segment_set_pixel_meas_mprof(&sdata->sdata, 0, 0, 0, 0, 1234.);

	// printf(">>> %f\n", grasp_segment_get_pixel_meas_mprof(&sdata->sdata, 0, 0, 0, 0));
}
void test17(){
//    char* lili = malloc(sizeof(char)*500);
//    char* tmp = malloc(sizeof(char)*500);
//
//     sprintf(lili, "%d - %d = %d", 2,3,5);
//     sprintf(tmp, "%s - %s = %d", "deux", "trois", 5);
//     strcat(lili, tmp);
//
//
//    printf("%s\n", lili);
//
//    free(lili);

    char* str = grasp_compilation_information_str();

    printf("%s\n", str);


}

int test(int argc, char** argv){
    grasp_results_description r;
    char *settings=grasp_settings_to_string("/grasp/home/ZEN/processing/settings_ZEN_singlepixel_n_1_45_k_0_005_sf_0_01.yml");
    //char* dummy_args[] = { "/grasp", settings, "output.segment.function=none" ,"retrieval.convergence.stop_before_performing_retrieval", NULL };
    char* dummy_args[] = { "/grasp", settings, NULL };
    int status;

    argv = dummy_args;

    status = python_sequential(2, argv, &r, NULL, NULL); //change 2 by the number of arguments
    grasp_controller_free_results_description(&r);
    status = python_sequential(2, argv, &r, NULL, NULL); //change 2 by the number of arguments
    grasp_controller_free_results_description(&r);
    free(settings);



    return status;
}



/* DEPRECRATED
 * This test shows how to get a tile results strucure allocated in memory.
 * @param argc
 * @param argv
 * @return 
 *
int testCreateResults(int argc, char** argv){
    int status;
    char *settings_text=grasp_settings_to_string("examples/lidar_and_sunphotometer/settings_example_lidar_sunphotometer_inversion.yml");
    grasp_results_description *results_description = (grasp_results_description *) malloc(sizeof (grasp_results_description));
    
    
    // 1) Create the memory allocation for 6 pixels (t=6,x=1,y=1)
    status=python_new_results_description(settings_text,results_description, 3,1,1);

    
    // 2) ... python stuff ...
    
    
    // 3) Free the memory:
    grasp_controller_free_results_description(results_description); 
    
    
    
    // Resources allocatesd by this example (they should come allocated from python)
    free(results_description); //-> free_ptr
    free(settings_text);
    
    return status;
}
*/

int testDebugSettings(int argc, char** argv){
    char *settings_text=grasp_settings_to_string("examples/lidar_and_sunphotometer/settings_example_lidar_sunphotometer_inversion.yml");
    //char *dummy_args[3] = { settings_text, "input.driver=sdata", NULL };
    //char *dummy_args[3] = { settings_text, "asd.asd=3", NULL };
    char *dummy_args[2] = { settings_text, NULL };
    char *settings_debug;
    char *error_string;
    int errors;
    
    // Remember to change the first number to the number of parameters of the test
    errors=python_debug_settings(1, dummy_args, &settings_debug, &error_string);

    printf("%s", settings_debug);
    printf("%s", error_string);
    
    free(settings_text);
    
    printf("%d",errors);
    
    // Freeing all memory, python part
    free(settings_debug);
    free(error_string);
    
    return errors;
}


int testOutputFunction(int argc, char** argv){
    grasp_settings *settings = (grasp_settings *)malloc(sizeof(grasp_settings));
    memset(settings, 0, sizeof(grasp_settings));

    grasp_results_description r;
    char *settings_text=grasp_settings_to_string("examples/sunphotometer/settings_example_sunphotometer_inversion.yml");
    char* dummy_args[] = { "/grasp", ///Users/david/grasp/
                           settings_text,
                           "output.segment.function=none",
                           NULL };
    argv = dummy_args;

    int status;
    status = python_sequential(3, argv, &r, NULL, NULL);

    //grasp_output_tile_function_python_init(settings, r.description);
    //grasp_output_tile_function_python_process(NULL, settings, r.description, r.results);

    free(settings);

    return status;
}

/*
 *
 */
int main(int argc, char** argv) {
    //test1phSDATA(argc,argv);
    //testSettingsJson(argc,argv);
    //testCreateResults(argc,argv);
    //testDebugSettings(argc,argv);
    testOutputFunction(argc,argv);
    
    return 0;
}



