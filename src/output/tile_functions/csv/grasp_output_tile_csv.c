/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include "grasp_output_tile_csv.h"
#include "../../../global/grasp_retrieval_characteristic_type.h"
#include <grasp/utils.h>
#include <inttypes.h>
#include <grasp/utils.h>

#include "yamlsettings/yamlsettings.h"
#include "yamlsettings/yamlsettings_dictionary.h"
#include "yamlsettings/yamlsettings_assign_data.h"
#include "yamlsettings/yamlsettings_validators.h"
#include "yamlsettings/yamlsettings_data_types.h"
#include "grasp_output_tile_csv_settings.h"

#ifdef ZLIB_FOUND
/* 
   from: http://zlib.net/zpipe.c and https://stackoverflow.com/questions/13762918/setup-zlib-to-generate-gzipped-data
   zpipe.c: example of proper use of zlib's inflate() and deflate()
   Not copyrighted -- provided to the public domain
   Version 1.4  11 December 2005  Mark Adler 
 */
#include "zlib.h"


#if defined(MSDOS) || defined(OS2) || defined(WIN32) || defined(__CYGWIN__)
#  include <fcntl.h>
#  include <io.h>
#  define SET_BINARY_MODE(file) setmode(fileno(file), O_BINARY)
#else
#  define SET_BINARY_MODE(file)
#endif

#define CHUNK 16384

/* Compress from file source to file dest until EOF on source.
   def() returns Z_OK on success, Z_MEM_ERROR if memory could not be
   allocated for processing, Z_STREAM_ERROR if an invalid compression
   level is supplied, Z_VERSION_ERROR if the version of zlib.h and the
   version of the library linked do not match, or Z_ERRNO if there is
   an error reading or writing the files. */
static int def(FILE *source, FILE *dest, int level)
{
    int ret, flush;
    unsigned have;
    z_stream strm;
    unsigned char in[CHUNK];
    unsigned char out[CHUNK];

    /* allocate deflate state */
    strm.zalloc = Z_NULL;
    strm.zfree = Z_NULL;
    strm.opaque = Z_NULL;
    // Following line is commented to export in gz format instead of Z format
    //ret = deflateInit(&strm, level);
    // This line allow to export in gz format.
    ret = deflateInit2 (&strm, level, Z_DEFLATED,15+16,8,Z_DEFAULT_STRATEGY);
    if (ret != Z_OK)
        return ret;

    /* compress until end of file */
    do {
        strm.avail_in = fread(in, 1, CHUNK, source);
        if (ferror(source)) {
            (void)deflateEnd(&strm);
            return Z_ERRNO;
        }
        flush = feof(source) ? Z_FINISH : Z_NO_FLUSH;
        strm.next_in = in;

        /* run deflate() on input until output buffer not full, finish
           compression if all of source has been read in */
        do {
            strm.avail_out = CHUNK;
            strm.next_out = out;
            ret = deflate(&strm, flush);    /* no bad return value */
            assert(ret != Z_STREAM_ERROR);  /* state not clobbered */
            have = CHUNK - strm.avail_out;
            if (fwrite(out, 1, have, dest) != have || ferror(dest)) {
                (void)deflateEnd(&strm);
                return Z_ERRNO;
            }
        } while (strm.avail_out == 0);
        assert(strm.avail_in == 0);     /* all input will be used */

        /* done when last data in file processed */
    } while (flush != Z_FINISH);
    assert(ret == Z_STREAM_END);        /* stream will be complete */

    /* clean up and return */
    (void)deflateEnd(&strm);
    return Z_OK;
}
#endif

grasp_output_tile_function_t grasp_output_tile_function_csv(){
    grasp_output_tile_function_t x;
    
    x.init=grasp_output_tile_function_csv_init;
    x.function=grasp_output_tile_function_csv_process;
    x.close=grasp_output_tile_function_csv_close;
    
    return x;
}

int grasp_output_tile_function_csv_init(grasp_settings *settings, grasp_tile_description_t *input_information){
    return 0;
}

int grasp_output_tile_function_csv_close(void){
    return 0;
}

int grasp_output_tile_function_csv_process(grasp_output_stream *stream, grasp_settings *settings, grasp_tile_description_t *tile_description, grasp_results_t *results){
    int itime, ix, iy, iwln,inoise, iparam, isd, ipixel, ihlv, sd_index, j, igrid;
    float aerosol_concentration;
    char str_date_time[20 + 1];
    char param_title[64];
    FILE *f;
    char sep;
    int ichem;
    char *original_name;
    bool screen, writable;
#ifdef ZLIB_FOUND
    int zerr_code;
    FILE *fc;
    char *compress_name;
#endif
    
    sep=settings->output.tile_function.csv.delimiter;
    
    if(grasp_output_tile_information_tile_npixels(results)<=0){
        fprintf(stderr, "The tile is empty. Results with csv tile output function will not be printed in %s.\n", stream->filename );
    }else{  
        f=grasp_output_stream_open(stream, settings, NULL, NULL, &tile_description->dimensions, -1, -1, -1);
        if(grasp_output_stream_writable(stream)==false){ // If it is not writable, close stream and finish function
            grasp_output_stream_close(stream);
            grasp_output_stream_destroy(stream); 
            return 0;
        }
    
        fprintf(f,"row%ccol%cdatetime%cunixtimestamp%clon%clat%cniterations", sep,sep,sep,sep,sep,sep);

        if(settings->output.tile_function.csv.show_timing){
            fprintf(f,"%creal_time%cuser_time",sep, sep);
        }

        for (inoise = 0; inoise < settings->retrieval.NOISE.INOISE; inoise++) {
            fprintf(f,"%cresidual_relative_noise%d", sep, inoise);
        }   
        for (inoise = 0; inoise < settings->retrieval.NOISE.INOISE; inoise++) {
            fprintf(f,"%cresidual_absolute_noise%d", sep, inoise);
        }   

        fprintf(f, "%cland_percent",sep);        
        fprintf(f, "%ccloud_mask",sep);

        if(grasp_output_tile_products_retrieval_par(results)){
            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                grasp_parameters_get_characteric_type_pretty_name_by_parameter_number(&(settings->retrieval.NDIM), iparam, false, settings->retrieval.WAVE, settings->retrieval.IWW_SINGL, param_title, 64);
                fprintf(f, "%c%s", sep, param_title);
            }               
        }

        if(grasp_output_tile_products_aerosol_opt(results)){
            fprintf(f,"%cAExp",sep);
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%ctau%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cssa%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%caaod%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }            
            
            if(settings->retrieval.NSD>1){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%ctau%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cssa%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%caaod%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                }
            }
        }
                
        if(grasp_output_tile_products_aerosol_sd2m_ext(results)){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%ctauF%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }

            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%ctauC%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }  
        }
        
        if(grasp_output_tile_products_aerosol_sd2m_mph(results)){
            fprintf(f,"%csd_volume_concentration_total", sep);
            fprintf(f,"%csd_volume_concentration_fine", sep);
            fprintf(f,"%csd_volume_concentration_coarse", sep);
            fprintf(f,"%csd_volume_mean_radius_total", sep);
            fprintf(f,"%csd_volume_mean_radius_fine", sep);
            fprintf(f,"%csd_volume_mean_radius_coarse", sep);
            fprintf(f,"%csd_standard_deviation_total", sep);
            fprintf(f,"%csd_standard_deviation_fine", sep);
            fprintf(f,"%csd_standard_deviation_coarse", sep);
            fprintf(f,"%csd_effective_radii_total", sep);
            fprintf(f,"%csd_effective_radii_fine", sep);
            fprintf(f,"%csd_effective_radii_coarse", sep);
        }

        
        if(grasp_output_tile_products_aerosol_lidar(results)){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%clidar_ratio%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            if(settings->retrieval.NSD>1){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%clidar_ratio%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                }
            }
        }
        
        if(grasp_output_tile_products_forcing_bbflux(results)){
           ipixel=0;
           while (results->tile_result_map[ipixel] == NULL) ipixel++;
           for (ihlv = 0; ihlv < grasp_output_tile_forcing_bbflux_nhlv(results,0,0,ipixel); ihlv++) {
              fprintf(f,"%cbbufx0%0.0fkm", sep, grasp_output_tile_forcing_bbflux_hlv(results,0,0,ipixel, ihlv));
           }
           for (ihlv = 0; ihlv < grasp_output_tile_forcing_bbflux_nhlv(results,0,0,ipixel); ihlv++) {
              fprintf(f,"%cbbdfx0%0.0fkm", sep, grasp_output_tile_forcing_bbflux_hlv(results,0,0,ipixel, ihlv));
           }
           for (ihlv = 0; ihlv < grasp_output_tile_forcing_bbflux_nhlv(results,0,0,ipixel); ihlv++) {
              fprintf(f,"%cbbufxa%0.0fkm", sep, grasp_output_tile_forcing_bbflux_hlv(results,0,0,ipixel, ihlv));
           }
           for (ihlv = 0; ihlv < grasp_output_tile_forcing_bbflux_nhlv(results,0,0,ipixel); ihlv++) {
              fprintf(f,"%cbbdfxa%0.0fkm", sep, grasp_output_tile_forcing_bbflux_hlv(results,0,0,ipixel, ihlv));
           }
        }
        
        if(grasp_output_tile_products_forcing_forcing(results)){
           ipixel=0;
           while (results->tile_result_map[ipixel] == NULL) ipixel++;
           for (ihlv = 0; ihlv < grasp_output_tile_forcing_forcing_nhlv(results,0,0,ipixel); ihlv++) {
              fprintf(f,"%cnetforc%0.0fkm", sep, grasp_output_tile_forcing_forcing_hlv(results,0,0,ipixel, ihlv));
           }
           for (ihlv = 0; ihlv < grasp_output_tile_forcing_forcing_nhlv(results,0,0,ipixel); ihlv++) {
              fprintf(f,"%cforceff%0.0fkm", sep, grasp_output_tile_forcing_forcing_hlv(results,0,0,ipixel, ihlv));
           }
        }

        if(grasp_output_tile_products_surface_surf(results)){
            fprintf(f,"%cndvi",sep);
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cdhr%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
        } 
        
        if(grasp_output_tile_products_surface_bhr_iso(results)){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cbhr_iso%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
        } 
        
        if(grasp_output_tile_products_aerosol_pm(results)){
            for (iparam = 0; iparam < settings->retrieval.nPM_diam; iparam++) {
                fprintf(f,"%cPM(%f)", sep, settings->retrieval.PM_diam[iparam]);
            }
        }
        
        if(grasp_output_tile_products_aerosol_types(results)){
            fprintf(f,"%cAerosol_type_index",sep);
        }
        
        if(grasp_output_tile_products_aerosol_rind(results)){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                    fprintf(f,"%creff_index_real%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                }
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                    fprintf(f,"%creff_index_imag%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                }
            }
        }
        
        if(grasp_output_tile_products_aerosol_chem(results)){
            for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                fprintf(f,"%cchem_aer_relative_humidity_%d", sep, isd);
            }
            for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                fprintf(f,"%cchem_aer_water_fraction_%d", sep, isd);
            }
            for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                fprintf(f,"%cchem_aer_soluble_fraction_%d", sep, isd);
            }
            for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                for(ichem=0; ichem< grasp_output_tile_information_tile_nchem(results, isd); ichem++){
                    fprintf(f,"%cchem_aer_chemical_fraction_%d_%d",sep,isd,ichem);
                }
            }
        }
        
        if(grasp_output_tile_products_errest_par(results)){
            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                grasp_parameters_get_characteric_type_pretty_name_by_parameter_number(&(settings->retrieval.NDIM), iparam, false, settings->retrieval.WAVE, settings->retrieval.IWW_SINGL, param_title, 64);
                fprintf(f, "%cerrest_%s", sep, param_title);
            }  
            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                grasp_parameters_get_characteric_type_pretty_name_by_parameter_number(&(settings->retrieval.NDIM), iparam, false, settings->retrieval.WAVE, settings->retrieval.IWW_SINGL, param_title, 64);
                fprintf(f, "%cstd_%s", sep, param_title);
            }  
            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                grasp_parameters_get_characteric_type_pretty_name_by_parameter_number(&(settings->retrieval.NDIM), iparam, false, settings->retrieval.WAVE, settings->retrieval.IWW_SINGL, param_title, 64);
                fprintf(f, "%cbias_%s", sep, param_title);
            }    
        }
        if(grasp_output_tile_products_errest_aerosol_opt(results)){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cerrest_tau%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cstd_tau%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cbias_tau%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cerrest_ssa%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cstd_ssa%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cbias_ssa%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cerrest_aext%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cstd_aext%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cbias_aext%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
        }
        if(grasp_output_tile_products_errest_aerosol_mic(results)){
            for (j = 0; j < results->information.ngrid[0] ; j++) {
                fprintf(f,"%cerrest_sdt%d", sep, (int) (results->information.radius[0][j]));
            }
            for (j = 0; j < results->information.ngrid[0] ; j++) {
                fprintf(f,"%cstd_sdt%d", sep, (int) (results->information.radius[0][j]));
            }
            for (j = 0; j < results->information.ngrid[0] ; j++) {
                fprintf(f,"%cbias_sdt%d", sep, (int) (results->information.radius[0][j]));
            }
        }
/*        
        // This code prints the first radiance input/fit
        if(grasp_output_tile_products_retrieval_fit(results)){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%crad1_wl%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cfitrad1_wl%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
        }
 */
        
        fprintf(f,"\n");

        for (itime = 0; itime < tile_description->dimensions.tile_nt; itime++) {
            for (ix = 0; ix < tile_description->dimensions.tile_nx; ix++) {
                for (iy = 0; iy < tile_description->dimensions.tile_ny; iy++) {
                    if( results->tile_result_map[index3D(itime,ix,iy,tile_description->dimensions.tile_nx,tile_description->dimensions.tile_ny)] != NULL ){
                        time_to_string_r((time_t) grasp_output_tile_pixel_information_time(results,itime,ix,iy), "%FT%H:%M:%SZ", sizeof (str_date_time), str_date_time);
                        fprintf(f, "%d%c%d%c%s%c", grasp_output_tile_pixel_information_out_x(results,itime,ix,iy), sep, grasp_output_tile_pixel_information_out_y(results,itime,ix,iy), sep, str_date_time, sep);
                        fprintf(f, "%" PRId64 "%c%f%c%f", grasp_output_tile_pixel_information_time(results,itime,ix,iy), sep, grasp_output_tile_pixel_information_longitude(results,itime,ix,iy),sep, grasp_output_tile_pixel_information_latitude(results,itime,ix,iy));
                        fprintf(f, "%c%d", sep, grasp_output_tile_retrieval_res_niter(results,itime,ix,iy));
                        
                        if(settings->output.tile_function.csv.show_timing){
                            fprintf(f, "%c%f", sep, grasp_output_tile_pixel_information_real_time(results,itime,ix,iy));
                            fprintf(f, "%c%f", sep, grasp_output_tile_pixel_information_user_time(results,itime,ix,iy));
                        }
                        
                        for (inoise = 0; inoise < settings->retrieval.NOISE.INOISE; inoise++) {
                            fprintf(f,"%c%f", sep, grasp_output_tile_retrieval_res_resr(results,itime,ix,iy,inoise));
                        }        
                        for (inoise = 0; inoise < settings->retrieval.NOISE.INOISE; inoise++) {
                            fprintf(f,"%c%f", sep, grasp_output_tile_retrieval_res_resa(results,itime,ix,iy,inoise));
                        }        

                        fprintf(f, "%c%f", sep, grasp_output_tile_pixel_information_land_percent(results,itime,ix,iy));
                        fprintf(f, "%c%d", sep, grasp_output_tile_pixel_information_cloud_flag(results,itime,ix,iy));

                        if(grasp_output_tile_products_retrieval_par(results)){
                            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_retrieval_par_parameters(results,itime,ix,iy,iparam));
                            }  
                        }
                        
                        if(grasp_output_tile_products_aerosol_opt(results)){
                            fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_opt_aexp(results, itime,ix,iy));
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_opt_extt(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_opt_ssat(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_opt_aextt(results, itime,ix,iy,iwln));
                            }
                            if(settings->retrieval.NSD>1){
                                for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                    for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                        fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_opt_ext(results, itime,ix,iy,iwln,isd));   
                                    }   
                                }
                                for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                    for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                        fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_opt_ssa(results, itime,ix,iy,iwln,isd));   
                                    }   
                                }
                                for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                    for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                        fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_opt_aext(results, itime,ix,iy,iwln,isd));   
                                    }   
                                }
                            }
                        }
                        if(grasp_output_tile_products_aerosol_sd2m_ext(results)){
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_sd2m_opt_ext(results, itime,ix,iy,iwln,0)); 
                            }        
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_sd2m_opt_ext(results, itime,ix,iy,iwln,1));
                            } 
                        }
                        if(grasp_output_tile_products_aerosol_sd2m_mph(results)){
                            for (isd = 0; isd < 3; isd++) {
                                fprintf(f,"%c%f", sep, grasp_output_tile_aerosol_sd2m_mph_cv(results,itime,ix,iy, isd));
                            }
                            for (isd = 0; isd < 3; isd++) {
                                fprintf(f,"%c%f", sep, grasp_output_tile_aerosol_sd2m_mph_rm(results,itime,ix,iy, isd));
                            }                
                            for (isd = 0; isd < 3; isd++) {
                                fprintf(f,"%c%f", sep, grasp_output_tile_aerosol_sd2m_mph_std(results,itime,ix,iy, isd));
                            }
                            for (isd = 0; isd < 3; isd++) {
                                fprintf(f,"%c%f", sep, grasp_output_tile_aerosol_sd2m_mph_reff(results,itime,ix,iy, isd));
                            }
                        }
                        if(grasp_output_tile_products_aerosol_lidar(results)){ 
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f,"%c%f", sep, grasp_output_tile_aerosol_lidar_lrt(results, itime,ix,iy,iwln));
                            }
                            if(settings->retrieval.NSD>1){
                                for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                                        fprintf(f,"%c%f", sep, grasp_output_tile_aerosol_lidar_lr(results,itime,ix,iy,iwln,isd));
                                    }
                                }
                            }
                        }
                        if(grasp_output_tile_products_forcing_bbflux(results)){
                            for (ihlv = 0; ihlv < grasp_output_tile_forcing_bbflux_nhlv(results, itime,ix,iy); ihlv++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_forcing_bbflux_bbufx0(results, itime,ix,iy,ihlv));
                            }
                        }
                        if(grasp_output_tile_products_forcing_bbflux(results)){
                            for (ihlv = 0; ihlv < grasp_output_tile_forcing_bbflux_nhlv(results, itime,ix,iy); ihlv++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_forcing_bbflux_bbdfx0(results, itime,ix,iy,ihlv));
                            }
                        }
                        if(grasp_output_tile_products_forcing_bbflux(results)){
                            for (ihlv = 0; ihlv < grasp_output_tile_forcing_bbflux_nhlv(results, itime,ix,iy); ihlv++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_forcing_bbflux_bbufxa(results, itime,ix,iy,ihlv));
                            }
                        }
                        if(grasp_output_tile_products_forcing_bbflux(results)){
                            for (ihlv = 0; ihlv < grasp_output_tile_forcing_bbflux_nhlv(results, itime,ix,iy); ihlv++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_forcing_bbflux_bbdfxa(results, itime,ix,iy,ihlv));
                            }
                        }
                        if(grasp_output_tile_products_forcing_forcing(results)){
                            for (ihlv = 0; ihlv < grasp_output_tile_forcing_forcing_nhlv(results, itime,ix,iy); ihlv++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_forcing_forcing_netforc(results, itime,ix,iy,ihlv));
                            }
                        }
                        if(grasp_output_tile_products_forcing_forcing(results)){
                            for (ihlv = 0; ihlv < grasp_output_tile_forcing_forcing_nhlv(results, itime,ix,iy); ihlv++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_forcing_forcing_forceff(results, itime,ix,iy,ihlv));
                            }
                        }
                        if(grasp_output_tile_products_surface_surf(results)){
                            fprintf(f,"%c%f", sep, grasp_output_tile_surface_ndvi(results, itime,ix,iy));
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f,"%c%f", sep, grasp_output_tile_surface_dhr(results, itime,ix,iy,iwln));
                            }
                        }
                        if(grasp_output_tile_products_surface_bhr_iso(results)){
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f,"%c%f", sep, grasp_output_tile_surface_bhr_iso(results, itime,ix,iy,iwln));
                            }
                        }
                        
                        if(grasp_output_tile_products_aerosol_pm(results)){
                            for (iparam = 0; iparam < grasp_output_tile_information_npm_diam(results); iparam++) {
                                fprintf(f,"%c%f", sep, grasp_output_tile_aerosol_pm_pm(results, itime,ix,iy,iparam));
                            }
                        }
                        
                        if(grasp_output_tile_products_aerosol_types(results)){
                            fprintf(f,"%c%d", sep, grasp_output_tile_aerosol_types_index(results, itime,ix,iy));
                        }
                        
                        if(grasp_output_tile_products_aerosol_rind(results)){
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                    fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_rind_mreal(results, itime,ix,iy,iwln,isd));
                                }
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                    fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_rind_mimag(results, itime,ix,iy,iwln,isd));
                                }
                            }
                        }
                        
                        if(grasp_output_tile_products_aerosol_chem(results)){
                            for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_chem_rh(results, itime,ix,iy,isd));
                            }
                            for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_chem_fwtr(results, itime,ix,iy,isd));
                            }
                            for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_chem_fslbl(results, itime,ix,iy,isd));
                            }
                            for (isd = 0; isd < grasp_output_tile_information_nsd(results); isd++) {
                                for(ichem=0; ichem< grasp_output_tile_information_tile_nchem(results, isd); ichem++){
                                    fprintf(f, "%c%f", sep, grasp_output_tile_aerosol_chem_vfract(results, itime,ix,iy,isd,ichem));
                                }
                            }

                            
/* modify by lei on 15/11/2016 */
                        }
                        
                        if(grasp_output_tile_products_errest_par(results)){
                            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_par_tstdp(results,itime,ix,iy,iparam));
                            } 
                            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_par_errp(results,itime,ix,iy,iparam));
                            } 
                            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_par_biasp(results,itime,ix,iy,iparam));
                            } 
                        }
                        if(grasp_output_tile_products_errest_aerosol_opt(results)){
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_tstd_extt(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_err_extt(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_bias_extt(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_tstd_ssat(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_err_ssat(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_bias_ssat(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_tstd_aextt(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_err_aextt(results, itime,ix,iy,iwln));
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_opt_bias_aextt(results, itime,ix,iy,iwln));
                            }
                        }
                        if(grasp_output_tile_products_errest_aerosol_mic(results)){
                            for (j = 0; j < grasp_output_tile_information_radius(results, 1, igrid); j++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_mic_tstd_sd(results, itime,ix,iy,j));
                            }
                            for (j = 0; j < grasp_output_tile_information_radius(results, 1, igrid); j++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_mic_err_sd(results, itime,ix,iy,j));
                            }
                            for (j = 0; j < grasp_output_tile_information_radius(results, 1, igrid); j++) {
                                fprintf(f, "%c%f", sep, grasp_output_tile_errest_aerosol_mic_bias_sd(results, itime,ix,iy,j));
                            }
                        }
/*
                        // This code prints the first radiance input/fit
                        if(grasp_output_tile_products_retrieval_fit(results)){
                            const pixel_t *original=grasp_output_tile_retrieval_fit_pixel_original(results, itime,ix,iy);
                            const pixel_t *fit=grasp_output_tile_retrieval_fit_pixel_fit(results, itime,ix,iy);
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, original->meas[iwln].i[0]);
                            }
                            for (iwln = 0; iwln < grasp_output_tile_pixel_information_nwl(results, itime,ix,iy); iwln++) {
                                fprintf(f, "%c%f", sep, fit->meas[iwln].i[0]);
                            }
                        }
*/ 
                        fprintf(f, "\n");
                    }
                }
            }
        }
        
        original_name = (char *) malloc(sizeof (char)*(strlen(stream->filename)+10));
        strcpy(original_name,stream->filename);
        screen=stream->screen;
        writable=stream->writable;
        (void)screen; //to avoid unused compilation error
        (void)writable; //to avoid unused compilation error
        grasp_output_stream_close(stream);
#ifdef ZLIB_FOUND
        if(settings->output.tile_function.csv.compression && !screen && writable){
            compress_name = (char *) malloc(sizeof (char)*(strlen(original_name)+10));
            strcpy(compress_name,original_name);
            strcat(compress_name,".gz");
            f=fopen(original_name,"rb");
            fc=fopen(compress_name,"wb");
            rewind(f);
            zerr_code=def(f, fc, Z_DEFAULT_COMPRESSION );
            if(zerr_code==Z_OK){
                remove(original_name);
            }else{
                fprintf(stderr, "CSV Output tile: Error compresing output file\n");
            }   
            fclose(fc);
            fclose(f);
            free(compress_name);
        }
#endif
        free(original_name);
    }    
    
    return 0;
}

grasp_settings_parameter_array *grasp_output_tile_function_settings_csv(grasp_settings *settings){
    int i;
    
    // Static definition of a dictionary
    yamlsettings_parameter parameters[]= {
         //                 name                                                                                                                                                                                   , memory direction                                , counter memory direction              , func to set variable type (length of value)             , initial value                               ,  number of elements          , allow_array             , parameter description                                                                                                                                                                                                                                                                                                                ,                 validator 1                 ,                   validator 2                         ,                    validator 3                            , {input function, output function, assigned}           
        {"delimiter"                   , &settings->output.tile_function.csv.delimiter                          , NULL       , YS_DATA_TYPE_CHAR                 , {ys_d_all,{","}}              , {0,{}}     , YS_PARAM_SCALAR         , "Separator between fields"                                                          , {  },YAMLSETTINGS_END_VAR  },                  
#ifdef ZLIB_FOUND
        {"compression"                 , &settings->output.tile_function.csv.compression                        , NULL       , YS_DATA_TYPE_BOOLEAN              , {ys_d_all,{"false"}}          , {0,{}}     , YS_PARAM_SCALAR         , "If true output is compressed in GZ format (gz extension is automatically added)"   , {  },YAMLSETTINGS_END_VAR  },                  
#endif
        {"show_timing"                 , &settings->output.tile_function.csv.show_timing                        , NULL       , YS_DATA_TYPE_BOOLEAN              , {ys_d_all,{"true"}}          , {0,{}}     , YS_PARAM_SCALAR         , "If true 'time per pixel' information is added in the output (default). Hide this information is useful to compare results with diff command"   , {  },YAMLSETTINGS_END_VAR  },                  
    };
    grasp_settings_parameter_array *result;
    
    result = (grasp_settings_parameter_array *) malloc(sizeof (grasp_settings_parameter_array));
    
    result->nparameters=sizeof(parameters)/sizeof(yamlsettings_parameter);
    
    result->parameters = (yamlsettings_parameter *) malloc(sizeof (yamlsettings_parameter)*result->nparameters);
    
    for(i=0;i<sizeof(parameters)/sizeof(yamlsettings_parameter);i++){
       yamlsettings_copy_parameter(&(parameters[i]),&(result->parameters[i]));
    }
    
    return result;   
}

