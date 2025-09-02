/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include "grasp_output_segment_csv.h"
#include "../../grasp_output_stream.h"
#include <grasp/utils.h>
#include <inttypes.h>

#include "yamlsettings/yamlsettings.h"
#include "yamlsettings/yamlsettings_dictionary.h"
#include "yamlsettings/yamlsettings_assign_data.h"
#include "yamlsettings/yamlsettings_validators.h"
#include "yamlsettings/yamlsettings_data_types.h"
#include "grasp_output_segment_csv_settings.h"

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

grasp_output_segment_function_t grasp_output_segment_function_csv(){
    grasp_output_segment_function_t x;
    
    x.init=grasp_output_segment_function_csv_init;
    x.function=grasp_output_segment_function_csv_process;
    x.close=grasp_output_segment_function_csv_close;
    
    return x;
}

int grasp_output_segment_function_csv_init(grasp_settings *settings, grasp_tile_description_t *input_information){
    return 0;
}

int grasp_output_segment_function_csv_close(void){
    return 0;
}

int grasp_output_segment_function_csv_process(grasp_output_stream *stream, grasp_settings *settings, grasp_segment_t *segment, output_segment_general *output, grasp_tile_description_t *tile_description,int icol,int irow,int itime){
    int ipixel, iwln,inoise, iparam, ihlv, isd, j;
    char str_date_time[20 + 1];
    char param_title[64];
    FILE *f;
    char sep;
    char *original_name;
    bool screen, writable;
    int ichem;
#ifdef ZLIB_FOUND
    int zerr_code;
    FILE *fc;
    char *compress_name;
#endif
    
    sep=settings->output.segment_function.csv.delimiter;
        
    if(segment->sdata.npixels<=0){
        fprintf(stderr, "The segment in position time=%d, x=%d and y=%d has no pixels. Results with csv segment output function will not be printed in %s.\n", itime, icol, irow, stream->filename );
    }else{    
        f=grasp_output_stream_open(stream, settings, segment, output, &tile_description->dimensions, icol, irow, itime);
        if(grasp_output_stream_writable(stream)==false){ // If it is not writable, close stream and finish function
            grasp_output_stream_close(stream);
            grasp_output_stream_destroy(stream); 
            return 0;
        }
    
        fprintf(f,"row%ccol%cdatetime%cunixtimestamp%clon%clat%cniterations", sep, sep, sep, sep, sep, sep);

        if(settings->output.segment_function.csv.show_timing){
            fprintf(f,"%creal_time%cuser_time",sep, sep);
        }
        
        for (inoise = 0; inoise < settings->retrieval.NOISE.INOISE; inoise++) {
            fprintf(f,"%cresidual_relative_noise%d", sep, inoise);
        }
        for (inoise = 0; inoise < settings->retrieval.NOISE.INOISE; inoise++) {
            fprintf(f,"%cresidual_absolute_noise%d", sep, inoise);
        }

        fprintf(f, "%cland_percent", sep);
        fprintf(f, "%ccloud_mask", sep);

        if(settings->retrieval.products.retrieval.par==true && grasp_output_segment_products_retrieval_par(output)==true){
            for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                grasp_parameters_get_characteric_type_pretty_name_by_parameter_number(&(settings->retrieval.NDIM), iparam, false, settings->retrieval.WAVE, settings->retrieval.IWW_SINGL, param_title, 64);
                fprintf(f, "%c%s", sep, param_title);
            }             
        }
        
        if(settings->retrieval.products.aerosol.opt==true && grasp_output_segment_products_aerosol_opt(output)==true){
            fprintf(f,"%cAExp", sep);
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
        
        if(settings->retrieval.products.aerosol.sd2m_ext==true && grasp_output_segment_products_aerosol_sd2m_ext(output)==true){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%ctauF%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%ctauC%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }    
        }
        if(settings->retrieval.products.aerosol.sd2m_mph==true && grasp_output_segment_products_aerosol_sd2m_mph(output)==true){
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
        
        if(settings->retrieval.products.aerosol.lidar && grasp_output_segment_products_aerosol_lidar(output)){ 
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
        
        if(settings->retrieval.products.forcing.bbflux && grasp_output_segment_products_forcing_bbflux(output)){
           for (ihlv = 0; ihlv < grasp_output_segment_forcing_bbflux_nhlv(output, 0); ihlv++) {
              fprintf(f,"%cbbufx0%0.0fkm", sep, grasp_output_segment_forcing_bbflux_hlv(output, 0, ihlv));
           }
           for (ihlv = 0; ihlv < grasp_output_segment_forcing_bbflux_nhlv(output, 0); ihlv++) {
              fprintf(f,"%cbbdfx0%0.0fkm", sep, grasp_output_segment_forcing_bbflux_hlv(output, 0, ihlv));
           }
           for (ihlv = 0; ihlv < grasp_output_segment_forcing_bbflux_nhlv(output, 0); ihlv++) {
              fprintf(f,"%cbbufxa%0.0fkm", sep, grasp_output_segment_forcing_bbflux_hlv(output, 0, ihlv));
           }
           for (ihlv = 0; ihlv < grasp_output_segment_forcing_bbflux_nhlv(output, 0); ihlv++) {
              fprintf(f,"%cbbdfxa%0.0fkm", sep, grasp_output_segment_forcing_bbflux_hlv(output, 0, ihlv));
           }
        }
        
        if(settings->retrieval.products.forcing.forcing && grasp_output_segment_products_forcing_forcing(output)){
           for (ihlv = 0; ihlv < grasp_output_segment_forcing_forcing_nhlv(output, 0); ihlv++) {
              fprintf(f,"%cnetforc%0.0fkm", sep, grasp_output_segment_forcing_forcing_hlv(output, 0, ihlv));
           }
           for (ihlv = 0; ihlv < grasp_output_segment_forcing_forcing_nhlv(output, 0); ihlv++) {
              fprintf(f,"%cforceff%0.0fkm", sep, grasp_output_segment_forcing_forcing_hlv(output, 0, ihlv));
           }
        }    
                
        if(settings->retrieval.products.surface.surf && grasp_output_segment_products_surface_surf(output)){
            fprintf(f,"%cndvi", sep);
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cdhr%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
        }
        
        if(settings->retrieval.products.surface.bhr_iso && grasp_output_segment_products_surface_bhr_iso(output)){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cbhr_iso%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
        }
        
        if(settings->retrieval.products.aerosol.pm && grasp_output_segment_products_aerosol_pm(output)){
            for (iparam = 0; iparam < settings->retrieval.nPM_diam; iparam++) {
                fprintf(f,"%cPM(%f)", sep, settings->retrieval.PM_diam[iparam]);
            }
        }
        
        if(settings->retrieval.products.aerosol.types && grasp_output_segment_products_aerosol_types(output)){
            fprintf(f,"%cAerosol_type_index", sep);
        }
        
        if(grasp_output_segment_products_aerosol_rind(output)){
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
        
        if(grasp_output_segment_products_aerosol_chem(output)){
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
                for(ichem =0; ichem < grasp_output_segment_retrieval_information_nchem(output, isd); ichem++){
                   fprintf(f,"%cchem_aer_chemical_fraction_%d_%d", sep, isd, ichem); 
                }
            }
        }
        
        if(settings->retrieval.products.errest.par && grasp_output_segment_products_errest_par(output)){
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
        
        if(settings->retrieval.products.errest.aerosol.opt && grasp_output_segment_products_errest_aerosol_opt(output)){
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
                fprintf(f,"%cerrest_aaod%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cstd_aaod%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cbias_aaod%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }

            if(settings->retrieval.NSD>1){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cerrest_tau%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cstd_tau%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cbias_tau%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cerrest_ssa%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cstd_ssa%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cbias_ssa%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cerrest_aaod%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cstd_aaod%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cbias_aaod%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                }
            }
        }
        if(settings->retrieval.products.errest.aerosol.mic && grasp_output_segment_products_errest_aerosol_mic(output)){
            for (j = 0; j < output->retrieval.information.ngrid[0] ; j++) {
                fprintf(f,"%cerrest_sdt%d", sep, j+1);
            }
            for (j = 0; j < output->retrieval.information.ngrid[0] ; j++) {
                fprintf(f,"%cstd_sdt%d", sep, j+1);
            }
            for (j = 0; j < output->retrieval.information.ngrid[0] ; j++) {
                fprintf(f,"%cbias_sdt%d", sep, j+1);
            }
        }
        if(settings->retrieval.products.errest.aerosol.lidar && grasp_output_segment_products_errest_aerosol_lidar(output)){
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cerrest_lr%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cstd_lr%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                fprintf(f,"%cbias_lr%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000));
            }
            if(settings->retrieval.NSD>1){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cerrest_lr%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cstd_lr%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f,"%cbias_lr%d_%d", sep, (int)(settings->retrieval.WAVE[iwln]*1000),isd);
                    }
                }
            }
        }
        // if(settings->retrieval.products.retrieval.fit && output->retrieval->fit->segment_fit) {
        if(settings->retrieval.products.retrieval.fit || true) {
            // Assuming all pixels will have the same wavelengths
            int n_whls = output->retrieval.fit.segment_fit.pixel[0].nwl;
            // for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
            for (iwln = 0; iwln < n_whls; iwln++) {
                fprintf(f,"%cmeas_%f", sep, (settings->retrieval.WAVE[iwln]*1000));
            }
        }
        
        fprintf(f,"\n");

        for (ipixel = 0; ipixel < segment->sdata.npixels; ipixel++) {
            time_to_string_r((time_t)segment->sdata.pixel[ipixel].t, "%FT%H:%M:%SZ" , sizeof(str_date_time), str_date_time);        
            fprintf(f,"%d%c%d%c%s%c", segment->sdata.pixel[ipixel].irow, sep, segment->sdata.pixel[ipixel].icol, sep,  str_date_time, sep);
            fprintf(f,"%" PRId64 "%c%f%c%f", segment->sdata.pixel[ipixel].t, sep,segment->sdata.pixel[ipixel].x,sep,segment->sdata.pixel[ipixel].y);        
            
            if(settings->retrieval.IPFP.INVSING==2){
                fprintf(f,"%c%d", sep,grasp_output_segment_retrieval_res_niter(output));
            }else{
                fprintf(f,"%c%d", sep,grasp_output_segment_retrieval_res_pixel_niter(output,ipixel));
            }

            if(settings->output.segment_function.csv.show_timing){
                fprintf(f,"%c%f", sep, grasp_output_segment_retrieval_information_delta_ut(output)/grasp_output_segment_retrieval_information_npixels(output));
                fprintf(f,"%c%f", sep, grasp_output_segment_retrieval_information_delta_ct(output)/grasp_output_segment_retrieval_information_npixels(output));
            }
            for (inoise = 0; inoise < settings->retrieval.NOISE.INOISE; inoise++) {
                fprintf(f,"%c%f", sep,grasp_output_segment_retrieval_res_pixel_resr(output,ipixel,inoise));
            }        
            for (inoise = 0; inoise < settings->retrieval.NOISE.INOISE; inoise++) {
                fprintf(f,"%c%f", sep,grasp_output_segment_retrieval_res_pixel_resa(output,ipixel,inoise));
            }        
            
            fprintf(f, "%c%f", sep, segment->sdata.pixel[ipixel].land_percent);
            if(segment->sdata.pixel[ipixel].cloudy==0){
                fprintf(f, "%c1",sep);
            }else if(segment->sdata.pixel[ipixel].cloudy==1){
                fprintf(f, "%c0",sep);
            }else{
                fprintf(f, "%c%d", sep, segment->sdata.pixel[ipixel].cloudy);
            }
            
            if(settings->retrieval.products.retrieval.par==true && grasp_output_segment_products_retrieval_par(output)==true){
                for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                    fprintf(f, "%c%f", sep, grasp_output_segment_retrieval_par_parameters(output,ipixel,iparam));
                }  
            }            
            
            if(settings->retrieval.products.aerosol.opt==true && grasp_output_segment_products_aerosol_opt(output)==true){
                fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_opt_aexp(output,ipixel));
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_opt_extt(output,ipixel,iwln));
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_opt_ssat(output,ipixel,iwln));
                }   
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_opt_aextt(output,ipixel,iwln));
                }     
                if(settings->retrieval.NSD>1){
                    for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_opt_ext(output,ipixel,iwln,isd));   
                        }   
                    }
                    for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_opt_ssa(output,ipixel,iwln,isd));   
                        }   
                    }
                    for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_opt_aext(output,ipixel,iwln,isd));   
                        }   
                    }
                }
            }
            
            if(settings->retrieval.products.aerosol.sd2m_ext==true && grasp_output_segment_products_aerosol_sd2m_ext(output)){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_sd2m_opt_ext(output,ipixel, iwln, 0) );
                }        
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_sd2m_opt_ext(output,ipixel, iwln, 1));
                }        
            }
            if(settings->retrieval.products.aerosol.sd2m_mph==true && grasp_output_segment_products_aerosol_sd2m_mph(output)==true){
                for (isd = 0; isd < 3; isd++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_sd2m_mph_cv(output,ipixel, isd));
                }
                for (isd = 0; isd < 3; isd++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_sd2m_mph_rm(output,ipixel, isd));
                }                
                for (isd = 0; isd < 3; isd++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_sd2m_mph_std(output,ipixel, isd));
                }
                for (isd = 0; isd < 3; isd++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_sd2m_mph_reff(output,ipixel, isd));
                }
            }
            if(settings->retrieval.products.aerosol.lidar && grasp_output_segment_products_aerosol_lidar(output)){ 
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_lidar_lrt(output,ipixel,iwln));
                }
                if(settings->retrieval.NSD>1){
                    for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_lidar_lr(output,ipixel,iwln,isd));
                        }
                    }
                }
            }
            
            if(settings->retrieval.products.forcing.bbflux==true && grasp_output_segment_products_forcing_bbflux(output)==true){
               for (ihlv = 0; ihlv < grasp_output_segment_forcing_forcing_nhlv(output, 0); ihlv++) {
                    fprintf(f, "%c%f",  sep, grasp_output_segment_forcing_bbflux_bbufx0(output,ipixel,ihlv));
               }
               for (ihlv = 0; ihlv < grasp_output_segment_forcing_forcing_nhlv(output, 0); ihlv++) {
                    fprintf(f, "%c%f",  sep, grasp_output_segment_forcing_bbflux_bbdfx0(output,ipixel,ihlv));
               }
               for (ihlv = 0; ihlv < grasp_output_segment_forcing_forcing_nhlv(output, 0); ihlv++) {
                    fprintf(f, "%c%f",  sep, grasp_output_segment_forcing_bbflux_bbufxa(output,ipixel,ihlv));
               }
               for (ihlv = 0; ihlv < grasp_output_segment_forcing_forcing_nhlv(output, 0); ihlv++) {
                    fprintf(f, "%c%f",  sep, grasp_output_segment_forcing_bbflux_bbdfxa(output,ipixel,ihlv));
               }
            }
            
            if(settings->retrieval.products.forcing.forcing==true && grasp_output_segment_products_forcing_forcing(output)==true){
               for (ihlv = 0; ihlv < grasp_output_segment_forcing_forcing_nhlv(output, 0); ihlv++) {
                    fprintf(f, "%c%f",  sep, grasp_output_segment_forcing_forcing_netforc(output,ipixel,ihlv));
               }
               for (ihlv = 0; ihlv < grasp_output_segment_forcing_forcing_nhlv(output, 0); ihlv++) {
                    fprintf(f, "%c%f",  sep, grasp_output_segment_forcing_forcing_forceff(output,ipixel,ihlv));
               }
            }   
            
            if(settings->retrieval.products.surface.surf && grasp_output_segment_products_surface_surf(output)){
                fprintf(f,"%c%f", sep, grasp_output_segment_surface_ndvi(output, ipixel));
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_surface_dhr(output, ipixel, iwln));
                }
            }
            
            if(settings->retrieval.products.surface.bhr_iso && grasp_output_segment_products_surface_bhr_iso(output)){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_surface_bhr_iso(output, ipixel, iwln));
                }
            }

            if(settings->retrieval.products.aerosol.pm && grasp_output_segment_products_aerosol_pm(output)){
                for (iparam = 0; iparam < settings->retrieval.nPM_diam; iparam++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_aerosol_pm_pm(output, ipixel, iparam));
                }
            }
        
            if(settings->retrieval.products.aerosol.types && grasp_output_segment_products_aerosol_types(output)){
                fprintf(f,"%c%d", sep, grasp_output_segment_aerosol_types_index(output, ipixel));
            }

            if(grasp_output_segment_products_aerosol_rind(output)){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_rind_mreal(output, ipixel,iwln,isd));
                    }
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                        fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_rind_mimag(output, ipixel,iwln,isd));
                    }
                }
            }

            if(grasp_output_segment_products_aerosol_chem(output)){
                for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                    fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_chem_rh(output, ipixel,isd));
                }
                for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                    fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_chem_fwtr(output, ipixel,isd));
                }
                for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                    fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_chem_fslbl(output, ipixel,isd));
                }
                for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                    for (ichem = 0; ichem < grasp_output_segment_retrieval_information_nchem(output, isd); ichem++) {
                        fprintf(f, "%c%f", sep, grasp_output_segment_aerosol_chem_vfract(output, ipixel,isd, ichem));
                    }
                }
            }
            
            if(settings->retrieval.products.errest.par && grasp_output_segment_products_errest_par(output)){
                for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                    fprintf(f, "%c%f", sep, grasp_output_segment_errest_par_tstdp(output,ipixel,iparam));
                }  
                for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                    fprintf(f, "%c%f", sep, grasp_output_segment_errest_par_errp(output,ipixel,iparam));
                }  
                for (iparam = 0; iparam < settings->retrieval.KNSING; iparam++) {
                    fprintf(f, "%c%f", sep, grasp_output_segment_errest_par_biasp(output,ipixel,iparam));
                }  
            }  
            
            if(settings->retrieval.products.errest.aerosol.opt && grasp_output_segment_products_errest_aerosol_opt(output)){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_tstd_extt(output,ipixel,iwln));
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_err_extt(output,ipixel,iwln));
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_bias_extt(output,ipixel,iwln));
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_tstd_ssat(output,ipixel,iwln));
                }  
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_err_ssat(output,ipixel,iwln));
                }  
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_bias_ssat(output,ipixel,iwln));
                }   
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_tstd_aextt(output,ipixel,iwln));
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_err_aextt(output,ipixel,iwln));
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_bias_aextt(output,ipixel,iwln));
                }
                if(settings->retrieval.NSD>1){
                    for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_tstd_ext(output,ipixel,iwln, isd));
                        }
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_err_ext(output,ipixel,iwln, isd));
                        }
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_bias_ext(output,ipixel,iwln, isd));
                        }
                    }
                    for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_tstd_ssa(output,ipixel,iwln, isd));
                        }
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_err_ssa(output,ipixel,iwln, isd));
                        }
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_bias_ssa(output,ipixel,iwln, isd));
                        }
                    }
                    for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_tstd_aext(output,ipixel,iwln, isd));
                        }
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_err_aext(output,ipixel,iwln, isd));
                        }
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_opt_bias_aext(output,ipixel,iwln, isd));
                        }
                    }
                }
            }
            if(settings->retrieval.products.errest.aerosol.mic && grasp_output_segment_products_errest_aerosol_mic(output)){
                for (j = 0; j < output->retrieval.information.ngrid[0] ; j++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_mic_tstd_sd(output,ipixel,j));
                }
                for (j = 0; j < output->retrieval.information.ngrid[0] ; j++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_mic_err_sd(output,ipixel,j));
                }
                for (j = 0; j < output->retrieval.information.ngrid[0] ; j++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_mic_bias_sd(output,ipixel,j));
                }
            }
            if(settings->retrieval.products.errest.aerosol.lidar && grasp_output_segment_products_errest_aerosol_lidar(output)){
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_lidar_tstd_lrt(output,ipixel,iwln));
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_lidar_err_lrt(output,ipixel,iwln));
                }
                for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                    fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_lidar_bias_lrt(output,ipixel,iwln));
                }
                if(settings->retrieval.NSD>1){
                    for (iwln = 0; iwln < settings->retrieval.NW; iwln++) {
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_lidar_tstd_lr(output,ipixel,iwln,isd));
                        }
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_lidar_err_lr(output,ipixel,iwln,isd));
                        }
                        for (isd = 0; isd < settings->retrieval.NSD; isd++) {
                            fprintf(f,"%c%f", sep, grasp_output_segment_errest_aerosol_lidar_bias_lr(output,ipixel,iwln,isd));
                        }
                    }
                }
            }
            if(settings->retrieval.products.retrieval.fit || true) {
                int n_whls = output->retrieval.fit.segment_fit.pixel[ipixel].nwl;
                for (iwln = 0; iwln < n_whls; iwln++) {
                    if (output->retrieval.fit.segment_fit.pixel[ipixel].meas[iwln].meas_type[0] == 41) {
                        float data = output->retrieval.fit.segment_fit.pixel[ipixel].meas[iwln].i[0];
                        fprintf(f,"%c%f", sep, data);
                    } else {
                        fprintf(f,"%c%s", sep, "");
                    }
                }
            }
            
            fprintf(f,"\n");
        }

        original_name = (char *) malloc(sizeof (char)*(strlen(stream->filename)+10));
        strcpy(original_name,stream->filename);
        screen=stream->screen;
        writable=stream->writable;
        (void)screen; //to avoid unused compilation error
        (void)writable; //to avoid unused compilation error
        grasp_output_stream_close(stream);
#ifdef ZLIB_FOUND
        if(settings->output.segment_function.csv.compression && !screen && writable){
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
                fprintf(stderr, "CSV Output segment: Error compresing output file\n");
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


grasp_settings_parameter_array *grasp_output_segment_function_settings_csv(grasp_settings *settings){
    int i;
    
    // Static definition of a dictionary
    yamlsettings_parameter parameters[]= {
         //                 name                                                                                                                                                                                   , memory direction                                , counter memory direction              , func to set variable type (length of value)             , initial value                               ,  number of elements          , allow_array             , parameter description                                                                                                                                                                                                                                                                                                                ,                 validator 1                 ,                   validator 2                         ,                    validator 3                            , {input function, output function, assigned}   
        {"delimiter"                   , &settings->output.segment_function.csv.delimiter                          , NULL       , YS_DATA_TYPE_CHAR                 , {ys_d_all,{","}}              , {0,{}}     , YS_PARAM_SCALAR         , "Separator between fields"                                                         , {  },YAMLSETTINGS_END_VAR  },                  
#ifdef ZLIB_FOUND
        {"compression"                 , &settings->output.segment_function.csv.compression                        , NULL       , YS_DATA_TYPE_BOOLEAN              , {ys_d_all,{"false"}}          , {0,{}}     , YS_PARAM_SCALAR         , "If true output is compressed in GZ format (gz extension is automatically added)"   , {  },YAMLSETTINGS_END_VAR  },                  
#endif
        {"show_timing"                 , &settings->output.segment_function.csv.show_timing                        , NULL       , YS_DATA_TYPE_BOOLEAN              , {ys_d_all,{"true"}}          , {0,{}}     , YS_PARAM_SCALAR         , "If true 'time per pixel' information is added in the output (default). Hide this information is useful to compare results with diff command"   , {  },YAMLSETTINGS_END_VAR  },                  
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
