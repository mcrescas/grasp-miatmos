/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include "grasp_input_segment.h"
#include "../global/grasp_parameters.h"
#include "drivers/sdata/grasp_input_driver_sdata_error_codes.h"
#include "drivers/sdata/sdata-impl.h"
#include "drivers/sdata/sdata.h"

void grasp_segment_initialize(grasp_segment_t *segment) {
  assert(segment!=NULL);
  // Initialize
  memset(segment, 0, sizeof(grasp_segment_t));
  grasp_parameters_initialize(segment->iguess);
  segment->edges.N_I_EDGE=0;
}

int grasp_segment_validate(grasp_segment_t *segment) {
  return grasp_sdata_validate(&segment->sdata);
}

int grasp_sdata_validate(sensor_data_t *sdata) {
  int ipixel, iwl, ip, ibvm;
  pixel_t *pixel;
  SDATA_VALID_RANGES ranges;
  int nvertm=0;

  init_valid_ranges(&ranges);

  if (sdata->npixels<=0||sdata->npixels>=_KIMAGE) {
    fprintf(stderr, "SEGMENT ERROR: npixels has to be in the range [0..%d] (read: %d)\n", _KIMAGE, sdata->npixels);
    return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
  }
  if (sdata->nx<=0||sdata->nx>_KIX) {
    fprintf(stderr, "SEGMENT ERROR: nx has to be in the range [0..%d] (read: %d)\n", _KIX, sdata->nx);
    return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
  }
  if (sdata->ny<=0||sdata->ny>_KIY) {
    fprintf(stderr, "SEGMENT ERROR: ny has to be in the range [0..%d] (read: %d)\n", _KIY, sdata->ny);
    return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
  }
  if (sdata->nt<=0||sdata->nt>_KITIME) {
    fprintf(stderr, "SEGMENT ERROR: nt has to be in the range [0..%d] (read: %d)\n", _KITIME, sdata->nt);
    return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
  }

  for (ipixel=0; ipixel<sdata->npixels; ipixel++) {
    pixel=&sdata->pixel[ipixel];

    if (pixel->hobs<=ranges.hgr_min) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].hobs has to be in the range [%f..] (read: %f)\n", ipixel,
              ranges.hgr_min, pixel->hobs);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->nwl<=0||pixel->nwl>_KW) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].nwl has to be in the range [0..%d] (read: %d)\n", ipixel, _KW,
              pixel->nwl);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->cloudy<ranges.cloud_flag_min||pixel->cloudy>ranges.cloud_flag_max) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].cloudy has to be in the range [%d..%d] (read: %d)\n", ipixel,
              ranges.cloud_flag_min, ranges.cloud_flag_max, pixel->cloudy);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->x<=ranges.longitude_min||pixel->x>ranges.longitude_max) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].x has to be in the range [%f..%f] (read: %f)\n", ipixel,
              ranges.longitude_min, ranges.longitude_max, pixel->x);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->y<=ranges.latitude_min||pixel->y>ranges.latitude_max) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].y has to be in the range [%f..%f] (read: %f)\n", ipixel,
              ranges.latitude_min, ranges.latitude_max, pixel->y);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->ix<=0||pixel->ix>sdata->nx) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].ix has to be in the range [0..nx] -> [0..%d] (read: %d)\n", ipixel,
              sdata->nx, pixel->ix);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->iy<=0||pixel->iy>sdata->ny) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].iy has to be in the range [0..ny] -> [0..%d] (read: %d)\n", ipixel,
              sdata->ny, pixel->iy);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->it<=0||pixel->it>sdata->nt) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].it has to be in the range [0..nt] -> [0..%d] (read: %d)\n", ipixel,
              sdata->nt, pixel->it);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->out_x<0) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].out_y has to be in the range [0..]  (read: %d)\n", ipixel,
              pixel->out_x);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->out_y<0) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].out_y has to be in the range [0..]  (read: %d)\n", ipixel,
              pixel->out_y);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->out_t<0) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].out_t has to be in the range [0..]  (read: %d)\n", ipixel,
              pixel->out_t);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->masl<=ranges.hgr_min||pixel->masl>ranges.hgr_max) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].masl has to be in the range [%f..%f] (read: %f)\n", ipixel,
              ranges.hgr_min, ranges.hgr_max, pixel->masl);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->land_percent<ranges.land_percent_min||pixel->land_percent>ranges.land_percent_max) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].land_percent has to be in the range [%f..%f] (read: %f)\n", ipixel,
              ranges.land_percent_min, ranges.land_percent_max, pixel->land_percent);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    if (pixel->ifgas<0||pixel->ifgas>1) {
      fprintf(stderr, "SEGMENT ERROR: pixel[%d].ifgas has to be in the range [0..1] (read: %d)\n", ipixel,
              pixel->ifgas);
      return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
    }
    for (iwl=0; iwl<pixel->nwl; iwl++) {
      if (pixel->meas[iwl].wl<ranges.wavelength_min||pixel->meas[iwl].wl>ranges.wavelength_max) {
        fprintf(stderr, "SEGMENT ERROR: pixel[%d].meas[%d].wl has to be in the range [%f..%f] (read: %f)\n", ipixel,
                iwl, ranges.wavelength_min, ranges.wavelength_max, pixel->meas[iwl].wl);
        return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
      }
      //if(pixel->meas[iwl].ind_wl<0 || pixel->meas[iwl].ind_wl>pixel->nwl){
      //    fprintf(stderr, "SEGMENT ERROR: pixel[%d].meas[%d].ind_wl has to be in the range [0..%d] (read: %d)\n",ipixel,iwl,pixel->nwl,pixel->meas[iwl].ind_wl);
      //    return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
      //}
      if (pixel->meas[iwl].sza<ranges.thetas_min||pixel->meas[iwl].sza>ranges.thetas_max) {
        fprintf(stderr, "SEGMENT ERROR: pixel[%d].meas[%d].sza has to be in the range [%f..%f] (read: %f)\n", ipixel,
                iwl, ranges.thetas_min, ranges.thetas_max, pixel->meas[iwl].sza);
        return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
      }
      if (pixel->meas[iwl].nip<0||pixel->meas[iwl].nip>_KIP) {
        fprintf(stderr, "SEGMENT ERROR: pixel[%d].meas[%d].nip has to be in the range [%d..%d] (read: %d)\n", ipixel,
                iwl, 0, _KIP, pixel->meas[iwl].nip);
        return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
      }

      for (ip=0; ip<pixel->meas[iwl].nip; ip++) {
        if (pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_TOD&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_AOD&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_ABS&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P11&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P12&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P22&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P33&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P34&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P44&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_LS&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_RL&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_DPAR&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_DPER&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_DP&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_VEXT&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_VBS&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_I&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_Q&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_U&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_I&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P11_intd&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P11_intd_cut_off_1&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P11_intd_cut_off_2&&
            pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P11_intd_cut_off_3&&pixel->meas[iwl].meas_type[ip]!=MEAS_TYPE_P11_intd_cut_off_4) {
          fprintf(stderr, "SEGMENT ERROR: pixel[%d].meas[%d].meas_type[%d] is not known (read: %d)\n", ipixel, iwl, ip,
                  pixel->meas[iwl].meas_type[ip]);
          return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
        }
        if (pixel->meas[iwl].nbvm[ip]<ranges.num_valid_meas_min||pixel->meas[iwl].nbvm[ip]>ranges.num_valid_meas_max) {
          fprintf(stderr, "SEGMENT ERROR: pixel[%d].meas[%d].nbvm[%d] has to be in the range [%d..%d] (read: %d)\n",
                  ipixel, iwl, ip, ranges.num_valid_meas_min, ranges.num_valid_meas_max, pixel->meas[iwl].nbvm[ip]);
          return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
        }

        for (ibvm=0; ibvm<pixel->meas[iwl].nbvm[ip]; ibvm++) {
          if (pixel->meas[iwl].thetav[ip][ibvm]<ranges.thetav_min||
              pixel->meas[iwl].thetav[ip][ibvm]>ranges.thetav_max) {
            fprintf(stderr,
                    "SEGMENT ERROR: pixel[%d].meas[%d].thetav[%d][%d] has to be in the range [%f..%f] (read: %f)\n",
                    ipixel, iwl, ip, ibvm, ranges.thetav_min, ranges.thetav_max, pixel->meas[iwl].thetav[ip][ibvm]);
            return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
          }
          if (pixel->meas[iwl].phi[ip][ibvm]<ranges.dphi_min||pixel->meas[iwl].phi[ip][ibvm]>ranges.dphi_max) {
            fprintf(stderr,
                    "SEGMENT ERROR: pixel[%d].meas[%d].phi[%d][%d] has to be in the range [%f..%f] (read: %f)\n",
                    ipixel, iwl, ip, ibvm, ranges.dphi_min, ranges.dphi_max, pixel->meas[iwl].phi[ip][ibvm]);
            return GRASP_ERROR_SDATA_VALUE_OUT_OF_BOUNDS;
          }
        }

        for (ip=0; ip<pixel->meas[iwl].nip; ip++) {
          switch (pixel->meas[iwl].meas_type[ip]) {
            // Validation code for each measurement type
            case MEAS_TYPE_TOD:
              break;
            case MEAS_TYPE_AOD:
              break;
            case MEAS_TYPE_ABS:
              break;
            case MEAS_TYPE_P11:
              break;
            case MEAS_TYPE_P12:
              break;
            case MEAS_TYPE_P22:
              break;
            case MEAS_TYPE_P33:
              break;
            case MEAS_TYPE_P34:
              break;
            case MEAS_TYPE_P44:
              break;
            case MEAS_TYPE_LS:
              if (nvertm!=0&&nvertm!=pixel->meas[iwl].nbvm[ip]) {
                fprintf(stderr, "SEGMENT ERROR: LS with different number of points in vertical profile\n");
                return GRASP_ERROR_SDATA_MALFORMED;
              }
              nvertm=pixel->meas[iwl].nbvm[ip];
              break;
            case MEAS_TYPE_RL:
              if (nvertm!=0&&nvertm!=pixel->meas[iwl].nbvm[ip]) {
                fprintf(stderr, "SEGMENT ERROR: RL with different number of points in vertical profile\n");
                return GRASP_ERROR_SDATA_MALFORMED;
              }
              nvertm=pixel->meas[iwl].nbvm[ip];
              break;
            case MEAS_TYPE_DPAR:
              if (nvertm!=0&&nvertm!=pixel->meas[iwl].nbvm[ip]) {
                fprintf(stderr, "SEGMENT ERROR: DPAR with different number of points in vertical profile\n");
                return GRASP_ERROR_SDATA_MALFORMED;
              }
              nvertm=pixel->meas[iwl].nbvm[ip];
              break;
            case MEAS_TYPE_DPER:
              if (nvertm!=0&&nvertm!=pixel->meas[iwl].nbvm[ip]) {
                fprintf(stderr, "SEGMENT ERROR: DPAR with different number of points in vertical profile\n");
                return GRASP_ERROR_SDATA_MALFORMED;
              }
              nvertm=pixel->meas[iwl].nbvm[ip];
              break;
            case MEAS_TYPE_DP:
              if (nvertm!=0&&nvertm!=pixel->meas[iwl].nbvm[ip]) {
                fprintf(stderr, "SEGMENT ERROR: DP with different number of points in vertical profile\n");
                return GRASP_ERROR_SDATA_MALFORMED;
              }
              nvertm=pixel->meas[iwl].nbvm[ip];
              break;
            case MEAS_TYPE_VEXT:
              if (nvertm!=0&&nvertm!=pixel->meas[iwl].nbvm[ip]) {
                fprintf(stderr, "SEGMENT ERROR: VEXT with different number of points in vertical profile\n");
                return GRASP_ERROR_SDATA_MALFORMED;
              }
              nvertm=pixel->meas[iwl].nbvm[ip];
              break;
            case MEAS_TYPE_VBS:
              if (nvertm!=0&&nvertm!=pixel->meas[iwl].nbvm[ip]) {
                fprintf(stderr, "SEGMENT ERROR: VBS with different number of points in vertical profile\n");
                return GRASP_ERROR_SDATA_MALFORMED;
              }
              nvertm=pixel->meas[iwl].nbvm[ip];
              break;
            case MEAS_TYPE_I:
              break;
            case MEAS_TYPE_Q:
              break;
            case MEAS_TYPE_U:
              break;
            case MEAS_TYPE_P:
              break;
            case MEAS_TYPE_P11_intd:
              break;
            case MEAS_TYPE_P11_intd_cut_off_1:
              break;
            case MEAS_TYPE_P11_intd_cut_off_2:
              break;
            case MEAS_TYPE_P11_intd_cut_off_3:
              break;
            case MEAS_TYPE_P11_intd_cut_off_4:
              break;

            default:
              break;
          }
        }
      }

    }

    for (ipixel=0; ipixel<sdata->npixels-1; ipixel++) {
      if (sdata->pixel[ipixel].t>sdata->pixel[ipixel+1].t) {
        fprintf(stderr, "SEGMENT ERROR: pixels has to be time ordered sdata->pixel[%d].t>sdata->pixel[%d].\n", ipixel,
                ipixel+1);
        return GRASP_ERROR_SDATA_MALFORMED;
      }
    }

    for (ipixel=0; ipixel<sdata->npixels; ipixel++) {
      for (ibvm=0; ibvm<nvertm-1; ibvm++) {
        if (sdata->pixel[ipixel].hvp[ibvm]<sdata->pixel[ipixel].hvp[ibvm+1]) {
          fprintf(stderr,
                  "SEGMENT ERROR: Vertical profile has to be defined from upper to lower altitude:  sdata->pixel[%d].hvp[%d]<sdata->pixel[%d].hvp[%d]\n",
                  ipixel, ibvm, ipixel, ibvm+1);
          return GRASP_ERROR_SDATA_MALFORMED;
        }
      }
    }

  }

  return 0;
}

void grasp_segment_remove_pixel(grasp_segment_t *segment, int ipixel) {
  int i;
  int pixel_time;
  int pixels_in_time=0;
  bool last_time=false;

  // Check if nt is still valid or we have emptied a specific time and we have to reduce it in 1.
  pixel_time=segment->sdata.pixel[ipixel].it;
  for (i=0; i<segment->sdata.npixels; i++) {
    if (segment->sdata.pixel[i].it==pixel_time) {
      pixels_in_time++;
    }
  }
  assert(pixels_in_time>=1);
  if (pixels_in_time==
      1) { // The pixel we are going to remove is the only pixel in the current time so we have to remove completely this time
    last_time=true;
    if (segment->edges.N_I_EDGE>0) {
      // WARNING: This function does not support edges yet.
      assert(1!=1);
    }
    segment->sdata.nt--;
  }

  // Remove the pixel
  segment->sdata.npixels--;

  for (i=ipixel; i<segment->sdata.npixels; i++) {
    memcpy(&(segment->sdata.pixel[i]), &(segment->sdata.pixel[i+1]), sizeof(pixel_t));
    if (last_time==true) {
      segment->sdata.pixel[i].it--;
    }
    memcpy(segment->iguess[i], segment->iguess[i+1], sizeof(float)*_KPARS);
  }
}

void grasp_segment_remove_pixels(grasp_segment_t *segment, int npixels, int *pixels) {
  int i;

  for (i=0; i<npixels-1; i++) { // To ensure monitonic incrasing order
    assert(pixels[i]<pixels[i+1]);
  }

  for (i=0; i<npixels; i++) {
    grasp_segment_remove_pixel(segment, pixels[i]-i);
  }
}

void grasp_segment_set_nt(sensor_data_t *sdata, int nt) {
  assert(nt>=0);
  assert(nt<=_KITIME);

  sdata->nt=nt;
}

void grasp_segment_set_nx(sensor_data_t *sdata, int nx) {
  assert(nx>=0);
  assert(nx<=_KIX);

  sdata->nx=nx;
}

void grasp_segment_set_ny(sensor_data_t *sdata, int ny) {
  assert(ny>=0);
  assert(ny<=_KIY);

  sdata->ny=ny;
}

void grasp_segment_set_npixels(sensor_data_t *sdata, int npixels) {
  assert(npixels>=0);
  assert(npixels<=_KIMAGE);

  sdata->npixels=npixels;
}

void grasp_segment_set_pixel_it(sensor_data_t *sdata, int ipixel, int it) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(it>0);
  assert(it<=_KITIME);

  sdata->pixel[ipixel].it=it;
}

void grasp_segment_set_pixel_ix(sensor_data_t *sdata, int ipixel, int ix) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(ix>0);
  assert(ix<=_KIX);

  sdata->pixel[ipixel].ix=ix;
}

void grasp_segment_set_pixel_iy(sensor_data_t *sdata, int ipixel, int iy) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iy>0);
  assert(iy<=_KIY);


  sdata->pixel[ipixel].iy=iy;
}

void grasp_segment_set_pixel_cloudy(sensor_data_t *sdata, int ipixel, int cloudy) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(cloudy==0||cloudy==1);

  sdata->pixel[ipixel].cloudy=cloudy;
}

void grasp_segment_set_pixel_irow(sensor_data_t *sdata, int ipixel, int irow) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  sdata->pixel[ipixel].irow=irow;
}

void grasp_segment_set_pixel_icol(sensor_data_t *sdata, int ipixel, int icol) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  sdata->pixel[ipixel].icol=icol;
}

void grasp_segment_set_pixel_file_index(sensor_data_t *sdata, int ipixel, int file_index) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(file_index>=-1);

  sdata->pixel[ipixel].file_index=file_index;
}

void grasp_segment_set_pixel_x(sensor_data_t *sdata, int ipixel, float x) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(x>=-180.0);
  assert(x<=360.0);

  sdata->pixel[ipixel].x=x;
}

void grasp_segment_set_pixel_y(sensor_data_t *sdata, int ipixel, float y) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(y>=-90.0);
  assert(y<=90.0);

  sdata->pixel[ipixel].y=y;
}

void grasp_segment_set_pixel_t(sensor_data_t *sdata, int ipixel, int64_t t) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);


  sdata->pixel[ipixel].t=t;
}

void grasp_segment_set_pixel_out_t(sensor_data_t *sdata, int ipixel, int out_t) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  sdata->pixel[ipixel].out_t=out_t;
}

void grasp_segment_set_pixel_out_x(sensor_data_t *sdata, int ipixel, int out_x) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  sdata->pixel[ipixel].out_x=out_x;
}

void grasp_segment_set_pixel_out_y(sensor_data_t *sdata, int ipixel, int out_y) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  sdata->pixel[ipixel].out_y=out_y;
}

void grasp_segment_set_pixel_masl(sensor_data_t *sdata, int ipixel, float masl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  sdata->pixel[ipixel].masl=masl;
}

void grasp_segment_set_pixel_hobs(sensor_data_t *sdata, int ipixel, float hobs) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  sdata->pixel[ipixel].hobs=hobs;
}

void grasp_segment_set_pixel_land_percent(sensor_data_t *sdata, int ipixel, float land_percent) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(land_percent>=0.0);
  assert(land_percent<=100.0);

  sdata->pixel[ipixel].land_percent=land_percent;
}

void grasp_segment_set_pixel_nwl(sensor_data_t *sdata, int ipixel, int nwl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(nwl>0);
  assert(nwl<=_KWM);

  sdata->pixel[ipixel].nwl=nwl;
}

void grasp_segment_set_pixel_ifgas(sensor_data_t *sdata, int ipixel, int ifgas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(ifgas==0||ifgas==1);

  sdata->pixel[ipixel].ifgas=ifgas;
}

void grasp_segment_set_pixel_hvp(sensor_data_t *sdata, int ipixel, int ivm, float hvp) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(ivm>=0);
  assert(ivm<=_KVERTM);

  sdata->pixel[ipixel].hvp[ivm]=hvp;
}

void grasp_segment_set_pixel_meas_wl(sensor_data_t *sdata, int ipixel, int iwl, float wl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  sdata->pixel[ipixel].meas[iwl].wl=wl;
}

void grasp_segment_set_pixel_meas_ind_wl(sensor_data_t *sdata, int ipixel, int iwl, float ind_wl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  sdata->pixel[ipixel].meas[iwl].ind_wl=ind_wl;
}

void grasp_segment_set_pixel_meas_nsurf(sensor_data_t *sdata, int ipixel, int iwl, int nsurf) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(nsurf>=0);
  assert(nsurf<=_KSURF);

  sdata->pixel[ipixel].meas[iwl].nsurf=nsurf;
}

void grasp_segment_set_pixel_meas_gaspar(sensor_data_t *sdata, int ipixel, int iwl, float gaspar) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  sdata->pixel[ipixel].meas[iwl].gaspar=gaspar;
}

void grasp_segment_set_pixel_meas_sza(sensor_data_t *sdata, int ipixel, int iwl, float sza) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  sdata->pixel[ipixel].meas[iwl].sza=sza;
}

void grasp_segment_set_pixel_meas_groundpar(sensor_data_t *sdata, int ipixel, int iwl, int isurf, float groundpar) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(isurf>=0);
  assert(isurf<_KSURF);

  sdata->pixel[ipixel].meas[iwl].groundpar[isurf]=groundpar;
}

void grasp_segment_set_pixel_meas_nip(sensor_data_t *sdata, int ipixel, int iwl, int nip) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(nip>0);
  assert(nip<=_KIP);

  sdata->pixel[ipixel].meas[iwl].nip=nip;
}

void grasp_segment_set_pixel_meas_meas_type(sensor_data_t *sdata, int ipixel, int iwl, int ip, int meas_type) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  sdata->pixel[ipixel].meas[iwl].meas_type[ip]=meas_type;
}

void grasp_segment_set_pixel_meas_nbvm(sensor_data_t *sdata, int ipixel, int iwl, int ip, int nbvm) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(nbvm>=0);
  assert(nbvm<=_KNBVM);

  sdata->pixel[ipixel].meas[iwl].nbvm[ip]=nbvm;
}

void grasp_segment_set_pixel_meas_ifcov(sensor_data_t *sdata, int ipixel, int iwl, int ip, int ifcov) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ifcov==0||ifcov==1);

  sdata->pixel[ipixel].meas[iwl].ifcov[ip]=ifcov;
}

void grasp_segment_set_pixel_meas_ifmp(sensor_data_t *sdata, int ipixel, int iwl, int ip, int ifmp) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ifmp==0||ifmp==1);

  sdata->pixel[ipixel].meas[iwl].ifmp[ip]=ifmp;
}

void
grasp_segment_set_pixel_meas_thetav(sensor_data_t *sdata, int ipixel, int iwl, int ip, int ivalidmeas, float thetav) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<sdata->pixel[ipixel].meas[iwl].nbvm[ip]);

  sdata->pixel[ipixel].meas[iwl].thetav[ip][ivalidmeas]=thetav;
}

void grasp_segment_set_pixel_meas_phi(sensor_data_t *sdata, int ipixel, int iwl, int ip, int ivalidmeas, float phi) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<sdata->pixel[ipixel].meas[iwl].nbvm[ip]);

  sdata->pixel[ipixel].meas[iwl].phi[ip][ivalidmeas]=phi;
}

void
grasp_segment_set_pixel_meas_cmtrx(sensor_data_t *sdata, int ipixel, int iwl, int ip, int ivalidmeas, float cmtrx) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<sdata->pixel[ipixel].meas[iwl].nbvm[ip]);

  sdata->pixel[ipixel].meas[iwl].cmtrx[ip][ivalidmeas]=cmtrx;
}

void
grasp_segment_set_pixel_meas_mprof(sensor_data_t *sdata, int ipixel, int iwl, int ip, int ivalidmeas, float mprof) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<sdata->pixel[ipixel].meas[iwl].nbvm[ip]);
  assert(ivalidmeas<_KVERTM);

  sdata->pixel[ipixel].meas[iwl].mprof[ip][ivalidmeas]=mprof;
}

void grasp_segment_set_pixel_meas_tod(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float tod) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].tod[ivalidmeas]=tod;
}

void grasp_segment_set_pixel_meas_aod(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float aod) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].aod[ivalidmeas]=aod;
}

void grasp_segment_set_pixel_meas_aaod(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float aaod) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].aaod[ivalidmeas]=aaod;
}

void grasp_segment_set_pixel_meas_htod(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float htod) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].htod[ivalidmeas]=htod;
}

void grasp_segment_set_pixel_meas_p11(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p11[ivalidmeas]=p11;
}

void grasp_segment_set_pixel_meas_p12(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p12) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p12[ivalidmeas]=p12;
}

void grasp_segment_set_pixel_meas_p22(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p22) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p22[ivalidmeas]=p22;
}

void grasp_segment_set_pixel_meas_p33(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p33) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);
  
  sdata->pixel[ipixel].meas[iwl].p33[ivalidmeas]=p33;
}

void grasp_segment_set_pixel_meas_p34(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p34) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p34[ivalidmeas]=p34;
}

void grasp_segment_set_pixel_meas_p44(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p44) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p44[ivalidmeas]=p44;
}

void grasp_segment_set_pixel_meas_p11_rel_ang(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_rel_ang) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p11_rel_ang[ivalidmeas]=p11_rel_ang;
}

void grasp_segment_set_pixel_meas_p12_rel(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p12_rel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p12_rel[ivalidmeas]=p12_rel;
}

void grasp_segment_set_pixel_meas_i(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float i) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].i[ivalidmeas]=i;
}

void grasp_segment_set_pixel_meas_q(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float q) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].q[ivalidmeas]=q;
}


void grasp_segment_set_pixel_meas_u(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float u) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].u[ivalidmeas]=u;
}

void grasp_segment_set_pixel_meas_p(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p[ivalidmeas]=p;
}

void grasp_segment_set_pixel_meas_p11_intd(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p11_intd[ivalidmeas]=p11_intd;
}

void grasp_segment_set_pixel_meas_p11_intd_cut_off_1(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd_cut_off_1) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p11_intd_cut_off_1[ivalidmeas]=p11_intd_cut_off_1;
}

void grasp_segment_set_pixel_meas_p11_intd_cut_off_2(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd_cut_off_2) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p11_intd_cut_off_2[ivalidmeas]=p11_intd_cut_off_2;
}

void grasp_segment_set_pixel_meas_p11_intd_cut_off_3(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd_cut_off_3) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p11_intd_cut_off_3[ivalidmeas]=p11_intd_cut_off_3;
}

void grasp_segment_set_pixel_meas_p11_intd_cut_off_4(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p11_intd_cut_off_4) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p11_intd_cut_off_4[ivalidmeas]=p11_intd_cut_off_4;
}


void grasp_segment_set_pixel_meas_i_rel_sum(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float i_rel_sum) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].i_rel_sum[ivalidmeas]=i_rel_sum;
}

void grasp_segment_set_pixel_meas_p_rel(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float p_rel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  sdata->pixel[ipixel].meas[iwl].p_rel[ivalidmeas]=p_rel;
}

void grasp_segment_set_pixel_meas_ls(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float ls) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  sdata->pixel[ipixel].meas[iwl].ls[ivalidmeas]=ls;
}

void grasp_segment_set_pixel_meas_dp(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float dp) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  sdata->pixel[ipixel].meas[iwl].dp[ivalidmeas]=dp;
}

void grasp_segment_set_pixel_meas_dpar(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float dpar) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  sdata->pixel[ipixel].meas[iwl].dpar[ivalidmeas]=dpar;
}

void grasp_segment_set_pixel_meas_dper(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float dper) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  sdata->pixel[ipixel].meas[iwl].dper[ivalidmeas]=dper;
}

void grasp_segment_set_pixel_meas_rl(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float rl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  sdata->pixel[ipixel].meas[iwl].rl[ivalidmeas]=rl;
}

void grasp_segment_set_pixel_meas_vbs(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float vbs) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  sdata->pixel[ipixel].meas[iwl].vbs[ivalidmeas]=vbs;
}

void grasp_segment_set_pixel_meas_vext(sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas, float vext) {

  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  sdata->pixel[ipixel].meas[iwl].vext[ivalidmeas]=vext;
}

int grasp_segment_get_nt(const sensor_data_t *sdata) {

  return sdata->nt;
}

int grasp_segment_get_nx(const sensor_data_t *sdata) {

  return sdata->nx;
}

int grasp_segment_get_ny(const sensor_data_t *sdata) {

  return sdata->ny;
}

int grasp_segment_get_npixels(const sensor_data_t *sdata) {

  return sdata->npixels;
}

int grasp_segment_get_pixel_it(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].it;
}

int grasp_segment_get_pixel_ix(const sensor_data_t *sdata, int ipixel) {

  return sdata->pixel[ipixel].ix;
}

int grasp_segment_get_pixel_iy(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);


  return sdata->pixel[ipixel].iy;
}

int grasp_segment_get_pixel_cloudy(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].cloudy;
}

int grasp_segment_get_pixel_irow(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].irow;
}

int grasp_segment_get_pixel_icol(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].icol;
}

int grasp_segment_get_pixel_file_index(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].file_index;
}

float grasp_segment_get_pixel_x(const sensor_data_t *sdata, int ipixel) {

  return sdata->pixel[ipixel].x;
}

float grasp_segment_get_pixel_y(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].y;
}

int64_t grasp_segment_get_pixel_t(const sensor_data_t *sdata, int ipixel) {


  return sdata->pixel[ipixel].t;
}

int grasp_segment_get_pixel_out_t(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].out_t;
}

int grasp_segment_get_pixel_out_x(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].out_x;
}

int grasp_segment_get_pixel_out_y(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].out_y;
}

float grasp_segment_get_pixel_masl(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].masl;
}

float grasp_segment_get_pixel_hobs(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].hobs;
}

float grasp_segment_get_pixel_land_percent(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].land_percent;
}

int grasp_segment_get_pixel_nwl(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].nwl;
}

int grasp_segment_get_pixel_ifgas(const sensor_data_t *sdata, int ipixel) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].ifgas;
}

float grasp_segment_get_pixel_hvp(const sensor_data_t *sdata, int ipixel, int ivm) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(ivm>=0);
  assert(ivm<=_KVERTM);

  return sdata->pixel[ipixel].hvp[ivm];
}

float grasp_segment_get_pixel_meas_wl(const sensor_data_t *sdata, int ipixel, int iwl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);

  return sdata->pixel[ipixel].meas[iwl].wl;
}

float grasp_segment_get_pixel_meas_ind_wl(const sensor_data_t *sdata, int ipixel, int iwl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  return sdata->pixel[ipixel].meas[iwl].ind_wl;
}

int grasp_segment_get_pixel_meas_nsurf(const sensor_data_t *sdata, int ipixel, int iwl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  return sdata->pixel[ipixel].meas[iwl].nsurf;
}

float grasp_segment_get_pixel_meas_gaspar(const sensor_data_t *sdata, int ipixel, int iwl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  return sdata->pixel[ipixel].meas[iwl].gaspar;
}

float grasp_segment_get_pixel_meas_sza(const sensor_data_t *sdata, int ipixel, int iwl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  return sdata->pixel[ipixel].meas[iwl].sza;
}

float grasp_segment_get_pixel_meas_groundpar(const sensor_data_t *sdata, int ipixel, int iwl, int isurf) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(isurf>=0);
  assert(isurf<_KSURF);

  return sdata->pixel[ipixel].meas[iwl].groundpar[isurf];
}

int grasp_segment_get_pixel_meas_nip(const sensor_data_t *sdata, int ipixel, int iwl) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);

  return sdata->pixel[ipixel].meas[iwl].nip;
}

int grasp_segment_get_pixel_meas_meas_type(const sensor_data_t *sdata, int ipixel, int iwl, int ip) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  return sdata->pixel[ipixel].meas[iwl].meas_type[ip];
}

int grasp_segment_get_pixel_meas_nbvm(const sensor_data_t *sdata, int ipixel, int iwl, int ip) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  return sdata->pixel[ipixel].meas[iwl].nbvm[ip];
}

int grasp_segment_get_pixel_meas_ifcov(const sensor_data_t *sdata, int ipixel, int iwl, int ip) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  return sdata->pixel[ipixel].meas[iwl].ifcov[ip];
}

int grasp_segment_get_pixel_meas_ifmp(const sensor_data_t *sdata, int ipixel, int iwl, int ip) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  return sdata->pixel[ipixel].meas[iwl].ifmp[ip];
}

float grasp_segment_get_pixel_meas_thetav(const sensor_data_t *sdata, int ipixel, int iwl, int ip, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<sdata->pixel[ipixel].meas[iwl].nbvm[ip]);

  return sdata->pixel[ipixel].meas[iwl].thetav[ip][ivalidmeas];
}

float grasp_segment_get_pixel_meas_phi(const sensor_data_t *sdata, int ipixel, int iwl, int ip, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<sdata->pixel[ipixel].meas[iwl].nbvm[ip]);

  return sdata->pixel[ipixel].meas[iwl].phi[ip][ivalidmeas];
}

float grasp_segment_get_pixel_meas_cmtrx(const sensor_data_t *sdata, int ipixel, int iwl, int ip, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<sdata->pixel[ipixel].meas[iwl].nbvm[ip]);

  return sdata->pixel[ipixel].meas[iwl].cmtrx[ip][ivalidmeas];
}

float grasp_segment_get_pixel_meas_mprof(const sensor_data_t *sdata, int ipixel, int iwl, int ip, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<sdata->pixel[ipixel].meas[iwl].nbvm[ip]);
  assert(ivalidmeas<_KVERTM);

  return sdata->pixel[ipixel].meas[iwl].mprof[ip][ivalidmeas];
}

float grasp_segment_get_pixel_meas_tod(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {

  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].tod[ivalidmeas];
}

float grasp_segment_get_pixel_meas_aod(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {

  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].aod[ivalidmeas];
}

float grasp_segment_get_pixel_meas_aaod(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {

  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].aaod[ivalidmeas];
}

float grasp_segment_get_pixel_meas_htod(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {

  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].htod[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p11(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p11[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p12(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p12[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p22(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p22[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p33(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p33[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p34(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p34[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p44(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p44[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p11_rel_ang(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p11_rel_ang[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p12_rel(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p12_rel[ivalidmeas];
}


float grasp_segment_get_pixel_meas_i(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {

  return sdata->pixel[ipixel].meas[iwl].i[ivalidmeas];
}

float grasp_segment_get_pixel_meas_q(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].q[ivalidmeas];
}

float grasp_segment_get_pixel_meas_u(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].u[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p[ivalidmeas];
}

float grasp_segment_get_pixel_meas_i_rel_sum(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].i_rel_sum[ivalidmeas];
}

float grasp_segment_get_pixel_meas_p_rel(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return sdata->pixel[ipixel].meas[iwl].p_rel[ivalidmeas];
}

float grasp_segment_get_pixel_meas_ls(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return sdata->pixel[ipixel].meas[iwl].ls[ivalidmeas];
}

float grasp_segment_get_pixel_meas_dp(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return sdata->pixel[ipixel].meas[iwl].dp[ivalidmeas];
}

float grasp_segment_get_pixel_meas_rl(const sensor_data_t *sdata, int ipixel, int iwl, int ivalidmeas) {
  assert(ipixel>=0);
  assert(ipixel<_KIMAGE);
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return sdata->pixel[ipixel].meas[iwl].rl[ivalidmeas];
}

// ===== Setters for pixel =====


void grasp_segment_pixel_set_pixel_it(pixel_t *pixel, int it) {
  assert(it>0);
  assert(it<=_KITIME);

  pixel->it=it;
}

void grasp_segment_pixel_set_pixel_ix(pixel_t *pixel, int ix) {
  assert(ix>0);
  assert(ix<=_KIX);

  pixel->ix=ix;
}

void grasp_segment_pixel_set_pixel_iy(pixel_t *pixel, int iy) {
  assert(iy>0);
  assert(iy<=_KIY);


  pixel->iy=iy;
}

void grasp_segment_pixel_set_pixel_cloudy(pixel_t *pixel, int cloudy) {
  assert(cloudy==0||cloudy==1);

  pixel->cloudy=cloudy;
}

void grasp_segment_pixel_set_pixel_irow(pixel_t *pixel, int irow) {

  pixel->irow=irow;
}

void grasp_segment_pixel_set_pixel_icol(pixel_t *pixel, int icol) {

  pixel->icol=icol;
}

void grasp_segment_pixel_set_pixel_file_index(pixel_t *pixel, int file_index) {
  assert(file_index>=-1);

  pixel->file_index=file_index;
}

void grasp_segment_pixel_set_pixel_x(pixel_t *pixel, float x) {
  assert(x>=-180.0);
  assert(x<=360.0);

  pixel->x=x;
}

void grasp_segment_pixel_set_pixel_y(pixel_t *pixel, float y) {
  assert(y>=-90.0);
  assert(y<=90.0);

  pixel->y=y;
}

void grasp_segment_pixel_set_pixel_t(pixel_t *pixel, int64_t t) {


  pixel->t=t;
}

void grasp_segment_pixel_set_pixel_out_t(pixel_t *pixel, int out_t) {

  pixel->out_t=out_t;
}

void grasp_segment_pixel_set_pixel_out_x(pixel_t *pixel, int out_x) {

  pixel->out_x=out_x;
}

void grasp_segment_pixel_set_pixel_out_y(pixel_t *pixel, int out_y) {

  pixel->out_y=out_y;
}

void grasp_segment_pixel_set_pixel_masl(pixel_t *pixel, float masl) {

  pixel->masl=masl;
}

void grasp_segment_pixel_set_pixel_hobs(pixel_t *pixel, float hobs) {

  pixel->hobs=hobs;
}

void grasp_segment_pixel_set_pixel_land_percent(pixel_t *pixel, float land_percent) {
  assert(land_percent>=0.0);
  assert(land_percent<=100.0);

  pixel->land_percent=land_percent;
}

void grasp_segment_pixel_set_pixel_nwl(pixel_t *pixel, int nwl) {
  assert(nwl>0);
  assert(nwl<=_KWM);

  pixel->nwl=nwl;
}

void grasp_segment_pixel_set_pixel_ifgas(pixel_t *pixel, int ifgas) {
  assert(ifgas==0||ifgas==1);

  pixel->ifgas=ifgas;
}

void grasp_segment_pixel_set_pixel_hvp(pixel_t *pixel, int ivm, float hvp) {
  assert(ivm>=0);
  assert(ivm<=_KVERTM);

  pixel->hvp[ivm]=hvp;
}

void grasp_segment_pixel_set_pixel_meas_wl(pixel_t *pixel, int iwl, float wl) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  pixel->meas[iwl].wl=wl;
}

void grasp_segment_pixel_set_pixel_meas_ind_wl(pixel_t *pixel, int iwl, float ind_wl) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  pixel->meas[iwl].ind_wl=ind_wl;
}

void grasp_segment_pixel_set_pixel_meas_nsurf(pixel_t *pixel, int iwl, int nsurf) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(nsurf>=0);
  assert(nsurf<=_KSURF);

  pixel->meas[iwl].nsurf=nsurf;
}

void grasp_segment_pixel_set_pixel_meas_gaspar(pixel_t *pixel, int iwl, float gaspar) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  pixel->meas[iwl].gaspar=gaspar;
}

void grasp_segment_pixel_set_pixel_meas_sza(pixel_t *pixel, int iwl, float sza) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  pixel->meas[iwl].sza=sza;
}

void grasp_segment_pixel_set_pixel_meas_groundpar(pixel_t *pixel, int iwl, int isurf, float groundpar) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(isurf>=0);
  assert(isurf<_KSURF);

  pixel->meas[iwl].groundpar[isurf]=groundpar;
}

void grasp_segment_pixel_set_pixel_meas_nip(pixel_t *pixel, int iwl, int nip) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(nip>0);
  assert(nip<=_KIP);

  pixel->meas[iwl].nip=nip;
}

void grasp_segment_pixel_set_pixel_meas_meas_type(pixel_t *pixel, int iwl, int ip, int meas_type) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  pixel->meas[iwl].meas_type[ip]=meas_type;
}

void grasp_segment_pixel_set_pixel_meas_nbvm(pixel_t *pixel, int iwl, int ip, int nbvm) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(nbvm>=0);
  assert(nbvm<=_KNBVM);

  pixel->meas[iwl].nbvm[ip]=nbvm;
}

void grasp_segment_pixel_set_pixel_meas_ifcov(pixel_t *pixel, int iwl, int ip, int ifcov) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ifcov==0||ifcov==1);

  pixel->meas[iwl].ifcov[ip]=ifcov;
}

void grasp_segment_pixel_set_pixel_meas_ifmp(pixel_t *pixel, int iwl, int ip, int ifmp) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ifmp==0||ifmp==1);

  pixel->meas[iwl].ifmp[ip]=ifmp;
}

void grasp_segment_pixel_set_pixel_meas_thetav(pixel_t *pixel, int iwl, int ip, int ivalidmeas, float thetav) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);

  pixel->meas[iwl].thetav[ip][ivalidmeas]=thetav;
}

void grasp_segment_pixel_set_pixel_meas_phi(pixel_t *pixel, int iwl, int ip, int ivalidmeas, float phi) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);

  pixel->meas[iwl].phi[ip][ivalidmeas]=phi;
}

void grasp_segment_pixel_set_pixel_meas_cmtrx(pixel_t *pixel, int iwl, int ip, int ivalidmeas, float cmtrx) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);

  pixel->meas[iwl].cmtrx[ip][ivalidmeas]=cmtrx;
}

void grasp_segment_pixel_set_pixel_meas_mprof(pixel_t *pixel, int iwl, int ip, int ivalidmeas, float mprof) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  pixel->meas[iwl].mprof[ip][ivalidmeas]=mprof;
}

void grasp_segment_pixel_set_pixel_meas_tod(pixel_t *pixel, int iwl, int ivalidmeas, float tod) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].tod[ivalidmeas]=tod;
}

void grasp_segment_pixel_set_pixel_meas_aod(pixel_t *pixel, int iwl, int ivalidmeas, float aod) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].aod[ivalidmeas]=aod;
}

void grasp_segment_pixel_set_pixel_meas_aaod(pixel_t *pixel, int iwl, int ivalidmeas, float aaod) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].aaod[ivalidmeas]=aaod;
}

void grasp_segment_pixel_set_pixel_meas_htod(pixel_t *pixel, int iwl, int ivalidmeas, float htod) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].htod[ivalidmeas]=htod;
}

void grasp_segment_pixel_set_pixel_meas_p11(pixel_t *pixel, int iwl, int ivalidmeas, float p11) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p11[ivalidmeas]=p11;
}

void grasp_segment_pixel_set_pixel_meas_p12(pixel_t *pixel, int iwl, int ivalidmeas, float p12) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p12[ivalidmeas]=p12;
}

void grasp_segment_pixel_set_pixel_meas_p22(pixel_t *pixel, int iwl, int ivalidmeas, float p22) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p22[ivalidmeas]=p22;
}

void grasp_segment_pixel_set_pixel_meas_p33(pixel_t *pixel, int iwl, int ivalidmeas, float p33) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);
  assert(p33>=0.0);

  pixel->meas[iwl].p33[ivalidmeas]=p33;
}

void grasp_segment_pixel_set_pixel_meas_p34(pixel_t *pixel, int iwl, int ivalidmeas, float p34) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p34[ivalidmeas]=p34;
}

void grasp_segment_pixel_set_pixel_meas_p44(pixel_t *pixel, int iwl, int ivalidmeas, float p44) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p44[ivalidmeas]=p44;
}

void grasp_segment_pixel_set_pixel_meas_p11_rel_ang(pixel_t *pixel, int iwl, int ivalidmeas, float p11_rel_ang) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p44[ivalidmeas]=p11_rel_ang;
}

void grasp_segment_pixel_set_pixel_meas_p12_rel(pixel_t *pixel, int iwl, int ivalidmeas, float p12_rel) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p44[ivalidmeas]=p12_rel;
}

void grasp_segment_pixel_set_pixel_meas_i(pixel_t *pixel, int iwl, int ivalidmeas, float i) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].i[ivalidmeas]=i;
}

void grasp_segment_pixel_set_pixel_meas_q(pixel_t *pixel, int iwl, int ivalidmeas, float q) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].q[ivalidmeas]=q;
}

void grasp_segment_pixel_set_pixel_meas_u(pixel_t *pixel, int iwl, int ivalidmeas, float u) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].u[ivalidmeas]=u;
}

void grasp_segment_pixel_set_pixel_meas_p(pixel_t *pixel, int iwl, int ivalidmeas, float p) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p[ivalidmeas]=p;
}

void grasp_segment_pixel_set_pixel_meas_i_rel_sum(pixel_t *pixel, int iwl, int ivalidmeas, float i_rel_sum) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p[ivalidmeas]=i_rel_sum;
}

void grasp_segment_pixel_set_pixel_meas_p_rel(pixel_t *pixel, int iwl, int ivalidmeas, float p_rel) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  pixel->meas[iwl].p[ivalidmeas]=p_rel;
}

void grasp_segment_pixel_set_pixel_meas_ls(pixel_t *pixel, int iwl, int ivalidmeas, float ls) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  pixel->meas[iwl].ls[ivalidmeas]=ls;
}

void grasp_segment_pixel_set_pixel_meas_dp(pixel_t *pixel, int iwl, int ivalidmeas, float dp) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  pixel->meas[iwl].dp[ivalidmeas]=dp;
}

void grasp_segment_pixel_set_pixel_meas_dper(pixel_t *pixel, int iwl, int ivalidmeas, float dper) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  pixel->meas[iwl].dper[ivalidmeas]=dper;
}

void grasp_segment_pixel_set_pixel_meas_dpar(pixel_t *pixel, int iwl, int ivalidmeas, float dpar) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  pixel->meas[iwl].dpar[ivalidmeas]=dpar;
}

void grasp_segment_pixel_set_pixel_meas_rl(pixel_t *pixel, int iwl, int ivalidmeas, float rl) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  pixel->meas[iwl].rl[ivalidmeas]=rl;
}

void grasp_segment_pixel_set_pixel_meas_vbs(pixel_t *pixel, int iwl, int ivalidmeas, float vbs) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  pixel->meas[iwl].vbs[ivalidmeas]=vbs;
}

void grasp_segment_pixel_set_pixel_meas_vext(pixel_t *pixel, int iwl, int ivalidmeas, float vext) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  pixel->meas[iwl].vext[ivalidmeas]=vext;
}


// ===== Getters for pixel =====


int grasp_segment_pixel_get_pixel_it(const pixel_t *pixel) {

  return pixel->it;
}

int grasp_segment_pixel_get_pixel_ix(const pixel_t *pixel) {

  return pixel->ix;
}

int grasp_segment_pixel_get_pixel_iy(const pixel_t *pixel) {


  return pixel->iy;
}

int grasp_segment_pixel_get_pixel_cloudy(const pixel_t *pixel) {

  return pixel->cloudy;
}

int grasp_segment_pixel_get_pixel_irow(const pixel_t *pixel) {

  return pixel->irow;
}

int grasp_segment_pixel_get_pixel_icol(const pixel_t *pixel) {

  return pixel->icol;
}

int grasp_segment_pixel_get_pixel_file_index(const pixel_t *pixel) {

  return pixel->file_index;
}

float grasp_segment_pixel_get_pixel_x(const pixel_t *pixel) {

  return pixel->x;
}

float grasp_segment_pixel_get_pixel_y(const pixel_t *pixel) {

  return pixel->y;
}

int64_t grasp_segment_pixel_get_pixel_t(const pixel_t *pixel) {


  return pixel->t;
}

int grasp_segment_pixel_get_pixel_out_t(const pixel_t *pixel) {

  return pixel->out_t;
}

int grasp_segment_pixel_get_pixel_out_x(const pixel_t *pixel) {

  return pixel->out_x;
}

int grasp_segment_pixel_get_pixel_out_y(const pixel_t *pixel) {

  return pixel->out_y;
}

float grasp_segment_pixel_get_pixel_masl(const pixel_t *pixel) {

  return pixel->masl;
}

float grasp_segment_pixel_get_pixel_hobs(const pixel_t *pixel) {

  return pixel->hobs;
}

float grasp_segment_pixel_get_pixel_land_percent(const pixel_t *pixel) {

  return pixel->land_percent;
}

int grasp_segment_pixel_get_pixel_nwl(const pixel_t *pixel) {

  return pixel->nwl;
}

int grasp_segment_pixel_get_pixel_ifgas(const pixel_t *pixel) {

  return pixel->ifgas;
}

float grasp_segment_pixel_get_pixel_hvp(const pixel_t *pixel, int ivm) {
  assert(ivm>=0);
  assert(ivm<=_KVERTM);

  return pixel->hvp[ivm];
}

float grasp_segment_pixel_get_pixel_meas_wl(const pixel_t *pixel, int iwl) {

  return pixel->meas[iwl].wl;
}

float grasp_segment_pixel_get_pixel_meas_ind_wl(const pixel_t *pixel, int iwl) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  return pixel->meas[iwl].ind_wl;
}

int grasp_segment_pixel_get_pixel_meas_nsurf(const pixel_t *pixel, int iwl) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  return pixel->meas[iwl].nsurf;
}

float grasp_segment_pixel_get_pixel_meas_gaspar(const pixel_t *pixel, int iwl) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  return pixel->meas[iwl].gaspar;
}

float grasp_segment_pixel_get_pixel_meas_sza(const pixel_t *pixel, int iwl) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  return pixel->meas[iwl].sza;
}

float grasp_segment_pixel_get_pixel_meas_groundpar(const pixel_t *pixel, int iwl, int isurf) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(isurf>=0);
  assert(isurf<_KSURF);

  return pixel->meas[iwl].groundpar[isurf];
}

int grasp_segment_pixel_get_pixel_meas_nip(const pixel_t *pixel, int iwl) {
  assert(iwl>=0);
  assert(iwl<_KWM);

  return pixel->meas[iwl].nip;
}

int grasp_segment_pixel_get_pixel_meas_meas_type(const pixel_t *pixel, int iwl, int ip) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  return pixel->meas[iwl].meas_type[ip];
}

int grasp_segment_pixel_get_pixel_meas_nbvm(const pixel_t *pixel, int iwl, int ip) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  return pixel->meas[iwl].nbvm[ip];
}

int grasp_segment_pixel_get_pixel_meas_ifcov(const pixel_t *pixel, int iwl, int ip) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  return pixel->meas[iwl].ifcov[ip];
}

int grasp_segment_pixel_get_pixel_meas_ifmp(const pixel_t *pixel, int iwl, int ip) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);

  return pixel->meas[iwl].ifmp[ip];
}

float grasp_segment_pixel_get_pixel_meas_thetav(const pixel_t *pixel, int iwl, int ip, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);

  return pixel->meas[iwl].thetav[ip][ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_phi(const pixel_t *pixel, int iwl, int ip, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);

  return pixel->meas[iwl].phi[ip][ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_cmtrx(const pixel_t *pixel, int iwl, int ip, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);

  return pixel->meas[iwl].cmtrx[ip][ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_mprof(const pixel_t *pixel, int iwl, int ip, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ip>=0);
  assert(ip<_KIP);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return pixel->meas[iwl].mprof[ip][ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_tod(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].tod[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_aod(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].aod[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_aaod(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].aaod[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_htod(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].htod[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p11(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p11[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p12(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p12[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p22(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p22[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p33(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p33[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p34(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p34[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p44(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p44[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p11_rel_ang(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p11_rel_ang[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p12_rel(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p12_rel[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_i(const pixel_t *pixel, int iwl, int ivalidmeas) {

  return pixel->meas[iwl].i[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_q(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].q[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_u(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].u[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_i_rel_sum(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].i_rel_sum[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_p_rel(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_NBVM);

  return pixel->meas[iwl].p_rel[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_ls(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return pixel->meas[iwl].ls[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_dp(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return pixel->meas[iwl].dp[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_dpar(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return pixel->meas[iwl].dpar[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_dper(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return pixel->meas[iwl].dper[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_rl(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return pixel->meas[iwl].rl[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_vbs(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return pixel->meas[iwl].vbs[ivalidmeas];
}

float grasp_segment_pixel_get_pixel_meas_vext(const pixel_t *pixel, int iwl, int ivalidmeas) {
  assert(iwl>=0);
  assert(iwl<_KWM);
  assert(ivalidmeas>=0);
  assert(ivalidmeas<_KVERTM);

  return pixel->meas[iwl].vext[ivalidmeas];
}

