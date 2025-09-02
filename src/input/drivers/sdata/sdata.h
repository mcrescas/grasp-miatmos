/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/**
 * @file  sdata.h
 * @author Fabrice Ducos <fabrice.ducos@univ-lille1.fr>
 *
 */

#ifndef SDATA_H
#define SDATA_H

#include <grasp/utils.h>
#include "../../grasp_input.h"
#include "../../util/grasp_box.h"
#include "../../../global/grasp_retrieval_meas_type.h"
#include "../../util/grasp_driver_settings.h"
#include "mod_par_inv.inc"
#include "mod_par_OS.inc"
#include <time.h>
#include "grasp_input_driver_sdata_error_codes.h"

typedef struct SDATA_HANDLE_ SDATA_HANDLE;
typedef struct SDATA_RECORD_ SDATA_RECORD;


#define SDATA_MAX_NWL    _KWM    /* maximal number of wavelengths */
#define SDATA_MAX_NBVM   _NBVM   /* maximal number of geometries */
#define SDATA_MAX_KNBVM  _KNBVM  /* maximum between _NBVM and _KVERTM */
#define SDATA_MAX_NX     _KIX
#define SDATA_MAX_NY     _KIY
#define SDATA_MAX_NSURF  _KSURF
#define SDATA_MAX_NIP    _KIP
#define SDATA_MAX_KVERTM _KVERTM /* maximal number of vertical measurements */

typedef struct SDATA_TIMESTAMP_ {
  /* all the fields below are redundant, and are different expressions (or parts) of the same timestamp.
   * They are provided here as a convenience for the user and also for control.
   * Some fields may be removed in more mature versions of the project.
   */
  char str_date_time[20 + 1]; /* format YYYY-MM-DDThh:mm:ssZ */
  time_t unix_time; /* representation of the timestamp in number of seconds since the Unix Epoch */
  long index_of_day; /* representation of the timestamp in number of days since the Unix Epoch (starts at 0) */
} SDATA_TIMESTAMP;

typedef struct SDATA_TIME_LIMITS_ {
  SDATA_TIMESTAMP first_record;
  SDATA_TIMESTAMP last_record;
} SDATA_TIME_LIMITS;

typedef struct SDATA_HEADER_ {
  char format_version[31 + 1];
  int nx;
  int ny;
  int nrecords; /* number of records (one record is a SDATA_RECORD object). One record may contain several pixels with the same time index (it). */
  int npixels;  /* total number of pixels (SDATA_PIXEL objects) in the SDATA file (sum of the pixels of all the records) */
  int xmin, xmax; /* x-range of the pixels available in the SDATA */
  int ymin, ymax; /* y-range of the pixels available in the SDATA */
  SDATA_TIME_LIMITS time_limits;
} SDATA_HEADER;

typedef struct SDATA_PIXEL_ {
  
  int ix; /* relative coordinate in the SDATA segment, in the range [1..nx] */
  int iy; /* relative coordinate in the SDATA segment, in the range [1..ny] */
  int it; /* index of observation (the record number to which the pixel belongs in the SDATA file) */
  time_t unix_time; /* derived from the SDATA_RECORD the pixel belongs to */
  long index_of_day; /* representation of the timestamp in number of days since the Unix Epoch (starts at 0) */
  int cloud_flag;
  int irow; /* coordinate of the pixel in the satellite grid */
  int icol; /* coordinate of the pixel in the satellite grid */
  double latitude;
  double longitude;
  double hgr;  /* height of the ground */
  double hobs; /* height of the observation (same as hgr_km for AERONET, different for satellites) */
  double land_percent; /* between 0. and 100. */
  int num_wavelengths;
  
  int nsurf; /* derived from the SDATA_RECORD the pixel belongs to */
  int ifgas; /* derived from the SDATA_RECORD the pixel belongs to */
  
  int num_hvp; /* number of valid values for hvp */
  double hvp[SDATA_MAX_KVERTM]; /* heights for vertical profile */

  /* spectral properties */
  double wavelengths[SDATA_MAX_NWL];
  int    num_meas_types[SDATA_MAX_NWL];
  int    num_valid_meas[SDATA_MAX_NWL][SDATA_MAX_NIP];
  int    meas_types[SDATA_MAX_NWL][SDATA_MAX_NIP];
  double thetas[SDATA_MAX_NWL]; /* solar zenith angle (aka sza) */
  double thetav[SDATA_MAX_NWL][SDATA_MAX_NIP][SDATA_MAX_KNBVM]; /* view zenith angle (aka vza) */
  double dphi[SDATA_MAX_NWL][SDATA_MAX_NIP][SDATA_MAX_KNBVM]; /* relative azimuth angle, dphi = phis - phiv (aka raa) */
  double groundpar[SDATA_MAX_NWL][SDATA_MAX_NSURF]; /* only the first nsurf indices of MAX_NSURF are valid */
  double gaspar[SDATA_MAX_NWL]; 
  int    ifcov[SDATA_MAX_NWL][SDATA_MAX_NIP]; /* presence (0 - 1) of covariant matrix in input data */
  double cmtrx[SDATA_MAX_NWL][SDATA_MAX_NIP][SDATA_MAX_KNBVM]; /* diagonal of covariant matrix */
  int    ifmp[SDATA_MAX_NWL][SDATA_MAX_NIP]; /* presence (0 - 1) of molecular profile in input data */
  double mprof[SDATA_MAX_NWL][SDATA_MAX_NIP][SDATA_MAX_KVERTM]; /* vertical profile of Rayleigh Backscattering */

  double tod[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* optical thickness        (meas_type = 11) */
  double aod[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* optical thickness        (meas_type = 12) */
  double aaod[SDATA_MAX_NWL][SDATA_MAX_NBVM];   /* optical thickness        (meas_type = 13) */
  double htod[SDATA_MAX_NWL][SDATA_MAX_NBVM];   /* optical thickness        (meas_type = 13) */
  double p11[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p11 phase matrix element (meas_type = 21) */
  double p12[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p12 phase matrix element (meas_type = 22) */
  double p22[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p22 phase matrix element (meas_type = 23) */
  double p33[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p33 phase matrix element (meas_type = 24) */
  double p34[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p34 phase matrix element (meas_type = 25) */
  double p44[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p44 phase matrix element (meas_type = 26) */
  double p11_rel_ang[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p44 phase matrix element (meas_type = 27) */
  double p12_rel[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p44 phase matrix element (meas_type = 28) */

  double LS[SDATA_MAX_NWL][SDATA_MAX_KVERTM];   /* lidar signal             (meas_type = 31) */
  double RL[SDATA_MAX_NWL][SDATA_MAX_KVERTM];   /* Raman lidar signal       (meas_type = 32) */
  double DPAR[SDATA_MAX_NWL][SDATA_MAX_KVERTM]; /* Parallel polarized lidar (meas_type = 33) */
  double DPER[SDATA_MAX_NWL][SDATA_MAX_KVERTM]; /* Cross polarized lidar    (meas_type = 34) */
  double DP[SDATA_MAX_NWL][SDATA_MAX_KVERTM];   /* depolarization ratio     (meas_type = 35) */
  double VEXT[SDATA_MAX_NWL][SDATA_MAX_KVERTM]; /* Vertical extinction      (meas_type = 36) */
  double VBS[SDATA_MAX_NWL][SDATA_MAX_KVERTM];  /* Vertical backscatter     (meas_type = 39) */
  double I[SDATA_MAX_NWL][SDATA_MAX_NBVM];      /* I Stokes Parameter       (meas_type = 41) */
  double Q[SDATA_MAX_NWL][SDATA_MAX_NBVM];      /* Q Stokes Parameter       (meas_type = 42) */
  double U[SDATA_MAX_NWL][SDATA_MAX_NBVM];      /* U Stokes Parameter       (meas_type = 43) */
  double P[SDATA_MAX_NWL][SDATA_MAX_NBVM];      /* Polarization sqrt(Q*Q + U*U) (meas_type = 44) */
  double I_rel_sum[SDATA_MAX_NWL][SDATA_MAX_NBVM];      /* Polarization sqrt(Q*Q + U*U) (meas_type = 45) */
  double P_rel[SDATA_MAX_NWL][SDATA_MAX_NBVM];      /* Polarization sqrt(Q*Q + U*U) (meas_type = 46) */

  double p11_intd[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p11 integrated phase matrix element (meas_type = 51) */
  double p11_intd_cut_off_1[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p11 integrated phase matrix element (meas_type = 52) */
  double p11_intd_cut_off_2[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p11 integrated phase matrix element (meas_type = 53) */
  double p11_intd_cut_off_3[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p11 integrated phase matrix element (meas_type = 54) */
  double p11_intd_cut_off_4[SDATA_MAX_NWL][SDATA_MAX_NBVM];    /* p11 integrated phase matrix element (meas_type = 55) */

} SDATA_PIXEL;

extern const char *sdata_str_meas_types(int meas_type);
extern int sdata_open(const char *file_name, SDATA_HANDLE **ihandle);
extern const char *sdata_get_file_name(const SDATA_HANDLE *handle);
extern int sdata_get_header(const SDATA_HANDLE *handle, SDATA_HEADER *header);
extern void sdata_print_header(FILE *output_stream, const char *label, const SDATA_HEADER *header);
extern void sdata_print_pixel(FILE *output_stream, const char *label, const SDATA_PIXEL *pixel);
extern int sdata_close(SDATA_HANDLE *handle);
extern int sdata_dump_file(FILE *output_stream, const char *sdata_file);
extern void sdata_dump_record(FILE *output_stream, SDATA_RECORD *record);
extern void sdata_dump_pixel(FILE *output_stream, const SDATA_PIXEL *pixel);
extern void sdata_dump_header(FILE *output_stream, const SDATA_HEADER *header);
extern int sdata_load_box(SDATA_HANDLE *handle, const grasp_driver_settings_t *user_settings, grasp_box_t **ibox);
extern void sdata_set_error_stream(FILE *stream);
extern void sdata_set_debug_stream(FILE *stream);
extern FILE *sdata_get_error_stream(void);
extern FILE *sdata_get_debug_stream(void);

#endif /* SDATA_H */
