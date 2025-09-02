/*
 *  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
 *  
 *  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
 */

/* 
 * File:   grasp_settings_t.h
 * Author: fuertes
 *
 * Created on 3 de octubre de 2014, 14:46
 */

#ifndef GRASP_SETTINGS_T_H
#define	GRASP_SETTINGS_T_H

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef WARN_DRY
#warning "__RETRIEVAL_SETTINGS_DEFINITION__ binded"
#endif    
    
typedef struct dls_{
        int IWL                                    ;
        int key                                    ;
        int keyEL                                  ;
        int keyLS                                  ;
        char distname_O[_GBL_FILE_PATH_LEN]        ;
        char distname_N[_GBL_FILE_PATH_LEN]        ;       
        char internal_file_path[_GBL_FILE_PATH_LEN];   
        char external_file_path[_GBL_FILE_PATH_LEN];       
} dls;


typedef struct osh_{
    int IMSC;
    int NG;
    int NN;
    int NF;
} osh;

typedef struct single_pix_contraints_apriori_{
    int   IO  [_KIDIM1][_KIDIM2][_KIDIM3];
    float GSM [_KIDIM1][_KIDIM2][_KIDIM3];
} single_pix_contraints_apriori;

typedef struct single_pix_contraints_smoothness_{
    int   IO  [_KIDIM1][_KIDIM2];
    float GSM [_KIDIM1][_KIDIM2];
} single_pix_contraints_smoothness;

typedef struct single_pixel_smoothness_estimates_and_weights_{
    int IO[_KIDIM1][_KIDIM2];
    float EST[_KIDIM1][_KIDIM2][_KIDIM3];
    float WGT[_KIDIM1][_KIDIM2][_KIDIM3];
} single_pixel_smoothness_estimates_and_weights;

typedef struct multi_pix_constraints_{
    int   IOT  [_KIDIM1][_KIDIM2];
    int   IOX  [_KIDIM1][_KIDIM2];
    int   IOY  [_KIDIM1][_KIDIM2];
    float GSMT [_KIDIM1][_KIDIM2];
    float GSMX [_KIDIM1][_KIDIM2];
    float GSMY [_KIDIM1][_KIDIM2];
    float time_diff_threshold_sec;
}multi_pix_constraints;
           
typedef struct NOISE_param_{
    int meas_rnoise;
    int INOISE;
    float SGMS [_KKNOISE];
    float BIAS [_KKNOISE];
    float BIAS_EQ [_KKNOISE];
    int INN    [_KKNOISE];
    float DNN  [_KKNOISE];
    int NMT    [_KKNOISE];
    int MT     [_KKNOISE][_KIP];
    int NWLP   [_KKNOISE][_KIP];
    int IWLP   [_KKNOISE][_KIP][_KWM];
} NOISE_param;

typedef struct inter_pixel_fit_{
    int INVSING;
}inter_pixel_fit;

//  Edge sizes
typedef struct edges_size_{
         int nx;
         int ny;        
         int nt;         
}edges_size;


//  Settings for gases
typedef struct gases_opt_{
    
    bool igab;  //MH this probably goes in another place
    
    int  nlut_name;
    
    char lut_name [_NMG][_GBL_FILE_PATH_LEN];    //GRASP_MAX_OUTPUT_FUNC
    int  ngas_filters;
    char path_to_luts[_GBL_FILE_PATH_LEN];
    char path_to_filters[_GBL_FILE_PATH_LEN];
    
    char kdist[_GBL_FILE_PATH_LEN];
    int integration_method; // line-by-line: 0; k-distribution: 1
    int filters_meas_type[_N_FILTERS_MAX];
    float filters_spectral_resolution[_N_FILTERS_MAX];
    int nfilters_index_of_wavelength_involved[_KW];
    int filters_index_of_wavelength_involved[_N_FILTERS_MAX][_KW];
    char filters_file[_N_FILTERS_MAX][_KW][_GBL_FILE_PATH_LEN];
    
    int nsubchannels[_KW];
    int ngases_spectral_range[_NMG];
    float spectral_ranges_min[_NMG][_NG_SPEC_RANGE_MAX];
    float spectral_ranges_max[_NMG][_NG_SPEC_RANGE_MAX];
}gases_opt;
//! Functional retrieval approach for
//! SPECTRAL SURF, SPECTRAL COMPLEX REFRACTIVE INDEX
typedef struct functional_retrieval_{
//        ! method = 1 / 2 (default value 1)
//        ! 1 - full set of parameters is retrieved
//        ! 2 - subset of parameters is retrieved then function is applyed for the rest
        int method[_KIDIM1][_KIDIM2];
//        ! function = 1 / 2 (default value 0)
//        ! 1 - constant
//        ! 2 - linear in logarithmic scales
        int function[_KIDIM1][_KIDIM2];
//        ! total number of nodes in output for functional approach
//        ! (default value 0)
        int nn[_KIDIM1][_KIDIM2];
//        ! used if subset of parameters is retrieved:
//        ! indices of parameters involved in retrieval for SURF, RERI, IMRI
//        ! (default value 0)
        int ipar[_KIDIM1][_KIDIM2][_KIDIM3];
} functional_retrieval;

//Settings for transport model
typedef struct transport_settings_{

//        ! approach for phase matrix vertical profiles (1 - column average, 2 - tracer average)
        int flag_av_vprof;
//        ! number of tracers in transport model
        int ntrc;
//        ! tracer types
        char trcs[_KIDIM2][5];
//        ! flag of hydrophilic tracers (0/1)
        int flag_hphi[_KIDIM2];
//        ! density of dry tracers in kg/m^3
        float density[_KIDIM2];
//        ! number of levels in transport model
        int nlev;

} transport_settings;


//  p11_integrated_cut_off
typedef struct p11_integrated_cut_off_{

         int nrmax;
         float rmax [4];
         int ntb[4][_KSD];
         float rmax_tb [4][_KSD];
         bool cutoff_meas_diff;

}p11_integrated_cut_off;


//Settings for chemistry
typedef struct chemistry_opt_{

        char folder[_GBL_FILE_PATH_LEN];
        char soluble[_GBL_FILE_PATH_LEN];
        char species[_KSD][_N_CHEM_MAX][_GBL_FILE_PATH_LEN];
//        int nspecies[_KSD][_GBL_FILE_PATH_LEN];
        int nspecies[_KSD];

} chemistry_opt;


//  Settings for emission
typedef struct emission_opt_{
    
    bool planck;
    bool solar_irradiance;
    float threshold_for_starting_wavelength;
    char folder[_GBL_FILE_PATH_LEN]; //Aerosol kerneles for TIR spectrum
    
}emission_opt;

//  Settings for the vertical profile of the atmosphere
typedef struct atmospheric_vertical_profile_opt_{
    
    bool istdat;
    int stdat; // us_standard/us: 0; mid_latitude_summer/ms: 1; mid_latitude_winter/mw: 2; surbartic_summer/ss: 3; subartic_winter/sw: 4; tropical_atmosphere:tr: 5;
    char vtp[_GBL_FILE_PATH_LEN];
    
}atmospheric_vertical_profile_opt;


typedef struct retr_input_{
        int   KNSING      ;
        int   KNSINGF     ;
    
        int   KL          ;      
        bool  ISTOP       ;          
        bool  IPRI_additional_info;
    
        bool  IPRI_verbose;
        int   IMODE_LUT   ;
        int   NSD         ;
        int   NLYRS[2]    ;
    
        int   NLVLS_GEOM  ;
        int   ipplane     ;
        int   iPOBS       ;
        int   isurf_land[2];
        int   isurf_water ;
        int   Aexp_iwl[2] ;
        int   ndvi_iwl[2] ;
        int   naod_errest_iwl;
        int   aod_errest_iwl[_KW];
        int   nssa_errest_iwl;
        int   ssa_errest_iwl[_KW];
//MEH:
        int   naext_errest_iwl;
        int   aext_errest_iwl[_KW];
        int   nlidar_errest_iwl;
        int   lidar_errest_iwl[_KW];
        float SHIFT       ;
        // sca_ang_norm_p11 - value of scattering angle (in degrees) if provided p11
        // measurements are normalized by p11(sca_ang_norm_p11)
        // by delault sca_ang_norm_p11 = -180.0; valid values 0.0 - 90.0
        float sca_ang_norm_p11;
        // p11_intd_cut_off_? [1.0, 2.5, 4.0, 10.0]
        p11_integrated_cut_off CUTOFF;
        int   NW          ;
        float WAVE [_KW]  ;
        int   IBIN        ;
        float tiny_wvl_models;
        int   IMQ         ;
        int   IPSTOP      ;
        float LM_MIN      ;
        float CCOR_MIN    ;
        float CCOR_MAX    ;
        bool  INPUT       ;
        bool  ITRONC      ;
        bool  BOA_REF     ;
        char  LUT_path[_GBL_FILE_PATH_LEN];
        int   MAXP        ;
        float EPSP        ;
        float EPSQ        ;
        float DL          ;
        int  mol_prof_type;
        int  aer_prof_type; // type of the aerosol vertical profile used
        float  PM_diam[2] ; // diameters at wich PM is calculated
        int   nPM_diam    ; // number of PM diamteres
  
        dls DLSF          ;
        par_number_NDIM NDIM; 
        osh OSHF          ;
        osh OSHD          ;
        single_pix_contraints_apriori SPCA;
        single_pix_contraints_smoothness SPCS;
        single_pixel_smoothness_estimates_and_weights SMS;
        multi_pix_constraints MPCS;        
        NOISE_param NOISE ;   
        inter_pixel_fit IPFP;
        
        float APSING[_KPARS];
        float APSMIN[_KPARS];
        float APSMAX[_KPARS];
        bool  APSERREST[_KPARS];

        float RMIN[_KSD];
        float RMAX[_KSD];
        float RATIO1   [_KSD][_KIDIM3];
        float RADIUS1  [_KSD][_KIDIM3];
        int   IWW_SINGL[_KPARS];
        int   NBIN       [_KSD];
        
        int   KNLN       [_KSD];
  
        gases_opt gases;
        emission_opt emission;
        atmospheric_vertical_profile_opt atmospheric_vertical_profile;
        chemistry_opt chemistry;

        edges_size  edges;
        
        functional_retrieval FRETR;

        bool use_tmodel; //! use transport model
        transport_settings TMSET;
  
        bool indep_par;
        bool flag_plus;
        par_number_NDIM ndim_plus;
        
        char plotting_output_file [_GBL_FILE_PATH_LEN];
        char main_output_file     [_GBL_FILE_PATH_LEN];
        char sdata_sim_file       [_GBL_FILE_PATH_LEN]; 
        
        bool debug_covariance_matrix;
        bool debug_errest_lm;
                
        float eps_err;

        output_segment_products products;            
} retr_input;

typedef struct temporal_data_{
    int NOISE_ISGMS;
    int NOISE_BIAS;
    int NOISE_BIAS_EQ;
    int NOISE_IDNN;
    int NNLYRS;
    int IAPSING;
    int IAPSMIN;
    int IAPSMAX;
    int SPCA_IIO[_KIDIM1][_KIDIM2];
    int SPCA_IGSM[_KIDIM1][_KIDIM2];
    int SPCS_IIO[_KIDIM1];
    int SPCS_IGSM[_KIDIM1]; 
    int SMS_IEST[_KIDIM1][_KIDIM2];
    int SMS_IWGT[_KIDIM1][_KIDIM2];
    
    int MPCS_IIOT[_KIDIM1];
    int MPCS_IGSMT[_KIDIM1];
    int MPCS_IIOX[_KIDIM1];
    int MPCS_IGSMX[_KIDIM1];
    int MPCS_IIOY[_KIDIM1];
    int MPCS_IGSMY[_KIDIM1];    
  
    int IIWW_SINGL;    
    
    int NDIM_npar_type;
    int NDIM_npar_retr;
    
    float TAPSING[_KIDIM1][_KIDIM2][_KPARS];
    int NTAPSING[_KIDIM1][_KIDIM2];
    
    float TAPSMIN[_KIDIM1][_KIDIM2][_KPARS];
    int NTAPSMIN[_KIDIM1][_KIDIM2];
    
    float TAPSMAX[_KIDIM1][_KIDIM2][_KPARS];
    int NTAPSMAX[_KIDIM1][_KIDIM2];
    
    int TIWW_SINGL[_KIDIM1][_KIDIM2][_KPARS];
    int NTIWW_SINGL[_KIDIM1][_KIDIM2];
    
    bool TAPSERREST[_KIDIM1][_KIDIM2][_KPARS];
    int NTAPSERREST[_KIDIM1][_KIDIM2];
    
    int NRMIN;
    int NRMAX;
    int NRATIO1[_KIDIM3];
    
    int nisurf_land;
    
    int nAexp_iwl;
    
    int nndvi_iwl;
    
    int TMPSET_nhyd;

    
    int ngases_spectral_range[_NMG];
    int nfilters_path_to_filters[_KW];
    int nsubchannels;
    int nspecies[_KSD];
    int nfilters_spectral_resolution;

    
    int RERT_nmethod[_KIDIM1];
    int RERT_nfunction[_KIDIM1];

} temporal_data;

/*
 * Settings of grasp settings module
 */
typedef struct grasp_settings_settings_{
    // True if debug information have to be dumped
    char debug[_GBL_FILE_PATH_LEN];
    // True if help information will be printed
    char help[_GBL_FILE_PATH_LEN];
    // if there was warning during the reading process of settings file and this
    // variable is false we will continue the workflow without stop (but informing the user)
    bool strict;   
    // Output stream patter to dump setting loaded in short format
    char short_dump[_GBL_FILE_PATH_LEN];
    // Output stream patter to dump setting loaded in long format
    char long_dump[_GBL_FILE_PATH_LEN];
    // If it is true version code version will be printed in 
    bool version;
}grasp_settings_settings;

typedef struct grasp_global_t_{
    char resources_path[_GBL_FILE_PATH_LEN];     
}grasp_global_t;

typedef struct grasp_settings_{
    retr_input              retrieval;
    input_settings_t        input;
    controller_settings_t   controller;
    output_settings_t       output;
    grasp_settings_settings settings;
    grasp_global_t          global;
    temporal_data           tmp;
}grasp_settings;


#ifdef	__cplusplus
}
#endif

#endif	/* GRASP_SETTINGS_T_H */

