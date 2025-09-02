//
// Created by Juan Carlos Antuña-Sánchez on 10/8/21.
//
#include <stdbool.h>
#include "mod_par_OS.inc"
#include "mod_par_DLS.inc"
#include "mod_par_inv.inc"

#ifndef EXTERNAL_INTERFACE_H
#define EXTERNAL_INTERFACE_H
#define MAX(x,y) ((x>y)?x:y)

#define _NLEVEL_GAS _KVERT_WD

typedef struct radiative_transfer_args_ {
    int IW;  /**< @brief IW: Index of the position of the wavelength  */
    int NG;  /**< @brief NG - Number of gaussian quadrature points */
    int NN;  /**< @brief NN - Number of quadrature expansions */
    int NF;  /**< @brief NF - Maximum number of Fourier terms */
    int iBRDF;  /**< @brief iBRDF - Establish if surface directional scattering is taken into account */
    int iBPDF;  /**< @brief iBPDF - Establish if surface polarization is taken into */
    int iBRM_water;  /**< @brief iBRM_water - Type of water surface model */
    float land_percent;  /**< @brief land_percent - Percent of surface corresponding to land model */
    int NQDR;  /**< @brief NQDR - Number of angular quadrature points */
    int NT1[2];  /**< @brief NT1 - Number of layers */
    float tetas;  /**< @brief tetas - Solar Zenith Angle */
    int NBV;  /**< @brief NBV - Number of observations */
    float vis[2 * _NBVM];  /**< @brief vis - Observation Zenith angle (degrees) */
    float fiv[2 * _NBVM];  /**< @brief fiv - Observation Azimuth angle (degrees) */
    float WAVE;  /**< @brief WAVE - Wavelength in micrometers */
    int n_par_land;  /**< @brief n_par_land - total number of surface parameters (BRDF+BPDF) for land */
    int n_par_land_i;  /**< @brief n_par_land_i - number of BRDF surface parameters for land */
    float surf_land_par_vect[2 * _KBF];  /**< @brief surf_land_par_vect - Parameters describing land surface */
    int n_par_water;  /**< @brief n_par_water - number of BRDF surface parameters for water */
    float surf_water_par_vect[_KBF];  /**< @brief surf_water_par_vect - Parameters describing water surface */
    float EXT[_NMM + _NMG];  /**< @brief EXT - Extinction for each */
    float SSA[_NMM + _NMG];  /**< @brief SSA - Single scattering albedo for each atmospheric component */
    int NANG;  /**< @brief NANG - number of scattering angles of phase matrix NANG */
    float ANGL[_KMpar];  /**< @brief ANGL - Scattering angles in which is defined PF11_I */
    float SD[_KSD][_KIDIM3];  /**< @brief SD - RT kernels approach: Size distribution. Related to LUT generation */
    float CEXT[_KSD];  /**< @brief CEXT - RT kernels approach: extinction. Related to LUT generation */
    float HOBS_km;  /**< @brief HOBS_km - Height of observation in km */
    float HGR_km;  /**< @brief HGR_km - Station altitude in km */
    float HMAX_atm_km;  /**< @brief HMAX_atm_km - Atmosphere maximum altitude */
    int NHVP_retr;  /**< @brief NHVP_retr - Number of altitudes for retrieved vertical profile */
    float HVP_retr_km[_KVERTM];  /**< @brief HVP_retr_km - Altitudes for retrieved vertical profile */
    float H0[_KSD][_KVERTM];  /**< @brief H0 - Contains parameters of aerosol vertical distribution or distribution itself */
    float sigma_aerosol[_KSD];  /**< @brief sigma_aerosol - Parameters for aerosol vertical profile shape */
    int ifgas;  /**< @brief ifgas - Selects if gas absorption is incuded or not (old gas model) */
    float gaspar;  /**< @brief gaspar - Gas absorption value  (old gas model) */
    float T_profile[_NLEVEL_GAS];  /**< @brief T_profile - Temperature profile for each level Only necessary if emission is accounted */
    float STEMP;  /**< @brief STEMP - Surface temperature. Only necesary if emission is accounted */
    bool laerosol;  /**< @brief laerosol - Selects if aerosol is present in the atmosphere */
    bool lsurface;  /**< @brief lsurface - Selects if surface is present in the calculations */
    bool ATMOS_EMIS;  /**< @brief ATMOS_EMIS - To account for thermal emission */
    bool SOLAR_EMIS;  /**< @brief SOLAR_EMIS - If emission is accounted it is used  to account for solar contribution */
    float PF11_I[_KSD][_KMpar];  /**< @brief PF11_I - Aerosol phase matrix */
    float PF12_I[_KSD][_KMpar];  /**< @brief PF12_I - Aerosol phase matrix */
    float PF22_I[_KSD][_KMpar];  /**< @brief PF22_I - Aerosol phase matrix */
    float PF33_I[_KSD][_KMpar];  /**< @brief PF33_I - Aerosol phase matrix */
    int NLV;  /**< @brief NLV - Number of levels */
    float HLV[_KNT];  /**< @brief HLV - Height of levels */
    bool aerosol_analyt_prof;  /**< @brief aerosol_analyt_prof - Marks if the the exact aerosol profiles is known or an analytic shape is going to be taken */
    bool gas_abs_line;  /**< @brief gas_abs_line - Marks if there is gas absorption included (new model) */
    int N_level;  /**< @brief N_level - Number of levels */
    float Hight_level_km[MAX(_KVERTM, _KVERT_WD)];  /**< @brief Hight_level_km - Aerosol profile height */
    int nh;  /**< @brief nh - Number of layers in the aerosol profile */
    float h_km[_KVERT_WD];  /**< @brief h_km - Height of layers in the aerosol profile */
    int natm;  /**< @brief natm - Number of total atmospheric components */
    int naer;  /**< @brief naer - Number of total aerosol modes */
    int nmol;  /**< @brief nmol - Number of total molecular components */
    int ngas;  /**< @brief ngas - Number of total gaseous components */
    float norm_DISCRVD[_NMM + _NMG];  /**< @brief norm_DISCRVD - Norm of atmospheric vertical profile */
    bool ISGL;  /**< @brief ISGL - To restrict the code to single scattering calculations */
    bool IAER;  /**< @brief IAER - Selects if aerosol must be considered */
    bool IVEC_CTL;  /**< @brief IVEC_CTL - To restrict the calculation to scalar equations */
    bool ISRF;  /**< @brief ISRF - Selects if surface must be accounted */
    bool IDWN;  /**< @brief IDWN - Marks if the calculations are upwards or downwards */
    bool IFLX;  /**< @brief IFLX - Marks is flux calculations is going to be done (not possible with the actual approach of the stand alone RT) */
    bool IGQ_F;  /**< @brief IGQ_F - Quadrature calculation in forward mode */
    bool IGQ_D;  /**< @brief IGQ_D -  Quadrature calculation for dericatives in inversion mode */
    bool IGQ_BRM_FEXP;  /**< @brief IGQ_BRM_FEXP - Surface quadrature calculations for Fourier components */
    bool IGQ_BRM_HSPH;  /**< @brief IGQ_BRM_HSPH - High precision surface quadrature calculations */
    bool IP_VERBOSE;  /**< @brief IP_VERBOSE - Related with output format */
    bool IP_ADDITION;  /**< @brief IP_ADDITION - Related with output additional information */
    bool ITRC;  /**< @brief ITRC - Marks if Phase matrix truncation is going to be done */
    bool ISCA;  /**< @brief ISCA - Output in scattering or meridian plane */
    bool IVEC_SET;  /**< @brief IVEC_SET - Scalar or vector calculations */
    bool IWUT;  /**< @brief IWUT - Lut generation */
    bool ILUT;  /**< @brief ILUT - Use LUTs already generated */
    bool IATM;  /**< @brief IATM - Choose between bihemispherical or directional hemispherical reflectance of atmosphere */
    bool ICRR;  /**< @brief ICRR - Atmospheric correction */
    bool boa_ref;  /**< @brief boa_ref - Selectes between different surface models */
    int AER_PRF;  /**< @brief AER_PRF - Aerosol profile shape */
    int MOL_PRF;  /**< @brief MOL_PRF - Molecular profile shape */
    int NA;  /**< @brief NA - RT kernels approach: number of aerosol modes */
    int NB[_KSD];  /**< @brief NB - RT kernels appraoch: number of aerosol */
    int NLYR[2];  /**< @brief NLYR - Number of layers */
    float EPS;  /**< @brief EPS - Related with precision of some  numerical expansions */
} radiative_transfer_args;

typedef struct radiative_transfer_result_ {
    float thd[2*_NBVM];  /**< @brief thd - Measurement azimuth angle (degrees) */
    float SLout[2*_NBVM];  /**< @brief SLout - Calculated radiance in the selected viewing geometry */
    float SQout[2*_NBVM];  /**< @brief SQout - Calculated radiance in the selected viewing geometry */
    float SUout[2*_NBVM];  /**< @brief SUout - Calculated radiance in the selected viewing geometry*/
    float SLPout[2*_NBVM];  /**< @brief SLPout - Calculated radiance in the selected viewing geometry */
    float salb_out;  /**< @brief salb_out - Surface albedo */
    float UFX[_KNT];  /**< @brief UFX - Fluxes */
    float DFX[_KNT];  /**< @brief DFX - Fluxes */
} radiative_transfer_result;

/*
 * Actual function is implemented in fortran, which is why we have the full list of parameters here as well
 */
extern void grasp_rt_sos_(int IW, int NG, int NN, int NF,
                          int iBRDF, int iBPDF, int iBRM_water, float land_percent,
                          int NQDR, int NT1[2], float tetas, int NBV, float vis[2*_NBVM], float fiv[2*_NBVM],
                          float WAVE,
                          int n_par_land, int n_par_land_i, float surf_land_par_vect[2*_KBF],
                          int n_par_water, float surf_water_par_vect[_KBF],
                          float EXT[_NMM+_NMG], float SSA[_NMM+_NMG],
                          int NANG, float ANGL[_KMpar],
                          float SD[_KSD][_KIDIM3], float CEXT[_KSD],
                          float HOBS_km, float HGR_km, float HMAX_atm_km,
                          int NHVP_retr, float HVP_retr_km[_KVERTM],
                          float H0[_KSD][_KVERTM], float sigma_aerosol[_KSD],
                          int ifgas, float gaspar,
                          float T_profile[_NLEVEL_GAS], float STEMP,
                          bool laerosol, bool lsurface,
                          bool ATMOS_EMIS, bool SOLAR_EMIS,
                          float PF11_I[_KSD][_KMpar], float PF12_I[_KSD][_KMpar],
                          float PF22_I[_KSD][_KMpar], float PF33_I[_KSD][_KMpar],
                          int NLV, float HLV[_KNT],
                          bool  aerosol_analyt_prof, bool gas_abs_line,
                          int N_level,
                          float Hight_level_km[MAX(_KVERTM,_KVERT_WD)],
                          int nh,
                          float h_km[_KVERT_WD],
                          int natm, int naer, int nmol, int ngas,
                          float norm_DISCRVD[_NMM+_NMG],
                          bool ISGL, bool IAER, bool IVEC_CTL, bool ISRF, bool IDWN, bool IFLX,
                          bool IGQ_F, bool IGQ_D, bool IGQ_BRM_FEXP, bool IGQ_BRM_HSPH,
                          bool IP_VERBOSE, bool IP_ADDITION, bool ITRC, bool ISCA, bool IVEC_SET,
                          bool IWUT, bool ILUT, bool IATM, bool ICRR, bool boa_ref,
                          int AER_PRF, int MOL_PRF,
                          int NA, int NB[_KSD],
                          int NLYR[2], float EPS,
                          float thd[2*_NBVM],
                          float SLout[2*_NBVM], float SQout[2*_NBVM], float SUout[2*_NBVM], float SLPout[2*_NBVM],
                          float *salb_out,
                          float UFX[_KNT], float DFX[_KNT]);

void grasp_rt_sos(radiative_transfer_args *args, radiative_transfer_result *result);

#endif //EXTERNAL_INTERFACE_H
