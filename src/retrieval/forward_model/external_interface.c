//
// Created by Juan Carlos Antuña-Sánchez on 27/7/21.
//

#include "external_interface.h"

void grasp_rt_sos(radiative_transfer_args *args, radiative_transfer_result *result){
    grasp_rt_sos_(args->IW,args->NG,args->NN,args->NF,
                  args->iBRDF,args->iBPDF,args->iBRM_water,args->land_percent,
                  args->NQDR,args->NT1,args->tetas,args->NBV,args->vis,args->fiv,args->WAVE,
                  args->n_par_land,args->n_par_land_i,args->surf_land_par_vect,
                  args->n_par_water,args->surf_water_par_vect,
                  args->EXT,args->SSA,
                  args->NANG,args->ANGL,
                  args->SD,args->CEXT,
                  args->HOBS_km,args->HGR_km,args->HMAX_atm_km,
                  args->NHVP_retr,args->HVP_retr_km,
                  args->H0,args->sigma_aerosol,
                  args->ifgas,args->gaspar,
                  args->T_profile,args->STEMP,
                  args->laerosol,args->lsurface,
                  args->ATMOS_EMIS,args->SOLAR_EMIS,
                  args->PF11_I,args->PF12_I,
                  args->PF22_I,args->PF33_I,
                  args->NLV,args->HLV,
                  args->aerosol_analyt_prof,args->gas_abs_line,
                  args->N_level,
                  args->Hight_level_km,
                  args->nh,
                  args->h_km,
                  args->natm,args-> naer,args-> nmol,args-> ngas,
                  args->norm_DISCRVD,
                  args->ISGL,args->IAER,args->IVEC_CTL,args->ISRF,args->IDWN,args->IFLX,
                  args->IGQ_F,args->IGQ_D,args->IGQ_BRM_FEXP,args->IGQ_BRM_HSPH,
                  args->IP_VERBOSE,args->IP_ADDITION,args->ITRC,args->ISCA,args->IVEC_SET,
                  args->IWUT,args->ILUT,args->IATM,args->ICRR,args->boa_ref,
                  args->AER_PRF,args->MOL_PRF,
                  args->NA,args->NB,
                  args->NLYR,args->EPS,
                  result->thd,
                  result->SLout,result->SQout,result->SUout,result->SLPout,
                  &result->salb_out,
                  result->UFX,result->DFX);
}
