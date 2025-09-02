module mo_grasp_controller_ext_interface

    use mod_par_OS
    use MOD_RT_SOS_SETUP, ONLY : RT_SOS_SET, RT_SOS_CTL, RT_SOS_RES
    use mod_intrpl_linear
    !XH   2*KBF is the maximum length of the array containing surface parameters
    use mod_par_inv, only : KBF, KIDIM3, KVERTM
    use mod_par_DLS, only : KMpar
    use mod_vertical_distr_derived_type
    use MOD_RT_SOS_MS, ONLY : RT_SOS_MS_OUT, RT_SOS_MS, BETAM, GAMMM
    use MOD_RT_SOS_LUT,ONLY : ND, AOD, NMLUT,RTM_NM, RT_SOS_MS_MTRX, SRCH_LUT, RTM1
    use sub_gas_kd, only : NLEVEL_GAS
    use MOD_RT_SOS
    use iso_c_binding

    implicit none

    contains


    subroutine grasp_rt_sos(IW,NG,NN,NF,                                  &
                            iBRDF,iBPDF,iBRM_water,land_percent,          &
                            NQDR,NT1,tetas,NBV,vis,fiv,                   &
                            WAVE,                                         &
                            n_par_land,n_par_land_i,surf_land_par_vect,   &
                            n_par_water,surf_water_par_vect,              &
                            EXT,SSA,                                      &
                            NANG,ANGL,                                    &
                            SD,CEXT,                                      &
                            HOBS_km,HGR_km,HMAX_atm_km,                   &
                            NHVP_retr,HVP_retr_km,                        &
                            H0,sigma_aerosol,                             &
                            ifgas,gaspar,                                 &
                            T_profile,STEMP,                              &
                            laerosol,lsurface,                            &
                            ATMOS_EMIS,SOLAR_EMIS,                        &
                            PF11_I,PF12_I,                                &
                            PF22_I,PF33_I,                                &
                            NLV,HLV,                                      &
                            aerosol_analyt_prof, gas_abs_line,            &
                            N_level,                                      &
                            Hight_level_km,                               &
                            nh,                                           &
                            h_km,                                         &
                            natm, naer, nmol, ngas,                       &
                            norm_DISCRVD,                                 &
                            ISGL,IAER,IVEC_CTL,ISRF,IDWN,IFLX,            &
                            IGQ_F,IGQ_D,IGQ_BRM_FEXP,IGQ_BRM_HSPH,        &
                            IP_VERBOSE,IP_ADDITION,ITRC,ISCA,IVEC_SET,    &
                            IWUT,ILUT,IATM,ICRR,boa_ref,                  &
                            AER_PRF,MOL_PRF,                              &
                            NA,NB,                                        &
                            NLYR,EPS,                                     &
                            thd,                                          &
                            SLout,SQout,SUout,SLPout,                     &
                            salb_out,                                     &
                            UFX,DFX                                       &
                            )   bind(C, name='grasp_rt_sos_')

        integer(kind=C_INT), value, intent(in)                             :: IW
        integer(kind=C_INT), value, intent(in)                             :: NG
        integer(kind=C_INT), value, intent(in)                             :: NN
        integer(kind=C_INT), value, intent(in)                             :: NF

        integer(kind=C_INT), value, intent(in)                             :: iBRDF
        integer(kind=C_INT), value, intent(in)                             :: iBPDF
        integer(kind=C_INT), value, intent(in)                             :: iBRM_water
        real(kind=C_FLOAT),  value, intent(in)                           :: land_percent

        integer(kind=C_INT), value, intent(in)                             ::  NQDR
        integer(kind=C_INT), intent(in)                             ::  NT1(1:2) ! to be checked

        real(kind=C_FLOAT), value, intent(in)                              ::  tetas
        integer(kind=C_INT), value, intent(in)                             ::  NBV
        real(kind=C_FLOAT), dimension(2*NBVM),intent(in)            ::  vis
        real(kind=C_FLOAT), dimension(2*NBVM),intent(in)            ::  fiv

        real(kind=C_FLOAT), value, intent(in)                              ::  WAVE

        integer(kind=C_INT), value, intent(in)                             ::  n_par_land
        integer(kind=C_INT), value, intent(in)                             ::  n_par_land_i
        real(kind=C_FLOAT), dimension(2*KBF), intent(in)            ::  surf_land_par_vect

        integer(kind=C_INT), value, intent(in)                             ::  n_par_water
        real(kind=C_FLOAT), dimension(KBF), intent(in)              ::  surf_water_par_vect

        real(kind=C_FLOAT), dimension(NMM+NMG), intent(inout)       ::  EXT
        real(kind=C_FLOAT), dimension(NMM+NMG), intent(inout)       ::  SSA

        integer(kind=C_INT), value, intent(in)                             ::  NANG
        real(kind=C_FLOAT), dimension(KMpar), intent(in)            ::  ANGL

        real(kind=C_FLOAT), intent(in),dimension(KIDIM3,KSD)        ::  SD
        real(kind=C_FLOAT), intent(in),dimension(KSD)               ::  CEXT

        real(kind=C_FLOAT), value, intent(in)                              ::  HOBS_km
        real(kind=C_FLOAT), value, intent(in)                              ::  HGR_km
        real(kind=C_FLOAT), value, intent(in)                              ::  HMAX_atm_km
        integer(kind=C_INT), value, intent(in)                             ::  NHVP_retr
        real(kind=C_FLOAT), dimension(KVERTM), intent(in)           ::  HVP_retr_km
        real(kind=C_FLOAT), dimension(KVERTM,KSD), intent(in)       ::  H0
        real(kind=C_FLOAT), dimension(KSD), intent(in)              ::  sigma_aerosol
        integer(kind=C_INT), value, intent(in)                             ::  ifgas
        real(kind=C_FLOAT), value, intent(in)                              ::  gaspar

        real(kind=C_FLOAT), dimension(NLEVEL_GAS), intent(in)       ::  T_profile
        real(kind=C_FLOAT), value, intent(in)                              ::  STEMP
        logical(kind=C_BOOL), value, intent(in)                            ::  laerosol
        logical(kind=C_BOOL), value, intent(in)                            ::  lsurface
        logical(kind=C_BOOL), value, intent(in)                            ::  ATMOS_EMIS
        logical(kind=C_BOOL), value, intent(in)                            ::  SOLAR_EMIS


        real(kind=C_FLOAT), dimension(KMpar,KSD), intent(in)        ::  PF11_I
        real(kind=C_FLOAT), dimension(KMpar,KSD), intent(in)        ::  PF12_I
        real(kind=C_FLOAT), dimension(KMpar,KSD), intent(in)        ::  PF22_I
        real(kind=C_FLOAT), dimension(KMpar,KSD), intent(in)        ::  PF33_I

        ! output
        integer(kind=C_INT), value, intent(in)                             ::  NLV
        real(kind=C_FLOAT), dimension(KNT), intent(in)              ::  HLV
        logical(kind=C_BOOL), value, intent(in)                            ::  aerosol_analyt_prof, gas_abs_line
        integer(kind=C_INT), value, intent(in)                             ::  N_level
        real(kind=C_FLOAT), dimension(max(KVERTM,KVERT_WD)), intent(in)::  Hight_level_km

        !MH: Variables of vertical discretization
        integer(kind=C_INT), value, intent(in)                            :: nh
        real(kind=C_FLOAT), dimension(KVERT_WD), intent(in)         :: h_km
        integer(kind=C_INT), value, intent(in)                             :: natm, naer, nmol, ngas
        real(kind=C_FLOAT), dimension(NMM+NMG), intent(in)          :: norm_DISCRVD


        !MH: Variables of RT_SOS_CNTRL
!        logical(kind=C_BOOL), intent(in) :: IJCB
        logical(kind=C_BOOL), value, intent(in)                            :: ISGL
        logical(kind=C_BOOL), value, intent(in)                            :: IAER
        logical(kind=C_BOOL), value, intent(in)                            :: IVEC_CTL
        logical(kind=C_BOOL), value, intent(in)                            :: ISRF
        logical(kind=C_BOOL), value, intent(in)                            :: IDWN
        logical(kind=C_BOOL), value, intent(in)                            :: IFLX


        !MH: Variables of RT_SOS_RESET
        logical(kind=C_BOOL), value, intent(in)                            :: IGQ_F
        logical(kind=C_BOOL), value, intent(in)                            :: IGQ_D
        logical(kind=C_BOOL), value, intent(in)                            :: IGQ_BRM_FEXP
        logical(kind=C_BOOL), value, intent(in)                            :: IGQ_BRM_HSPH

        !MH: Variables of RT_SOS_SET
        logical(kind=C_BOOL), value, intent(in)                            :: IP_VERBOSE
        logical(kind=C_BOOL), value, intent(in)                            :: IP_ADDITION
        logical(kind=C_BOOL), value, intent(in)                            :: ITRC
        logical(kind=C_BOOL), value, intent(in)                            :: ISCA
        logical(kind=C_BOOL), value, intent(in)                            :: IVEC_SET
        logical(kind=C_BOOL), value, intent(in)                            :: IWUT
        logical(kind=C_BOOL), value, intent(in)                            :: ILUT
        logical(kind=C_BOOL), value, intent(in)                            :: IATM
        logical(kind=C_BOOL), value, intent(in)                            :: ICRR
        logical(kind=C_BOOL), value, intent(in)                            :: boa_ref
        integer(kind=C_INT), value, intent(in)                             :: AER_PRF
        integer(kind=C_INT), value, intent(in)                             :: MOL_PRF
        integer(kind=C_INT), value, intent(in)                             :: NA
        integer(kind=C_INT), dimension(KSD), intent(in)             :: NB
        integer(kind=C_INT), dimension( 2 ), intent(in)             :: NLYR
        real(kind=C_FLOAT), value, intent(in)                              :: EPS

        real(kind=C_FLOAT), dimension(2*NBVM), intent(out)           ::  thd
        real(kind=C_FLOAT), dimension(2*NBVM), intent(out)           ::  SLout,SQout,SUout,SLPout
        real(kind=C_FLOAT), intent(out)                             ::  salb_out
        real(kind=C_FLOAT), DIMENSION(KNT),    intent(out)           ::  UFX,DFX

        logical                         ::  ATMOS_EMIS_
        logical                         ::  SOLAR_EMIS_
        logical                         ::  laerosol_
        logical                         ::  lsurface_
        logical                         ::  aerosol_analyt_prof_
        logical                         ::  gas_abs_line_

        type(discret_vertical_distribution) ::  DISCRVD

        ATMOS_EMIS_ = ATMOS_EMIS
        SOLAR_EMIS_ = SOLAR_EMIS
        laerosol_   = laerosol
        lsurface_   = lsurface
        gas_abs_line_ = gas_abs_line
        aerosol_analyt_prof_ = aerosol_analyt_prof

        

        DISCRVD%nh   = nh
        DISCRVD%h_km = h_km
        DISCRVD%natm = natm
        DISCRVD%naer = naer
        DISCRVD%nmol = nmol
        DISCRVD%ngas = ngas
        DISCRVD%norm = norm_DISCRVD

        !MH: Variables of RT_SOS_CNTRL
        RT_SOS_CTL%ISGL = ISGL
        RT_SOS_CTL%IAER = IAER
        RT_SOS_CTL%IVEC = IVEC_CTL
        RT_SOS_CTL%ISRF = ISRF
        RT_SOS_CTL%IDWN = IDWN
        RT_SOS_CTL%IFLX = IFLX


        !MH: Variables of RT_SOS_RESET
        !RT_SOS_RES%IGQ_F = IGQ_F
        !RT_SOS_RES%IGQ_D = IGQ_D
        !RT_SOS_RES%IGQ_BRM_FEXP = IGQ_BRM_FEXP
        !RT_SOS_RES%IGQ_BRM_HSPH = IGQ_BRM_HSPH


        RT_SOS_SET%IP_VERBOSE = IP_VERBOSE
        RT_SOS_SET%IP_ADDITION = IP_ADDITION
        RT_SOS_SET%ITRC = ITRC
        RT_SOS_SET%ISCA = ISCA
        RT_SOS_SET%IVEC = IVEC_SET
        RT_SOS_SET%IWUT = IWUT
        RT_SOS_SET%ILUT = ILUT
        RT_SOS_SET%IATM = IATM
        RT_SOS_SET%ICRR = ICRR
        RT_SOS_SET%boa_ref = boa_ref
        RT_SOS_SET%AER_PRF = AER_PRF
        RT_SOS_SET%MOL_PRF = MOL_PRF
        RT_SOS_SET%NA = NA
        RT_SOS_SET%NB = NB
        RT_SOS_SET%NLYR = NLYR
        RT_SOS_SET%EPS = EPS

!        write(*,*) 'IW = ',IW
!        write(*,*) 'NG = ',NG
!        write(*,*) 'NN = ',NN
!        write(*,*) 'NF = ',NF
!        write(*,*) 'iBRDF = ',iBRDF
!        write(*,*) 'iBPDF = ',iBPDF
!        write(*,*) 'iBRM_water = ',iBRM_water
!        write(*,*) 'land_percent = ',land_percent
!        write(*,*) 'NQDR = ',NQDR
!        write(*,*) 'NT1 = ',NT1
!        write(*,*) 'tetas = ',tetas
!        write(*,*) 'NBV = ',NBV
!        write(*,*) 'vis = ',vis
!        write(*,*) 'fiv = ',fiv
!        write(*,*) 'WAVE = ',WAVE
!        write(*,*) 'n_par_land = ',n_par_land
!        write(*,*) 'n_par_land_i = ',n_par_land_i
!        write(*,*) 'surf_land_par_vect = ',surf_land_par_vect
!        write(*,*) 'n_par_water = ',n_par_water
!        write(*,*) 'surf_water_par_vect = ',surf_water_par_vect
!        write(*,*) 'EXT = ',EXT
!        write(*,*) 'SSA = ',SSA
!        write(*,*) 'NANG = ',NANG
!        write(*,*) 'ANGL = ',ANGL
!        write(*,*) 'SD = ',SD
!        write(*,*) 'CEXT = ',CEXT
!        write(*,*) 'HOBS_km = ',HOBS_km
!        write(*,*) 'HGR_km = ',HGR_km
!        write(*,*) 'HMAX_atm_km = ',HMAX_atm_km
!        write(*,*) 'NHVP_retr = ',NHVP_retr
!        write(*,*) 'HVP_retr_km = ',HVP_retr_km
!        write(*,*) 'H0 = ',H0
!        write(*,*) 'sigma_aerosol = ',sigma_aerosol
!        write(*,*) 'ifgas = ',ifgas
!        write(*,*) 'gaspar = ',gaspar
!        write(*,*) 'T_profile = ',T_profile
!        write(*,*) 'STEMP = ',STEMP
!        write(*,*) 'laerosol = ',laerosol
!        write(*,*) 'lsurface = ',lsurface
!        write(*,*) 'ATMOS_EMIS = ',ATMOS_EMIS
!        write(*,*) 'SOLAR_EMIS = ',SOLAR_EMIS
!        write(*,*) 'PF11_I = ',PF11_I
!        write(*,*) 'PF12_I = ',PF12_I
!        write(*,*) 'PF22_I = ',PF22_I
!        write(*,*) 'PF33_I = ',PF33_I
!        write(*,*) 'NLV = ',NLV
!        write(*,*) 'HLV = ',HLV
!        write(*,*) 'aerosol_analyt_prof = ',aerosol_analyt_prof
!        write(*,*) 'gas_abs_line = ',gas_abs_line
!        write(*,*) 'N_level = ',N_level
!        write(*,*) 'Hight_level_km = ',Hight_level_km
!        write(*,*) 'nh = ',nh
!        write(*,*) 'h_km = ',h_km
!        write(*,*) 'natm = ',natm
!        write(*,*) 'naer = ',naer
!        write(*,*) 'nmol = ',nmol
!        write(*,*) 'ngas = ',ngas
!        write(*,*) 'norm_DISCRVD = ',norm_DISCRVD
!        write(*,*) 'ISGL = ',ISGL
!        write(*,*) 'IAER = ',IAER
!        write(*,*) 'IVEC_CTL = ',IVEC_CTL
!        write(*,*) 'ISRF = ',ISRF
!        write(*,*) 'IDWN = ',IDWN
!        write(*,*) 'IGQ_F = ',IGQ_F
!        write(*,*) 'IGQ_D = ',IGQ_D
!        write(*,*) 'IGQ_BRM_FEXP = ',IGQ_BRM_FEXP
!        write(*,*) 'IGQ_BRM_HSPH = ',IGQ_BRM_HSPH
!        write(*,*) 'IP_VERBOSE = ',IP_VERBOSE
!        write(*,*) 'IP_ADDITION = ',IP_ADDITION
!        write(*,*) 'ITRC = ',ITRC
!        write(*,*) 'ISCA = ',ISCA
!        write(*,*) 'IVEC_SET = ',IVEC_SET
!        write(*,*) 'IWUT = ',IWUT
!        write(*,*) 'ILUT = ',ILUT
!        write(*,*) 'IATM = ',IATM
!        write(*,*) 'ICRR = ',ICRR
!        write(*,*) 'boa_ref = ',boa_ref
!        write(*,*) 'AER_PRF = ',AER_PRF
!        write(*,*) 'MOL_PRF = ',MOL_PRF
!        write(*,*) 'NA = ',NA
!        write(*,*) 'NB = ',NB
!        write(*,*) 'NLYR = ',NLYR
!        write(*,*) 'EPS = ',EPS


        call RT_discret_vertical_distr_with_gas(aerosol_analyt_prof_,                   &
                              gas_abs_line_, HGR_km, HMAX_atm_km,                       &
                              N_level, Hight_level_km(1:N_level),                      &
                              H0, sigma_aerosol,                                       &
                              ifgas, gaspar,                                           &
                              DISCRVD )

        call radiative_transfer_SOS (   ATMOS_EMIS_, SOLAR_EMIS_,                     & ! IN
                                        IW, NG, NN, NF,                               &
                                        iBRDF, iBPDF, iBRM_water, land_percent,       &
                                        NQDR, NT1,                                    &
                                        tetas, NBV, vis, fiv,                         &
                                        WAVE,                                         &
                                        n_par_land, n_par_land_i, surf_land_par_vect, &
                                        n_par_water, surf_water_par_vect,             &
                                        EXT, SSA,                                     &
                                        NANG, ANGL,                                   &
                                        SD,CEXT,                                      &
                                        HOBS_km, HGR_km, HMAX_atm_km, DISCRVD,        &
                                        T_profile, STEMP,                             &
                                        laerosol_, lsurface_,                         &
                                        PF11_I, PF12_I, PF22_I, PF33_I,               &

                                        thd, SLout, SQout, SUout, SLPout,             & ! OUT
                                        salb_out, NLV, HLV, UFX, DFX                  &
                                      )

    end subroutine grasp_rt_sos

end module mo_grasp_controller_ext_interface
