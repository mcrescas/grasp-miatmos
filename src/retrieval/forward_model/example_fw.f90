#include "../constants_set/mod_globals.inc"

program main

    use mod_par_OS
    !XH   2*KBF is the maximum length of the array containing surface parameters
    use mod_par_inv, only : KBF, KIDIM3, KVERTM
    use mod_par_DLS, only : KMpar
    use sub_gas_kd, only : NLEVEL_GAS
    use MOD_RT_SOS

    use mo_grasp_controller_ext_interface, only: grasp_rt_sos
    
  implicit none

  !type(compute_optchar_config) :: INSET ! In structure to forward (config)
  !type(single_scattering_optchar) :: FW_OUT ! Out structure from forward (results))
  integer(kind=C_INT) :: status = 0
  integer(kind=C_INT) :: i
  
  ! We define specific example:
  ! code here


  ! Calling forward model, simple direct interface
    integer(kind=C_INT)                             :: IW = 1
    integer(kind=C_INT)                             :: NG = 15
    integer(kind=C_INT)                             :: NN = 5
    integer(kind=C_INT)                             :: NF = 4

    integer(kind=C_INT)                             :: iBRDF = 1
    integer(kind=C_INT)                             :: iBPDF = 0
    integer(kind=C_INT)                             :: iBRM_water = 0
    real(kind=C_FLOAT)                              :: land_percent = 0.0

    integer(kind=C_INT)                             ::  NQDR = 15
    integer(kind=C_INT)                             ::  NT1(1:2) ! to be checked

    real(kind=C_FLOAT)                              ::  tetas = 24.1095009
    integer(kind=C_INT)                             ::  NBV = 1 ! It could be more!
    real(kind=C_FLOAT),dimension(2*NBVM)            ::  vis = (58.8371010)
    real(kind=C_FLOAT),dimension(2*NBVM)            ::  fiv = (107.692001)

    real(kind=C_FLOAT)                              ::  WAVE = 0.442999989

    integer(kind=C_INT)                             ::  n_par_land = 4
    integer(kind=C_INT)                             ::  n_par_land_i = 3
    real(kind=C_FLOAT), dimension(2*KBF)            ::  surf_land_par_vect

    integer(kind=C_INT)                             ::  n_par_water = 3
    real(kind=C_FLOAT), dimension(KBF)              ::  surf_water_par_vect

    real(kind=C_FLOAT),dimension(NMM+NMG)           ::  EXT = 0 ! explanation missing. initilization below.
    real(kind=C_FLOAT),dimension(NMM+NMG)           ::  SSA = 0 ! explanation missing. initilization below.

    integer(kind=C_INT)                             ::  NANG = 35 !181
    real(kind=C_FLOAT),dimension(KMpar)             ::  ANGL = 0.

    real(kind=C_FLOAT),dimension(KIDIM3,KSD)        ::  SD = 0.0 ! initialization below
    real(kind=C_FLOAT),dimension(KSD)               ::  CEXT = 0.0 ! I do not know what's is this!

    real(kind=C_FLOAT)                              ::  HOBS_km = 705.000061
    real(kind=C_FLOAT)                              ::  HGR_km  = 5.70000038E-02
    real(kind=C_FLOAT)                              ::  HMAX_atm_km = 40.000
    integer(kind=C_INT)                             ::  NHVP_retr = 1       !MH: number of altitudes for retrieved vertical profile
    real(kind=C_FLOAT),dimension(KVERTM)            ::  HVP_retr_km = 0.0   !MH: altitudes for retrieved vertical profile
    real(kind=C_FLOAT),dimension(KVERTM,KSD)        ::  H0                  !AL: contains parameters of aerosol vertical distribution or distribution itself
    real(kind=C_FLOAT),dimension(KSD)               ::  sigma_aerosol = 0.0 !MH: For aerosol vertical profile shape
    integer(kind=C_INT)                             ::  ifgas  = 1          !MH: Selects if gas absorption is incuded or not
    real(kind=C_FLOAT)                              ::  gaspar = 0.0        !MH: Gas absorption value
    real(kind=C_FLOAT),dimension(NLEVEL_GAS)        ::  T_profile = 0.0     !MH: Temperature profile for each level. Only necesary if emission is accounted
    real(kind=C_FLOAT)                              ::  STEMP = 0.0         !MH: Surface temperature for each level. Only necesary if emission is accounted
    logical(kind=C_BOOL)                            ::  laerosol = .true.
    logical(kind=C_BOOL)                            ::  lsurface = .true.  !MH I think that this is compulsory true to make RT calculations with multiple scattering
    logical(kind=C_BOOL)                            ::  ATMOS_EMIS = .false. !MH: To account for emission
    logical(kind=C_BOOL)                            ::  SOLAR_EMIS = .false. !MH: If emission is account to acount for solar contribution


    real(kind=C_FLOAT),dimension(KMpar,KSD)         ::  PF11_I
    real(kind=C_FLOAT),dimension(KMpar,KSD)         ::  PF12_I
    real(kind=C_FLOAT),dimension(KMpar,KSD)         ::  PF22_I
    real(kind=C_FLOAT),dimension(KMpar,KSD)         ::  PF33_I
     
    ! output
    integer(kind=C_INT)                             ::  NLV
    real(kind=C_FLOAT),DIMENSION(KNT)                 ::  HLV
    real(kind=C_FLOAT),dimension(2*NBVM)              ::  thd
    real(kind=C_FLOAT),dimension(2*NBVM)              ::  SLout,SQout,SUout,SLPout
    real(kind=C_FLOAT)                                ::  salb_out
    real(kind=C_FLOAT),DIMENSION(KNT)                 ::  UFX,DFX
    logical(kind=C_BOOL)                             ::  aerosol_analyt_prof, gas_abs_line
    integer(kind=C_INT)                             ::  N_level
    real(kind=C_FLOAT),DIMENSION(max(KVERTM,KVERT_WD))::  Hight_level_km

    integer(kind=C_INT)                             :: nh
    real(kind=C_FLOAT), dimension(KVERT_WD)           :: h_km
    integer(kind=C_INT)                             :: natm, naer, nmol, ngas
    real(kind=C_FLOAT), dimension(NMM+NMG)            :: norm_DISCRVD

    !MH: Variables of RT_SOS_CNTRL
    logical(kind=C_BOOL) :: ISGL = .false.
    logical(kind=C_BOOL) :: IAER = .true.
    logical(kind=C_BOOL) :: IVEC_CTL = .true.
    logical(kind=C_BOOL) :: ISRF = .true.
    logical(kind=C_BOOL) :: IDWN = .false.
    logical(kind=C_BOOL) :: IFLX = .false.


    !MH: Variables of RT_SOS_RESET
    logical(kind=C_BOOL) :: IGQ_F = .false. !??
    logical(kind=C_BOOL) :: IGQ_D  = .false.
    logical(kind=C_BOOL) :: IGQ_BRM_FEXP = .false.
    logical(kind=C_BOOL) :: IGQ_BRM_HSPH = .false. !??

    !ICMB ??
    !IJCB ??

    !MH: Variables of RT_SOS_RESET
    logical(kind=C_BOOL) :: IP_VERBOSE = .true.
    logical(kind=C_BOOL) :: IP_ADDITION = .false.
    logical(kind=C_BOOL) :: ITRC = .true.
    logical(kind=C_BOOL) :: ISCA = .false.
    logical(kind=C_BOOL) :: IVEC_SET = .true.
    logical(kind=C_BOOL) :: IWUT = .false.
    logical(kind=C_BOOL) :: ILUT = .false.
    logical(kind=C_BOOL) :: IATM = .false.
    logical(kind=C_BOOL) :: ICRR = .false.
    logical(kind=C_BOOL) :: boa_ref = .false.
    integer(kind=C_INT)  :: AER_PRF = 0
    integer(kind=C_INT)  :: MOL_PRF = 0
    integer(kind=C_INT)  :: NA = 1
    integer(kind=C_INT), dimension(KSD) :: NB
    integer(kind=C_INT), dimension( 2 ) :: NLYR
    real(kind=C_FLOAT)    :: EPS = 5.00000024E-04

    surf_land_par_vect(1:8) = (/9.99999940E-02,9.99999940E-02,9.99999940E-02,2.09999990,0.00000000,0.00000000,0.00000000  ,0.00000000/)

    surf_water_par_vect(1:4) = (/9.99999978E-03,0.899999976,9.99999978E-03,0.00000000/)

    NB(1) = 5
    NB(2) = 0
    NLYR(1) =50
    NLYR(2) =59

    ! TODO: call Rayleigh calculation (harcoded 0.240760148)
    EXT(1:4) = (/0.267643005, 0.234841362,  0.00000000, 0.00000000/)
    SSA(1:4) = (/0.959613979, 1.00000000, 0.00000000, 0.00000000/)



    !MH Initialize vertcial profile for values corresponding to sunphotometer example
    H0(:,1) = 0.0
    H0(1,1) = 2000.00024
    H0(:,2) = 0.0


    NT1(1:2) = (/51,51/)
    SD(1:22,1) = (/9.99999978E-03,9.99999978E-03,9.99999978E-03,9.99999978E-03,9.99999978E-03,   0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000/)

    SD(1:22,2) = 0.0

    !MH Initialize phase matrix

    ANGL(1:35) = (/0.00000000,2.59999990,3.46000004,5.19999981,6.92999983,8.65999985,10.3900003,12.1199999,15.5699997,17.2999992,21.6100006,25.8999996,30.1900005,42.9399986,59.5699997,67.6500015,75.5199966,83.1200027,97.1800003,108.940002,117.050003,120.000000,125.000000,130.000000,135.000000,140.000000,147.500000,154.300003,165.000000,167.699997,170.000000,172.500000,175.000000,177.500000,180.000000/)

PF11_I(1:35,1) = (/47.6394730, 25.1858082, 21.0667286, 16.3418388, 13.7347383, 12.0047817, 10.7165308, 9.67327309, 8.00905132, 7.30169582, 5.79060984, 4.58816767, 3.63343048, 1.84604311,0.825588465,0.582695842,0.427834779,0.327849090,0.218963295,0.171959251,0.154124215,0.149824232,0.144890025,0.142721251,0.142901465,0.145416722,0.152995154,0.163561746,0.185849935,0.191917032,0.199015573,0.209076643,0.220770255,0.230718821,0.236013860/)

PF12_I(1:35,1)= (/0.00000000,4.05577524E-03,3.56888003E-03,4.45997663E-04,-3.75478878E-03,-8.89717322E-03,-1.48161594E-02,-2.11354494E-02,-3.43322791E-02,-4.10582796E-02,-5.72876073E-02,-7.23126084E-02,-8.56272876E-02,-0.110879928,-0.114230826,-0.107783630,-9.89508554E-02,-8.90221745E-02,-6.87668994E-02,-5.10714054E-02,-3.84344980E-02,-3.36857624E-02,-2.54153088E-02,-1.68152470E-02,-7.77174300E-03,1.78835704E-03,1.65786669E-02,2.79730763E-02,2.74789724E-02,2.28554234E-02,1.83241535E-02,1.30986366E-02,7.72413658E-03,2.53695110E-03,0.00000000/)

    PF22_I(1:35,1) = (/47.6055565, 25.1522102, 21.0333500, 16.3089886, 13.7024345, 11.9730206, 10.6853151, 9.64260006, 7.97946024, 7.27264547, 5.76288128, 4.56169367, 3.60811734, 1.82349455,0.805077851,0.562627792,0.408007652,0.308231860,0.200011909,0.154078543,0.137298390,0.133439690,0.129311681,0.128011063,0.129109219,0.132569984,0.141492903,0.152809039,0.173424914,0.178088129,0.183630720,0.191610798,0.200902894,0.208809704,0.213459611/)
    PF33_I(1:35,1) = (/47.6055603,25.1505642,21.0305271,16.3032360,13.6939840,11.9619455,10.6717644,9.62655735,7.95972013,7.25124741,5.73679686,4.52994490,3.59252405,1.80358493,0.734181881,0.483873099,0.322244704,0.215602994,9.43241045E-02,3.60917896E-02,8.64461996E-03,3.57155310E-04,-1.21472636E-02,-2.32224539E-02,-3.35234515E-02,-4.37801108E-02,-6.10400252E-02,-8.14868510E-02,-0.125870556,-0.137827367,-0.149198964,-0.164411560,-0.183045253,-0.203353629,-0.213638574/)

    !MH Initialize Vertical discretization
        nh = 100
        natm = 2
        naer = 1
        nmol = 1
        ngas = 0
        norm_DISCRVD(1:4) = (/2.50131297,7.94906139,0.00000000,0.00000000/)
        aerosol_analyt_prof = .TRUE.
        gas_abs_line = .FALSE.
        N_level = 1
        Hight_level_km = 0.0
        Hight_level_km(1) = 1.000
        H0 = 0.00
        H0(1,1) = 2500.00024
        sigma_aerosol = 0.0


  
        call grasp_rt_sos(IW,NG,NN,NF,                                  &
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
                                )
                                  
                                  
  ! We print some values that we will use as reference
  if ( status .eq. 0 ) then

    ! expected = ...
!    write(*,'(a,20f12.4)')  'WL:       ',INSET%wl(1:INSET%nwl)
!    write(*,'(a,20es12.4)') 'AOD:      ',PHMX%cext(1:INSET%nwl,0)
!    write(*,'(a,20es12.4)') 'expected: ',0.391, 0.239, 0.075, 0.054
    write(*,'(a,20f12.4)') 'Expected: 0.134284660'
    write(*,*) 'Radiance: ',SLout(1:NBV)
  endif
  write (*,*) 'Exit with status code ', status

end program main
