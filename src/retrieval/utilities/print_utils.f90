! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! file contains :

! subroutine write_rad_transf_input
! subroutine print_classic_plot

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      SUBROUTINE write_rad_transf_input ( IW, IMSC, NG, NN, NF,                           &
                                          iBRF_land, iBPF_land, iBRM_water, land_percent, &
                                          NQDR, NT1,                                      &
                                          sza, NBV, vis, fiv,                             &
                                          WAVE,                                           &
                                          n_par_land, n_par_land_i, surf_land_par_vect,   &
                                          n_par_water, surf_water_par_vect,               &
                                          EXT, SSA,                                       &
                                          NANG, ANGL,                                     &
                                          SD, CEXT,                                       &
                                          HOBS_km, HGR_km, HMAX_atm_km, DISCRVD,          &
                                          laerosol, lsurface,                             &
                                          PF11_I, PF12_I, PF22_I, PF33_I,                 &
                                          external_file_path                              &
                                        )


      !USE MOD_RT_SOS, only : rad_sp, radiative_transfer_SOS
      !USE MOD_RT_SOS_SETUP, ONLY : RT_SOS_CTL, RT_SOS_SET
      use mod_par_DLS,   only : KMpar
      use mod_par_inv,   only : KBF, KIDIM3
      use mod_par_OS,    only : NMG, KSD, NBVM, NMM, KNT, NG0T, KVERT_WD
      use mod_vertical_distr_derived_type
      USE MOD_RT_SOS_SETUP, ONLY : RT_SOS_CTL, RT_SOS_SET, RT_SOS_RES
!XH   modules related to gas absorption
      !use mod_abs_kd

      use mod_vertical_distr_derived_type

	    implicit none
!	-----------------------------------------------------------------------------------
! IN:
      integer,                           intent(in) ::  NQDR,NANG
      real,dimension(KMpar),             intent(in) ::  ANGL
      real,dimension(KMpar,KSD),         intent(in) ::  PF11_I, PF12_I, PF22_I, PF33_I
      real,dimension(NMM+NMG),           intent(in) ::  EXT,SSA
      integer,                           intent(in) ::  IW, IMSC, NG, NN, NF
      integer,                           intent(in) ::  NBV
      real,                              intent(in) ::  WAVE
      integer,                           intent(in) ::  iBRF_land, iBPF_land, iBRM_water
      integer,                           intent(in) ::  n_par_land, n_par_land_i, n_par_water
      real,                              intent(in) ::  land_percent
      real, dimension(2*KBF),            intent(in) ::  surf_land_par_vect
      real, dimension(KBF),              intent(in) ::  surf_water_par_vect
      real,                              intent(in) ::  sza
      real,dimension(2*NBVM),            intent(in) ::  vis,fiv
      real,                              intent(in) ::  HOBS_km, HGR_km, HMAX_atm_km
      INTEGER,                           intent(in) ::  NT1(1:2)
      logical,                           intent(in) ::  laerosol, lsurface
      !integer,               optional,   intent(in) ::  NLV
      !real,DIMENSION(KNT),   optional,   intent(in) ::  HLV
      type(discret_vertical_distribution), intent(in) :: DISCRVD

      real,dimension(KIDIM3,KSD), intent(in)  ::  SD
      real,dimension(KSD),        intent(in)  ::  CEXT
      character (*),              intent(in)  ::  external_file_path
!	-----------------------------------------------------------------------------------
!	-----------------------------------------------------------------------------------
      integer :: natm, naer, nmol, ngas
      integer        ::  IA, IV, ISD, iu_check_OS
      !integer, save  ::  counter=1
      integer  ::  counter
      character(len=15) :: naer_str
!	-----------------------------------------------------------------------------------
!	-----------------------------------------------------------------------------------
      counter = 1
!c***************************************************************
!      DIMENSION  FWAVE(122), WFBEAM(122)
!OD:  DO NOT DELETE DATA FWAVE and WFBEAM ( data can be used)
!c***************************************************************
!      DATA FWAVE /0.30, 0.31, 0.31, 0.32, 0.32, 0.33,
!     & 0.33, 0.34, 0.34, 0.35, 0.35, 0.36, 0.37, 0.38, 0.39,
!     & 0.40, 0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48,
!     & 0.49, 0.50, 0.51, 0.52, 0.53, 0.54, 0.55, 0.57, 0.59,
!     & 0.61, 0.63, 0.66, 0.67, 0.69, 0.71, 0.72, 0.72, 0.74,
!     & 0.75, 0.76, 0.76, 0.77, 0.78, 0.80, 0.82, 0.82, 0.83,
!     & 0.84, 0.86, 0.88, 0.91, 0.92, 0.93, 0.93, 0.94, 0.95,
!     & 0.97, 0.98, 0.99, 1.04, 1.07, 1.10, 1.12, 1.13, 1.15,
!     & 1.16, 1.17, 1.20, 1.24, 1.27, 1.29, 1.32, 1.35, 1.40,
!     & 1.44, 1.46, 1.48, 1.50, 1.52, 1.54, 1.56, 1.58, 1.59,
!     & 1.61, 1.63, 1.65, 1.68, 1.74, 1.80, 1.86, 1.92, 1.96,
!     & 1.99, 2.01, 2.04, 2.07, 2.10, 2.15, 2.20, 2.27, 2.36,
!     & 2.45, 2.50, 2.60, 2.70, 2.80, 2.90, 3.00, 3.10, 3.20,
!     & 3.30, 3.40, 3.50, 3.60, 3.70, 3.80, 3.90, 4.00/ 

!      DATA WFBEAM /535.90,	558.30,	622.00,	692.70,
!     & 715.10, 832.90, 961.90, 931.90, 900.60, 911.30, 
!     & 975.50, 975.90, 1119.90,	1103.80, 1033.80, 1479.10,	
!     & 1701.30,	1740.40, 1587.20, 1837.00, 2005.00,	2043.00,
!     & 1987.00,	2027.00, 1896.00, 1909.00, 1927.00, 1831.00,
!     & 1891.00,	1898.00, 1892.00, 1840.00, 1768.00,	1728.00,
!     & 1658.00,	1524.00, 1531.00, 1420.00, 1399.00,	1374.00,
!     & 1373.00,	1298.00, 1269.00, 1245.00, 1223.00,	1205.00,
!     & 1183.00,	1148.00, 1091.00, 1062.00, 1038.00,	1022.00,
!     & 998.70, 947.20, 893.20, 868.20, 829.70, 830.30, 
!     & 814.00, 786.90, 768.30, 767.00, 757.60, 688.10,
!     & 640.70, 606.20, 585.90, 570.20, 564.10, 544.20,
!     & 533.40, 501.60, 477.50, 442.70, 440.00, 416.80,
!     & 391.40, 358.90, 327.50, 317.50, 307.30, 300.40, 
!     & 292.80, 275.50, 272.10, 259.30, 246.90, 244.00,	
!     & 243.50, 234.80, 220.00, 190.80, 171.10, 144.50,
!     & 135.70, 123.00, 123.80, 113.00, 108.50, 97.50,
!     & 92.40, 82.40, 74.60,	68.30, 63.80, 49.50, 48.50,
!     & 38.60, 36.60, 32.00, 28.10, 24.80, 22.10, 19.60,
!     & 17.50, 15.70, 14.10, 12.70, 11.50, 10.40, 9.50, 8.60/
!c***************************************************************
      natm = DISCRVD%natm
      naer = DISCRVD%naer
      nmol = DISCRVD%nmol
      ngas = DISCRVD%ngas

!       IF(counter .le. 10) THEN
!        IF(IW .eq. 1 .and. counter .eq. 1) THEN
! Print RT Inputs for 10 last RT calls
      !IF(counter .eq. 1 .or. (counter .ge. 10 .and. MOD(counter,10) .eq. 0)) THEN
      iu_check_OS = 22

      IF(counter .eq. 1) THEN
        open(iu_check_OS,file=trim(external_file_path)// &
                       'check_OS_input.dat',status='replace')
      ELSE
        open(iu_check_OS,file=trim(external_file_path)// &
         'check_OS_input.dat',status='old',position='append')
      ENDIF
      counter=counter+1
      WRITE(iu_check_OS,*) '**********************  counter=',counter-1,'  IMSC =',IMSC

      WRITE(iu_check_OS,*) IW, WAVE,"   - IW, WAVE"
      WRITE(iu_check_OS,*) NG, NN, NF, NQDR,"   - NG, NN, NF, NQDR"
      WRITE(iu_check_OS,*) iBRF_land, iBPF_land, iBRM_water, land_percent, &
                     "   - iBRF_land, iBPF_land, iBRM_water, land_percent"
      WRITE(iu_check_OS,*) NQDR
            WRITE(iu_check_OS,*) sza,' solar_zenith_angl_THETAS'
      WRITE(iu_check_OS,*) NBV,  "   NBV"
      DO IV=1,NBV
        WRITE(iu_check_OS,'(2f11.5,i5,a23)') vis(IV), fiv(IV), IV,     &
        '   - vis(IV), fiv(IV), IV'
      ENDDO

      WRITE(iu_check_OS,*)  n_par_land, n_par_land_i,'   - n_par_land, n_par_land_i'
      WRITE(iu_check_OS,*)  surf_land_par_vect(1:n_par_land),'   - surf_land_par_vect(1:npar)'
      WRITE(iu_check_OS,*)  n_par_water,"   - n_par_water"
      WRITE(iu_check_OS,*)  surf_water_par_vect(1:n_par_water),'   - surf_water_par_vect'

      WRITE(iu_check_OS,'(a)') 'EXT(1:natm)'
      WRITE(iu_check_OS,'(10e16.7)') EXT(1:natm)
      WRITE(iu_check_OS,'(a)') 'SSA(1:natm)'
      WRITE(iu_check_OS,'(10e16.7)') SSA(1:natm)
      !WRITE(iu_check_OS,'(a)') 'CEXT(1:natm)'
      !WRITE(iu_check_OS,'(10e16.7)') CEXT(1:natm)

      DO ISD=1,naer
        DO IA=1,NANG
          WRITE(iu_check_OS,'(f9.3,4e18.8,a)') ANGL(IA),          &
          PF11_I(IA,ISD), PF12_I(IA,ISD), PF22_I(IA,ISD), PF33_I(IA,ISD), &
          '   - ANGL(IA), PF11(IA,ISD), PF12(IA,ISD), PF22(IA,ISD), PF33(IA,ISD)'
        ENDDO ! IA
       ENDDO ! ISD

      WRITE(iu_check_OS,'(4(i0,2x),a)') natm, naer, nmol, ngas, &
                                       '   - natm, naer, nmol, ngas'

      WRITE(iu_check_OS,'(a)') 'iv, h_km(iv), VD(iv,1:natm)'
      DO iv=1,DISCRVD%nh
        WRITE(iu_check_OS,'(x,i0,2x,f15.6,10es15.6)') &
                              iv,DISCRVD%h_km(iv),DISCRVD%val(iv,1:natm)
      ENDDO ! iv

       WRITE(iu_check_OS,'(2(x,i0),a)') NT1(1:2),'   - NT1(1:2)'
       WRITE(iu_check_OS,'(3es11.4,a)') HOBS_km,HGR_km,HMAX_atm_km,  &
                         '   - HOBS_km, HGR_km, HMAX_atm_km'
       WRITE(iu_check_OS,'(2l2,a)') laerosol,lsurface,'   - laerosol, lsurface'

       WRITE(iu_check_OS,'(/,a)') 'RT_SOS_SET:'
       WRITE(iu_check_OS,'(7l2,3x,a)') RT_SOS_SET%ITRC, RT_SOS_SET%ISCA, RT_SOS_SET%IVEC, &
                                       RT_SOS_SET%IWUT, RT_SOS_SET%ILUT, RT_SOS_SET%IATM, &
                                       RT_SOS_SET%ICRR,'- ITRC, ISCA, IVEC, IWUT, ILUT, IATM, ICRR'

       WRITE(naer_str,'(i0)') naer
       WRITE(iu_check_OS,'(x,3(i0,x),'//trim(adjustl(naer_str))//'(i0,x),2(i0,x),2x,a)') &
                 RT_SOS_SET%AER_PRF, RT_SOS_SET%MOL_PRF, RT_SOS_SET%NA, &
                 RT_SOS_SET%NB(1:naer), RT_SOS_SET%NLYR(1:2),           &
                 '- AER_PRF, MOL_PRF, NA, NB(1:naer), NLYR(1:2)'
       WRITE(iu_check_OS,'(es11.4,3x,a)') RT_SOS_SET%EPS,'- EPS'
       WRITE(iu_check_OS,'(x,a,3x,a)') trim(RT_SOS_SET%INTL_PATH),'- NTL_PATH'
       WRITE(iu_check_OS,'(x,a,3x,a)') trim(RT_SOS_SET%EXTL_PATH),'- EXTL_PATH'

       WRITE(iu_check_OS,'(/,a)') 'RT_SOS_CTL:'
       WRITE(iu_check_OS,'(7l2,3x,a)') RT_SOS_CTL%IJCB, RT_SOS_CTL%ISGL, RT_SOS_CTL%IAER, &
                                       RT_SOS_CTL%IVEC, RT_SOS_CTL%ISRF, RT_SOS_CTL%IDWN, &
                                       RT_SOS_CTL%IFLX,'- IJCB, ISGL, IAER, IVEC, ISRF, IDWN, IFLX'

       WRITE(iu_check_OS,'(/,a)') 'RT_SOS_RES:'
       WRITE(iu_check_OS,'(4l2,3x,a)') RT_SOS_RES%IGQ_F, RT_SOS_RES%IGQ_D, RT_SOS_RES%IGQ_BRM_FEXP, &
                                       RT_SOS_RES%IGQ_BRM_HSPH,'- IGQ_F, IGQ_D, IGQ_BRM_FEXP, IGQ_BRM_HSPH'

       close(iu_check_OS)

!      ENDIF ! counter .le. 10
	   
      END SUBROUTINE write_rad_transf_input
	  
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! This routine finds and returns the indices i1, i2, i3 of a retrieved
! parameter (identified by the index ipar in the GOUT_retrieval_par structure)
! after unpacking into the usual multidimensional array.
subroutine get_i1_i2_i3_from_ipar(RIN, ipar, i1, i2, i3)
  use mod_retr_settings_derived_type
  use mod_par_inv, only : KPARS
  implicit none

  ! input and output
  type(retr_input_settings),   intent(in)  :: RIN
  integer,                     intent(in)  :: ipar
  integer,                     intent(out) :: i1, i2, i3
  ! local variables
  integer                         :: n1, n2, n3, i3offs
  integer, dimension(KPARS), save :: i1_cache = 0, i2_cache = 0, i3_cache = 0

  ! Take indices from cache, if present, and return immediately.
  if (i1_cache(ipar) .ne. 0) then
    i1 = i1_cache(ipar)
    i2 = i2_cache(ipar)
    i3 = i3_cache(ipar)
  end if

  ! Otherwise we find the corresponding indices,
  ! save them in the cache, and return them.

  n1 = RIN%NDIM%n1
  do i1 = 1, n1
    n2 = RIN%NDIM%n2(i1)
    do i2=1,n2
      n3 = RIN%NDIM%n3(i2,i1)
      i3offs = RIN%NDIM%ISTARSING(i2,i1) - 1
      do i3 = 1, n3
        !par(i3,i2,:) = GOUT_retrieval_par%pixel(:)%par(i3offs + i3)
        if (ipar .eq. i3offs + i3) then
          i1_cache(ipar) = i1
          i2_cache(ipar) = i2
          i3_cache(ipar) = i3
          return
        end if
      enddo
    enddo
  enddo

  ! We should not arrive here, since we expect to find the indices for ipar.
  stop 'stop in get_i1_i2_i3_from_ipar: invalid index ipar'
end subroutine

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
subroutine par_type_to_string(RIN, par_type, par_type_str)
  use mod_retr_settings_derived_type
  implicit none

  ! input and output
  type(retr_input_settings),   intent(in)  :: RIN
  integer,                     intent(in)  :: par_type
  character(LEN=50),           intent(out) :: par_type_str

  select case (par_type)
    case(par_type_SD_TB)
      par_type_str = 'SD_TB'
    case(par_type_SD_LB)
      par_type_str = 'SD_LB'
    case(par_type_SD_LN)
      par_type_str = 'SD_LN'
    case(par_type_RERI_spect)
      par_type_str = 'RERI_spect'
    case(par_type_RERI_const)
      par_type_str = 'RERI_const'
    case(par_type_CXRI_nmix)
      par_type_str = 'CXRI_nmix'
    case(par_type_CXRI_chem)
      par_type_str = 'CXRI_chem'
    case(par_type_IMRI_spect)
      par_type_str = 'IMRI_spect'
    case(par_type_IMRI_const)
      par_type_str = 'IMRI_const'
    case(par_type_SHD_fsph)
      par_type_str = 'SHD_fsph'
    case(par_type_SHD_distr)
      par_type_str = 'SHD_distr'
    case(par_type_AVP_par_height)
      par_type_str = 'AVP_par_height'
    case(par_type_AVP_prof)
      par_type_str = 'AVP_prof'
    case(par_type_Cv)
      par_type_str = 'Cv'
    case(par_type_CL)
      par_type_str = 'CL'
    case(par_type_AVP_par_std)
      par_type_str = 'AVP_par_std'
    case(par_type_SURF1_land_Ross_Li_BRDF)
      par_type_str = 'SURF1_land_Ross_Li_BRDF'
    case(par_type_SURF1_land_RPV_BRDF)
      par_type_str = 'SURF1_land_RPV_BRDF'
    case(par_type_SURF1_land_Litvinov)
      par_type_str = 'SURF1_land_Litvinov'
    case(par_type_SURF1_land_Litvinov_fast)
      par_type_str = 'SURF1_land_Litvinov_fast'
    case(par_type_SURF2_land_Maignan_Breon)
      par_type_str = 'SURF2_land_Maignan_Breon'
    case(par_type_SURF2_land_Litvinov)
      par_type_str = 'SURF2_land_Litvinov'
    case(par_type_SURF_water_Cox_Munk_iso)
      par_type_str = 'SURF_water_Cox_Munk_iso'
    case(par_type_SURF_water_Cox_Munk_ani)
      par_type_str = 'SURF_water_Cox_Munk_ani'
    case(par_type_SURF_water_Litvinov)
      par_type_str = 'SURF_water_Litvinov'
    case(par_type_SURF_water_Cox_Munk_iso_2par)
      par_type_str = 'SURF_water_Cox_Munk_iso_2par'
    case default
      write(par_type_str,'(i15)') par_type
  end select

end subroutine

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
subroutine i3_to_label(RIN, par_type, i3, i3label)
  use mod_retr_settings_derived_type
  implicit none

  ! input and output
  type(retr_input_settings), intent(in)  :: RIN
  integer,                   intent(in)  :: par_type, i3
  character(LEN=50),         intent(out) :: i3label

  select case(par_type)
    case(par_type_RERI_beg:par_type_RERI_end-1,             &
         par_type_IMRI_beg:par_type_IMRI_end-1,             &
         par_type_SURF1_land_beg:par_type_SURF1_land_end-1, &
         par_type_SURF2_land_beg:par_type_SURF2_land_end-1, &
         par_type_SURF_water_beg:par_type_SURF_water_end-1)
      ! Use the wavelength in nm as the label.
      write(i3label, '(i15)') nint(1000*RIN%wave(i3))
    case default
      ! By default, just use the index as the label.
      write(i3label, '(i15)') i3
  end select
end subroutine

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
subroutine ipar_to_label(RIN, ipar, strpar)
  use mod_retr_settings_derived_type

  implicit none

  ! input and output
  type(retr_input_settings), intent(in)  :: RIN
  integer,                   intent(in)  :: ipar
  CHARACTER(LEN=50),         intent(out) :: strpar
  ! local variables
  integer                                :: i1, i2, i3
  integer                                :: par_type
  character(LEN=50)                      :: i1label, i2label, i3label

  call get_i1_i2_i3_from_ipar(RIN, ipar, i1, i2, i3)

  ! The first component of the label is the parameter type
  par_type = RIN%NDIM%par_type(i1)
  call par_type_to_string(RIN, par_type, i1label)

  ! The second comoent of the label is a number used for grouping.
  write(i2label, '(i15)') i2

  ! The third component of the label refers to some attribute,
  ! usually (but not always) the wavelength.
  ! In the case of the aerosol size distributions this is the radius.
  call i3_to_label(RIN, par_type, i3, i3label)

  write(strpar, '(a)') &
    trim(adjustl(i1label)) // '_' // &
    trim(adjustl(i2label)) // '_' // &
    trim(adjustl(i3label))

end subroutine

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! development needed for sunphotometer+lidar retrievals

      subroutine print_classic_plot(RIN,segment_meas,GOUT)

      use iso_fortran_env, only : output_unit, error_unit
      use mod_par_inv,      only : KIDIM3, KIDIM2, KIMAGE, KSHAPE
      USE mod_par_OS,       only : KSD
      use mod_retr_settings_derived_type
      use mod_retr_general_output_derived_type
      use mod_time_utils
      use mod_sdata_derived_type
      use mod_globals, only: GBL_FILE_PATH_LEN
      use mod_c_utils
      
      implicit none  
!	------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------
! IN :
      type(retr_input_settings),   intent(in) :: RIN
      type(output_segment_general),intent(in) :: GOUT
      type(segment_data),          intent(in) :: segment_meas
!	------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------
! LOCAL :	  	  
      integer                             :: IDIM1, IDIM2, IDIM3, KN,  &
                                             IW, ipix, npixels, NDIM3, &
                                             I, i1, i2
      character(LEN=20)                   :: KNC, INOISEC
      character(LEN=150)                  :: CFMT 
      character(LEN=12),dimension(KIMAGE) :: pdate
      character(LEN=12),dimension(KIMAGE) :: ptime
      real,dimension(KIMAGE)              :: pjday
      real,dimension(KSD,KIMAGE)          :: h01
      real,dimension(KSHAPE,KSD,KIMAGE)   :: sph
      integer                             :: id_wr_TL_output	

      integer                             :: err_code
      character(LEN=255)                  :: err_msg
      integer                             :: par_type, isurf1, isurf2
      character(LEN=50)                   :: strpar
      character(LEN=GBL_FILE_PATH_LEN)    :: plotting_output_file
!	------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------
      isurf1 = 0
      isurf2 = 0

      KN = GOUT%retrieval%information%ngrid(0)
      npixels = segment_meas%npixels

      call cstring2fstring(RIN%plotting_output_file, plotting_output_file)
      open( newunit=id_wr_TL_output,file=trim(plotting_output_file), &
                                                  status='replace')
      do IDIM1=1,RIN%NDIM%n1
      par_type = RIN%NDIM%par_type(IDIM1)
      if(par_type .gt. par_type_SHD_beg  .and. par_type .lt. par_type_SHD_end) then      
        do ipix=1,npixels
          do IDIM2=1,RIN%NDIM%n2(IDIM1) 
              NDIM3=RIN%NDIM%n3(IDIM2,IDIM1)
              i1=RIN%NDIM%ISTARSING(IDIM2,IDIM1)
              i2=i1+NDIM3-1
              sph(1:NDIM3,IDIM2,ipix) =   & 
			                    GOUT%retrieval%par%pixel(ipix)%par(i1:i2)
          enddo ! IDIM2
        enddo ! ipix
      elseif(par_type .gt. par_type_AVP_beg  .and. par_type .lt. par_type_AVP_end) then
        do ipix=1,npixels
          do IDIM2=1,RIN%NDIM%n2(IDIM1) 
              NDIM3=RIN%NDIM%n3(IDIM2,IDIM1)
              i1=RIN%NDIM%ISTARSING(IDIM2,IDIM1)
              i2=i1+NDIM3-1
              h01(1:NDIM3,ipix) = GOUT%retrieval%par%pixel(ipix)%par(i1:i2)	
          enddo ! IDIM2
        enddo ! ipix
      elseif(par_type .gt. par_type_SURF1_land_beg  .and. par_type .lt. par_type_SURF1_land_end) then
              isurf1=RIN%NDIM%ISTARSING(1,IDIM1)
      elseif(par_type .gt. par_type_SURF2_land_beg  .and. par_type .lt. par_type_SURF2_land_end) then
              isurf2=RIN%NDIM%ISTARSING(1,IDIM1)
      endif ! par_type .gt. par_type_SHD_beg  .and.
      enddo ! IDIM1
      
      write( id_wr_TL_output,'(500i5)') RIN%DLSF%IWL,RIN%NDIM%n1,  &
               (RIN%NDIM%n2(IDIM1),(RIN%NDIM%n3(IDIM2,IDIM1),      & 
				       IDIM2=1,RIN%NDIM%n2(IDIM1)),IDIM1=1,RIN%NDIM%n1)

! ***  Print Retrieval Results for Visualization     **************	  
      write(KNC,*) RIN%nw
      CFMT = '(8i5,'//trim(adjustl(KNC))//'f15.6,a)'
      write( id_wr_TL_output,trim(CFMT))      &
      npixels,RIN%KNSING,RIN%KNSINGF,RIN%NSD,KN,isurf1,isurf2,RIN%nw,RIN%wave(1:RIN%nw), &
      ' - npixels, KNSING, KNSINGF, NSD, NBIN, isurf1, isurf2, NW, wl(1:NW)'

      write( id_wr_TL_output, '(3A)', advance='no')  &
      ' pixel yyyy-mm-dd  hh:mm:ss     Day_number         ', &
      '  lon            lat       Residual      '

      do iw = 1,RIN%nw
        write(id_wr_TL_output, '(A,i0,A)', advance='no') ' AOT_', nint(1000*RIN%wave(iw)), '       '
      enddo
      do iw = 1,RIN%nw
        write(id_wr_TL_output, '(A,i0,A)', advance='no') ' SSA_', nint(1000*RIN%wave(iw)), '       '
      enddo
      do iw = 1,RIN%nw
        write(id_wr_TL_output, '(A,i0,A)', advance='no') ' SALB_', nint(1000*RIN%wave(iw)), '      '
      enddo
      do iw = 1,RIN%nw
        write(id_wr_TL_output, '(A,i0,A)', advance='no') ' RRE_', nint(1000*RIN%wave(iw)), '       '
      enddo
      do iw = 1,RIN%nw
        write(id_wr_TL_output, '(A,i0,A)', advance='no') ' RIM_', nint(1000*RIN%wave(iw)), '       '
      enddo

      write( id_wr_TL_output, '(A)', advance='no') ' %sphere        height      '

      do I=1, RIN%KNSING
        call ipar_to_label(RIN, I, strpar)
        write( id_wr_TL_output, '(A35)', advance='no') adjustr(strpar(1:35))
      enddo

      do iw = 1,RIN%nw
        write(id_wr_TL_output, '(A,i0)', advance='no') '        sza_', nint(1000*RIN%wave(iw))
      enddo
      write(id_wr_TL_output, '(A)', advance='no') '   MASL'
      write(id_wr_TL_output, '(A)') '      x         y'

      call yyyymmdd_hhmmss ( segment_meas,pdate,ptime )
      call day_number_curr_year ( segment_meas,pjday )

  LOOP_PIXEL: DO ipix=1,npixels

      WRITE( id_wr_TL_output,'(i4,2x,a10,2x,a8,f15.7,1000e15.6)', advance='no') &
         ipix,                                             & !pixel
         trim(pdate(ipix)),                                & !yyyy-mm-dd
         trim(ptime(ipix)),                                & !hh:mm:ss
         pjday(ipix),                                      & !Day_number
         segment_meas%pixels(ipix)%x,                      & !lon
         segment_meas%pixels(ipix)%y,                      & !lat
         GOUT%retrieval%res%pixel(ipix)%resr(1),           & !Residual
         (GOUT%aerosol%opt%pixel(ipix)%wl(iw)%extt, iw=1,RIN%nw), & !AOT
         (GOUT%aerosol%opt%pixel(ipix)%wl(iw)%ssat, iw=1,RIN%nw), & !SSA
         (GOUT%surface%pixel(ipix)%wl(iw)%dhr,  iw=1,RIN%nw), & !SALB
         ((GOUT%aerosol%rind%pixel(ipix)%wl(iw)%mreal(I),IW=1,RIN%nw),I=1,RIN%NSD),   & !RRE
         ((GOUT%aerosol%rind%pixel(ipix)%wl(iw)%mimag(I),IW=1,RIN%nw),I=1,RIN%NSD),   & !RIM
         sph(1,1,ipix)*100., & !%sphere
         h01(1,ipix)           !height

      WRITE( id_wr_TL_output,'(1000e35.6)', advance='no') &
         (GOUT%retrieval%par%pixel(ipix)%par(I),I=1,RIN%KNSING)  !parameters i

      WRITE( id_wr_TL_output,'(1000e15.6)', advance='no') &
         (segment_meas%pixels(ipix)%meas(iw)%sza,IW=1,RIN%nw),        & !sza
         segment_meas%pixels(ipix)%MASL                                 !masl

      WRITE( id_wr_TL_output,'(2i10)' ) &
         segment_meas%pixels(ipix)%irow, & !x
         segment_meas%pixels(ipix)%icol    !y

   ENDDO LOOP_PIXEL  ! ipix

! ***  Print Residual              *********************************
 
      WRITE(id_wr_TL_output,*)
	   
      IF(RIN%IPFP%INVSING .ge. 2) THEN
  LOOP_PIXEL1: DO ipix=1,npixels
      WRITE(INOISEC,*) RIN%NOISE%INOISE
      CFMT = '(f10.5,'//trim(adjustl(INOISEC))//  &
	                                  '(i5,a,e14.5,f15.5),i5,4x,a,i5,a,2x,i5,a)'
      WRITE(id_wr_TL_output,TRIM(CFMT)) GOUT%retrieval%res%pixel(ipix)%res, &
      (I,':',GOUT%retrieval%res%pixel(ipix)%resa(I), &
             GOUT%retrieval%res%pixel(ipix)%resr(I), &
      I=1,RIN%NOISE%INOISE),ipix,'  - Residual after LP=',  &
      GOUT%retrieval%res%niter,' - iteration', ipix,' - PIXEL'

  ENDDO LOOP_PIXEL1  ! IMAGE
      WRITE(id_wr_TL_output,*)
      CFMT = '(f10.5,'//trim(adjustl(INOISEC))//'(i5,a,e14.5,f15.5),9x,a,i5,a)'
      WRITE(id_wr_TL_output,TRIM(CFMT))  GOUT%retrieval%res%rest,     &
      (I,':',GOUT%retrieval%res%resat(I),GOUT%retrieval%res%resrt(I), &
      I=1,RIN%NOISE%INOISE),  &
      '  - Residual after LP=',GOUT%retrieval%res%niter,' - iteration TOTAL IMAGE'

      ELSEIF(RIN%IPFP%INVSING .ge. 0 .and. RIN%IPFP%INVSING .le. 1) THEN
  LOOP_PIXEL2: DO ipix=1,npixels
      WRITE(INOISEC,*) RIN%NOISE%INOISE
      CFMT = '(f10.5,'//trim(adjustl(INOISEC))//  &
	                                  '(i5,a,e14.5,f15.5),i5,a)'
      WRITE(id_wr_TL_output,TRIM(CFMT)) GOUT%retrieval%res%pixel(ipix)%res,              &
      (I,':',GOUT%retrieval%res%pixel(ipix)%resa(I),GOUT%retrieval%res%pixel(ipix)%resr(I),  &
      I=1,RIN%NOISE%INOISE),ipix,' - PIXEL'
  ENDDO LOOP_PIXEL2  ! IMAGE
      ENDIF ! RIN%IPFP%INVSING .ge. 2

      ! Size Distribution
      call print_size_distribution(id_wr_TL_output,RIN,segment_meas,GOUT)

      
      close( id_wr_TL_output, iostat=err_code, iomsg=err_msg)
	  
      if (err_code /= 0) then
         write(error_unit, '("print_classic_plot: attempt to close ",A," failed: ",A)') &
         trim(plotting_output_file), trim(err_msg)
      else
         write(output_unit, '("print_classic_plot: ",A," done ")') &
         trim(plotting_output_file)
      end if
	  
      flush(output_unit)
      flush(error_unit)
      return
      end subroutine print_classic_plot

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine yyyymmdd_hhmmss ( segment_meas,pdate,ptime )

      use mod_par_inv, only : KIMAGE
      use mod_sdata_derived_type
      
      implicit none
! ----------------------------------------------------------------	   
      type(segment_data),                 intent(in)   ::  segment_meas
      character(len=12),dimension(KIMAGE),intent(out)  ::  pdate,ptime
! ----------------------------------------------------------------	  
      integer  ::  ipix
! ----------------------------------------------------------------	  
! http://www.cplusplus.com/reference/ctime/strftime/

      pdate(:) = ' '
      ptime(:) = ' '
      do ipix=1,segment_meas%npixels
         call convert_time_to_string(segment_meas%pixels(ipix)%t, "%F", pdate(ipix))
         call convert_time_to_string(segment_meas%pixels(ipix)%t, "%X", ptime(ipix))
      enddo ! ipix

      return
      end subroutine yyyymmdd_hhmmss
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine day_number_curr_year ( segment_meas,pjday )

      use mod_par_inv, only : KIMAGE
      use mod_sdata_derived_type
      
      implicit none
! ----------------------------------------------------------------	   
      type(segment_data),      intent(in)   ::  segment_meas
      real,dimension(KIMAGE),  intent(out)  ::  pjday
! ----------------------------------------------------------------	  
      integer           ::  ipix
	    real              ::  XX,ZZ
      character(LEN=2)  ::  HH,MM,SS
      character(LEN=3)  ::  DD
! ----------------------------------------------------------------
! http://www.cplusplus.com/reference/ctime/strftime/

! calculate day number in current year (like proveded in Aeronet retrieval product)
        pjday(:) = 0.0
        do ipix=1,segment_meas%npixels
          call convert_time_to_string(segment_meas%pixels(ipix)%t, "%j", DD)
          read(DD,*) XX
          pjday(ipix) = XX
          HH = ' '
          MM = ' '
          SS = ' '      
          call convert_time_to_string(segment_meas%pixels(ipix)%t, "%H", HH)
          call convert_time_to_string(segment_meas%pixels(ipix)%t, "%M", MM)
          call convert_time_to_string(segment_meas%pixels(ipix)%t, "%S", SS)
          !write(*,*) 'HH=',HH,'  MM=',MM,'  SS=',SS
          ZZ = 0.
          read(HH,*) XX
          ZZ=ZZ+XX*3600.
          read(MM,*) XX
          ZZ=ZZ+XX*60.
          read(SS,*) XX
          ZZ=ZZ+XX
          ZZ=ZZ/(24.*3600.)		
          pjday(ipix)=pjday(ipix)+ZZ
           !write(*,*) 'ipix=','  pjday(ipix)=',pjday(ipix)
        enddo ! ipix

      return
      end subroutine day_number_curr_year

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss 
    subroutine print_ssa_errest_components (  iu_main_output, RIN, NSD1, npixels, RES, &
                                              ERR_ext,  ERR_sca,  ERR_mix, &
                                              BIAS_ext, BIAS_sca, r )

      use mod_par_inv, only : KIMAGE, KW
      use mod_retr_settings_derived_type

      implicit none
! ......................................................................
      !character(len=12),dimension(KIMAGE) :: pdate, ptime
      type(retr_input_settings), intent(in) :: RIN
      integer, intent(in) :: NSD1, npixels, iu_main_output
      real, dimension(KW,NSD1,KIMAGE), intent(in) :: ERR_ext,  ERR_sca,  ERR_mix
      real, dimension(KW,NSD1,KIMAGE), intent(in) :: BIAS_ext, BIAS_sca, r
      real, dimension(KIMAGE), intent(in) :: RES
! ......................................................................
      real,dimension(KIMAGE,KW,NSD1) :: TEMP
      character(len=GBL_FILE_PATH_LEN) :: opt_funct, str
      integer :: num_opt_errest_iwl
      integer, dimension(KW) :: opt_errest_iwl
      integer :: NW, INVSING, KNSINGF, KNF
      integer :: i, i1, IW, ISD, ipix
! ......................................................................

      NW      = RIN%nw
      KNSINGF = RIN%KNSINGF
      INVSING = RIN%IPFP%INVSING
      if ( INVSING .eq. 2 ) then
        KNF = KNSINGF*npixels
        num_opt_errest_iwl = RIN%nssa_errest_iwl
        opt_errest_iwl(1:num_opt_errest_iwl) = &
                            RIN%ssa_errest_iwl(1:num_opt_errest_iwl)
      endif

      write(iu_main_output,'(a)') &
      '--------------------------------------------------------------------------------------'
      if(RIN%KL .eq. 1) then
         write(iu_main_output,'(a)') 'Standard deviations of retrieved optical characteristic logarithms (~relative errors) :'
      else
         write(iu_main_output,'(a)') 'Standard deviations of retrieved optical characteristic  (absolute errors) :'      
      endif
      write(iu_main_output,'(a)') &
      '--------------------------------------------------------------------------------------'
      !write(iu_main_output,'(a5,10x,20000(3x,A10,x))')  &
      !            "Date:",(pdate(ipix),ipix=1,npixels)
      !write(iu_main_output,'(a5,12x,20000(3x,A8,3x))')  &
      !            "Time:",(ptime(ipix),ipix=1,npixels)
      write(iu_main_output,'(a5,12x,20000(3x,i8,3x))')  &
                  "pix#:",(ipix,ipix=1,npixels)
      write(iu_main_output,'(a,10x,20000(e14.5))') 'RES:',(RES(ipix),ipix=1,npixels)
      opt_funct = 'Wavelength (um), Single Scattering Albedo '
      do i1=1,6
        select case ( i1 )
        case ( 1 )
          str = trim(opt_funct)//' (correlation coefficient)'
          do ipix=1,npixels
            if(NSD1 .eq. 3) then
            TEMP(ipix,1:NW,NSD1) = r(1:NW,NSD1,ipix)
            endif
            do ISD=1,RIN%NSD
            TEMP(ipix,1:NW,ISD) = r(1:NW,ISD,ipix)
            enddo ! ISD
          enddo ! ipix
        case ( 2 )
          str = trim(opt_funct)//' (ERR_sca)'
          do ipix=1,npixels
            if(NSD1 .eq. 3) then
            TEMP(ipix,1:NW,NSD1) = ERR_sca(1:NW,NSD1,ipix)
            endif
            do ISD=1,RIN%NSD
            TEMP(ipix,1:NW,ISD) = ERR_sca(1:NW,ISD,ipix)
            enddo ! ISD
          enddo ! ipix
        case ( 3 )
          str = trim(opt_funct)//' (ERR_ext)'
          do ipix=1,npixels
            if(NSD1 .eq. 3) then
            TEMP(ipix,1:NW,NSD1) = ERR_ext(1:NW,NSD1,ipix)
            endif
            do ISD=1,RIN%NSD
            TEMP(ipix,1:NW,ISD) = ERR_ext(1:NW,ISD,ipix)
            enddo ! ISD
          enddo ! ipix
        case ( 4 )
          str = trim(opt_funct)//' (ERR_mix)'
          do ipix=1,npixels
            if(NSD1 .eq. 3) then
            TEMP(ipix,1:NW,NSD1) = ERR_mix(1:NW,NSD1,ipix)
            endif
            do ISD=1,RIN%NSD
            TEMP(ipix,1:NW,ISD) = ERR_mix(1:NW,ISD,ipix)
            enddo ! ISD
          enddo ! ipix
        case ( 5 )
          str = trim(opt_funct)//' (BIAS_sca)'
          do ipix=1,npixels
            if(NSD1 .eq. 3) then
            TEMP(ipix,1:NW,NSD1) = BIAS_sca(1:NW,NSD1,ipix)
            endif
            do ISD=1,RIN%NSD
            TEMP(ipix,1:NW,ISD) = BIAS_sca(1:NW,ISD,ipix)
            enddo ! ISD
          enddo ! ipix
        case ( 6 )
          str = trim(opt_funct)//' (BIAS_ext)'
          do ipix=1,npixels
            if(NSD1 .eq. 3) then
            TEMP(ipix,1:NW,NSD1) = BIAS_ext(1:NW,NSD1,ipix)
            endif
            do ISD=1,RIN%NSD
            TEMP(ipix,1:NW,ISD) = BIAS_ext(1:NW,ISD,ipix)
            enddo ! ISD
          enddo ! ipix
        end select

        if ( INVSING .eq. 2 ) then
        ! multi pixel scenario
          if(NSD1 .eq. 3) then
          write(iu_main_output,'(2a)') trim(str),',   Total '
          do i=1,num_opt_errest_iwl
          IW = opt_errest_iwl(i)
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(ipix,IW,NSD1),ipix=1,npixels)
          enddo ! i
          endif
          do ISD=1,RIN%NSD
          write(iu_main_output,'(2a,i3)') trim(str),' for Particle component ',ISD
          do i=1,num_opt_errest_iwl
          IW = opt_errest_iwl(i)
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(ipix,IW,ISD),ipix=1,npixels)
          enddo ! i
          enddo ! ISD
        else
        ! single pixel scenario
          if(NSD1 .eq. 3) then
          write(iu_main_output,'(2a)') trim(str),',   Total '
          do IW=1,NW
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(ipix,IW,NSD1),ipix=1,npixels)
          enddo ! i
          endif
          do ISD=1,RIN%NSD
          write(iu_main_output,'(2a,i3)') trim(str),' for Particle component ',ISD
          do IW=1,NW
          write(iu_main_output,'(f14.4,1000e14.5)')  &
                  RIN%wave(IW),(TEMP(ipix,IW,ISD),ipix=1,npixels)
          enddo ! i
          enddo ! ISD
        endif ! INVSING

      enddo ! i1

    return
    end subroutine print_ssa_errest_components

