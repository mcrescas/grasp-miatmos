! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **


!   file contains:
!
!     SUBROUTINE radiative_transfer_SOS
!     subroutine surface1
!     subroutine aerosol1
!     SUBROUTINE phase_matrix_intrpl_NQDR
!     SUBROUTINE MODIF_DE_TRONC
!     subroutine gauss
!     subroutine developpe_ocean_land
!     subroutine betal
!     subroutine legendre
!     subroutine root
!     subroutine profiles_WD
!     subroutine get_WD
!     FUNCTION   ANGTURN
!     FUNCTION   LINEAR_LN



#include "../../../constants_set/mod_globals.inc"
MODULE MOD_RT_SOS
   USE MOD_GLOBALS,ONLY : GBL_FILE_PATH_LEN
   use mod_stop_report
!PL   Module for different BRDF/BPDF
   use Mod_BRM, only: x_vect,n_par_max,         &
      BRM_Fexp_OSH_XH,BRM,BRM_ocean !BRM_Fexp_opt
   use sub_gas_kd, only : NLEVEL_GAS
!      use mod_molecular_scattering
!------------------------------------------------------
   real, parameter :: pi_sp=3.141592653589793 !23846264338327950 !=3.141592653
   real, parameter :: deg_sp=180.e0/pi_sp
   real, parameter :: rad_sp=pi_sp/180.e0
!------------------------------------------------------
contains

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
   !> PF11_I, PF12_I, PF22_I, PF33_I elements of phase matrix

   SUBROUTINE radiative_transfer_SOS ( ATMOS_EMIS, SOLAR_EMIS,                       & ! IN
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
      laerosol, lsurface,                           &
      PF11_I, PF12_I, PF22_I, PF33_I,               &

      thd, SLout, SQout, SUout, SLPout,             & ! OUT
      salb_out, NLV, HLV, UFX, DFX                  &
      )

!	------------------------------------------------------------------------------------
      USE mod_par_OS
      USE MOD_RT_SOS_SETUP, ONLY : RT_SOS_SET, RT_SOS_CTL, RT_SOS_RES
      USE mod_intrpl_linear
!XH   2*KBF is the maximum length of the array containing surface parameters
      USE mod_par_inv, only : KBF, KIDIM3
      USE mod_par_DLS, only : KMpar
      USE mod_vertical_distr_derived_type
      USE MOD_RT_SOS_MS, ONLY : RT_SOS_MS_OUT, RT_SOS_MS, BETAM, GAMMM
      USE MOD_RT_SOS_LUT,ONLY : ND, AOD, NMLUT,RTM_NM, RT_SOS_MS_MTRX, SRCH_LUT, RTM1
      USE sub_gas_kd, ONLY : DATATM_GAS,NLEVEL_GAS


      implicit none
!XH   number of scattering angles of phase matrix NANG <= KANG
      INTEGER, PARAMETER  ::  KANG=2*NG0T+1
!	------------------------------------------------------------------------------------
!   IN :
      integer,                               intent(in)     ::  NQDR,NANG
      real,dimension(KMpar),                 intent(in)     ::  ANGL
      real,dimension(KMpar,KSD),             intent(in)     ::  PF11_I,PF12_I,PF22_I,PF33_I
      real,dimension(NMM+NMG),               intent(inout)  ::  EXT,SSA
      integer,                               intent(in)     ::  IW,NG,NN,NF,iBRDF,iBPDF,iBRM_water
      integer,                               intent(in)     ::  NBV
      real,                                  intent(in)     ::  WAVE
      integer,                               intent(in)     ::  n_par_land,n_par_land_i,n_par_water
      real,                                  intent(in)     ::  land_percent
      real, dimension(2*KBF),                intent(in)     ::  surf_land_par_vect
      real, dimension(KBF),                  intent(in)     ::  surf_water_par_vect
      real,                                  intent(in)     ::  tetas
      real,dimension(2*NBVM),                intent(in)     ::  vis,fiv
      real,                                  intent(in)     ::  HOBS_km,HGR_km,HMAX_atm_km
      INTEGER,                               intent(in)     ::  NT1(1:2)
      logical,                               intent(in)     ::  laerosol,lsurface
      integer,               optional,       intent(in)     ::  NLV
      real,DIMENSION(KNT),   optional,       intent(in)     ::  HLV
      type(discret_vertical_distribution),   intent(in)     :: DISCRVD

      LOGICAL,                               intent(in)     :: ATMOS_EMIS, SOLAR_EMIS

      real,dimension(KIDIM3,KSD),            intent(in)     ::  SD
      real,dimension(KSD),                   intent(in)     ::  CEXT
      real,dimension(NLEVEL_GAS),            intent(in)     ::  T_profile
      real,                                  intent(in)     ::  STEMP
!	--------------------------------------------------------------------------------------
!   OUT :
      real,dimension(2*NBVM),                intent(out)    ::  thd
      real,dimension(2*NBVM),                intent(out)    ::  SLout,SQout,SUout,SLPout
      real,                                  intent(out)    ::  salb_out
      real,DIMENSION(KNT),   optional,       intent(out)    ::  UFX,DFX
!	--------------------------------------------------------------------------------------
!   LOCAL :
      INTEGER                               ::  n_el, NM, ngas, NM1
      INTEGER                               ::  NANG1
      REAL,DIMENSION(KANG)                  ::  ANGL1
      REAL,DIMENSION(KANG,KSD)              ::  PF11,PF12,PF22,PF33
!PL   save parameters
      REAL*8,DIMENSION(-NG0:NG0),     SAVE  ::  xmu_F,xg_F,xmu_D,xg_D
      REAL*8,DIMENSION(-NN0:NN0),     SAVE  ::  rmu_F,rg_F,rmu_D,rg_D
      REAL*8,DIMENSION(-NG0T:NG0T),   SAVE  ::  zmu_F,zg_F,zmu_D,zg_D
      REAL,DIMENSION(-NN0:NN0),       SAVE  ::  thv_F,thv_D
      REAL,DIMENSION(0:2*NG0,NMM),    SAVE  ::  alp,bet,gam,zet
      INTEGER,                        SAVE  ::  NAV,NT
      REAL, DIMENSION(KNT),           SAVE  ::  EXTH
      REAL, DIMENSION(KNT-1,NMM+NMG), SAVE  ::  WD
      REAL,DIMENSION(2*NBVM),         SAVE  ::  SL1,RL1,SQ1,RQ1
      REAL,DIMENSION(NN0,NN0,0:NF0),  SAVE  ::  R11,R21,R22,R31,R32,R33
      REAL,                           SAVE  ::  SALB, SBT
!>>>>>>>
      !REAL,DIMENSION(-1:2*NG0,-NN0:NN0,0:NF0),TARGET, SAVE  ::  PSL_F,RSL_F,TSL_F
      !REAL,DIMENSION(-1:2*NG0,-NN0:NN0,0:NF0),TARGET, SAVE  ::  PSL_D,RSL_D,TSL_D
!>>>>>>> Tested. Works good

      REAL*8,DIMENSION(-NG0T:NG0T)          ::  zmu,zg
      REAL*8,DIMENSION(-NG0:NG0)            ::  xmu,xg
      REAL*8,DIMENSION(-NN0:NN0)            ::  rmu,rg
      REAL,  DIMENSION(-NN0:NN0)            ::  thv
      INTEGER,DIMENSION(2*NBVM)             ::  idir
      REAL,   DIMENSION(2*NBVM)             ::  coff,chi,rmuv
      REAL,   DIMENSION(2*NBVM)             ::  cos_2etv,sin_2etv
      REAL,   DIMENSION(2*NBVM)             ::  AT,AT_EMIS,TUD
      REAL,DIMENSION(-NG0:NG0,NMM)          ::  pha11,pha12,pha22,pha33
!      REAL, POINTER                         ::  PTR_PSL(:,:,:),  &
!                                                PTR_RSL(:,:,:),  &
!                                                PTR_TSL(:,:,:)
      INTEGER                               ::  iv,ISD,j,IS,k
      REAL                                  ::  rmus,tang,xx,yy,zz,TD
!	------------------------------------------------------------------------------------------------------
      REAL                                  ::  FF
      REAL(8)                               ::  x_vect_water(1:n_par_water), x_vect_land(1:n_par_land)
      REAL                                  ::  tetot_os !,tevtot
      REAL,DIMENSION(NMM+NMG)               ::  EXT_os,SSA_os
      REAL,DIMENSION(2*NBVM)                ::  DDL1, DDQ1,SLP,SQ,SU,SLPsig
      REAL                                  ::  cc,ss,DOMEGA

!XH   list of levels used internally in the radiative transfer calculation
      REAL,DIMENSION(KNT)                   ::  HLV0
!    ------------------------------------------------------------------------------------------------------
      REAL,DIMENSION(2*NBVM), TARGET        ::  SL,SQos,SUos
      real,DIMENSION(KNT),    TARGET        ::  UFX0,DFX0
!	------------------------------------------------------------------------------------------------------
      TYPE(RT_SOS_MS_OUT)                   ::  RT_OUT
      CHARACTER(LEN=GBL_FILE_PATH_LEN)      ::  FN_LUT,FN_LUT_DIR
      INTEGER                               ::  ID, IMOD, ICMP
      REAL                                  ::  CMAX, CTMP
      LOGICAL                               ::  EX
!	------------------------------------------------------------------------------------------------------
!MH EMIS
      REAL,dimension(KNT),SAVE                ::	BT
      REAL,SAVE                               ::  E0
      INTEGER,DIMENSION(KNT)                ::  index_T
      INTEGER                               ::  jjj,i1,i
      REAL                                  ::  TTH, aux_ext

      REAL,DIMENSION(KNT-1)                 ::  T_layer
      REAL,DIMENSION(KNT)                  ::  T_level
      REAL,DIMENSION(NMM+NMG,KVERT_WD)      ::  tauH

      REAL,DIMENSION(KVERT_WD)               ::  tauH_TOT_int

!	------------------------------------------------------------------------------------------------------


      LOGICAL     :: surf_iso=.false., surf_albedo, surf_cor
      real                                    :: abs_total

!     DESCRIPTION:

!>    @LOCAL			rmus	Solar emitting direction, mu = cos(theta_0)
!>    @LOCAL			EXTH	Total extinction corresponding to each level
!>    @LOCAL			NT		Total number of level
!>    @LOCAL			NAV		Level where your measurements are taken; satellite => = 1; ground based => = NT; airbourne => = NTEMP + 1
!>    @param[in]		NBVM	Number of observation angles
!>    @param[in]		NM		Aerosol components
!>    @LOCAL			WD		SSA for each layer weighted for the corresponding extinction
!>    @param[in]		WAVE	Wavelength in micrometrers
!>    @param[in]		iBRDF/iBPDF	[Booleans] Selecting the model to descriebe surface in case of scattering or polarization
!>    @param[in]		IVEC [Boolean]	Selecting scalar or vector radiative transfer equation
!>    @param[in]		LL/NQDR	Number of quadrature points
!>    @param[in]		laerosol [Boolean]	Taking into account aerosol calculations
!>    @param[in]		lsurface [Boolean]	Taking into account surface calculations
!>    @param[in]		ISGL [Boolean]		If TRUE only single scattering calculations
!>    @param[in]		IWUT [Boolean]		Look up table approach

!	  write(*,*) 'boa_ref, folder ', RT_SOS_SET%boa_ref, ' ',RT_SOS_SET%LUT_path
!	  stop
!write(*,*) 'SSA',SSA
!write(*,*) 'EXT',EXT
!write(*,*) 'P11', PF11_I
!write(*,*) 'P22', PF12_I


      NM    = DISCRVD%naer
      ngas  = DISCRVD%ngas

      SSA_os(:)=0.0
      EXT_os(:)=0.0

      SLout  = 0.0
      SQout  = 0.0
      SUout  = 0.0
      SLPout = 0.0
      thd    = 0.0

!     n_par_land   - total number of surface parameters (BRDF+BPDF) for land
!     n_par_land_i - number of BRDF surface parameters for land
!     n_par_water  - number of parameters for water

      x_vect_land(1:n_par_land)=surf_land_par_vect(1:n_par_land)
      x_vect_water(1:n_par_water)=surf_water_par_vect(1:n_par_water)

      if (RT_SOS_CTL%ISGL) then
         if(NANG .gt. KANG) then
            write(tmp_message,'(2(a,i0))') 'NANG = ',NANG,' .gt. KANG = ',KANG
            G_ERROR(trim(tmp_message))
         endif
      endif



!PL   Calculating Gaussian weights and quadratures
!PL   "zg" and "zmu" for phase matrix expansion
!PL   "xg" and "xmu" for expansion of trancated Phase matrix
!PL   "rg" and "rmu" for intagrals calculations in RT

!PL   RT_SOS_CTL%IJCB = .FALSE. - forward calculations
!PL   RT_SOS_CTL%IJCB = .TRUE.  - Jacobean matrix calculations

!PL   Array zmu,zg can be defined in the range  -NQDR:NQDR !!!
!PL   Array xmu,xg can be defined in the range  -NG  :NG   !!!
!PL   Array rmu,rg can be defined in the range  -NN  :NN   !!!
!PL   Therefore NG0T>=NQDR ; NG0>=NG ; NN0>=NN             !!!

      IF (.NOT. RT_SOS_CTL%ISGL) THEN
         IF (RT_SOS_CTL%IJCB) THEN
!XH           derivatives (multiple scattering)
            IF (.NOT. RT_SOS_RES%IGQ_D) THEN
!MH				  Forward Peak Phase Matrix quadrature
               zmu_D(-NG0T:NG0T) = 0.0
               zg_D (-NG0T:NG0T) = 0.0
               call GAUSS(NQDR,zmu_D,zg_D,NG0T)
!MH				  Truncated Phase Matrix quadrature
               xmu_D(-NG0 :NG0 ) = 0.0
               xg_D (-NG0 :NG0 ) = 0.0
               call gauss(NG  ,xmu_D,xg_D,NG0 )
!MH				  RT angular integral quadrature
               rmu_D(-NN0 :NN0 ) = 0.0
               rg_D (-NN0 :NN0 ) = 0.0
               call gauss(NN  ,rmu_D,rg_D,NN0 )
               thv_D(-NN0 :NN0 ) = 0.0
               thv_D(-NN  :NN  ) = acos(rmu_D(-NN:NN))*deg_sp
!                  PSL_D(:,:,:)=0.0
!                  TSL_D(:,:,:)=0.0
!                  RSL_D(:,:,:)=0.0
!                  Do IS=0,NF
!                      call legendre(IS,rmu_D,NG,NN,PSL_D(:,:,IS),TSL_D(:,:,IS),RSL_D(:,:,IS))
!                  End do ! IS
               RT_SOS_RES%IGQ_D = .TRUE.
            END IF ! IGQ_D

            xmu(-NG0 :NG0 ) = xmu_D(-NG0 :NG0 )
            xg (-NG0 :NG0 ) = xg_D (-NG0 :NG0 )
            rmu(-NN0 :NN0 ) = rmu_D(-NN0 :NN0 )
            rg (-NN0 :NN0 ) = rg_D (-NN0 :NN0 )
            thv(-NN0 :NN0 ) = thv_D(-NN0 :NN0 )
            zmu(-NG0T:NG0T) = zmu_D(-NG0T:NG0T)
            zg (-NG0T:NG0T) = zg_D (-NG0T:NG0T)
!               PTR_PSL(-1:,-NN0:,0:) => PSL_D
!               PTR_TSL(-1:,-NN0:,0:) => TSL_D
!               PTR_RSL(-1:,-NN0:,0:) => RSL_D
         ELSE
!XH            forward modeling (multiple scattering)
            IF (.NOT. RT_SOS_RES%IGQ_F) THEN
               zmu_F(-NG0T:NG0T) = 0.0
               zg_F (-NG0T:NG0T) = 0.0
               call GAUSS(NQDR,zmu_F,zg_F,NG0T)
               xmu_F(-NG0 :NG0 ) = 0.0
               xg_F (-NG0 :NG0 ) = 0.0
               call gauss(NG  ,xmu_F,xg_F,NG0 )
               rmu_F(-NN0 :NN0 ) = 0.0
               rg_F (-NN0 :NN0 ) = 0.0
               call gauss(NN  ,rmu_F,rg_F,NN0 )
               thv_F(-NN0 :NN0 ) = 0.0
               thv_F(-NN  :NN  ) = acos(rmu_F(-NN:NN))*deg_sp
!                   PSL_F(:,:,:)=0.0
!                   TSL_F(:,:,:)=0.0
!                   RSL_F(:,:,:)=0.0
!                   Do IS=0,NF
!                       call legendre(IS,rmu_F,NG,NN,PSL_F(:,:,IS),TSL_F(:,:,IS),RSL_F(:,:,IS))
!                   End do ! IS
               RT_SOS_RES%IGQ_F = .TRUE.
            END IF ! IGQ_F
            zmu(-NG0T:NG0T) = zmu_F(-NG0T:NG0T)
            zg (-NG0T:NG0T) = zg_F (-NG0T:NG0T)
            xmu(-NG0 :NG0 ) = xmu_F(-NG0 :NG0 )
            xg (-NG0 :NG0 ) = xg_F (-NG0 :NG0 )
            rmu(-NN0 :NN0 ) = rmu_F(-NN0 :NN0 )
            rg (-NN0 :NN0 ) = rg_F (-NN0 :NN0 )
            thv(-NN0 :NN0 ) = thv_F(-NN0 :NN0 )
!               PTR_PSL(-1:,-NN0:,0:) => PSL_F
!               PTR_TSL(-1:,-NN0:,0:) => TSL_F
!               PTR_RSL(-1:,-NN0:,0:) => RSL_F
         END IF ! RT_SOS_CTL%IJCB
!           WRITE(*,*) 'zmu:'
!           WRITE(*,'(10e14.4)') zmu(-NG0T:NG0T)
!           WRITE(*,*) 'zg:'
!           WRITE(*,'(10e14.4)') zg (-NG0T:NG0T)
!           WRITE(*,*) 'xmu:'
!           WRITE(*,'(10e14.4)') xmu(-NG0 :NG0 )
!           WRITE(*,*) 'xg:'
!           WRITE(*,'(10e14.4)') xg (-NG0 :NG0 )
!           WRITE(*,*) 'rmu:'
!           WRITE(*,'(10e14.4)') rmu(-NN0 :NN0 )
!           WRITE(*,*) 'rg:'
!           WRITE(*,'(10e14.4)') rg (-NN0 :NN0 )
      END IF ! .NOT. RT_SOS_CTL%ISGL
!	------------------------------------------------------------------------------------------------------

!XH   cosine of solar zenith angle - rmus
      rmus=cos(tetas*rad_sp)
!      do j=-NN,NN
!         thv(j)=acos(rmu(j))*deg_sp
!         WRITE(*,*) thv(j),j
!      end do ! j

!XH   observation geometries
      idir   = 0
      coff   = 0.

      do iv=1,NBV
         xx=cos(vis(iv)*rad_sp)
!PL      Rotation angle between meridian plane and scattering
         chi(iv)=ANGTURN(tetas,vis(iv),fiv(iv))
!PL       cos_2etv=-cc !!! Because of positive SQ1,SL1
!PL       sin_2etv=-ss !!! The reason is not clear yet
!PL       call ANGTURN_PL(tetas,vis(iv),fiv(iv),cos_2etv(iv),sin_2etv(iv))
!MH		 Scattering angle calculation
         zz=-rmus*xx-real(dsqrt(1.d0-rmus*rmus)*dsqrt(1.d0-xx*xx))*cos(fiv(iv)*rad_sp)
         if (zz .GT. 1.0) then
            write(*,*) 'Incorrect cosine of scattering angle',zz
            zz = 1.0
         else if (zz .LT. -1.0) then
            write(*,*) 'Incorrect cosine of scattering angle',zz
            zz =-1.0
         end if ! zz .GT. 1.0
         thd(iv)=acos(zz)*deg_sp
         if (.NOT. RT_SOS_CTL%ISGL) then
            if (thv(NN) .lt. vis(iv)) then
               if (abs(thv(-NN)-vis(iv)) .le. 1e-4) then !pl&tl 23-02-2015
                  idir(iv) = -NN
                  coff(iv) = 0.
               else
                  k = NN-1
                  do while (thv(k) .lt. vis(iv))
                     k = k-1
                  end do
                  idir(iv) = k
                  coff(iv) = (vis(iv)-thv(k+1))/(thv(k)-thv(k+1))
               endif
            else
!XH            for the exact nadir direction
               idir(iv) = NN-1
               coff(iv) = 0.
            end if
!         write(*,*) 'in OS_H NBV=',NBV
!         write(*,*) 'vis:'
!         write(*,'(10f16.4)') vis(1:NBV)
!         write(*,*) 'in OS_H NN=',NN,'  thv(k)=',thv(k),'  thv(k+1)=',thv(k+1),'  iv=',iv,'  vis(iv)=',vis(iv)
         endif ! .NOT. RT_SOS_CTL%ISGL
      end do !iv
!	------------------------------------------------------------------------------------------------------

      LOOP_AOD: DO ID = 1, ND
!     Phase matrix interpolation
!PL   Subroutine "phase_matrix_intrpl_NQDR" interpolates Phase Matrix "PF11_I"
!PL   defined at angles "ANGL" into the Phase Matrix  "PFij" which is defined at
!PL   angles "ANGL1"=acos(zmu)
         If (laerosol) then
!         write(*,*) NM,NQDR,KMpar,NANG,'  - NM,NQDR,KMpar,NANG'
!         do i=1,NANG
!            write(*,'(f14.3,10e14.5)') ANGL(i),PF11_I(i,1:NM)
!         end do
!         write(*,*) 'zmu:'
!         write(*,'(10e14.5)') zmu(1:NQDR)
!         write(*,*) 'zg:'
!         write(*,'(10e14.5)') zg(1:NQDR)
            IF (RT_SOS_CTL%ISGL) THEN
!XH         single scattering
!MH			The angles from kernels/models are taken directly
               if (KANG .lt. KMpar) then
                  write(tmp_message,'(2(a,i0))') 'KANG = ',KANG,' lt. KMpar = ',KMpar
                  G_ERROR(trim(tmp_message))
               endif
               NANG1 = NANG
               ANGL1(1:NANG) = ANGL(1:NANG)
               PF11(1:NANG,1:NM) = PF11_I(1:NANG,1:NM)
               if (RT_SOS_CTL%IVEC) then
                  PF12(1:NANG,1:NM) = PF12_I(1:NANG,1:NM)
                  PF22(1:NANG,1:NM) = PF22_I(1:NANG,1:NM)
                  PF33(1:NANG,1:NM) = PF33_I(1:NANG,1:NM)
               endif
            ELSE
!XH         multiple scattering
!MH			from kernel/model angles to quadrature (Scatt angles)
               call phase_matrix_intrpl_NQDR (                                 &
                  RT_SOS_CTL%IVEC,               & ! IN
                  NM,NQDR,                       &
                  zmu,zg,                        &
                  KMpar,NANG,ANGL,               &
                  PF11_I,PF12_I,PF22_I,PF33_I,    &
                  NANG1,ANGL1,                   & ! OUT
                  PF11,PF12,PF22,PF33            &
                  )

            END IF ! RT_SOS_CTL%ISGL

            NM1=NM+1
            SSA_os(1:NM1+ngas)=SSA(1:NM1+ngas)
            EXT_os(1:NM1+ngas)=EXT(1:NM1+ngas) ! aerosol, molecular, gas

            tetot_os=SUM(EXT_os(1:NM1+ngas))

! write(*,'(a)') 'EXT'
! write(*,'(10es12.4)') EXT_os(1:NM1+ngas)
! write(*,'(a)') 'SSA'
! write(*,'(10es12.4)') SSA_os(1:NM1+ngas)
!stop 888

!if(RT_SOS_RES%IGQ_D) then
!write(*,*) IW,EXT_os(:), '  - 1: IW, EXT_os(:) in radiative_transfer_SOS'
!write(*,*) IW,SSA_os(:), '  - 1: IW, SSA_os(:) in radiative_transfer_SOS'
!endif

!PL LUT generation (into FN_LUT) or reading
            if (RT_SOS_SET%IWUT) then
               CMAX = 0.
               DO ISD = 1, RT_SOS_SET%NA
                  CTMP = MAXVAL(SD(1:RT_SOS_SET%NB(ISD),ISD))
                  IF (CTMP .GT. CMAX) THEN
                     ICMP = ISD
                     IMOD = MAXLOC(SD(1:RT_SOS_SET%NB(ISD),ISD),DIM=1)
                     CMAX = CTMP
                  END IF
               END DO
!PL			 the name of LUT file
               if (RT_SOS_CTL%IVEC) then
                  WRITE(FN_LUT,'(A4,I0.4,A2,I1,I1,A2,I0.3,A2)')         &
                     TRIM(RTM_NM), INT(WAVE*1000.+0.5),       &
                     '_M', ICMP, IMOD,                         &
                     '_T', INT(AOD(ID)*100.+0.5),'_V'
               else
                  WRITE(FN_LUT,'(A4,I0.4,A2,I1,I1,A2,I0.3,A2)')         &
                     TRIM(RTM_NM), INT(WAVE*1000.+0.5),       &
                     '_M', ICMP, IMOD,                         &
                     '_T', INT(AOD(ID)*100.+0.5),'_S'
               end if
!PL LUT directory from RIN
               FN_LUT_DIR=TRIM(RT_SOS_SET%INTL_PATH)//'/'//TRIM(NMLUT)//'/'   &
                  //TRIM(RT_SOS_SET%LUT_path)
               FN_LUT=TRIM(FN_LUT_DIR)//'/'//TRIM(FN_LUT)

               RTM1%EXT=  CEXT(ICMP)
               RTM1%SSA=SSA_os(ICMP)

!XH          use LUT AOD list
               EXT_os(1:NM) = 0.
               EXT_os(ICMP) = AOD(ID)
               tetot_os     = SUM(EXT_os(1:NM1+ngas))
            else if (RT_SOS_SET%ILUT) then
               CALL SRCH_LUT(RT_SOS_CTL%IVEC,IW,RT_SOS_SET%NA,RT_SOS_SET%NB,SUM(EXT_OS(1:NM)),SD)
               if ( error_present() ) return
            else if (.NOT. RT_SOS_CTL%IAER) then
!XH          not considering aerosol
               EXT_os(1:NM)=0.
               tetot_os=SUM(EXT_os(1:NM1+ngas))

            end if ! RT_SOS_SET%IWUT

!         tevtot = tetot_os

!PL      Subroutine "MODIF_DE_TRONC" substitutes the Phase Matrix  "PFij" with the truncated
!PL      matrix, and modifies the AOD (tetot_os,EXT_os(1:NM))
!PL      and single scattering albedo "SSA_os" according to the truncation

!         write(*,*)  RT_SOS_CTL%ISGL,RT_SOS_SET%ITRC,NM,NQDR
!         write(*,*) 'before MODIF_DE_TRONC: tetot_os',tetot_os
!         write(*,*) 'before MODIF_DE_TRONC: EXT_os(1:NM1+ngas)',EXT_os(1:NM1+ngas)
!         write(*,*) 'before MODIF_DE_TRONC: SSA_os(1:NM1+ngas)',SSA_os(1:NM1+ngas)
!write(*,*) 'WAVE',WAVE
!write(*,*)EXT_os(:)
!write(*,*)SSA_os(:)
!stop
            if ((.NOT. RT_SOS_CTL%ISGL .AND. RT_SOS_SET%ITRC)) then

!XH         if only single scattering is needed, no need to truncate phase matrix
               CALL MODIF_DE_TRONC_new (                                    &
                  RT_SOS_CTL%IVEC,                    &
                  RT_SOS_CTL%IDWN,                    &
                  NM,NQDR,                            & ! IN
                  zmu,zg,NANG1,ANGL1,                 &
                  NBV,thd,rmus,                       &
                  tetot_os,EXT_os(1:NM),SSA_os(1:NM), & ! INOUT
                  PF11,PF12,PF22,PF33,                &
                  DDL1,DDQ1                           & ! OUT
                  )
!write(*,*) 'after MODIF_DE_TRONC_new'
!write(*,*) 'EXT',EXT_os(1:NM1+ngas)
!write(*,*) 'SSA',SSA_os(1:NM1+ngas)
!write(*,*) 'tetot_os',tetot_os
!stop 888
            end if

!PL      Subroutine "profiles_WD" calculates aerosol-Reyleigh altitude
!PL      AOD profile "WD"

            call profiles_WD (  RT_SOS_CTL%IFLX,ATMOS_EMIS,        &  ! IN
               RT_SOS_SET%IP_ADDITION,            &
               RT_SOS_CTL%IDWN,                   &
               tetot_os, EXT_os, SSA_os,          &
               HGR_km, HOBS_km, HMAX_atm_km, NT1, &
               T_profile,                         &
               DISCRVD,                           &
               NT, NAV, EXTH, WD, HLV0,           & ! OUT
               T_layer, T_level,                  &
               index_T, tauH_TOT_int              &
               )

!write(*,*)'NT1', NT1
!write(*,*)'EXTH', EXTH(1:NT)
!write(*,*)'Number of layers', NT
!write(*,*)'Phase function', PF11
!write(*,*)'rmus', rmus
!write(*,*)'vis', vis(1:NBV)
!write(*,*)'fiv', fiv(1:NBV)

            if ( error_present() ) return

!PL      Reyleigh + Aeroslol phase matrix for incident and viewing geometries
!         write(*,*) NM,NT,NBV,NAV,'  - NM,NT,NBV,NAV'
!         write(*,*) 'ANGL1:'
!         do i=1,NANG1
!            write(*,'(f14.3,10e14.5)') ANGL1(i),PF11(i,1:NM),PF12(i,1:NM)
!         end do
!XH      the first order scattering is given by SL1 and SQ1
!write(*,*) WAVE


!		call source_func(RT_SOS_CTL%IVEC,       & ! IN
!						 RT_SOS_CTL%IDWN,       &
!                         lsurface,              &
!                         ATMOS_EMIS,            &
!                         SOLAR_EMIS,            &
!						 NM,NT,NBV,NAV,         &
!						 NANG1,ANGL1,           &
!						 rmus,thd,vis,          &
!						 EXTH,WD,               &
!						 BETAM,GAMMM,           &
!						 PF11,PF12,		        &
!						 T_layer,STEMP, WAVE,   &
!
!						 SL1,SQ1,BT,SBT,E0      & ! OUT
!						 )

            call source_func_T_level(RT_SOS_CTL%IVEC,       & ! IN
               RT_SOS_CTL%IDWN,       &
               lsurface,              &
               ATMOS_EMIS,            &
               SOLAR_EMIS,            &
               NM,NT,NBV,NAV,         &
               NANG1,ANGL1,           &
               rmus,thd,vis,          &
               EXTH,WD,               &
               BETAM,GAMMM,           &
               PF11,PF12,             &
               T_level,STEMP, WAVE,   &
               index_T, tauH_TOT_int, &
               T_profile,DISCRVD%NH,  &
               SL1,SQ1,BT,SBT,E0      & ! OUT
               )
!write(*,*)WAVE,BT(1:NT)
!PL      Diffuse caclulation (begin)
!        Interpolation of phase matrix at the required angles:

            IF (.NOT. RT_SOS_CTL%ISGL .AND. .NOT. RT_SOS_SET%ILUT) THEN

!PL         PF11 is defined in the range (1:2*NQDR+1) (that is (-NQDR,NQDR))
!PL         pha11 is defined in the range (-NG,NG)
               IF (NQDR .EQ. NG) THEN
                  pha11(-NG:NG,1:NM)=PF11(1:NG+NG+1,1:NM)
                  if (RT_SOS_CTL%IVEC) then
!XH               considering linear polarization
                     pha12(-NG:NG,1:NM)=PF12(1:NG+NG+1,1:NM)
                     pha22(-NG:NG,1:NM)=PF22(1:NG+NG+1,1:NM)
                     pha33(-NG:NG,1:NM)=PF33(1:NG+NG+1,1:NM)
                  end if
               ELSE
                  DO j=-NG,NG
                     tang=acos(xmu(j))*deg_sp
                     DO ISD=1,NM
                        pha11(j,ISD)=LINEAR_LN(ANGL1(1:NANG1),PF11(1:NANG1,ISD),NANG1,tang)
                     END DO
                     if (RT_SOS_CTL%IVEC) then
!XH                  considering linear polarization
                        DO ISD=1,NM
                           pha12(j,ISD)=LINEAR(ANGL1(1:NANG1),PF12(1:NANG1,ISD),NANG1,tang)
                           pha22(j,ISD)=LINEAR_LN(ANGL1(1:NANG1),PF22(1:NANG1,ISD),NANG1,tang)
                           pha33(j,ISD)=LINEAR(ANGL1(1:NANG1),PF33(1:NANG1,ISD),NANG1,tang)
                        END DO
                     end if
                  END DO
               END IF ! NQDR .EQ. NG

               do ISD=1,NM
                  call betal(RT_SOS_CTL%IVEC,NG,NG0,xmu,xg,pha11(:,ISD),pha12(:,ISD),pha22(:,ISD),pha33(:,ISD),  &
                     alp(0:NG0+NG0-2,ISD),bet(0:NG0+NG0-2,ISD),  &
                     gam(0:NG0+NG0-2,ISD),zet(0:NG0+NG0-2,ISD)  )
               end do ! ISD

            END IF ! .NOT. RT_SOS_CTL%ISGL .AND. .NOT. RT_SOS_SET%ILUT
         end if  ! laerosol



!	  ------------------------------------------------------------------------------------------------------

!      isaut=int(land_percent/100)+1
!	  select case(isaut)
!PL   Ocean
!      case(1)
!         xind=Rem_water
!PL   Land Surfaces with different BRDFs
!      case(2)
!         xind=Rem_land
!      case default
!         write(*,*) 'isaut=',isaut,'  - unknown value'
!         stop 'stop in MOD_RT_SOS: radiative_transfer_SOS'
!	  end select ! isaut

!      write(*,*) 'in radiative_transfer_SOS: NF,iBRDF,iBPDF,iBRM_water,isaut,IW : ',NF,iBRDF,iBPDF,iBRM_water,isaut,IW

! ------------------------------------------------------------------------------------------------------

!PL   surface (begin)
         if (lsurface) then
!PL      Surface reflection for incident and viewing geometries
            RL1(:)=0.0
            if (RT_SOS_CTL%IVEC) then
!XH         consider linear polarization
               RQ1(:)=0.0
               n_el=3
            else
               n_el=1
            end if

            if (RT_SOS_CTL%ISRF) then
               if (.NOT. RT_SOS_CTL%IDWN) then
!XH              surface only constributes to upward radiance at TOA for the first order of scattering
                  call surface1 ( n_el,NBV,iBRDF,iBPDF,iBRM_water,land_percent,WAVE,              & ! IN
                     rmus,vis,fiv,n_par_land,n_par_land_i,x_vect_land(1:n_par_land), &
                     n_par_water,x_vect_water(1:n_par_water),                        &
                     RL1,RQ1                                                         & ! OUT
                     )
               end if

               if (.NOT. RT_SOS_CTL%ISGL) then
!PL              calculates Fourier expansion coefficients for surfaces
                  call BRM_Fexp_OSH_XH ( RT_SOS_SET%ILUT,n_el,land_percent,NF,NN,rmu(1:NN),rg(1:NN),dble(WAVE), &
                     iBRDF,iBPDF,n_par_land,n_par_land_i,x_vect_land(1:n_par_land),         &
                     iBRM_water,n_par_water,x_vect_water(1:n_par_water),                    &
                     R11(1:NN,1:NN,0:NF),R21(1:NN,1:NN,0:NF),R22(1:NN,1:NN,0:NF),           &
                     R31(1:NN,1:NN,0:NF),R32(1:NN,1:NN,0:NF),R33(1:NN,1:NN,0:NF),           &
                     SALB                                                                   &
                     )
               end if ! .NOT. RT_SOS_CTL%ISGL
            end if ! RT_SOS_CTL%ISRF
         end if ! lsurface
!XH   taking saved surface albedo
         salb_out=SALB

!     TEST
!      write(*,*) ' IN OS_H laerosol=',laerosol,' lsurface=',lsurface
!      write(*,*)
!      write(*,*) 'SL1:'
!      write(*,'(10e16.6)') SL1
!      write(*,*) 'SQ1:'
!      write(*,'(10e16.6)') SQ1
!      write(*,*)

!     TEST
!      write(*,*) 'RL1:'
!      write(*,'(10e16.6)') RL1
!      write(*,*) 'RQ1:'
!      write(*,'(10e16.6)') RQ1
!      write(*,*)

!     TEST
!      write(*,*) 'R11:'
!      write(*,'(10e16.6)') R11
!      write(*,*) 'R21:'
!      write(*,'(10e16.6)') R21
!      write(*,*) 'R22:'
!      write(*,'(10e16.6)') R22
!      write(*,*) 'R31:'
!      write(*,'(10e16.6)') R31
!      write(*,*) 'R32:'
!      write(*,'(10e16.6)') R32
!      write(*,*) 'R22:'
!      write(*,'(10e16.6)') R33
!      write(*,*)


!      open(100,file=trim(RT_SOS_SET%EXTL_PATH)//'test_OSH.txt')

!      write(100,*) 'R11:'
!      write(100,'(10e16.6)') R11
!      write(100,*) 'R21:'
!      write(100,'(10e16.6)') R21
!      write(100,*) 'R22:'
!      write(100,'(10e16.6)') R22
!      write(100,*) 'R31:'
!      write(100,'(10e16.6)') R31
!      write(100,*) 'R32:'
!      write(100,'(10e16.6)') R32
!      write(100,*) 'R22:'
!      write(100,'(10e16.6)') R33
!      write(100,*)

!      close(100)

! ------------------------------------------------------------------------------------------------------
!PL   Single scattering + surface (end)
! ------------------------------------------------------------------------------------------------------

         SL = 0.0
         if (RT_SOS_CTL%IVEC) then
            SQ   = 0.0
            SU   = 0.0
            SQos = 0.0
            SUos = 0.0
         end if
         if (RT_SOS_CTL%IFLX) then
            UFX0 = 0.0
            DFX0 = 0.0
         end if
!if(RT_SOS_RES%IGQ_D) then
!write(*,*) IW,EXT_os(:), '  - 2: IW, EXT_os(:) in radiative_transfer_SOS'
!write(*,*) IW,SSA_os(:), '  - 2: IW, SSA_os(:) in radiative_transfer_SOS'
!endif
!#warning "TEMPORARY solution by TL : double assign a value to variable NM1 = NM+1"
!PL Confirmed
!      NM1 = NM+1 ! assign a value to NM1 by TL; before was assign under laerosol=T condition

!PL



         IF (.NOT. RT_SOS_CTL%ISGL .AND. EXT_os(NM+1+ngas) .LT. 100.) THEN
!PL Modification for total Optical Depth for gasses should be fixed in
!PL next tug:
!PL   IF (.NOT. RT_SOS_CTL%ISGL .AND. SUM(EXT_os(NM1:NM1+ngas)) .LT. 100.) THEN

!XH       if gas absorption is too strong, then skip multiple
!XH       scattering calculation to prevent overflow in the analytical
!XH       integral in the first order scattering calculation
            !IF (.NOT. RT_SOS_CTL%ISGL) THEN
!XH       if only single scattering is concerned, then skip multiple
!XH       scattering calculation
            RT_OUT%SVI => SL(1:NBV)
            RT_OUT%SVQ => SQos(1:NBV)
            RT_OUT%SVU => SUos(1:NBV)
            RT_OUT%UFX => UFX0(1:NT)
            RT_OUT%DFX => DFX0(1:NT)
            RT_OUT%ASRF=  salb
            if (RT_SOS_SET%IWUT .OR. RT_SOS_SET%ILUT) then
               RT_OUT%AATM => RTM1%AATM
               RT_OUT%TATM => RTM1%TATM
               RT_OUT%SVIM => RTM1%IM
               RT_OUT%SVQM => RTM1%QM
               RT_OUT%SVUM => RTM1%UM
            end if

            CALL RT_SOS_MS(RT_SOS_SET,RT_SOS_CTL,ATMOS_EMIS,                                        &
               NF,2*NG,tetas,NAV,                                                       &
               NN,rmu(-NN:NN),rg(-NN:NN),                                               &
               NBV,idir(1:NBV),coff(1:NBV),fiv(1:NBV),                                  &
               NM,alp(0:2*NG,1:NM),bet(0:2*NG,1:NM),gam(0:2*NG,1:NM),zet(0:2*NG,1:NM),  &
               NT,EXTH(1:NT),WD(1:NT-1,1:NM+1),                                         &
               R11(1:NN,1:NN,0:NF),R21(1:NN,1:NN,0:NF),R22(1:NN,1:NN,0:NF),             &
               R31(1:NN,1:NN,0:NF),R32(1:NN,1:NN,0:NF),R33(1:NN,1:NN,0:NF),             &
               BT(1:NT-1), SBT, E0, SALB,		     									  &
               RT_OUT)
!write(*,*)'OUT',SL(1:NBV)
            if (RT_SOS_CTL%IVEC) then
!XH         rotation to the scattering plane
!PL The rotation of Q and U to the scattering plane is necessary to add trancation part of RT
!PL as well as single scattering properties which are calculated in the scattering plane.
!PL The rotation back to meridian plan is presented below.
!PL Another polibility is to rotate TR trancation, single aerosol and surface to meridian plan

               do iv=1,NBV
!              Original
                  cc=cos(2.0*chi(iv))
!              Original
                  ss=sin(2.0*chi(iv))
!PL            Stokes vector in the scattering plane (SL, SQ)
!              Original
                  SQ(iv)= cc*SQos(iv)+ss*SUos(iv)
!PL            U Stokes parameter in scattering plane
!              Original
                  SU(iv)=-ss*SQos(iv)+cc*SUos(iv)
!PL
!               cc=-cos(2.*chi(iv))
!               ss=-sin(2.*chi(iv))
!PL
!               cc=cos_2etv(iv)
!PL
!               ss=sin_2etv(iv)
!
!               SQ(iv)=-(cc*SQos(iv)-ss*SUos(iv)) !sign is changed
!               SU(iv)=ss*SQos(iv)+cc*SUos(iv)
               end do ! iv

            end if ! RT_SOS_CTL%IVEC

!MH Sum back to the total radiance the part eliminated by truncation
            if ((RT_SOS_SET%ITRC .AND. RT_SOS_CTL%IDWN))then
               SL(1:NBV)=SL(1:NBV)+ (DDL1(1:NBV)*E0)
               if (RT_SOS_CTL%IVEC) SQ(1:NBV)=SQ(1:NBV)+(DDQ1(1:NBV)*E0)
            end if


         END IF ! .NOT. RT_SOS_CTL%ISGL

         if (RT_SOS_SET%IWUT) then
!XH       output LUT
!          INQUIRE(FILE=TRIM(RT_SOS_SET%INTL_PATH)//'/'//TRIM(NMLUT),EXIST=EX)
!          IF (.NOT. EX) call execute_command_line ('mkdir -p '//TRIM(RT_SOS_SET%INTL_PATH)//'/'//TRIM(NMLUT))
!PL modification of dir for output
            INQUIRE(FILE=TRIM(FN_LUT_DIR),EXIST=EX)

            IF (.NOT. EX) call execute_command_line ('mkdir -p '//TRIM(FN_LUT_DIR))
            open(9,FILE=FN_LUT,FORM="UNFORMATTED",ACCESS="SEQUENTIAL",STATUS="UNKNOWN")
            if (RT_SOS_CTL%IVEC) then
               write(9) RTM1%EXT, RTM1%SSA, RTM1%AATM, RTM1%TATM, RTM1%IM, RTM1%QM, RTM1%UM
            else
               write(9) RTM1%EXT, RTM1%SSA, RTM1%AATM, RTM1%TATM, RTM1%IM
            end if
            close(9)
         ELSE
            exit LOOP_AOD
         end if

      END DO LOOP_AOD

! ------------------------------------------------------------------------------------------------------
      AT = 0.0
      AT_EMIS = 0.0

      if (.NOT. RT_SOS_CTL%IDWN) then ! Satellite ! ask OD or PL
         do j=1,NBV
            xx=cos(vis(j)*rad_sp)
!            if(xx. lt. 0.0) cycle ! do not delete
!            AT(j)=exp(-EXTH(NT)/rmus)*exp(-(EXTH(NT)-EXTH(NAV))/xx)

!MH         Including Surface reflected radiance in emission approach:
            AT(j)=exp(-EXTH(NT)/rmus)*exp(-(EXTH(NT)-EXTH(NAV))/xx)*E0

            IF(ATMOS_EMIS) THEN
               AT_EMIS(j)=exp(-(EXTH(NT)-EXTH(NAV))/xx)
            END IF
         end do
      end if ! .NOT. RT_SOS_CTL%IDWN
      if (RT_SOS_SET%ILUT) then
!XH      interpolate to observation geometry to get upward diffuse+direct transmittance
!XH      may be more accurate to separate direct and diffuse transmittance and interpolate diffuse transmittance only
         DO IV = 1, NBV
            K = IDIR(IV)
            TUD(IV) = RTM1%TATM(K+1) + COFF(IV)                         &
               *(RTM1%TATM(K)-RTM1%TATM(K+1))
         END DO
!XH      interpolate to observation geometry to get downward diffuse+direct transmittance
         IF (RMUS .LT. RMU(NN)) THEN
            K = NN-1
            DO WHILE (RMUS .LT. RMU(K))
               K = K-1
            END DO
            IF (K .GT. 0) THEN
               XX = (RMUS-RMU(K+1))/(RMU(K)-RMU(K+1))
            ELSE
               XX = (RMUS-RMU(  1))/(      -RMU(  1))
            END IF
         ELSE
            K = NN-1
            XX = 0.
         END IF
         IF (K .GT. 0) THEN
            TD = (1.-XX)*RTM1%TATM(K+1) + XX*RTM1%TATM(K)
         ELSE IF (K .EQ. 0) THEN
            TD = (1.-XX)*RTM1%TATM(  1)
         END IF
         TUD(1:NBV) = TD*TUD(1:NBV)
!XH      considering atmosphere surface coupling using atmosphere correction
!PL      AC RT for isotropic or anysotropic surface defined by flag "surf_iso"
!PL      "surf_albedo" flag defines the approach for isotropic surface
         surf_iso=.false. !.true.
         If (surf_iso) then

!PL Flag to switch between surface and aerosol correction
            surf_cor=.true.
            if (surf_cor) then
!PL test1
!                SL(1:NBV)  = SL(1:NBV) + SL1(1:NBV)                        &
!                     +RL1(1:NBV)*(1.-SL1(1:NBV)/RMUS)*(1.-SL1(1:NBV)/RMUS)/(1.-RL1(1:NBV)/RMUS*SL1(1:NBV)/RMUS)
!PL test2
!                SL(1:NBV)  = SL(1:NBV) + SL1(1:NBV)                        &
!                     +RMUS*SALB*(1.-SL1(1:NBV)/RMUS)*(1.-SL1(1:NBV)/RMUS)/(1.-RMUS*SALB*SL1(1:NBV)/RMUS)

!PL test3
!                SL(1:NBV)  = SL(1:NBV) + SL1(1:NBV)                        &
!                     +RMUS*x_vect_land(1)*(1.-SL1(1:NBV)/RMUS)*(1.-SL1(1:NBV)/RMUS)/(1.-RMUS*x_vect_land(1)*SL1(1:NBV)/RMUS)

!PL test1_2
!PL                abs_total=1.-(sum((EXT(1:NM1+ngas)*SSA(1:NM1+ngas))))/sum(EXT(1:NM1+ngas))
!PL test1_3
               abs_total=1.-(sum((EXT(1:NM)*SSA(1:NM))))/sum(EXT(1:NM))

               SL(1:NBV)  = SL(1:NBV) + SL1(1:NBV)                        &
                  +RL1(1:NBV)*(1.-SL1(1:NBV)/RMUS-abs_total)*(1.-SL1(1:NBV)/RMUS-abs_total)/(1.-RL1(1:NBV)/RMUS*SL1(1:NBV)/RMUS)

!PL x_vect
            else

!PL Flag to switch between surface albedo and surface reflectance
               surf_albedo=.true.
               if (surf_albedo) then
                  SL(1:NBV)  = SL(1:NBV) + SL1(1:NBV)                        &
                     +RMUS*SALB*TUD(1:NBV)/(1.-SALB*RTM1%AATM)
               else
                  SL(1:NBV)  = SL(1:NBV) + SL1(1:NBV)                        &
                     +RL1(1:NBV)*TUD(1:NBV)/(1.-RL1(1:NBV)/RMUS*RTM1%AATM)
               endif

            endif



         else
            SL(1:NBV)  = SL(1:NBV) + SL1(1:NBV)                            &
               +(RL1(1:NBV)-RMUS*SALB)*AT(1:NBV)                       &
               +RMUS*SALB*TUD(1:NBV)/(1.-SALB*RTM1%AATM)
         endif

      else

         SL(1:NBV)  = SL(1:NBV) + SL1(1:NBV) + RL1(1:NBV)*AT(1:NBV)

         IF(ATMOS_EMIS) THEN

            SL(1:NBV)  = SL(1:NBV) + ((1-SALB)*SBT*AT_EMIS(1:NBV))

         END IF


      end if

      do iv=1,NBV
         SLout(iv)=SL(iv)

!MH Solar direct Radiance

!            xx=cos(vis(iv)*rad_sp)
!            if(thd(iv).lt.3) then
!                  SLout(iv)=SL(iv) + (E0*exp(-(EXTH(NT))/xx))
!            else
!                   SLout(iv)=SL(iv)
!            end if



!         IF (thd(iv).EQ.0) THEN
!!PL      Extinction in  the exact forward direction
!         if (ABS(thd(iv)) .lt. 0.05) then
!            DOMEGA=-2.0*pi_sp*(COS(0.6*rad_sp)-COS(0.0))
!!PL         This is an absolute flux  for F0=pi
!            SLout(iv)=pi*exp(-tevtot/rmus)+DOMEGA*SL(iv)
!!PL         This is TAU measured by sunphotometer
!            write(*,*) tevtot,'tevtot'
!!PL          SLout(iv)=-log(SLout(iv)/pi)*rmus
!         end if
      end do ! iv
!write(*,*) SLout(1:NBV)
!stop
      if (RT_SOS_CTL%IVEC) then
!XH      considering linear polarization
!PL      Stokes vector in the scattering plane (SL, SQ)
         SQ(1:NBV)=SQ(1:NBV)+SQ1(1:NBV)+RQ1(1:NBV)*AT(1:NBV)

!PL The rotation back to meridian plan.
         do iv=1,NBV
!           Original
            cc=cos(2.0*chi(iv))
!           Original
            ss=sin(2.0*chi(iv))
!PL         Calculation of Q and U Stokes parameters in meridional plane
!           Original
            SQos(iv)=cc*SQ(iv)-ss*SU(iv)
!           Original
            SUos(iv)=ss*SQ(iv)+cc*SU(iv)

!PL         del
!		     SQos(iv)=cc*SQ(iv)-ss*SU(iv)
!		     SUos(iv)=ss*SQ(iv)+cc*SU(iv)
!PL
!            cc=-cos(2.*chi(iv))
!            ss=-sin(2.*chi(iv))
!PL
!            cc=cos_2etv(iv)
!PL
!            ss=sin_2etv(iv)
!PL         Calculation of Q and U Stokes parameters in meridional plane
!            SQos(iv)=-cc*SQ(iv)+ss*SU(iv)
!            SUos(iv)=ss*SQ(iv)+cc*SU(iv)

            xx=SQ(iv)
            yy=SU(iv)
            SLP(iv)=sqrt(xx*xx+yy*yy)
            SLPsig(iv)=SLP(iv)
            if (SQ(iv) .lt. 0.0) then
               SLPsig(iv)=-SLP(iv)
            endif
         end do ! iv

         if (RT_SOS_SET%ISCA) then
!           out is scattering plane
            do iv=1,NBV
               SQout(iv)=SQ(iv)
               SUout(iv)=SU(iv)
               SLPout(iv)=SLPsig(iv)
            end do
         else
!           out is meridian plane
            do iv=1,NBV
               SQout(iv)=SQos(iv)
               SUout(iv)=SUos(iv)
               SLPout(iv)=SLPsig(iv)
            end do
         end if
      end if

!XH   interpolate flux to the user-defined levels
      IF (RT_SOS_CTL%IFLX) THEN
         DO IV = 1, NLV
            IF (HLV(IV) .GE. HLV0(1)) THEN
               UFX(IV)=UFX0(1)
               DFX(IV)=DFX0(1)
            ELSE IF (HLV(IV) .LE. HLV0(NT)) THEN
               UFX(IV)=UFX0(NT)
               DFX(IV)=DFX0(NT)
            ELSE
               DO J=2,NT
                  IF (HLV(IV) .EQ. HLV0(J-1)) THEN
                     UFX(IV)=UFX0(J-1)
                     DFX(IV)=DFX0(J-1)
                     EXIT
                  ELSE IF (HLV(IV) .EQ. HLV0(J)) THEN
                     UFX(IV)=UFX0(J)
                     DFX(IV)=DFX0(J)
                     EXIT
                  ELSE IF (HLV(IV) .LT. HLV0(J-1) .AND. HLV(IV) .GT. HLV0(J)) THEN
!XH                  interpolation in linear scale
                     !FF = (HLV(IV)-HLV0(J))/(HLV0(J-1)-HLV0(J))
!XH                  interpolation in logrithmic scale
                     FF = LOG((HLV(IV)+1.)/(HLV0(J)+1.))/LOG((HLV0(J-1)+1.)/(HLV0(J)+1.))
                     UFX(IV)=UFX0(J)+FF*(UFX0(J-1)-UFX0(J))
                     DFX(IV)=DFX0(J)+FF*(DFX0(J-1)-DFX0(J))
                     EXIT
                  END IF
               END DO
            END IF
         END DO
      END IF

!
!write(*,*) SLout(1:NBV)
!      stop
      RETURN
   END SUBROUTINE radiative_transfer_SOS

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   subroutine surface1 (n_el,NBV,iBRDF,iBPDF,iBRM_water,land_percent,WAVE,        & ! IN
      rmus,vis,fiv,n_par,n_par_i,x_vect_land,                   &
      n_par_water,x_vect_water,                                 &
      RL1,RQ1                                                   & ! OUT
      )
!XH   optional parameters for vector case
      use mod_par_OS, only : NBVM
      implicit none

!	------------------------------------------------------------------------------------
!   IN:
!XH   add variable n_el to determine dimension of Rip for scalar and vector cases
      integer,                          intent(in)    ::  NBV,iBRDF,iBPDF,iBRM_water,n_el
      real,dimension(2*NBVM),           intent(in)    ::  vis,fiv
      real,                             intent(in)    ::  land_percent,rmus,WAVE
      integer,                          intent(in)    ::  n_par,n_par_i,n_par_water
      Real(8),                          intent(in)    ::  x_vect_land(1:n_par),x_vect_water(1:n_par_water)
!	------------------------------------------------------------------------------------
!   OUT:
      real,dimension(2*NBVM),           intent(out)   ::  RL1,RQ1
!	------------------------------------------------------------------------------------
!   LOCAL:
      Real(8)  ::  Rip_land(1:n_el,1:n_el),Rip(1:n_el,1:n_el)
      real     ::  xxx!, AT
      integer  ::  j
!	------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------

      do j=1,NBV
         xxx=cos(vis(j)*rad_sp)
!         if (xxx .ge. 0.0) then ! do not delete
!            AT=exp(-EXTH(NT)/rmus)*exp(-(EXTH(NT)-EXTH(NAV))/xxx)
         select case(nint(land_percent))
          case(0)
            call BRM_ocean(n_el,iBRM_water,n_par_water,dble(rmus),dble(xxx),dble(fiv(j)),dble(WAVE), &
               x_vect_water(1:n_par_water),Rip)
          case(100)
            call BRM(n_el,iBRDF,iBPDF,n_par,n_par_i,dble(rmus),dble(xxx),dble(fiv(j)),dble(WAVE),    &
               x_vect_land(1:n_par),Rip)
          case default
            call BRM_ocean(n_el,iBRM_water,n_par_water,dble(rmus),dble(xxx),dble(fiv(j)),dble(WAVE),   &
               x_vect_water(1:n_par_water),Rip)
            call BRM(n_el,iBRDF,iBPDF,n_par,n_par_i,dble(rmus),dble(xxx),dble(fiv(j)),dble(WAVE),      &
               x_vect_land(1:n_par),Rip_land)
            Rip=(1. - 0.01*land_percent)*Rip+0.01*land_percent*Rip_land
!            write(*,*) 'land_percent=',land_percent,'  - unknown'
!            stop 'stop in MOD_RT_SOS'
         end select ! land_percent
!         RQ1(j)=AT*rmus*dsqrt(Rip(2,1)*Rip(2,1)+Rip(3,1)*Rip(3,1))
!	      RL1(j)=AT*rmus*Rip(1,1)
         RL1(j)=rmus*Rip(1,1)
!XH      considering linear polarization
         if (n_el .eq. 3) RQ1(j)=rmus*dsqrt(Rip(2,1)*Rip(2,1)+Rip(3,1)*Rip(3,1))
!		  Write(*,*) 'surface 1: ',fiv(j), RL1(j),rmus*Rip(2,1),rmus*Rip(3,1)
!         end if ! xxx .ge. 0.0 ! do not delete
      end do ! j
!
      return
   end subroutine surface1


!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


   subroutine source_func_T_level(IVEC,          & ! IN
      IDWN,            &
      lsurface,        &
      ATMOS_EMIS,      &
      SOLAR_EMIS,      &
      NM,NT,NBV,NAV,   &
      NANG1,ANGL1,     &
      rmus,thd,vis,    &
      EXTH,WD,         &
      BETAM,GAMMM,     &
      PF11,PF12,              &
      T_level, STEMP, WAVE,   &
      index_T, tauH_TOT_int,  &
      T_profile, NH,          &
      SL1,SQ1,BT,SBT,E0       & ! OUT
      )

      use mod_par_OS, only : NG0T,KSD,NBVM,KNT,NMM,NMG,KVERT_WD
      use sub_gas_kd, ONLY : NLEVEL_GAS
      implicit none

      integer, parameter  ::  KANG=2*NG0T+1

      !    ------------------------------------------------------------------------------------
      ! IN:
      logical,                          intent(in)  ::  IVEC,IDWN,lsurface,ATMOS_EMIS,SOLAR_EMIS
      integer,                          intent(in)  ::  NM,NT,NBV,NAV
      integer,                          intent(in)  ::  NANG1
      real,dimension(KANG),             intent(in)  ::  ANGL1
      real,dimension(KNT-1,NMM+NMG),    intent(in)  ::  WD
      real,dimension(KNT),              intent(in)  ::  EXTH
      real,dimension(2*NBVM),           intent(in)  ::  thd,vis
      real,                             intent(in)  ::  rmus,BETAM,GAMMM
      real,                             intent(in)  ::  WAVE
      real,dimension(KNT),              intent(in)  ::  T_level
      real,                             intent(in)  ::  STEMP
      real,dimension(KANG,KSD),         intent(in)  ::  PF11,PF12
      integer,dimension(KNT),           intent(in)  ::  index_T
      real,dimension(KVERT_WD),         intent(in)  ::  tauH_TOT_int
      real,dimension(NLEVEL_GAS),       intent(in)  ::  T_profile
      integer,                          intent(in)  ::  NH

      !    ------------------------------------------------------------------------------------
      !   LOCAL:
      REAL, PARAMETER     :: pi = 3.141592654
      REAL                :: Solar_FOV, sum_dtau
      INTEGER             :: I, i_tau
      real,dimension(KNT) :: BT_level
      real,dimension(NLEVEL_GAS) :: BT_profile
      !    ------------------------------------------------------------------------------------
      ! OUT:
      real,dimension(2*NBVM),           intent(out) ::  SL1,SQ1
      real,dimension(KNT),              intent(out) ::  BT
      real,                             intent(out) ::  SBT
      real,                             intent(out) ::  E0
      !    ------------------------------------------------------------------------------------

      IF(ATMOS_EMIS)THEN

         !MH    Planck Calculation for Atmospheric levels
         DO I=1, NT
            call planck(WAVE,T_level(I),    & ! IN
               BT_level(I)         & ! OUT
               )
         END DO

!    MH linear Planck variation between levels like in RTTOV
!            DO I=1, NT-1
!                    BT(I) = BT_level(I) + (BT_level(I+1) - BT_level(I))*(EXTH(I)/(EXTH(I+1)-EXTH(I)))
!            END DO
!    MH Full linear Planck variation between levels like in DISORT
!            DO I=1, NT-1
!                    BT(I) = (BT_level(I+1) + BT_level(I))/2
!            END DO

         DO I=1, NH
            call planck(WAVE,T_profile(I),    & ! IN
               BT_profile(I)         & ! OUT
               )
         END DO

         do I=1,NT-1

            sum_dtau = 0.0
            BT(I) = 0.0

            if(ABS(index_T(I+1) - index_T(I)) .gt. 1)then

               do i_tau=index_T(I),index_T(I+1)
                  BT(I) = BT(I) + (BT_profile(i_tau)*(tauH_TOT_int(i_tau) - tauH_TOT_int(i_tau-1)))
                  sum_dtau = sum_dtau + (tauH_TOT_int(i_tau) - tauH_TOT_int(i_tau-1))
               end do

               BT(I) = BT(I)/sum_dtau

            else
               BT(I) = (BT_level(I) + BT_level(I+1))/2
            end if

         end do


         IF(lsurface)THEN
            !MH    Planck Calculation for Surface
            call planck(WAVE,STEMP,         & ! IN
               SBT                 & ! OUT
               )
         END IF

         !MH    Planck Calculation for Sun, in future maybe this will be substitued by a real solar Spectrum
         !MH    The input radiance for GRASP has to be Irradiance/pi
         IF(SOLAR_EMIS) THEN

            Solar_FOV = 0.53
            call planck_IRR(WAVE,5250.0,Solar_FOV,     & ! IN
               E0               & ! OUT
               )

         ELSE

            !MH Under these conditions we assume that the solar radiation is negligible
            E0 = 0.0

         END IF


         call aerosol_EMIS(IVEC, & ! IN
            IDWN, &
            NM,NT,NBV,NAV,   &
            NANG1,ANGL1,     &
            rmus,thd,vis,    &
            EXTH,WD,         &
            BETAM,GAMMM,     &
            PF11,PF12,       &
            BT,E0,           &

            SL1,SQ1          & ! OUT
            )

      ELSE

         !MH No planck emission is considered
         BT(:) = -1

         !MH For the visible case we keep the radiances normalized
         E0 = 1.0


         call aerosol1 (IVEC, & ! IN
            IDWN, &
            NM,NT,NBV,NAV,   &
            NANG1,ANGL1,     &
            rmus,thd,vis,    &
            EXTH,WD,         &
            BETAM,GAMMM,     &
            PF11,PF12,       &

            SL1,SQ1          & ! OUT
            )

      END IF

   end subroutine source_func_T_level





   subroutine source_func(IVEC,          & ! IN
      IDWN,            &
      lsurface,        &
      ATMOS_EMIS,      &
      SOLAR_EMIS,      &
      NM,NT,NBV,NAV,   &
      NANG1,ANGL1,     &
      rmus,thd,vis,    &
      EXTH,WD,         &
      BETAM,GAMMM,     &
      PF11,PF12,		  &
      T_layer, STEMP, WAVE,   &

      SL1,SQ1,BT,SBT,E0    & ! OUT
      )

      use mod_par_OS, only : NG0T,KSD,NBVM,KNT,NMM,NMG
      implicit none

      integer, parameter  ::  KANG=2*NG0T+1

!	------------------------------------------------------------------------------------
! IN:
      logical,                          intent(in)  ::  IVEC,IDWN,lsurface,ATMOS_EMIS,SOLAR_EMIS
      integer,                          intent(in)  ::  NM,NT,NBV,NAV
      integer,                          intent(in)  ::  NANG1
      real,dimension(KANG),             intent(in)  ::  ANGL1
      real,dimension(KNT-1,NMM+NMG),    intent(in)  ::  WD
      real,dimension(KNT),              intent(in)  ::  EXTH
      real,dimension(2*NBVM),           intent(in)  ::  thd,vis
      real,                             intent(in)  ::  rmus,BETAM,GAMMM
      real,                             intent(in)  ::  WAVE
      real,dimension(KNT-1),            intent(in)  ::  T_layer
      real,                             intent(in)  ::  STEMP
      real,dimension(KANG,KSD),         intent(in)  ::  PF11,PF12

!	------------------------------------------------------------------------------------
!   LOCAL:
      REAL, PARAMETER :: pi = 3.141592654
      REAL            :: Solar_FOV
      INTEGER         :: I
!	------------------------------------------------------------------------------------
! OUT:
      real,dimension(2*NBVM),           intent(out) ::  SL1,SQ1
      real,dimension(KNT),              intent(out) ::  BT
      real,                             intent(out) ::  SBT
      real,                             intent(out) ::  E0
!	------------------------------------------------------------------------------------

      IF(ATMOS_EMIS)THEN

!MH    Planck Calculation for Atmospheric layers
!write(*,*) T_layer(1:NT-1)
!stop
         DO I=1, NT-1
            call planck(WAVE,T_layer(I),    & ! IN
               BT(I)	            & ! OUT
               )
         END DO

         IF(lsurface)THEN
!MH    Planck Calculation for Surface
            call planck(WAVE,STEMP,         & ! IN
               SBT                 & ! OUT
               )
         END IF

!MH    Planck Calculation for Sun, in future maybe this will be substitued by a real solar Spectrum
!MH    The input radiance for GRASP has to be Irradiance/pi
         IF(SOLAR_EMIS) THEN

            Solar_FOV = 0.53
            call planck_IRR(WAVE,5250.0,Solar_FOV,     & ! IN
               E0               & ! OUT
               )

         ELSE

            !MH Under these conditions we assume that the solar radiation is negligible
            E0 = 0.0

         END IF


         call aerosol_EMIS(IVEC, & ! IN
            IDWN, &
            NM,NT,NBV,NAV,   &
            NANG1,ANGL1,     &
            rmus,thd,vis,    &
            EXTH,WD,         &
            BETAM,GAMMM,     &
            PF11,PF12,       &
            BT,E0,           &

            SL1,SQ1          & ! OUT
            )

      ELSE

         !MH No planck emission is considered
         BT(:) = -1

         !MH For the visible case we keep the radiances normalized
         E0 = 1.0


         call aerosol1 (IVEC, & ! IN
            IDWN, &
            NM,NT,NBV,NAV,   &
            NANG1,ANGL1,     &
            rmus,thd,vis,    &
            EXTH,WD,         &
            BETAM,GAMMM,     &
            PF11,PF12,       &

            SL1,SQ1          & ! OUT
            )

      END IF

   end subroutine source_func


!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   subroutine planck(WAVE,T, & ! IN

      BT	  & ! OUT
      )

!	------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------

      implicit none

      REAL, PARAMETER :: c = 3e8  ! [m/s]
      REAL, PARAMETER :: h = 6.62607004e-34 ! m2 kg / s
      REAL, PARAMETER :: k = 1.38064852e-23 ! m2 kg s-2 K-1
      REAL, PARAMETER :: pi = 3.141592654

!	------------------------------------------------------------------------------------
! IN:

      REAL,                          intent(in) :: WAVE
      REAL,                          intent(in) :: T

!	------------------------------------------------------------------------------------
! OUT:

      real,                           intent(out) :: BT

!	------------------------------------------------------------------------------------
!   LOCAL:

!	------------------------------------------------------------------------------------

      ! np = (2*pi/(WAVE*(1e-6)))
      ! ns = (1/(WAVE*(1e-6)))

      BT = (2*h*c**2)*(1e-6)/((WAVE*(1e-6))**5) * (1/(exp((h*c)/(WAVE*(1e-6)*k*T))-1))
!    write(*,*)BT
   end subroutine planck

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

!    sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   subroutine planck_IRR(WAVE,T,FOV, & ! IN

      BT      & ! OUT
      )

!    ------------------------------------------------------------------------------------
!    ------------------------------------------------------------------------------------

      implicit none

      REAL, PARAMETER :: c = 3e8  ! [m/s]
      REAL, PARAMETER :: h = 6.62607004e-34 ! m2 kg / s
      REAL, PARAMETER :: k = 1.38064852e-23 ! m2 kg s-2 K-1
      REAL, PARAMETER :: pi = 3.141592654

!    ------------------------------------------------------------------------------------
! IN:

      REAL,                          intent(in) :: WAVE
      REAL,                          intent(in) :: T, FOV

!    ------------------------------------------------------------------------------------
! OUT:

      real,                           intent(out) :: BT

!    ------------------------------------------------------------------------------------
!   LOCAL:

!    ------------------------------------------------------------------------------------

!write(*,*) (2*h*c**2)*(1e-6)*pi*(1 - cos(FOV/2*(pi/180))**2)/((WAVE*(1e-6))**5) * (1/(exp((h*c)/(WAVE*(1e-6)*k*T))-1))

!MH Solar Irradiance divided by PI
      BT = (2*h*c**2)*(1e-6)*pi*(1 - cos(FOV/2*(pi/180))**2)/((WAVE*(1e-6))**5) * (1/(exp((h*c)/(WAVE*(1e-6)*k*T))-1))/pi

   end subroutine planck_IRR

   subroutine aerosol_EMIS (IVEC,IDWN,       & ! IN
      NM,NT,NBV,NAV,   &
      NANG1,ANGL1,     &
      rmus,thd,vis,    &
      EXTH,WD,         &
      betm,gamm,       &
      PF11,PF12,       &
      BT,E0,              &

      SL1,SQ1          & ! OUT
      )

!	------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------
      use mod_par_OS, only : NG0T,KSD,NBVM,KNT,NMM,NMG
      use mod_intrpl_linear
!      use mod_intrpl_spline

      implicit none

      integer, parameter  ::  KANG=2*NG0T+1

!	------------------------------------------------------------------------------------
! IN:
      logical,                          intent(in)  ::  IVEC,IDWN
      integer,                          intent(in)  ::  NM,NT,NBV,NAV
      integer,                          intent(in)  ::  NANG1
      real,dimension(KANG),             intent(in)  ::  ANGL1
      real,dimension(KNT-1,NMM+NMG),    intent(in)  ::  WD
      real,dimension(KNT),              intent(in)  ::  EXTH
      real,dimension(2*NBVM),           intent(in)  ::  thd,vis
      real,                             intent(in)  ::  rmus,betm,gamm, E0
      real,dimension(KANG,KSD),         intent(in)  ::  PF11,PF12
      real,dimension(KNT),              intent(in)  ::  BT
!	------------------------------------------------------------------------------------
! OUT:
      real,dimension(2*NBVM),           intent(out) ::  SL1,SQ1
!	------------------------------------------------------------------------------------
! LOCAL:
      real,dimension(NM+1)       ::  f11,f12
      integer                    ::  j,m,n, i1
      real                       ::  xx,xi,xp,xxx,YY,WW,VV, Temis, SolarScat, aux_ext
!	------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------
!write(*,*)
!write(*,*) 'NM,NT,NBV,NAV,NANG1:'
!write(*,*) NM,NT,NBV,NAV,NANG1
!write(*,*) 'ANGL1:'
!write(*,'(10e16.6)') ANGL1
!write(*,*) 'PF11:'
!write(*,'(10e16.6)') PF11
!write(*,*) 'PF12:'
!write(*,'(10e16.6)') PF12
!write(*,*) 'rmus:'
!write(*,'(10e16.6)') rmus
!write(*,*) 'thd:'
!write(*,'(10e16.6)') thd
!write(*,*) 'vis:'
!write(*,'(10e16.6)') vis
!write(*,*) 'EXTH:'
!write(*,'(10e16.6)') EXTH
!write(*,*) 'WD:'
!do j=1,NMM+NMG
!write(*,'(10e16.6)') WD(1:KNT-1,j)
!write(*,*)
!enddo
!write(*,*) 'betm:'
!write(*,'(e16.6)') betm
!write(*,*) 'gamm:'
!write(*,'(e16.6)') gamm

!write(*,*)
!PL   Single scattering + surface (begin)

      SL1(:)=0.
      if (IVEC) SQ1(:)=0.
      do j=1,NBV
         xxx=cos(vis(j)*rad_sp)
         ! write(*,*) vis(j)
!PL      xx is recalculated several times!!!
         xx=cos(thd(j)*rad_sp)
!PL      f11(j,NM+1) - Reyleigh scattering for viewing geometry
         f11(NM+1)=1.0+betm*(3.*xx*xx-1.)*0.5
!MH 	 In the calculation of thd is converted from viewing geometry to the corresponding  scattering angle
         do m=1,NM
!PL         Interpolation for viewing geometry
            f11(m)=LINEAR_LN(ANGL1(1:NANG1),PF11(1:NANG1,m),NANG1,thd(j))
         end do ! m

!XH      for vector case
         if (IVEC) then
            f12(NM+1)=sqrt(3./8.)*(1.-xx*xx)*gamm
            do m=1,NM
               f12(m)=LINEAR(ANGL1(1:NANG1),PF12(1:NANG1,m),NANG1,thd(j))
            end do ! m
         end if
         aux_ext = 0

!stop


         if (xxx .gt. 0.0 .and. NAV .lt. NT) then
!PL         For airborne and satellite observation
            do n=NAV,NT-1
!MH            Emission without scattering
               XX=exp(-(EXTH(n)-EXTH(NAV))/xxx)
               YY=1-exp(-(EXTH(n+1)-EXTH(n))/xxx)
               xi=BT(n)*(1-SUM(WD(n,1:NM+1)))

               Temis = XX*YY*xi

               WW=(EXTH(n+1)-EXTH(n))*(1./rmus+1./xxx)
               XX=(1.0-exp(-WW))/(1.0+xxx/rmus)
               YY=exp(-EXTH(n)/rmus)*exp(-(EXTH(n)-EXTH(NAV))/xxx)
!PL            Single aerosol+Rayleigh scattering
               xi=dot_product(f11,WD(n,1:NM+1))

               SolarScat=XX*YY*xi*0.25*E0

               SL1(j)=SL1(j)+SolarScat+Temis

!XH            for vector case
               if (IVEC) then
                  xp=dot_product(f12,WD(n,1:NM+1))
                  SQ1(j)=SQ1(j)-XX*YY*xp*0.25*E0
               end if
            end do ! n
         end if ! xxx .gt. 0.0 .and. NAV .lt. NT


         if (xxx .lt. 0.0) then

!PL         For ground based observation
            do n=1,NT-1
!MH            Atmospheric Emission Calculation
               XX=exp(-(EXTH(n+1)-EXTH(NT))/xxx)
               YY=1-exp(-(EXTH(n)-EXTH(n+1))/xxx)
               xi=BT(n)*(1-SUM(WD(n,1:NM+1)))
               Temis = XX*YY*xi
!MH            Solar Scattering Calculation

               WW=(EXTH(n+1)-EXTH(n))*(1./rmus+1./xxx)
               XX=(1.0-exp(-WW))/(1.0+xxx/rmus)
               VV=(EXTH(n+1)-EXTH(n))/xxx
               if (abs(rmus+xxx) .lt. 0.001) XX=(EXTH(n+1)-EXTH(n))/xxx
               YY=-exp(-EXTH(n)/rmus)*exp((EXTH(NT)-EXTH(n+1))/xxx)
               XX=XX*exp(VV)
!PL            Single aerosol+Reyleigh scattering
               xi=dot_product(f11,WD(n,1:NM+1))
               SolarScat=XX*YY*xi*0.25*E0

               SL1(j)=SL1(j)+Temis+SolarScat

!XH            for vector case
               if (IVEC) then
                  xp=dot_product(f12,WD(n,1:NM+1))
                  SQ1(j)=SQ1(j)-XX*YY*xp*0.25*E0
               end if
!PL            The sign changes: -XX*YY*xp*0.25. So SQ1 for Reyleigh is > 0 !!!
!PL            Because of this cs (ANGTURN) is oposit !!!!!
            end do ! n


         end if ! xxx .lt. 0.0

      end do ! j


      return

   end subroutine aerosol_EMIS



!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   subroutine aerosol1 (IVEC,IDWN,       & ! IN
      NM,NT,NBV,NAV,   &
      NANG1,ANGL1,     &
      rmus,thd,vis,    &
      EXTH,WD,         &
      betm,gamm,       &
      PF11,PF12,       &

      SL1,SQ1          & ! OUT
      )

!	------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------
      use mod_par_OS, only : NG0T,KSD,NBVM,KNT,NMM,NMG
      use mod_intrpl_linear
!      use mod_intrpl_spline

      implicit none

      integer, parameter  ::  KANG=2*NG0T+1

!	------------------------------------------------------------------------------------
! IN:
      logical,                          intent(in)  ::  IVEC,IDWN
      integer,                          intent(in)  ::  NM,NT,NBV,NAV
      integer,                          intent(in)  ::  NANG1
      real,dimension(KANG),             intent(in)  ::  ANGL1
      real,dimension(KNT-1,NMM+NMG),    intent(in)  ::  WD
      real,dimension(KNT),              intent(in)  ::  EXTH
      real,dimension(2*NBVM),           intent(in)  ::  thd,vis
      real,                             intent(in)  ::  rmus,betm,gamm
      real,dimension(KANG,KSD),         intent(in)  ::  PF11,PF12
!	------------------------------------------------------------------------------------
! OUT:
      real,dimension(2*NBVM),           intent(out) ::  SL1,SQ1
!	------------------------------------------------------------------------------------
! LOCAL:
      real,dimension(NM+1)       ::  f11,f12
      integer                    ::  j,m,n
      real                       ::  xx,xi,xp,xxx,YY,WW,VV
!	------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------

!write(*,*)
!PL   Single scattering + surface (begin)
      SL1(:)=0.

      if (IVEC) SQ1(:)=0.
      do j=1,NBV
         xxx=cos(vis(j)*rad_sp)
!PL      xx is recalculated several times!!!
         xx=cos(thd(j)*rad_sp)
!PL      f11(j,NM+1) - Rayleigh scattering for viewing geometry
         f11(NM+1)=1.0+betm*(3.*xx*xx-1.)*0.5
         do m=1,NM
!PL         Interpolation for viewing geometry
!MH 	 In the calculation of thd is converted from viewing geometry to the corresponding  scattering angle
            f11(m)=LINEAR_LN(ANGL1(1:NANG1),PF11(1:NANG1,m),NANG1,thd(j))
         end do ! m
!XH      for vector case
         if (IVEC) then
            f12(NM+1)=sqrt(3./8.)*(1.-xx*xx)*gamm
            do m=1,NM
               f12(m)=LINEAR(ANGL1(1:NANG1),PF12(1:NANG1,m),NANG1,thd(j))
            end do ! m
         end if

         if (xxx .gt. 0.0 .and. NAV .lt. NT) then
!PL         For airborne and satellite observation
            do n=NAV,NT-1
               WW=(EXTH(n+1)-EXTH(n))*(1./rmus+1./xxx)
               XX=(1.0-exp(-WW))/(1.0+xxx/rmus)
               YY=exp(-EXTH(n)/rmus)*exp(-(EXTH(n)-EXTH(NAV))/xxx)
!PL            Single aerosol+Reyleigh scattering
               xi=dot_product(f11,WD(n,1:NM+1))

               SL1(j)=SL1(j)+XX*YY*xi*0.25

!XH            for vector case
               if (IVEC) then
                  xp=dot_product(f12,WD(n,1:NM+1))
                  SQ1(j)=SQ1(j)-XX*YY*xp*0.25
               end if
            end do ! n
         end if ! xxx .gt. 0.0 .and. NAV .lt. NT


         if (xxx .lt. 0.0) then

!PL         For ground based observation
!PL		(Notes for future developments)
!PL 	Potentially, taking instead of NT NAV the upward observation
!PL		(AERONET like) can be simulated at the level NAV

            do n=1,NT-1
               WW=(EXTH(n+1)-EXTH(n))*(1./rmus+1./xxx)
               XX=(1.0-exp(-WW))/(1.0+xxx/rmus)
!AL            inserted correction xxx instead of xxx (typo?)
!	            W=(EXTH(n+1)-EXTH(n))/xxx
               VV=(EXTH(n+1)-EXTH(n))/xxx
!c             CORRECTION ALMUCANTAR
!c             Erreur 1 corrigee le 28 mai 2009
!c              if(rmus+xxx.lt.0.001) XX=(EXTH(n+1)-EXTH(n))/xxx
               if (abs(rmus+xxx) .lt. 0.001) XX=(EXTH(n+1)-EXTH(n))/xxx
!c             Erreur 2 corrigee le 28 mai 2009
!c              YY=-exp(-EXTH(n)/rmus)*exp((EXTH(NT)-EXTH(n))/xxx)
               YY=-exp(-EXTH(n)/rmus)*exp((EXTH(NT)-EXTH(n+1))/xxx)
!AL            inserted correction
               XX=XX*exp(VV)
!PL            Single aerosol+Reyleigh scattering
               xi=dot_product(f11,WD(n,1:NM+1))
               !SL1(j)=SL1(j)+XX*YY*xi*0.25
               SL1(j)=SL1(j) + (XX*YY*xi*0.25)
!XH            for vector case
               if (IVEC) then
                  xp=dot_product(f12,WD(n,1:NM+1))
                  SQ1(j)=SQ1(j)-XX*YY*xp*0.25
               end if
!PL            The sign changes: -XX*YY*xp*0.25. So SQ1 for Reyleigh is > 0 !!!
!PL            Because of this cs (ANGTURN) is oposit !!!!!
            end do ! n
         end if ! xxx .lt. 0.0
      end do ! j

      return
   end subroutine aerosol1

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   SUBROUTINE phase_matrix_intrpl_NQDR (                                 &
      IVEC,                          & ! IN
      NM,NQDR,                       &
      zmu,zg,                        &
      KMpar,NANG,ANGL,               &
      PF11_I,PF12_I,PF22_I,PF33_I,   &

      NANG1,ANGL1,                   & ! OUT
      PF11,PF12,PF22,PF33            &
      )

!	------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------
      use mod_par_OS, only : NG0T,KSD
      use mod_intrpl_spline

      implicit none

      integer, parameter  ::  KANG=2*NG0T+1
!	------------------------------------------------------------------------------------
! IN :
      logical,                           intent(in)  ::  IVEC
      integer,                           intent(in)  ::  NM,NQDR,NANG,KMpar
      real,dimension(KMpar),             intent(in)  ::  ANGL
      real,dimension(KMpar,KSD),         intent(in)  ::  PF11_I,PF12_I,PF22_I,PF33_I
      REAL*8,DIMENSION(-NG0T:NG0T),      intent(in)  ::  zmu,zg

!	--------------------------------------------------------------------------------------
! OUT :
      integer,                          intent(out)  ::  NANG1
      REAL,DIMENSION(KANG),             intent(out)  ::  ANGL1
      REAL,DIMENSION(KANG,KSD),         intent(out)  ::  PF11,PF12,PF22,PF33

!	--------------------------------------------------------------------------------------
! LOCAL :
      DOUBLE PRECISION :: XARG, YFIT
      DOUBLE PRECISION, DIMENSION(NANG) :: XS, YS, b, c, d
      REAL ::  zz0
      INTEGER :: I,j,k,ISD
!	------------------------------------------------------------------------------------------------------
!	-------------------------------------------------------------------------------------------------------
      NANG1=0
      ANGL1(:) =0.0
      PF11(:,:)=0.0
      if (IVEC) then
         PF12(:,:)=0.0
         PF22(:,:)=0.0
         PF33(:,:)=0.0
      end if

!PL this condition is not necessary here.
!      if (NANG .gt. KANG) then
!         write(tmp_message,'(2(a,i0))') 'NANG = ',NANG,' .gt. KANG = ',KANG
!         G_ERROR(trim(tmp_message))
!      endif

!C CORRIGER D'ABORD LA MATRICE PFij?
!C Tres peu d'influence. Modifie de qqs 10-3 en relatif

!PL   ISD is aerosol component

      DO ISD=1,NM
!XH      for P11
         IF (ANGL(2).GT.ANGL(1)) THEN
            XS(1:NANG)=ANGL(1:NANG)
            YS(1:NANG)=log(PF11_I(1:NANG,ISD))
         ELSE
            XS(1:NANG)=ANGL(NANG:1:-1)
            YS(1:NANG)=log(PF11_I(NANG:1:-1,ISD))
         END IF
!PL      Calling for Phase matrix Interpolation
!PL      Pase matrix Interpolation. Result: YFIT(PFij) at
!PL      Gaussian quadratures zmu
         DO j=-NQDR,NQDR
            XARG=acos(zmu(j))*deg_sp
            IF(XARG .GT. 180.0) XARG=180.0
            ANGL1(NQDR+j+1) = XARG
            CALL intrpl_spline(NANG, XS, YS, XARG, YFIT, 1, NQDR+j+1, b, c, d)
            PF11(NQDR+j+1,ISD)=exp(YFIT)
         END DO !j
!XH      for other elements in vector case
         if (IVEC) then
!XH         for P12
            IF (ANGL(2).GT.ANGL(1)) THEN
               YS(1:NANG)=PF12_I(1:NANG,ISD)
            ELSE
               YS(1:NANG)=PF12_I(NANG:1:-1,ISD)
            END IF
!PL         Calling for Phase matrix Interpolation
!PL         Phase matrix Interpolation. Result: YFIT(PFij) at
!PL         Gaussian quadratures zmu
            DO j=-NQDR,NQDR
               XARG=acos(zmu(j))*deg_sp
               IF(XARG .GT. 180.0) XARG=180.0
               ANGL1(NQDR+j+1) = XARG
               CALL intrpl_spline(NANG, XS, YS, XARG, YFIT, 1, NQDR+j+1, b, c, d)
               PF12(NQDR+j+1,ISD)=YFIT
            END DO !j
!XH         for P22
            IF (ANGL(2).GT.ANGL(1)) THEN
               YS(1:NANG)=log(PF22_I(1:NANG,ISD))
            ELSE
               YS(1:NANG)=log(PF22_I(NANG:1:-1,ISD))
            END IF
!PL         Calling for Phase matrix Interpolation
!PL         Pase matrix Interpolation. Result: YFIT(PFij) at
!PL         Gaussian quadratures zmu
            DO j=-NQDR,NQDR
               XARG=acos(zmu(j))*deg_sp
               IF(XARG .GT. 180.0) XARG=180.0
               ANGL1(NQDR+j+1) = XARG
               CALL intrpl_spline(NANG, XS, YS, XARG, YFIT, 1, NQDR+j+1, b, c, d)
               PF22(NQDR+j+1,ISD)=exp(YFIT)
            END DO !j
!XH         for P33
            IF (ANGL(2).GT.ANGL(1)) THEN
               YS(1:NANG)=PF33_I(1:NANG,ISD)
            ELSE
               YS(1:NANG)=PF33_I(NANG:1:-1,ISD)
            END IF
!PL         Calling for Phase matrix Interpolation
!PL         Pase matrix Interpolation. Result: YFIT(PFij) at
!PL         Gaussian quadratures zmu
            DO j=-NQDR,NQDR
               XARG=acos(zmu(j))*deg_sp
               IF(XARG .GT. 180.0) XARG=180.0
               ANGL1(NQDR+j+1) = XARG
               CALL intrpl_spline(NANG, XS, YS, XARG, YFIT, 1, NQDR+j+1, b, c, d)
               PF33(NQDR+j+1,ISD)=YFIT
            END DO !j
         end if
      END DO !PL ISD

!PL   Check for phase matrix normalization.
!PL   In ideal case Phase Matrix must be normalazed is such way that zz0=1.
!PL   To insure this the renormalization is performed here
      NANG1=2*NQDR+1
      do ISD=1,NM
         zz0 = 0.5*dot_product(PF11(1:NANG1,ISD),zg(-NQDR:NQDR))
         PF11(1:NANG1,ISD)=PF11(1:NANG1,ISD)/zz0
         if (IVEC) then
            PF12(1:NANG1,ISD)=PF12(1:NANG1,ISD)/zz0
            PF22(1:NANG1,ISD)=PF22(1:NANG1,ISD)/zz0
            PF33(1:NANG1,ISD)=PF33(1:NANG1,ISD)/zz0
         end if
      end do

   END SUBROUTINE phase_matrix_intrpl_NQDR




!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   SUBROUTINE MODIF_DE_TRONC_new (                                &
      IVEC,IDWN,                     & ! IN
      NM,NQDR,                       &
      zmu,zg,NANG1,ANGL1,            &
      NBV,thd,rmus,                  &

      tetot,EXT,SSA,                 & ! INOUT
      PF11,PF12,PF22,PF33,           &

      DDL1,DDQ1                      & ! OUT
      )

!	------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------

      use mod_par_OS, only : NG0T,KSD,NBVM

      implicit none

      integer, parameter  ::  KANG=2*NG0T+1
!	------------------------------------------------------------------------------------
! IN :
      logical,                      intent(in)  ::  IVEC,IDWN
      integer,                      intent(in)  ::  NM,NQDR
      real*8,dimension(-NG0T:NG0T), intent(in)  ::  zmu,zg
      integer,                      intent(in)  ::  NBV
      real,dimension(2*NBVM),       intent(in)  ::  thd
      real,                         intent(in)  ::  rmus
      integer,                      intent(in)  ::  NANG1
      real,dimension(KANG),         intent(in)  ::  ANGL1
!	--------------------------------------------------------------------------------------
! OUT :
      real,dimension(NM),               intent(inout) ::  EXT,SSA
      real,                             intent(inout) ::  tetot
      real,dimension(KANG,KSD),         intent(inout) ::  PF11,PF12,PF22,PF33
      real,dimension(2*NBVM),           intent(out)   ::  DDL1,DDQ1
!	--------------------------------------------------------------------------------------
!	--------------------------------------------------------------------------------------
! LOCAL :
      REAL                         ::  tdav,tdav1,tevtot
      REAL,DIMENSION(KANG)         ::  P11av,P12av,P22av,P33av,Vav,Qav,Uav,Wav


!CD MODIFICATION SI TRONCATURE 0
      REAL,DIMENSION(-NG0T:NG0T)   ::  Q11,Q12,Q22,Q33

      REAL                         ::  zz0,stron,textnew,a,albnew,  & ! pi,
         ANGTRONC,b,pente,tpointe,TT,xx,yy
      INTEGER                      ::  j,k,ISD,JMAX
!	------------------------------------------------------------------------------------------------------
      REAL,DIMENSION(0:2*NG0T)      ::  alp,bet,gam,zet

      real       ::   ck,dd,ee,                   &
         p0,pp0,pp1,p1,p2,pp2,       &
         ppri,pav,                   &
         qpri,qqav,                  &
         yyy,zzz
!PL
      Integer, parameter:: N_od_tr=4
      real Yav(2*NBVM,N_od_tr),QYav(2*NBVM,N_od_tr),coeff(N_od_tr)

!	-------------------------------------------------------------------------------------------------------

      alp(:)=0
      bet(:)=0
      gam(:)=0
      zet(:)=0

      tevtot = tetot

!C    CORRIGER D'ABORD LA MATRICE PFij?
!C    Tres peu d'influence. Modifie de qqs 10-3 en relatif

!c    MODIFICATION DE TRONCATURE 3
!c    Initialiser la pointe avant

      P11av(1:NANG1)=0.0
      if (IVEC) then
         P12av(1:NANG1)=0.0
         P22av(1:NANG1)=0.0
         P33av(1:NANG1)=0.0
      endif

!c    On tronque a 16‚àû (MODIFIABLE-PARAMETRER?)
!PL   ANGTRONC is the angle of phase matrix truncation
      ANGTRONC=16.0*rad_sp

      k=NQDR
      do while(acos(zmu(k)) .lt. ANGTRONC)
         k=k-1
      enddo

      JMAX=k+NQDR+1
!PL   JMAX defines the maximum number of angles before the truncation
      tpointe=0.0
      tdav=0.0

!c    Boucle sur les modeles
!PL   additional trancated elements are added
      do ISD=1,NM
!c       Allure avant du modele
         xx=PF11(JMAX,ISD)
         yy=PF11(JMAX-1,ISD)
         pente=(log(xx)-log(yy))/(ANGL1(JMAX)-ANGL1(JMAX-1))
         b=-pente*0.5/ANGL1(JMAX)
         a=log(PF11(JMAX,ISD))+b*ANGL1(JMAX)*ANGL1(JMAX)
!c       Matrice tronquee
         do j=1,NANG1
            if(j.lt.JMAX)   TT=PF11(j,ISD)
            if(j.gt.JMAX-1) TT=exp(a-b*ANGL1(j)*ANGL1(j))
            Vav(j)=PF11(j,ISD)-TT
            if (IVEC) then
               Qav(j)=Vav(j)*PF12(j,ISD)/PF11(j,ISD)
!PL            additional trancated elements F22a,F33a
               Uav(j)=Vav(j)*PF22(j,ISD)/PF11(j,ISD)
               Wav(j)=Vav(j)*PF33(j,ISD)/PF11(j,ISD)
!PL            Truncated phase matrix
               PF12(j,ISD)=PF12(j,ISD)*TT/PF11(j,ISD)
               PF22(j,ISD)=PF22(j,ISD)*TT/PF11(j,ISD)
               PF33(j,ISD)=PF33(j,ISD)*TT/PF11(j,ISD)
            end if
            PF11(j,ISD)=TT
         enddo ! j
         !do j=1,NANG
         !   write(6,*)ANGL(j),Vav(j),Qav(j),PF11(j,ISD)
         !enddo
!        Re-normalisation
         zz0=0.5*dot_product(PF11(1:NQDR+NQDR+1,ISD),zg(-NQDR:NQDR))
         if (zz0 .ne. 1.0) then
            stron=zz0
            !write(6,*)'stron',stron
            do j=1,NANG1
               PF11(j,ISD)=PF11(j,ISD)/zz0
               Vav(j)=Vav(j)/abs(1.0-zz0)
               if (IVEC) then
                  PF12(j,ISD)=PF12(j,ISD)/zz0
                  PF22(j,ISD)=PF22(j,ISD)/zz0
                  PF33(j,ISD)=PF33(j,ISD)/zz0

                  Qav(j)=QAV(j)/abs(1.0-zz0)
                  Uav(j)=Uav(j)/abs(1.0-zz0)
                  Wav(j)=WAV(j)/abs(1.0-zz0)
               endif
            enddo ! j
         else
            stron=1.0
         endif ! zz0

         !write(6,*) "zz0=",zz0
         !do j=1,NANG1
         !   write(6,*) ANGL1(j),PF11(j,ISD),PF12(j,ISD) !,PF22(j,ISD),PF33(j,ISD)
         !enddo
!c       Correction des epaisseurs optiques abs et diff
         textnew=EXT(ISD)*(1.0-SSA(ISD)+stron*SSA(ISD))
         tdav1=EXT(ISD)*SSA(ISD)*(1.0-stron)
         albnew=stron*SSA(ISD)/(1.0-SSA(ISD)+stron*SSA(ISD))
         EXT(ISD)=textnew
         SSA(ISD)=albnew
         tpointe=tpointe+tdav1
         P11av(1:NANG1)=P11av(1:NANG1)+tdav1*Vav(1:NANG1)
         if (IVEC) then
            P12av(1:NANG1)=P12av(1:NANG1)+tdav1*Qav(1:NANG1)
            P22av(1:NANG1)=P22av(1:NANG1)+tdav1*Uav(1:NANG1)
            P33av(1:NANG1)=P33av(1:NANG1)+tdav1*Wav(1:NANG1)
         endif

         tdav=tdav+tdav1

      enddo ! ISD

      tetot=tetot-tpointe

      DDL1(:) = 0.0
      DDQ1(:) = 0.0
      if (.NOT. IDWN) RETURN
!     Ground based observations

      Q11(:)=0.0
      Q11(-NQDR:NANG1-NQDR-1)=P11av(1:NANG1)/tpointe
      if (IVEC) then
         Q12(:)=0.0
         Q22(:)=0.0
         Q33(:)=0.0
         Q12(-NQDR:NANG1-NQDR-1)=P12av(1:NANG1)/tpointe
         Q22(-NQDR:NANG1-NQDR-1)=P22av(1:NANG1)/tpointe
         Q33(-NQDR:NANG1-NQDR-1)=P33av(1:NANG1)/tpointe
      end if

!c    Correction des mesures dans la pointe tronquee
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!PL   1). It is necessary to check wether formula (6) in Moris description is correct!!!!!!!
!         ck=(exp(-tetot/rmus)-exp(-tevtot/rmus))*0.25
!PL   Expansion of the trancated peak
!PL   This expansions are initialized just for ground measurements
!PL   when cos(vis(iv))<0 (for sunphotometer)
!PL   the number of expansion coefficients is 2*NQDR-2
!PL   Therefore the tha range of arrays xalp,xbet,xgam,xzet
!PL   can be (0:2*NQDR-2) !!! The corresponding changes should be
!PL   also done in subroutine betal
!PL   new Resulting forward scattering phase functions Yav(n), Qav(n), for n=2, 3...
!PL   new scatterings in the forward peak
      call betal( IVEC,NQDR,NG0T,zmu,zg,Q11,Q12,Q22,Q33,                                  &   ! in
         alp(0:2*NG0T-2),bet(0:2*NG0T-2),gam(0:2*NG0T-2),zet(0:2*NG0T-2) )            ! out
      call CONVOL_PAV_PL( IVEC,ANGTRONC,N_od_tr,NG0T,2*NQDR-2,alp,bet,gam,                &
         2*NBVM,NBV,thd*rad_sp,Yav,QYav)

      coeff(1)=1.0
      do k=2,N_od_tr
         coeff(k)=coeff(k-1)*tdav/float(k)/rmus
      enddo !k

!PL	  Write(*,*) 'coef', coeff,CTOT
!PL   old: ck=(exp(-tetot/rmus)-exp(-tevtot/rmus))*0.25/CTOT
!PL   new
      ck=tdav*0.25/rmus*exp(-tevtot/rmus)

      do j=1,NBV
         !Write(*,*),'Yav ',thd(j),Yav(j,1),Yav(j,2),Yav(j,3),Yav(j,4)!,QYav(j,2)

!PL      Correction of I (scalar and vector case)
         pav=Yav(j,1)
         do k=2,N_od_tr
            pav=pav+Yav(j,k)*coeff(k)
         enddo !k
         DDL1(j)=pav*ck



!PL      Correction of Q (vector case)
         if (IVEC) then
            qqav=QYav(j,1)
            do k=2,N_od_tr
               qqav=qqav+QYav(j,k)*coeff(k)
            enddo !k
            DDQ1(j)=-qqav*ck
         endif
         !Write(*,*) 'coef ', coeff(1),coeff(2),coeff(3)
         !Write(*,*),'QYav ',thd(j),QYav(j,1),QYav(j,2),QYav(j,3),QYav(j,4)!,QYav(j,2)
      enddo !j

      RETURN
   END SUBROUTINE MODIF_DE_TRONC_new


!PL   Convolution calculation
   Subroutine CONVOL_PAV_PL( IVEC, ANGTR,NDAV,NP,NBUAV,             &
      alpav,betav,gamav,NV,NBV,thd,Yav,Qav )
!
!     Lumiere diffusee n fois dans la pointe; polar. paral. ou perp. au plan de diffusion:
!     L(n)=K(n)*p(n,thd); Lpol(n)=K(n)*q(n,thd) (K(n):cf. sub.AUREOLE);
!     p(n)=S bet(n,l)P(l,thd); q(n)=S gam(n,l)P2(l,thd)
!     bet(1,l)=betav(l); gam(1,l)=gamav(l)
!     bet(n+1,l)=bet(n,l)*betav/(2*l+1)
!     gam(n+1,l)=[bet(n,l)*gamav(l)+gam(n,l)*alpav(l)]/(2*l+1)
!
      implicit none
      logical, intent(in) :: IVEC
      integer, intent(in) :: NDAV,NP,NBUAV,NV,NBV
      real,    intent(in) :: ANGTR,thd(NV)
      real,dimension(0:2*NP),  intent(in) :: alpav,betav,gamav
      real,dimension(NV,NDAV), intent(out):: Yav,Qav

      real, dimension(NDAV,0:2*NP):: gam,bet

      integer j,k,l,m
      real pa,pb,pc,xx,dd,ee

!PL    pi = 3.141592653
      bet(1,:)=betav
      do k=2,NDAV
         do l=0,NBUAV
            bet(k,l)=bet(k-1,l)*betav(l)/(2*l+1)
         enddo
      enddo
      Yav(:,:)=0.0

!PL    For vector case
      if (IVEC) then
         gam(1,:)=gamav(:)
         do k=2,NDAV
            do l=0,NBUAV
               gam(k,l)=(bet(k-1,l)*gamav(l)+gam(k-1,l)*alpav(l))/(2*l+1)
            enddo
         enddo
         Qav(:,:)=0.0
      endif

      do j=1,NBV
!PL       if(thd(j) .le. 2*ANGTR) then
         xx=cos(thd(j))
!PL       Correction of I
         pa=0.
         pb=1.
         do k=0,NBUAV
            pc=((2*k+1.)*xx*pb-(k)*pa)/(k+1)
            do m=1,NDAV
               Yav(j,m)=Yav(j,m)+bet(m,k)*pb
            enddo
            pa=pb
            pb=pc
         enddo !k

         if (IVEC) then
            pa=0.0
            pb=3.0*(1.0-xx*xx)/2.0/sqrt(6.0)
            do k=2,NBUAV
               dd=(2*k+1.)/sqrt((k+3.)*(k-1.))
               ee=sqrt((k+2.)*(k-2.))/(2*k+1.)
               pc=dd*(xx*pb-ee*pa)
               do m=1,NDAV
                  Qav(j,m)=Qav(j,m)+gam(m,k)*pb
               enddo
               pa=pb
               pb=pc
            enddo !k
         endif ! IVEC
!         endif ! thd(j) .le. 2*ANGTR
      enddo !j
      return
   end Subroutine CONVOL_PAV_PL

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

!MH		Changes from viewing geometry to scattering angle
   FUNCTION ANGTURN(tsol,avis,afiv)
      x0=cos(tsol*rad_sp)
      x1=cos(avis*rad_sp)
      z=cos(afiv*rad_sp)
      x2=x0*x1+z*sqrt(1-x1*x1)*sqrt(1-x0*x0)
      sbeta=(x0-x1*x2)/sqrt(1-x2*x2)/sqrt(1-x1*x1)
      if(sbeta.gt.1.0)sbeta=1.0
      if(sbeta.lt.-1.0)sbeta=-1.0
      ANGTURN=acos(sbeta)-pi_sp*0.5
!c CORRECTION
      if(afiv.gt.180.)ANGTURN=-ANGTURN
      if(afiv.lt.0.1.or.afiv.gt.359.9		&
         .or.abs(afiv-180.0).lt.0.1)ANGTURN=pi_sp*0.5
      if(avis.lt.0.1.or.avis.gt.179.9)ANGTURN=-pi_sp*afiv/180.0+pi_sp*0.5
      RETURN
   END FUNCTION ANGTURN

!	  FUNCTION ANGTURN(tsol,avis,afiv)

!	  write(*,'(2f25.15)') rad_sp,pi_sp

!      x0=cos(tsol*pi/180.)
!      x1=cos(avis*pi/180.)
!      z=cos(afiv*pi/180.)
!      x2=x0*x1+z*sqrt(1-x1*x1)*sqrt(1-x0*x0)
!      sbeta=(x0-x1*x2)/sqrt(1-x2*x2)/sqrt(1-x1*x1)
!      if(sbeta.gt.1.0)sbeta=1.0
!      if(sbeta.lt.-1.0)sbeta=-1.0
!      ANGTURN=acos(sbeta)-pi*0.5!/2.
!!c CORRECTION
!      if(afiv.gt.180.)ANGTURN=-ANGTURN
!      if(afiv.lt.0.1.or.afiv.gt.359.9		&
!      .or.abs(afiv-180.0).lt.0.1)ANGTURN=pi*0.5!/2.0
!      if(avis.lt.0.1.or.avis.gt.179.9)ANGTURN=-pi*afiv/180.0+pi*0.5!/2.
!      RETURN
!      END FUNCTION ANGTURN


! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   subroutine gauss(MM,AMU,PMU,NPAR)
!C     ORDRE DE LA QUADRATURE N=2*MM-2
      !use mod_par_OS, only : IIX

!CD      PARAMETER (IX=600)
!CD      PARAMETER(NG0=91)
      !PARAMETER (IX=2*IIX)
      IMPLICIT Real(8) (A-H,O-Z)
!tl      DIMENSION Z(IX),PA(IX),W(IX),R(IX),AMU(-NG0:NG0),PMU(-NG0:NG0)
      DIMENSION Z(2*MM),PA(2*MM),W(2*MM),R(2*MM),AMU(-NPAR:NPAR),PMU(-NPAR:NPAR)
      TOL = 1.0D-15
      PI  = 3.141592653589793D+00
      PI2 = PI*PI
      N=2*MM-2
      XL=-1.
      XU=+1.
      AA = 2.0D+00/PI2
      AB = -62.0D+00/(3.0D+00*PI2*PI2)
      AC = 15116.0D+00/(15.0D+00*PI2*PI2*PI2)
      AD = -12554474.D+00/(105.0D+00*PI2*PI2*PI2*PI2)
      PA(1) = 1.D0
      EN = N
      NP1 = N+1
      U= 1.0D+00-(2.0D+00/PI)**2
      D = 1.0D+00/DSQRT((EN+0.5D+00)**2+U*0.25D+00)

      DO I= 1,N
         SM = I
         AZ = 4.0D+00*SM-1.0D+00
         AE = AA/AZ
         AF = AB/AZ**3
         AG = AC/AZ**5
         AH = AD/AZ**7
         Z(I) = 0.25D+00*PI*(AZ+AE+AF+AG+AH)
      ENDDO ! I

      DO K = 1,N
         X = COS(Z(K)*D)
         XDD=1.d0
         do while(XDD .gt. 0.d0) ! tl
            PA(2) = X
            DO NN = 3,NP1
               ENN = NN-1
               PA(NN) =		&
                  ((2.0D+00*ENN-1.0D+00)*X*PA(NN-1)-(ENN-1.0D+00)*PA(NN-2))/ENN
            ENDDO ! NN
            PNP = EN*(PA(N)-X*PA(NP1))/(1.0D+00-X*X)
            XI = X-PA(NP1)/PNP
            XD = ABS(XI-X)
            XDD = XD-TOL
            X = XI
         enddo ! while(XDD .gt. 0.d0)
         R(K) = X
         W(K) = 2.0D+00*(1.0D+00-X*X)/(EN*PA(N))**2
      ENDDO ! K

      AP = (XU-XL)/2.D0
      BP = (XU+XL)/2.D0
      do I=1,MM-1
         K=MM-I
         AMU(K)=BP+AP*R(I)
         PMU(K)=AP*W(I)
         AMU(-K)=-AMU(K)
         PMU(-K)=PMU(K)
      enddo
      AMU(-MM)=-1.
      AMU(MM)=1.
      AMU(0)=0.
      PMU(0)=0.
      PMU(-MM)=0.
      PMU(MM)=0.
      RETURN
   END subroutine gauss

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

!!PL   new Subroutine for BRDF Fourier expansion
!!PL   Pij(NN0,NN0,0:2*NG0-2), but just 0:NN expansion are used !!!
!!XH   output independent elements only
!
!      Subroutine developpe_ocean_land ( n_el,land_percent,NN,M_surf,rmu,rg,WAVE,    &
!                                        iBRDF,iBPDF,n_par,n_par_i,x_vect,           &
!                                        iBRM_water,n_par_water,x_vect_water,        &
!                                        P11,P21,P22,P31,P32,P33,salb)
!      use mod_par_OS, only : NN0,NG0,NF0
!      Integer,                       intent(in)  :: n_el,iBRDF,iBPDF,iBRM_water,NN,M_surf
!      integer,                       intent(in)  :: n_par,n_par_i,n_par_water
!      Real,                          intent(in)  :: land_percent,WAVE
!      Real(8),dimension(-NN0:NN0),   intent(in)  :: rmu, rg
!      Real(8),                       intent(in)  :: x_vect(1:n_par),x_vect_water(1:n_par_water)
!      real,dimension(NN0,NN0,0:NF0), intent(out) :: P11,P21,P22,P31,P32,P33
!      Real,                          intent(out) :: salb
!!XH   R_exp requires lots of memory
!!XH   introduce n_el to determine the dimension of R_exp we need
!      Real(8) :: R_exp(n_el,n_el,NN,NN,0:M_surf)
!      Integer :: j,k,IS
!!
!      iBRM1=iBRM_water
!      If (iBRM_water .eq. 1) iBRM1=0 !Switch to isotropic model
!          call BRM_Fexp_OSH(n_el,land_percent,M_surf,NN,rmu(1:NN),dble(WAVE),       &
!                            iBRDF,iBPDF,n_par,n_par_i,x_vect(1:n_par),              &
!                            iBRM1,n_par_water,x_vect_water(1:n_par_water),          &
!                            R_exp)
!      salb=0.
!!PL   R_exp(mu0,muv) but P(muv,mu0), then jk -> kj
!      do k=1,NN
!         do j=1,NN
!!PL         Since in PL BRDFs (R_exp) the second index corresponds to viewing
!!PL         direction whereas the first one corresponds to incident (solar)
!!PL         direction, then:
!            P11(j,k,0:M_surf)=R_exp(1,1,k,j,0:M_surf)*rmu(k)
!            if (n_el .ge. 3) then
!!XH            for vector case only
!               P21(j,k,0:M_surf)=R_exp(2,1,k,j,0:M_surf)*rmu(k)
!               P31(j,k,0:M_surf)=R_exp(3,1,k,j,0:M_surf)*rmu(k)
!               P22(j,k,0:M_surf)=R_exp(2,2,k,j,0:M_surf)*rmu(k)
!               P32(j,k,0:M_surf)=R_exp(3,2,k,j,0:M_surf)*rmu(k)
!               P33(j,k,0:M_surf)=R_exp(3,3,k,j,0:M_surf)*rmu(k)
!            end if
!         end do !j
!         salb=salb+rg(k)*rmu(k)*dot_product(R_exp(1,1,k,1:NN,0),rg(1:NN)*rmu(1:NN))
!      end do !k
!!
!      return
!      end subroutine developpe_ocean_land

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!PL   Expansion coeficientcal calculation
   subroutine betal(IVEC,LL,NG0,zmu,zp,f11,f12,f22,f33,      &  ! in
      alpa,beta,gama,zeta           )             ! out
!XH   optional parameters for vector case
      logical,                                intent(in)  :: IVEC
      integer,                                intent(in)  :: LL, NG0
      real*8, dimension(-NG0:NG0),            intent(in)  :: zmu,zp
      real,   dimension(-NG0:NG0),            intent(in)  :: f11,f12,f22,f33
!
      real,   dimension(0:2*NG0-2),           intent(out) :: beta,alpa,gama,zeta
!
      integer :: k,j
      real,   dimension(-1:2*NG0)  :: pl,pol,ppl,pml
      real,   dimension(0:2*NG0-2) :: betap,betam

!XH   it can be simplified to series expansion in Legendre polynomials for scalar case
      pl(-1)=0.
      pl(0) =1.
      beta(0:LL+LL-2)=0.0
      do j=-LL,LL
         xx=zmu(j)
         do k=0,LL+LL-2
            pl(k+1)=((2*k+1.)*xx*pl(k)-k*pl(k-1))/(k+1.)
            beta(k)=beta(k)+zp(j)*pl(k)*f11(j)*(k+0.5)
         end do ! k
      end do ! j
      if (IVEC) then
         gama(0:LL+LL-2) =0.
         alpa(0:LL+LL-2) =0.
         zeta(0:LL+LL-2) =0.
         betap(0:LL+LL-2)=0.
         betam(0:LL+LL-2)=0.
         pol(0)=0.0
         pol(1)=0.0
         ppl(0)=0.0
         ppl(1)=0.0
         pml(0)=0.0
         pml(1)=0.0
         do j=-LL,LL
            xx=zmu(j)
            pol(2)=3.0*(1.0-xx*xx)*0.5/sqrt(6.0)
            ppl(2)=(1.+xx)*(1.+xx)*0.25
            pml(2)=(1.-xx)*(1.-xx)*0.25
            do k=0,2*LL-2
               if (k .gt. 1)then
                  dd=(2*k+1.)/sqrt((k+3.)*(k-1.))
                  ee=sqrt((k+2.)*(k-2.))/(2*k+1.)
                  cc=(k+1.)*(k+2.)*(k-2.)/k/(k+3.)/(k-1.)
                  bb=(2*k+1.)/k/(k+3.)/(k-1)
                  pol(k+1)=dd*(xx*pol(k)-ee*pol(k-1))
                  ppl(k+1)=bb*(k*(k+1.)*xx-4.)*ppl(k)-cc*ppl(k-1)
                  pml(k+1)=bb*(k*(k+1.)*xx+4.)*pml(k)-cc*pml(k-1)
               end if ! k .gt. 1
               gama(k)=gama(k)+zp(j)*pol(k)*f12(j)*(k+0.5)
               betap(k)=betap(k)+zp(j)*ppl(k)*(f22(j)+f33(j))*(k+0.5)
               betam(k)=betam(k)+zp(j)*pml(k)*(f22(j)-f33(j))*(k+0.5)
               zeta(k)=(betap(k)-betam(k))*0.5
               alpa(k)=(betap(k)+betam(k))*0.5
            end do ! k
         end do ! j
      end if
!
      return
   end subroutine betal

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   subroutine profiles_WD (  IHLV,ATMOS_EMIS,                   & ! IN
      IP_ADDITION,                       &
      IDWN,                              &
      TEXT, EXT, SSA,                    &
      HGR_km, HOBS_km, HMAX_atm_km, NT1, &
      T_profile,                         &
      DISCRVD,                           &
      NT, NAV, EXTH, WD, HLV,            & ! OUT
      T_layer, T_level,                  &
      index_T, tauH_TOT_int              &
      )

      use mod_par_OS, only : KNT, NMM, NMG, KVERT_WD
      use sub_gas_kd, ONLY : NLEVEL_GAS
      use mod_intrpl_linear
      use mod_vertical_distr_derived_type
      use mod_mixture_ref_index_chem, only: linear_near_neighbor_fit_dp,linear_near_neighbor_fit_dp_index

      implicit none
!	------------------------------------------------------------------------------------------------------
      logical,                          intent(in)  ::  IHLV,IP_ADDITION,IDWN,ATMOS_EMIS
      integer,                          intent(in)	::  NT1(1:2)
      real,                             intent(in)  ::  TEXT, HGR_km
      real,                             intent(in)  ::  HOBS_km, HMAX_atm_km
      type(discret_vertical_distribution), intent(in) :: DISCRVD
      real,dimension(NMM+NMG),          intent(in)  ::  EXT,SSA
      real,dimension(NLEVEL_GAS),       intent(in)  ::  T_profile
      integer,                          intent(out) ::  NT
      integer,                          intent(out) ::  NAV
      real,dimension(KNT),              intent(out) ::  EXTH
      real,dimension(KNT-1,NMM+NMG),    intent(out) ::  WD
      real,dimension(KNT),              intent(out) ::  HLV
      real,dimension(KNT-1),            intent(out) ::  T_layer
      real,dimension(KNT),              intent(out) ::  T_level
      integer,dimension(KNT),           intent(out) ::  index_T
      real,dimension(KVERT_WD),         intent(out) ::  tauH_TOT_int
!	------------------------------------------------------------------------------------------------------
      real, dimension(KVERT_WD)          ::  H
      real, dimension(NMM+NMG,KVERT_WD)  ::  tauH
      real, dimension(KVERT_WD)          ::  tauH_TOT
      real, dimension(KVERT_WD,NMM+NMG)  ::  PROF
      integer :: natm
!	------------------------------------------------------------------------------------------------------
      integer                     ::  i,i1,i2
!	------------------------------------------------------------------------------------------------------
      real, parameter             ::  tiny = 1e-6
      integer                     ::  NH, ip, NTEMP, NTEMP1
      real                        ::  dtau, dtaup, TEXT_HP, TEXT_HP0

      real,dimension(KVERT_WD)        ::  H_temp
      real,dimension(NMM+NMG,KVERT_WD)::  tauH_temp
      real,dimension(KNT)             ::  EXTH_temp
      real,dimension(KNT-1,NMM+NMG)   ::  WD_temp
      real,dimension(KNT)             ::  HL, HL_temp
      real,dimension(NMM+NMG)         ::  xnorm, tauHP, EXT_HP, EXT_HP0
      integer                         ::  iplane, i_tau
      real                            ::  xnorm0
      real                            ::  DH
      real                            ::  sum_dtau

!PL_tau
      real, parameter :: dtau_max=0.5
      integer :: NT_max

! iplane = 1  - satellite data
!        = 2  - airplane  meas
!        = 3  - ground based meas

!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------
!  a1 and b1 - coefficients for straight y(h) between grid heghts
!  y(i-1)=a1*h(i-1)+b1
!  y(i)  =a1*h(i)+b1
!  a1=(y(i)-y(i-1))/(h(i)-h(i-1))
!  b1=y(i-1)-a1*h(i-1)
!
!  NT - number of layers ; L - layer index
!  dh=h(i)-h(i-1) < 0.0
!  dtau=TEXT/NT
!  diff=dtau-tauS(i-1) ; tauS(i-1)=(y(h(L-1))+y(h(i-1))*(h(L-1)-h(i-1))
!
!  0.5*(y(i-1)+y(i))*(h(i-1)-h(i))=diff
!  0.5*(a1*h(i-1)+b+a1*(h(i-1)+dh)+b))*(-dh)=diff
!  0.5*a1*dh**2+(a1*h(i-1)+b)+diff=0.0
!
!  A = 0.5*a1
!  B = a1*h(i-1)+b1 ; B = y(h(i-1))
!  C = diff
!
!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------
      natm = DISCRVD%natm ! number of atmosphere components = aerosol+clouds+molecular+gas
      WD(:,:) = 0.0
      EXTH(:) = 0.0
      HL(:)   = 0.0

      if (HOBS_km .ge. HMAX_atm_km ) then
         iplane=1 ! satellite
         if ( IDWN .eqv. .true. ) then
            write(*,'(a,2(/,a))') 'WARNING in subroutine profiles_WD:', &
               'Temporary solution for satellite (space lidar) and ground based', &
               'instruments in single pixel.'
            iplane=3 ! ground based
         endif
      else if ( HOBS_km .gt. HGR_km .and. HOBS_km .lt. HMAX_atm_km ) then
         iplane=2 ! airplane
      else if ( HOBS_km .eq. HGR_km ) then
         iplane=3 ! ground based
      else if ( HGR_km .ge. HMAX_atm_km ) then
         write(tmp_message,'(2(a,f8.4))') &
            'HGR_km =',HGR_km,' .ge. HMAX_atm_km =',HMAX_atm_km
         G_ERROR(trim(tmp_message))
      endif

      NH=DISCRVD%nh
!	------------------------------------------------------------------------------------------------------

! calculate NT - number of layers with tau<=dtau
      dtau = 0.02

!write(*,'(a,i0,2x,a)') 'natm = ',natm,'EXT(1:natm):'
!write(*,'(10es12.4)') EXT(1:natm)
!write(*,'(a,es12.4)') 'TEXT =',TEXT

!stop

!MH Change to avoid problems when gas abosprtion is accounted
      !   NH=KVERT_WD


      if(ATMOS_EMIS)then

         NT = NT1(1)-1

      else
         NT = int(TEXT/dtau)
         if ( MOD(TEXT,dtau) .gt. 0. ) NT = NT+1
         !PL_tau
         NT_max = int(TEXT/dtau_max)
         if ( MOD(TEXT,dtau_max) .gt. 0. ) NT_max = NT_max+1
         NT_max = max(NT_max,NT1(1)-1)

         if (NT .gt. NT_max)   NT = NT_max
         if (NT .gt. NT1(2)-1) NT = NT1(2)-1

      end if

      !  NT = 100
      !write(*,*) NT, NT_max, NT1(1),'  - NT, NT_max, NT1(1)'

      dtau=TEXT/real(NT)

      ! write(*,'(3(a,i0,3x),2(a,es11.4,3x))') &
      ! 'used NT = ',NT,'NT1(1) = ',NT1(1),'NT1(2) = ',NT1(2), &
      ! 'dtau =',dtau,'tetot =',text

      NH = DISCRVD%NH

      H(1:DISCRVD%nh) = DISCRVD%h_km(1:DISCRVD%nh)

      do i1=1,natm
         tauH(i1,1:DISCRVD%nh) = DISCRVD%val(1:DISCRVD%nh,i1)*EXT(i1)/DISCRVD%norm(i1)
      end do ! i1



      IF (iplane .ne. 2) THEN
         TEXT_HP0 = 0.0
         EXT_HP0(:) = 0.0
         !HL_temp(:) = 0.0
         call get_WD (     IP_ADDITION,         &
            natm,NT,NH,         & ! IN
            TEXT,EXT,SSA,dtau,   &
            TEXT_HP0,EXT_HP0,    &
            NH,H,tauH,           &
            EXTH,WD,HL_temp      & ! OUT
            )
         if ( error_present() ) return

         HL(1:NT+1)=HL_temp(1:NT+1)

      ELSE ! iplane=2, airplane observations

         if ( NT .eq. 1 ) then
            NT = 2 ! 1 layer above plane + 1 layer under plane
         endif

! define extinction for each present atmospheric component at altitude HOBS_km
         do i1=1,natm
            tauHP(i1) = linear(H(1:NH),tauH(i1,1:NH),NH,HOBS_km)
         enddo ! i1

! integrate extinction for each present atmospheric component over
         ! altitude from HOBS_km up to HMAX_atm_km

         EXT_HP(:)=0.0
         ip=1	! H(1) = HMAX_atm_km
         do while ( HOBS_km .le. H(ip) )
            ip=ip+1
            EXT_HP(1:natm)=EXT_HP(1:natm)+0.5*(tauH(1:natm,ip-1)+tauH(1:natm,ip))*(H(ip-1)-H(ip))
         enddo ! while(HP_km .lt. H(ip))
         ! adjust EXT_HP(1:natm) because H(ip) can be lower than HOBS_km
         EXT_HP(1:natm)=EXT_HP(1:natm)-0.5*(tauH(1:natm,ip-1)+tauH(1:natm,ip))*(H(ip-1)-H(ip))+  &
            0.5*(tauH(1:natm,ip-1)+tauHP(1:natm))*(H(ip-1)-HOBS_km)
         ! total optical thickness above HOBS_km
         TEXT_HP=SUM(EXT_HP(1:natm))
         !write(*,*) 'ip=',ip,'  HOBS_km=',HOBS_km,'  H(ip)=',H(ip)

! above airplane altitude
         !write(*,'(a,/,a)') 'WARNING in profiles_WD: ', &
         !'GRASP application to airplane measurements had not been extensively tested yet.'
         NTEMP = 0
         NTEMP1 = 0
         NTEMP = int(TEXT_HP/dtau )
         if ( MOD(TEXT_HP,dtau) .gt. 0. ) NTEMP = NTEMP+1
         ! NT - total number of layers with tau<=dtau from MASL_km up to HMAX_atm_km
         if ( NTEMP .gt. NT ) NTEMP = NT
         NTEMP1 = NTEMP
         dtaup = TEXT_HP/real(NTEMP1)
         !write(*,*) 'dtaup=',dtaup,'  dtau=',dtau,'  NTEMP1_above=',NTEMP1
         !write(*,*) 'ip=',ip,'  HOBS_km=',HOBS_km,'  TEXT_HP=',TEXT_HP

         H_temp(1:ip-1) = H(1:ip-1)
         H_temp(ip) = HOBS_km
         tauH_temp(1:natm,1:ip-1) = tauH(1:natm,1:ip-1)
         tauH_temp(1:natm,ip) = tauHP(1:natm)
         !EXTH_temp(:) = 0.0
         !WD_temp(:,:) = 0.0
         !HL_temp(:) = 0.0
         TEXT_HP0 = 0.0
         EXT_HP0(1:natm) = 0.0
         call get_WD (     IP_ADDITION,               &
            natm,NTEMP,NH,            & ! IN
            TEXT_HP,EXT_HP,SSA,dtaup,  &
            TEXT_HP0,EXT_HP0,          &
            ip,H_temp,tauH_temp,       &
            EXTH_temp,WD_temp,HL_temp  & ! OUT
            )
         if ( error_present() ) return

         EXTH(1:NTEMP+1) = EXTH_temp(1:NTEMP+1)
         WD(1:NTEMP,1:natm) = WD_temp(1:NTEMP,1:natm)
         HL(1:NTEMP+1) = HL_temp(1:NTEMP+1)

! under airplane layer

         NTEMP = int((TEXT-TEXT_HP)/dtau)
         if ( MOD((TEXT-TEXT_HP),dtau) .gt. 0. ) then
            NTEMP = NTEMP + 1
         endif
         ! NT - total number of layers with tau<=dtau from MASL_km up to HMAX_atm_km
         if ( NTEMP1+NTEMP .gt. NT ) then
            NTEMP = NT - NTEMP1
         endif
         if ( NTEMP .le. 0 ) then
            write(tmp_message,'(3(a,es11.4,3x),a,i0)') &
               'TEXT = ',TEXT,'TEXT_HP = ',TEXT_HP,'dtau = ',dtau,'NTEMP_under_plane=',NTEMP
            G_ERROR(trim(tmp_message))
         endif ! NTEMP .le. 0

         dtaup = (TEXT-TEXT_HP)/real(NTEMP)

         if(NTEMP1+NTEMP+1 .ne. NT+1) then
            write(tmp_message,'(2(a,i0))') 'NTEMP+1 = ',NTEMP1+NTEMP+1,' .NE. NT+1 = ',NT+1
            G_ERROR(trim(tmp_message))
         endif

         H_temp(1) = HOBS_km
         H_temp(2:NH-ip+2) = H(ip:NH)
         tauH_temp(1:natm,1) = tauHP(1:natm)
         tauH_temp(1:natm,2:NH-ip+2) = tauH(1:natm,ip:NH)
         !EXTH_temp(:)=0.0
         !WD_temp(:,:)=0.0
         !HL_temp(:)=0.0
         ip = NH-ip+2
         TEXT_HP0 = TEXT_HP
         EXT_HP0(1:natm) = EXT_HP(1:natm)

         call get_WD (     IP_ADDITION,                  &
            natm,NTEMP,NH,                & ! IN
            TEXT,EXT,SSA,dtaup,           &
            TEXT_HP0,EXT_HP0,             &
            ip,H_temp,tauH_temp,          &
            EXTH_temp,WD_temp,HL_temp     & ! OUT
            )

         if ( error_present() ) return

         do i=1,NTEMP+1
            EXTH_temp(i) = EXTH_temp(i)+TEXT_HP
         enddo ! i

         EXTH(NTEMP1+1:NT+1) = EXTH_temp(1:NTEMP+1)
         WD(NTEMP1+1:NT,1:natm) = WD_temp(1:NTEMP,1:natm)
         HL(NTEMP1+1:NT+1) = HL_temp(1:NTEMP+1)

      ENDIF ! iplane.ne.2



      if(ATMOS_EMIS) then

!MH Linear interpolation from the original of temperature in grid heights to the Optical depth of the levels finally used in radiative transfer

         do i1=2,NH
!               tauH_TOT(i1) = SUM(tauH(1:natm,i1))
            tauH_TOT(i1) = 0.5*(SUM(tauH(1:natm,i1-1)) + SUM(tauH(1:natm,i1)))*(H(i1-1) - H(i1))
         end do ! i1
         tauH_TOT(1) = EXTH(1)
         do i1=1,NH
            tauH_TOT_int(i1) = SUM(tauH_TOT(1:i1))
         end do ! i1


!MH linear interpolation of temperature in levels from DISCRD grid to EXTH grid
         call linear_near_neighbor_fit_dp_index(NH,                    & !IN
            tauH_TOT_int(1:NH),     &
            T_profile(1:NH),        &
            NT+1,                   &
            EXTH(1:NT+1),           &
            T_level(1:NT+1),        & !OUT
            index_T(1:NT+1))

!MH Assuming that the variations between layers are linear, the temperature in the layer is just the average of the temperature in the levels.
!        do i=1,NT
!            T_layer(i) = (T_level(i) + T_level(i+1))/2
!        end do

         do i=1,NT

            T_layer(i) = 0.0
            sum_dtau = 0.0

            if(ABS(index_T(i+1) - index_T(i)) .gt. 1)then

               do i_tau=index_T(i),index_T(i+1)
                  T_layer(i) = T_layer(i) + (T_profile(i_tau)*(tauH_TOT_int(i_tau) - tauH_TOT_int(i_tau-1)))
                  sum_dtau = sum_dtau + (tauH_TOT_int(i_tau) - tauH_TOT_int(i_tau-1))
               end do

               T_layer(i) = T_layer(i)/sum_dtau

            else
               T_layer(i) = (T_level(i) + T_level(i+1))/2
            end if

         end do

      else

         T_layer(:) = 0.0
         T_level(:) = 0.0

      end if

!write(*,*)'T_layer_new', T_layer(1:NT)
!stop

!write(*,*) 'T_layer',T_layer(1:NT)


      !do i=1,NT+1
      !write(*,'(a,i5,f15.6,10e14.6)') 'i,HL(i),EXTH(i),TEXT: ',  &
      !								 i,HL(i),EXTH(i),TEXT
      !enddo ! i

      !do i=1,NT
      !write(*,*) i,WD(i,1:natm),'  - i,WD(i,1:natm)'
      !enddo ! i

      !NT becomexs the number of bonderies
      NT = NT+1

      select case(iplane)
       case(1)
         NAV = 1         ! satellite
       case(2)
         NAV = NTEMP1+1  ! airplane
       case(3)
         NAV = NT        ! ground based
       case default
         write(tmp_message,'(a,i0,a)') 'iplane = ',iplane,'  - unknown value'
         G_ERROR(trim(tmp_message))
      end select ! iplane
!XH   optional output of altitude for each level
      if (IHLV) HLV = HL

      return
   end subroutine profiles_WD

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
   subroutine root(A,B,C,x1,x2)

!quadratic equation solutions

      implicit none
!  ------------------------------------------------------------------------------------------------------
      real, intent(in)    ::  A, B, C
      real, intent(out)   ::  x1, x2
!  ------------------------------------------------------------------------------------------------------
      real                ::  D
      real                ::  sgn
      real                ::  tiny
      logical             ::  lind
!  ------------------------------------------------------------------------------------------------------
! A careful floating point computer implementation combines several strategies to produce a robust result.
! Assuming the discriminant, b2 − 4ac, is positive and b is nonzero, the computation would be as follows:
! x1 = (-b - sgn( b ) * sqrt( b^2 - 4ac) ) / (2*a), x2 = 2c / (-b -sgn( b ) * sqrt( b^2 - 4ac ) = c / (a*x1).
! Here sgn denotes the sign function, where sgn(b) is 1 if b is positive and −1 if b is negative.
! This avoids cancellation problems between b and the square root of the discriminant by ensuring that
! only numbers of the same sign are added.
!  ------------------------------------------------------------------------------------------------------
      x1 = 0.0
      x2 = 0.0

      tiny = 1e-30
      lind=.false.

      D = B*B-4.0*A*C

!      if(abs(D) .le. tiny) then
      if(D .eq. 0.0) then
         x1 = -0.5*B/A
         x2 = -0.5*B/A
         lind=.true.
      elseif(D .gt. 0.0) then
         if(B .ne. 0.0) then
! Floating-point implementation
            if(B .lt. 0.0) then
               sgn = -1.0
            else
               sgn = +1.0
            endif ! B .lt. 0.0
            x1 = 0.5*(-B-sgn*sqrt(D))/A
            x2 = C/(A*x1)
            lind=.true.
         else
            x1 = 0.5*(-B+sqrt(D))/A
            x2 = 0.5*(-B-sqrt(D))/A
            lind=.true.
         endif ! B .ne. 0.0
!      elseif(D .lt. -tiny) then
      elseif(D .lt. 0.0) then
         write(tmp_message,'(a,es11.4,a)') 'D =',D,'  Determinant < 0.0'
         G_ERROR(trim(tmp_message))
      endif ! abs(D) .le. tiny

      if(.not. lind) then
         write(tmp_message,'(a)') 'roots are not defined'
         G_ERROR(trim(tmp_message))
      endif ! .not. lind

      return
   end subroutine root

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   subroutine get_WD (IP_ADDITION,           &
      natm,NT,max_NH,        & ! IN
      TEXT,EXT,SSA,dtau,     &
      TEXT_HP0,EXT_HP0,      &
      NH,H,TH,               &
      EXTH,WD,HL             & ! OUT
      )


      use mod_par_OS, only : KNT,NMM,NMG
!	------------------------------------------------------------------------------------------------------
      implicit none
!	------------------------------------------------------------------------------------------------------
      integer,                        intent(in)  ::  natm, NT, NH, max_NH
      logical,                        intent(in)  ::  IP_ADDITION
      real,                           intent(in)  ::  TEXT, dtau
      real,dimension(NMM+NMG),        intent(in)  ::  EXT, SSA
      real, dimension(max_NH),        intent(in)  ::  H
      real, dimension(NMM+NMG,max_NH),intent(in)  ::  TH
      real, dimension(NMM+NMG),       intent(in)  ::  EXT_HP0
      real,                           intent(in)  ::  TEXT_HP0

      real,dimension(KNT),            intent(out) ::  EXTH
      real,dimension(KNT-1,NMM+NMG),  intent(out) ::  WD
      real,dimension(KNT),            intent(out) ::  HL
!	------------------------------------------------------------------------------------------------------
!     natm         - number of component (aerosol+molecular+gas)
!     NH         - number of grid heights for profiles
!     H          - grid heights
!     TH         - PROF*EXT/norm - calculated at grid heights (see subroutine discr_profile)
!     TEXT       - total EXT
!     EXT        - EXT for each component
!     SSA        - single scattering albedo for each component
!     dtau       - total EXT of layer
!     TEXT_HP0   - total EXT at aiplane height
!     EXT_HP0    - EXT for each component at airplane height
!
!     TEXT_HP0=0.0 and EXT_HP0=0.0 for satellite and ground observations
!
!     tau(0)     - tau for sum of aerosol component
!     tau(1:NMM) - tau for each aerosol component

      real, dimension(0:NMM+NMG,KNT)   ::  TL
      real, dimension(0:NMM+NMG)       ::  TH1, TH2, a1, b1, &
         TS1, TS2, TS
      real, dimension(1:NMM+NMG,KNT)   ::  EXTH1
      real                             ::  x1, x2, DH1, DH2,       &
         A, B, C, TTH,           &
         H1, H2, diff, diff_test

      integer                          ::  i, i1, IL
      real                             ::  tiny, tiny1
      integer :: istop
      istop = 0
!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------
!     a1 and b1 - coefficients for straight y(h) between grid heights
!     y(i-1)=  a1*h(i-1)+b1
!     y(i)  =  a1*h(i)+b1
!     a1    =  (y(i)-y(i-1))/(h(i)-h(i-1))
!     b1    =  y(i-1)-a1*h(i-1)
!
!     NT - number of layers ; L - layer index
!     dh     = h(i)-h(i-1) < 0.0
!     dtau   = TEXT/NT
!     diff   = dtau-TS(i-1)
!     TS(i-1)= (y(h(L-1))+y(h(i-1))*(h(L-1)-h(i-1))
!
!     0.5*(y(i-1)+y(i))*(h(i-1)-h(i))=diff
!     0.5*(a1*h(i-1)+b+a1*(h(i-1)+dh)+b))*(-dh)=diff
!     0.5*a1*dh**2+(a1*h(i-1)+b)+diff=0.0
!
!     A = 0.5*a1
!     B = a1*h(i-1)+b1 ; B = y(h(i-1))
!     C = diff
!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------
      tiny  = 1e-4
      tiny1 = 1e-30
!	------------------------------------------------------------------------------------------------------
      EXTH(:)=0.0
      WD(:,:)=0.0

!     Calculate heights, EXTH (at levels) and WD (for layers) with tau<=dtau

      !write(*,*) natm, NT, KNT, max_NH, dtau,' - natm, NT, KNT, max_NH, dtau,'
      !write(*,*) 'TEXT    =',TEXT,    '  EXT:     ',EXT(1:natm),'  SSA:   ',SSA(1:natm)
      !write(*,*) 'TEXT_HP0=',TEXT_HP0,'  EXT_HP0: ',EXT_HP0(1:natm)
      !write(*,*) 'In get WD'
      !do i=1,NH
      !write(*,*) 'i=',i,'  H(i)=',H(i) !,'  TH(1:natm,i)',TH(1:natm,i),sum(TH(1:natm,i))
      !end do

!XH   integration from the top
      !TTH=0.0
      !do i=2,NH
      !   TTH=TTH+0.5*(SUM(TH(1:natm,i-1)+TH(1:natm,i)))*(H(i-1)-H(i))
      !end do
      !write(*,*) 'TTH=',TTH,' TEXT=',TEXT-TEXT_HP0,' TEXT_HP0=',TEXT_HP0,' EXT_HP0=',EXT_HP0

!XH   integration from the bottom
      !TTH=0.0
      !do i=NH,2,-1
      !   TTH=TTH+0.5*(SUM(TH(1:natm,i-1)+TH(1:natm,i)))*(H(i-1)-H(i))
      !end do
      !write(*,*) 'TTH=',TTH,' TEXT=',TEXT-TEXT_HP0,' TEXT_HP0=',TEXT_HP0,' EXT_HP0=',EXT_HP0

      HL(:)    = 0.0
      HL(1)    = H(1)
      HL(NT+1) = H(NH)
!write(*,*) '1'
!write(*,*) TH(1, NH)
!write(*,*) '2'
!write(*,*) TH(2, NH)
!write(*,*) '3'
!write(*,*) TH(3, NH)
!write(*,*) '4'
!write(*,*) TH(4, NH)
!write(*,*) NH
!stop
      TL(:,:)       = 0.0
      TL(1:natm,   1) = TH(1:natm, 1)
      TL(1:natm,NT+1) = TH(1:natm,NH)
      TL(0,   1) = SUM(TH(1:natm, 1))
      TL(0,NT+1) = SUM(TH(1:natm,NH))

      EXTH1(1:natm,1)    = 0.0
      EXTH1(1:natm,NT+1) = EXT(1:natm)-EXT_HP0(1:natm)

      IL=1
      TS(:) = 0.0

!write(*,*) TH(1:natm, 1)
!write(*,*) TH(1:natm, NH)
!stop
      LOOP_HEIGHT: do i=2,NH
         H1 = H(i-1)
         H2 = H(i)
         TH1(1:natm) = TH(1:natm,i-1)
         TH1(0)    = SUM(TH1(1:natm))
         TH2(1:natm) = TH(1:natm,i)
         TH2(0)    = SUM(TH2(1:natm))

         TS1(0:natm) = TS(0:natm)
         TS2(0:natm) = 0.5*(TH1(0:natm)+TH2(0:natm))*(H1-H2)
         TS(0:natm)  = TS(0:natm) + TS2(0:natm)

!         write(*,*) '0: i=',i,'  IL=',IL, TS1(0), TS(0), dtau, &
!                                   abs(TS(0)-dtau)/dtau, tiny, &
!                ' - TS1, TS, dtau, abs(TS(0)-dtau)/dtau, tiny'

         if (IL .eq. NT .and. i .eq. NH) then

            IL=IL+1
            HL(IL) = H(NH)
            EXTH1(1:natm,IL)=EXT(1:natm)-EXT_HP0(1:natm)
            if(IP_ADDITION) then
               if ((abs(TS(0)-dtau)/dtau) .ge. tiny) then
                  write(*,'(a,4e18.6,i6,e18.6)') 'Warning in get_WD! dtau(NT),dtau,tiny,(dtau(NT)-dtau)/dtau),NT+1, TEXT:  ',  &
                     TS(0),dtau,tiny,(abs(TS(0)-dtau)/dtau),NT+1, TEXT

!               istop = 1
               end if
            end if ! IP_ADDITION
            cycle LOOP_HEIGHT
         end if

         a1(0:natm) = (TH2(0:natm)-TH1(0:natm))/(H2-H1)
         b1(0:natm) = TH1(0:natm)-a1(0:natm)*H1

!  --------------------------------------------------------------------------------

         if (abs(TS(0)-dtau)/dtau .le. tiny) then
            TS(0:natm) = 0.0
            IL=IL+1
            HL(IL) = H2
            TL(0:natm,IL)=TH2(0:natm)
            EXTH1(1:natm,IL)=EXTH1(1:natm,IL-1)+TS2(1:natm)
!            write(*,*) '1: i=',i,'  IL=',IL, TS1(0), TS(0), dtau, &
!            abs(TS(0)-dtau)/dtau, tiny,' - TS1, TS, dtau, abs(TS(0)-dtau)/dtau, tiny'
            cycle LOOP_HEIGHT
         else if (TS(0) .gt. dtau) then
            do while(TS(0) .gt. dtau)
               diff  = dtau-TS1(0)
               if (diff .lt. 0.0) then
                  write(tmp_message,'(a,es11.4,a)') 'diff =',diff,'  - diff can not be <0.'
                  G_ERROR(trim(tmp_message))
               end if
               A = 0.5*a1(0)
               B = TH1(0)
               C = diff

               IL = IL+1
               if (IL .gt. NT+1) then
                  write(tmp_message,'(2(a,i0),a)') &
                     'IL = ',IL,'  NT+1 = ',NT+1,'  - Number of layers with tau<=dtau .GT. NT+1'
                  G_ERROR(trim(tmp_message))
               end if
!              ROOT
!               if (abs(A) .ge. tiny1) then
               if (A .ne. 0.0) then
                  call root(A,B,C,DH1,DH2)
                  x1=DH1+H1
                  x2=DH2+H1
                  if(x1 .le. H1 .and. x1 .ge. H2) HL(IL)=x1
                  if(x2 .le. H1 .and. x2 .ge. H2) HL(IL)=x2
               else ! A=0
                  HL(IL) = -C/B+H1
               end if ! A.ne.0.0
!               write(*,*) A,B,C,a1(0),b1(0),TH1(0),TH2(0),i,'  - A,B,C,a1,b1,TH1(0),TH2(0),i'
!               write(*,'(15x,7e14.6,a,i5)') DH1,DH2,H1,H2,x1,x2,HL(IL),'  - DH1,DH2,H1,H2,x1,x2,HL(IL),IL=',IL

               TL(0:natm,IL) = a1(0:natm)*HL(IL)+b1(0:natm)
!              test 1:
!               diff_test= -1.0*(A*(HL(IL)-H1)*(HL(IL)-H1)+B*(HL(IL)-H1))
!               if (abs(diff_test-diff)/diff .gt. tiny) then
!                  write(*,'(3(a,i4),4(a,e14.6))') 'CHECK1:  i-1=',i-1,'  i=',i,'  IL=',IL,  &
!                                                  '  HL(IL)=',HL(IL),'  diff_test=',diff_test,'  diff=',diff
!                  write(*,*) '1: diff_test=',diff_test,' .NE. diff=',diff
!                  stop 'stop in get_WD'
!               end if
!              test 2:
!               diff_test = 0.5*(TH1(0)+TL(0,IL))*(H1-HL(IL))
!               if (abs(diff_test-diff)/diff .gt. tiny) then
!                  write(*,'(3(a,i4),3(a,e14.6))') 'CHECK2:  i-1=',i-1,'  i=',i,'  IL=',IL,  &
!                                                  '  HL(IL)=',HL(IL),'  diff_test=',diff_test,'  diff=',diff
!                  write(*,*) '2: diff_test=',diff_test,' .NE. diff=',diff
!                  stop 'stop in get_WD'
!               end if

               EXTH1(1:natm,IL) = EXTH1(1:natm,IL-1)+TS1(1:natm)+0.5*(TH1(1:natm)+TL(1:natm,IL))*(H1-HL(IL))
               TS2(0:natm) = 0.5*(TL(0:natm,IL)+TH2(0:natm))*(HL(IL)-H2)
!               write(*,'(3e12.4,2f10.4,a)') TS2(0),TL(0,IL),TH2(0),HL(IL),H2,  &
!                                        ' - TS2(0),TL(0,IL),TH2(0),HL(IL),H2'

               if (abs((TS2(0)-dtau)/dtau) .le. tiny) then

                  TS(0:natm) = 0.0
                  IL=IL+1
                  HL(IL) = H2
                  TL(0:natm,IL)=TH2(0:natm)
                  EXTH1(1:natm,IL)=EXTH1(1:natm,IL-1)+TS2(1:natm)
!                  write(*,*) '2: i=',i,'  IL=',IL, TS1(0), TS(0), dtau, &
!                                            abs(TS(0)-dtau)/dtau, tiny, &
!                         ' - TS1, TS, dtau, abs(TS(0)-dtau)/dtau, tiny'
                  if (IL .eq. NT .and. i .eq. NH) then
                     IL=IL+1
                     HL(IL) = H(NH)
                     EXTH1(1:natm,IL)=EXT(1:natm)-EXT_HP0(1:natm)
                  end if
                  cycle LOOP_HEIGHT
               else if(TS2(0) .lt. dtau) then
                  TS(0:natm) = TS2(0:natm)
                  if (IL .eq. NT .and. i .eq. NH) then
                     IL=IL+1
                     HL(IL) = H(NH)
                     EXTH1(1:natm,IL)=EXT(1:natm)-EXT_HP0(1:natm)
                  end if
!                  write(*,*) '3: i=',i,'  IL=',IL, TS1(0), TS(0), dtau, &
!                                            abs(TS(0)-dtau)/dtau, tiny, &
!                         ' - TS1, TS, dtau, abs(TS(0)-dtau)/dtau, tiny'
                  cycle LOOP_HEIGHT
               else if(TS2(0) .gt. dtau) then
                  TS1(0:natm) = 0.0
                  TS(0:natm)  = TS2(0:natm)
                  H1        = HL(IL)
                  TH1(0:natm) = TL(0:natm,IL)
!                  write(*,*) '4: i=',i,'  IL=',IL, TS1(0), TS(0), dtau, &
!                                            abs(TS(0)-dtau)/dtau, tiny, &
!                         ' - TS1, TS, dtau, abs(TS(0)-dtau)/dtau, tiny'
               end if ! TS2(0) .eq. dtau

!               if (IL .eq. NT+1) then
!                  EXTH1(1:natm,IL)=EXT(1:natm)-EXT_HP0(1:natm)
!                  cycle LOOP_HEIGHT
!               end if
            end do ! while(TS(0) .gt. dtau)
         end if ! abs(TS(0)-dtau)/dtau .le. tiny
!  --------------------------------------------------------------------------------

      end do LOOP_HEIGHT

      if (IL .ne. NT+1) then
         write(tmp_message,'(2(a,i0))') &
            'Execution has to be terminated. IL = ',IL,' .ne. NT+1 = ',NT+1
         G_ERROR(trim(tmp_message))
      end if ! IL

      EXTH1(1:natm,NT+1)=EXT(1:natm)-EXT_HP0(1:natm)

!      if (ABS(HL(NT+1)-H(NH))/H(NH) .gt. tiny) then
!         write(*,*) '!!! WARNING in get_WD: HL(NT+1) .ne. H(NH)'
!         write(*,*) 'NT+1=',NT+1,'  HL(NT+1)=',HL(NT+1),'  H(NH)=',H(NH)
!         write(*,*) '!!! WARNING in get_WD: H(NH) => HL(NT+1)'
!      end if

!     Calculate output: EXTH(1:NT+1) and WD(1:NT,1:natm)

      do i=1,NT+1
         EXTH(i)=SUM(EXTH1(1:natm,i))
!         write(*,*) i,EXTH(i),EXTH1(1:natm,i),EXT(1:natm),'  - i,EXTH(i),EXTH(1:natm,i),EXT(1:natm)'
      end do ! i

!     if (abs(EXTH(NT+1)-(TEXT-TEXT_HP0))/(TEXT-TEXT_HP0) .gt. tiny) then
!        write(*,*)
!        write(*,'(a,i4,2(a,f12.8))') 'NT+1=',NT+1,'  TEXT=',TEXT-TEXT_HP0,'  EXTH(NT+1)=',EXTH(NT+1)
!        write(*,*) 'Total extinction .NE. EXTH(NT+1)'
!        stop 'stop in get_WD'
!     end if

      do i=1,NT
         TTH=SUM((EXTH1(1:natm,i+1)-EXTH1(1:natm,i)))
!         write(*,*) i,TTH,dtau,'  - i,TTH,dtau'

         do i1=1,natm
            WD(i,i1)=WD(i,i1)+(EXTH1(i1,i+1)-EXTH1(i1,i))*SSA(i1)/TTH
         end do ! i1
      end do ! i


!AL for testing purposes only
!      do i1=1,natm
!        write(*,*)'Component:', i1
!        write(*,*)'WD='
!        do i=1,NT
!           write(*,*) WD(i,i1)
!        enddo
!      enddo
!      stop 'AL:test stop in get_WD'
!AL for testing purposes only

      if(istop .eq. 1) stop 'test stop in get_WD'
!	------------------------------------------------------------------------------------------------------
      return
   end subroutine get_WD
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

END MODULE MOD_RT_SOS

