!.......................................................................
!     Generalized Retrieval of Atmosphere and Surface Properties
!     GRASP S.A.S
!.......................................................................
!
!     MODULE: MOD_RT_SOS_MS
!>    @author
!>    Xin Huang, GRASP S.A.S (Based on J. Lenoble et al., 2007 and developments By M.Herman and P. Litvinov)
!
!     DESCRIPTION:
!>    Multiple Scattering Calculation using Successive Order of
!>    Scattering Method
!
!     REVISION HISTORY:
!     13/02/2018 - Initial Version
!.......................................................................
MODULE MOD_RT_SOS_MS
!      USE MOD_PAR_OS
   IMPLICIT NONE

!>    variable precision
   INTEGER, PARAMETER :: KD  = 8
!>    pi
   REAL, PARAMETER :: PI  = 3.141592653589793
!>    degree to radiance
   REAL, PARAMETER :: D2R = PI/180.
!>    refer to eqn. (14), J. Lenoble et al., 2007
!>    RHO is Rayleigh depolarization
!>@{
   REAL, PARAMETER :: RHO = 0.0279
   REAL, PARAMETER :: XXX = 2.*(1.-RHO)/(2.+RHO)
   REAL, PARAMETER :: BETAM =       0.5 *XXX
   REAL, PARAMETER :: GAMMM = -SQRT(1.5)*XXX
   REAL, PARAMETER :: ALPHM =       3.  *XXX
!>@}

!.......................................................................
!     maximum of Gaussian quadratures for zenith angular integration
!      INTEGER, PARAMETER :: MNG  = NN0
!     maximum of Fourier terms
!      INTEGER, PARAMETER :: MNF  = NF0
!     maximum of layers of atmosphere
!      INTEGER, PARAMETER :: MNLY = KNT-1
!     maximum of levels of atmosphere
!      INTEGER, PARAMETER :: MNL = KNT
!     maximum of aerosol components
!      INTEGER, PARAMETER :: MNM  = NMM
!     maximum of observation geometry
!      INTEGER, PARAMETER :: MNV= NBVM
!.......................................................................

   TYPE :: RT_SOS_MS_OUT
!>        Stokes vector at observation directions @{
      REAL, DIMENSION(:),     POINTER :: SVI,  SVQ,  SVU
!>@}

!>        Fourier component of Stockes vector at Gaussian quadrature @{
      REAL, DIMENSION(:,:,:), POINTER :: SVIM, SVQM, SVUM
!>@}

!>        bi-hemispherical reflectance of surface
      REAL :: ASRF

!>        bi-hemispherical reflectance of atmosphere
      REAL, POINTER :: AATM

!>        directional hemispherical transmittance of atmosphere
!>        at Gaussian quadratures
      REAL, DIMENSION(:), POINTER :: TATM

!>        directional hemispherical reflectance/transmittance
!>        at each level @{
      REAL, DIMENSION(:), POINTER :: UFX, DFX
!>@}
   END TYPE RT_SOS_MS_OUT

CONTAINS

   SUBROUTINE RT_SOS_MS (                                            &
      RT_SOS_SET, RT_SOS_CTL,ATMOS_EMIS,         &
      NF, NP, THS, IL,                           &
      NG, UG, WG,                                &
      NV, IDV, CFV, PHV,                         &
      NM, ALPH, BETA, GAMM, ZETA,                &
      NL, TAU, WD,                               &
      R11, R21, R22,                             &
      R31, R32, R33,                             &
      BT, SBT, E0, SALB,                         &
      RT_OUT                                     &
      )
!.......................................................................
!     DESCRIPTION:
!>    Fourier Decomposition of Multiple Scattering
!
!>    @param[in]  RT_SOS_SET setup parameters of radiative transfer
!>    @param[in]  RT_SOS_CTL control parameters of radiative transfer
!>    @param[in]  NF maximum of Fourier terms
!>    @param[in]  NP number of GSF expansion terms
!>    @param[in]  THS solar zenith angle in degree
!>    @param[in]  IL index of level where results will be calculated
!>    @param[in]  NG number of Gaussian quadratures
!>    @param[in]  UG Gaussian quadrature points
!>    @param[in]  WG weights of Gaussian quadratures
!>    @param[in]  NV number of viewing directions
!>    @param[in]  IDV indices of viewing directions
!>    @param[in]  CFV coefficients of viewing directions
!>    @param[in]  PHV relative azimuth angles
!>    @param[in]  NM number of aerosol components
!>    @param[in]  ALPH GSF expansion coefficients
!>    @param[in]  BETA GSF expansion coefficients
!>    @param[in]  GAMM GSF expansion coefficients
!>    @param[in]  ZETA GSF expansion coefficients
!>    @param[in]  NL number of levels
!>    @param[in]  TAU optical thickness of each layer
!>    @param[in]  WD scattering fraction in each layer
!>    @param[in]  R11 Fourier decomposition of element of surface BRM
!>    @param[in]  R21 Fourier decomposition of element of surface BRM
!>    @param[in]  R22 Fourier decomposition of element of surface BRM
!>    @param[in]  R31 Fourier decomposition of element of surface BRM
!>    @param[in]  R32 Fourier decomposition of element of surface BRM
!>    @param[in]  R33 Fourier decomposition of element of surface BRM
!>    @param[out] RT_OUT various output of radiative transfer
!.......................................................................
      USE MOD_RT_SOS_SETUP, ONLY : RT_SOS_SETUP, RT_SOS_CNTRL
!.......................................................................
!     if using spline interpolation for solar direction
!.......................................................................
!      USE MOD_INTRPL_SPLINE, ONLY : INTRPL_SPLINE

!
      TYPE(RT_SOS_SETUP),INTENT(IN) :: RT_SOS_SET
      TYPE(RT_SOS_CNTRL),INTENT(IN) :: RT_SOS_CTL
      LOGICAL,INTENT(IN)            :: ATMOS_EMIS
      INTEGER,           INTENT(IN) :: NF, NP, NG, NV, IL, NL, NM
      REAL,              INTENT(IN) :: THS
      INTEGER, DIMENSION(NV),     INTENT(IN) :: IDV
      REAL,DIMENSION(NV),         INTENT(IN) :: PHV, CFV
      REAL(KD),DIMENSION(-NG:NG), INTENT(IN) :: UG, WG
      REAL,DIMENSION(NL),         INTENT(IN) :: TAU
      REAL,DIMENSION(NL-1,NM+1),  INTENT(IN) :: WD
      REAL,DIMENSION(0:NP,NM),    INTENT(IN) :: ALPH, BETA, GAMM, ZETA
!.......................................................................
!     due to reciprocity principle Rij(K,L,M)/UG(L) = Rji(L,K,M)/UG(K)
!.......................................................................
      REAL,DIMENSION(NG,NG,0:NF), INTENT(IN) :: R11, R21, R22
      REAL,DIMENSION(NG,NG,0:NF), INTENT(IN) :: R31, R32, R33
      REAL,DIMENSION(NL-1),       INTENT(IN) :: BT
      REAL,                       INTENT(IN) :: E0
      REAL,			              INTENT(IN) :: SALB, SBT
      TYPE(RT_SOS_MS_OUT),        INTENT(OUT):: RT_OUT
!
      INTEGER :: M, NMX, IG, IV, K, IU!, I, J
      REAL    :: US, CS, Z, XI, XQ, XU, ANG, LI
      REAL    :: RA, TA
!.......................................................................
!     if using spline interpolation for solar direction
!.......................................................................
!      REAL(KD) :: YS
!      REAL(KD), DIMENSION(NG)         :: SPL1, SPL2, SPL3, YG

!.......................................................................
!     negative angle for downward direction, and positive for upward
!.......................................................................
      REAL, DIMENSION(-NG:NG,NG,NM+1) :: P11,P21,P22,P31,P32,P33
      REAL, DIMENSION(-NG:NG,NL)      :: I1, Q1, U1
      REAL, DIMENSION(-NG:NG)         :: IM, QM, UM
      REAL, DIMENSION(:),     POINTER :: SVI, SVQ, SVU
      REAL, DIMENSION(:),     POINTER :: TATM
      REAL,                   POINTER :: AATM
      REAL, DIMENSION(:,:,:), POINTER :: SVIM,SVQM,SVUM
!
      I1(:,:) = 0.0
      Q1(:,:) = 0.0
      U1(:,:) = 0.0


      SVI => RT_OUT%SVI
      IF (RT_SOS_SET%IWUT .OR. RT_SOS_SET%ILUT) THEN
!.......................................................................
!        for look-up-table
!.......................................................................
         SVIM=> RT_OUT%SVIM
         TATM=> RT_OUT%TATM
         AATM=> RT_OUT%AATM
         IF (RT_SOS_SET%IWUT) AATM = 0.
      END IF
      IF (RT_SOS_CTL%IVEC) THEN
         SVQ => RT_OUT%SVQ
         SVU => RT_OUT%SVU
         IF (RT_SOS_SET%IWUT .OR. RT_SOS_SET%ILUT) THEN
            SVQM=> RT_OUT%SVQM
            SVUM=> RT_OUT%SVUM
         END IF
      END IF
!
      LP_SLR_ZTH: DO IU = 1, NG
!.......................................................................
!         for each incident angle
!.......................................................................
         IF (RT_SOS_SET%IWUT) THEN
!.......................................................................
!             IG is negative for LUT generation at exact Gaussian points
!.......................................................................
            IG = -IU
            US = UG(IU)
            CS = 1.
         ELSE
!.......................................................................
!             interpolation for incident solar zenith angle
!.......................................................................
            US = COS(THS*D2R)
            IF (US .LT. UG(NG)) THEN
               IG = NG-1
               DO WHILE (US .LT. UG(IG))
                  IG = IG-1
               END DO

               IF (IG .GT. 0) THEN
                  CS = (US-UG(IG+1))/(UG(IG)-UG(IG+1))
               ELSE
!.......................................................................
!                     when solar zenith angle is close to 90 degrees
!.......................................................................
                  CS = (US-UG(   1))/(      -UG(   1))
               END IF
            ELSE
!.......................................................................
!                 for the exact nadir direction
!.......................................................................
               IG = NG-1
               CS = 0.
            END IF
         END IF ! RT_SOS_SET%IWUT

!.......................................................................
!         print Gaussian quadrature angles
!.......................................................................
!          DO I = 1, NG
!              PRINT *, I, ACOS(UG(I))/D2R
!          END DO

!.......................................................................
! If we are working in the thermal infrared spectrum some mistake in the calculations
! introduces some undesirable azimuth dependance, to fix it, at least momentaneously
! we are we are setting in this case the fourier parameter to 0.
!.......................................................................

         LP_FR_EXP: DO M = 0, NF

            IF (RT_SOS_SET%ILUT) THEN

!.......................................................................
!                 linear interpolation
!.......................................................................
               IF (IG .GT. 0) THEN
                  IM(-NG:NG)= (1.-CS)*SVIM(-NG:NG,IG+1,M)           &
                     +    CS *SVIM(-NG:NG,IG  ,M)
                  IF (RT_SOS_CTL%IVEC) THEN
                     QM(-NG:NG)= (1.-CS)*SVQM(-NG:NG,IG+1,M)       &
                        +    CS *SVQM(-NG:NG,IG  ,M)
                     UM(-NG:NG)= (1.-CS)*SVUM(-NG:NG,IG+1,M)       &
                        +    CS *SVUM(-NG:NG,IG  ,M)
                  END IF
               ELSE IF (IG .EQ. 0) THEN
                  IM(-NG:NG)= (1.-CS)*SVIM(-NG:NG,   1,M)
                  IF (RT_SOS_CTL%IVEC) THEN
                     QM(-NG:NG)= (1.-CS)*SVQM(-NG:NG,   1,M)
                     UM(-NG:NG)= (1.-CS)*SVUM(-NG:NG,   1,M)
                  END IF
               END IF

!.......................................................................
!                 Lagrange interpolation
!.......................................................................
!                  IF (CS .EQ. 0.) THEN
!                      IM(-NG:NG) = SVIM(-NG:NG,IG+1,M)
!                      IF (RT_SOS_CTL%IVEC) THEN
!                          QM(-NG:NG) = SVQM(-NG:NG,IG+1,M)
!                          UM(-NG:NG) = SVUM(-NG:NG,IG+1,M)
!                      END IF
!                  ELSE IF (CS .EQ. 1.) THEN
!                      IM(-NG:NG) = SVIM(-NG:NG,IG,M)
!                      IF (RT_SOS_CTL%IVEC) THEN
!                          QM(-NG:NG) = SVQM(-NG:NG,IG,M)
!                          UM(-NG:NG) = SVUM(-NG:NG,IG,M)
!                      END IF
!                  ELSE
!                      IM(-NG:NG) = 0.
!                      IF (RT_SOS_CTL%IVEC) THEN
!                          QM(-NG:NG) = 0.
!                          UM(-NG:NG) = 0.
!                      END IF
!                      DO I = -NG, NG
!                          IF (I .EQ. 0) CYCLE
!                          LI = 1.
!                          DO J = -NG, NG
!                              IF (J .EQ. 0) CYCLE
!                              IF (J .NE. I) LI = LI*(US   -UG(J))       &
!                                                   /(UG(I)-UG(J))
!                          END DO
!                          IM(-NG:NG)=      IM(-NG:NG)                   &
!                                     +LI*SVIM(-NG:NG,ABS(I),M)
!                          IF (RT_SOS_CTL%IVEC) THEN
!                              QM(-NG:NG)=      QM(-NG:NG)               &
!                                         +LI*SVQM(-NG:NG,ABS(I),M)
!                              UM(-NG:NG)=      UM(-NG:NG)               &
!                                         +LI*SVUM(-NG:NG,ABS(I),M)
!                          END IF
!                      END DO
!                  END IF

!.......................................................................
!             spline interpolation
!.......................................................................
!              DO I = -NG, NG
!                  YG(1:NG) = REAL(SVIM(I,1:NG,M),KD)
!                  CALL INTRPL_SPLINE(NG, UG(1:NG), YG(1:NG),            &
!                                     REAL(US,KD), YS,                   &
!                                     1, 1, SPL1,SPL2,SPL3)
!                  IM(I) = REAL(YS)
!                  IF (RT_SOS_CTL%IVEC) THEN
!                      YG(1:NG) = REAL(SVQM(I,1:NG,M),KD)
!                      CALL INTRPL_SPLINE(NG, UG(1:NG), YG(1:NG),        &
!                                         REAL(US,KD), YS,               &
!                                         1, 1, SPL1,SPL2,SPL3)
!                      QM(I) = REAL(YS)
!                      YG(1:NG) = REAL(SVUM(I,1:NG,M),KD)
!                      CALL INTRPL_SPLINE(NG, UG(1:NG), YG(1:NG),        &
!                                         REAL(US,KD), YS,               &
!                                         1, 1, SPL1,SPL2,SPL3)
!                      UM(I) = REAL(YS)
!                  END IF
!              END DO

            ELSE

               CALL PHMX_M (                                            &
                  RT_SOS_CTL%IVEC,                           &
                  M,NP,NG,UG,                                &
                  NM,ALPH,BETA,GAMM,ZETA,                    &
                  NMX,P11,P21,P22,P31,P32,P33                &
                  )


               CALL ORD_ALL (                                           &
                  RT_SOS_CTL%IVEC,                          &
                  RT_SOS_CTL%ISRF,                          &
                  RT_SOS_CTL%IDWN,                          &
                  RT_SOS_CTL%IFLX,                          &
                  RT_SOS_SET%IWUT,                          &
                  ATMOS_EMIS,                               &
                  NM,M,IL,NL,TAU,WD(1:NL-1,1:NMX),          &
                  NMX,NG,UG,IG,CS,WG,US,                    &
                  BT(1:NL-1),SBT,E0, SALB,			       &
                  RT_SOS_SET%EPS,                           &
                  P11(-NG:NG,1:NG,1:NMX),                   &
                  P21(-NG:NG,1:NG,1:NMX),                   &
                  P22(-NG:NG,1:NG,1:NMX),                   &
                  P31(-NG:NG,1:NG,1:NMX),                   &
                  P32(-NG:NG,1:NG,1:NMX),                   &
                  P33(-NG:NG,1:NG,1:NMX),                   &
                  R11(1:NG,1:NG,M),R21(1:NG,1:NG,M),        &
                  R22(1:NG,1:NG,M),                         &
                  R31(1:NG,1:NG,M),R32(1:NG,1:NG,M),        &
                  R33(1:NG,1:NG,M),                         &
                  I1,Q1,U1,                                 &
                  IM,QM,UM,                                 &
                  RT_OUT%UFX,RT_OUT%DFX,                    &
                  RA,TA                                     &
                  )


               IF (RT_SOS_SET%IWUT) THEN
                  IF (M .EQ. 0) THEN
                     AATM = AATM + 2.*WG(-IG)*UG(-IG)*RA
                     TATM(-IG) = TA
                  END IF
                  SVIM(-NG:NG,-IG,M)=IM(-NG:NG)
                  IF (RT_SOS_CTL%IVEC) THEN
                     SVQM(-NG:NG,-IG,M)=QM(-NG:NG)
                     SVUM(-NG:NG,-IG,M)=UM(-NG:NG)
                  END IF
                  CYCLE LP_FR_EXP
               END IF
            END IF ! RT_SOS_SET%ILUT

            IF (RT_SOS_CTL%IFLX) EXIT LP_FR_EXP

            Z=0.
            DO IV=1,NV
               K=IDV(IV)
!PL Interpolation to observation angles for I component
               XI=IM(K+1)+CFV(IV)*(IM(K)-IM(K+1))
               Z=MAX(Z,ABS(XI))
               IF (M .EQ. 0) THEN
                  SVI(IV)=XI
               ELSE
                  ANG=M*(PHV(IV)-180.)*D2R
                  SVI(IV)=SVI(IV)+2.*COS(ANG)*XI
               END IF ! M .EQ. 0
            END DO ! IV=1,NV
            IF (RT_SOS_CTL%IVEC) THEN
               DO IV=1,NV
                  K=IDV(IV)
!PL Interpolation to observation angles fro Q and U components
                  XQ=QM(K+1)+CFV(IV)*(QM(K)-QM(K+1))
                  XU=UM(K+1)+CFV(IV)*(UM(K)-UM(K+1))
                  Z=MAX(Z,ABS(XQ),ABS(XU))
                  IF (M .EQ. 0) THEN
                     SVQ(IV)=XQ
                     SVU(IV)=0.
                  ELSE
                     ANG=M*(PHV(IV)-180.)*D2R
                     SVQ(IV)=SVQ(IV)+2.*COS(ANG)*XQ
                     SVU(IV)=SVU(IV)+2.*SIN(ANG)*XU
                  END IF
               END DO ! IV=1,NV
            END IF ! IVEC
            IF (Z .LT. RT_SOS_SET%EPS) EXIT LP_FR_EXP

         END DO LP_FR_EXP

         IF (.NOT. RT_SOS_SET%IWUT) EXIT LP_SLR_ZTH

      END DO LP_SLR_ZTH
!
      RETURN
   END SUBROUTINE RT_SOS_MS

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   SUBROUTINE ORD_ALL (                                      &
      IVEC,                          &
      ISRF,                          &
      IDWN,                          &
      IFLX,                          &
      IWUT,                          &
      ATMOS_EMIS,                    &
      NM,M,IL,NL,TAU,WD,             &
      NMX,NG,UG,IG,CS,WG,US,         &
      BT, SBT, E0, SALB,		        &
      EPS,                           &
      P11,P21,P22,P31,P32,P33,       &
      R11,R21,R22,                   &
      R31,R32,R33,                   &
      I1,Q1,U1,                      &
      IM,QM,UM,                      &
      UFX,DFX,                       &
      RA,TA                          &
      )


!MH		Selection of the calculation of the first order of scattering with/without emission part
!MH		The sucesive orders are independent of the wavelength range

!	------------------------------------------------------------------------------------
!   IN:
      LOGICAL,                           INTENT(IN) :: IVEC, ISRF, IDWN, IFLX, IWUT, ATMOS_EMIS
      INTEGER,                           INTENT(IN) :: M, NL, NG, NMX, IL, IG, NM
      REAL,                              INTENT(IN) :: US, EPS, CS
      REAL(KD),DIMENSION(-NG:NG),        INTENT(IN) :: UG, WG
      REAL,DIMENSION(NL),                INTENT(IN) :: TAU
      REAL,DIMENSION(NL-1,NMX),          INTENT(IN) :: WD
      REAL,DIMENSION(-NG:NG,NG,NMX),     INTENT(IN) :: P11,P21,P22
      REAL,DIMENSION(-NG:NG,NG,NMX),     INTENT(IN) :: P31,P32,P33
      REAL,DIMENSION(NG,NG),             INTENT(IN) :: R11, R21, R22
      REAL,DIMENSION(NG,NG),             INTENT(IN) :: R31, R32, R33
      REAL,DIMENSION(NL-1),              INTENT(IN) :: BT
      REAL,                              INTENT(IN) :: E0
      REAL, 					         INTENT(IN) :: SALB, SBT
!    ------------------------------------------------------------------------------------
!   INOUT:
      REAL,DIMENSION(-NG:NG,NL),         INTENT(INOUT) :: I1, Q1, U1
!    ------------------------------------------------------------------------------------
!   OUT:
      REAL,DIMENSION(-NG:NG),            INTENT(OUT)   :: IM, QM, UM
      REAL,                              INTENT(OUT)   :: RA, TA
!    ------------------------------------------------------------------------------------
!   LOCAL:
      REAL,DIMENSION(-NG:NG,NL)            :: I1S, Q1S, U1S, I1_temp, Q1_temp, U1_temp
      REAL,DIMENSION(:)                    :: UFX, DFX
      INTEGER                              :: L,J


      IF (.NOT. ATMOS_EMIS) THEN

!MH             In the case of Visible range, radiance is still normalized with E0=1
         CALL ORD1 (                                             &
            IVEC,                                     &
            ISRF,                                     &
            NL,TAU,WD,NMX,NG,UG,IG,US,CS,1.0,         &
            P11,P21,P31,                              &
            R11,R21,R31,                              &
            I1,Q1,U1                                  &
            )


         CALL ORDN (                                     &
            IVEC,                             &
            ISRF,                             &
            IDWN,                             &
            IFLX,                             &
            IWUT,                             &
            M,IL,NL,TAU,WD,NMX,NG,UG,WG,US,   &
            EPS,                              &
            P11,P21,P22,P31,P32,P33,          &
            R11,R21,R22,                      &
            R31,R32,R33,                      &
            I1,Q1,U1,                         &
            IM,QM,UM,                         &
            UFX,DFX,                          &
            RA,TA                             &
            )
!write(*,*) M,IL,NL,NMX,NG

      ELSE

!MH         Solar Scattering Calculation
         CALL ORD1 (                                         &
            IVEC,                                     &
            ISRF,                                     &
            NL,TAU,WD,NMX,NG,UG,IG,US,CS,E0,          &
            P11,P21,P31,                              &
            R11,R21,R31,                              &
            I1S,Q1S,U1S                               &
            )

!MH         Emission Calculation
         CALL ORD1_EMIS (                                        &
            IVEC,                                     &
            ISRF,                                     &
            NM,NL,TAU,WD,NMX,NG,UG,US,CS,             &
            BT, SBT, SALB,                            &
            P11,P21,P31,                              &
            R11,R21,R31,                              &
            I1,Q1,U1                                  &
            )

!MH      Sum of the total Radiance for Single Scattering
         DO L=1,NL
            DO J = -NG, NG
               I1(J,L) = I1(J,L) + I1S(J,L)
               Q1(J,L) = Q1(J,L) + Q1S(J,L)
               U1(J,L) = U1(J,L) + U1S(J,L)

            END DO
         END DO


         IF(M .EQ. 0) THEN

            DO L=1,NL
               DO J = -NG, NG
                  I1_temp(J,L) = I1(J,L)
                  Q1_temp(J,L) = Q1(J,L)
                  U1_temp(J,L) = U1(J,L)
               END DO
            END DO

         ELSE

            DO L=1,NL
               DO J = -NG, NG
                  I1_temp(J,L) = I1S(J,L)
                  Q1_temp(J,L) = Q1S(J,L)
                  U1_temp(J,L) = U1S(J,L)
               END DO
            END DO

         END IF

         CALL ORDN (                                   &
            IVEC,                             &
            ISRF,                             &
            IDWN,                             &
            IFLX,                             &
            IWUT,                             &
            M,IL,NL,TAU,WD,NMX,NG,UG,WG,US,   &
            EPS,                              &
            P11,P21,P22,P31,P32,P33,          &
            R11,R21,R22,                      &
            R31,R32,R33,                      &
            I1_temp,Q1_temp,U1_temp,          &
            IM,QM,UM,                         &
            UFX,DFX,                          &
            RA,TA                             &
            )
      END IF


   END SUBROUTINE ORD_ALL


!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


   SUBROUTINE ORD1_EMIS (                                            &
      IVEC,                                     &
      ISRF,                                     &
      NM,NL,TAU,WD,NMX,NG,UG,US,CS,             &
      BT, SBT, SALB,                            &
      P11,P21,P31,                              &
      R11,R21,R31,                              &
      I1,Q1,U1                                  &
      )
!.......................................................................
!     DESCRIPTION:
!>    First Order of Scattering
!
!>    @param[in]  IVEC using vector or scalar radiative transfer
!>    @param[in]  ISRF considering surface BRDF or not
!>    @param[in]  NL number of levels
!>    @param[in]  TAU optical thickness of each layer
!>    @param[in]  WD scattering fraction in each layer
!>    @param[in]  NMX number of atmosphere components
!>    @param[in]  NG number of Gaussian quadratures
!>    @param[in]  UG Gaussian quadrature points
!>    @param[in]  US solar zenith angle in radians
!>    @param[in]  CS coefficient of incident direction
!>    @param[in]  P11 element of phase matrix of atmosphere components
!>    @param[in]  P21 element of phase matrix of atmosphere components
!>    @param[in]  P31 element of phase matrix of atmosphere components
!>    @param[in]  R11 Fourier decomposition of element of surface BRM
!>    @param[in]  R21 Fourier decomposition of element of surface BRM
!>    @param[in]  R31 Fourier decomposition of element of surface BRM
!>    @param[out] I1 first order scattering contribution to I
!>    @param[out] Q1 first order scattering contribution to Q
!>    @param[out] U1 first order scattering contribution to U
!.......................................................................
!
      LOGICAL,                        INTENT(IN) :: IVEC, ISRF
      INTEGER,                        INTENT(IN) :: NL, NG, NMX, NM
      REAL,                           INTENT(IN) :: US, CS
      REAL(KD),DIMENSION(-NG:NG),     INTENT(IN) :: UG
      REAL,DIMENSION(NL),             INTENT(IN) :: TAU
      REAL,DIMENSION(NL-1,NMX),       INTENT(IN) :: WD
      REAL,DIMENSION(-NG:NG,NG,NMX),  INTENT(IN) :: P11,P21,P31
      REAL,DIMENSION(NG,NG),          INTENT(IN) :: R11, R21, R31
      REAL,DIMENSION(NL-1),           INTENT(IN) :: BT
      REAL,                           INTENT(IN) :: SALB, SBT


      REAL,DIMENSION(-NG:NG,NL), INTENT(OUT) :: I1, Q1, U1
!
      INTEGER :: L, J, FLG1
      REAL:: C, RT
      REAL, DIMENSION(-NG:NG,NL-1) :: SFI, SFQ, SFU
!
!.......................................................................
!     Vertical integration flag for single scattering
!     FLG1 = 0 : numerical integral over optical thickness assuming
!                that optical thickness of each layer is much smaller
!                than 1., but may be not accurate when optical thickness
!                is bigger than 1., even though wouldn't fail
!     FLG1 = 1 : analytical integral over optical thickness, but may
!                fail when optical thickness is much bigger than 1.
!.......................................................................
      FLG1 = 0
!.......................................................................
!    Mixing aerosols and Rayleigh components according to the weights WD
!.......................................................................
      DO L = 1, NL-1
         ! C = 0.25 !MH I dont understand this normalization, but I think in EMIS is nor necessary
         DO J = -NG, NG
!.......................................................................
!MH            EMIS is independent of the incident direction, so no calculation
!			   related with the IG parameter is necessary, also no polarization at all is
!			   taken into account.
!.......................................................................

            SFI(-J,L)= BT(L)* (1-SUM(WD(L,1:NMX)))

            IF (IVEC) THEN
               SFQ(-J,L)= 0.
               SFU(-J,L)= 0.
            END IF

         END DO ! J = -NG, NG
      END DO ! L = 1, NL-1


!.......................................................................
!     calculation of first order scattering
!.......................................................................
      I1(0,NL) = 0.
      I1(0, 1) = 0.
      IF (IVEC) THEN
         Q1(0,NL) = 0.
         Q1(0, 1) = 0.
         U1(0,NL) = 0.
         U1(0, 1) = 0.
         Q1(1:NG,NL) = 0.
         U1(1:NG,NL) = 0.
      END IF

      IF (ISRF) THEN

         I1(1:NG,NL)=(1-SALB)*SBT

      ELSE
         I1(1:NG,NL)=0.
      END IF ! ISRF


      DO L=NL-1,1,-1
         DO J = 1, NG
            CALL INT1_EMIS (                                          &
               IVEC,UG(J),TAU(L+1),TAU(L),                   &
               SFI(J,L),SFQ(J,L),SFU(J,L),                   &
               I1(J,L+1),Q1(J,L+1),U1(J,L+1),                &
               I1(J,L  ),Q1(J,L  ),U1(J,L  )                 &
               )
         END DO
      END DO ! L=NL-1,1,-1

      I1(-NG:-1,1) = 0.
      IF (IVEC) THEN
         Q1(-NG:-1,1)=0.
         U1(-NG:-1,1)=0.
      END IF
!.......................................................................
!     integration over TAU for downward direction Eq. (31a)
!.......................................................................

      DO L=2,NL
         DO J=-NG,-1
            CALL INT1_EMIS (                                          &
               IVEC,UG(J),TAU(L-1),TAU(L),                   &
               SFI(J,L-1),SFQ(J,L-1),SFU(J,L-1),             &
               I1(J,L-1),Q1(J,L-1),U1(J,L-1),                &
               I1(J,L  ),Q1(J,L  ),U1(J,L  )                 &
               )
         END DO
      END DO ! L=2,NL

      RETURN
   END	SUBROUTINE ORD1_EMIS

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


   SUBROUTINE ORDN (                                                 &
      IVEC,ISRF,IDWN,IFLX,IWUT,                       &
      M,IL,NL,TAU,WD,NMX,NG,UG,WG,US,EPS,             &
      P11,P21,P22,P31,P32,P33,                        &
      R11,R21,R22,                                    &
      R31,R32,R33,                                    &
      I1,Q1,U1,                                       &
      IM,QM,UM,                                       &
      UFX,DFX,                                        &
      RA,TA                                           &
      )
!.......................................................................
!     DESCRIPTION:
!>    Higher Order of Scattering
!
!>    @param[in]  IVEC using vector or scalar radiative transfer
!>    @param[in]  ISRF considering surface BRDF or not
!>    @param[in]  IDWN calculating downward or upward radiance
!>    @param[in]  IFLX calculating flux or not
!>    @param[in]  IWUT generating LUT or not
!>    @param[in]  M Mth Fourier component
!>    @param[in]  IL index of level where results will be calculated
!>    @param[in]  NL number of levels
!>    @param[in]  TAU optical thickness of each layer
!>    @param[in]  WD scattering fraction in each layer
!>    @param[in]  NMX number of atmosphere components
!>    @param[in]  NG number of Gaussian quadratures
!>    @param[in]  UG Gaussian quadrature points
!>    @param[in]  WG weights of Gaussian quadratures
!>    @param[in]  US solar zenith angle in radians
!>    @param[in]  EPS required precision
!>    @param[in]  P11 element of phase matrix of atmosphere components
!>    @param[in]  P21 element of phase matrix of atmosphere components
!>    @param[in]  P22 element of phase matrix of atmosphere components
!>    @param[in]  P31 element of phase matrix of atmosphere components
!>    @param[in]  P32 element of phase matrix of atmosphere components
!>    @param[in]  P33 element of phase matrix of atmosphere components
!>    @param[in]  R11 Fourier decomposition of element of surface BRM
!>    @param[in]  R21 Fourier decomposition of element of surface BRM
!>    @param[in]  R22 Fourier decomposition of element of surface BRM
!>    @param[in]  R31 Fourier decomposition of element of surface BRM
!>    @param[in]  R32 Fourier decomposition of element of surface BRM
!>    @param[in]  R33 Fourier decomposition of element of surface BRM
!>    @param[out] I1 first order scattering contribution to I
!>    @param[out] Q1 first order scattering contribution to Q
!>    @param[out] U1 first order scattering contribution to U
!>    @param[out] IM multiple scattering contribution to I
!>    @param[out] QM multiple scattering contribution to Q
!>    @param[out] UM multiple scattering contribution to U
!>    @param[out] UFX upward flux at each level
!>    @param[out] DFX downward flux at each level
!>    @param[out] RA reflectance of atmosphere for SZA = US
!>    @param[out] TA transmittance of atmosphere for SZA = US
!.......................................................................
      LOGICAL, INTENT(IN) :: IVEC, ISRF, IDWN, IFLX, IWUT
      INTEGER, INTENT(IN) :: M, NL, NG, NMX, IL
      REAL,INTENT(IN) :: US, EPS
      REAL(KD),DIMENSION(-NG:NG), INTENT(IN) :: UG, WG
      REAL,DIMENSION(NL), INTENT(IN) :: TAU
      REAL,DIMENSION(NL-1,NMX), INTENT(IN) :: WD
      REAL,DIMENSION(-NG:NG,NG,NMX), INTENT(IN) :: P11,P21,P22
      REAL,DIMENSION(-NG:NG,NG,NMX), INTENT(IN) :: P31,P32,P33
      REAL,DIMENSION(NG,NG), INTENT(IN) :: R11, R21, R22
      REAL,DIMENSION(NG,NG), INTENT(IN) :: R31, R32, R33
      REAL,DIMENSION(-NG:NG,NL), INTENT(INOUT) :: I1, Q1, U1
      REAL,DIMENSION(-NG:NG), INTENT(OUT) :: IM, QM, UM
      REAL,DIMENSION(:) :: UFX, DFX
      REAL, INTENT(OUT) :: RA, TA
!
      INTEGER, PARAMETER :: MN = 50
      INTEGER :: FLGN
      REAL:: X1, Y1, Z1, YI1, YQ1, YU1, INTL1, INTL2
      INTEGER :: N, L, J, K
      REAL:: C, W, X, Y, Z, YI, YQ, YU, RS, RS1
      REAL, DIMENSION(-NG:NG) :: I0
      REAL, DIMENSION(-NG:NG,NL-1) :: SFI, SFQ, SFU
      REAL, DIMENSION(:)  , ALLOCATABLE :: IMG, IMT
      REAL, DIMENSION(:,:), ALLOCATABLE :: IML
      REAL, DIMENSION(NG,NG) :: R12, R13, R23
!
!.......................................................................
!     order of Tylor series considered in vertical integration
!     for multiple scattering:
!     FLGN = 0 : numerical integral, upto the first Tylor expansion term
!     FLGN = 2 : upto the second Tylor expansion term
!.......................................................................
      FLGN = 2
!
!.......................................................................
!     reciprocity relation
!.......................................................................
      DO J = 1, NG
         R12(J,:) =R21(:,J)/UG(J)*UG(1:NG)
         R13(J,:) =R31(:,J)/UG(J)*UG(1:NG)
         R23(J,:) =R32(:,J)/UG(J)*UG(1:NG)
      END DO
!
      IF (M .EQ. 0) THEN
         I0(-NG:NG)=I1(-NG:NG,IL)
         IF (IWUT) THEN
            ALLOCATE(IMT(-NG:NG))
            ALLOCATE(IMG(-NG:NG))
            IMT = I1(-NG:NG,1 )
            IMG = I1(-NG:NG,NL)
         END IF
         IF (IFLX) THEN
            ALLOCATE(IML(-NG:NG,NL))
            IML = I1
         END IF
      END IF

      IM(-NG:NG)=0.
      IF (IVEC) THEN
         QM(-NG:NG)=0.
         UM(-NG:NG)=0.
      END IF
!write(*,*) 'wl'
      N=1
      LP_SCA_ORD: DO
         N=N+1
!write(*,*) 'order'
!write(*,*)'I1'
!DO L=1,NL
!write(*,*)'layer'
!write(*,*) I1(1:NG,L)
!end DO
        IF (IVEC) THEN
            DO L=1,NL-1
               DO J=-NG,-1
                  X=0.
                  Y=0.
                  Z=0.
                  IF (FLGN .EQ. 2) THEN
                     CALL INT1T2(UG(J),TAU(L+1),TAU(L),INTL1,INTL2)
                  END IF
                  DO K=1,NG
                     W=WG(K)
                     YI=I1(K,L+1)+I1(K,L)
                     YQ=Q1(K,L+1)+Q1(K,L)
                     YU=U1(K,L+1)+U1(K,L)
                     IF (FLGN .EQ. 2) THEN
                        YI1=I1(K,L+1)-I1(K,L)
                        YQ1=Q1(K,L+1)-Q1(K,L)
                        YU1=U1(K,L+1)-U1(K,L)
                        YI=INTL1*YI*0.5+INTL2*YI1
                        YQ=INTL1*YQ*0.5+INTL2*YQ1
                        YU=INTL1*YU*0.5+INTL2*YU1
                     END IF
                     X=X+W*DOT_PRODUCT( P11( J, K,1:NMX)*YI            &
                        +P21(-K,-J,1:NMX)*YQ            &
                        -P31(-K,-J,1:NMX)*YU,WD(L,1:NMX))
                     Y=Y+W*DOT_PRODUCT( P21( J, K,1:NMX)*YI            &
                        +P22( J, K,1:NMX)*YQ            &
                        -P32(-K,-J,1:NMX)*YU,WD(L,1:NMX))
                     Z=Z+W*DOT_PRODUCT( P31( J, K,1:NMX)*YI            &
                        +P32( J, K,1:NMX)*YQ            &
                        +P33( J, K,1:NMX)*YU,WD(L,1:NMX))
                  END DO ! K=1,NG
                  DO K=-NG,-1
                     W=WG(K)
                     YI=I1(K,L+1)+I1(K,L)
                     YQ=Q1(K,L+1)+Q1(K,L)
                     YU=U1(K,L+1)+U1(K,L)
                     IF (FLGN .EQ. 2) THEN
                        YI1=I1(K,L+1)-I1(K,L)
                        YQ1=Q1(K,L+1)-Q1(K,L)
                        YU1=U1(K,L+1)-U1(K,L)
                        YI=INTL1*YI*0.5+INTL2*YI1
                        YQ=INTL1*YQ*0.5+INTL2*YQ1
                        YU=INTL1*YU*0.5+INTL2*YU1
                     END IF
                     X=X+W*DOT_PRODUCT( P11(-J,-K,1:NMX)*YI            &
                        +P21(-K,-J,1:NMX)*YQ            &
                        -P31(-K,-J,1:NMX)*YU,WD(L,1:NMX))
                     Y=Y+W*DOT_PRODUCT( P21(-J,-K,1:NMX)*YI            &
                        +P22(-J,-K,1:NMX)*YQ            &
                        -P32(-K,-J,1:NMX)*YU,WD(L,1:NMX))
                     Z=Z+W*DOT_PRODUCT(-P31(-J,-K,1:NMX)*YI            &
                        -P32(-J,-K,1:NMX)*YQ            &
                        +P33(-J,-K,1:NMX)*YU,WD(L,1:NMX))
                  END DO ! K=-NG,-1
                  SFI(J,L)=0.5*X
                  SFQ(J,L)=0.5*Y
                  SFU(J,L)=0.5*Z
               END DO ! J=-NG,-1

               DO J=1,NG
                  X=0.
                  Y=0.
                  Z=0.
                  IF (FLGN .EQ. 2) THEN
                     CALL INT1T2(UG(J),TAU(L),TAU(L+1),INTL1,INTL2)
                  END IF
                  DO K=1,NG
                     W=WG(K)
                     YI=I1(K,L+1)+I1(K,L)
                     YQ=Q1(K,L+1)+Q1(K,L)
                     YU=U1(K,L+1)+U1(K,L)
                     IF (FLGN .EQ. 2) THEN
                        YI1=I1(K,L+1)-I1(K,L)
                        YQ1=Q1(K,L+1)-Q1(K,L)
                        YU1=U1(K,L+1)-U1(K,L)
                        YI=INTL1*YI*0.5+INTL2*YI1
                        YQ=INTL1*YQ*0.5+INTL2*YQ1
                        YU=INTL1*YU*0.5+INTL2*YU1
                     END IF
                     X=X+W*DOT_PRODUCT( P11( J, K,1:NMX)*YI            &
                        +P21( K, J,1:NMX)*YQ            &
                        +P31( K, J,1:NMX)*YU,WD(L,1:NMX))
                     Y=Y+W*DOT_PRODUCT( P21( J, K,1:NMX)*YI            &
                        +P22( J, K,1:NMX)*YQ            &
                        +P32( K, J,1:NMX)*YU,WD(L,1:NMX))
                     Z=Z+W*DOT_PRODUCT( P31( J, K,1:NMX)*YI            &
                        +P32( J, K,1:NMX)*YQ            &
                        +P33( J, K,1:NMX)*YU,WD(L,1:NMX))
                  END DO ! K=1,NG
                  DO K=-NG,-1
                     W=WG(K)
                     YI=I1(K,L+1)+I1(K,L)
                     YQ=Q1(K,L+1)+Q1(K,L)
                     YU=U1(K,L+1)+U1(K,L)
                     IF (FLGN .EQ. 2) THEN
                        YI1=I1(K,L+1)-I1(K,L)
                        YQ1=Q1(K,L+1)-Q1(K,L)
                        YU1=U1(K,L+1)-U1(K,L)
                        YI=INTL1*YI*0.5+INTL2*YI1
                        YQ=INTL1*YQ*0.5+INTL2*YQ1
                        YU=INTL1*YU*0.5+INTL2*YU1
                     END IF
                     X=X+W*DOT_PRODUCT( P11(-J,-K,1:NMX)*YI            &
                        +P21( K, J,1:NMX)*YQ            &
                        +P31( K, J,1:NMX)*YU,WD(L,1:NMX))
                     Y=Y+W*DOT_PRODUCT( P21(-J,-K,1:NMX)*YI            &
                        +P22(-J,-K,1:NMX)*YQ            &
                        +P32( K, J,1:NMX)*YU,WD(L,1:NMX))
                     Z=Z+W*DOT_PRODUCT(-P31(-J,-K,1:NMX)*YI            &
                        -P32(-J,-K,1:NMX)*YQ            &
                        +P33(-J,-K,1:NMX)*YU,WD(L,1:NMX))
                  END DO ! K=-NG,-1
                  SFI(J,L)=0.5*X
                  SFQ(J,L)=0.5*Y
                  SFU(J,L)=0.5*Z
               END DO ! J=1,NG
            END DO ! L=1,NL-1
        ELSE
            DO L=1,NL-1
               DO J=-NG,-1
                  X=0.
                  IF (FLGN .EQ. 2) THEN
                     CALL INT1T2(UG(J),TAU(L+1),TAU(L),INTL1,INTL2)
                  END IF
                  DO K=1,NG
                     W=WG(K)
                     YI=I1(K,L+1)+I1(K,L)
                     IF (FLGN .EQ. 2) THEN
                        YI1=I1(K,L+1)-I1(K,L)
                        YI=INTL1*YI*0.5+INTL2*YI1
                     END IF
                     X=X+W*YI*DOT_PRODUCT(P11( J, K,1:NMX),WD(L,1:NMX))
                  END DO ! K=1,NG
                  DO K=-NG,-1
                     W=WG(K)
                     YI=I1(K,L+1)+I1(K,L)
                     IF (FLGN .EQ. 2) THEN
                        YI1=I1(K,L+1)-I1(K,L)
                        YI=INTL1*YI*0.5+INTL2*YI1
                     END IF
                     X=X+W*YI*DOT_PRODUCT(P11(-J,-K,1:NMX),WD(L,1:NMX))
                  END DO ! K=-NG,-1
                  SFI(J,L)=0.5*X

               END DO ! J=-NG,-1

               DO J=1,NG
                  X=0.
                  IF (FLGN .EQ. 2) THEN
                     CALL INT1T2(UG(J),TAU(L),TAU(L+1),INTL1,INTL2)
                  END IF
                  DO K=1,NG
                     W=WG(K)
                     YI=I1(K,L+1)+I1(K,L)
                     IF (FLGN .EQ. 2) THEN
                        YI1=I1(K,L+1)-I1(K,L)
                        YI=INTL1*YI*0.5+INTL2*YI1
                     END IF
                     X=X+W*YI*DOT_PRODUCT(P11( J, K,1:NMX),WD(L,1:NMX))
                  END DO ! K=1,NG
                  DO K=-NG,-1
                     W=WG(K)
                     YI=I1(K,L+1)+I1(K,L)
                     IF (FLGN .EQ. 2) THEN
                        YI1=I1(K,L+1)-I1(K,L)
                        YI=INTL1*YI*0.5+INTL2*YI1
                     END IF
                     X=X+W*YI*DOT_PRODUCT(P11(-J,-K,1:NMX),WD(L,1:NMX))
                  END DO ! K=-NG,-1
                  SFI(J,L)=0.5*X

               END DO ! J=-NG,-1

            END DO ! L=1,NT-1
!write(*,*)'SFI'
!DO L=1,NL
!write(*,*)'layer'
!write(*,*) SFI(1:NG,L)
!end DO
!write(*,*)'order'

         END IF ! IVEC

         IF (ISRF) THEN
            IF (IVEC) THEN
               DO J=1,NG
                  I1(J,NL)=2.*SUM( WG(1:NG)                         &
                     *(I1(-1:-NG:-1,NL)*R11(J,:)       &
                     +Q1(-1:-NG:-1,NL)*R12(J,:)       &
                     +U1(-1:-NG:-1,NL)*R13(J,:)))
                  Q1(J,NL)=2.*SUM( WG(1:NG)                         &
                     *(I1(-1:-NG:-1,NL)*R21(J,:)       &
                     +Q1(-1:-NG:-1,NL)*R22(J,:)       &
                     +U1(-1:-NG:-1,NL)*R23(J,:)))
                  U1(J,NL)=2.*SUM( WG(1:NG)                         &
                     *(I1(-1:-NG:-1,NL)*R31(J,:)       &
                     +Q1(-1:-NG:-1,NL)*R32(J,:)       &
                     +U1(-1:-NG:-1,NL)*R33(J,:)))
               END DO
            ELSE
               DO J=1,NG
                  I1(J,NL)=2.*SUM( WG(1:NG)                         &
                     *I1(-1:-NG:-1,NL)*R11(J,:))
               END DO
            END IF
         ELSE
            I1(1:NG,NL)=0.
            IF (IVEC) THEN
               Q1(1:NG,NL)=0.
               U1(1:NG,NL)=0.
            END IF
         END IF ! ISRF

         DO L=NL-1,1,-1
            DO J = 1, NG
               CALL INTN (                                           &
                  IVEC,FLGN,UG(J),US,TAU(L+1),TAU(L),       &
                  SFI(J,L),SFQ(J,L),SFU(J,L),               &
                  I1(J,L+1),Q1(J,L+1),U1(J,L+1),            &
                  I1(J,L  ),Q1(J,L  ),U1(J,L  )             &
                  )
            END DO
         END DO ! L=NL-1,1,-1


         I1(-NG:-1,1)=0.
         IF (IVEC) THEN
            Q1(-NG:-1,1)=0.
            U1(-NG:-1,1)=0.
         END IF

         DO L=2,NL
            DO J=-NG,-1
               CALL INTN (                                           &
                  IVEC,FLGN,UG(J),US,TAU(L-1),TAU(L),       &
                  SFI(J,L-1),SFQ(J,L-1),SFU(J,L-1),         &
                  I1(J,L-1),Q1(J,L-1),U1(J,L-1),            &
                  I1(J,L  ),Q1(J,L  ),U1(J,L  )             &
                  )
            END DO
         END DO ! L=2,NL
!write(*,*)'after'
!DO L=1,NL
!write(*,*)'layer'
!write(*,*) SFI(-NG:-1,L)
!end DO
!write(*,*) 'order'

         IF (IDWN) THEN
            IM(-NG:-1)=IM(-NG:-1)+I1(-NG:-1,IL)
            IF (IVEC) THEN
               QM(-NG:-1)=QM(-NG:-1)+Q1(-NG:-1,IL)
               UM(-NG:-1)=UM(-NG:-1)+U1(-NG:-1,IL)
            END IF
         ELSE
            IM(1:NG)=IM(1:NG)+I1(1:NG,IL)
            IF (IVEC) THEN
               QM(1:NG)=QM(1:NG)+Q1(1:NG,IL)
               UM(1:NG)=UM(1:NG)+U1(1:NG,IL)
            END IF
         END IF ! IDWN

         IF (M .EQ. 0) THEN
            IF (IWUT) THEN
               IMT(-NG:NG)=IMT(-NG:NG)+I1(-NG:NG,1 )
               IMG(-NG:NG)=IMG(-NG:NG)+I1(-NG:NG,NL)
            END IF
            IF (IFLX) THEN
               IML(-NG:NG,1:NL)=IML(-NG:NG,1:NL)+I1(-NG:NG,1:NL)
            END IF
            IF (N .EQ. MN) THEN
               RS = SUM(I1(1:NG,IL)/I0(1:NG))/NG
               RS1 = RS/(1.-RS)
               IF (IDWN) THEN
                  IM(-NG:-1)=IM(-NG:-1)+RS1*I1(-NG:-1,IL)
               ELSE
                  IM(  1:NG)=IM(  1:NG)+RS1*I1(  1:NG,IL)
               END IF
               IF (IWUT) THEN
                  IMT(-NG:NG)= IMT(-NG:NG)+RS1*I1(-NG:NG,1 )
                  IMG(-NG:NG)= IMG(-NG:NG)+RS1*I1(-NG:NG,NL)
               END IF
               IF (IFLX) THEN
                  IML(-NG:NG,1:NL)= IML(-NG:NG,1:NL)                &
                     +RS1*I1(-NG:NG,1:NL)
               END IF
               EXIT LP_SCA_ORD
            END IF ! N .EQ. MN
         END IF ! M .EQ. 0

         IF (IDWN) THEN
            Z=MAX(0.,MAXVAL(ABS(I1(-NG:-1,IL))))
            IF (IVEC) THEN
               Z=MAX(Z,MAXVAL(ABS(Q1(-NG:-1,IL))),                   &
                  MAXVAL(ABS(U1(-NG:-1,IL))))
            END IF
         ELSE
            Z=MAX(0.,MAXVAL(ABS(I1(  1:NG,IL))))
            IF (IVEC) THEN
               Z=MAX(Z,MAXVAL(ABS(Q1(  1:NG,IL))),                   &
                  MAXVAL(ABS(U1(  1:NG,IL))))
            END IF
         END IF ! IDWN
         IF (Z .LE. EPS .OR. N .GT. MN) EXIT LP_SCA_ORD
      END DO LP_SCA_ORD

      IF (M .EQ. 0) THEN
         IF (IWUT) THEN
!.......................................................................
!             reflectance at TOA and transmittance at BOA
!.......................................................................
            RA= 2.*SUM(WG(1:NG)*UG(1:NG)*IMT( 1: NG   ))/US
            TA= EXP(-TAU(NL)/US)                                      &
               +2.*SUM(WG(1:NG)*UG(1:NG)*IMG(-1:-NG:-1))/US
            DEALLOCATE(IMT)
            DEALLOCATE(IMG)
         END IF
         IF (IFLX) THEN
            DO L=1,NL
               UFX(L)= 2.*SUM(WG(1:NG)*UG(1:NG)*IML( 1: NG,   L))
               DFX(L)= US*EXP(-TAU(L)/US)                            &
                  +2.*SUM(WG(1:NG)*UG(1:NG)*IML(-1:-NG:-1,L))
            END DO
            DEALLOCATE(IML)
         END IF
      END IF
!
      RETURN
   END SUBROUTINE ORDN

   SUBROUTINE ORD1 (                                                 &
      IVEC,                                           &
      ISRF,                                           &
      NL,TAU,WD,NMX,NG,UG,IG,US,CS,E0,                &
      P11,P21,P31,                                    &
      R11,R21,R31,                                    &
      I1,Q1,U1                                        &
      )
!.......................................................................
!     DESCRIPTION:
!>    First Order of Scattering
!
!>    @param[in]  IVEC using vector or scalar radiative transfer
!>    @param[in]  ISRF considering surface BRDF or not
!>    @param[in]  NL number of levels
!>    @param[in]  TAU optical thickness of each layer
!>    @param[in]  WD scattering fraction in each layer
!>    @param[in]  NMX number of atmosphere components
!>    @param[in]  NG number of Gaussian quadratures
!>    @param[in]  UG Gaussian quadrature points
!>    @param[in]  IG index of incident direction
!>    @param[in]  US solar zenith angle in radians
!>    @param[in]  CS coefficient of incident direction
!>    @param[in]  P11 element of phase matrix of atmosphere components
!>    @param[in]  P21 element of phase matrix of atmosphere components
!>    @param[in]  P31 element of phase matrix of atmosphere components
!>    @param[in]  R11 Fourier decomposition of element of surface BRM
!>    @param[in]  R21 Fourier decomposition of element of surface BRM
!>    @param[in]  R31 Fourier decomposition of element of surface BRM
!>    @param[out] I1 first order scattering contribution to I
!>    @param[out] Q1 first order scattering contribution to Q
!>    @param[out] U1 first order scattering contribution to U
!.......................................................................
!
      LOGICAL, INTENT(IN) :: IVEC, ISRF
      INTEGER, INTENT(IN) :: NL, NG, NMX, IG
      REAL,INTENT(IN) :: US, CS, E0
      REAL(KD),DIMENSION(-NG:NG), INTENT(IN) :: UG
      REAL,DIMENSION(NL), INTENT(IN) :: TAU
      REAL,DIMENSION(NL-1,NMX), INTENT(IN) :: WD
      REAL,DIMENSION(-NG:NG,NG,NMX), INTENT(IN) :: P11,P21,P31
      REAL,DIMENSION(NG,NG), INTENT(IN) :: R11, R21, R31
      REAL,DIMENSION(-NG:NG,NL), INTENT(OUT) :: I1, Q1, U1
!
      INTEGER :: L, J, FLG1
      REAL:: C, RT
      REAL, DIMENSION(-NG:NG,NL-1) :: SFI, SFQ, SFU
!
!.......................................................................
!     Vertical integration flag for single scattering
!     FLG1 = 0 : numerical integral over optical thickness assuming
!                that optical thickness of each layer is much smaller
!                than 1., but may be not accurate when optical thickness
!                is bigger than 1., even though wouldn't fail
!     FLG1 = 1 : analytical integral over optical thickness, but may
!                fail when optical thickness is much bigger than 1.
!.......................................................................
      FLG1 = 0
!.......................................................................
!    Mixing aerosols and Rayleigh components according to the weights WD
!.......................................................................
      DO L = 1, NL-1
         C = 0.25
         DO J = -NG, NG
!.......................................................................
!            Incident raidation is assumed to be unpolarized as for
!            passive remote sensing. Therefore, the interpolation is
!            performed only for the elements P11, P21, and P31. The
!            sign of P31 is changed to satisfy angular convention
!            defined for POLDER Stokes parameter U
!.......................................................................
            IF (IG .GT. 0) THEN
               SFI(-J,L)=C*DOT_PRODUCT(WD(L,1:NMX),                      &
                  (1.-CS)*P11(J,IG+1,1:NMX)+CS*P11(J,IG,1:NMX)) * E0
               IF (IVEC) THEN
                  SFQ(-J,L)= C*DOT_PRODUCT(WD(L,1:NMX),                 &
                     (1.-CS)*P21(J,IG+1,1:NMX)+CS*P21(J,IG,1:NMX)) * E0
                  SFU(-J,L)=-C*DOT_PRODUCT(WD(L,1:NMX),                 &
                     (1.-CS)*P31(J,IG+1,1:NMX)+CS*P31(J,IG,1:NMX)) * E0
               END IF
            ELSE IF (IG .EQ. 0) THEN
!.......................................................................
!                 when solar zenith angle is close to 90 degrees
!                 assuming plane-parallel model
!.......................................................................
               SFI(-J,L)=C*DOT_PRODUCT(WD(L,1:NMX),                      &
                  (1.-CS)*P11(J,   1,1:NMX)) * E0
               IF (IVEC) THEN
                  SFQ(-J,L)= C*DOT_PRODUCT(WD(L,1:NMX),                 &
                     (1.-CS)*P21(J,   1,1:NMX)) * E0
                  SFU(-J,L)=-C*DOT_PRODUCT(WD(L,1:NMX),                 &
                     (1.-CS)*P31(J,   1,1:NMX)) * E0
               END IF
            ELSE IF (IG .LT. 0) THEN
               SFI(-J,L)=C*DOT_PRODUCT(WD(L,1:NMX),P11(J,-IG,1:NMX)) * E0
               IF (IVEC) THEN
                  SFQ(-J,L)= C*DOT_PRODUCT(WD(L,1:NMX),P21(J,-IG,1:NMX)) * E0
                  SFU(-J,L)=-C*DOT_PRODUCT(WD(L,1:NMX),P31(J,-IG,1:NMX)) * E0
               END IF
            END IF ! IG .GT. 0
         END DO ! J = -NG, NG
      END DO ! L = 1, NL-1

!.......................................................................
!     calculation of first order scattering
!.......................................................................
      I1(0,NL) = 0.
      I1(0, 1) = 0.
      IF (IVEC) THEN
         Q1(0,NL) = 0.
         Q1(0, 1) = 0.
         U1(0,NL) = 0.
         U1(0, 1) = 0.
      END IF

      IF (ISRF) THEN
!.......................................................................
!         interpolation of Mth Fourier component (Rij) of the surface
!         reflection matrix for incident geometry
!.......................................................................
         RT = EXP(-TAU(NL)/US)*E0
         IF (IG .GT. 0) THEN
            I1(1:NG,NL)=((1.-CS)*R11(:,IG+1)+CS*R11(:,IG))*RT
            IF (IVEC) THEN
               Q1(1:NG,NL)=((1.-CS)*R21(:,IG+1)+CS*R21(:,IG))*RT
               U1(1:NG,NL)=((1.-CS)*R31(:,IG+1)+CS*R31(:,IG))*RT
            END IF
         ELSE IF (IG .EQ. 0) THEN
            I1(1:NG,NL)=(1.-CS)*R11(:,1)*RT
            IF (IVEC) THEN
               Q1(1:NG,NL)=(1.-CS)*R21(:,1)*RT
               U1(1:NG,NL)=(1.-CS)*R31(:,1)*RT
            END IF
         ELSE IF (IG .LT. 0) THEN
            I1(1:NG,NL)=R11(:,-IG)*RT
            IF (IVEC) THEN
               Q1(1:NG,NL)=R21(:,-IG)*RT
               U1(1:NG,NL)=R31(:,-IG)*RT
            END IF
         END IF ! IG .GT. 0
      ELSE
         I1(1:NG,NL)=0.
         IF (IVEC) THEN
            Q1(1:NG,NL)=0.
            U1(1:NG,NL)=0.
         END IF
      END IF ! ISRF

!.......................................................................
!     Combine aerosol-Rayleigh with surface reflection and integrate
!     over TAU for upward direction, refer to Eq. (31b) J. Lenoble,
!     M. Herman et al, JQSRT, 2007.
!.......................................................................

!MH The same function is used to vertically integrate source function for up/downward integration, just the order of previous/next tau in the input has to be change

      DO L=NL-1,1,-1
         DO J = 1, NG
            CALL INT1 (                                               &
               IVEC,FLG1,UG(J),US,TAU(L+1),TAU(L),           &
               SFI(J,L),SFQ(J,L),SFU(J,L),                   &
               I1(J,L+1),Q1(J,L+1),U1(J,L+1),                &
               I1(J,L  ),Q1(J,L  ),U1(J,L  )                 &
               )
         END DO
      END DO ! L=NL-1,1,-1

      I1(-NG:-1,1) = 0.
      IF (IVEC) THEN
         Q1(-NG:-1,1)=0.
         U1(-NG:-1,1)=0.
      END IF

!.......................................................................
!     integration over TAU for downward direction Eq. (31a)
!.......................................................................
      DO L=2,NL
         DO J=-NG,-1
            CALL INT1 (                                               &
               IVEC,FLG1,UG(J),US,TAU(L-1),TAU(L),           &
               SFI(J,L-1),SFQ(J,L-1),SFU(J,L-1),             &
               I1(J,L-1),Q1(J,L-1),U1(J,L-1),                &
               I1(J,L  ),Q1(J,L  ),U1(J,L  )                 &
               )
         END DO
      END DO ! L=2,NL
!
      RETURN
   END SUBROUTINE ORD1

   SUBROUTINE PHMX_M (                                               &
      IVEC,M,NP,NG,UG,                              &
      NM,ALPH,BETA,GAMM,ZETA,                       &
      NMX,P11,P21,P22,P31,P32,P33                   &
      )
!.......................................................................
!     DESCRIPTION:
!>    Mth Fourier Terms of Phase Matrix of Each Atmosphere Components
!
!>    @param[in]  IVEC using vector or scalar radiative transfer
!>    @param[in]  M Mth Fourier component
!>    @param[in]  NP number of GSF expansion terms
!>    @param[in]  NG number of Gaussian quadratures
!>    @param[in]  UG Gaussian quadrature points
!>    @param[in]  NM number of aerosol components
!>    @param[in]  ALPH GSF expansion coefficients
!>    @param[in]  BETA GSF expansion coefficients
!>    @param[in]  GAMM GSF expansion coefficients
!>    @param[in]  ZETA GSF expansion coefficients
!>    @param[out] NMX number of atmosphere components
!>    @param[out] P11 element of phase matrix of atmosphere components
!>    @param[out] P21 element of phase matrix of atmosphere components
!>    @param[out] P22 element of phase matrix of atmosphere components
!>    @param[out] P31 element of phase matrix of atmosphere components
!>    @param[out] P32 element of phase matrix of atmosphere components
!>    @param[out] P33 element of phase matrix of atmosphere components
!.......................................................................
!
      LOGICAL, INTENT(IN) :: IVEC
      INTEGER, INTENT(IN) :: M, NP, NG, NM
      REAL(KD),DIMENSION(-NG:NG),INTENT(IN) :: UG
      REAL,DIMENSION(0:NP,NM),INTENT(IN) :: ALPH, BETA, GAMM, ZETA
      INTEGER, INTENT(OUT):: NMX
      REAL,DIMENSION(-NG:NG,NG,NM+1),INTENT(OUT) :: P11, P21, P22
      REAL,DIMENSION(-NG:NG,NG,NM+1),INTENT(OUT) :: P31, P32, P33
!
      INTEGER :: J, K, L
      REAL:: CK, SK, CJ, SJ
      REAL:: CJ2, SJ2
      REAL,DIMENSION(0:NP,-NG:NG) :: PML, RML, TML
!
      IF (M .GT. 2) THEN
         NMX = NM
      ELSE
         NMX = NM+1
         DO K = 1, NG
            CK = UG(K)
            SK = SQRT(1.-CK*CK)
            DO J = -NG, NG
               CJ = UG(J)
               SJ = SQRT(1.-CJ*CJ)
               SELECT CASE(M)
                CASE(0)
                  P11(J,K,NMX)=1. + 0.25*BETAM                      &
                     *(3.*CJ*CJ-1.)                &
                     *(3.*CK*CK-1.)
                  IF (IVEC) THEN
                     P21(J,K,NMX)= SQRT(0.09375)*GAMMM             &
                        *SJ*SJ*(3.*CK*CK-1.)
                     P22(J,K,NMX)=        0.375 *ALPHM             &
                        *SJ*SJ*       SK*SK
                     P31(j,k,NMX)=           0.
                     P32(j,k,NMX)=           0.
                     P33(j,k,NMX)=           0.
                  END IF
                CASE(1)
                  P11(J,K,NMX)=1.5*BETAM*CJ*SJ*CK*SK
                  IF (IVEC) THEN
                     P21(J,K,NMX)=-SQRT(0.375)*GAMMM*CJ*SJ*CK*SK
                     P22(J,K,NMX)=       0.25 *ALPHM*CJ*SJ*CK*SK
                     P31(J,K,NMX)= SQRT(0.375)*GAMMM   *SJ*CK*SK
                     P32(J,K,NMX)=      -0.25 *ALPHM   *SJ*CK*SK
                     P33(J,K,NMX)=       0.25 *ALPHM   *SJ   *SK
                  END IF
                CASE(2)
                  P11(J,K,NMX)= 0.375                               &
                     *BETAM*(1.-CJ*CJ)*(1.-CK*CK)
                  IF (IVEC) THEN
                     P21(J,K,NMX)= 0.25*SQRT(0.375)*GAMMM          &
                        *(1.+CJ*CJ)*(1.-CK*CK)
                     P22(J,K,NMX)=             0.0625 *ALPHM       &
                        *(1.+CJ*CJ)*(1.+CK*CK)
                     P31(J,K,NMX)= -0.5*SQRT(0.375)*GAMMM          &
                        *       CJ *(1.-CK*CK)
                     P32(J,K,NMX)=             -0.125 *ALPHM       &
                        *       CJ *(1.+CK*CK)
                     P33(J,K,NMX)=               0.25 *ALPHM       &
                        *       CJ *       CK
                  END IF
               END SELECT
            END DO ! J = -NG, NG
         END DO ! K = 1, NG
      END IF ! M .GT. 2

      PML=0.
      IF (IVEC) THEN
         TML=0.
         RML=0.
         CALL LEGENDRE( M,NG,UG,NP,PML,TML,RML )
         DO L = 1, NM
            DO K = 1, NG
               DO J = -NG, NG
                  P21(J,K,L)= SUM(GAMM(M:NP-2,L)*RML(M:NP-2,J)      &
                     *PML(M:NP-2,K))
                  P31(J,K,L)=-SUM(GAMM(M:NP-2,L)*TML(M:NP-2,J)      &
                     *PML(M:NP-2,K))
                  P22(J,K,L)= SUM(ALPH(M:NP-2,L)*RML(M:NP-2,J)      &
                     *RML(M:NP-2,K))     &
                     +SUM(ZETA(M:NP-2,L)*TML(M:NP-2,J)      &
                     *TML(M:NP-2,K))
                  P33(J,K,L)= SUM(ALPH(M:NP-2,L)*TML(M:NP-2,J)      &
                     *TML(M:NP-2,K))     &
                     +SUM(ZETA(M:NP-2,L)*RML(M:NP-2,J)      &
                     *RML(M:NP-2,K))
                  P32(J,K,L)=-SUM(ALPH(M:NP-2,L)*TML(M:NP-2,J)      &
                     *RML(M:NP-2,K))     &
                     -SUM(ZETA(M:NP-2,L)*RML(M:NP-2,J)      &
                     *TML(M:NP-2,K))
               END DO
            END DO
         END DO
      ELSE
         CALL LEGENDRE ( M,NG,UG,NP,PML )
      END IF ! IVEC
      DO L = 1, NM
         DO K = 1, NG
            DO J = -NG, NG
               P11(J,K,L)=SUM(BETA(M:NP-2,L)*PML(M:NP-2,J)           &
                  *PML(M:NP-2,K))
            END DO
         END DO
      END DO
!
      RETURN
   END SUBROUTINE PHMX_M

   SUBROUTINE LEGENDRE ( M,N,U,L,PML,TML,RML )
!.......................................................................
!     DESCRIPTION:
!>    GSFs at U for Mth Fourier Component and Order 0 to L
!
!>    @param[in]  M Mth Fourier component
!>    @param[in]  N number of U
!>    @param[in]  U variable of GSF
!>    @param[in]  L maximum momentum
!>    @param[out] PML generalized spherical function
!>    @param[out] TML generalized spherical function
!>    @param[out] RML generalized spherical function
!.......................................................................
      INTEGER,                            INTENT(IN)  :: M, N, L
      REAL(KD),DIMENSION(-N:N),           INTENT(IN)  :: U
      REAL,DIMENSION(0:L,-N:N),           INTENT(OUT) :: PML
      REAL,DIMENSION(0:L,-N:N), OPTIONAL, INTENT(OUT) :: RML, TML
!
      INTEGER :: I, J, K, IP, IM
      REAL:: CJ, SJ2, A, B, C, D, E, F
!
      SELECT CASE(M)
       CASE(0)
         DO J = -N, N
            CJ=U(J)
            PML(0,J)=1.
            PML(1,J)=CJ
            PML(2,J)=1.5*CJ*CJ-0.5
            IF (PRESENT(TML)) THEN
               TML(1,J)=0.
               TML(2,J)=0.
               RML(1,J)=0.
               RML(2,J)=1.5/SQRT(6.)*(1.-CJ*CJ)
            END IF
         END DO ! J = -N, N
       CASE(1)
         DO J = -N, N
            CJ=U(J)
            SJ2=1.-CJ*CJ
            PML(0,J)=0.
            PML(1,J)=SQRT(0.5*SJ2)
            PML(2,J)=SQRT(3.)*CJ*PML(1,J)
            IF (PRESENT(TML)) THEN
               TML(1,J)= 0.
               TML(2,J)=-0.5*SQRT(SJ2)
               RML(1,J)= 0.
               RML(2,J)= CJ*TML(2,J)
            END IF
         END DO ! J = -N, N
       CASE DEFAULT
         A=1.
         DO I =1, M
            A=A*SQRT(REAL(I+M,KD)/I)*0.5
         END DO
         B=A*SQRT(M/(M+1.))*SQRT((M-1.)/(M+2.))
         DO J = -N, N
            CJ = U(J)
            SJ2=1.-CJ*CJ
            C  =M*0.5-1.
            PML(M-1,J)=0.
            PML(M,J)=A*SJ2**(M*0.5)
            IF (PRESENT(TML)) THEN
               D = B*SJ2**C
               TML(M-1,J)=0.
               TML(M,  J)=2.*CJ*D
               RML(M-1,J)=0.
               RML(M,  J)=(1.+CJ*CJ)*D
            END IF
         END DO
      END SELECT

      K=MAX(2,M)
      IF (K .NE. L-2) THEN
         DO I = K, L-1
            IP=I+1
            IM=I-1
            A=(I+I+1.)/SQRT((I+M+1.)*(I-M+1.))
            B=SQRT(REAL((I+M)*(I-M)))/(I+I+1.)
            PML(IP,-N:N)=A*(U(-N:N)*PML(I,-N:N)-B*PML(IM,-N:N))
            IF (PRESENT(TML)) THEN
               D = (I+1.)*(I+I+1.)/SQRT((I+3.)              &
                  *(I-1.)*(I+M+1.)*(I-M+1.))
               E = SQRT((I+2.)*(I-2.)*(I+M)*(I-M))/(I*(I+I+1))
               F = 2.*M/(I*(I+1.))
               RML(IP,-N:N)=D*(U(-N:N)*RML(I,-N:N)-F*TML(I, -N:N)    &
                  -E*RML(IM,-N:N))
               TML(IP,-N:N)=D*(U(-N:N)*TML(I,-N:N)-F*RML(I, -N:N)    &
                  -E*TML(IM,-N:N))
            END IF ! PRESENT(TML)
         END DO ! I = K, L+L-1
      END IF ! K .NE. L-2
!
      RETURN
   END SUBROUTINE LEGENDRE

   SUBROUTINE INT1_EMIS (                                                 &
      IVEC,U,T1,T2,SFI,SFQ,SFU,I1,Q1,U1,      &
      I2,Q2,U2                                        &
      )
!.......................................................................
!     DESCRIPTION:
!>    Integration from Optical Depth T1 to T2 for the First Order
!>    Scattering
!
!>    @param[in]  IVEC using vector or scalar radiative transfer
!>    @param[in]  FLG1 order of Taylor series expansion considered
!>    @param[in]  U outgoing angle in radians
!>    @param[in]  T1 optical depth T1
!>    @param[in]  T2 optical depth T2
!>    @param[in]  SFI source function between optical depth T1 and T2
!>    @param[in]  SFQ source function between optical depth T1 and T2
!>    @param[in]  SFU source function between optical depth T1 and T2
!>    @param[in]  I1 I at optical depth T1
!>    @param[in]  Q1 Q at optical depth T1
!>    @param[in]  U1 U at optical depth T1
!>    @param[out] I2 I at optical depth T2
!>    @param[out] Q2 Q at optical depth T2
!>    @param[out] U2 U at optical depth T2
!.......................................................................
      LOGICAL, INTENT(IN) :: IVEC
      REAL(KD),INTENT(IN) :: U
      REAL,    INTENT(IN) :: T1, T2, SFI, SFQ, SFU
      REAL,    INTENT(IN) :: I1,Q1,U1
      REAL,    INTENT(OUT):: I2,Q2,U2
!
      REAL(KD), PARAMETER :: EPS_C = 1.E-6_KD
      REAL(KD), PARAMETER :: EPS_U = 1.E-10_KD
      REAL(KD) :: CU, U0
      REAL :: C, CS
!
      C = EXP( (T2-T1)/U )
      ! IF (FLG1 .EQ. 1) THEN
      ! CU = (U0-U)/(U0*U)
      ! IF ( (U0 .EQ. U) .OR. (ABS(CU) .LT. EPS_C) ) THEN
      ! INTL=EXP(T2/U0)*(T1-T2)/U
      ! ELSE
      ! INTL=EXP(T2/U0)/(U*CU)*(1.-EXP(CU*(T2-T1)))
      ! END IF
      ! END IF
      ! CS = EXP( 0.5*(T2-T1)/US )
      ! INTL = 0.5*INTL*(CS+1./CS)
      ! ELSE
      ! INTL = 0.5*(1.-C)*( EXP(-T2/US) + EXP(-T1/US) )
      ! END IF
!	  write(*,*) 'level'
!      write(*,*) SFI, C, I1
      I2 = (C*I1)+(SFI*(1-C))
!	  write(*,*) I2
      IF (IVEC) THEN
         Q2 = 0.0
         U2 = 0.0
      END IF
!
      RETURN
   END SUBROUTINE INT1_EMIS

   SUBROUTINE INT1 (                                                 &
      IVEC,FLG1,U,US,T1,T2,SFI,SFQ,SFU,I1,Q1,U1,      &
      I2,Q2,U2                                        &
      )
!.......................................................................
!     DESCRIPTION:
!>    Integration from Optical Depth T1 to T2 for the First Order
!>    Scattering
!
!>    @param[in]  IVEC using vector or scalar radiative transfer
!>    @param[in]  FLG1 order of Taylor series expansion considered
!>    @param[in]  U outgoing angle in radians
!>    @param[in]  US solar zenith angle in radians
!>    @param[in]  T1 optical depth T1
!>    @param[in]  T2 optical depth T2
!>    @param[in]  SFI source function between optical depth T1 and T2
!>    @param[in]  SFQ source function between optical depth T1 and T2
!>    @param[in]  SFU source function between optical depth T1 and T2
!>    @param[in]  I1 I at optical depth T1
!>    @param[in]  Q1 Q at optical depth T1
!>    @param[in]  U1 U at optical depth T1
!>    @param[out] I2 I at optical depth T2
!>    @param[out] Q2 Q at optical depth T2
!>    @param[out] U2 U at optical depth T2
!.......................................................................
      LOGICAL, INTENT(IN) :: IVEC
      INTEGER, INTENT(IN) :: FLG1
      REAL(KD),INTENT(IN) :: U
      REAL,    INTENT(IN) :: US, T1, T2, SFI, SFQ, SFU
      REAL,    INTENT(IN) :: I1,Q1,U1
      REAL,    INTENT(OUT):: I2,Q2,U2
!
      REAL(KD), PARAMETER :: EPS_C = 1.E-6_KD
      REAL(KD), PARAMETER :: EPS_U = 1.E-10_KD
      REAL(KD) :: CU, U0
      REAL :: C, CS, INTL
!
      U0=-DBLE(US)
      C = EXP( (T2-T1)/U )
      IF (FLG1 .EQ. 1) THEN
         IF ( (ABS(U0) .LT. EPS_U) .OR. (ABS(U) .LT. EPS_U)            &
            .AND. (U0 .NE. U) ) THEN
            INTL = 0.
         ELSE
            CU = (U0-U)/(U0*U)
            IF ( (U0 .EQ. U) .OR. (ABS(CU) .LT. EPS_C) ) THEN
               INTL=EXP(T2/U0)*(T1-T2)/U
            ELSE
               INTL=EXP(T2/U0)/(U*CU)*(1.-EXP(CU*(T2-T1)))
            END IF
         END IF
         CS = EXP( 0.5*(T2-T1)/US )
         INTL = 0.5*INTL*(CS+1./CS)
      ELSE
         INTL = 0.5*(1.-C)*( EXP(-T2/US) + EXP(-T1/US) )
      END IF

      I2 = C*I1+INTL*SFI
      IF (IVEC) THEN
         Q2 = C*Q1+INTL*SFQ
         U2 = C*U1+INTL*SFU
      END IF
!
      RETURN
   END SUBROUTINE INT1

   SUBROUTINE INTN (                                                 &
      IVEC,FLGN,U,US,T1,T2,SFI,SFQ,SFU,I1,Q1,U1,      &
      I2,Q2,U2                                        &
      )
!.......................................................................
!     DESCRIPTION:
!>    Integration from Optical Depth T1 to T2 for Higher Order
!>    Scattering
!
!>    @param[in]  IVEC using vector or scalar radiative transfer
!>    @param[in]  FLGN number of Taylor series terms considered
!>    @param[in]  U outgoing angle in radians
!>    @param[in]  US solar zenith angle in radians
!>    @param[in]  T1 optical depth T1
!>    @param[in]  T2 optical depth T2
!>    @param[in]  SFI source function between optical depth T1 and T2
!>    @param[in]  SFQ source function between optical depth T1 and T2
!>    @param[in]  SFU source function between optical depth T1 and T2
!>    @param[in]  I1 I at optical depth T1
!>    @param[in]  Q1 Q at optical depth T1
!>    @param[in]  U1 U at optical depth T1
!>    @param[out] I2 I at optical depth T2
!>    @param[out] Q2 Q at optical depth T2
!>    @param[out] U2 U at optical depth T2
!.......................................................................
      LOGICAL, INTENT(IN) :: IVEC
      INTEGER, INTENT(IN) :: FLGN
      REAL(KD),INTENT(IN) :: U
      REAL,    INTENT(IN) :: US, T1, T2, SFI, SFQ, SFU
      REAL,    INTENT(IN) :: I1,Q1,U1
      REAL,    INTENT(OUT):: I2,Q2,U2
!
      REAL :: C, INTL

      C = EXP( (T2-T1)/U )
      IF (FLGN .EQ. 2) THEN
         INTL = 1.
      ELSE
         INTL = 0.5*(1.-C)
      END IF
      I2 = C*I1+INTL*SFI
      IF (IVEC) THEN
         Q2 = C*Q1+INTL*SFQ
         U2 = C*U1+INTL*SFU
      END IF
!
      RETURN
   END SUBROUTINE INTN

   SUBROUTINE INT1T2 ( U,T1,T2,INTL1,INTL2 )
!.......................................................................
!     DESCRIPTION:
!>    Quadrature of Source function Integrated over T1 to T2
!
!>    @param[in]  U outgoing angle in radians
!>    @param[in]  T1 optical depth T1
!>    @param[in]  T2 optical depth T2
!>    @param[out] INTL1 1st quadrature of integrated source function
!>    @param[out] INTL2 2nd quadrature of integrated source function
!.......................................................................
      REAL(KD), INTENT(IN) :: U
      REAL,     INTENT(IN) :: T1, T2
      REAL,     INTENT(OUT):: INTL1, INTL2
!
      REAL :: C, DT
!
      DT = T1 -T2
      C  = EXP(DT/U)
      INTL1=1.-C
      INTL2=(0.5*DT*(1.+C)+U*INTL1)/ABS(DT)
!
      RETURN
   END SUBROUTINE INT1T2

END MODULE MOD_RT_SOS_MS

