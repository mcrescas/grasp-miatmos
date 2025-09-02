!.......................................................................
!     Generalized Retrieval of Atmosphere and Surface Properties
!     GRASP S.A.S
!.......................................................................
!
!     MODULE: MOD_RT_SOS_LUT
!>    @author
!>    Xin Huang, GRASP S.A.S
!
!     DESCRIPTION:
!>    LUT operations in radiative transfer
!
!     REVISION HISTORY:
!     13/02/2018 - Initial Version
!.......................................................................
#include "../../../constants_set/mod_globals.inc"

      MODULE MOD_RT_SOS_LUT
      USE MOD_GLOBALS,ONLY : GBL_FILE_PATH_LEN
      USE MOD_STOP_REPORT
      USE MOD_C_UTILS
	  USE MOD_RT_SOS_SETUP, ONLY : RT_SOS_SET
!      USE MOD_PAR_OS
      IMPLICIT NONE

!PL original      
!>    number of optical thicknesses for LUT
      INTEGER, PARAMETER :: ND   = 12
!>    list of optical thicknesses for LUT
      REAL,PARAMETER,DIMENSION(ND) :: AOD=(/0.00,0.05,0.10,0.20,        &
                                            0.33,0.50,0.75,1.00,        &
                                            1.40,2.00,2.80,4.00/)
!PL kernels for simulation/test in range 0.-1.5      
!      INTEGER, PARAMETER :: ND   = 33
!>    list of optical thicknesses for LUT
!      REAL,PARAMETER,DIMENSION(ND) :: AOD=(/0.00,0.025,0.05,0.075,      &
!                                            0.10,0.15,0.20,0.25,        &
!                                            0.30,0.35,0.40,0.45,        &
!                                            0.50,0.55,0.60,0.65,        &
!                                            0.70,0.75,0.80,0.85,        &
!                                            0.90,0.95,1.00,1,05,        &
!                                            1.10,1.15,1.20,1.25,        &
!                                            1.30,1.35,1.40,1.45,1.50/)


!>    number of optical thicknesses for LUT
!PL      INTEGER, PARAMETER :: ND   = 20
!>    list of optical thicknesses for LUT
!      REAL,PARAMETER,DIMENSION(ND) :: AOD=(/0.00,0.05,0.10,0.20,        &
!                                            0.33,0.50,0.75,1.00,        &
!                                            1.40,2.00,2.80,4.00,        &
!										    5.,  6.00,7.0, 8.00,        &
!										   10., 15.00,20.0,30.00/)


!>    folder name to store LUT
      CHARACTER(LEN=GBL_FILE_PATH_LEN), PARAMETER :: NMLUT='RT_kernels'
	  CHARACTER(LEN=GBL_FILE_PATH_LEN), PARAMETER :: RTM_NM='RTM_'
	  
!PL	  NMLUT='RTMaerosol'
      


!.......................................................................
!     maximum of Gaussian quadratures for zenith angular integration
!      INTEGER, PARAMETER :: MNG  = NN0
!     maximum of Fourier terms
!      INTEGER, PARAMETER :: MNF  = NF0
!     maximum of aerosol components
!      INTEGER, PARAMETER :: MNM  = NMM
!.......................................................................

      TYPE :: RT_SOS_MS_MTRX
!>        extinction cross section
          REAL :: EXT
!>        single scattering albedo
          REAL :: SSA
!>        bi-directional hemispherical reflectance of atmosphere
          REAL :: AATM
!>        directional hemispherical reflectance of atmosphere
          REAL, DIMENSION(:    ), ALLOCATABLE :: TATM
!>        Fourier components of radiative transfer matrcies of
!>        aerosol models @{
          REAL, DIMENSION(:,:,:), ALLOCATABLE :: IM
          REAL, DIMENSION(:,:,:), ALLOCATABLE :: QM
          REAL, DIMENSION(:,:,:), ALLOCATABLE :: UM
!>@}
      END TYPE RT_SOS_MS_MTRX

      TYPE(RT_SOS_MS_MTRX), DIMENSION(:,:,:,:), ALLOCATABLE :: RTM
      TYPE(RT_SOS_MS_MTRX), TARGET :: RTM1

      CONTAINS

      SUBROUTINE INIT_LUT(RIN)
!.......................................................................
!     DESCRIPTION:
!>    Load the whole LUT
!.......................................................................
      USE mod_retr_settings_derived_type, ONLY : retr_input_settings
!
      TYPE(retr_input_settings), INTENT(IN) :: RIN
!
      INTEGER :: IW, IM, IC, ID
      INTEGER :: NW, NM, NC, NN, NF
      INTEGER :: WAVE
      LOGICAL :: IVEC, EX
      CHARACTER(LEN=GBL_FILE_PATH_LEN) :: FN_LUT
      CHARACTER(LEN=GBL_FILE_PATH_LEN) :: INTL_PATH
      CHARACTER(LEN=GBL_FILE_PATH_LEN) :: MSG

      character(LEN=GBL_FILE_PATH_LEN)       ::  LUT_path
!
!PL      call cstring2fstring(RIN%DLSF%internal_file_path ,INTL_PATH)
      IVEC = RIN%DLSF%keyEL .EQ. 4
      NW = RIN%NW
      NC = RIN%NSD
      NM = MAXVAL(RIN%NBIN(1:NC))
      NN = RIN%OSHF%NN
      NF = RIN%OSHF%NF
      
!      INQUIRE(FILE=TRIM(INTL_PATH)//'/'//TRIM(NMLUT),EXIST=EX)

 	  call cstring2fstring(RIN%LUT_path,LUT_path)

 !PL INTL_PATH=TRIM(RT_SOS_SET%INTL_PATH)//'/'//TRIM(NMLUT)//'/'//TRIM(LUT_path)
      INTL_PATH=TRIM(RT_SOS_SET%INTL_PATH)//'/'//TRIM(LUT_path)

      INQUIRE(FILE=TRIM(INTL_PATH),EXIST=EX)

      IF (.NOT. EX) THEN
          WRITE(MSG,'(A)') 'NO LUT found'
          G_ERROR(TRIM(MSG))
      END IF
      IF (ALLOCATED(RTM)) DEALLOCATE(RTM)
      ALLOCATE(RTM(ND,NM,NC,NW))
      DO IW = 1, NW
          WAVE = INT(RIN%WAVE(IW)*1000.+0.5)
          DO IC = 1, NC
              DO IM = 1, RIN%NBIN(IC)
                  DO ID = 1, ND
                      ALLOCATE(RTM(ID,IM,IC,IW)%TATM(NN))
                      ALLOCATE(RTM(ID,IM,IC,IW)%IM(-NN:NN,NN,0:NF))
                      IF (IVEC) THEN
                          ALLOCATE(RTM(ID,IM,IC,IW)%QM(-NN:NN,NN,0:NF))
                          ALLOCATE(RTM(ID,IM,IC,IW)%UM(-NN:NN,NN,0:NF))
                          WRITE(FN_LUT,'(A4,I0.4,A2,I1,I1,A2,I0.3,A2)')&
                                       TRIM(RTM_NM),WAVE,'_M',IC,IM,   &
                                       '_T',INT(AOD(ID)*100.+0.5),'_V'
                      ELSE
                          WRITE(FN_LUT,'(A4,I0.4,A2,I1,I1,A2,I0.3,A2)')&
                                       TRIM(RTM_NM),WAVE,'_M',IC,IM,   &
                                       '_T',INT(AOD(ID)*100.+0.5),'_S'
                      END IF
!                      FN_LUT=TRIM(INTL_PATH)//'/'//TRIM(NMLUT)//'/'     &
!                                                 //TRIM(FN_LUT)

 					  FN_LUT=TRIM(INTL_PATH)//'/'//TRIM(FN_LUT)


                      OPEN(9, FILE=FN_LUT, FORM="UNFORMATTED",          &
                              ACCESS="SEQUENTIAL",STATUS="OLD")
                      IF (IVEC) THEN
                          READ(9) RTM(ID,IM,IC,IW)%EXT,                 &
                                  RTM(ID,IM,IC,IW)%SSA,                 &
                                  RTM(ID,IM,IC,IW)%AATM,                &
                                  RTM(ID,IM,IC,IW)%TATM,                &
                                  RTM(ID,IM,IC,IW)%IM,                  &
                                  RTM(ID,IM,IC,IW)%QM,                  &
                                  RTM(ID,IM,IC,IW)%UM
                      ELSE
                          READ(9) RTM(ID,IM,IC,IW)%EXT,                 &
                                  RTM(ID,IM,IC,IW)%SSA,                 &
                                  RTM(ID,IM,IC,IW)%AATM,                &
                                  RTM(ID,IM,IC,IW)%TATM,                &
                                  RTM(ID,IM,IC,IW)%IM
                      END IF
                      CLOSE(9)
                  END DO
              END DO
          END DO
      END DO
!
      RETURN
      END SUBROUTINE INIT_LUT

      SUBROUTINE INIT_LUT1(RIN)
!.......................................................................
!     DESCRIPTION:
!>    Allocate LUT for one specific case
!.......................................................................
      USE mod_retr_settings_derived_type, ONLY : retr_input_settings
!
      TYPE(retr_input_settings), INTENT(IN) :: RIN
!
      INTEGER :: NN, NF
      LOGICAL :: IVEC
      IVEC = RIN%DLSF%keyEL .EQ. 4
      NN = RIN%OSHF%NN
      NF = RIN%OSHF%NF
!AL to avoid allocation arror if the previous instance crashed under control unit
      IF (.NOT. ALLOCATED(RTM1%TATM)) ALLOCATE(RTM1%TATM(NN))
      IF (.NOT. ALLOCATED(RTM1%IM)) ALLOCATE(RTM1%IM(-NN:NN,NN,0:NF))
      IF (IVEC) THEN
         IF (.NOT. ALLOCATED(RTM1%QM)) ALLOCATE(RTM1%QM(-NN:NN,NN,0:NF))
         IF (.NOT. ALLOCATED(RTM1%UM)) ALLOCATE(RTM1%UM(-NN:NN,NN,0:NF))
      END IF
!
      RETURN
      END SUBROUTINE INIT_LUT1

      SUBROUTINE SRCH_LUT(IVEC,IW,NA,NB,AOD1,CB)
!.......................................................................
!     DESCRIPTION:
!>    Searching LUT for given wavelength, model, and AOD
!.......................................................................
      USE MOD_PAR_OS, ONLY : KSD
      USE MOD_PAR_INV,ONLY : KIDIM3
!
      LOGICAL, INTENT(IN) :: IVEC
      INTEGER, INTENT(IN) :: IW, NA
      INTEGER, DIMENSION(KSD), INTENT(IN) :: NB
      REAL, INTENT(IN) :: AOD1
      REAL, DIMENSION(KIDIM3,KSD), INTENT(IN) :: CB
!
      INTEGER :: IDH, IDL, ID, IA, IB
      REAL    :: F, TC, C, TEXT, CEXT
      CHARACTER(LEN=GBL_FILE_PATH_LEN) :: MSG
!
      IF (AOD1 .LT. AOD(1)) THEN
          WRITE(MSG,'(A,ES11.4,A)') 'AOD = ',AOD1, ' out of boundary'
          G_ERROR(TRIM(MSG))
      ELSE IF (AOD1 .GT. AOD(ND)) THEN
          ID = ND-1
      ELSE
          IDL = 1
          IDH = ND
          DO
              ID = (IDL+IDH)/2
              IF (AOD(ID) .LT. AOD1) THEN
                  IDL = ID
              ELSE
                  IDH = ID
              END IF
              IF (IDL .GE. IDH-1) EXIT
          END DO
          ID = IDL
      END IF
      F = (AOD1-AOD(ID))/(AOD(ID+1)-AOD(ID))

      TC   = 0.
      TEXT = 0.
      RTM1%EXT = 0.
      RTM1%SSA = 0.
      RTM1%AATM= 0.
      RTM1%TATM= 0.
      RTM1%IM  = 0.
      IF (IVEC) THEN
          RTM1%QM = 0.
          RTM1%UM = 0.
      END IF

      DO IA = 1, NA
          DO IB = 1, NB(IA)
              C    = (1.-F)*CB(IB,IA)
              CEXT = C   *RTM(ID,IB,IA,IW)%EXT
              TC   = TC + C
              TEXT = TEXT + CEXT
              RTM1%SSA = RTM1%SSA + CEXT*RTM(ID,IB,IA,IW)%SSA
              RTM1%AATM= RTM1%AATM+ CEXT*RTM(ID,IB,IA,IW)%AATM
              RTM1%TATM= RTM1%TATM+ CEXT*RTM(ID,IB,IA,IW)%TATM
              RTM1%IM  = RTM1%IM  + CEXT*RTM(ID,IB,IA,IW)%IM
              IF (IVEC) THEN
                  RTM1%QM  = RTM1%QM  + CEXT*RTM(ID,IB,IA,IW)%QM
                  RTM1%UM  = RTM1%UM  + CEXT*RTM(ID,IB,IA,IW)%UM
              END IF
          END DO
      END DO
      DO IA = 1, NA
          DO IB = 1, NB(IA)
              C    = F*CB(IB,IA)
              CEXT = C   *RTM(ID+1,IB,IA,IW)%EXT
              TC   = TC + C
              TEXT = TEXT + CEXT
              RTM1%SSA = RTM1%SSA + CEXT*RTM(ID+1,IB,IA,IW)%SSA
              RTM1%AATM= RTM1%AATM+ CEXT*RTM(ID+1,IB,IA,IW)%AATM
              RTM1%TATM= RTM1%TATM+ CEXT*RTM(ID+1,IB,IA,IW)%TATM
              RTM1%IM  = RTM1%IM  + CEXT*RTM(ID+1,IB,IA,IW)%IM
              IF (IVEC) THEN
                  RTM1%QM  = RTM1%QM  + CEXT*RTM(ID+1,IB,IA,IW)%QM
                  RTM1%UM  = RTM1%UM  + CEXT*RTM(ID+1,IB,IA,IW)%UM
              END IF
          END DO
      END DO

      RTM1%EXT = TEXT/TC
      RTM1%SSA = RTM1%SSA /TEXT
      RTM1%AATM= RTM1%AATM/TEXT
      RTM1%TATM= RTM1%TATM/TEXT
      RTM1%IM  = RTM1%IM  /TEXT
      IF (IVEC) THEN
          RTM1%QM  = RTM1%QM  /TEXT
          RTM1%UM  = RTM1%UM  /TEXT
      END IF
!
      RETURN
      END SUBROUTINE SRCH_LUT

      SUBROUTINE DLLC_LUT(RIN)
!.......................................................................
!     DESCRIPTION:
!>    Release memory for whole LUT
!.......................................................................
      USE mod_retr_settings_derived_type, ONLY : retr_input_settings
!
      TYPE(retr_input_settings), INTENT(IN) :: RIN
!
      LOGICAL :: IVEC
      INTEGER :: IW, IM, IC, ID
      INTEGER :: NW, NM, NC
!
      IVEC = RIN%DLSF%keyEL .EQ. 4
      NW = RIN%NW
      NC = RIN%NSD
      NM = MAXVAL(RIN%NBIN(1:NC))
      DO IW = 1, NW
          DO IC = 1, NC
              DO IM = 1, RIN%NBIN(IC)
                  DO ID = 1, ND
                      DEALLOCATE(RTM(ID,IM,IC,IW)%TATM)
                      DEALLOCATE(RTM(ID,IM,IC,IW)%IM)
                      IF (IVEC) THEN
                          DEALLOCATE(RTM(ID,IM,IC,IW)%QM)
                          DEALLOCATE(RTM(ID,IM,IC,IW)%UM)
                      END IF
                  END DO
              END DO
          END DO
      END DO
      DEALLOCATE(RTM)
!
      RETURN
      END SUBROUTINE DLLC_LUT

      SUBROUTINE DLLC_LUT1(RIN)
!.......................................................................
!     DESCRIPTION:
!>    Release memory for temporary LUT
!.......................................................................
      USE mod_retr_settings_derived_type, ONLY : retr_input_settings
!
      TYPE(retr_input_settings), INTENT(IN) :: RIN
!
      LOGICAL :: IVEC
!
      IVEC = RIN%DLSF%keyEL .EQ. 4
      DEALLOCATE(RTM1%TATM)
      DEALLOCATE(RTM1%IM)
      IF (IVEC) THEN
          DEALLOCATE(RTM1%QM)
          DEALLOCATE(RTM1%UM)
      END IF
!
      RETURN
      END SUBROUTINE DLLC_LUT1

      END MODULE MOD_RT_SOS_LUT

