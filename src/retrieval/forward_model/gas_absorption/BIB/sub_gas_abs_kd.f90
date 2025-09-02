! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

    MODULE sub_gas_kd

	use mod_par_OS,  only : KVERT_WD,NMG,N_WL_GLUT_MAX,N_P_GLUT_MAX,N_T_GLUT_MAX,N_WL_CHANNEL_MAX,NMG,N_SUB_CHANNEL_MAX
    use mod_par_inv, only : KW
    use mod_globals, only  : GBL_FILE_PATH_LEN
    use mod_alloc_gas_lut,  only : MAXKD

    IMPLICIT NONE

    INTEGER, PARAMETER        :: NLEVEL_GAS=KVERT_WD

    INTEGER, PARAMETER        :: NWL_GAS = N_WL_CHANNEL_MAX  !MH MAXIMUM NUMBER OF WAVELENGTHS INTEGRATED IN ONE CHANNEL
    INTEGER, PARAMETER       :: NATM = 6       !NUMBER OF STANDARD ATMOSPHERES IN THE LUT


    INTEGER, PARAMETER       :: NSPECIES_MAX = NMG   !MH THIS WILL BE A CONSTANT FROM CONSTANT SET



        TYPE DATA_KD_Ch

           REAL, DIMENSION(1)                           :: TOTAL_EXT_KD
           INTEGER                                      :: NSubCH
        !MH      number of levels OF SUBCHANNEL
           INTEGER,DIMENSION(N_SUB_CHANNEL_MAX)         :: NLV_SC
           INTEGER,DIMENSION(N_SUB_CHANNEL_MAX)         :: NWL_SC

        !XH      number of elements in AIKD
           INTEGER,DIMENSION(N_SUB_CHANNEL_MAX)         :: NEXP
           INTEGER,DIMENSION(N_SUB_CHANNEL_MAX)         :: NGAS
           INTEGER,DIMENSION(NWL_GAS,N_SUB_CHANNEL_MAX) :: MAPPING_FUNCTION
           REAL,DIMENSION(NWL_GAS,N_SUB_CHANNEL_MAX)    :: WVL_SC
        !XH      weights for K-distribution
           REAL,DIMENSION(MAXKD,N_SUB_CHANNEL_MAX)      :: AIKD !MH Dimensions: [bin,subchannel]
        !XH      tau for vertical layers
           REAL,DIMENSION(NLEVEL_GAS-1,MAXKD,N_SUB_CHANNEL_MAX) :: COEFKD !MH Dimensions: [level,bin,subchannel]
        END TYPE DATA_KD_Ch


          TYPE DATA_ABS_GS_WL
    !XH      wavelength corresponding to the current dataset
             REAL, DIMENSION(NWL_GAS)                     :: WVL
    !XH      tau for continuum
             REAL, DIMENSION(NLEVEL_GAS-1,NWL_GAS)        :: COEFCN
    !XH      number of elements in AIKD
             INTEGER,DIMENSION(NMG)                       :: NGAS
          END TYPE DATA_ABS_GS_WL


         TYPE DATA_GAS
        !XH      number of levels
                 INTEGER :: NLV
        !XH      number of layers
                 INTEGER :: NLY
        !XH      solar zenith angle
                 REAL    :: SZA
        !MH      Number of subchannels
                 INTEGER :: NSubCH
        !MH Number of lines for the actual (sub)channel
                 INTEGER, DIMENSION(N_SUB_CHANNEL_MAX) :: NWL
        !XH      altitudes of the levels stored in CEOFKD and AIKD
                 REAL,    DIMENSION(NLEVEL_GAS) :: HLV
        !XH      altitudes of the layers stored in CEOFKD and AIKD
                 REAL,    DIMENSION(NLEVEL_GAS-1) :: HLY
        !MH      depth of each layer
                 REAL,    DIMENSION(NLEVEL_GAS-1) :: DZ
!MH      set of data for absorbing gases at all wavelengths for LBL
         TYPE(DATA_ABS_GS_WL), DIMENSION(N_SUB_CHANNEL_MAX) :: ABS_GS_WL
!MH      set of data for absorbing gases at all wavelengths for KD
         TYPE(DATA_KD_Ch), DIMENSION(1) :: DATA_KD_Channel
        END TYPE DATA_GAS


    CONTAINS


     SUBROUTINE READ_KDIST(path_KD,NGAS,ISPECIE)

      use mod_alloc_gas_lut, only: GAS_KD_DATA

        IMPLICIT NONE

! -------------------
! IN:
        CHARACTER(*),               INTENT(IN)     ::  PATH_KD
        INTEGER,                    INTENT(IN)     ::  NGAS
        LOGICAL,DIMENSION(KW,NMG),  INTENT(IN)     ::  ISPECIE
! -------------------
! LOCAL:
        INTEGER                                    ::  I,J, NWL_MAPPING, IK, IL, NINTERVAL, INI
        REAL,DIMENSION(NLEVEL_GAS)                 ::  AUX_INT
        
        LOGICAL           :: file_exists
        INTEGER           :: read_bins
        CHARACTER(200)    :: AUX


        INQUIRE(file=TRIM(path_KD), EXIST=file_exists)


        IF(file_exists) THEN

            OPEN(2,file=TRIM(PATH_KD))

            READ(2,*) AUX, GAS_KD_DATA%NCH

            READ(2,*) AUX, AUX_INT(1:GAS_KD_DATA%NCH)


            NINTERVAL = 0

            DO I=1,GAS_KD_DATA%NCH

              GAS_KD_DATA%ABS_GS_KD(I)%NSubCH = AUX_INT(I)
                IF(AUX_INT(I) .GT. N_SUB_CHANNEL_MAX)THEN
                    WRITE(*,*) "ERROR: Number of subchannels in K-distribution: ",AUX_INT(INI),",  bigger than N_SUB_CHANNEL_MAX: ", N_SUB_CHANNEL_MAX
                    STOP
                END IF
              NINTERVAL = NINTERVAL + AUX_INT(I)
            END DO



            READ(2,*) AUX, AUX_INT(1:NINTERVAL)

            INI = 1

            DO I=1,GAS_KD_DATA%NCH

                DO J=1,GAS_KD_DATA%ABS_GS_KD(I)%NSubCH

                    GAS_KD_DATA%ABS_GS_KD(I)%NGAS(J) = AUX_INT(INI)
                    INI = INI + 1
                    
                END DO

            END DO


            READ(2,*) AUX

            DO IK=1,NGAS
                
                READ(2,*) GAS_KD_DATA%TOTAL_GAS_C_REF_KD(IK)

            END DO

            READ(2,*) AUX, AUX_INT(1:NINTERVAL)

            INI = 1
            DO I=1,GAS_KD_DATA%NCH

                DO J=1,GAS_KD_DATA%ABS_GS_KD(I)%NSubCH

                    IF(AUX_INT(INI) .GT. MAXKD)THEN
                        WRITE(*,*) "ERROR: Number of bins in K-distribution: ",AUX_INT(INI),",  bigger than MAXKD: ", MAXKD
                        STOP
                    END IF

                    GAS_KD_DATA%ABS_GS_KD(I)%NEXP(J) = AUX_INT(INI)
                    
                    INI = INI + 1
                END DO

            END DO
            

            READ(2,*) AUX, AUX_INT(1:NINTERVAL)

            INI = 1
            DO I=1,GAS_KD_DATA%NCH

                DO J=1,GAS_KD_DATA%ABS_GS_KD(I)%NSubCH

                    GAS_KD_DATA%ABS_GS_KD(I)%NLV_SC(J) = AUX_INT(INI)
                    GAS_KD_DATA%ABS_GS_KD(I)%NLV_SC(J) = GAS_KD_DATA%ABS_GS_KD(I)%NLV_SC(J) + 1 !MH Here we find number of layers but to keep consostency with the rest of the code we need to pass to levels
                    INI = INI + 1
                END DO

            END DO

            READ(2,*) AUX, AUX_INT(1:NINTERVAL)
            INI = 1
            DO I=1,GAS_KD_DATA%NCH

                DO J=1,GAS_KD_DATA%ABS_GS_KD(I)%NSubCH

                    IF(AUX_INT(INI) .GT. N_WL_CHANNEL_MAX)THEN
                        WRITE(*,*) "ERROR: Number of lines in K-distribution inverse mapping function: ",AUX_INT(INI),",  bigger than N_WL_CHANNEL_MAX: ", N_WL_CHANNEL_MAX
                        STOP
                    END IF

                    GAS_KD_DATA%ABS_GS_KD(I)%NWL_SC(J) = AUX_INT(INI)
                    INI = INI + 1
                END DO

            END DO

            READ(2,*) AUX
            
            DO I=1,GAS_KD_DATA%NCH

                DO J=1,GAS_KD_DATA%ABS_GS_KD(I)%NSubCH

                    READ(2,*) GAS_KD_DATA%ABS_GS_KD(I)%WVL_SC(1:GAS_KD_DATA%ABS_GS_KD(I)%NWL_SC(J),J)
                    GAS_KD_DATA%ABS_GS_KD(I)%WVL_SC(1:GAS_KD_DATA%ABS_GS_KD(I)%NWL_SC(J),J) = GAS_KD_DATA%ABS_GS_KD(I)%WVL_SC(1:GAS_KD_DATA%ABS_GS_KD(I)%NWL_SC(J),J)/1000
                    
                END DO

            END DO

            READ(2,*) AUX

            DO I=1,GAS_KD_DATA%NCH

                DO J=1,GAS_KD_DATA%ABS_GS_KD(I)%NSubCH

                    READ(2,*) GAS_KD_DATA%ABS_GS_KD(I)%MAPPING_FUNCTION(1:GAS_KD_DATA%ABS_GS_KD(I)%NWL_SC(J),J)
                    
                END DO

            END DO

            READ(2,*) AUX
            
            DO I=1,GAS_KD_DATA%NCH

                DO J=1,GAS_KD_DATA%ABS_GS_KD(I)%NSubCH

                    IF(GAS_KD_DATA%ABS_GS_KD(I)%NEXP(J) .LT. 1)THEN
                        read_bins = 1
                    ELSE
                        read_bins = GAS_KD_DATA%ABS_GS_KD(I)%NEXP(J)
                    END IF

                    READ(2,*) GAS_KD_DATA%ABS_GS_KD(I)%AIKD(1:read_bins,J)
                    
                    
                END DO

            END DO


            READ(2,*) AUX

            DO I=1,GAS_KD_DATA%NCH

                DO J=1,GAS_KD_DATA%ABS_GS_KD(I)%NSubCH

!                    DO IK=1,GAS_KD_DATA%ABS_GS_KD(I)%NGAS(J)
                    DO IK=1,NGAS

                        IF(ISPECIE(I,IK))THEN   !MH This is done to match the index order everywhere the same

                            READ(2,*) AUX
                            
                            DO IL=1,GAS_KD_DATA%ABS_GS_KD(I)%NLV_SC(J)-1
                                READ(2,*) GAS_KD_DATA%ABS_GS_KD(I)%COEFKD(IL,1:GAS_KD_DATA%ABS_GS_KD(I)%NEXP(J),IK,J)
                            END DO !NLV

                        END IF

                    END DO !NCONSTITUEYENTS
                END DO !NSUBCH

            END DO !NCH

            CLOSE(2)

        ELSE
             WRITE(*,*) 'ERROR: ', PATH_KD,  ' must be an existing file. Please, install it correctly or choose a proper K-Distribution file name.'
             STOP
        END IF

        

     END SUBROUTINE READ_KDIST


    SUBROUTINE DATATM_GAS (RIN,PATH_VTP,ISTDAT,stdat)

    use mod_par_OS,  only : KVERT_WD
    use mod_c_utils, only: cstring2fstring
    use mod_globals, only  : GBL_FILE_PATH_LEN
    use mod_alloc_gas_lut, only: ATM_PROF
    use mod_retr_settings_derived_type

    IMPLICIT NONE
! -------------------
! IN:
    type(retr_input_settings),        INTENT(IN)   :: RIN
    CHARACTER(*),                     INTENT(IN)   :: PATH_VTP
    LOGICAL,                          INTENT(IN)   :: ISTDAT
    INTEGER,                          INTENT(IN)   :: stdat
! -------------------
! LOCAL:
    CHARACTER(40)                                  :: AUX
    INTEGER                                        :: J
    LOGICAL                                        :: file_exists
    CHARACTER(len=GBL_FILE_PATH_LEN)               :: PATH,internal_file_path


    IF(ISTDAT)THEN

            call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)

           !MH Select atmosphere type

           IF(stdat .EQ. 0) THEN
               PATH = TRIM(internal_file_path)//"atm_model_pt/US.vtp"
           ELSE IF (stdat .EQ. 1) THEN
               PATH = TRIM(internal_file_path)//"atm_model_pt/MS.vtp"
           ELSE IF (stdat .EQ. 2) THEN
               PATH = TRIM(internal_file_path)//"atm_model_pt/MW.vtp"
           ELSE IF (stdat .EQ. 3) THEN
               PATH = TRIM(internal_file_path)//"atm_model_pt/SS.vtp"
           ELSE IF (stdat .EQ. 4) THEN
               PATH = TRIM(internal_file_path)//"atm_model_pt/SW.vtp"
           ELSE IF (stdat .EQ. 5) THEN
               PATH = TRIM(internal_file_path)//"atm_model_pt/TR.vtp"
           END IF


    ELSE

        PATH = PATH_VTP

    END IF

    INQUIRE(FILE=TRIM(PATH), EXIST=file_exists)

    IF(file_exists) THEN

        OPEN(2,FILE=PATH)

        READ(2,*) AUX, ATM_PROF%NLV

        IF(ATM_PROF%NLV .GT. KVERT_WD) THEN
            WRITE(*,*) 'ERROR: The number of levels in atmospheric profile: ', ATM_PROF%NLV, 'bigger than the maximum amount of values allowed for radiative transfer: ',KVERT_WD
            STOP
        END IF

        READ(2,*) AUX, AUX,ATM_PROF%STEMP

        !MH Headers
        READ(2,*) AUX
        DO J=1,ATM_PROF%NLV
            READ(2,*) AUX,ATM_PROF%ALT(J),ATM_PROF%P(J),ATM_PROF%T(J)
            ATM_PROF%ALT(J) = ATM_PROF%ALT(J)*0.001
        END DO

        CLOSE(2)

    ELSE
         WRITE(*,*) 'ERROR: ', PATH, ' must be an existing file. Please, install it correctly or choose a proper vtp file name.'
         STOP
    END IF

    RETURN

    END SUBROUTINE DATATM_GAS




  SUBROUTINE READ_EXT_PRO_LUT_V2(PATH,NAME,WL_GAS,K,T_LUT,P_LUT,CCT_Ref,ALT_LUT,NWL_LUT,NP_LUT,NT_LUT)

     use mod_alloc_gas_lut, only: LUT_GASES

     IMPLICIT NONE
! -------------------
! IN:
     CHARACTER(*),                       INTENT(IN)        :: PATH, NAME
     INTEGER,                            INTENT(IN)        :: K
! -------------------
! OUT:
     REAL, DIMENSION(N_WL_GLUT_MAX),     INTENT(OUT)       :: WL_GAS
     REAL, DIMENSION(N_P_GLUT_MAX),      INTENT(OUT)       :: P_LUT, CCT_Ref, ALT_LUT
     REAL, DIMENSION(N_T_GLUT_MAX),      INTENT(OUT)       :: T_LUT
     INTEGER,                            INTENT(OUT)       :: NWL_LUT
     INTEGER,                            INTENT(OUT)       :: NP_LUT
     INTEGER,                            INTENT(OUT)       :: NT_LUT
! -------------------
! LOCAL:
     INTEGER           :: J,b,iT
     CHARACTER(200)    :: AUX,CHECK_WL
     REAL              :: MIN_WL,MAX_WL,WL_AUX
     LOGICAL           :: file_exists


     INQUIRE(file=TRIM(path)//TRIM(name), EXIST=file_exists)

     IF(file_exists) THEN

         OPEN(UNIT = 1,file=TRIM(path)//TRIM(name), status = 'old', iostat=b, action='read')

         READ(1,*) AUX,MIN_WL,MAX_WL,NWL_LUT,AUX,AUX,AUX,AUX,NP_LUT,NT_LUT

         !MH Validators to avoid segmentation faults

         IF(NWL_LUT .GT. N_WL_GLUT_MAX)THEN
            WRITE(*,*) 'ERROR: The number of wavelengths in LUT: ', NWL_LUT, ', is bigger than the maximum possible number: ', N_WL_GLUT_MAX
            STOP
         END IF

         IF(NP_LUT .GT. N_P_GLUT_MAX)THEN
            WRITE(*,*) 'ERROR: The number of pressure levels in LUT: ', NP_LUT, ', is bigger than the maximum possible number: ', N_P_GLUT_MAX
            STOP
         END IF

         IF(NT_LUT .GT. N_T_GLUT_MAX)THEN
            WRITE(*,*) 'ERROR: The number of temperature values in LUT: ', NT_LUT, ', is bigger than the maximum possible number: ', N_T_GLUT_MAX
            STOP
         END IF


         !MH READING HEADERS
         READ(1,*) AUX
         READ(1,*) P_LUT(1:NP_LUT)
         READ(1,*) AUX
         READ(1,*) T_LUT(1:NT_LUT)
         READ(1,*) AUX
         READ(1,*) ALT_LUT(1:NP_LUT)
         READ(1,*) AUX
         READ(1,*) CCT_Ref(1:NP_LUT)
         READ(1,*) AUX,WL_AUX,AUX,CHECK_WL


         J = 1
         
         DO WHILE ( (WL_AUX .LE. MAX_WL) .AND. (WL_AUX .GE. MIN_WL ) )
            IF(TRIM(CHECK_WL) .EQ. 'Y') THEN

                IF(WL_AUX .EQ. MAX_WL) THEN
                    EXIT
                ELSE

                    LUT_GASES%SPECIE(K)%WL_GAS(J) = WL_AUX
                    
                    DO iT=1,NT_LUT
                        
                        READ(1,*)LUT_GASES%SPECIE(K)%EXT(J,iT,1:NP_LUT)

                    END DO
                    
                    READ(1,*) AUX,WL_AUX,AUX,CHECK_WL
                    J = J + 1
                END IF

            ELSE

                IF(WL_AUX .EQ. MAX_WL) THEN
                    EXIT
                END IF
                LUT_GASES%SPECIE(K)%WL_GAS(J) = WL_AUX
                READ(1,*) AUX,WL_AUX,AUX,CHECK_WL
                LUT_GASES%SPECIE(K)%EXT(J,:,:) = 0.0
                J = J + 1

            END IF

             
         END DO

         CLOSE(1)

    ELSE
         WRITE(*,*) 'ERROR: ', TRIM(path)//TRIM(name),  ' must be an existing file. LUT ', TRIM(name) , ' does not exist or is not installed in ', TRIM(path), '. Please, install it correctly or choose a proper LUT name.'
         STOP
    END IF

 END SUBROUTINE READ_EXT_PRO_LUT_V2



SUBROUTINE LUT_BILINEAR_INTERP_LEVELS(P_LUT,                 &
                                       T_LUT,                &
                                       ALT_LUT,              &
                                       CCT_LUT,              &
                                       N_P_LUT,              &
                                       N_T_LUT,              &
                                       NLV,                  &
                                       EXT_LUT,              &
                                       P,T,ALT,              &
                                       CONC,                 &
                                       EXT)
! -------------------
! IN:
    REAL, DIMENSION(N_P_GLUT_MAX),                   INTENT(IN)       ::    P_LUT, CCT_LUT, ALT_LUT
    REAL, DIMENSION(N_T_GLUT_MAX),                   INTENT(IN)       ::    T_LUT
    REAL, DIMENSION(N_T_GLUT_MAX,N_P_GLUT_MAX),      INTENT(IN)       ::    EXT_LUT
    REAL, DIMENSION(NLEVEL_GAS),                     INTENT(IN)       ::    P, T, ALT
    INTEGER,                                         INTENT(IN)       ::    NLV,N_P_LUT,N_T_LUT
! -------------------
! OUT:
    REAL, DIMENSION(NLEVEL_GAS-1),                   INTENT(OUT)      ::    EXT,CONC
! -------------------
! LOCAL:
    INTEGER                       :: J,I,INDEX_P,INDEX_T, INDEX_ALT
    REAL                          :: tiny, Q11,Q12,Q21,Q22,X1,X2,Y1,Y2,X,Y,C1,C2
    REAL, DIMENSION(NLEVEL_GAS)   :: EXT_LEVELS, CONC_LEVELS

    DO J=1,NLV


        IF((P(J).LT.P_LUT(1)) .OR. ((P(J).GT.P_LUT(N_P_LUT)))) THEN
            WRITE(*,*) 'P = ', P(J), 'Pa OUTSIDE LUT LIMITS'
            STOP
        END IF

        IF((T(J).LT.T_LUT(1)) .OR. ((T(J).GT.T_LUT(N_T_LUT)))) THEN
            WRITE(*,*) 'T = ', T(J), 'K OUTSIDE LUT LIMITS'
            STOP
        END IF

        IF(((ALT(J)*1000) .GT. ALT_LUT(1)) .OR. (((ALT(J)*1000) .LT. ALT_LUT(N_P_LUT)))) THEN
            WRITE(*,*) 'ALT = ', (ALT(J)*1000), 'm OUTSIDE LUT LIMITS'
            STOP
        END IF



        !MH LOOK FOR THE CLOSEST VALUES IN LUT

        DO I=1,N_T_LUT-1
            IF((T(J).GE.T_LUT(I)) .AND. (T(J).LE.T_LUT(I+1)))THEN
                INDEX_T = I
                EXIT
            END IF
        END DO

        DO I=1,N_P_LUT-1
            IF((P(J).GE.P_LUT(I)) .AND. (P(J).LE.P_LUT(I+1)))THEN
                INDEX_P = I
                EXIT
            END IF
        END DO

        DO I=1,N_P_LUT-1
            IF((ALT(J)*1000 .LE. ALT_LUT(I)) .AND. (ALT(J)*1000 .GE. ALT_LUT(I+1)))THEN
                INDEX_ALT = I
                EXIT
            END IF
        END DO

        X = T(J)
        Y = P(J)

        X1 = T_LUT(INDEX_T)
        X2 = T_LUT(INDEX_T+1)

        Y1 = P_LUT(INDEX_P)
        Y2 = P_LUT(INDEX_P+1)

        Q11 = EXT_LUT(INDEX_T,INDEX_P)
        Q12 = EXT_LUT(INDEX_T,INDEX_P+1)
        Q21 = EXT_LUT(INDEX_T+1,INDEX_P)
        Q22 = EXT_LUT(INDEX_T+1,INDEX_P+1)

!        EXT(J) = ( 1 / ((X2 - X1) *(Y2-Y1))) * ( (Q11*(X2-X)*(Y2-Y)) + (Q21*(X-X1)*(Y2-Y)) + (Q12*(X2-X)*(Y-Y1)) + (Q22*(X-X1)*(Y-Y1)) )
        EXT_LEVELS(J) = ( 1 / ((X2 - X1) *(Y2-Y1))) * ( (Q11*(X2-X)*(Y2-Y)) + (Q21*(X-X1)*(Y2-Y)) + (Q12*(X2-X)*(Y-Y1)) + (Q22*(X-X1)*(Y-Y1)) )

        !MH Concentration Density interpolation

!        C1 = CCT_LUT(INDEX_ALT)
!        C2 = CCT_LUT(INDEX_ALT+1)
!
!        Y1 = ALT_LUT(INDEX_ALT)
!        Y2 = ALT_LUT(INDEX_ALT+1)
!
!        CONC_LEVELS(J) = ((C1*(Y2 - (ALT(J)*1000))) + (C2*((ALT(J)*1000) - Y1)))/(Y2 - Y1)

        C1 = CCT_LUT(INDEX_P)
        C2 = CCT_LUT(INDEX_P+1)

        Y1 = P_LUT(INDEX_P)
        Y2 = P_LUT(INDEX_P+1)

        CONC_LEVELS(J) = ((C1*(Y2 - (P(J)))) + (C2*((P(J)) - Y1)))/(Y2 - Y1)

    END DO

    !MH RENORMALIZATION OF THE EXTINCTION FROM LEVELS TO LAYERS
    DO J=1,NLV-1
        !MH INTEGRATION IN EACH LAYER BY TRAPEZOIDAL RULE
        !MH the 1000 is needed beacause the LUT are normalized by meters
        EXT(J)  = (EXT_LEVELS(J) + EXT_LEVELS(J+1))   * (ALT(J)-ALT(J+1))*1000/2
        CONC(J) = (CONC_LEVELS(J) + CONC_LEVELS(J+1)) * (ALT(J)-ALT(J+1))*1000/2

    END DO


  END SUBROUTINE LUT_BILINEAR_INTERP_LEVELS




SUBROUTINE LUT_BILINEAR_INTERP_LAYERS(P_LUT,                 &
                                       T_LUT,                &
                                       ALT_LUT,              &
                                       CCT_LUT,              &
                                       N_P_LUT,              &
                                       N_T_LUT,              &
                                       NLV,                  &
                                       EXT_LUT,              &
                                       P,T,ALT,              &
                                       CONC,                 &
                                       EXT)
! -------------------
! IN:
    REAL, DIMENSION(N_P_GLUT_MAX),                   INTENT(IN)       ::    P_LUT, CCT_LUT, ALT_LUT
    REAL, DIMENSION(N_T_GLUT_MAX),                   INTENT(IN)       ::    T_LUT
    REAL, DIMENSION(N_T_GLUT_MAX,N_P_GLUT_MAX),      INTENT(IN)       ::    EXT_LUT
    REAL, DIMENSION(NLEVEL_GAS),                     INTENT(IN)       ::    P, T, ALT
    INTEGER,                                         INTENT(IN)       ::    NLV,N_P_LUT,N_T_LUT
! -------------------
! OUT:
    REAL, DIMENSION(NLEVEL_GAS-1),                   INTENT(OUT)      ::    EXT,CONC
! -------------------
! LOCAL:
    INTEGER                       :: J,I,INDEX_P,INDEX_T, INDEX_ALT
    REAL                          :: tiny, Q11,Q12,Q21,Q22,X1,X2,Y1,Y2,X,Y,C1,C2
    REAL, DIMENSION(NLEVEL_GAS)   :: EXT_LEVELS, CONC_LEVELS, H_layers, T_layers, P_layers

    DO J=1,NLV-1

    H_layers(j) = (ALT(J)+ALT(J+1))/2
    P_layers(j) = (P(J)+P(J+1))/2
    T_layers(j) = (T(J)+T(J+1))/2

    end do

    DO J=1,NLV-1


        IF((P_layers(J).LT.P_LUT(1)) .OR. ((P_layers(J).GT.P_LUT(N_P_LUT)))) THEN
            WRITE(*,*) 'P = ', P(J), 'Pa OUTSIDE LUT LIMITS'
            STOP
        END IF

        IF((T_layers(J).LT.T_LUT(1)) .OR. ((T_layers(J).GT.T_LUT(N_T_LUT)))) THEN
            WRITE(*,*) 'T = ', T(J), 'K OUTSIDE LUT LIMITS'
            STOP
        END IF

        IF(((ALT(J)*1000) .GT. ALT_LUT(1)) .OR. (((ALT(J)*1000) .LT. ALT_LUT(N_P_LUT)))) THEN
            WRITE(*,*) 'ALT = ', (ALT(J)*1000), 'm OUTSIDE LUT LIMITS'
            STOP
        END IF



        !MH LOOK FOR THE CLOSEST VALUES IN LUT

        DO I=1,N_T_LUT-1
            IF((T_layers(J).GE.T_LUT(I)) .AND. (T_layers(J).LE.T_LUT(I+1)))THEN
                INDEX_T = I
                EXIT
            END IF
        END DO

        DO I=1,N_P_LUT-1
            IF((P_layers(J).GE.P_LUT(I)) .AND. (P_layers(J).LE.P_LUT(I+1)))THEN
                INDEX_P = I
                EXIT
            END IF
        END DO

        DO I=1,N_P_LUT-1
            IF((H_layers(J)*1000 .LE. ALT_LUT(I)) .AND. (H_layers(J)*1000 .GE. ALT_LUT(I+1)))THEN
                INDEX_ALT = I
                EXIT
            END IF
        END DO

        X = T_layers(J)
        Y = P_layers(J)

        X1 = T_LUT(INDEX_T)
        X2 = T_LUT(INDEX_T+1)

        Y1 = P_LUT(INDEX_P)
        Y2 = P_LUT(INDEX_P+1)

        Q11 = EXT_LUT(INDEX_T,INDEX_P)
        Q12 = EXT_LUT(INDEX_T,INDEX_P+1)
        Q21 = EXT_LUT(INDEX_T+1,INDEX_P)
        Q22 = EXT_LUT(INDEX_T+1,INDEX_P+1)

        EXT_LEVELS(J) = ( 1 / ((X2 - X1) *(Y2-Y1))) * ( (Q11*(X2-X)*(Y2-Y)) + (Q21*(X-X1)*(Y2-Y)) + (Q12*(X2-X)*(Y-Y1)) + (Q22*(X-X1)*(Y-Y1)) )

        C1 = CCT_LUT(INDEX_P)
        C2 = CCT_LUT(INDEX_P+1)

        Y1 = P_LUT(INDEX_P)
        Y2 = P_LUT(INDEX_P+1)

        CONC_LEVELS(J) = ((C1*(Y2 - (P_layers(J)))) + (C2*((P_layers(J)) - Y1)))/(Y2 - Y1)

    END DO

    !MH RENORMALIZATION OF THE EXTINCTION FROM LEVELS TO LAYERS
    DO J=1,NLV-1
        !MH INTEGRATION IN EACH LAYER BY TRAPEZOIDAL RULE
        !MH the 1000 is needed beacause the LUT are normalized by meters
        EXT(J)  = (EXT_LEVELS(J))   * (ALT(J)-ALT(J+1))*1000
        CONC(J) = (CONC_LEVELS(J)) * (ALT(J)-ALT(J+1))*1000

    END DO



  END SUBROUTINE LUT_BILINEAR_INTERP_LAYERS


SUBROUTINE  get_filter_shape(nsubchannels,  IW,           &
                             filters_path)

     use mod_retr_settings_derived_type
     use mod_alloc_gas_lut,  only : filter_channel_type
     use mod_alloc_gas_lut,  only: filters_shape

     IMPLICIT NONE
! -------------------
! IN:
     character(GBL_FILE_PATH_LEN),               INTENT(IN)  :: filters_path
     INTEGER,                                    INTENT(IN)  :: nsubchannels
     INTEGER,                                    INTENT(IN)  :: IW
! -------------------
! OUT:
     
! -------------------
! LOCAL:
     CHARACTER(200)              :: AUX
     integer                     :: b, iwl, nlines, I, ISUB, I_save, NWL_total, I_ini
     LOGICAL                     :: file_exists
     REAL,DIMENSION(NWL_GAS)     :: WVL, TRANSMISSION


     INQUIRE(file=filters_path, EXIST=file_exists)

     IF(file_exists) THEN

        !MH First we need to know the number of lines in the file
        WVL = 0.0
        nlines = 0
        OPEN (22, file = TRIM(filters_path))
        DO
          READ(22,*,iostat=b)
          IF (b/=0) EXIT
          nlines = nlines + 1
        END DO
        CLOSE (22)

        OPEN(UNIT = 23,file=TRIM(filters_path), status = 'old', iostat=b, action='read')

        iwl = 1
        do while(.not. (iwl .eq. (nlines + 1)) )
           READ(23,*) WVL(iwl), TRANSMISSION(iwl)
           iwl = iwl + 1
        end do

        CLOSE(23)
        
        filters_shape%filter_channel(IW)%NWL_total = nlines
        

        !MH Storage of the different parts of the filter corresponding to each subchannel
        ISUB = 1
        I_save = 1
        
        DO I=1,filters_shape%filter_channel(IW)%NWL_total
            
            IF(WVL(I) .gt. ((WVL(filters_shape%filter_channel(IW)%NWL_total) - WVL(1)) *ISUB/(nsubchannels) + WVL(1) )) THEN

                IF(ISUB .gt. 1) then
                    filters_shape%filter_channel(IW)%NWL(ISUB) = I_save
                    filters_shape%filter_channel(IW)%WVL(1,ISUB)  = WVL(I_ini-1)
                    filters_shape%filter_channel(IW)%TRANSMISSION(1,ISUB) = TRANSMISSION(I_ini-1)
                    filters_shape%filter_channel(IW)%bandwidth(ISUB) = (filters_shape%filter_channel(IW)%WVL(filters_shape%filter_channel(IW)%NWL(ISUB),ISUB) - filters_shape%filter_channel(IW)%WVL(1,ISUB))
                    filters_shape%filter_channel(IW)%center_subchannels(ISUB) = (filters_shape%filter_channel(IW)%WVL(filters_shape%filter_channel(IW)%NWL(ISUB),ISUB) + filters_shape%filter_channel(IW)%WVL(1,ISUB))/2

                ELSE
                    
                    filters_shape%filter_channel(IW)%NWL(ISUB) = I_save - 1
                    filters_shape%filter_channel(IW)%bandwidth(ISUB) = (filters_shape%filter_channel(IW)%WVL(filters_shape%filter_channel(IW)%NWL(ISUB),ISUB) - filters_shape%filter_channel(IW)%WVL(1,ISUB))
                    filters_shape%filter_channel(IW)%center_subchannels(ISUB) = (filters_shape%filter_channel(IW)%WVL(filters_shape%filter_channel(IW)%NWL(ISUB),ISUB) + filters_shape%filter_channel(IW)%WVL(1,ISUB))/2

                END IF


                ISUB = ISUB + 1
                I_save = 1
            END IF

            IF(I_save .EQ. 1) THEN
                I_ini = I
            end IF


            IF(ISUB .eq. 1) then
                filters_shape%filter_channel(IW)%WVL(I_save,ISUB)  = WVL(I)
                filters_shape%filter_channel(IW)%TRANSMISSION(I_save,ISUB) = TRANSMISSION(I)
                I_save = I_save + 1
            ELSE
                filters_shape%filter_channel(IW)%WVL(I_save+1,ISUB)  = WVL(I)
                filters_shape%filter_channel(IW)%TRANSMISSION(I_save+1,ISUB) = TRANSMISSION(I)
                I_save = I_save + 1
            END IF

            
            
        END DO
        
        IF(nsubchannels .gt. 1) then
            filters_shape%filter_channel(IW)%WVL(1,ISUB)  = WVL(I_ini-1)
            filters_shape%filter_channel(IW)%TRANSMISSION(1,ISUB) = TRANSMISSION(I_ini-1)
            filters_shape%filter_channel(IW)%NWL(ISUB) = I_save
        ELSE
            filters_shape%filter_channel(IW)%NWL(ISUB) = I_save-1
        END IF

        
        filters_shape%filter_channel(IW)%bandwidth(ISUB) = filters_shape%filter_channel(IW)%WVL(filters_shape%filter_channel(IW)%NWL(ISUB),ISUB) - filters_shape%filter_channel(IW)%WVL(1,ISUB)
        filters_shape%filter_channel(IW)%center_subchannels(ISUB) = (filters_shape%filter_channel(IW)%WVL(filters_shape%filter_channel(IW)%NWL(ISUB),ISUB) + filters_shape%filter_channel(IW)%WVL(1,ISUB))/2

    ELSE

        WRITE(*,*) 'ERROR: ', TRIM(filters_path),  ' must be an existing file. Please, install it correctly or choose a proper filter file name.'
        STOP

    END IF

  END SUBROUTINE get_filter_shape


SUBROUTINE GET_SPECTRAL_INTERVALS(ikdist,           &
                                  nsubchannels,     &
                                  bandwidth,        &
                                  wl,               &
                                  IW,               &
                                  WL_Subchannels)

    use mod_alloc_gas_lut, only: GAS_KD_DATA

    IMPLICIT NONE
! -------------------
! IN:
    LOGICAL,                                  INTENT(IN)       ::  ikdist
    INTEGER,                                  INTENT(IN)       ::  IW
    INTEGER,DIMENSION(KW),                    INTENT(IN)       ::  nsubchannels
    REAL,DIMENSION(KW),                       INTENT(IN)       ::  wl
! -------------------
! INOUT:
    REAL, DIMENSION(KW,N_SUB_CHANNEL_MAX),    INTENT(INOUT)    ::  bandwidth
! -------------------
! OUT:
    REAL,DIMENSION(KW,N_SUB_CHANNEL_MAX),     INTENT(OUT)      ::  WL_Subchannels
! -------------------
! LOCAL:
    INTEGER       ::  ISUB
    REAL          ::  WL_INIT,WL_AUX

    
    IF(nsubchannels(IW) .GT. 1) THEN

        IF(ikdist)THEN

            DO ISUB = 1,nsubchannels(IW)
                bandwidth(IW,ISUB) = GAS_KD_DATA%ABS_GS_KD(IW)%WVL_SC(GAS_KD_DATA%ABS_GS_KD(IW)%NWL_SC(ISUB),ISUB) - GAS_KD_DATA%ABS_GS_KD(IW)%WVL_SC(1,ISUB)
            END DO

        END IF

        WL_INIT  = wl(IW) - (SUM(bandwidth(IW,1:nsubchannels(IW)))/2)

        WL_AUX = WL_INIT

        !MH Calculation of the center of each subchannel, proceeding like this enables in future to work with non-linear subchannel divisions
        DO ISUB = 1,nsubchannels(IW)

             WL_AUX = WL_AUX + (bandwidth(IW,ISUB)/2)

             WL_Subchannels(IW,ISUB) = WL_AUX

             WL_AUX = WL_AUX + (bandwidth(IW,ISUB)/2)

        END DO

    ELSE
        !MH avoids loose fo precision in center calculation
        WL_Subchannels(IW,1) = wl(IW)

    END IF


RETURN

END SUBROUTINE



SUBROUTINE filter_spectral_resampling(ikdist,IW,                   &
                                      GAS_ABS_DATA,                &
                                      WL_SC_KD,                    &
                                      BANDWIDTH,                   &
                                      filters_trans                &
                                      )

    use mod_intrpl_linear, only: LINEAR
    use mod_alloc_gas_lut, only: filters_shape, filter_channel_type, filter_transmission

    IMPLICIT NONE
! -------------------
! IN:
    INTEGER,                                        INTENT(IN)       ::  IW
    REAL,DIMENSION(NWL_GAS,N_SUB_CHANNEL_MAX),      INTENT(IN)       ::  WL_SC_KD
    LOGICAL,                                        INTENT(IN)       ::  IKDIST
    TYPE(DATA_GAS),                                 INTENT(IN)       ::  GAS_ABS_DATA
! -------------------
! INOUT:
    REAL,DIMENSION(N_SUB_CHANNEL_MAX),              INTENT(INOUT)    ::  BANDWIDTH
! -------------------
! OUT:
    TYPE(filter_transmission),                      INTENT(OUT)       ::  filters_trans
! -------------------
! LOCAL:
    INTEGER  :: IWL, ISUB
    REAL     :: wl_intrp

        DO ISUB=1,GAS_ABS_DATA%NSubCH !MH Number of subchannels for the actual channel

            DO IWL=1,GAS_ABS_DATA%NWL(ISUB)

                IF(ikdist) THEN
                    wl_intrp = WL_SC_KD(IWL,ISUB)
                ELSE
                    wl_intrp = GAS_ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWL)
                END IF

                filters_trans%transmission(IWL,ISUB) = LINEAR( filters_shape%filter_channel(IW)%WVL(1:filters_shape%filter_channel(IW)%NWL(ISUB),ISUB), filters_shape%filter_channel(IW)%transmission(1:filters_shape%filter_channel(IW)%NWL(ISUB),ISUB),filters_shape%filter_channel(IW)%NWL(ISUB), wl_intrp)

                IF(filters_trans%transmission(IWL,ISUB) .lt. 0.0) THEN
                    filters_trans%transmission(IWL,ISUB) = 0.0
                END IF

            END DO

            IF(ikdist) THEN
                BANDWIDTH(ISUB) = WL_SC_KD(GAS_ABS_DATA%NWL(ISUB),ISUB) - WL_SC_KD(1,ISUB)
            ELSE
                BANDWIDTH(ISUB) = GAS_ABS_DATA%ABS_GS_WL(ISUB)%WVL(GAS_ABS_DATA%NWL(ISUB)) - GAS_ABS_DATA%ABS_GS_WL(ISUB)%WVL(1)
            END IF

            

            filters_trans%NWL(ISUB) = GAS_ABS_DATA%NWL(ISUB)

        END DO

RETURN

END SUBROUTINE filter_spectral_resampling


END MODULE
