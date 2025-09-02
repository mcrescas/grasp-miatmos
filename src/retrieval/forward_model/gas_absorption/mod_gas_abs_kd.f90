! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

      MODULE mod_bbgas_kd
!XH   this module contains the constants required for k-distribution
!XH   subroutines
      IMPLICIT NONE

!XH   for k-distribution
      INTEGER, PARAMETER :: MAXP  = 23
!XH   maximum number of wavelengths affected by absorbing gases
      INTEGER, PARAMETER :: MAXNU = 208


!XH   for solar spectrum
      INTEGER, PARAMETER :: NBNU   = 208
!XH   default solar constant 1361.1 W/m2
      REAL,    PARAMETER :: CSLR   = 1361.1

      CONTAINS

 
    SUBROUTINE get_absorption_flags(RIN,                          &
                            igab,                                 &
                            ISTDAT,                               &
                            ikdist,                               &
                            VTP_PATH,KDIST_PATH)
    
    
    use mod_alloc_gas_lut, only: LUT_GASES
    use mod_retr_settings_derived_type
    use mod_c_utils, only: cstring2fstring

    IMPLICIT NONE
! --------------------------
! IN:
    type(retr_input_settings),        intent(in)          :: RIN
! --------------------------
! OUT:
    logical,                          intent(OUT)          :: igab, ikdist, ISTDAT
    CHARACTER(*),                     INTENT(OUT)          :: VTP_PATH,KDIST_PATH
! --------------------------
! LOCAL:
    character(len=GBL_FILE_PATH_LEN)                       :: aux_name,internal_file_path
    INTEGER                                                :: IS
    

    igab = RIN%gases%igab


    IF(igab .OR. RIN%emission%planck) then

        if(RIN%atmospheric_vertical_profile%stdat .eq. -1) then
            istdat = .False.

            call cstring2fstring(RIN%atmospheric_vertical_profile%VTP, aux_name)
            VTP_PATH = TRIM(aux_name)
        else
            istdat = .True.
        end if

    END IF

    IF(igab) THEN

      call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)

      if(RIN%gases%integration_method .eq. 1) then
         ikdist = .True.
       else
         ikdist = .False.
       end if
      
      if(ikdist) THEN

        call cstring2fstring(RIN%gases%kdist, aux_name)
        KDIST_PATH = TRIM(aux_name)

      else

        call cstring2fstring(RIN%gases%path_to_luts, aux_name)
        LUT_GASES%PATH = TRIM(internal_file_path)//TRIM(aux_name)

        call cstring2fstring(RIN%atmospheric_vertical_profile%vtp, aux_name)
        LUT_GASES%vtp = TRIM(aux_name)

        LUT_GASES%NSPECIES = RIN%gases%nlut_name

        do IS=1,LUT_GASES%NSPECIES
          call cstring2fstring(RIN%gases%lut_name(:,IS), aux_name)
          LUT_GASES%SPECIE(IS)%NAME = TRIM(aux_name)
        enddo

      end if

    ELSE

        ikdist = .False.

    END IF


    END SUBROUTINE get_absorption_flags


    SUBROUTINE get_absorption(RIN,                           &
                              igab,ihyper,                   &
                              ISTDAT,ikdist,                 &
                              IWb,IWe,wl,bandwidth,          &
                              VTP_PATH,                      &
                              NSubchannels,                  &
                              WL_Subchannels,                &
                              ISPECIE,                       &
                              NGAS_channel,                  &
                              ifilter,                       &
                              KDIST_PATH)
        use sub_gas_kd
        use mod_alloc_gas_lut, only: read_gas_abs, read_filter_shape,filters_shape
        use mod_par_inv, only : KW
        use mod_par_OS, only : NMG
        use mod_retr_settings_derived_type
        use mod_sdata_meas_type
        use mod_c_utils,  only: cstring2fstring

        IMPLICIT NONE
! --------------------------
! IN:
        type(retr_input_settings),            INTENT(IN)    :: RIN
        logical,                              INTENT(IN)    :: igab, ikdist, ISTDAT
        real,dimension(KW),                   INTENT(IN)    :: wl
        INTEGER,                              INTENT(IN)    :: IWb,IWe
        CHARACTER(*),                         INTENT(IN)    :: VTP_PATH,KDIST_PATH
! --------------------------
! INOUT:
        logical,dimension(KW),                INTENT(INOUT) :: ihyper
! --------------------------
! OUT:
        INTEGER,DIMENSION(KW),                INTENT(OUT)   :: NSubchannels
        REAL,DIMENSION(KW,N_SUB_CHANNEL_MAX), INTENT(OUT)   :: WL_Subchannels
        REAL, DIMENSION(KW,N_SUB_CHANNEL_MAX),INTENT(OUT)   :: bandwidth
        logical,dimension(KW),                INTENT(OUT)   :: ifilter
        LOGICAL,DIMENSION(KW,NMG),            INTENT(OUT)   :: ISPECIE
        INTEGER,dimension(KW),                INTENT(OUT)   :: NGAS_channel
! --------------------------
! LOCAL:
        INTEGER   :: IW,IS,II
        character(GBL_FILE_PATH_LEN)                        :: filters_file,internal_file_path, filters_folder
        
        call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)
        call cstring2fstring(RIN%gases%path_to_filters, filters_folder)

        IHYPER = .FALSE. !MH This boolean let us know if the actual wavelength is hyperspectral or it can be calculated in just one line of the RT.

        !MH For chemistry calculation we need to set this to 1 by default to avoid problems if filter info is not provided
        bandwidth = 1.0
        NSubchannels = 1
        ifilter = .FALSE.

          DO IW=IWb,IWe

               DO IS=1,RIN%gases%ngas_filters

                 DO II = 1,RIN%gases%nfilters_index_of_wavelength_involved(IS)
                    
                     IF(RIN%gases%filters_index_of_wavelength_involved(II,IS) == IW) THEN
                         IF(RIN%gases%filters_meas_type(IS) .EQ. meas_type_tod .OR. &    !MH In near future this will be restricted just to htod
                            RIN%gases%filters_meas_type(IS) .EQ. meas_type_htod .OR. &
                            RIN%gases%filters_meas_type(IS) .EQ. meas_type_I) THEN
                              IHYPER(IW) = .TRUE.

                              call cstring2fstring(RIN%gases%filters_file(:,II, IS), filters_file)
        
                              IF(TRIM(filters_file) .EQ. '.') THEN
                                    ifilter(IW) = .FALSE.
                              ELSE
                                    ifilter(IW) = .TRUE.
                                    filters_file = TRIM(internal_file_path)//TRIM(filters_folder)//TRIM(filters_file)
                              END IF

                              NSubchannels(IW) = RIN%gases%nsubchannels(IS)

                              IF(ifilter(IW)) THEN
                                    
                                    IF(read_filter_shape) THEN
                                        call get_filter_shape(RIN%gases%nsubchannels(IS),  IW,      & !IN
                                                              filters_file)    !OUT
                                    END IF

                                    bandwidth(IW,:) = filters_shape%filter_channel(IW)%bandwidth(:)
                                    WL_Subchannels(IW,:) = filters_shape%filter_channel(IW)%center_subchannels(:)
                                

                              ELSE

                                    bandwidth(IW,1:RIN%gases%nsubchannels(IS)) = RIN%gases%filters_spectral_resolution(IS)/RIN%gases%nsubchannels(IS)

                              ENDIF

                              !MHG We only stop reading the filters when all channels have been stored
                              IF(IW .EQ. IWe) THEN
                                    read_filter_shape = .FALSE.
                              END IF
                         ENDIF
                     ENDIF

                 END DO

               END DO

            if(igab) then
                call get_species_per_channel(RIN,WL(IW),           &
                                             NGAS_channel(IW),     &
                                             ISPECIE(IW,:)         &
                                             )
            end if

          END DO

        if(read_gas_abs) then

            !MH READ USER ATMOSPHERIC PROFILE
            if(igab .OR. RIN%emission%planck) then
                 CALL DATATM_GAS(RIN,TRIM(VTP_PATH),ISTDAT,RIN%atmospheric_vertical_profile%stdat)
            end if
            
             if(igab) then
                !MH READ COMPLETE LUT OR INTEGRATED LUT AND/OR USER ATMOSPHERIC PROFILE
                if(ikdist) then
                    call READ_KDIST(TRIM(KDIST_PATH),RIN%gases%nlut_name,ISPECIE)
                else
                    call READ_LUT(IGAB)

                end if

              end if
            
              read_gas_abs = .FALSE.

         end if !READ_LUT/KD/ATM_prof

          if(igab) then
        
              !MH Calculation of the center of each subchannel if flat filter selected
              DO IW=IWb,IWe
                 if(.not. ifilter(IW)) then
                    call GET_SPECTRAL_INTERVALS(ikdist,             &
                                                nsubchannels,       &
                                                bandwidth,          &
                                                wl,                 &
                                                IW,                 &
                                                WL_Subchannels)
                 end if
             END DO

          else

                DO IW=IWb,IWe
                    DO II=1,1
                        WL_Subchannels(IW,II) = wl(IW)
                    END DO
                END DO

          end if !igab
          

    END SUBROUTINE get_absorption



    SUBROUTINE READ_LUT(IGAB)

      use mod_abs_kd, only : DATA_ABS
      use sub_gas_kd
      use mod_alloc_gas_lut, only: LUT_GASES 

      IMPLICIT NONE

      INTEGER   :: K

    ! --------------------------
    ! IN:
          LOGICAL,                      INTENT(IN)    :: IGAB

        IF(IGAB) THEN
                               
                    !MH READING LOOK UP TABLE FOR SELECTED SPECIES
                    DO K=1, LUT_GASES%NSPECIES !MH LOOP OVER SPECIES
                        !MH HERE WE READ EXTINCTIO IN LEVEL AND THEN IS INTEGRATED TO EACH LAYER
                        CALL READ_EXT_PRO_LUT_V2(TRIM(LUT_GASES%PATH),                        &
                                                 LUT_GASES%SPECIE(K)%NAME,                    &
                                                 LUT_GASES%SPECIE(K)%WL_GAS,                  &
                                                 K,LUT_GASES%SPECIE(K)%T_LUT,                 &
                                                 LUT_GASES%SPECIE(K)%P_LUT,                   &
                                                 LUT_GASES%SPECIE(K)%CCT,                     &
                                                 LUT_GASES%SPECIE(K)%ALT_LUT,                 &
                                                 LUT_GASES%SPECIE(K)%NWL_LUT,                 &
                                                 LUT_GASES%SPECIE(K)%NP_LUT,                  &
                                                 LUT_GASES%SPECIE(K)%NT_LUT                   &
                                                 )
                    END DO


        END IF !IGAB

    RETURN
    
    END SUBROUTINE READ_LUT



      SUBROUTINE GET_GAS_DATA(RIN,                       &
                              ipix,ind_wl,IW,WL,         &
                              IKDIST,IHYPER,             &
                              ifilter,                   &
                              BANDWIDTH,                 &
                              nsubchannels,              &
                              WL_Subchannels,            &
                              ISPECIE,                   &
                              NGAS_channel,              &
                              NGAS, CGAS,                &
                              GAS_ABS_DATA,              &
                              GOUT_gases,                &
                              filters_trans,             &
                              WL_Planck                  &
                              )

     use mod_par_OS,  only : NMG
     use mod_retr_settings_derived_type
     use mod_retr_general_output_derived_type

     use mod_abs_kd, only : DATA_ABS
     use sub_gas_kd

     use mod_alloc_gas_lut, only: LUT_GASES
     use mod_alloc_gas_lut, only: GAS_KD_DATA
     use mod_alloc_gas_lut, only: ATM_PROF
     use mod_alloc_gas_lut, only: filters_shape, filter_channel_type, filter_transmission

     IMPLICIT NONE

! --------------------------
! IN:
      TYPE(retr_input_settings),           INTENT(IN)       ::    RIN
      LOGICAL,                             INTENT(IN)       ::    IKDIST, IHYPER, ifilter
      INTEGER,                             INTENT(IN)       ::    NGAS,ipix,ind_wl,nsubchannels, IW
      REAL,DIMENSION(NMG),                 INTENT(IN)       ::    CGAS
      real,dimension(N_SUB_CHANNEL_MAX),   INTENT(IN)       ::    WL_Subchannels
      real,                                INTENT(IN)       ::    WL
      LOGICAL, DIMENSION(NMG),             INTENT(IN)       ::    ISPECIE
      INTEGER,                             INTENT(IN)       ::    NGAS_channel
! --------------------------
! INOUT:
      TYPE(output_segment_gases),          INTENT(INOUT)    ::    GOUT_gases
      REAL,DIMENSION(N_SUB_CHANNEL_MAX),   INTENT(INOUT)    ::    BANDWIDTH

! --------------------------
! OUT:
      TYPE(DATA_GAS),                      INTENT(OUT)      ::    GAS_ABS_DATA
      REAL,DIMENSION(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX), INTENT(OUT)    ::    WL_Planck
      TYPE(filter_transmission),               INTENT(OUT)      ::     filters_trans
! --------------------------
! LOCAL:
      INTEGER                                               :: INU, ISUB, J, K, IK, IL, IIKD

      REAL                                                   :: min_diff, aux_wl
      INTEGER, DIMENSION (NMG,2,N_SUB_CHANNEL_MAX)           :: index
      REAL                                                   :: LUT_width
      REAL,DIMENSION(NLEVEL_GAS-1)                           :: EXT_INTERP,CCT_INTERP
      REAL,DIMENSION(NMG)                                    :: GAS_C_REF
      REAL                                                   :: average_transmittance
      REAL,DIMENSION(MAXKD)                                  :: weigths
      INTEGER                                                :: nlines_bin
      LOGICAL,DIMENSION(2)                                   :: init_KD
      REAL                                                   :: filter_shape_beg, filter_shape_end 

    !MH   read extinction Coefficientes/k-distribution for different especies

    !MH set default value
    GAS_ABS_DATA%NSubCH = 1
    LUT_width = 0.0

    IF(IKDIST) THEN

         GAS_ABS_DATA%NWL = 1   !MH WITH KD WE ONL CONSIDER ONE CHANNEL WITH BINS INSIDE OR SUBCHANNELS
         GAS_ABS_DATA%NWL(1:nsubchannels) = GAS_KD_DATA%ABS_GS_KD(IW)%NWL_SC(1:nsubchannels)

        !MH HERE IS MISSING THE CHECK OF CONSISTENCY BETWEEN THE NUMBER OF SPECIES IN K-DIST AND IN SETTINGS and between number of subchannels

    ELSE
        
        !MH LUT WAVELENGTH SELECTION for each subchannel
        DO ISUB=1, nsubchannels !MH Number of subchannels for the actual channel

            !MH If there is no a specific filter shape selected the filters_shape structure is not declared
            IF(ifilter) THEN
                filter_shape_beg = filters_shape%filter_channel(IW)%WVL(1,ISUB)
                filter_shape_end = filters_shape%filter_channel(IW)%WVL(filters_shape%filter_channel(IW)%NWL(ISUB),ISUB)
            ELSE
                filter_shape_beg = 0
                filter_shape_end = 0
            END IF

            call get_LUT_spectral_limits(RIN,ikdist,                                       &
                                         ifilter,                                          &
                                         nsubchannels,                                     &
                                         WL_Subchannels(ISUB),                             &
                                         BANDWIDTH(ISUB),                                  &
                                         filter_shape_beg,                                 &
                                         filter_shape_end,                                 &
                                         ISPECIE,                                          &
                                         index(:,:,ISUB),                                  &
                                         GAS_ABS_DATA%NWL(ISUB)                            &
                                         )

        END DO

    ENDIF

!write(*,*)WL_Subchannels(1:nsubchannels)
!write(*,*)BANDWIDTH(1:nsubchannels), ind_wl, IKDIST
   !MH Vertical profile variable assigment
    GAS_ABS_DATA%NLV=ATM_PROF%NLV
    GAS_ABS_DATA%HLV(1:ATM_PROF%NLV)=ATM_PROF%ALT(1:ATM_PROF%NLV)
    GAS_ABS_DATA%NLY=ATM_PROF%NLV-1
    GAS_ABS_DATA%HLY(1:ATM_PROF%NLV-1)=0.5*(ATM_PROF%ALT(1:ATM_PROF%NLV-1)+ATM_PROF%ALT(2:ATM_PROF%NLV))
    GAS_ABS_DATA%NSubCH = nsubchannels

    DO K=1, GAS_ABS_DATA%NLY !MH LOOP OVER SPECIES
        GAS_ABS_DATA%DZ(K) = GAS_ABS_DATA%HLV(K) - GAS_ABS_DATA%HLV(K+1)
    END DO


    !MH Fill the wavlengths and number of wavlengths per channel/subchannel for LBL approach

    IF(.NOT. ikdist)THEN

        DO ISUB=1,GAS_ABS_DATA%NSubCH !MH Number of subchannels for the actual channel
            
            IF(GAS_ABS_DATA%NWL(ISUB) .GT. NWL_GAS) THEN

                WRITE(*,*) 'ERROR: Channel ', WL_Subchannels(ISUB)*1000, 'nm contains ', GAS_ABS_DATA%NWL(ISUB) ,' lines. The maximum number of lines per channel is ', NWL_GAS
                STOP

            END IF

            GAS_ABS_DATA%ABS_GS_WL(ISUB)%WVL(:) = -999

            DO INU=1,GAS_ABS_DATA%NWL(ISUB) !MH Number of lines for the actual (sub)channel

                DO K=1, LUT_GASES%NSPECIES !MH LOOP OVER SPECIES

                    IF (((GAS_ABS_DATA%ABS_GS_WL(ISUB)%WVL(INU) .LT. 0.0)) .and. (ISPECIE(K)) ) THEN
                        WL_Planck(INU,ISUB) = LUT_GASES%SPECIE(K)%WL_GAS(index(K,1,ISUB) + INU - 1)
                        GAS_ABS_DATA%ABS_GS_WL(ISUB)%WVL(INU)   = LUT_GASES%SPECIE(K)%WL_GAS(index(K,1,ISUB) + INU - 1)
                    END IF

                END DO

            END DO

        END DO

    END IF


    IF(ifilter) THEN
        
            CALL filter_spectral_resampling(ikdist,IW,                        &
                                            GAS_ABS_DATA,                     &
                                            GAS_KD_DATA%ABS_GS_KD(IW)%WVL_SC, &
                                            BANDWIDTH,                        & !MH readjust bandwidth to LUT limits
                                            filters_trans                     &
                                            )
                
    END IF


    !MH initialization for gas absorption calculations
    GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(:) = 0.0

        IF(IKDIST) THEN
                
                init_KD(1) = .TRUE.

                DO J=1, GAS_KD_DATA%ABS_GS_KD(IW)%NSubCH

                    !MH assign correponding mapping function
                    GAS_ABS_DATA%DATA_KD_Channel(1)%MAPPING_FUNCTION(1:GAS_KD_DATA%ABS_GS_KD(IW)%NWL_SC(J),J) = GAS_KD_DATA%ABS_GS_KD(IW)%MAPPING_FUNCTION(1:GAS_KD_DATA%ABS_GS_KD(IW)%NWL_SC(J),J)
                    !MH assign correponding a ount of wvl per subchannel (POSSIBILY THIS CAN BE REMOVED IN FUTURE)
                    GAS_ABS_DATA%DATA_KD_Channel(1)%NWL_SC(J) =GAS_KD_DATA%ABS_GS_KD(IW)%NWL_SC(J)

                    DO K=1, NGAS

                        IF(ISPECIE(K)) THEN

                            IF(J .EQ. 1) THEN
                                GAS_C_REF(K) = GAS_KD_DATA%TOTAL_GAS_C_REF_KD(K)
                            END IF

                            IF(init_KD(1)) THEN
                                    GAS_ABS_DATA%DATA_KD_Channel(1)%COEFKD = 0.0
                                    GAS_ABS_DATA%NSubCH = GAS_KD_DATA%ABS_GS_KD(IW)%NSubCH
                                    IF(GAS_ABS_DATA%NSubCH  .ne. nsubchannels)THEN
                                        WRITE(*,*) 'ERROR: Number of subchannels in k-distribution file: ', GAS_ABS_DATA%NSubCH, ' not consistent with number of subchannels in settings file: ', nsubchannels, 'for channel: ', WL, ' um.'
                                        STOP
                                    END IF
                                    IF(GAS_KD_DATA%ABS_GS_KD(IW)%NGAS(J) .ne. NGAS_channel)THEN
                                        WRITE(*,*)'ERROR: Number of gaseous species in k-distribution file: ', GAS_KD_DATA%ABS_GS_KD(IW)%NGAS(J), ' not consistent with number of gaseous species in settings file: ', NGAS, 'for channel: ', WL, ' um.'
                                        STOP
                                    END IF
                                    init_KD(1) = .FALSE.
                            END IF

                            init_KD(2) = .TRUE.

                            GAS_ABS_DATA%DATA_KD_Channel(1)%NEXP(J)  = GAS_KD_DATA%ABS_GS_KD(IW)%NEXP(J)

                            DO IK = 1, GAS_ABS_DATA%DATA_KD_Channel(1)%NEXP(J) !MH LOOP OVER BINS

                                GAS_ABS_DATA%DATA_KD_Channel(1)%AIKD(IK,J)=GAS_KD_DATA%ABS_GS_KD(IW)%AIKD(IK,J)

                                !MH: TODO: Further developments are needed to unify the calculation of filter weights for god and for radiance to avoid duplicity of the calculations

                                IF(init_KD(2)) THEN !MH This logic is to avoid the repetition of the same calculation

                                    average_transmittance = 0.0
                                    nlines_bin = 0

                                    IF(ifilter)THEN

                                        DO IIKD=1,filters_trans%NWL(J) !MH Average transmittance of each bin

                                            IF(GAS_ABS_DATA%DATA_KD_Channel(1)%MAPPING_FUNCTION(IIKD,J) == IK-1)THEN
                                                average_transmittance = average_transmittance + filters_trans%transmission(IIKD,J)
                                                nlines_bin = nlines_bin + 1
                                            END IF
                                            
                                        END DO

                                        !!!!!!!!!!!!!
                                        !MH: Average transmittance method, gives the best results for OLCI19
                                        average_transmittance = average_transmittance/nlines_bin
                                        weigths(IK) = average_transmittance*GAS_KD_DATA%ABS_GS_KD(IW)%AIKD(IK,J)
                                        !!!!!!!!!!!!!

                                    ELSE

                                        average_transmittance = 1.0
                                        weigths(IK) = GAS_KD_DATA%ABS_GS_KD(IW)%AIKD(IK,J)

                                    END IF

                                END IF
                    
                                DO IL=1, ATM_PROF%NLV-1

                                     GAS_ABS_DATA%DATA_KD_Channel(1)%COEFKD(IL,IK,J)= GAS_ABS_DATA%DATA_KD_Channel(1)%COEFKD(IL,IK,J) + (GAS_KD_DATA%ABS_GS_KD(IW)%COEFKD(IL,IK,K,J)*CGAS(K)/GAS_C_REF(K))
                                     
                                END DO !NLEVEL_GAS

                                GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(K) = GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(K) + (SUM( GAS_KD_DATA%ABS_GS_KD(IW)%COEFKD(1:ATM_PROF%NLV-1,IK,K,J)*CGAS(K)/GAS_C_REF(K))*(weigths(IK))/SUM(GAS_KD_DATA%ABS_GS_KD(IW)%AIKD(1:GAS_KD_DATA%ABS_GS_KD(IW)%NEXP(J),J))) * bandwidth(J)/SUM(bandwidth(1:GAS_KD_DATA%ABS_GS_KD(IW)%NSubCH))
                                
                            END DO!NEXP

                            init_KD(2) = .FALSE.

                        ELSE

                            GAS_ABS_DATA%DATA_KD_Channel(1)%NEXP(J)  = 0

                        END IF !ISPECIE
                        
                    END DO !NGAS
                END DO !NSubCH
            
                GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(0) = SUM(GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(1:NGAS))

        ELSE

            
            DO ISUB=1,GAS_ABS_DATA%NSubCH !MH Number of subchannels for the actual channel
                
                GAS_ABS_DATA%ABS_GS_WL(ISUB)%COEFCN= 0.0


                DO INU=1,GAS_ABS_DATA%NWL(ISUB) !MH Number of lines for the actual (sub)channel

                    DO K=1, LUT_GASES%NSPECIES !MH LOOP OVER SPECIES

                        
                        IF(ISPECIE(K)) THEN
                                CALL LUT_BILINEAR_INTERP_LAYERS(LUT_GASES%SPECIE(K)%P_LUT,                    &
                                                        LUT_GASES%SPECIE(K)%T_LUT,                            &
                                                        LUT_GASES%SPECIE(K)%ALT_LUT,                          &
                                                        LUT_GASES%SPECIE(K)%CCT,                              &
                                                        LUT_GASES%SPECIE(K)%NP_LUT,                           &
                                                        LUT_GASES%SPECIE(K)%NT_LUT,                           &
                                                        ATM_PROF%NLV,                                         &
                                                        LUT_GASES%SPECIE(K)%EXT(index(K,1,ISUB) + INU - 1,:,:),    &
                                                        ATM_PROF%P,ATM_PROF%T,ATM_PROF%ALT,CCT_INTERP,EXT_INTERP)
                                                        
        
                                IF(ISUB .EQ. 1) THEN
                                    GAS_C_REF(K) = SUM(CCT_INTERP(1:ATM_PROF%NLV-1))
                                END IF
                                !MH Here we sum up all the gases as the same component
                                        GAS_ABS_DATA%ABS_GS_WL(ISUB)%COEFCN(1:ATM_PROF%NLV-1,INU)= GAS_ABS_DATA%ABS_GS_WL(ISUB)%COEFCN(1:ATM_PROF%NLV-1,INU) + ( EXT_INTERP(1:ATM_PROF%NLV-1)*CGAS(K)/GAS_C_REF(K))

                                        if(ifilter) then

                                            if(INU .EQ. GAS_ABS_DATA%NWL(ISUB))THEN

                                                average_transmittance = (filters_trans%transmission(INU-1,ISUB) + filters_trans%transmission(INU,ISUB))/2

                                            ELSE
                                                    average_transmittance = (filters_trans%transmission(INU+1,ISUB) + filters_trans%transmission(INU,ISUB))/2

                                            END IF
                                        else
                                            average_transmittance = 1.0
                                        end if                                        
                                        
                                !MH Store the integrated values in the output structure
                                        GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(K) = GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(K) + SUM( EXT_INTERP(1:ATM_PROF%NLV-1)*CGAS(K)/GAS_C_REF(K))*(LUT_GASES%SPECIE(K)%WL_GAS(index(K,1,ISUB) + INU) - LUT_GASES%SPECIE(K)%WL_GAS(index(K,1,ISUB) + INU-1) )*average_transmittance

                                        IF(K .EQ. 1) THEN
                                            LUT_width = LUT_width + (LUT_GASES%SPECIE(K)%WL_GAS(index(K,1,ISUB) + INU) - LUT_GASES%SPECIE(K)%WL_GAS(index(K,1,ISUB) + INU-1) )
                                        END IF

                        END IF

                    END DO  !NSPECIES


                END DO !NWL

            END DO !NSubch

        END IF

    IF(.NOT. IKDIST) THEN
        !MH Renormalization of Total gas absorption for this channel
        !MH If k-dist is selected this is done in the k-dist itself
        DO K=1, LUT_GASES%NSPECIES !MH LOOP OVER SPECIES
            GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(K) = GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(K)/LUT_width
        END DO
        GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(0) = SUM(GOUT_gases%pixel(ipix)%wl(ind_wl)%abs(1:NGAS))
    END IF


    RETURN
    END SUBROUTINE GET_GAS_DATA




    SUBROUTINE  get_species_per_channel(RIN,WL,           &
                                        NGAS_channel,     &
                                        ISPECIE           &
                                        )

    use mod_retr_settings_derived_type
    use mod_par_OS,  only : NMG

    IMPLICIT NONE

    ! --------------------------
    ! IN:
      TYPE(retr_input_settings),     INTENT(IN)  :: RIN
      REAL,                          INTENT(IN)  :: WL
    ! --------------------------
    ! OUT:
     INTEGER,                        INTENT(OUT) :: NGAS_channel
     LOGICAL, DIMENSION(NMG),        INTENT(OUT) :: ISPECIE
    ! --------------------------
    ! LOCAL:
      INTEGER   :: K,IK

     NGAS_channel = 0
   
     DO K=1, RIN%gases%nlut_name !MH LOOP OVER SPECIES
              
          ISPECIE(K) = .FALSE.
          DO IK=1, RIN%gases%ngases_spectral_range(K) !MH LOOP OVER AL POSSIBLE SPECTRAL RANGES OF EACH SPECIE
              IF( (WL .GE. RIN%gases%spectral_ranges_min(IK,K)) .AND. (WL .LE. RIN%gases%spectral_ranges_max(IK,K)) ) THEN

                  ISPECIE(K) = .TRUE.
                  NGAS_channel = NGAS_channel + 1
                  EXIT
              END IF
          END DO !MH GAS SPECTRAL RANGE

     END DO !MH SPECIE


     END SUBROUTINE get_species_per_channel



     SUBROUTINE  get_LUT_spectral_limits(RIN,ikdist,       &
                                         ifilter,          &
                                         nsubchannels,     &
                                         WL,BANDWIDTH,     &
                                         filter_shape_beg, &
                                         filter_shape_end, &
                                         ISPECIE,          &
                                         index,            &
                                         NWL               &
                                         )

     use mod_retr_settings_derived_type
    
     use mod_par_OS,  only : NMG
     use mod_alloc_gas_lut, only: LUT_GASES

     IMPLICIT NONE

     ! --------------------------
     ! IN:
       TYPE(retr_input_settings),     INTENT(IN) :: RIN
       LOGICAL,                       INTENT(IN) :: ikdist, ifilter
       INTEGER,                       INTENT(IN) :: nsubchannels
       REAL,                          INTENT(IN) :: WL,BANDWIDTH,filter_shape_beg,filter_shape_end
       LOGICAL, DIMENSION(NMG),       INTENT(IN) :: ISPECIE
    ! --------------------------
    ! OUT:
      INTEGER, DIMENSION(NMG,2),      INTENT(OUT)  :: index
      INTEGER,                        INTENT(OUT)  :: NWL


    ! --------------------------
    ! LOCAL:
      LOGICAL   :: i_any_gas
      REAL      :: min_diff, var_margin
      INTEGER   :: K,IK


      var_margin = 0.001 !MH Maximum difference allowed to find LUT wavelength position in um
!      var_margin = 0.001 !MH Maximum difference allowed to find LUT wavelength position in um
      NWL = -999

      DO K=1, LUT_GASES%NSPECIES !MH LOOP OVER SPECIES
                
            DO IK=1, RIN%gases%ngases_spectral_range(K) !MH LOOP OVER AL POSSIBLE SPECTRAL RANGES OF EACH SPECIE
                IF( (WL .GE. RIN%gases%spectral_ranges_min(IK,K)) .AND. (WL .LE. RIN%gases%spectral_ranges_max(IK,K)) ) THEN
                         
                    min_diff = 1.0

                    IF(ifilter) THEN
                        
                        !MH Set begining of channel
                        CALL find_closest_value(LUT_GASES%SPECIE(K)%WL_GAS,LUT_GASES%SPECIE(K)%NWL_LUT, filter_shape_beg,min_diff,index(K,1))


                        IF(min_diff .GT. var_margin) THEN

                            WRITE(*,*) 'WARNING: Begining of channel corresponding to WL =', WL, 'um found in LUT number',K,'with a spectral difference bigger than ', var_margin*1000,' nm. Actual difference:',min_diff*1000 ,'nm. Begining of channel: ',filter_shape_beg*1000, ' nm, closest LUT value: ', LUT_GASES%SPECIE(K)%WL_GAS(index(K,1))*1000, ' nm.'
                                EXIT
                        END IF

                        !MH Set end of channel
                        min_diff = 1.0

                        CALL find_closest_value(LUT_GASES%SPECIE(K)%WL_GAS,LUT_GASES%SPECIE(K)%NWL_LUT, filter_shape_end,min_diff,index(K,2))
                        !MH To avoid integrating twice the same line (if nsubchannels > 1)
                        index(K,2) = index(K,2) - 1


                        IF(min_diff .GT. var_margin) THEN
                            WRITE(*,*) 'WARNING: End of channel corresponding to WL =', WL, 'um found in LUT number',K,'with a spectral difference bigger than 1.5 nm. Actual difference:',min_diff*1000 ,'nm. End of channel: ',filter_shape_end*1000, ' nm, closest LUT value: ', LUT_GASES%SPECIE(K)%WL_GAS(index(K,2))*1000, ' nm.'
                        END IF
            
                    ELSE

                        !MH Set begining of channel
                        
                        CALL find_closest_value(LUT_GASES%SPECIE(K)%WL_GAS,LUT_GASES%SPECIE(K)%NWL_LUT, WL-(BANDWIDTH/2),min_diff,index(K,1))

                        IF(min_diff .GT. var_margin) THEN
                            WRITE(*,*) 'WARNING: Begining of channel corresponding to WL =', WL, 'um found in LUT number',K,'with a spectral difference bigger than 1.5 nm. Actual difference:',min_diff*1000 ,'nm. Begining of channel: ',(WL-(BANDWIDTH/2))*1000, ' nm, closest LUT value: ', LUT_GASES%SPECIE(K)%WL_GAS(index(K,1))*1000, ' nm.'
                            EXIT
                        END IF

                        !MH Set end of channel
                        min_diff = 1.0

                        CALL find_closest_value(LUT_GASES%SPECIE(K)%WL_GAS,LUT_GASES%SPECIE(K)%NWL_LUT, WL+(BANDWIDTH/2),min_diff,index(K,2))
                        !MH To avoid integrating twice the same line (if nsubchannels > 1)
                        index(K,2) = index(K,2) - 1
                        
                        IF(min_diff .GT. var_margin) THEN
                            WRITE(*,*) 'WARNING: End of channel corresponding to WL =', WL, 'um found in LUT number',K,'with a spectral difference bigger than 1.5 nm. Actual difference:',min_diff*1000 ,'nm. End of channel: ',(WL+(BANDWIDTH/2))*1000, ' nm, closest LUT value: ', LUT_GASES%SPECIE(K)%WL_GAS(index(K,2))*1000, ' nm.'
                            EXIT
                        END IF
    
                    END IF

                    !MH WITH THIS APPROACH IF BANDWITH = 0 WE ONLY TAKE ONE LINE
                    IF(NWL .EQ. -999) THEN
                        NWL = index(K,2) - index(K,1) + 1
                    END IF

                    IF(ABS(NWL - (index(K,2) - index(K,1) + 1)) .GT. 3) THEN
                        WRITE(*,*) 'ERROR: The LUTs have not the same spectral resolution', NWL, index(K,2), index(K,1), K, LUT_GASES%SPECIE(K)%WL_GAS(100:120)
                        STOP
                    END IF
                    
                    !MH Avoiding memory issues caused by cgasa miscalculations of WL
!                    IF(NWL .GT. (index(K,2) - index(K,1) + 1)) THEN
!                        NWL = index(K,2) - index(K,1) + 1
!                    END IF
                    NWL = index(K,2) - index(K,1) + 1


                    EXIT
                END IF
            END DO !MH GAS SPECTRAL RANGE

     END DO !MH SPECIE

     !MH CHECK THAT AT LEAST ONE GAS SPECIE IS TAKEN INTO ACCOUNT
     i_any_gas = .FALSE.
     DO K=1, LUT_GASES%NSPECIES !MH LOOP OVER SPECIES

        IF(ISPECIE(K)) THEN
            i_any_gas = .TRUE.
            EXIT
        END IF
    END DO


    IF((.NOT. i_any_gas) .and. (.not. ikdist)) THEN
        WRITE(*,*) 'ERROR: Gas absorption active but any gas spectral range overlapping in WL =', WL, 'um. Or gas LUT and filter ranges are not matching. At least one gas per wavelength has to exist if gas absorption is active.'
        stop
    END IF


    END SUBROUTINE get_LUT_spectral_limits



      SUBROUTINE GET_GAS_PROFILE ( ABS_DATA,IKDIST,          &  ! IN
                                   DISCRVD,                  &
                                   IWG,ISUB,                 &
                                   NLV_MAX,                  &
                                   NSPECIES,                 &
                                   NLV, HLV, PRF,            &  ! OUT
                                   EXT                       &
                                 )
      USE mod_par_inv,   ONLY : KVERTM
      USE sub_gas_kd, only : DATA_GAS
      USE mod_vertical_distr_derived_type
      IMPLICIT NONE
! --------------------------
! IN:
      INTEGER,                   INTENT(IN) :: IWG,ISUB
      LOGICAL,                   INTENT(IN) :: IKDIST
!XH   data of gas absorption
      TYPE(DATA_GAS),            INTENT(IN) :: ABS_DATA
      INTEGER,                   INTENT(IN)  :: NLV_MAX, NSPECIES
! --------------------------
! INOUT:
      INTEGER,                  INTENT(INOUT) :: NLV
      REAL, DIMENSION(NLV_MAX), INTENT(INOUT) :: HLV, PRF
      TYPE(discret_vertical_distribution), INTENT(INOUT) :: DISCRVD
! --------------------------
! OUT:
      REAL,                     INTENT(OUT) :: EXT
! --------------------------
! LOCAL:
      INTEGER :: NLY, I
      REAL, DIMENSION(NLV_MAX) :: HLY, TAU
!
!XH   number of levels
!PL      NLV = ABS_DATA%NLV
!PL      HLV(NLV:1:-1) = ABS_DATA%HLV(1:NLV)

      IF (NLV_MAX .LT. NLV) THEN
         WRITE(*,*) 'ABS_GAS_DATA in mod_abs_kd : ',                    &
                    'number of levels', NLV, ' > ',                     &
                    'maximum of levels', NLV_MAX
         STOP
      END IF

      NLY = NLV - 1
      HLY(1:NLY) = ABS_DATA%HLY(1:NLY)

      TAU(1:NLY) = 0.0


       IF(IKDIST) THEN
          TAU(1:NLY) =  ABS_DATA%DATA_KD_Channel(1)%COEFKD(1:NLY,IWG,ISUB)
       ELSE
          TAU(1:NLY) =  ABS_DATA%ABS_GS_WL(ISUB)%COEFCN(1:NLY,IWG)
       END IF

     DISCRVD%gas_vert_prof(1:NLY) = TAU(1:NLY)

!XH   calculate gas profile, assuming prof = 0.0 at TOA
      PRF(1) = 0.0

      DO I = 2, NLV

!XH      TAU(I-1) = 0.5*[PRF(I-1)+PRF(I)]*[HLV(I-1)-HLV(I)]
          PRF(I) = (2.0*TAU(I-1)/(HLV(I-1)-HLV(I)))-PRF(I-1)
!          write(*,*) (HLV(I-1)-HLV(I))
!		 if (PRF(I) .lt. 0) then
!			 write(*,*) 'negative profile',I,PRF(I),PRF(I-1),TAU(I-1),HLV(I-1),HLV(I),HLV(I-1)-HLV(I)
!			 stop
!		 endif
      END DO

!XH   total extinction of all absorbing gases for all layers
      EXT = SUM(TAU(1:NLY))

      RETURN
      END SUBROUTINE GET_GAS_PROFILE


    SUBROUTINE modified_gas_profile(NLV,HLV, GAS_VERT, NH, H_KM,norm,VAL)

    IMPLICIT NONE
! -------------------
! IN:
    INTEGER,              INTENT(IN)  :: NLV,NH
    REAL, DIMENSION(NH), INTENT(IN)  :: HLV, GAS_VERT, H_KM
! -------------------
! OUT:

    REAL, INTENT(OUT):: norm
    REAL, DIMENSION(NH), INTENT(OUT) :: VAL
! -------------------
! LOCAL:

    REAL      ::   xdum
    INTEGER   ::   ifit,i_high

    do ifit = 1, NH  !MH Loop over Aerosol profile points
       
       !LL Determine the location of the desired abscissas within
       !LL the known abscissas
       i_high = 1
       do while ( (HLV(i_high) > H_KM(ifit)) .and. (i_high <= NLV) )
          i_high = i_high + 1
       enddo


       xdum = H_KM(ifit)
       !LL If the abscissa is located outside of the known span of abscissas,
       !LL extrapolate slope of interior points. Otherwise, interpolate
       !LL between nearest neighbors.
       if ( (i_high == 1) .or. (i_high == NLV+1) ) then
          call extrapolate_linear_fit_dp &
               (xdum, NLV, i_high, HLV, GAS_VERT, VAL(ifit))
       else
          call interpolate_linear_fit_dp (xdum, NLV, i_high, HLV, GAS_VERT, VAL(ifit))
       endif

    end do

    !MH  Renormalize Gas profile for the altitudes corresponding to aereosol profile
    norm = 0
    do ifit = 1, (NH-1)

        norm = norm + (((VAL(ifit) + VAL(ifit+1))/2)*(H_KM(ifit) - H_KM(ifit+1)))

    end do
    
    END SUBROUTINE modified_gas_profile





      SUBROUTINE ABS_GAS_PROFILE ( ABS_DATA,       &  ! IN
                                   IW,             &
                                   I1, I2, I3,     &
                                   NLV_MAX,        &
                                   NLV, HLV, PRF,  &  ! OUT
                                   EXT             &
                                 )
      USE mod_par_inv,   ONLY : KVERTM
      USE mod_abs_kd, only : DATA_ABS
      IMPLICIT NONE
!------
! IN:
      INTEGER,                   INTENT(IN) :: IW
      INTEGER,                   INTENT(IN) :: I1,I2,I3
!XH   data of gas absorption
      TYPE(DATA_ABS),            INTENT(IN) :: ABS_DATA
      INTEGER,                   INTENT(IN)  :: NLV_MAX
!------
! INOUT:
      INTEGER,                  INTENT(INOUT) :: NLV
      REAL, DIMENSION(NLV_MAX), INTENT(INOUT) :: HLV, PRF

!------
! OUT:
      REAL,                     INTENT(OUT) :: EXT

!------
! LOCAL:
      INTEGER :: NLY, I
      REAL, DIMENSION(NLV_MAX) :: HLY, TAU
!
!XH   number of levels
      NLV = ABS_DATA%NLV
      HLV(NLV:1:-1) = ABS_DATA%HLV(1:NLV)
      IF (NLV_MAX .LT. NLV) THEN
         WRITE(*,*) 'ABS_GAS_DATA in mod_abs_kd : ',                    &
                    'number of levels', NLV, ' > ',                     &
                    'maximum of levels', NLV_MAX
         STOP
      END IF
!XH   absorbing gas optical thickness
      NLY = ABS_DATA%NLY
      HLY(NLY:1:-1) = ABS_DATA%HLY(1:NLY)
      TAU(NLY:1:-1) = ABS_DATA%ABS_GS_WL(IW)%COEFCN(1:NLY)              &
                     +ABS_DATA%ABS_GS_WL(IW)%COEFKD(1:NLY,I1,1)         &
                     +ABS_DATA%ABS_GS_WL(IW)%COEFKD(1:NLY,I2,2)         &
                     +ABS_DATA%ABS_GS_WL(IW)%COEFKD(1:NLY,I3,3)
!XH   calculate gas profile, assuming prof = 0.0 at TOA
      PRF(1) = 0.0
      DO I = 2, NLV
!XH      TAU(I-1) = 0.5*[PRF(I-1)+PRF(I)]*[HLV(I-1)-HLV(I)]
         PRF(I) = 2.0*TAU(I-1)/(HLV(I-1)-HLV(I))-PRF(I-1)
      END DO
!XH   total extinction of all absorbing gases for all layers
      EXT = SUM(TAU(1:NLY))
!
      RETURN
      END SUBROUTINE ABS_GAS_PROFILE


    SUBROUTINE RT_GAS_PROFILE(ABS_DATA,IKDIST,       &
                              IWG,ISUB,              &
                              NSPECIES,              &       !IN
                              DISCRVD,               &       !OUT
                              EXT_gas                &
                              )

    USE mod_par_inv,   ONLY : KVERTM
    USE mod_par_OS,    ONLY : KVERT_WD
    USE sub_gas_kd, only : DATA_GAS
    USE mod_vertical_distr_derived_type
    IMPLICIT NONE

    ! -------------------
    ! IN:
    INTEGER,                   INTENT(IN) :: IWG,ISUB,NSPECIES
    LOGICAL,                   INTENT(IN) :: IKDIST
    

    ! -------------------
    ! INOUT:
    TYPE(DATA_GAS),            INTENT(INOUT) :: ABS_DATA
    TYPE(discret_vertical_distribution), INTENT(INOUT) :: DISCRVD

    ! -------------------
    ! OUT:
    REAL, INTENT(OUT)                                  ::   EXT_gas

    !--------------------
    ! LOCAL:
    real,dimension(KVERT_WD)                           ::   gas_vert_prof


    CALL GET_GAS_PROFILE (    ABS_DATA,ikdist,DISCRVD,           & ! IN
                              IWG,ISUB,                          &
                              ABS_DATA%NLV,                      &
                              NSPECIES,                          &
                              ABS_DATA%NLV,                      & ! OUT
                              ABS_DATA%HLV(1:ABS_DATA%NLV),      &
                              gas_vert_prof(1:ABS_DATA%NLV),     &
                              EXT_gas                            &
                       )

     !MH  gas profile interpolation (and renormalization) from input levels to aerosol levels  defined in RT_discret_vertical_distr
      CALL modified_gas_profile(ABS_DATA%NLV,                                  &        !IN
                                    ABS_DATA%HLV(1:ABS_DATA%NLV),              &
                                    gas_vert_prof(1:ABS_DATA%NLV),             &
                                    DISCRVD%nh,DISCRVD%h_km(1:DISCRVD%nh),     &
                                    DISCRVD%norm(DISCRVD%natm),                &         !OUT
                                    DISCRVD%val(1:DISCRVD%nh,DISCRVD%natm)     &
                                    )


    RETURN
    END SUBROUTINE RT_GAS_PROFILE



    SUBROUTINE INT_KD(ifilter,                  &
                      filters_trans,     &
                      SLout_SC,                 &
                      ABS_DATA,                 &
                      NBV_comb,                 &
                      ISUB,IW,NBINS,            &
                      SLout_int                 &
                      )

    USE sub_gas_kd,          ONLY : DATA_GAS,NWL_GAS
    USE mod_par_OS,          ONLY : NBVM
    USE mod_alloc_gas_lut,   ONLY : MAXKD, filter_transmission
    
    IMPLICIT NONE
! -------------------
! IN:
    LOGICAL,                         INTENT(IN)     ::  ifilter
    REAL, DIMENSION(NWL_GAS,2*NBVM), INTENT(IN)     ::  SLout_SC
    INTEGER,                         INTENT(IN)     ::  ISUB,IW,NBINS
    TYPE(filter_transmission),           INTENT(IN)     ::  filters_trans
! -------------------
! INOUT:
    INTEGER,                         INTENT(INOUT)  ::  NBV_comb
    TYPE(DATA_GAS),                  INTENT(INOUT)  ::  ABS_DATA
! -------------------
! OUT:
    REAL, DIMENSION(2*NBVM),         INTENT(OUT)    ::  SLout_int
! -------------------
! LOCAL:
    INTEGER                                         ::  IKD, IIKD, nlines_bin
    REAL                                            ::  average_transmittance, bin_filt, sum_T
    REAL,DIMENSION(MAXKD)                           ::  weigths

    SLout_int(1:NBV_comb) = 0.0
    bin_filt = 0.0
    weigths = 0.0
    sum_T = 0.0

    DO IKD=1,NBINS

        IF(ifilter)THEN

            average_transmittance = 0.0
            nlines_bin = 0

            DO IIKD=1,filters_trans%NWL(ISUB) !MH Average transmittance of each bin

                IF(ABS_DATA%DATA_KD_Channel(1)%MAPPING_FUNCTION(IIKD,ISUB) == IKD-1)THEN
                    average_transmittance = average_transmittance + filters_trans%transmission(IIKD,ISUB)
                    nlines_bin = nlines_bin + 1
                END IF
                
            END DO

!!!!!!!!!!!!!
!MH: Average transmittance method, gives the best results for OLCI19
            average_transmittance = average_transmittance/nlines_bin
            weigths(IKD) = average_transmittance*ABS_DATA%DATA_KD_Channel(1)%AIKD(IKD,ISUB)
!!!!!!!!!!!!!


!            average_transmittance = average_transmittance/SUM(filters_transmission%transmission(1:filters_transmission%NWL(ISUB),ISUB))
!write(*,*)average_transmittance
!            weigths(IKD) = average_transmittance
!            sum_T = sum_T +weigths(IKD)

        ELSE

            average_transmittance = 1.0
            weigths(IKD) = ABS_DATA%DATA_KD_Channel(1)%AIKD(IKD,ISUB)

        END IF
    
!        SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb) + (GAS_ABS_DATA%DATA_KD_Channel(1)%AIKD(IKD,ISUB)*SLout_SC(IKD,1:NBV_comb)*average_transmittance)
!write(*,*) sum_T, weigths(IKD)
!        SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb) + (SLout_SC(IKD,1:NBV_comb)*weigths(IKD))

!!!!!!!!!!!!!
!MH: Average transmittance method,
        SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb) + (SLout_SC(IKD,1:NBV_comb)*weigths(IKD))
!!!!!!!!!!!!!

!        SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb) + (SLout_SC(IKD,1:NBV_comb)*average_transmittance)
!write(*,*)GAS_ABS_DATA%DATA_KD_Channel(1)%AIKD(IKD,ISUB)
!write(*,*)average_transmittance
!        SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb) + (GAS_ABS_DATA%DATA_KD_Channel(1)%AIKD(IKD,ISUB)*SLout_SC(IKD,1:NBV_comb)*average_transmittance/(SUM(filters_transmission%transmission(1:filters_transmission%NWL(ISUB),ISUB))))

!        bin_filt = bin_filt + (average_transmittance)
!
!        SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb) + (SLout_SC(IKD,1:NBV_comb)*average_transmittance)

    END DO
!write(*,*)filters_transmission%transmission(1:filters_transmission%NWL(ISUB),ISUB)
!    SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb)/(SUM(filters_transmission%transmission(1:filters_transmission%NWL(ISUB),ISUB)))
!    SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb)/(bin_filt)
    SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb)/(SUM(ABS_DATA%DATA_KD_Channel(1)%AIKD(1:NBINS,ISUB)))

    RETURN
    END SUBROUTINE INT_KD



    SUBROUTINE INT_LINES(ikdist,ifilter,            &
                         SLout_SC,                  &
                         bandwidth,                 &
                         filters_trans,             &
                         ABS_DATA,                  &
                         NBV_comb,                  &
                         ISUB,                      &
                         n_gas_w,                   &
                         delta_wl,                  &
                         SLout_int                  &
                         )

    USE sub_gas_kd,          ONLY : DATA_GAS,NWL_GAS
    USE mod_alloc_gas_lut,   ONLY : filter_channel_type, filter_transmission
    USE mod_par_OS,          ONLY : NBVM
    IMPLICIT NONE
! -------------------
! IN:
    LOGICAL,                         INTENT(IN)     ::  ikdist, ifilter
    REAL, DIMENSION(NWL_GAS,2*NBVM), INTENT(IN)     ::  SLout_SC
    REAL,                            INTENT(IN)     ::  bandwidth
    type(filter_transmission),           INTENT(IN)     ::   filters_trans
    INTEGER,                         INTENT(IN)     ::  n_gas_w
    INTEGER,                         INTENT(IN)     ::  ISUB
! -------------------
! INOUT:
    INTEGER,                         INTENT(INOUT)  ::  NBV_comb
    TYPE(DATA_GAS),                  INTENT(INOUT)  ::  ABS_DATA
! -------------------
! OUT:
    REAL,                            INTENT(OUT)    ::  delta_wl
    REAL, DIMENSION(2*NBVM),         INTENT(OUT)    ::  SLout_int
    
! -------------------
! LOCAL:
    INTEGER                                         ::  IWG
    REAL                                            ::  filter_weigth, norm_filter

    !MH Necessary initialization in case of gas absorption lines present
    SLout_int(:) = 0.0
    delta_wl = 0.0
    norm_filter = 0.0

    DO IWG=1,n_gas_w !MH Loop over all gas lines for a constant aerosol value

        if(IWG .lt. (n_gas_w)) then
            
!MH Calculating like this the delta_WL enables to go trough not constant spectral gas extinction information
            if(ifilter) then
                filter_weigth = (filters_trans%transmission(IWG,ISUB) + filters_trans%transmission(IWG+1,ISUB))/2
            else
                filter_weigth = 1.0
            end if

            SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb) + ( ((SLout_SC(IWG,1:NBV_comb) + SLout_SC(IWG+1,1:NBV_comb))/2 )*(ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG+1) - ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG)) * filter_weigth)

            delta_wl = delta_wl + (ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG+1) - ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG))

            norm_filter = norm_filter + ((ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG+1) - ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG)) * filter_weigth)

        else

            if(ifilter) then
                filter_weigth = (filters_trans%transmission(IWG-1,ISUB) + filters_trans%transmission(IWG,ISUB))/2
            else
                filter_weigth = 1.0
            end if
            
            !MH Asume that the last spectral interval is the same as the previous one
            SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb) + (SLout_SC(IWG,1:NBV_comb)*(ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG) - ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG-1)) * filter_weigth)

!MH This needs to be use instead of bandwidth beacuse when trapezoids are used to integrate the filter
!MH the length changes slightly, so withs we minimize this loss.
            delta_wl = delta_wl + (ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG) - ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG-1))
            norm_filter = norm_filter + ((ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG) - ABS_DATA%ABS_GS_WL(ISUB)%WVL(IWG-1))* filter_weigth)
            
        end if
        

    END DO !n_gas_w

!MH Normalization to the full channel width
    SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb)/delta_wl
    ! SLout_int(1:NBV_comb) = SLout_int(1:NBV_comb)/norm_filter


    RETURN
    END SUBROUTINE INT_LINES


SUBROUTINE INT_SC(SLout_int,                   &
                  bandwidth,                   &
                  NBV_comb,                    &
                  NSubCH,                      &
                  SLout_comb                   &
                  )

        USE sub_gas_kd,          ONLY : DATA_GAS,NWL_GAS
        USE mod_par_OS,          ONLY : NBVM,N_SUB_CHANNEL_MAX
        IMPLICIT NONE
! -------------------
! IN:
        REAL, DIMENSION(N_SUB_CHANNEL_MAX,2*NBVM),  INTENT(IN)   ::  SLout_int
        REAL, DIMENSION(N_SUB_CHANNEL_MAX),         INTENT(IN)   ::  bandwidth
        INTEGER,                                    INTENT(IN)   ::  NBV_comb
        INTEGER,                                    INTENT(IN)   ::  NSubCH
! -------------------
! OUT:
        REAL, DIMENSION(2*NBVM),                    INTENT(OUT)  ::  SLout_comb
! -------------------
! LOCAL:
        INTEGER         :: ISUB


        SLout_comb(:) = 0.0

        DO ISUB=1, NSubCH

            SLout_comb(1:NBV_comb) = SLout_comb(1:NBV_comb) + (SLout_int(ISUB,1:NBV_comb)*bandwidth(ISUB))
            
        END DO

        SLout_comb(1:NBV_comb) = SLout_comb(1:NBV_comb)/SUM(bandwidth(1:NSubCH))

    RETURN
    END SUBROUTINE INT_SC


!------------------------------------------------------------------------

!MH   Based in LL functions of mixture_ref_index_chem.f90

    subroutine interpolate_linear_fit_dp (xdum, num_knowns, i_high, xknown, yknown, ydum)

    implicit none
    ! Global variables
    ! ... Input
    integer,   intent(IN) ::  num_knowns
    real,    intent(IN) ::              xdum
    integer               i_high
    real , dimension(num_knowns), intent(IN) :: xknown
    real , dimension(num_knowns), intent(IN) :: yknown

    real ,                      intent(OUT)   :: ydum

    ! Local variables
    real                  slope


    slope = (yknown(i_high) - yknown(i_high-1)) / &
      (xknown(i_high) - xknown(i_high-1))


    ydum = yknown(i_high) + slope*(xdum - xknown(i_high))

    end subroutine interpolate_linear_fit_dp


subroutine extrapolate_linear_fit_dp &
     (xdum, num_knowns, i_high, xknown, yknown, ydum)


  implicit none
  ! Global variables
  ! ... Input
  real,    intent(IN) ::              xdum
  integer                        num_knowns
  integer                        i_high
  real , dimension(num_knowns), intent(IN) :: xknown
  real , dimension(num_knowns), intent(IN) :: yknown
  ! .... Output
  real,                         intent(OUT) :: ydum
  
  ! Local variables:
  real                                      :: slope
  
  
  if (i_high == 1) then
     slope = (yknown(2) - yknown(1)) / &
             (xknown(2) - xknown(1))
     ydum  = yknown(1)  - slope*(xknown(1) - xdum)
  else if (i_high == num_knowns+1) then
     slope = (yknown(num_knowns) - yknown(num_knowns-1)) / &
             (xknown(num_knowns) - xknown(num_knowns-1))
     ydum  = yknown(num_knowns)  + slope*(xdum - xknown(num_knowns))
  else
     write(6,*) "FATAL ERROR: subroutine linear_fit.f90:"
     write(6,*) "i_high /= 1 or num_knowns+1 in subroutine extrapolate"
     stop
  endif

end subroutine extrapolate_linear_fit_dp

!------------------------------------------------------------------------


    subroutine find_closest_value(a,size_a,to_find,min_diff,idx)

    integer,                   intent(IN)     ::   size_a
    real,dimension(size_a),    intent(IN)     ::   a
    real,                      intent(IN)     ::   to_find
    real,                      intent(INOUT)  ::   min_diff
    integer,                   intent(OUT)    ::   idx


    real                 ::   diff
    integer              ::   i

    idx = 0

    do i = 1, size_a
      diff = abs(a(i)-to_find)
      if ( diff < min_diff) then
        idx = i
        min_diff = diff
      end if
    end do

    end subroutine find_closest_value




      END MODULE mod_bbgas_kd
