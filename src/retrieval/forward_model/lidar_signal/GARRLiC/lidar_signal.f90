! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **
        !> @file lidar_signal.f90
        !> File contains soubroutines to simulate lidar measurements
        !>
! contains soubroutines
! lidar_signal_elastic
! lidar_signal_raman
! lidar_signal_parallel
! lidar_signal_perpendicular
! vertical_backscatter
! vertical_extinction
! profile_normalization
! lidar_signal_elastic_VLDP

#include "../../../constants_set/mod_globals.inc"
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      SUBROUTINE lidar_signal_elastic (           &
                                        HOBS_km,  & !> @param[in]  HOBS_km is altitude of observation above sea level in km
                                        HGR_km,   & !> @param[in]  HGR_km is ground elevation above sea level in km
                                        NVERT,    & !> @param[in]  NVERT - number of heights for vertical aerosol profile
                                        HVP_km,   & !> @param[in]  HVP_km(NVERT) – altitudes of layers of vertical profiles in descending order
                                        NSD,      & !> @param[in]  NSD - number of aerosol fractions
                                        AVP_norm, & !> @param[in]  APV_norm(NSD,NVERT) - Aerosol Vertival Profile normalized (dimentionless)
                                        EXTA,     & !> @param[in]  EXTA(NSD) aerosol extinction in 1/km
                                        LRA,      & !> @param[in]  LRA - Lidar Ratio of Aerosol in Sr
                                        MVP_norm, & !> @param[in]  MVP_norm molecular vertical profile normalized (dimentionless)
                                        EXTM,     & !> @param[in]  EXTM - molecular extinction in 1/km
                                        LS        & !> @param[out] LS(NVERT) - ouput vector containing lidar measurement calculaions
                                      )

!> @brief Forward lidar problem for Single pixel and Single wavelength
!> @brief with Molecular correction (extinction and backscatter)
!> @brief
!>
      USE mod_molecular_scattering, only : LRM
      USE mod_par_inv, only  : KVERTM
      USE mod_par_os,  only  : KSD, HMAX_atm
      USE mod_stop_report

      IMPLICIT NONE

      INTEGER,                    INTENT(IN)     :: NSD, NVERT ! number of aerosol modes, number of AVP_norm
      REAL, DIMENSION(KVERTM,KSD),INTENT(IN)     :: AVP_norm   ! normalized Aerosol extinction Vertical Profile (AVP)
      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: HVP_km     ! Vertical Profile Heights 
      REAL, DIMENSION(KSD),       INTENT(IN)     :: EXTA       ! aerosol extinction
      REAL, DIMENSION(KSD),       INTENT(IN)     :: LRA        ! aerosol lidar ratio
      REAL,                       INTENT(IN)     :: HOBS_km    ! oservation altitude above sea level
      REAL,                       INTENT(IN)     :: HGR_km     ! ground altitude above sea level
      REAL,                       INTENT(IN)     :: EXTM       ! aerosol extinction (scattering)
      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: MVP_norm   ! normalized Molecular extinction Vertical Profile (MVP)
      REAL, DIMENSION(KVERTM),    INTENT(OUT)    :: LS         ! lidar signal
!-- internal variables ------------------------------------------- 
      INTEGER                     :: ISD,IVERT  ! indexes
      REAL, DIMENSION(KSD)        :: AVP_INT    ! AVP_int integrated aerosol profile  
      REAL                        :: LS_INT     ! accumulator for lidar signal norm
      REAL                        :: MVP_INT    ! integrated molecular profile
      REAL                        :: BA         ! aerosol backscatter
      REAL                        :: EA         ! aerosol extinction
      REAL                        :: BM         ! molecular backscatter
      REAL                        :: EM         ! molecular extinction
      REAL                        :: DH         ! height step for profile interation
!      REAL                        :: COSZA      ! cosine of sounding zenith angle

!------------------------------------------------------------------------------------------
! [EXTA] = 1/km
! [LRA]  = Sr
! [EXTM] = 1/km
! [LRM]  = Sr
!
! [APV_norm]  = 
! [MPV_norm]  = 
! WRITE(*,*),'NVERT=',NVERT,'KVERTM=',KVERTM
!      IF(HOBS_km .GT. HVP_km(1)) THEN
!        WRITE(tmp_message,'(2(a,es11.4),a)') &
!        'HOBS_km =',HOBS_km ,' .GT. HVP_km(1) =', HVP_km(1),'  - invalid measurements height'
!        G_ERROR(trim(tmp_message))
!      ENDIF

!WRITE(*,*) 'NVERT=', NVERT
!WRITE(*,*) 'EXTA=', EXTA
!WRITE(*,*) 'HOBS_km=', HOBS_km
!WRITE(*,*) 'HMAX_atm=',HMAX_atm
!WRITE(*,*) 'HVP_km(NVERT)=', HVP_km(NVERT)
!stop 'in lidar_signal_elastic'
      LS(:)      = 0.0      
      AVP_INT(:) = 0.0
      MVP_INT    = 0.0
      LS_INT     = 0.0
!     COSZA=cos(42.4*3.1416/180.0)
!WRITE(*,*),'NVERT=',NVERT
! UPward looking lidar: observation height is 0.5m close to the ground height
     IF (ABS(HOBS_km-HGR_km) .LE. 0.0005) THEN
       DO IVERT=NVERT-1,1,-1
          DH = HVP_km(IVERT)-HVP_km(IVERT+1)
          AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_norm(IVERT,1:NSD)+AVP_norm(IVERT+1,1:NSD))*DH
          MVP_INT        = MVP_INT + 0.5*(MVP_norm(IVERT)+MVP_norm(IVERT+1))*DH
          BA = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD)/LRA(1:NSD))
!AL           BA = SUM(AVP_norm(IVERT,1:NSD)/LRA(1:NSD)) ! for testing purposes only
!          BA = 0.332739*AVP_norm(IVERT,1)/69.5112
          EA = SUM(EXTA(1:NSD)*AVP_INT(1:NSD))
!AL           EA = SUM(AVP_INT(1:NSD)) ! for testing purposes only
!          EA = 0.332739*AVP_INT(1)
!          write(*,*) 'ivert, EXTA', IVERT,0.332739*AVP_norm(IVERT,1)
          BM = EXTM*MVP_norm(IVERT)/LRM
          EM = EXTM*MVP_INT
!          LS(IVERT) = (BA+BM)*EXP(-2.*(EA+EM)/COSZA)
          LS(IVERT) = (BA+BM)*EXP(-2.*(EA+EM))

        ENDDO ! IVERT
      ! Lidar Signal normalization
      DO IVERT=NVERT-2,1,-1
         DH = HVP_km(IVERT)-HVP_km(IVERT+1)
         LS_INT = LS_INT + 0.5*(LS(IVERT)+LS(IVERT+1))*DH
      ENDDO ! IVERT
      LS(1:NVERT-1) = LS(1:NVERT-1)/LS_INT !NVERT-1 because there is added one ground point that is extra


!DOWNward looking lidar: observation height is above maximum height of the profile
     ELSEIF (HOBS_km .GE. HVP_km(1)) THEN

       DO IVERT=1,NVERT-1
          IF(IVERT .EQ. 1) THEN
             DH = HMAX_atm*0.001 - HVP_km(IVERT) ! HAMX_atm is given in meters HVP_km in kilometers
             AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_norm(IVERT,1:NSD)+0.0)*DH ! assuming that profile at HMAX_atm is zero
             MVP_INT        = MVP_INT + 0.5*(MVP_norm(IVERT)+0.0)*DH ! assuming that profile at HMAX_atm is zero
          ELSE
             DH = HVP_km(IVERT-1)-HVP_km(IVERT)
             AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_norm(IVERT-1,1:NSD)+AVP_norm(IVERT,1:NSD))*DH
             MVP_INT        = MVP_INT + 0.5*(MVP_norm(IVERT-1)+MVP_norm(IVERT))*DH
          ENDIF

          BA = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD)/LRA(1:NSD))
          EA = SUM(EXTA(1:NSD)*AVP_INT(1:NSD))
          BM = EXTM*MVP_norm(IVERT)/LRM
          EM = EXTM*MVP_INT
          LS(IVERT) = (BA+BM)*EXP(-2.*(EA+EM))
          IF (LS(IVERT).EQ.0) LS(IVERT)=1.0e-10 ! hack to overcome saturation of exp during derivative calculation
       ENDDO !IVERT

! Lidar Signal normalization
!        DH = HMAX_atm*0.001 - HVP_km(1)
!        LS_INT =0.5*LS(1)


        DO IVERT=2,NVERT-1
           DH = HVP_km(IVERT-1)-HVP_km(IVERT)
           LS_INT = LS_INT + 0.5*(LS(IVERT-1)+LS(IVERT))*DH
        ENDDO ! IVERT
!al        LS(2:NVERT) = LS(2:NVERT)/LS_INT 
!checking nvert point wich makes sense only for downward looking lidar later check everything else
        LS(1:NVERT-1) = LS(1:NVERT-1)/LS_INT !NVERT-1 because there is an added ground point that is extra (true only for downward looking lidars)
     ELSE
        write(tmp_message,'(a)') 'lidar_elastic: invalid HOBS and HGR, do not correspond to ground or space'
        G_ERROR(trim(tmp_message))
     ENDIF ! up or down looking
     DO IVERT=1,NVERT
        IF(LS(IVERT).EQ.0) LS(IVERT)=1.0e-10 ! hack to overcome saturation of exp during derivative calculation
     ENDDO
      IF(ANY(LS(1:NVERT-1) .LE. 0. .OR. IsNaN(LS(1:NVERT-1)))) THEN
        GOTO 33
        WRITE(*,*) 'ERROR IN lidar_signal_elastic:'
        WRITE(*,*) 'INVALID OUTPUT'
        WRITE(*,*) 'DEBUG INFORMATION:'
        WRITE(*,*) 'LS:'
        WRITE(*,'(10e14.5)') LS(1:NVERT-1)
        WRITE(*,*) 'AVP_INT: ', AVP_INT(1:NSD)
        WRITE(*,*) 'MVP_INT: ', MVP_INT
        WRITE(*,*) 'EA=',EA,'  BA=',BA
        WRITE(*,*) 'EM=',EM,'  BM=',BM
        WRITE(*,*) 'LS_INT=',LS_INT
        WRITE(*,*) 'SUBROUTINE INPUTS:'
        WRITE(*,*) 'LRA=',LRA,' LRM=',LRM
        DO ISD=1,NSD
          WRITE(*,*) 'AVP_norm: ISD=',ISD
          WRITE(*,'(10e14.5)') AVP_norm(1:NVERT,ISD)
        ENDDO ! ISD
        WRITE(*,*) 'HVP in km:'
        WRITE(*,'(10f14.5)') HVP_km(1:NVERT)
        WRITE(*,*) 'MVP_norm:'
        WRITE(*,'(10e14.5)') MVP_norm(1:NVERT)
        WRITE(*,*) 'NVERT=', NVERT
        WRITE(*,*) 'EXTA=',SUM(EXTA(1:NSD)),'  EXTA(1:NSD)=', EXTA(1:NSD)
        WRITE(*,*) 'HOBS_km=', HOBS_km
        WRITE(*,*) 'HVP_km AVP_norm(1:NSD), MVP_norm : '
        DO IVERT=1,NVERT
          WRITE(*,*) HVP_km(IVERT),AVP_norm(IVERT,1:NSD), MVP_norm(IVERT)
        ENDDO
        write(*,*) 'EXTM=',EXTM
33      CONTINUE
        write(tmp_message,'(a)') 'invalid output, negative or NaN in lidar_elastic'
        G_ERROR(trim(tmp_message))
      ENDIF ! ANY(LS(1:NVERT) .LT. 0. .OR. ....
!WRITE(*,*),'EXTA=',EXTA
!WRITE(*,*),HVP_km
!WRITE(*,*),AVP_norm              
      RETURN
      END SUBROUTINE lidar_signal_elastic
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      SUBROUTINE vertical_backscatter (           &
                                        NVERT,    &!> @param[in]  NVERT - number of heights for vertical aerosol profile
                                        HVP_km,   &!> @param[in]  HVP_km(NVERT) – altitudes of layers of vertical profiles in descending order
                                        NSD,      &!> @param[in]  NSD - number of aerosol fractions
                                        AVP_norm, &!> @param[in]  APV_norm(NSD,NVERT) - Aerosol Vertival Profile normalized (1/km)
                                        EXTA,     &!> @param[in]  EXTA(NSD) aerosol extinction (dimentionless)
                                        LRA,      &!> @param[in]  LRA - Lidar Ratio of Aerosol in Sr
                                        VBS       &!> @param[out] VBS - Vertical profile of aerosol backscatter in 1/(km*Sr)
                                       )
!> @brief vertical aerosol backscatter profile
!> @brief for Single pixel and Single wavelength
!> @brief
!>
      USE mod_par_inv, only  : KVERTM
      USE mod_par_os,  only  : KSD
      USE mod_stop_report

      IMPLICIT NONE

      INTEGER,                    INTENT(IN)     :: NSD, NVERT ! number of aerosol modes, number of AVP_norm
      REAL, DIMENSION(KVERTM,KSD),INTENT(IN)     :: AVP_norm   ! normalized Aerosol extinction Vertical Profile (AVP)
      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: HVP_km     ! Vertical Profile Heights
      REAL, DIMENSION(KSD),       INTENT(IN)     :: EXTA, LRA  ! aerosol extinction and lidar ratio
      REAL, DIMENSION(KVERTM),    INTENT(OUT)    :: VBS        ! V_ertical B_ackS_catter profile
!-- internal variables -------------------------------------------
      INTEGER                                    :: IVERT,ISD  ! indicies
!------------------------------------------------------------------------------------------

      VBS(:)      = 0.0

      do IVERT=1,NVERT-1 ! NVERT-1 because there was ground level point added

         VBS(IVERT) = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD)/LRA(1:NSD))

      enddo ! IVERT

     IF(ANY(VBS(1:NVERT-1) .LE. 0. .OR. IsNaN(VBS(1:NVERT-1)))) THEN
        GOTO 33
        WRITE(*,*) 'ERROR IN vertical_backscatter:'
        WRITE(*,*) 'INVALID OUTPUT'
        WRITE(*,*) 'DEBUG INFORMATION:'
        WRITE(*,*) 'VBS:'
        WRITE(*,'(10e14.5)') VBS(1:NVERT-1)
        WRITE(*,*) 'SUBROUTINE INPUTS:'
        WRITE(*,*) 'LRA=',LRA
        DO ISD=1,NSD
           WRITE(*,*) 'AVP_norm: ISD=',ISD
           WRITE(*,'(10e14.5)') AVP_norm(1:NVERT,ISD)
        ENDDO ! ISD
        WRITE(*,*) 'HVP in km:'
        WRITE(*,'(10f14.5)') HVP_km(1:NVERT)
        WRITE(*,*) 'NVERT=', NVERT
        WRITE(*,*) 'EXTA=',SUM(EXTA(1:NSD)),'  EXTA(1:NSD)=', EXTA(1:NSD)

33      CONTINUE
        write(tmp_message,'(a)') 'invalid output, negative or NaN in vertical_backscatter'
        G_ERROR(trim(tmp_message))
     ENDIF ! ANY(LS(1:NVERT) .LT. 0. .OR. ....

     RETURN
     END SUBROUTINE vertical_backscatter

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      SUBROUTINE lidar_signal_raman (           &
                                      HOBS_km,  &!> @param[in] HOBS_km altitude above sea level of observation [km]
                                      HGR_km,   & !> @param[in]  HGR_km is ground elevation above sea level in km
                                      NVERT,    &!> @param[in] NVERT - number of heights for vertical aerosol profile
                                      HVP_km,   &!> @param[in] HVP_km(NVERT) is vector of heights of layers of vertical profiles, descending order of height [km]
                                      NSD,      &!> @param[in] NSD - number of aerosol fractions
                                      AVP_norm, &!> @param[in] APV_norm(NSD,NVERT) - vector of optimized parameters, normalized vertical distribution (Aerosol Vertival Profile)
                                      EXTA_I,   &!> @param[in] EXTA_I(NSD) aerosol extinction on the wavelength of initial pulse [1\km]
                                      EXTA_R,   &!> @param[in] EXTA_R(NSD) aerosol extinction on the wavelength of Raman shift   [1\km]
                                      MVP_norm, &!> @param[in] MVP_norm(NVERT) — Molecular Vertical Profile normalized [dimensionless]
                                      EXTM_I,   &!> @param[in] EXTM_I – molecular extinction at initial wavelength [1\km]
                                      EXTM_R,   &!> @param[in] EXTM_R – molecular extinction at Raman wavelength [1\km]
                                      LS        &!> @param[out] LS(NVERT) - ouput vector containing lidar measurement calculaions
                                    )

!> @brief Soubroutine calculates forward lidar problem
!> @brief for Single pixel and a pair of RAMAN wavelengths
!> @brief with Molecular correction (extinction and backscatter)
!>
!>
      USE mod_molecular_scattering, only : LRM
      USE mod_par_inv, only  : KVERTM
      USE mod_par_os,  only  : KSD, HMAX_atm
      USE mod_stop_report

      IMPLICIT NONE

      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: HVP_km   ! vector of heights corresponds to AVP
      REAL, DIMENSION(KVERTM,KSD),INTENT(IN)     :: AVP_norm  ! normalized aerosol vertical distribution
      REAL, DIMENSION(KSD),       INTENT(IN)     :: EXTA_I,EXTA_R ! aerosol extinction for all modes for initial and raman wl in 1/km
      INTEGER,                    INTENT(IN)     :: NSD, NVERT !number of modes, number of vert strobes
      REAL,                       INTENT(IN)     :: HOBS_km ! altitude above seal level of the observation
      REAL,                       INTENT(IN)     :: HGR_km     ! ground altitude above sea level
      REAL,                       INTENT(IN)     :: EXTM_I,EXTM_R ! molecular extinction for all modes for initial and raman wl in 1/km
      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: MVP_norm  !normalized molecular profile
      REAL, DIMENSION(KVERTM),    INTENT(OUT)    :: LS   !lidar signal
!-- internal variables ------------------------------------------- 
      INTEGER                     :: ISD,IVERT      ! indexes
      REAL, DIMENSION(KSD)        :: AVP_INT        ! AVP_int integrated aerosol profile
      REAL                        :: EA_I,EA_R      ! aerosol extinction at Initial nad Raman shifted wl
      REAL                        :: BM_R,EM_I,EM_R ! molecular backscatter and extinction at Initial nad Raman shifted wl
      REAL                        :: LS_INT         ! accumulator for lidar signal norm
      REAL                        :: MVP_INT        ! MVP_int integrated molecular backscatter
      REAL                        :: DH             ! height step for profile interation
!      REAL                        :: COSZA          ! cosine of sounding zenith angle

!      REAL, DIMENSION(KVERTM)     :: DH_temp    ! reversed vector of heights corresponds to AVP
!      REAL, DIMENSION(KVERTM,KSD) :: AVP_temp   ! reversed aerosol vertical distribution
!------------------------------------------------------------------------------------------ 
! [EXTA] = 1
! [LRA]  =  
!
! [BM0]  = 1/m
! [EA]   = 1/m 
! [BA]   = 1/m
! [EM]   = 1/m
!
! [FPV]  = 1/m

! [DH]   = km
 
!      IF(HOBS_km .GT. HVP_km(1)) THEN
!        WRITE(*,*) 'HOBS_km=',HOBS_km ,' .GT. HVP_km(1)=', HVP_km(1)
!        STOP 'ERROR IN lidar_signal_raman: invalid measurements height'
!      ENDIF

!WRITE(*,*) 'NVERT=', NVERT
!WRITE(*,*) 'EXTA=', EXTA
!WRITE(*,*) 'HOBS_km=', HOBS_km

! units conversion to meters

!      DH_temp(1:NVERT)        = DH(1:NVERT)*1000.        ! [DH_temp]=m
!      AVP_temp(1:NVERT,1:NSD) = AVP(1:NVERT,1:NSD)*0.001 ! because heigh in kilometers has been used for AVP integrating
                                                         ! to normalize Aerosol Vertical Profile      
      LS(:)      = 0.0      
      AVP_INT(:) = 0.0
      MVP_INT    = 0.0
      LS_INT     = 0.0

!      COSZA=cos(43.5*3.1416/180.0)
     IF (ABS(HOBS_km-HGR_km) .LE. 0.0005) THEN
        DO IVERT=NVERT-1,1,-1
          DH = HVP_km(IVERT)-HVP_km(IVERT+1)
          AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_norm(IVERT,1:NSD)+AVP_norm(IVERT+1,1:NSD))*DH
          MVP_INT        = MVP_INT + 0.5*(MVP_norm(IVERT)+MVP_norm(IVERT+1))*DH
          EA_I = SUM(EXTA_I(1:NSD)*AVP_INT(1:NSD))
          EA_R = SUM(EXTA_R(1:NSD)*AVP_INT(1:NSD)) 
          BM_R = EXTM_R*MVP_norm(IVERT)/LRM
          EM_I = EXTM_I*MVP_INT
          EM_R = EXTM_R*MVP_INT
          LS(IVERT) = BM_R*EXP(-(EA_I+EA_R+EM_I+EM_R))

        ENDDO ! IVERT

! Lidar Signal normalization
        DO IVERT=NVERT-2,1,-1
           DH = HVP_km(IVERT)-HVP_km(IVERT+1)
           LS_INT = LS_INT + 0.5*(LS(IVERT)+LS(IVERT+1))*DH
        ENDDO ! IVERT
        LS(1:NVERT-1) = LS(1:NVERT-1)/LS_INT !NVERT-1 because there is added one ground point that is extra

    ELSEIF (HOBS_km .GE. HVP_km(1)) THEN

        DO IVERT=1,NVERT-1
           IF(IVERT .EQ. 1) THEN
             DH = HMAX_atm*0.001 - HVP_km(IVERT) ! HAMX_atm is given in meters HVP_km in kilometers
             AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_norm(IVERT,1:NSD)+0)*DH ! assuming that profile at HMAX_atm is zero
             MVP_INT        = MVP_INT + 0.5*(MVP_norm(IVERT)+0)*DH ! assuming that profile at HMAX_atm is zero
          ELSE
             DH = HVP_km(IVERT-1)-HVP_km(IVERT)
             AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_norm(IVERT-1,1:NSD)+AVP_norm(IVERT,1:NSD))*DH
             MVP_INT        = MVP_INT + 0.5*(MVP_norm(IVERT-1)+MVP_norm(IVERT))*DH
          ENDIF

          EA_I = SUM(EXTA_I(1:NSD)*AVP_INT(1:NSD))
          EA_R = SUM(EXTA_R(1:NSD)*AVP_INT(1:NSD))
          BM_R = EXTM_R*MVP_norm(IVERT)/LRM
          EM_I = EXTM_I*MVP_INT
          EM_R = EXTM_R*MVP_INT
          LS(IVERT) = BM_R*EXP(-(EA_I+EA_R+EM_I+EM_R))
        ENDDO !IVERT

! Lidar Signal normalization
!        DH = HMAX_atm*0.001 - HVP_km(1)
!        LS_INT =0.5*LS(1)
        DO IVERT=2,NVERT-1
           DH = HVP_km(IVERT-1)-HVP_km(IVERT)
           LS_INT = LS_INT + 0.5*(LS(IVERT-1)+LS(IVERT))*DH
        ENDDO ! IVERT
        LS(1:NVERT-1) = LS(1:NVERT-1)/LS_INT !NVERT-1 because there is an added ground point that is extra (true only for downward looking lidars)
!checking nvert point wich makes sense only for downward looking lidar later check everything else
        ELSE
            write(tmp_message,'(a)') 'lidar_signal_raman: invalid HOBS and HGR, do not correspond to ground or space'
            G_ERROR(trim(tmp_message))
!        IF(LS(NVERT) .LE. 0. .OR. IsNaN(LS(NVERT))) THEN
!           write(tmp_message,'(a)') 'invalid output, negative or NaN in lidar_signal_raman: downward'
!           G_ERROR(trim(tmp_message))
!        ENDIF ! non positive or nan

      ENDIF ! UPward or DOWNward looking
      DO IVERT=1,NVERT
         IF(LS(IVERT).EQ.0) LS(IVERT)=1.0e-10 ! hack to overcome saturation of exp during derivative calculation
      ENDDO
      IF(ANY(LS(1:NVERT-1) .LE. 0. .OR. IsNaN(LS(1:NVERT-1)))) THEN
        GOTO 33
        WRITE(*,*) 'ERROR IN lidar_signal_raman:'
        WRITE(*,*) 'INVALID OUTPUT'
        WRITE(*,*) 'DEBUG INFORMATION:'
        WRITE(*,*) 'FPV:'
        WRITE(*,'(10e14.5)') LS(1:NVERT-1)
        WRITE(*,*) 'AVP_INT: ', AVP_INT(1:NSD)
        WRITE(*,*) 'MVP_INT: ', MVP_INT
        WRITE(*,*) 'EA_I=',EA_I,' EA_R=',EA_R
        WRITE(*,*) 'EM_I=',EM_I,' EM_R=',EM_R
        WRITE(*,*) 'BM_R=',BM_R
        WRITE(*,*) 'LS_INT=',LS_INT 
        WRITE(*,*) 'SUBROUTINE INPUTS:'
        WRITE(*,*) 'LRM=',LRM
        DO ISD=1,NSD
          WRITE(*,*) 'AVP_norm: ISD=',ISD
          WRITE(*,'(10e14.5)') AVP_norm(1:NVERT,ISD)
        ENDDO ! ISD   
        WRITE(*,*) 'HVP_km:'
        WRITE(*,'(10f14.5)') HVP_km(1:NVERT)
        WRITE(*,*) 'BM0:'
        WRITE(*,'(10e14.5)') MVP_norm(1:NVERT)
        WRITE(*,*) 'NVERT=', NVERT
        WRITE(*,*) 'EXTM_I=', EXTM_I
        WRITE(*,*) 'EXTM_R=', EXTM_R
        WRITE(*,*) 'EXTA_I=',SUM(EXTA_I(1:NSD)),'  EXTA_I(1:NSD)=', EXTA_I(1:NSD)
        WRITE(*,*) 'EXTA_R=',SUM(EXTA_R(1:NSD)),'  EXTA_R(1:NSD)=', EXTA_R(1:NSD)
        WRITE(*,*) 'HOBS_km=', HOBS_km
        !WRITE(*,*) 'DH AVP(1) AVP(2)'
        !DO IVERT=1,NVERT
          !WRITE(*,*) DH(IVERT),AVP(IVERT,1:NSD)
        !ENDDO
    !IF(ANY(LS(1:NVERT-1) .LT. 0. .OR. IsNaN(LS(1:NVERT-1)) .OR. LS(1:NVERT-1) .GT. 1.)) &
33      CONTINUE
        write(tmp_message,'(a)') 'invalid output, negative or NaN in raman'
        G_ERROR(trim(tmp_message))
      ENDIF ! ANY(LS(1:NVERT) .LT. 0. .OR. ....
                        
      RETURN
      END SUBROUTINE lidar_signal_raman

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

SUBROUTINE lidar_signal_elastic_parallel (           &
                                           HOBS_km,  &!> @param[in] HOBS_km is observation height above sea level
                                           HGR_km,   & !> @param[in]  HGR_km is ground elevation above sea level in km
                                           NVERT,    &!> @param[in] NVERT - number of heights for vertical aerosol profile
                                           HVP_km,   &!> @param[in] HVP_km(NVERT) is vector of heights of layers of vertical profiles, descending order of height in km
                                           NSD,      &!> @param[in] NSD - number of aerosol fractions it is supposed
                                           AVP_norm, &!> @param[in] APV_norm(NSD,NVERT) - vector of optimized parameters, vertical distribution (Aerosol Vertival Profile)
                                           EXTA,     &!> @param[in] EXTA(NSD) aerosol extinction
                                           DR_PAR,   &!> @param[in] DR_PAR(NSD) – aerosol columnar depolarization ration for PARALLEL component
                                           MVP_norm, &!> @param[in] MVP_norm(NVERT) — Molecular Vertical Profile
                                           EXTM,     &!> @param[in] EXTM – molecular extinction
                                           LS        &!> @param[out] LS(NVERT) - ouput vector containing lidar measurement calculaions
                                         )

!> @brief Soubroutine calculates forward lidar problem for PARALLEL depolarization cahnnels
!> @brief for Single pixel and Single wavelength
!> @brief with molecular correction (extinction and backscatter)
!> @brief
!>
      USE mod_molecular_scattering, only : LRM
      USE mod_par_inv, only  : KVERTM
      USE mod_par_os,  only  : KSD
      USE mod_stop_report

      IMPLICIT NONE

      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: HVP_km   ! vector of heights corresponds to AVP in km
      REAL, DIMENSION(KVERTM,KSD),INTENT(IN)     :: AVP_norm  ! normalized aerosol vertical distribution
      REAL, DIMENSION(KSD),       INTENT(IN)     :: EXTA, DR_PAR !extinction and lidar depolarisation ratio for all modes
      INTEGER,                    INTENT(IN)     :: NSD, NVERT !number of modes, number of vert strobes
      REAL,                       INTENT(IN)     :: HOBS_km ! altitude above seal level of the observation
      REAL,                       INTENT(IN)     :: HGR_km     ! ground altitude above sea level
      REAL,                       INTENT(IN)     :: EXTM ! molecular extinction in 1\km
      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: MVP_norm  !molecular profile
      REAL, DIMENSION(KVERTM),    INTENT(OUT)    :: LS   !lidar signal
!-- internal variables ------------------------------------------- 
      INTEGER                     :: ISD,IVERT  ! indexes
      REAL, DIMENSION(KSD)        :: AVP_INT    ! AVP_int integrated aerosol profile  
      REAL                        :: EA,BA      ! aerosol backscatter and extinction 
      REAL                        :: EM,BM      ! molecular backscatter and extinction
      REAL                        :: LS_INT     ! accumulator for lidar signal normalization
      REAL                        :: MVP_INT    ! MVP_int integrated molecular backscatter
      REAL                        :: CHI        ! constant that defines depolarization of Rayleigh backscatter
      REAL                        :: DH         ! height step for profile interation
      REAL, DIMENSION(KVERTM,KSD) :: AVP_temp   ! reversed aerosol vertical distribution
!------------------------------------------------------------------------------------------ 
! [EXTA] = 1
! [LRA]  =  
!
! [BM0]  = 1/m
! [EA]   = 1/m 
! [BA]   = 1/m
! [EM]   = 1/m
!
! [FPV]  = 1/m

! [DH]   = km

!AL define CHI constant

      CHI=1/(1+0.014)
 
      IF(HOBS_km .GT. HVP_km(1)) THEN
        WRITE(*,*) 'HOBS_km=',HOBS_km ,' .GT. DH(1)=', HVP_km(1)
        STOP 'ERROR IN lidar_signal_elastic_parallel: invalid measurements height'
      ENDIF

!WRITE(*,*) 'NVERT=', NVERT
!WRITE(*,*) 'EXTA=', EXTA
!WRITE(*,*) 'HOBS_km=', HOBS_km


      LS(:)      = 0.0      
      AVP_INT(:) = 0.0
      MVP_INT    = 0.0
      LS_INT     = 0.0

!      select case(IFMP)
!      case(1)
!        do IVERT=NVERT-1,1,-1
!          AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_temp(IVERT,1:NSD)+AVP_temp(IVERT+1,1:NSD))*(DH_temp(IVERT)-DH_temp(IVERT+1))
!          MVP_INT        = MVP_INT + 0.5*(MVP(IVERT)+MVP(IVERT+1))*(DH_temp(IVERT)-DH_temp(IVERT+1))
!          BA = SUM(EXTA(1:NSD)*AVP_temp(IVERT,1:NSD)*DR_PAR(1:NSD))
!          EA = SUM(EXTA(1:NSD)*AVP_INT(1:NSD))
!          BM = MVP(IVERT)
!          EM = MVP_INT*LRM
!          LS(IVERT) = (BA+CHI*BM)*EXP(-2.*(EA+EM))
!        enddo ! IVERT
!      case(0)
        do IVERT=NVERT-1,1,-1
          DH = HVP_km(IVERT)-HVP_km(IVERT+1)
          AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_norm(IVERT,1:NSD)+AVP_norm(IVERT+1,1:NSD))*DH
          MVP_INT        = MVP_INT + 0.5*(MVP_norm(IVERT)+MVP_norm(IVERT+1))*DH
          BA = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD)/DR_PAR(1:NSD))
          EA = SUM(EXTA(1:NSD)*AVP_INT(1:NSD)) 
          BM = EXTM*MVP_norm(IVERT)/LRM
          EM = EXTM*MVP_INT
          LS(IVERT) = (BA+CHI*BM)*EXP(-2.*(EA+EM))
        enddo ! IVERT

!      end select

! LRM=4*pi*EM/BM0(i)  EM=LRM*BM0(i)
!write(*,*) 'EA=',EA,'  EXTA=',SUM(EXTA(1:NSD)),'  AVP_INT(1:NSD): ',AVP_INT(1:NSD)
!write(*,*) 'EM=',EM,'  EXTM=',EM,              '  MVP_INT       : ',MVP_INT

! Lidar Signal normalization
      DO IVERT=NVERT-2,1,-1
        DH = HVP_km(IVERT)-HVP_km(IVERT+1)
        LS_INT = LS_INT + 0.5*(LS(IVERT)+LS(IVERT+1))*DH
      ENDDO ! IVERT
      LS(1:NVERT-1) = LS(1:NVERT-1)/LS_INT !NVERT-1 because there is added one ground point that is extra

      IF(ANY(LS(1:NVERT-1) .LE. 0. .OR. IsNaN(LS(1:NVERT-1)))) THEN
        WRITE(*,*) 'ERROR IN lidar_signal_elastic_parallel:'
        WRITE(*,*) 'INVALID OUTPUT'
        WRITE(*,*) 'DEBUG INFORMATION:'
        WRITE(*,*) 'LS:'
        WRITE(*,'(10e14.5)') LS(1:NVERT-1)
        WRITE(*,*) 'AVP_INT: ', AVP_INT(1:NSD)
        WRITE(*,*) 'MVP_INT: ', MVP_INT
        WRITE(*,*) 'EA=',EA,'  BA=',BA
        WRITE(*,*) 'EM=',EM,'  BM=',BM  
        WRITE(*,*) 'LS_INT=',LS_INT 
        WRITE(*,*) 'SUBROUTINE INPUTS:'
        WRITE(*,*) 'DR_PAR=',DR_PAR,' LRM=',LRM
        DO ISD=1,NSD
          WRITE(*,*) 'AVP: ISD=',ISD
          WRITE(*,'(10e14.5)') AVP_norm(1:NVERT,ISD)
        ENDDO ! ISD   
        WRITE(*,*) 'HVP_km:'
        WRITE(*,'(10f14.5)') HVP_km(1:NVERT)
        WRITE(*,*) 'BM0:'
        WRITE(*,'(10e14.5)') MVP_norm(1:NVERT)
        WRITE(*,*) 'NVERT=', NVERT
        WRITE(*,*) 'EXTA=',SUM(EXTA(1:NSD)),'  EXTA(1:NSD)=', EXTA(1:NSD)
        WRITE(*,*) 'HOBS_km=', HOBS_km
        WRITE(*,*) 'EXTM=', EXTM
        !WRITE(*,*) 'DH AVP(1) AVP(2)'
        !DO IVERT=1,NVERT
          !WRITE(*,*) DH(IVERT),AVP(IVERT,1:NSD)
        !ENDDO
    !IF(ANY(LS(1:NVERT-1) .LT. 0. .OR. IsNaN(LS(1:NVERT-1)) .OR. LS(1:NVERT-1) .GT. 1.)) &
        STOP 'execution terminated in lidar_signal_elastic_parallel'
      ENDIF ! ANY(LS(1:NVERT) .LT. 0. .OR. ....
                        
      RETURN
      END SUBROUTINE lidar_signal_elastic_parallel

SUBROUTINE lidar_signal_elastic_perpendicular (           &
                                                HOBS_km,  &!> @param[in] HOBS_km is observation height above sea level
                                                HGR_km,   & !> @param[in]  HGR_km is ground elevation above sea level in km
                                                NVERT,    &!> @param[in] NVERT - number of heights for vertical aerosol profile
                                                HVP_km,   &!> @param[in] HVP_km(NVERT) is vector of heights of layers of vertical profiles, descending order of height
                                                NSD,      &!> @param[in] NSD - number of aerosol fractions it is supposed that all of them come in successive order
                                                AVP_norm, &!> @param[in] APV_norm(NSD,NVERT) - vector of optimized parameters, vertical distribution (Aerosol Vertival Profile)
                                                EXTA,     &!> @param[in] EXTA(NSD) aerosol extinction
                                                DR_PAR,   &!> @param[in] DR_PAR(NSD) – aerosol columnar depolarization ration for PARALLEL component
                                                DR_PER,   &!> @param[in] DR_PER(NSD) – aerosol columnar depolarization ration for PERPENDICULAR component
                                                MU,       &!> @param[in] MU — crosstalk between parralel and cross-parallel channels
                                                MVP_norm, &!> @param[in] MVP_norm(NVERT) — Molecular Vertical Profile
                                                EXTM,     &!> @param[in] EXTM – molecular extinction
                                                LS        &!> @param[out] LS(NVERT) - ouput vector containing lidar measurement calculaions
                                              )

!> @brief soubroutine calculates forward lidar problem for PERPENDICULAR depolarization cahnnels
!> @brief for Single pixel and Single wavelength
!> @brief with Molecular correction (extinction and backscatter)
!> @brief
!>
      USE mod_molecular_scattering, only : LRM
      USE mod_par_inv, only  : KVERTM
      USE mod_par_os,  only  : KSD
      USE mod_stop_report

      IMPLICIT NONE

      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: HVP_km   ! vector of heights corresponds to AVP
      REAL, DIMENSION(KVERTM,KSD),INTENT(IN)     :: AVP_norm  ! aerosol vertical distribution
      REAL, DIMENSION(KSD),       INTENT(IN)     :: EXTA, DR_PAR, DR_PER !extinction and lidar ratio for all modes
      INTEGER,                    INTENT(IN)     :: NSD, NVERT !number of modes, number of vert strobes
      REAL,                       INTENT(IN)     :: HOBS_km ! height of the measures instalation point and altitude boundaries
      REAL,                       INTENT(IN)     :: HGR_km     ! ground altitude above sea level
      REAL,                       INTENT(IN)     :: EXTM ! molecular extinction
      REAL,                       INTENT(IN)     :: MU ! polarization channels crosstalk
      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: MVP_norm  !molecular profile
      REAL, DIMENSION(KVERTM),    INTENT(OUT)    :: LS   !lidar signal
!-- internal variables ------------------------------------------- 
      INTEGER                     :: ISD,IVERT  ! indices
      REAL, DIMENSION(KSD)        :: AVP_INT    ! AVP_int integrated aerosol profile  
      REAL                        :: EA,BA_PAR,BA_PER      ! aerosol extinction and backscatter for parallel and cross-parallel polarization
      REAL                        :: BM,EM      ! molecular backscatter and extinction
      REAL                        :: LS_INT     ! accumulator for lidar signal norm
      REAL                        :: MVP_INT    ! MVP_int integrated molecular backscatter
      REAL                        :: CHI        ! constant that defines depolarization of Rayleigh backscatter
      REAL                        :: DH         ! reversed vector of heights corresponds to AVP
      REAL, DIMENSION(KVERTM,KSD) :: AVP_temp   ! reversed aerosol vertical distribution
!------------------------------------------------------------------------------------------ 

!AL define CHI constant

      CHI=(0.014+MU)/(1+0.014)
 
      IF(HOBS_km .GT. HVP_km(1)) THEN
        WRITE(*,*) 'HOBS_km=',HOBS_km ,' .GT. DH(1)=', HVP_km(1)
        STOP 'ERROR IN lidar_signal_elastic_perpendicular: invalid measurements height'
      ENDIF

!WRITE(*,*) 'NVERT=', NVERT
!WRITE(*,*) 'EXTA=', EXTA
!WRITE(*,*) 'HOBS_km=', HOBS_km

      LS(:)      = 0.0      
      AVP_INT(:) = 0.0
      MVP_INT    = 0.0
      LS_INT     = 0.0

!      select case(IFMP)
!      case(1)
!        do IVERT=NVERT-1,1,-1
!          AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_temp(IVERT,1:NSD)+AVP_temp(IVERT+1,1:NSD))*(DH_temp(IVERT)-DH_temp(IVERT+1))
!          MVP_INT        = MVP_INT + 0.5*(MVP(IVERT)+MVP(IVERT+1))*(DH_temp(IVERT)-DH_temp(IVERT+1))
!          BA_PAR = SUM(EXTA(1:NSD)*AVP_temp(IVERT,1:NSD)*DR_PAR(1:NSD))
!          BA_PER = SUM(EXTA(1:NSD)*AVP_temp(IVERT,1:NSD)*DR_PER(1:NSD))
!          EA = SUM(EXTA(1:NSD)*AVP_INT(1:NSD))
!          BM = MVP(IVERT)
!          EM = MVP_INT*LRM
!          LS(IVERT) = (BA_PER+MU*BA_PAR+CHI*BM)*EXP(-2.*(EA+EM))
!        enddo ! IVERT
!      case(0)
        do IVERT=NVERT-1,1,-1
          DH = HVP_km(IVERT)-HVP_km(IVERT+1)
          AVP_INT(1:NSD) = AVP_INT(1:NSD) + 0.5*(AVP_norm(IVERT,1:NSD)+AVP_norm(IVERT+1,1:NSD))*DH
          MVP_INT        = MVP_INT + 0.5*(MVP_norm(IVERT)+MVP_norm(IVERT+1))*DH
          BA_PAR = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD)/DR_PAR(1:NSD))
          BA_PER = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD)/DR_PER(1:NSD))
          EA = SUM(EXTA(1:NSD)*AVP_INT(1:NSD)) 
          BM = EXTM*MVP_norm(IVERT)/LRM
          EM = EXTM*MVP_INT
          LS(IVERT) = (BA_PER+MU*BA_PAR+CHI*BM)*EXP(-2.*(EA+EM))
        enddo ! IVERT
!      end select

! LRM=4*pi*EM/BM0(i)  EM=LRM*BM0(i)
!write(*,*) 'EA=',EA,'  EXTA=',SUM(EXTA(1:NSD)),'  AVP_INT(1:NSD): ',AVP_INT(1:NSD)
!write(*,*) 'EM=',EM,'  EXTM=',EM,              '  MVP_INT       : ',MVP_INT

! Lidar Signal normalization
      DO IVERT=NVERT-2,1,-1
        DH = HVP_km(IVERT)-HVP_km(IVERT+1)
        LS_INT = LS_INT + 0.5*(LS(IVERT)+LS(IVERT+1))*DH
      ENDDO ! IVERT
      LS(1:NVERT-1) = LS(1:NVERT-1)/LS_INT !NVERT-1 because there is added one ground point that is extra

      IF(ANY(LS(1:NVERT-1) .LE. 0. .OR. IsNaN(LS(1:NVERT-1)))) THEN
        WRITE(*,*) 'ERROR IN lidar_signal_elastic_perpendicular:'
        WRITE(*,*) 'INVALID OUTPUT'
        WRITE(*,*) 'DEBUG INFORMATION:'
        WRITE(*,*) 'LS:'
        WRITE(*,'(10e14.5)') LS(1:NVERT-1)
        WRITE(*,*) 'AVP_INT: ', AVP_INT(1:NSD)
        WRITE(*,*) 'MVP_INT: ', MVP_INT
        WRITE(*,*) 'BA_PAR=', BA_PAR
        WRITE(*,*) 'BA_PER=', BA_PER
        WRITE(*,*) 'EA=',EA
        WRITE(*,*) 'EM=',EM,'  BM=',BM  
        WRITE(*,*) 'LS_INT=',LS_INT 
        WRITE(*,*) 'SUBROUTINE INPUTS:'
        WRITE(*,*) 'DR_PAR=',DR_PAR
        WRITE(*,*) 'DR_PER=',DR_PER
        WRITE(*,*) 'LRM=',LRM
        WRITE(*,*) 'MU=',MU
        DO ISD=1,NSD
          WRITE(*,*) 'AVP: ISD=',ISD
          WRITE(*,'(10e14.5)') AVP_norm(1:NVERT,ISD)
        ENDDO ! ISD   
        WRITE(*,*) 'HVP_km:'
        WRITE(*,'(10f14.5)') HVP_km(1:NVERT)
        WRITE(*,*) 'BM0:'
        WRITE(*,'(10e14.5)') MVP_norm(1:NVERT)
        WRITE(*,*) 'NVERT=', NVERT
        WRITE(*,*) 'EXTM=', EXTM
        WRITE(*,*) 'EXTA=',SUM(EXTA(1:NSD)),'  EXTA(1:NSD)=', EXTA(1:NSD)
        WRITE(*,*) 'HOBS_km=', HOBS_km
        !WRITE(*,*) 'DH AVP(1) AVP(2)'
        !DO IVERT=1,NVERT
          !WRITE(*,*) DH(IVERT),AVP(IVERT,1:NSD)
        !ENDDO
    !IF(ANY(LS(1:NVERT-1) .LT. 0. .OR. IsNaN(LS(1:NVERT-1)) .OR. LS(1:NVERT-1) .GT. 1.)) &
        STOP 'execution terminated in lidar_signal_elastic_perpendicular'
      ENDIF ! ANY(LS(1:NVERT) .LT. 0. .OR. ....
                        
      RETURN
      END SUBROUTINE lidar_signal_elastic_perpendicular

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      SUBROUTINE vertical_extinction (            &
                                       NVERT,     &!> @param[in] NVERT - number of heights for vertical aerosol profile
                                       HVP_km,    &!> @param[in] HVP_km(NVERT) – altitudes of layers of vertical profiles in descending order
                                       NSD,       &!> @param[in] NSD - number of aerosol fractions
                                       AVP_norm,  &!> @param[in] APV_norm(NSD,NVERT) - Aerosol Vertival Profile normalized (dimentionless)
                                       EXTA,      &!> @param[in] EXTA(NSD) aerosol extinction in 1/km
                                       VEXT       &!> @param[out] VEXT(NVERT) - ouput vector containing lidar measurement calculaions
                                     )

!> @brief Calculates vertical ectinction profile for Single pixel and Single wavelength
!> @brief
!>

      USE mod_par_inv, only  : KVERTM
      USE mod_par_os,  only  : KSD
      USE mod_stop_report

      IMPLICIT NONE
!-- input variables ----------------------------------------------

      INTEGER,                    INTENT(IN)     :: NSD, NVERT ! number of aerosol modes, number of AVP_norm
      REAL, DIMENSION(KVERTM,KSD),INTENT(IN)     :: AVP_norm   ! normalized Aerosol extinction Vertical Profile (AVP)
      REAL, DIMENSION(KVERTM),    INTENT(IN)     :: HVP_km     ! Vertical Profile Heights 
      REAL, DIMENSION(KSD),       INTENT(IN)     :: EXTA       ! aerosol extinction

!-- output variables ---------------------------------------------

      REAL, DIMENSION(KVERTM),    INTENT(OUT)    :: VEXT         ! lidar signal

!-- internal variables ------------------------------------------- 

      INTEGER                     :: ISD,IVERT  ! vertical index

!------------------------------------------------------------------------------------------
! [EXTA] = 1/km
 
!      IF(HOBS_km .GT. HVP_km(1)) THEN
!        WRITE(*,*) 'HOBS_km=',HOBS_km ,' .GT. HVP_km(1)=', HVP_km(1)
!        STOP 'ERROR IN vertical_extinction: invalid measurements height'
!      ENDIF

      VEXT(:) = 0.0

      do IVERT=1,NVERT
          VEXT(IVERT) = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD))
      enddo ! IVERT

      IF(ANY(VEXT(1:NVERT) .LE. 0. .OR. IsNaN(VEXT(1:NVERT)))) THEN
        GOTO 33
        WRITE(*,*) 'ERROR in vertical extinction:'
        WRITE(*,*) 'INVALID OUTPUT'
        WRITE(*,*) 'DEBUG INFORMATION:'
        WRITE(*,*) 'VEXT:'
        WRITE(*,'(10e14.5)') VEXT(1:NVERT)
        DO ISD=1,NSD
          WRITE(*,*) 'AVP_norm: ISD=',ISD
          WRITE(*,'(10e14.5)') AVP_norm(1:NVERT,ISD) 
        ENDDO ! ISD   
        WRITE(*,*) 'HVP in km:'
        WRITE(*,'(10f14.5)') HVP_km(1:NVERT)
        WRITE(*,*) 'NVERT=', NVERT
33      CONTINUE
        write(tmp_message,'(a)') 'invalid output, negative or NaN in vertical extinction'
        G_ERROR(trim(tmp_message))
      ENDIF ! ANY(LS(1:NVERT) .LT. 0. .OR. ....
                        
      RETURN
      END SUBROUTINE vertical_extinction

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine profile_normalization (NH, H, PROF_in, PROF_out)

      implicit none
!     INPUT
      integer,intent(in)             :: NH                    ! number of heights and points in profile
      real,intent(in)                :: H(NH),PROF_in(NH)     ! height and profile that be normalized
!     LOCAL
      integer                        :: ih
      real                           :: norma
!     OUTPUT
      real,intent(out)               :: PROF_out(NH)          ! normalized profile
!     ----------------------------------------------------------

      PROF_out(:) = 0.0

         norma=0
         do ih=2,NH
            norma = norma + 0.5 * (PROF_in(ih-1)+PROF_in(ih))  &
                                                       * (H(ih-1)-H(ih))
         enddo

         PROF_out = PROF_in/norma
      

!!begin       checking the normalization 
!         norma=0
!         do ih=2,NH
!            norma = norma + 0.5*(PROF_out(ih-1)+PROF_out(ih))  &
!                                                        *(H(ih-1)-H(ih))
!         enddo
!         write(*,*) norma,'  - if equal 1 than it is good normalization'
!!end

      end subroutine profile_normalization

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

subroutine lidar_signal_elastic_DP(                   &
                                    NVERT,            & !> @param[in]  NVERT - number of heights for vertical aerosol profile
                                    HVP_km,           & !> @param[in]  HVP_km(NVERT) – altitudes of layers of vertical profiles in descending order
                                    NSD,              & !> @param[in]  NSD - number of aerosol fractions
                                    AVP_norm,         & !> @param[in]  APV_norm(NSD,NVERT) - Aerosol Vertival Profile normalized (dimentionless)
                                    DR_PAR,           & !> @param[in]  DR_PAR — P11 for each aerosol component
                                    DR_PER,           & !> @param[in]  DR_PER — P22 for each aerosol component
                                    MVP_norm,         & !> @param[in]  MVP_norm — molecular vertical profile normalized (dimentionless)
                                    EXTM,             & !> @param[in]  EXTM - molecular extinction in 1/km
                                    DP                & !> @param[out] DP(NVERT) - ouput vector containing particle depolarisation calculaions
                                   )
!> @brief Soubroutine calculates vertical profile of particle depolarisation problem
!> @brief for Single pixel and one selected wavelenght
!>


    USE mod_molecular_scattering, only : LRM
    USE mod_par_inv, only  : KVERTM
    USE mod_par_os,  only  : KSD

    IMPLICIT NONE

    REAL, DIMENSION(KVERTM),    INTENT(IN)     :: HVP_km   ! vector of heights corresponds to AVP in km
    REAL, DIMENSION(KVERTM,KSD),INTENT(IN)     :: AVP_norm  ! normalized aerosol vertical distribution
    REAL, DIMENSION(KSD),       INTENT(IN)     :: DR_PAR, DR_PER !volume concentration, P11 and P22
    INTEGER,                    INTENT(IN)     :: NSD, NVERT !number of modes, number of vert strobes
    REAL,                       INTENT(IN)     :: EXTM ! molecular extinction in 1\km
    REAL, DIMENSION(KVERTM),    INTENT(IN)     :: MVP_norm  !molecular profile
    REAL, DIMENSION(KVERTM),    INTENT(OUT)    :: DP   !lidar signal
!-- internal variables ------------------------------------------------------
    INTEGER :: IVERT
    REAL::P11_temp,P22_temp
   ! WRITE(*,*),'L729 lidar_signal.f90, NVERT=',NVERT
 !BA = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD)/LRA(1:NSD))

    DO IVERT=NVERT,1,-1
!        P11_temp=SUM(DR_PAR(1:NSD)*AVP_norm(IVERT,1:NSD)*A_conc(1:NSD))
!        P22_temp=SUM(DR_PER(1:NSD)*AVP_norm(IVERT,1:NSD)*A_conc(1:NSD))
        P11_temp=SUM(DR_PAR(1:NSD)*AVP_norm(IVERT,1:NSD))
        P22_temp=SUM(DR_PER(1:NSD)*AVP_norm(IVERT,1:NSD))
        DP(IVERT) = (P11_temp-P22_temp)/(P11_temp+P22_temp)*100
    ENDDO
    !DP(:) = (1-P22_temp/P11_temp)/(1+P22_temp/P11_temp)*100
    !WRITE(*,*),DP
end subroutine

!!!!   ~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~
!!!!   2017/03/11 by Qiaoyun HU
!!!!    this subroutine compute the total depolarization ratio determined by the backscattering ratio, molecular depolarization
!!!!    and particle depolarization ratio which is taken directly from the spheroid model.
!!!!   ~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~


subroutine lidar_signal_elastic_VLDP(                         &
                                        NVERT,                & !> @param[in]  NVERT - number of heights for vertical aerosol profile
                                        HVP_km,               & !> @param[in]  HVP_km(NVERT) – altitudes of layers of vertical profiles in descending order
                                        NSD,                  & !> @param[in]  NSD - number of aerosol fractions
                                        AVP_norm,             & !> @param[in]  APV_norm(NSD,NVERT) - Aerosol Vertival Profile normalized (dimentionless)
                                        MDPR_wl,              & !> @param[in]  MDPR_wl — molecular depolarization ratio for current channel
                                        EXTA,                 & !> @param[in]  EXTA(NSD) aerosol extinction in 1/km
                                        LRA,                  & !> @param[in]  LRA - Lidar Ratio of Aerosol in Sr
                                        DR_PAR,               & !> @param[in]  DR_PAR — P11 for each aerosol component
                                        DR_PER,               & !> @param[in]  DR_PER — P22 for each aerosol component
                                        MVP_norm,             & !> @param[in]  MVP_norm — molecular vertical profile normalized (dimentionless)
                                        EXTM,                 & !> @param[in]  EXTM - molecular extinction in 1/km
                                        VLDP                  & !> @param[out] VLDP(NVERT) - ouput vector containing volume depolarisation calculaions
                                      )
!> @brief Soubroutine calculates vertical profile of volume depolarisation problem
!> @brief for Single pixel and one selected wavelenght
!>
USE mod_molecular_scattering, only : LRM
USE mod_par_inv, only  : KVERTM
USE mod_par_os,  only  : KSD
USE mod_stop_report

IMPLICIT NONE

REAL, DIMENSION(KVERTM),    INTENT(IN)     :: HVP_km         ! vector of heights corresponds to AVP in km
REAL, DIMENSION(KVERTM,KSD),INTENT(IN)     :: AVP_norm       ! normalized aerosol vertical distribution
REAL, DIMENSION(KSD),       INTENT(IN)     :: DR_PAR, DR_PER ! P11(NSD) and P22(NSD)
INTEGER,                    INTENT(IN)     :: NSD, NVERT     ! number of modes, number of vert strobes
REAL,                       INTENT(IN)     :: MDPR_wl        ! molecular depolarization ratio
REAL,                       INTENT(IN)     :: EXTM           ! molecular extinction in 1\km
REAL, DIMENSION(KSD),       INTENT(IN)     :: EXTA           ! aerosol extinction
REAL, DIMENSION(KVERTM),    INTENT(IN)     :: MVP_norm       ! molecular profile
REAL, DIMENSION(KSD),       INTENT(IN)     :: LRA            ! aerosol lidar ratio
REAL, DIMENSION(KVERTM),    INTENT(OUT)    :: VLDP           !volume depolarization ratio
!!! -- internal variables ------------------------------------------------------
INTEGER :: IVERT, ISD
REAL::P11_temp,P22_temp
REAL, DIMENSION(KVERTM)    :: DP   !    particle depolarization ratio
REAL, DIMENSION(KVERTM)    :: BA   !    aerosol backscattering coefficient
REAL, DIMENSION(KVERTM)    :: BM   !    molecular backscattering coefficient
REAL, DIMENSION(KVERTM)    :: B_ratio   !   backscattering ratio (BA+BM)/BM


!!! ------------------------------------------------------------------------------
!!!         set molecular depolarization ratio
!!! ------------------------------------------------------------------------------


!    IF(HOBS_km .GT. HVP_km(1)) THEN
!        WRITE(*,*) 'HOBS_km=',HOBS_km ,' .GT. HVP_km(1)=', HVP_km(1)
!        STOP 'ERROR IN lidar_signal_elastic_v0: invalid measurements height'
!    ENDIF
!!! ------------------------------------------------------------------------------
!!! ----           compute the aerosol and molecular backscattering
    DO IVERT=NVERT,1,-1
        BA(IVERT) = SUM(EXTA(1:NSD)*AVP_norm(IVERT,1:NSD)/LRA(1:NSD))!*A_conc(1:NSD)/LRA(1:NSD))
        BM(IVERT) = EXTM*MVP_norm(IVERT)/LRM
    END DO

    B_ratio=(BA+BM)/BM      ! backscattering ratio
!    write(*,*) 'The molecular DPR=',MDPR_wl
!    do IVERT=NVERT,1,-1
!        write(*,*) HVP_km(IVERT),B_ratio(IVERT)
!    end do

!!! ------------------------------------------------------------------------------
!!! ----           compute particle depolarization

    DO IVERT=NVERT,1,-1
        P11_temp=SUM(DR_PAR(1:NSD)*AVP_norm(IVERT,1:NSD))
        P22_temp=SUM(DR_PER(1:NSD)*AVP_norm(IVERT,1:NSD))
!P11_temp=SUM(DR_PAR(1:NSD)*AVP_norm(IVERT,1:NSD)*A_conc(1:NSD))
!P22_temp=SUM(DR_PER(1:NSD)*AVP_norm(IVERT,1:NSD)*A_conc(1:NSD))
        DP(IVERT) = (P11_temp-P22_temp)/(P11_temp+P22_temp)     ! particle depolarization

!!!         The formular for calculating the total/volume depolarization ratio is 
!!!         --------------------------------------------------------------------------------------------------------
!!!         (R * delta_a * (delta_m + 1) + delta_m - delta_a ) / (R * (delta_m + 1) + delta_a - delta_m)
!!!         --------------------------------------------------------------------------------------------------------
!!!         R: backscattering ratio;  delta_a: Particle depolarization ratio   delta_m: Molecular depolarization ratio

        VLDP(IVERT)= (B_ratio(IVERT)*DP(IVERT)*(MDPR_wl+1)+MDPR_wl-DP(IVERT))/   &
                     (B_ratio(IVERT)*(MDPR_wl+1)+DP(IVERT)-MDPR_wl)
    IF (VLDP(IVERT) .LE. 0 ) VLDP(IVERT)=1.0e-10 !dirty hack for simulations
    ENDDO
    VLDP = VLDP*100

      IF(ANY(VLDP(1:NVERT) .LE. 0. .OR. IsNaN(VLDP(1:NVERT)))) THEN
!        GOTO 33
        WRITE(*,*) 'ERROR IN lidar_signal_elastic_VLDP:'
        WRITE(*,*) 'INVALID OUTPUT'
        WRITE(*,*) 'DEBUG INFORMATION:'
        WRITE(*,*) 'VLDP:'
        WRITE(*,'(10e14.5)') VLDP(1:NVERT)
        DO ISD=1,NSD
          WRITE(*,*) 'AVP_norm: ISD=',ISD
          WRITE(*,'(10e14.5)') AVP_norm(1:NVERT,ISD)
        ENDDO ! ISD
        WRITE(*,*) 'HVP in km:'
        WRITE(*,'(10f14.5)') HVP_km(1:NVERT)
        WRITE(*,*) 'NVERT=', NVERT
33      CONTINUE
        write(tmp_message,'(a)') 'invalid output, negative or NaN in lidar_signal_elastic_VLDP'
        G_ERROR(trim(tmp_message))
      ENDIF ! ANY(LS(1:NVERT) .LT. 0. .OR. ....

      RETURN
end subroutine
