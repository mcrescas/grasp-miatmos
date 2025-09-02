#include "../../../constants_set/mod_globals.inc"
        !> @brief Module contains kernels derived type structure and routines
        !> @brief to allocate and deallocate kernels for triangle bins
        !>

module mod_alloc_gas_lut
	  
      use mod_stop_report
      use mod_par_OS,  only : NMG
      use mod_par_inv, only : KW
      use mod_par_OS,  only : KVERT_WD,N_SUB_CHANNEL_MAX,N_WL_CHANNEL_MAX

      implicit none
    
    !MH   Maximun number of bins
    INTEGER, PARAMETER :: MAXKD = 300

! ......................................................................
        !>
!MH Filter information from the filter files

!MH This variable stores the original filter information as it is read from the input
   TYPE filter_channel_type
       INTEGER                                      :: NWL_total
       REAL,     DIMENSION(:,:),     ALLOCATABLE    :: WVL
       REAL,     DIMENSION(:),       ALLOCATABLE    :: center_subchannels
       REAL,     DIMENSION(:),       ALLOCATABLE    :: bandwidth
       INTEGER,  DIMENSION(:),       ALLOCATABLE    :: NWL
       REAL,     DIMENSION(:,:),     ALLOCATABLE    :: TRANSMISSION
   END TYPE filter_channel_type


    TYPE channels_filter
            TYPE(filter_channel_type), DIMENSION(KW) :: filter_channel
    END TYPE channels_filter

!MH This variable is used to accumulate the reinterpolated transmission to the K-Dist or LUT spectral resolution
!MH This is just a temporary solution this variable should only be calculated once per GRASP call not in all FW calls
  TYPE filter_transmission
      INTEGER,DIMENSION(N_SUB_CHANNEL_MAX)           :: NWL
      REAL,    DIMENSION(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX)  :: TRANSMISSION
  END TYPE filter_transmission

! ......................................................................
        !>
    !MH Information about atmospheric profile
        TYPE ATM_ATP_PROF
            INTEGER                                :: NLV     !MH Number of levels in atmospheric profile
            REAL                                   :: STEMP   !MH Surface temperature in K
            REAL,  DIMENSION(:),     ALLOCATABLE   :: ALT     !MH Levels altitude in m
            REAL,  DIMENSION(:),     ALLOCATABLE   :: P       !MH Levels pressure, units just have to be coherent with LUTs.
            REAL,  DIMENSION(:),     ALLOCATABLE   :: T       !MH Levels temperature in K
        END TYPE ATM_ATP_PROF
!NLEVEL_GAS
! ......................................................................
        !>
      TYPE LUT_SPECIE
        CHARACTER(LEN=GBL_FILE_PATH_LEN)              ::    NAME
        REAL, DIMENSION (:,:,:),     ALLOCATABLE      ::    EXT
        REAL, DIMENSION (:),         ALLOCATABLE      ::    CCT
        REAL, DIMENSION (:),         ALLOCATABLE      ::    WL_GAS
        INTEGER                                       ::    NWL_LUT
        INTEGER                                       ::    NP_LUT
        INTEGER                                       ::    NT_LUT
        REAL, DIMENSION (:),         ALLOCATABLE      ::    P_LUT
        REAL, DIMENSION (:),         ALLOCATABLE      ::    ALT_LUT
        REAL, DIMENSION (:),         ALLOCATABLE      ::    T_LUT
      END TYPE LUT_SPECIE
! ......................................................................
      TYPE LUT_ABS
              CHARACTER(LEN=GBL_FILE_PATH_LEN)            :: PATH
              CHARACTER(LEN=GBL_FILE_PATH_LEN)            :: VTP
              INTEGER                                     :: NSPECIES
              LOGICAL                                     :: READ_LUT
              TYPE(LUT_SPECIE), DIMENSION(NMG)   :: SPECIE
      END TYPE LUT_ABS
! ......................................................................
        !>

    TYPE DATA_ABS_GS_KD
       
       REAL, DIMENSION(1)                           :: TOTAL_EXT_KD
       INTEGER                                      :: NSubCH
    !MH      number of levels OF SUBCHANNEL
       INTEGER,DIMENSION(:),    ALLOCATABLE         :: NLV_SC
       INTEGER,DIMENSION(:),    ALLOCATABLE         :: NWL_SC
       
    !XH      number of elements in AIKD
       INTEGER,DIMENSION(:),    ALLOCATABLE         :: NEXP
       INTEGER,DIMENSION(:),    ALLOCATABLE         :: NGAS
       INTEGER,DIMENSION(:,:),  ALLOCATABLE         :: MAPPING_FUNCTION
       REAL,DIMENSION(:,:),     ALLOCATABLE         :: WVL_SC
    !XH      weights for K-distribution
       REAL,DIMENSION(:,:),     ALLOCATABLE         :: AIKD !MH Dimensions: [bin,subchannel]
    !XH      tau for vertical layers
       REAL,DIMENSION(:,:,:,:),   ALLOCATABLE       :: COEFKD !MH Dimensions: [level,bin,gas,subchannel] 
    END TYPE DATA_ABS_GS_KD

! ......................................................................
    TYPE DATA_KD
            
            INTEGER                                 :: NCH
    !XH      number of levels
            INTEGER                                 :: NLV
            REAL, DIMENSION(NMG)                    :: TOTAL_GAS_C_REF_KD
            TYPE(DATA_ABS_GS_KD), DIMENSION(KW)     :: ABS_GS_KD
    END TYPE DATA_KD
! ......................................................................
        !>

      type(LUT_ABS)         :: LUT_GASES
      type(DATA_KD)         :: GAS_KD_DATA
      type(ATM_ATP_PROF)    :: ATM_PROF
      type(channels_filter) :: filters_shape
      logical               :: read_gas_abs
      logical               :: read_filter_shape
      logical               :: filter_exists

contains

subroutine alloc_filter_shape (j)
  
  implicit none


  integer, intent(in) :: j

! ......................................................................
  integer :: ierr
! ......................................................................

  allocate ( filters_shape%filter_channel(j)%WVL(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX), stat=ierr )
    if ( ierr/=0 ) then
    write(*,*)j
    write(tmp_message,'(a)') 'Can not allocate channels_filter%filter_channel%WVL array.'
    G_ERROR(trim(tmp_message))
    endif
    filters_shape%filter_channel(j)%WVL(:,:) = 0.0
    allocate ( filters_shape%filter_channel(j)%NWL(N_SUB_CHANNEL_MAX), stat=ierr )
      if ( ierr/=0 ) then
      write(tmp_message,'(a)') 'Can not allocate channels_filter%filter_channel%NWL array.'
      G_ERROR(trim(tmp_message))
      endif
      filters_shape%filter_channel(j)%NWL(:) = 0.0
    allocate ( filters_shape%filter_channel(j)%TRANSMISSION(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX), stat=ierr )
      if ( ierr/=0 ) then
      write(tmp_message,'(a)') 'Can not allocate channels_filter%filter_channel%TRANSMISSION array.'
      G_ERROR(trim(tmp_message))
      endif
      filters_shape%filter_channel(j)%TRANSMISSION(:,:) = 0.0
    allocate ( filters_shape%filter_channel(j)%center_subchannels(N_SUB_CHANNEL_MAX), stat=ierr )
      if ( ierr/=0 ) then
      write(tmp_message,'(a)') 'Can not allocate channels_filter%filter_channel%center_subchannels array.'
      G_ERROR(trim(tmp_message))
      endif
      filters_shape%filter_channel(j)%center_subchannels(:) = 0.0
    allocate ( filters_shape%filter_channel(j)%bandwidth(N_SUB_CHANNEL_MAX), stat=ierr )
      if ( ierr/=0 ) then
      write(tmp_message,'(a)') 'Can not allocate channels_filter%filter_channel%bandwidth array.'
      G_ERROR(trim(tmp_message))
      endif
      filters_shape%filter_channel(j)%bandwidth(:) = 0.0


  return
  end subroutine alloc_filter_shape


subroutine alloc_atm_prof ()
  
  implicit none

! ......................................................................
  integer :: ierr
! ......................................................................

    allocate ( ATM_PROF%ALT(KVERT_WD), stat=ierr )
      if ( ierr/=0 ) then
      write(tmp_message,'(a)') 'Can not allocate ATM_PROF%ALT array.'
      G_ERROR(trim(tmp_message))
      endif
      ATM_PROF%ALT(:) = 0.0
    allocate ( ATM_PROF%P(KVERT_WD), stat=ierr )
      if ( ierr/=0 ) then
      write(tmp_message,'(a)') 'Can not allocate ATM_PROF%P array.'
      G_ERROR(trim(tmp_message))
      endif
      ATM_PROF%P(:) = 0.0
    allocate ( ATM_PROF%T(KVERT_WD), stat=ierr )
      if ( ierr/=0 ) then
      write(tmp_message,'(a)') 'Can not allocate ATM_PROF%T array.'
      G_ERROR(trim(tmp_message))
      endif
      ATM_PROF%T(:) = 0.0

  return
  end subroutine alloc_atm_prof


    subroutine alloc_kd_gas (j)
      
      implicit none


      integer, intent(in) :: j

! ......................................................................
      integer :: ierr
! ......................................................................

      allocate ( GAS_KD_DATA%ABS_GS_KD(j)%COEFKD(KVERT_WD-1,MAXKD,NMG,N_SUB_CHANNEL_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate GAS_KD_DATA%ABS_GS_KD%COEFKD array.'
        G_ERROR(trim(tmp_message))
        endif
        GAS_KD_DATA%ABS_GS_KD(j)%COEFKD(:,:,:,:) = 0.0
      allocate ( GAS_KD_DATA%ABS_GS_KD(j)%AIKD(MAXKD,N_SUB_CHANNEL_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate GAS_KD_DATA%ABS_GS_KD%AIKD array.'
        G_ERROR(trim(tmp_message))
        endif
        GAS_KD_DATA%ABS_GS_KD(j)%AIKD(:,:) = 0.0
      allocate ( GAS_KD_DATA%ABS_GS_KD(j)%WVL_SC(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate GAS_KD_DATA%ABS_GS_KD%WVL_SC array.'
        G_ERROR(trim(tmp_message))
        endif
        GAS_KD_DATA%ABS_GS_KD(j)%WVL_SC(:,:) = 0.0
      allocate ( GAS_KD_DATA%ABS_GS_KD(j)%MAPPING_FUNCTION(N_WL_CHANNEL_MAX,N_SUB_CHANNEL_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate GAS_KD_DATA%ABS_GS_KD%MAPPING_FUNCTION array.'
        G_ERROR(trim(tmp_message))
        endif
        GAS_KD_DATA%ABS_GS_KD(j)%MAPPING_FUNCTION(:,:) = 0.0
      allocate ( GAS_KD_DATA%ABS_GS_KD(j)%NGAS(N_SUB_CHANNEL_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate GAS_KD_DATA%ABS_GS_KD%NGAS array.'
        G_ERROR(trim(tmp_message))
        endif
        GAS_KD_DATA%ABS_GS_KD(j)%NGAS(:) = 0.0
      allocate ( GAS_KD_DATA%ABS_GS_KD(j)%NEXP(N_SUB_CHANNEL_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate GAS_KD_DATA%ABS_GS_KD%NEXP array.'
        G_ERROR(trim(tmp_message))
        endif
        GAS_KD_DATA%ABS_GS_KD(j)%NEXP(:) = 0.0
      allocate ( GAS_KD_DATA%ABS_GS_KD(j)%NWL_SC(N_SUB_CHANNEL_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate GAS_KD_DATA%ABS_GS_KD%NWL_SC array.'
        G_ERROR(trim(tmp_message))
        endif
        GAS_KD_DATA%ABS_GS_KD(j)%NWL_SC(:) = 0.0
      allocate ( GAS_KD_DATA%ABS_GS_KD(j)%NLV_SC(N_SUB_CHANNEL_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate GAS_KD_DATA%ABS_GS_KD%NLV_SC array.'
        G_ERROR(trim(tmp_message))
        endif
        GAS_KD_DATA%ABS_GS_KD(j)%NLV_SC(:) = 0.0

      return
      end subroutine alloc_kd_gas



! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine allocates LUT for gases and initializstion for them
        !>
        !> @param[inout] ISPEC     - Index of the gas that is being allocated
        !> @param[inout] LUT_GASES - Structure of LUT for gases
        !>
      subroutine alloc_lut_gas_header ( ISPEC,NWL_LUT,NP_LUT,NT_LUT)
            
            implicit none

      !MH We need all these parameters as input because eac LUT can have a different size, so we need yo call this subroutine once per lut. This is done to enable the optimization of the LUTs, to make ones bigger and others smaller.

            integer, intent(in) :: ISPEC,NWL_LUT,NP_LUT,NT_LUT
      ! ......................................................................
            integer :: ierr
      ! ......................................................................
            LUT_GASES%SPECIE(ISPEC)%NWL_LUT  = NWL_LUT  ! number of wavelengths
            LUT_GASES%SPECIE(ISPEC)%NP_LUT   = NP_LUT   ! number of pressure levels
            LUT_GASES%SPECIE(ISPEC)%NT_LUT   = NT_LUT   ! number of temperature levels

            allocate ( LUT_GASES%SPECIE(ISPEC)%P_LUT(NP_LUT), stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%P_LUT array.'
              G_ERROR(trim(tmp_message))
              endif
              LUT_GASES%SPECIE(ISPEC)%P_LUT(:) = 0.0
            allocate ( LUT_GASES%SPECIE(ISPEC)%T_LUT(NT_LUT), stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%T_LUT array.'
              G_ERROR(trim(tmp_message))
              endif
              LUT_GASES%SPECIE(ISPEC)%T_LUT(:) = 0.0
            allocate ( LUT_GASES%SPECIE(ISPEC)%ALT_LUT(NP_LUT), stat=ierr )
                if ( ierr/=0 ) then
                write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%ALT_LUT array.'
                G_ERROR(trim(tmp_message))
                endif
                LUT_GASES%SPECIE(ISPEC)%ALT_LUT(:) = 0.0
            allocate ( LUT_GASES%SPECIE(ISPEC)%WL_GAS(NWL_LUT), stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%WL_GAS array.'
              G_ERROR(trim(tmp_message))
              endif
              LUT_GASES%SPECIE(ISPEC)%WL_GAS(:) = 0.0
            allocate ( LUT_GASES%SPECIE(ISPEC)%CCT(NP_LUT), stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%CCT array.'
              G_ERROR(trim(tmp_message))
              endif
              LUT_GASES%SPECIE(ISPEC)%CCT(:) = 0.0
            allocate ( LUT_GASES%SPECIE(ISPEC)%EXT(NWL_LUT,NT_LUT,NP_LUT), stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%EXT array.'
              G_ERROR(trim(tmp_message))
              endif
              LUT_GASES%SPECIE(ISPEC)%EXT(:,:,:) = 0.0

            return
            end subroutine alloc_lut_gas_header



subroutine alloc_lut_gas_fixed (ISPEC,NWL_LUT,NP_LUT,NT_LUT)
      use mod_par_OS,  only : NMG,N_WL_GLUT_MAX,N_P_GLUT_MAX,N_T_GLUT_MAX
      implicit none


      integer, intent(in) :: ISPEC,NWL_LUT,NP_LUT,NT_LUT

! ......................................................................
      integer :: ierr
! ......................................................................
      LUT_GASES%SPECIE(ISPEC)%NWL_LUT  = NWL_LUT  ! number of wavelengths
      LUT_GASES%SPECIE(ISPEC)%NP_LUT   = NP_LUT   ! number of pressure levels
      LUT_GASES%SPECIE(ISPEC)%NT_LUT   = NT_LUT   ! number of temperature levels

      allocate ( LUT_GASES%SPECIE(ISPEC)%P_LUT(N_P_GLUT_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%P_LUT array.'
        G_ERROR(trim(tmp_message))
        endif
        LUT_GASES%SPECIE(ISPEC)%P_LUT(:) = 0.0
      allocate ( LUT_GASES%SPECIE(ISPEC)%T_LUT(N_T_GLUT_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%T_LUT array.'
        G_ERROR(trim(tmp_message))
        endif
        LUT_GASES%SPECIE(ISPEC)%T_LUT(:) = 0.0
      allocate ( LUT_GASES%SPECIE(ISPEC)%ALT_LUT(NP_LUT), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%ALT_LUT array.'
        G_ERROR(trim(tmp_message))
        endif
        LUT_GASES%SPECIE(ISPEC)%ALT_LUT(:) = 0.0
      allocate ( LUT_GASES%SPECIE(ISPEC)%WL_GAS(N_WL_GLUT_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%WL_GAS array.'
        G_ERROR(trim(tmp_message))
        endif
        LUT_GASES%SPECIE(ISPEC)%WL_GAS(:) = 0.0
      allocate ( LUT_GASES%SPECIE(ISPEC)%CCT(N_P_GLUT_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%CCT array.'
        G_ERROR(trim(tmp_message))
        endif
        LUT_GASES%SPECIE(ISPEC)%CCT(:) = 0.0
      allocate ( LUT_GASES%SPECIE(ISPEC)%EXT(N_WL_GLUT_MAX,N_T_GLUT_MAX,N_P_GLUT_MAX), stat=ierr )
        if ( ierr/=0 ) then
        write(tmp_message,'(a)') 'Can not allocate LUT_GASES%SPECIE%EXT array.'
        G_ERROR(trim(tmp_message))
        endif
        LUT_GASES%SPECIE(ISPEC)%EXT(:,:,:) = 0.0

      return
      end subroutine alloc_lut_gas_fixed


! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine deallocates filter shape transmissions
        !>
        !> @param[inout] j           - Index of the channel/subchannel that is being allocated
        !> @param[inout] channels_filter - Structure of filters
        !>
      subroutine dealloc_filter_shape ( c_sub )

      implicit none
         
      integer, intent(in) ::  c_sub
! ......................................................................
      integer :: ierr, j
! ......................................................................

      do j=1, c_sub

            deallocate ( filters_shape%filter_channel(j)%WVL, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate channels_filter%filter_channel%WV array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( filters_shape%filter_channel(j)%NWL, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate channels_filter%filter_channel%NWL array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( filters_shape%filter_channel(j)%TRANSMISSION, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate channels_filter%filter_channel%TRANSMISSION array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( filters_shape%filter_channel(j)%center_subchannels, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate channels_filter%filter_channel%center_subchannels array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( filters_shape%filter_channel(j)%bandwidth, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate channels_filter%filter_channel%bandwidth array.'
              G_ERROR(trim(tmp_message))
              endif

      end do

      return
      end subroutine dealloc_filter_shape


! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine deallocates profile with atmospheric information and surface temperature
        !>
        !>
      subroutine dealloc_atm_prof ()

      implicit none
         
! ......................................................................
      integer :: ierr
! ......................................................................

        deallocate ( ATM_PROF%ALT, stat=ierr )
          if ( ierr/=0 ) then
          write(tmp_message,'(a)') 'Can not deallocate ATM_PROF%ALT array.'
          G_ERROR(trim(tmp_message))
          endif
        deallocate ( ATM_PROF%P, stat=ierr )
          if ( ierr/=0 ) then
          write(tmp_message,'(a)') 'Can not deallocate ATM_PROF%P array.'
          G_ERROR(trim(tmp_message))
          endif
        deallocate ( ATM_PROF%T, stat=ierr )
          if ( ierr/=0 ) then
          write(tmp_message,'(a)') 'Can not deallocate ATM_PROF%T array.'
          G_ERROR(trim(tmp_message))
          endif
            
      return
      end subroutine dealloc_atm_prof




! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine deallocates KD information for gases
        !>
        !> @param[inout] j           - Index of the channel/subchannel that is being allocated
        !> @param[inout] GAS_KD_DATA - Structure of KD for gases
        !>
      subroutine dealloc_kd_gas ( c_sub )

      implicit none
         
      integer, intent(in) ::  c_sub
! ......................................................................
      integer :: ierr, j
! ......................................................................

      do j=1, c_sub

            deallocate ( GAS_KD_DATA%ABS_GS_KD(j)%COEFKD, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate GAS_KD_DATA%ABS_GS_KD%COEFKD array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( GAS_KD_DATA%ABS_GS_KD(j)%AIKD, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate GAS_KD_DATA%ABS_GS_KD%AIKD array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( GAS_KD_DATA%ABS_GS_KD(j)%WVL_SC, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate GAS_KD_DATA%ABS_GS_KD%WVL_SC array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( GAS_KD_DATA%ABS_GS_KD(j)%MAPPING_FUNCTION, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate GAS_KD_DATA%ABS_GS_KD%MAPPING_FUNCTION array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( GAS_KD_DATA%ABS_GS_KD(j)%NGAS, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate GAS_KD_DATA%ABS_GS_KD%NGAS array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( GAS_KD_DATA%ABS_GS_KD(j)%NEXP, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate GAS_KD_DATA%ABS_GS_KD%NEXP array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( GAS_KD_DATA%ABS_GS_KD(j)%NWL_SC, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate GAS_KD_DATA%ABS_GS_KD%NWL_SC array.'
              G_ERROR(trim(tmp_message))
              endif
            deallocate ( GAS_KD_DATA%ABS_GS_KD(j)%NLV_SC, stat=ierr )
              if ( ierr/=0 ) then
              write(tmp_message,'(a)') 'Can not deallocate GAS_KD_DATA%ABS_GS_KD%NLV_SC array.'
              G_ERROR(trim(tmp_message))
              endif

      end do

      return
      end subroutine dealloc_kd_gas



! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine deallocates LUT for gases
        !>
        !> @param[inout] ISPEC     - Index of the gas that is being allocated
        !> @param[inout] LUT_GASES - Structure of LUT for gases
        !>
      subroutine dealloc_lut_gas ( NLUTS )

      implicit none	  	  
         
      integer, intent(in) ::  NLUTS
! ......................................................................
      integer :: ierr, ISPEC
! ......................................................................

      do ISPEC =1, NLUTS

          deallocate ( LUT_GASES%SPECIE(ISPEC)%P_LUT, stat=ierr )
            if ( ierr/=0 ) then
            write(tmp_message,'(a)') 'Can not deallocate LUT_GASES%SPECIE%P_LUT array.'
            G_ERROR(trim(tmp_message))
            endif
          deallocate ( LUT_GASES%SPECIE(ISPEC)%T_LUT, stat=ierr )
            if ( ierr/=0 ) then
            write(tmp_message,'(a)') 'Can not deallocate LUT_GASES%SPECIE%T_LUT array.'
            G_ERROR(trim(tmp_message))
            endif
          deallocate ( LUT_GASES%SPECIE(ISPEC)%ALT_LUT, stat=ierr )
            if ( ierr/=0 ) then
            write(tmp_message,'(a)') 'Can not deallocate LUT_GASES%SPECIE%ALT_LUT array.'
            G_ERROR(trim(tmp_message))
            endif
          deallocate ( LUT_GASES%SPECIE(ISPEC)%WL_GAS, stat=ierr )
            if ( ierr/=0 ) then
            write(tmp_message,'(a)') 'Can not deallocate LUT_GASES%SPECIE%WL_GAS array.'
            G_ERROR(trim(tmp_message))
            endif
          deallocate ( LUT_GASES%SPECIE(ISPEC)%CCT, stat=ierr )
            if ( ierr/=0 ) then
            write(tmp_message,'(a)') 'Can not deallocate LUT_GASES%SPECIE%CCT array.'
            G_ERROR(trim(tmp_message))
            endif
          deallocate ( LUT_GASES%SPECIE(ISPEC)%EXT, stat=ierr )
            if ( ierr/=0 ) then
            write(tmp_message,'(a)') 'Can not deallocate LUT_GASES%SPECIE%EXT array.'
            G_ERROR(trim(tmp_message))
            endif

      end do

      return
      end subroutine dealloc_lut_gas


     subroutine read_lut_head(PATH,NAME,NWL_LUT,NP_LUT,NT_LUT)

     IMPLICIT NONE

     CHARACTER(*),                       INTENT(IN)        :: PATH, NAME

     INTEGER,                            INTENT(OUT)       :: NWL_LUT
     INTEGER,                            INTENT(OUT)       :: NP_LUT
     INTEGER,                            INTENT(OUT)       :: NT_LUT

     CHARACTER(200)    :: AUX,CHECK_WL
     LOGICAL           :: file_exists
     INTEGER           :: b

     inquire(file=TRIM(path)//TRIM(name), EXIST=file_exists)
    
     if(file_exists) then

        OPEN(UNIT = 1,file=TRIM(path)//TRIM(name), status = 'old', iostat=b, action='read')

        READ(1,*) AUX,AUX,AUX,NWL_LUT,AUX,AUX,AUX,AUX,NP_LUT,NT_LUT

        CLOSE(1)

     else

        WRITE(*,*) 'ERROR: ', TRIM(path)//TRIM(name),  ' must be an existing file. LUT ', TRIM(name) , ' does not exist or is not installed in ', TRIM(path), '. Please, install it correctly or choose a proper LUT name.'
        STOP
           
     end if

    end subroutine read_lut_head


! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine for_avoiding_warnings_in_empty_module()
      end subroutine for_avoiding_warnings_in_empty_module

end module mod_alloc_gas_lut
