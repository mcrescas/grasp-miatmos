!.......................................................................
!     Generalized Retrieval of Atmosphere and Surface Properties
!     GRASP S.A.S
!.......................................................................
!
!     MODULE: MOD_RT_SOS_SETUP
!>    @author
!>    Xin Huang, GRASP S.A.S
!
!     DESCRIPTION:
!>    setup and control flags in radiative transfer
!
!     REVISION HISTORY:
!     13/02/2018 - Initial Version
!.......................................................................
      MODULE MOD_RT_SOS_SETUP
      USE MOD_GLOBALS,ONLY : GBL_FILE_PATH_LEN
      USE MOD_PAR_OS, ONLY : KSD
      USE MOD_C_UTILS
      IMPLICIT NONE

      TYPE :: RT_SOS_SETUP
!>        verbose mode
          LOGICAL :: IP_VERBOSE
!>        print additional information
          LOGICAL :: IP_ADDITION

!>        flag for truncation
          LOGICAL :: ITRC
!>        flag for output to scattering plane or meridian plane
          LOGICAL :: ISCA

!>        flag for using vector or scalar radiative transfer
          LOGICAL :: IVEC
!>        flag for LUT generation
          LOGICAL :: IWUT
!>        flag for using LUT
          LOGICAL :: ILUT
!>        flag for bi-hemispherical reflectance and directional
!>        hemispherical of atmosphere
          LOGICAL :: IATM
!>        flag for atmosphere correction
          LOGICAL :: ICRR

          LOGICAL :: boa_ref

          INTEGER :: AER_PRF
          INTEGER :: MOL_PRF
          INTEGER :: NA
          INTEGER, DIMENSION(KSD) :: NB
          INTEGER, DIMENSION( 2 ) :: NLYR

          REAL    :: EPS

          CHARACTER(LEN=GBL_FILE_PATH_LEN) :: INTL_PATH
          CHARACTER(LEN=GBL_FILE_PATH_LEN) :: EXTL_PATH
          CHARACTER(LEN=GBL_FILE_PATH_LEN) :: LUT_path


      END TYPE RT_SOS_SETUP

      TYPE :: RT_SOS_CNTRL
!>        flag for forward or derivative calculation
          LOGICAL :: IJCB
!>        flag for single scattering calculation
          LOGICAL :: ISGL
!>        flag for considering aerosol
          LOGICAL :: IAER

!>        using vector or scalar radiative transfer
          LOGICAL :: IVEC
!>        considering surface BRDF or not
          LOGICAL :: ISRF
!>        calculating downward or upward radiance
          LOGICAL :: IDWN
!>        calculating flux or not
          LOGICAL :: IFLX
!>        downward and upward measurement pixels are present in data 
          LOGICAL :: ICMB !combined_up_down
      END TYPE RT_SOS_CNTRL

      TYPE :: RT_SOS_RESET
          LOGICAL :: IGQ_F
          LOGICAL :: IGQ_D
          LOGICAL :: IGQ_BRM_FEXP
          LOGICAL :: IGQ_BRM_HSPH
      END TYPE RT_SOS_RESET

      TYPE(RT_SOS_SETUP) ::  RT_SOS_SET
      TYPE(RT_SOS_CNTRL) ::  RT_SOS_CTL
      TYPE(RT_SOS_RESET) ::  RT_SOS_RES

!.......................................................................
      CONTAINS
!.......................................................................
      SUBROUTINE SETUP_RT_SOS(RIN)
      USE mod_retr_settings_derived_type, ONLY : retr_input_settings
!
      TYPE(retr_input_settings), INTENT(IN) :: RIN
!
      RT_SOS_SET%IP_VERBOSE = RIN%IPRI_verbose
      RT_SOS_SET%IP_ADDITION= RIN%IPRI_additional_info
      RT_SOS_SET%ITRC       = RIN%ITRONC
      RT_SOS_SET%ISCA       = RIN%ipplane .EQ. 0
      RT_SOS_SET%IVEC       = RIN%DLSF%keyEL .EQ. 4
      RT_SOS_SET%IWUT       = RIN%IMODE_LUT .EQ. 1
      RT_SOS_SET%ILUT       = RIN%IMODE_LUT .EQ. 2
      RT_SOS_SET%IATM       = .FALSE.
      RT_SOS_SET%ICRR       = .FALSE.

      RT_SOS_SET%AER_PRF    =  RIN%aer_prof_type
      RT_SOS_SET%MOL_PRF    =  RIN%mol_prof_type
      RT_SOS_SET%NA         =  RIN%NSD
      RT_SOS_SET%NB         =  RIN%NBIN
      RT_SOS_SET%NLYR       =  RIN%NLYRS
      RT_SOS_SET%EPS        =  RIN%eps_err

      RT_SOS_SET%boa_ref	= RIN%boa_ref

      call cstring2fstring(RIN%DLSF%internal_file_path, RT_SOS_SET%INTL_PATH)
      call cstring2fstring(RIN%DLSF%external_file_path, RT_SOS_SET%EXTL_PATH)
      call cstring2fstring(RIN%LUT_path, RT_SOS_SET%LUT_path)

      RETURN
      END SUBROUTINE SETUP_RT_SOS
!.......................................................................

      SUBROUTINE RESET_RT_SOS ()
!
      RT_SOS_RES%IGQ_F        = .FALSE.
      RT_SOS_RES%IGQ_D        = .FALSE.
      RT_SOS_RES%IGQ_BRM_FEXP = .FALSE.
      RT_SOS_RES%IGQ_BRM_HSPH = .FALSE.
!
      RETURN
      END SUBROUTINE RESET_RT_SOS

      END MODULE MOD_RT_SOS_SETUP
