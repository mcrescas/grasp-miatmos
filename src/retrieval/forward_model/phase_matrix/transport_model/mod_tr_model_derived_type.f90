#include "../../../constants_set/mod_globals.inc"

module mod_derivative_type_transport_model

      use mod_globals, only : sp, GBL_FILE_PATH_LEN
      use mod_par_inv, only : KIDIM2, KVERTM, KIMAGE, KW
      use mod_par_OS,  only : KSD

      implicit none
!	..................................................................................
      integer, parameter :: hphi = 1, hpho = 0
      integer, parameter :: column_average = 1, tracer_average = 2
!	..................................................................................
    type water_properties
        real(sp) :: density ! [kg/m^3]
        integer :: nwl     ! number of wave lengths
        real(sp) :: wl(KW) ! wave length values (um)
        real(sp) :: reri(KW)
        real(sp) :: imri(KW)
    end type water_properties

! Transport model structure
      type :: transport_model

        ! flag to average phase matrix (1 - column average, 2 - tracer average)
        integer flag_av_vprof
        ! number of tracers
        integer ntrc
        ! tracer names
        character(len=5) trcs(KIDIM2)
        ! flag of hydrophilic tracers (0/1)
        integer flag_hphi(KIDIM2)
        ! density of dry tracers
        real(sp) density_dry(KIDIM2)
        ! number of levels
        integer :: nlev
        ! relative humidity (%) profile values at each layer
        real(sp) :: rh(KVERTM,KIMAGE) ! KVERTM ???
        ! hight of transport model grid box
        real(sp) :: h(KVERTM,KIMAGE)
        ! concentrations of dry tracers
        real(sp) :: c_dry(KVERTM,KSD,KIMAGE)
        ! vertical profile (values of volume concentrations of tracers)
        real(sp) :: vp(KVERTM,KSD,KIMAGE)

        real(sp) :: density_rh(KVERTM,KSD,KIMAGE) ! density modified by relative humidity
        real(sp) :: hgfactor(KVERTM,KSD,KIMAGE) ! humidity growth factor for effective radius

        type(water_properties) :: water

        character(len=GBL_FILE_PATH_LEN) :: tracer_average_phmx_file_path

      end type transport_model

      type(transport_model) :: TM
      
!	..................................................................................

      contains

      subroutine initialize_transport_model ( TM )

      implicit none
!  ..................................................................................
      type(transport_model), intent(inout) :: TM
!  ..................................................................................

      TM%flag_av_vprof = -1
      TM%ntrc = 0
      TM%trcs(:) = ''
      TM%flag_hphi(:) = -1
      TM%density_dry(:) = 0
      TM%nlev = 0
      TM%RH(:,:) = 0.
      TM%H(:,:) = 0.
      TM%c_dry(:,:,:) = 0.
      TM%tracer_average_phmx_file_path = ''

      return
      end subroutine initialize_transport_model

      !subroutine dummy_subroutine_for_avoiding_warnings_in_an_empty_module()
      !end subroutine dummy_subroutine_for_avoiding_warnings_in_an_empty_module

end module mod_derivative_type_transport_model


