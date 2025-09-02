#include "../../../constants_set/mod_globals.inc"

module mod_derivative_type_tracer_average

      use mod_globals, only : sp
      use mod_par_inv, only : KVERTM
      use mod_par_DLS, only : KMpar

      implicit none
!	..................................................................................
! Transport model: tracer-level structure
      type :: tracer_average_scamx
        integer :: nlev
        ! scattering matrix
        real(sp)  ::  ext(KVERTM)
        real(sp)  ::  sca(KVERTM)
        real(sp)  ::  ssa(KVERTM)
        real(sp),dimension(KMpar,KVERTM) ::  ph11,  ph12,  ph22,  &
                                           ph33,  ph34,  ph44
      end type tracer_average_scamx

      type(tracer_average_scamx) :: TRCAVG
      
!	..................................................................................

      contains

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine initialize_tracer_average_sca_matrix ( TRCAVG )

      implicit none
!  .................................................................................
      type(tracer_average_scamx), intent(inout) :: TRCAVG
!  .................................................................................

      TRCAVG%nlev = 0
      TRCAVG%ext(:) = 0._sp
      TRCAVG%sca(:) = 0._sp
      TRCAVG%ssa(:) = 0._sp
      TRCAVG%ph11(:,:) = 0._sp
      TRCAVG%ph12(:,:) = 0._sp
      TRCAVG%ph22(:,:) = 0._sp
      TRCAVG%ph33(:,:) = 0._sp
      TRCAVG%ph34(:,:) = 0._sp
      TRCAVG%ph44(:,:) = 0._sp

      return
      end subroutine initialize_tracer_average_sca_matrix

      !subroutine dummy_subroutine_for_avoiding_warnings_in_an_empty_module()
      !end subroutine dummy_subroutine_for_avoiding_warnings_in_an_empty_module

end module mod_derivative_type_tracer_average


