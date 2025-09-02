#include "../constants_set/mod_globals.inc"
! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

module mod_functional_retrieval

        integer,parameter ::	retr_method_fullset = 1  ! all parameters
        integer,parameter ::	retr_method_subset  = 2  ! subset of parameters

        integer,parameter ::	retr_function_const  = 1      ! constant
        integer,parameter ::	retr_function_lns_linear = 2  ! linear in lns

      contains
      
      !subroutine dummy_subroutine_for_avoiding_warnings_in_an_empty_module()
      !end subroutine dummy_subroutine_for_avoiding_warnings_in_an_empty_module

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Returns characteristic for given node depend on given function
      subroutine get_functional_retrieval_parameter( RIN, idim1, idim2, nx, X, Y, ipar, Y1 )

      use mod_retr_settings_derived_type
      use mod_par_inv, only : KIDIM3
      use mod_stop_report

      implicit none
! ............................................................................
      type(retr_input_settings), intent(in) :: RIN
      integer, intent(in) :: idim1, idim2
      integer, intent(in) :: nx
      real, dimension(nx) :: X
      real, dimension(KIDIM3), intent(in) :: Y ! set of retrieved parameters
      integer, intent(in) :: ipar ! index of parameter in full set of characteristic parameters
      real, intent(out) :: Y1 ! parameter for given node
! ............................................................................
      integer :: ndim3
      integer :: i
! ............................................................................

      select case ( RIN%FRETR%function(IDIM2,IDIM1) )
      case ( retr_function_const )
        call function_const( RIN, idim1, idim2, nx, X, Y, ipar, Y1 )
      case ( retr_function_lns_linear )
        call function_lns_linear( RIN, idim1, idim2, nx, X, Y, ipar, Y1 )
      case default
        write(tmp_message,'(2(a,i0,2x),2a,i0,2x,a)') &
        'For characteristic # ',idim1,'mode # ',idim2, &
        NEW_LINE('A'), &
        'retrieval function = ',RIN%FRETR%function(IDIM2,IDIM1), &
        'is not a valid value.'
        G_ERROR(trim(tmp_message))
      end select

      end subroutine get_functional_retrieval_parameter

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Returns characteristic for given node for const function in lns function
      subroutine function_const( RIN, idim1, idim2, nx, X, Y, ipar, Y1 )

      use mod_retr_settings_derived_type, only : retr_input_settings
      use mod_par_inv, only : KW, KIDIM3
      use mod_stop_report

      implicit none
! ............................................................................
      type(retr_input_settings), intent(in) :: RIN
      integer, intent(in) :: idim1, idim2
      integer, intent(in) :: nx
      real, dimension(nx) :: X
      real, dimension(KIDIM3), intent(in) :: Y
      integer, intent(in) :: ipar ! index of parameter in full set of characteristic parameters
      real, intent(out) :: Y1 ! parameter at iwl-th wavelwngth
! ............................................................................
      integer :: ndim3, nwl
      integer :: i, iretr(KIDIM3)
! ............................................................................
      Y1 = 0.0
      ndim3 = RIN%NDIM%n3(idim2,idim1)
      iretr(1:ndim3) = RIN%FRETR%ipar(1:ndim3,idim2,idim1)

      Y1 = Y(1)

 goto 10

      select case ( RIN%FRETR%method(idim2,idim1) )
      case ( retr_method_subset )
        ! Retrieved parameter for given ipar index
        do i=1,ndim3
        if ( iretr(i) .eq. ipar ) then
        Y1 = Y(i)
        return
        endif
        enddo
        ! Calculated parameter for given ipar index
        if ( ipar .lt. iretr(1) ) then
        ! X(ipar) < X(1)
        Y1 = Y(1)
        return
        elseif ( ipar .gt. iretr(ndim3) ) then
        ! X(ipar) > X(ndim3)
        Y1 = Y(ndim3)
        return
        ! X(1) < X(ipar) < X(ndim3)
        elseif ( ipar .gt. iretr(1) .and. ipar .lt. iretr(ndim3) ) then
        do i=1,ndim3-1
        if ( ipar .gt. iretr(i) .and. ipar .lt. iretr(i+1)  ) then
        Y1 = Y(i)
        return
        endif
        enddo
        endif
      case default
        write(tmp_message,'(a,f7.4,2(2x,a,i0),a)') &
        'i-th parameter =',X(ipar), &
        'characteristic # ',idim1,'mode # ',idim2,  &
        NEW_LINE('A'), &
        'retrieval method is not valid.'
        G_ERROR(trim(tmp_message))
      end select

10 continue

      if ( Y1 .eq. 0. ) then
        write(tmp_message,'(a,f7.4,2(2x,a,i0),a)') &
        'i-th parameter =',X(ipar), &
        'characteristic # ',idim1,'mode # ',idim2,  &
        NEW_LINE('A'), &
        'function is not defined.'
        G_ERROR(trim(tmp_message))
      endif

      end subroutine function_const

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Returns characteristic for given node for linear in lns function
      subroutine function_lns_linear( RIN, idim1, idim2, nx, X, Y, ipar, Y1 )

      use mod_retr_settings_derived_type, only : retr_input_settings
      use mod_par_inv, only : KW, KIDIM3
      use mod_stop_report

      implicit none
! ............................................................................
      type(retr_input_settings), intent(in) :: RIN
      integer, intent(in) :: idim1, idim2
      integer, intent(in) :: nx
      real, dimension(nx) :: X
      real, dimension(KIDIM3), intent(in) :: Y
      integer, intent(in) :: ipar ! index of parameter in full set of characteristic parameters
      real, intent(out) :: Y1 ! parameter at iwl-th wavelwngth
! ............................................................................
      integer :: ndim3, nwl
      real :: AE
      integer :: i, iretr(KIDIM3)
! ............................................................................
      Y1 = 0.0
      ndim3 = RIN%NDIM%n3(idim2,idim1)
      iretr(1:ndim3) = RIN%FRETR%ipar(1:ndim3,idim2,idim1)

      select case ( RIN%FRETR%method(idim2,idim1) )
      case ( retr_method_subset )
        ! Retrieved parameter for given ipar index
        do i=1,ndim3
        if ( iretr(i) .eq. ipar ) then
        Y1 = Y(i)
        return
        endif
        enddo
        ! Calculated parameter for given ipar index
        if ( ipar .lt. iretr(1) ) then
        ! X(ipar) < X(1)
        AE = log( Y(1)/Y(2) ) / log( X(iretr(1))/X(iretr(2)) )
        Y1 = Y(1) * (X(ipar)/X(iretr(1)))**AE
        return
        elseif ( ipar .gt. iretr(ndim3) ) then
        ! X(ipar) > X(ndim3)
        AE = log( Y(ndim3-1)/Y(ndim3) ) / log( X(iretr(ndim3-1))/X(iretr(ndim3)) )
        Y1 = Y(ndim3) * (X(ipar)/X(iretr(ndim3)))**AE
        return
        elseif ( ipar .gt. iretr(1) .and. ipar .lt. iretr(ndim3) ) then
        ! X(1) < X(ipar) < X(ndim3)
        do i=1,ndim3-1
        if ( ipar .gt. iretr(i) .and. ipar .lt. iretr(i+1)  ) then
        AE = log( Y(i)/Y(i+1) ) / log( X(iretr(i))/X(iretr(i+1)) )
        Y1 = Y(i) * (X(ipar)/X(iretr(i)))**AE
        return
        endif
        enddo
        endif
      case default
        write(tmp_message,'(a,f7.4,2(2x,a,i0),a)') &
        'i-th parameter =',X(ipar), &
        'characteristic # ',idim1,'mode # ',idim2,  &
        NEW_LINE('A'), &
        'retrieval method is not valid.'
        G_ERROR(trim(tmp_message))
      end select

      if ( Y1 .eq. 0. ) then
        write(tmp_message,'(a,f7.4,2(2x,a,i0),a)') &
        'i-th parameter =',X(ipar), &
        'characteristic # ',idim1,'mode # ',idim2,  &
        NEW_LINE('A'), &
        'function is not defined.'
        G_ERROR(trim(tmp_message))
      endif

      end subroutine function_lns_linear

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

end module mod_functional_retrieval


