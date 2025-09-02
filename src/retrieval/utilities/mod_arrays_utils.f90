! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine compare elements of two (real) arrays with nonzero elements
        !> 
        !> @param[in]    n - size of arrays
        !> @param[in]    array1
        !> @param[in]    array2
        !>

module mod_equal_arrays_check
integer, parameter :: sp = kind(1.0)               !> single precision
integer, parameter :: dp = kind(1.0d0)             !> double precision
public :: equal_arrays_check
private :: sp, dp
private :: D_equal_arrays_check, R_equal_arrays_check, I_equal_arrays_check
interface equal_arrays_check
  module procedure d_equal_arrays_check, r_equal_arrays_check, i_equal_arrays_check
end interface equal_arrays_check
contains

logical function D_equal_arrays_check(n, array1, array2)

      implicit none
! ......................................................................
      integer, intent(in) :: n
      real (dp), dimension(:), intent(in) :: array1, array2
! ......................................................................
      integer :: i
      real (dp), parameter :: tiny = 0.0001_dp
! ......................................................................
      D_equal_arrays_check = .false.

      do i=1,n
        if ( abs(array1(i)) .eq. 0.0_dp .or. abs(array2(i)) .eq. 0.0_dp) then
          if( abs(array1(i)-array2(i)) .le. tiny ) &
          D_equal_arrays_check = .true.
        else
          if (abs((array1(i)-array2(i))/array1(i)) .le. tiny) then
          D_equal_arrays_check = .true.
          else
          D_equal_arrays_check = .false.
          return
          endif
        endif
      enddo

end function D_equal_arrays_check

logical function R_equal_arrays_check(n, array1, array2)

      implicit none
! ......................................................................
      integer, intent(in) :: n
      real (sp), dimension(:), intent(in) :: array1, array2
! ......................................................................
      integer :: i
      real (sp), parameter :: tiny = 0.0001_sp
! ......................................................................
      R_equal_arrays_check = .false.

      do i=1,n
        if ( abs(array1(i)) .eq. 0.0_sp .or. abs(array2(i)) .eq. 0.0_sp) then
          if( abs(array1(i)-array2(i)) .le. tiny ) &
          R_equal_arrays_check = .true.
        else
          if (abs((array1(i)-array2(i))/array1(i)) .le. tiny) then
          R_equal_arrays_check = .true.
          else
          R_equal_arrays_check = .false.
          return
          endif
        endif
      enddo

end function R_equal_arrays_check

logical function I_equal_arrays_check(n, array1, array2)

      implicit none
! ......................................................................
      integer, intent(in) :: n
      integer, dimension(:), intent(in) :: array1, array2
! ......................................................................
      integer :: i
      real (sp) :: tmp
      real (sp), parameter :: tiny=0.0001_sp
! ......................................................................
      I_equal_arrays_check = .false.

      do i=1,n
        if ( abs(array1(i)) .eq. 0 .or. abs(array2(i)) .eq. 0) then
          if( abs(array1(i)-array2(i)) .eq. 0 ) &
          I_equal_arrays_check = .true.
        else
          if (abs((real(array1(i))-real(array2(i)))/real(array1(i))) .le. tiny) then
          I_equal_arrays_check = .true.
          else
          I_equal_arrays_check = .false.
          return
          endif
        endif
      enddo

end function I_equal_arrays_check

end module mod_equal_arrays_check

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
