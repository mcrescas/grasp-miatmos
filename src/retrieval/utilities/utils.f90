! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
        !> @brief Module contains routines to check if array elements are in ascending order
        !>
        !> @param[in]  n - size of arrays
        !> @param[in]  array - values of elements
        !>
        !> @author Tatsiana Lapionak
        !> @date 07 FEB 2018
        !>

module mod_ascending_order_array_check
integer, parameter :: sp = kind(1.0)               !> sp - single precision
integer, parameter :: dp = kind(1.0d0)             !> sp - double precision
public :: ascending_order_array_check
private :: sp, dp
private :: D_ascending_order_array_check, R_ascending_order_array_check, I_ascending_order_array_check
interface ascending_order_array_check
  module procedure d_ascending_order_array_check, r_ascending_order_array_check, i_ascending_order_array_check
end interface ascending_order_array_check
contains

logical function D_ascending_order_array_check ( n, array )

      implicit none
! ----------------------------------------------------------------------
      integer, intent(in) :: n
      real (dp), dimension(:), intent(in) :: array
! ----------------------------------------------------------------------
      integer :: i, i1
! ----------------------------------------------------------------------
      D_ascending_order_array_check = .true.
      do i=2,n
        if ( array(i) .le. array(i-1) ) then
          D_ascending_order_array_check = .false.
          write(*,'(3x,a,5x,a)') 'i','array(i)'
          do i1=1,n
            write(*,'(i0,e14.7)') i1,array(i)
          enddo ! i1
        exit
        endif
      enddo ! i

end function D_ascending_order_array_check

logical function R_ascending_order_array_check ( n, array )

      implicit none
! ----------------------------------------------------------------------
      integer, intent(in) :: n
      real (sp), dimension(:), intent(in) :: array
! ----------------------------------------------------------------------
      integer :: i, i1
! ----------------------------------------------------------------------
      R_ascending_order_array_check = .true.
      do i=2,n
        if ( array(i) .le. array(i-1) ) then
          R_ascending_order_array_check = .false.
          write(*,'(3x,a,5x,a)') 'i','array(i)'
          do i1=1,n
            write(*,'(i0,e14.7)') i1,array(i)
          enddo ! i1
        exit
        endif
      enddo ! i

end function R_ascending_order_array_check

logical function I_ascending_order_array_check ( n, array )

      implicit none
! ----------------------------------------------------------------------
      integer, intent(in) :: n
      integer, dimension(:), intent(in) :: array
! ----------------------------------------------------------------------
      integer :: i, i1
! ----------------------------------------------------------------------
      I_ascending_order_array_check = .true.
      do i=2,n
        if ( array(i) .le. array(i-1) ) then
          I_ascending_order_array_check = .false.
          write(*,'(3x,a,5x,a)') 'i','array(i)'
          do i1=1,n
            write(*,'(i0,e14.7)') i1,array(i)
          enddo ! i1
        exit
        endif
      enddo ! i

end function I_ascending_order_array_check

end module mod_ascending_order_array_check

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
        !> @brief Module contains routines to check if all nodes
        !> @brief from second array are coincident with nodes from first array
        !>
        !> @param[in]  n1 - size of array1
        !> @param[in]  array1 - values of array1 elements
        !> @param[in]  n2 - size of array2
        !> @param[in]  array2 - values of array2 elements
        !>
        !> @author Tatsiana Lapionak
        !> @date 24 APR 2018
        !>

module mod_nodes_coincidence_check
integer, parameter :: sp = kind(1.0)               !> sp - single precision
integer, parameter :: dp = kind(1.0d0)             !> dp - double precision
public :: nodes_coincidence_check
private :: sp, dp
private :: D_nodes_coincidence_check, R_nodes_coincidence_check, I_nodes_coincidence_check
interface nodes_coincidence_check
  module procedure d_nodes_coincidence_check, r_nodes_coincidence_check, i_nodes_coincidence_check
end interface nodes_coincidence_check
contains

logical function D_nodes_coincidence_check ( n1, array1, n2, array2 )

      implicit none
! ----------------------------------------------------------------------
      integer, intent(in) :: n1, n2
      real (dp), dimension(:), intent(in) :: array1, array2
! ----------------------------------------------------------------------
      integer :: i1, i2
      real (dp) :: tiny, tmp
! ----------------------------------------------------------------------
      tiny = 0.0001_dp

      do i2=1,n2
        D_nodes_coincidence_check = .false.
        do i1=1,n1
          tmp = abs((array2(i2)-array1(i1))/array2(i2))
          if ( tmp .lt. tiny ) then
          D_nodes_coincidence_check = .true.
          exit
          endif
        enddo ! i1
        if (.not. D_nodes_coincidence_check) return
      enddo ! i2

end function D_nodes_coincidence_check

logical function R_nodes_coincidence_check ( n1, array1, n2, array2 )

      implicit none
! ----------------------------------------------------------------------
      integer, intent(in) :: n1, n2
      real (sp), dimension(:), intent(in) :: array1, array2
! ----------------------------------------------------------------------
      integer :: i1, i2
      real (sp) :: tiny, tmp
! ----------------------------------------------------------------------
      tiny = 0.0001_sp

      do i2=1,n2
        R_nodes_coincidence_check = .false.
        do i1=1,n1
          tmp = abs((array2(i2)-array1(i1))/array2(i2))
          if ( tmp .lt. tiny ) then
          R_nodes_coincidence_check = .true.
          exit
          endif
        enddo ! i1
        if (.not. R_nodes_coincidence_check) return
      enddo ! i2

end function R_nodes_coincidence_check

logical function I_nodes_coincidence_check ( n1, array1, n2, array2 )

      implicit none
! ----------------------------------------------------------------------
      integer, intent(in) :: n1, n2
      integer, dimension(:), intent(in) :: array1, array2
! ----------------------------------------------------------------------
      integer :: i1, i2
      real (sp) :: tiny, tmp
! ----------------------------------------------------------------------
      tiny = 0.0001_sp

      do i2=1,n2
        I_nodes_coincidence_check = .false.
        do i1=1,n1
          tmp = abs((real(array2(i2))-real(array1(i1)))/real(array2(i2)))
          if ( tmp .lt. tiny ) then
          I_nodes_coincidence_check = .true.
          exit
          endif
        enddo ! i1
        if (.not. I_nodes_coincidence_check) return
      enddo ! i2

end function I_nodes_coincidence_check

end module mod_nodes_coincidence_check

! mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
        !> @brief Routine returns a value of integrated in given scattering angle limits phase function
        !>
        !> @param[in]   nsd - number of components of particles
        !> @param[in]   sca - scattering coefficients
        !> @param[in]   nang - number of scattering angles for both X1 and Y1 arrays
        !> @param[in]   angles - set of scattering angles
        !> @param[in]   p11 - phase function array
        !> @param[in]   limits - angle limits for integration
        !> @param[out]  f11_intd - integrated phase function
        !>
        !> @author Tatsiana Lapionak
        !> @date 11 SEP 2020

      subroutine integrated_f11(nsd, sca, nang, angles, p11, ang1, ang2, f11_intd)

      use mod_globals, only : sp, pi_sp
      use mod_intrpl_linear, only : LINEAR

      implicit none
! ......................................................................
      integer, intent(in) :: nsd, nang
      real(sp), dimension(nsd), intent(in) :: sca
      real(sp), dimension(nang), intent(in) :: angles
      real(sp), dimension(nang,nsd), intent(in) :: p11
      real(sp), intent(in) :: ang1, ang2
      real(sp), intent(out) :: f11_intd
! ......................................................................
      integer, parameter :: nsimps_par = 721
      integer :: nsimps
      integer :: i
      real(sp), dimension(nang):: f11_total
      real(sp) :: xh ! xh - angle step (degree)
      real(sp) :: x, y
      real (sp) :: simps_coeff
! ......................................................................

      do i=1,nang
      f11_total(i) = log( sum( sca(1:nsd) * p11(i,1:nsd) ) )
      enddo

      xh = (ang2-ang1) / float(nsimps_par-1)
      !nsimps = int( (ang2-ang1)/xh + 1.0_sp )
      nsimps = nsimps_par

      f11_intd = 0.0_sp
      x = ang1
      do i=1,nsimps
        y = exp(LINEAR(angles, f11_total, nang, x))
        f11_intd = f11_intd + y*sin(x*pi_sp/180.0_sp)*simps_coeff(nsimps,i)
        if(i .eq. nsimps) then
          !write(*,'(3es16.8,a)') ang2,x,(x-ang2)/ang2,'  - ang2,x,(x-ang2)/ang2'
          if (abs(ang2-x)/ang2 .gt. 1e-4) then
            write(*,'(4es16.8,a)') ang2,ang1,x,abs(x-ang2)/ang2, &
            '  - ang2,ang1,x,abs(x-ang2)'
            write(*,'(a,es16.8,a)') 'abs(x-ang2)/ang2 =',abs(x-ang2)/ang2,' > 1e-4'
            stop 'stop in integrated_f11'
          else
            x = ang2
          endif
        endif
        x = x + xh
      enddo ! i
      f11_intd = 0.5_sp * f11_intd * (xh*pi_sp/180.0_sp)

      return
      end subroutine integrated_f11

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine returns Simpson's rule coefficient for numerical integration
        !> 
        !> @param[in]    nsimps - number of points for numerical integration
        !> @param[in]    i - order index
        !>

      function simps_coeff ( nsimps, i )

      use mod_globals, only : sp

      implicit none
! ----------------------------------------------------------------------
      integer, intent(in) :: nsimps, i
      real (sp) :: simps_coeff
! ----------------------------------------------------------------------
      simps_coeff = 0.0_sp
      if(i .eq. 1 .or. i .eq. nsimps) then
        simps_coeff = 1.0_sp/3.0_sp
      else
        select case(mod(i,2))
        case (0) ! i even number
          simps_coeff = 4.0_sp/3.0_sp
        case (1) ! i odd  number
          simps_coeff = 2.0_sp/3.0_sp
        case default
          write(*,'(a,i0,2x,a)') 'mod(i,2) = ',mod(i,2),'(possible values 0 or 1)'
        end select
      endif

! delete after testing
      if(simps_coeff .eq. 0.0_sp) stop 'stop in simps_coeff: simps_coeff = 0.'

      end function simps_coeff

! ****************************************************************

