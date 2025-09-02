#include "../../../constants_set/mod_globals.inc"

module mod_hydroscopic_growth

      use mod_globals, only : sp, dp

      implicit none

      contains
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine calculates hydroscopic aerosol factor of aerosol effective radius.
        !>
        !> @author Tatsiana Lapionak
        !> @date 5 MAY 2015
        !>
        !> @param[in]   nlev - number of levels
        !> @param[in]   rh   - relative humidity [0,99]
        !> @param[in]   aero_type - particle mean radius        
        !> @param[out]  factor - hydroscopic growth factor of effective radius 
        
      subroutine reff_hydroscopic_growth_factor(nlev, rh, aero_type, factor)

      use mod_intrpl_linear
                 
!	----------------------------------------------------------------------
      integer, intent(in) :: nlev
      real(dp), dimension(nlev), intent(in) :: rh
      character(len=5), intent(in) :: aero_type
      real(sp), dimension(nlev), intent(out)  :: factor
!	----------------------------------------------------------------------
      integer, parameter :: nrh_grid = 36 ! UoL 28 ! Mian's paper 7 ! NASA-GSFC 36
      real(sp), dimension(nrh_grid) :: rh_grid, hgf_temp, &
                          hgf_oc2, hgf_bc2, &
                          hgf_ss1, hgf_ss2, hgf_ss3, hgf_ss4, hgf_ss5, &
                          hgf_su1, hgf_su2
      real(sp) :: rrh
      integer :: l
!	----------------------------------------------------------------------
      !data rh_grid/ 0.0_sp, 0.5_sp, 0.7_sp, 0.8_sp, 0.9_sp, 0.95_sp, 0.99_sp /
      !data hgf_su/ 1.0_sp, 1.4_sp, 1.5_sp, 1.6_sp, 1.8_sp, 1.9_sp, 2.2_sp /
      !data hgf_oc/ 1.0_sp, 1.2_sp, 1.4_sp, 1.5_sp, 1.6_sp, 1.8_sp, 2.2_sp /
      !data hgf_bc/ 1.0_sp, 1.0_sp, 1.0_sp, 1.2_sp, 1.4_sp, 1.5_sp, 1.9_sp /
      !data hgf_ss/ 1.0_sp, 1.6_sp, 1.8_sp, 2.0_sp, 2.4_sp, 2.9_sp, 4.8_sp /

! GEOS-chem: aerosol growth is capped at 90% RH.
! SUBROUTINE RDAER in aerosol_mod.f
! In this case we can use below data ???
      !data rh_grid/ 0.0_sp, 0.5_sp, 0.7_sp, 0.8_sp, 0.9_sp, 0.95_sp, 0.99_sp /
      !data hgf_su/ 1.0_sp, 1.4_sp, 1.5_sp, 1.6_sp, 1.8_sp, 1.8_sp, 1.8_sp /
      !data hgf_oc/ 1.0_sp, 1.2_sp, 1.4_sp, 1.5_sp, 1.6_sp, 1.6_sp, 1.6_sp /
      !data hgf_bc/ 1.0_sp, 1.0_sp, 1.0_sp, 1.2_sp, 1.4_sp, 1.4_sp, 1.4_sp /
      !data hgf_ss/ 1.0_sp, 1.6_sp, 1.8_sp, 2.0_sp, 2.4_sp, 2.4_sp, 2.4_sp /
!	----------------------------------------------------------------------
! NASA-GSFC
!RH
  data rh_grid /  0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, &
                  0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.81, 0.82, 0.83, &
                  0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.90, 0.91, 0.92, 0.93, &
                  0.94, 0.95, 0.96, 0.97, 0.98, 0.99 /
  data hgf_bc2 /  1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, &
                  1.00, 1.00, 1.01, 1.01, 1.03, 1.10, 1.19, 1.21, 1.23, 1.25, &
                  1.27, 1.30, 1.32, 1.34, 1.36, 1.38, 1.41, 1.43, 1.46, 1.48, &
                  1.52, 1.55, 1.59, 1.65, 1.72, 1.89 /
  data hgf_oc2 /  1.00, 1.02, 1.05, 1.07, 1.09, 1.12, 1.14, 1.17, 1.19, 1.21, &
                  1.24, 1.26, 1.29, 1.32, 1.34, 1.39, 1.44, 1.46, 1.47, 1.49, &
                  1.50, 1.52, 1.54, 1.56, 1.58, 1.61, 1.64, 1.67, 1.71, 1.76, &
                  1.81, 1.88, 1.97, 2.08, 2.25, 2.52 /

  data hgf_ss1 / 1.00,     1.06727, 1.0859,   1.10255, 1.11894,  1.13581, 1.15367,  1.17296, 1.19414,  1.21777,  &
                 1.24455,  1.27538, 1.31153,  1.35486, 1.40816,  1.47602, 1.56654,  1.58855, 1.61224,  1.63784,  &
                 1.66563,  1.69593, 1.72915,  1.7658,  1.80651,  1.85209, 1.90363,  1.96256, 2.03088,  2.11144,  &
                 2.20851,  2.32887, 2.32887,  2.32887, 2.32887,  2.32887 /
  data hgf_ss2 / 1.00,     1.07361, 1.09389,  1.11197, 1.12975,  1.14802, 1.16733,  1.18815, 1.211,    1.23645,  &
                 1.26525,  1.29837, 1.33717,  1.38363, 1.44076,  1.51349, 1.61059,  1.63423, 1.65969,  1.68723,  &
                 1.71715,  1.74981, 1.78567,  1.82529, 1.86939,  1.9189,  1.97505,  2.03949, 2.11457,  2.20366,  &
                 2.31192,  2.44772, 2.44772,  2.44772, 2.44772,  2.44772 /
  data hgf_ss3 / 1.00,     1.08286, 1.10546,  1.12555, 1.14524,  1.16542, 1.18669,  1.20957, 1.2346,   1.26241,  &
                 1.29379,  1.32979, 1.37183,  1.42202, 1.48354,  1.56161, 1.66552,  1.69077, 1.71796,  1.74734,  &
                 1.77925,  1.81407, 1.85228,  1.89447, 1.94142,  1.99412, 2.05386,  2.12245, 2.20235,  2.29722,  &
                 2.41262,  2.55759, 2.55759,  2.55759, 2.55759,  2.55759 /
  data hgf_ss4 / 1.00,     1.08977, 1.11408,  1.13563, 1.1567,   1.17825, 1.20093,  1.22528, 1.25186,  1.28134,  &
                 1.31453,  1.35252, 1.39681,  1.44956, 1.51408,  1.59575, 1.70419,  1.7305,  1.75882,  1.78941,  &
                 1.8226,   1.85881, 1.89852,  1.94235, 1.9911,   2.04577, 2.10774,  2.17883, 2.26162,  2.35986,  &
                 2.4793,   2.6293,  2.6293,   2.6293,  2.6293,   2.6293 /
  data hgf_ss5 / 1.00,     1.09795, 1.12424,  1.14749, 1.17016,  1.1933,  1.21758,  1.2436,  1.27195,  1.30332,  &
                 1.33857,  1.37882, 1.42563,  1.48126, 1.54912,  1.63482, 1.7483,   1.77579, 1.80536,  1.83728,  &
                 1.87191,  1.90965, 1.95103,  1.99667, 2.04739,  2.10426, 2.16867,  2.24252, 2.32847,  2.4304,   &
                 2.55425,  2.7097,  2.7097,   2.7097,  2.7097,   2.7097 /

  data hgf_su1 / 1.00,     1.0400,  1.0800,   1.1200,  1.1600,   1.2000,  1.2300,   1.2700,  1.3100,   1.3500,   &
                 1.3900,   1.4300,  1.4600,   1.5000,  1.5400,   1.5900,  1.6400,   1.6500,  1.6600,   1.6700,   &
                 1.6800,   1.6900,  1.7100,   1.7200,  1.7400,   1.7500,  1.7700,   1.7900,  1.8200,   1.8400,   &
                 1.8700,   1.9100,  1.9400,   1.9900,  2.0500,   2.1600 /
  data hgf_su2 / 1.00,     1.0205,  1.0423,   1.0656,  1.0907,   1.1178,  1.1473,   1.1795,  1.2149,   1.2544,   &
                 1.2986,   1.3489,  1.4069,   1.4751,  1.5573,   1.6595,  1.7926,   1.8245,  1.8587,   1.8955,   &
                 1.9352,   1.9784,  2.0255,   2.0773,  2.1346,   2.1986,  2.2708,   2.3533,  2.4488,   2.5617,   &
                 2.6982,   2.8688,  3.0920,   3.4050,  3.8998,   4.9161 /
!	----------------------------------------------------------------------
! UoL
!!RH (%)	0.00	5.00	10.00	15.00	20.00	25.00	30.00	35.00	40.00	45.00	50.00	55.00	60.00	65.00	70.00	75.00	80.00	81.00	82.00	83.00	84.00	85.00	86.00	87.00	88.00	89.00	90.00	91.00
!  data rh_grid / 0.00,  0.50,  0.10,  0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, &
!                 0.50,  0.55,  0.60,  0.65, 0.70, 0.75, 0.80, 0.81, 0.82, 0.83, &
!                 0.84,  0.85,  0.86,  0.87, 0.88, 0.89, 0.90, 0.91 /
!  data hgf_bc2 / 1.00,	1.00,	 1.00,	1.00,	1.00,	1.00,	1.00,	1.00,	1.00,	1.00,	&
!                 1.00,	1.00,	 1.01,	1.01,	1.03,	1.10,	1.19,	1.21,	1.23,	1.25,	&
!                 1.27,	1.30,	 1.32,	1.34,	1.36,	1.38,	1.41,	1.43 /
!  data hgf_oc2 / 1.00,	1.02,  1.05,	1.07,	1.09,	1.12,	1.14,	1.17,	1.19,	1.21,	&
!                 1.24,	1.26,	 1.29,	1.32,	1.34,	1.39,	1.44,	1.46,	1.47,	1.49,	&
!                 1.50,	1.52,	 1.54,	1.56,	1.58,	1.61,	1.64,	1.67 /
!!ss mode 1
!  data hgf_ss1 /	1.00,	1.07,	 1.09,	1.11,	1.13,	1.15,	1.17,	1.19,	1.21,	1.24,	&
!                  1.27,	1.30,	 1.34,	1.38,	1.44,	1.51,	1.61,	1.63,	1.66,	1.69,	&
!                  1.72,	1.75,	 1.79,	1.83,	1.87,	1.92,	1.98,	2.04 /
!  data hgf_ss2 /  1.00, 1.07,  1.09,  1.11, 1.13, 1.15, 1.17, 1.19, 1.21, 1.24, &
!                  1.27, 1.30,  1.34,  1.38, 1.44, 1.51, 1.61, 1.63, 1.66, 1.69, &
!                  1.72, 1.75,  1.79,  1.83, 1.87, 1.92, 1.98, 2.04 /
!  data hgf_ss3 /  1.00, 1.07,  1.09,  1.11, 1.13, 1.15, 1.17, 1.19, 1.21, 1.24, &
!                  1.27, 1.30,  1.34,  1.38, 1.44, 1.51, 1.61, 1.63, 1.66, 1.69, &
!                  1.72, 1.75,  1.79,  1.83, 1.87, 1.92, 1.98, 2.04 /
!!ss mode 2
!  data hgf_ss4 /	1.00,	1.09,	 1.11,	1.13,	1.15,	1.17,	1.20,	1.22,	1.25,	1.27,	&
!                  1.31,	1.34,	 1.39,	1.44,	1.50,	1.59,	1.69,	1.72,	1.75,	1.77,	&
!                  1.81,	1.84,	 1.88,	1.92,	1.97,	2.03,	2.09,	2.16 /
!  data hgf_ss5 /  1.00, 1.09,  1.11,  1.13, 1.15, 1.17, 1.20, 1.22, 1.25, 1.27, &
!                  1.31, 1.34,  1.39,  1.44, 1.50, 1.59, 1.69, 1.72, 1.75, 1.77, &
!                  1.81, 1.84,  1.88,  1.92, 1.97, 2.03, 2.09, 2.16 /

!  data hgf_su1 /	1.00,	1.04,	 1.08,	1.12,	1.16,	1.20,	1.23,	1.27,	1.31,	1.35,	&
!                  1.39,	1.43,	 1.46,	1.50,	1.54,	1.59,	1.64,	1.65,	1.66,	1.67,	&
!                  1.68,	1.69,	 1.71,	1.72,	1.74,	1.75,	1.77,	1.79 /
!	----------------------------------------------------------------------

      factor(:) = -999.0_sp

      select case(aero_type(1:3))
      case('bc2') ! Black carbon
        hgf_temp(:) = hgf_bc2(:)
      case('oc2') ! Organic carbon
        hgf_temp(:) = hgf_oc2(:)
      case('ss1') ! Sea salt
        hgf_temp(:) = hgf_ss1(:)
      case('ss2') ! Sea salt
        hgf_temp(:) = hgf_ss2(:)
      case('ss3') ! Sea salt
        hgf_temp(:) = hgf_ss3(:)
      case('ss4') ! Sea salt
        hgf_temp(:) = hgf_ss4(:)
      case('ss5') ! Sea salt
        hgf_temp(:) = hgf_ss5(:)
      case('su1') ! Sulfate
        hgf_temp(:) = hgf_su1(:)
      case('su2') ! Sulfate
        hgf_temp(:) = hgf_su2(:)
      case default
        write(*,'(a,a3,a)') 'aero_type =',aero_type,'  is not valid aero_type'
        stop 'stop in reff_hydroscopic_growth_factor' 
      end select

      loop_layer : do l=1, nlev
        rrh = rh(l)
        factor(l) = linear(rh_grid(1:nrh_grid), hgf_temp(1:nrh_grid), nrh_grid, rrh )
      end do  loop_layer    

      end subroutine reff_hydroscopic_growth_factor
      
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

        !> @brief Routine calculates aerosol particle characteristic modified with relative humidity.
        !>
        !> @author Tatsiana Lapionak
        !> @date 5 MAY 2015
        !>
        !> @param[in]   xd - dry particle characteristic
        !> @param[in]   xw - water characteristic
        !> @param[in]   fd - dry particle fraction     
        !> @param[out]  x  - particle characteristic modified with relative humidity
        
      function rh_modified_particle_characteristic(xd, xw, fd)

!	----------------------------------------------------------------------
      real(sp), intent(in) :: xd, xw, fd
      real(sp) :: rh_modified_particle_characteristic
!	----------------------------------------------------------------------
      rh_modified_particle_characteristic = fd*xd + (1.0_sp-fd)*xw

      end function rh_modified_particle_characteristic

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

end module mod_hydroscopic_growth


