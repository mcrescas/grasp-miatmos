! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

!
!	mod_molecular_scattering.f90
!	contains routines to calculate molecular extintion and backscatter
! coefficients for the different atmosfere models
!
!	Developed by Anton Lopatin on 7/14/15.
!	
!
module mod_molecular_scattering
  
!    use lidar_par
      REAL, PARAMETER       :: LRM=8.739225! MOLECULAR LIDAR RATIO	
	
      contains


! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! this subroutine copied here from mod_os_2009_m.f90

      subroutine rayleia_gas(wavel,h,LAT,TAURA)
! This soubroutine calculates molecualr extinction for given
! altitude, lattitude and wavelength
!INput parameters:
!   wavel — wavelength [mum]
!   h — altitude of the measurement [m]
!   LAT — lattitude in decimal degrees
!OUTput parameters
!   TAURA — molecular optical thickness

! ----------------------------------------------------------------
      REAL*8 :: twofi,zc,costwofi,costwofi2,F,wr,wr2,   &
                          n360,n2,n21,n22
      REAL*8 :: h,LAT
      REAL :: wavel
      real   :: TAURA
! ----------------------------------------------------------------
!Cs      write(6,*)'wavel,h,LAT',wavel,h,LAT
      twofi = LAT*0.03490658504
      costwofi = cos(twofi)
      costwofi2 = costwofi * costwofi
      wr=1./(wavel*wavel)
      wr2=wr*wr
      zc=0.73737*h+5517.56
      F=0.78084*(1.034+3.17e-4*wr)+0.20946*   &
      (1.096+1.385e-3*wr+1.448e-4*wr2)+0.009754
      n360=8.06077e-5+2.481070e-2/(132.274-wr)+1.74563e-4/    &
      (39.32957-wr)+1
      n2=n360*n360
      n21=(n2-1)
      n22=1./(n2+2)
      TAURA=241675320.0752709888768463125378*n21*n21*F*n22*n22*wr2 *  &
      exp(-1.188e-4*h-1.16e-9*h*h)/        &
      (980.6160*( 1.-0.002637*costwofi+0.0000059*costwofi2)-        &
      (3.085462e-4+2.27e-7*costwofi)*zc+    &
      (7.254e-11+1.0e-13*costwofi)*zc*zc    &
      -(1.517e-17+6.0e-20*costwofi)*zc*zc*zc)
      !write(*,*),'wave=',wavel,'tau=',TAURA,'  - in rayleia'

      ! ! MIKE : Added for debugging
      ! TAURA = 0.0
      return
      end subroutine rayleia_gas





!ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! this subroutine copied here from mod_os_2009_m.f90

      subroutine rayleia(wavel,h,LAT,TAURA)
! This soubroutine calculates molecualr extinction for given
! altitude, lattitude and wavelength
!INput parameters:
!   wavel — wavelength [mum]
!   h — altitude of the measurement [m]
!   LAT — lattitude in decimal degrees
!OUTput parameters
!   TAURA — molecular optical thickness

! ----------------------------------------------------------------
      REAL*8 :: twofi,zc,costwofi,costwofi2,F,wr,wr2,   &
                          n360,n2,n21,n22
      REAL*8 :: wavel,h,LAT
      real   :: TAURA
! ----------------------------------------------------------------  
!Cs      write(6,*)'wavel,h,LAT',wavel,h,LAT
      twofi = LAT*0.03490658504
      costwofi = cos(twofi)
      costwofi2 = costwofi * costwofi
      wr=1./(wavel*wavel)
      wr2=wr*wr
      zc=0.73737*h+5517.56
      F=0.78084*(1.034+3.17e-4*wr)+0.20946*   &
      (1.096+1.385e-3*wr+1.448e-4*wr2)+0.009754
      n360=8.06077e-5+2.481070e-2/(132.274-wr)+1.74563e-4/    &
      (39.32957-wr)+1
      n2=n360*n360
      n21=(n2-1)
      n22=1./(n2+2)
      TAURA=241675320.0752709888768463125378*n21*n21*F*n22*n22*wr2 *  &
      exp(-1.188e-4*h-1.16e-9*h*h)/        &
      (980.6160*( 1.-0.002637*costwofi+0.0000059*costwofi2)-        &
      (3.085462e-4+2.27e-7*costwofi)*zc+    &
      (7.254e-11+1.0e-13*costwofi)*zc*zc    &
      -(1.517e-17+6.0e-20*costwofi)*zc*zc*zc)
      !write(*,*) 'wave=',wavel,'tau=',TAURA,'  - in rayleia'

      ! MIKE : Added for debugging
      ! TAURA = 0.0
      ! return
      end subroutine rayleia

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      SUBROUTINE STD_ATM_DENSITY ( NH,H,   & !IN
                                   PROF )    !OUT
!XH   Profile of molecule scattering based on the US standard atmosphere
      use mod_intrpl_linear
      IMPLICIT NONE
!-----------------------------------------------------------------------------------------
! IN
      INTEGER,                         INTENT(IN) :: NH
      REAL,             DIMENSION(NH), INTENT(IN) :: H
!-----------------------------------------------------------------------------------------
! OUT
      DOUBLE PRECISION, DIMENSION(NH), INTENT(OUT) :: PROF
!-----------------------------------------------------------------------------------------
! LOCAL
      INTEGER, PARAMETER :: NH_STD = 51
      REAL, DIMENSION(NH_STD), PARAMETER :: H_STD = (/      -0.5,       0.0,       1.0,       2.0,       3.0,       4.0,  &
                                                             5.0,       6.0,       7.0,       8.0,       9.0,  &
                                                            10.0,      11.0,      12.0,      13.0,      14.0,  &
                                                            15.0,      16.0,      17.0,      18.0,      19.0,  &
                                                            20.0,      21.0,      22.0,      23.0,      24.0,  &
                                                            25.0,      27.5,      30.0,      32.5,      35.0,  &
                                                            37.5,      40.0,      42.5,      45.0,      47.5,  &
                                                            50.0,      55.0,      60.0,      65.0,      70.0,  &
                                                            75.0,      80.0,      85.0,      90.0,      95.0,  &
                                                           100.0,     105.0,     110.0,     115.0,     120.0/)
      REAL, DIMENSION(NH_STD), PARAMETER :: P_STD = (/ 1.040E+03, 1.013E+03, 8.988E+02, 7.950E+02, 7.012E+02, 6.166E+02,  &
                                                       5.405E+02, 4.722E+02, 4.111E+02, 3.565E+02, 3.080E+02,  &
                                                       2.650E+02, 2.270E+02, 1.940E+02, 1.658E+02, 1.417E+02,  &
                                                       1.211E+02, 1.035E+02, 8.850E+01, 7.565E+01, 6.467E+01,  &
                                                       5.529E+01, 4.729E+01, 4.047E+01, 3.467E+01, 2.972E+01,  &
                                                       2.549E+01, 1.743E+01, 1.197E+01, 8.010E+00, 5.746E+00,  &
                                                       4.150E+00, 2.871E+00, 2.060E+00, 1.491E+00, 1.090E+00,  &
                                                       7.978E-01, 4.250E-01, 2.190E-01, 1.090E-01, 5.220E-02,  &
                                                       2.400E-02, 1.050E-02, 4.460E-03, 1.840E-03, 7.600E-04,  &
                                                       3.200E-04, 1.450E-04, 7.100E-05, 4.010E-05, 2.540E-05/)
      REAL, DIMENSION(NH_STD), PARAMETER :: T_STD = (/    288.20,    288.20,    281.70,    275.20,    268.70,    262.20,  &
                                                          255.70,    249.20,    242.70,    236.20,    229.70,  &
                                                          223.30,    216.80,    216.70,    216.70,    216.70,  &
                                                          216.70,    216.70,    216.70,    216.70,    216.70,  &
                                                          216.70,    217.60,    218.60,    219.60,    220.60,  &
                                                          221.60,    224.00,    226.50,    230.00,    236.50,  &
                                                          242.90,    250.40,    257.30,    264.20,    270.60,  &
                                                          270.70,    260.80,    247.00,    233.30,    219.60,  &
                                                          208.40,    198.60,    188.90,    186.90,    188.40,  &
                                                          195.10,    208.80,    240.00,    300.00,    360.00/)
      INTEGER :: I
!-----------------------------------------------------------------------------------------
      PROF = 0.D0
      DO I = 1, NH
         IF (H(I) .LT. -0.5) THEN
            PROF(I) = P_STD(1)/T_STD(1)
         ELSE IF (H(I) .GT. H_STD(NH_STD)) THEN
            PROF(I) = P_STD(NH_STD)/T_STD(NH_STD)
         ELSE
            PROF(I) = LINEAR_LN(H_STD(1:NH_STD),P_STD(1:NH_STD),NH_STD,H(I))  &
                    / LINEAR(H_STD(1:NH_STD),T_STD(1:NH_STD),NH_STD,H(I))
         END IF
      END DO
      PROF=PROF/PROF(NH)

      ! MIKE: ADDED FOR DEBUGGING
      ! PROF = 1e-25
      RETURN
      END SUBROUTINE STD_ATM_DENSITY

      END MODULE mod_molecular_scattering
	  
	
	
