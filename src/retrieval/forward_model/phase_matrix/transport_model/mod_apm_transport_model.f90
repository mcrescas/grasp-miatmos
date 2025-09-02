!> @file grasp_apm_model_mod.f90
        !> File contains routines related to calculation of Aerosol Particle Microphysics
        !>
! APM - Aerosol Particle Microphysics
! function reff_discrete_sd
! function reff_lognormal_sd
! function rmean_lognormal_sd
! subroutine convert_lognormal_sd_parameters

module mod_apm_transport_model

      implicit none
contains
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine calculates effective radius for descrete particle
        !> @brief size distribution.
        !>
        !> @author Tatsiana Lapionak
        !> @date 5 MAY 2015
        !>
        !> @param[in]   id_sd  - 0 - number, 1 - radius, 2 - cross section, 3 - volume sd
        !> @param[in]   nbin   - number of radii equidistant in logarithm  
        !> @param[in]   radius - particle radii        
        !> @param[in]   sd     - particle size distribution      
        !> @param[out]  reff   - effective radius 
        
      function reff_discrete_sd( id_sd, nbin, sd, radius )
      
!      implicit none
!	----------------------------------------------------------------------
      integer, intent(in) :: id_sd, nbin
      real(4), dimension(nbin), intent(in) :: sd, radius
      real(4) :: reff_discrete_sd
!	----------------------------------------------------------------------
      real(4) :: pi
      real(4), dimension(nbin) :: sdn
      logical :: lstop = .false.
!	----------------------------------------------------------------------
!  R32(effective*radius) 
      pi = acos( -1.0)
      sdn(:) = 0.0
      reff_discrete_sd = -999.0

      select case(id_sd)
      case(0) ! number
        sdn(1:nbin) = sd(1:nbin)
      case(1) ! radius
        sdn(1:nbin) = sd(1:nbin)/radius(1:nbin)
      case(2) ! cross section
        sdn(1:nbin) = sd(1:nbin)/( pi*radius(1:nbin)**2 )
      case(3) ! volume
        sdn(1:nbin) = sd(1:nbin)*3./( 4.*pi*radius(1:nbin)**3 )
      case default 
        write(*,*) 'id_sd=',id_sd,' is not valid id_sd of aerosol particle size distribution'
        stop 'stop in reff_discrete_sd'       
      end select
      reff_discrete_sd = sum( sdn(1:nbin)*radius(1:nbin)**3 )/sum( sdn(1:nbin)*radius(1:nbin)**2 )

!      write(*,*) 'in reff_discrete_sd: reff =',reff_discrete_sd

      end function reff_discrete_sd
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine calculates effective radius for lognormal size distribution.
        !>
        !> @author Tatsiana Lapionak
        !> @date 5 MAY 2015
        !>
        !> @param[in]   id_sd  - 0 - number, 1 - radius, 2 - cross section, 3 - volume sd
        !> @param[in]   rm     - mean radius in micromiters
        !> @param[in]   sigma  - standard deviation      
        !> @param[out]  reff   - effective radius in micromiters
        
! rn - mean radius of number SD
! df/dlnr = 1./(ln(sigma)*sqrt(2.*PI)) *  &
!           exp(-(ln(r)-ln(rn))**2/(2.*ln(sigma)*ln(sigma)))
! reff = rn*exp(5.*ln(sigma)*ln(sigma)/2.)

      function reff_lognormal_sd( id_sd, sigma, rm )
      
!      implicit none
!	----------------------------------------------------------------------
      integer, intent(in) :: id_sd
      real(4), intent(in) :: sigma, rm
      real(4) :: reff_lognormal_sd
!	----------------------------------------------------------------------
      real(4) :: lnsigma, rn
!	----------------------------------------------------------------------
!  R32(effective*radius) 
      reff_lognormal_sd = -999.0
      lnsigma = log(sigma)
      
      select case(id_sd)
      case(0) ! number      
        rn   = rm
      case(1) ! radius   
        rn   = exp( log(rm) - lnsigma*lnsigma)
      case(2) ! cross section     
        rn   = exp( log(rm) - 2.0*lnsigma*lnsigma )
      case(3) ! volume     
        rn   = exp( log(rm) - 3.0*lnsigma*lnsigma )
      case default      
        write(*,*) 'id_sd=',id_sd,  &
        ' is not valid id_sd of aerosol particle size distribution'
        stop 'stop in reff_lognormal_SD' 
      end select
      reff_lognormal_sd = rn * exp( 5.0*lnsigma*lnsigma*0.5 )

!      write(*,*) 'in reff_lognormal_sd: reff =',reff_lognormal_sd

      end function reff_lognormal_sd

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine computes mean radius for lognormal size distribution and given reff and sigma.
        !>
        !> @author Tatsiana Lapionak
        !> @date 5 MAY 2015
        !>
        !> @param[in]   reff   - effective radius   
        !> @param[in]   sigma  - standard deviation      
        !> @param[out]  rm     - mean radius
        
! df/dlnr = 1. / (ln(sigma)*sqrt(2.*PI)) *  & 
!           exp(-(ln(r)-ln(rm))**2 / (2.*ln(sigma)*ln(sigma)))
! reff = rm * exp(5.*ln(sigma)*ln(sigma)/2.) 
! rm = reff / exp(5.*ln(sigma)*ln(sigma)/2.)

      function rmean_lognormal_sd( id_sd, sigma, reff )
      
!      implicit none
!	----------------------------------------------------------------------
      integer, intent(in) :: id_sd
      real(4), intent(in) :: sigma, reff
      real(4) :: rmean_lognormal_sd
!	----------------------------------------------------------------------
      real(4) :: lnsigma, rn, rm
!	----------------------------------------------------------------------
!  mean radius 
      rm = -999.0
      lnsigma = log( sigma )
      rn = reff / exp( 5.*lnsigma*lnsigma*0.5 )
      
      select case(id_sd)
      case(0) ! number      
        rm = rn
      case(1) ! radius   
        rm = exp( log(rn) + lnsigma*lnsigma )
      case(2) ! cross section     
        rm = exp( log(rn) + 2.*lnsigma*lnsigma )
      case(3) ! volume     
        rm = exp( log(rn) + 3.*lnsigma*lnsigma )
      case default      
        write(*,'(a,i2,a)') 'id_sd=',id_sd,  &
        ' is not valid id_sd of aerosol particle size distribution'
        stop 'stop in reff_lognormal_SD' 
      end select
      rmean_lognormal_sd = rm
!      write(*,*) 'in rmean_lognormal_sd: rm =',rm

      end function rmean_lognormal_sd

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine convertes concentration and parameters of lognormal size distribution
        !>
        !> @author Tatsiana Lapionak
        !> @date 26 JUL 2016
        !>
        !> @param[in] id1 - size distribution ID
        !> @param[in] c1 - concentration
        !> @param[in] lnsigma1  - ln(sigma1), sigma1 - standard deviation
        !> @param[in] rmean1 - mean radius in micrometers
        !> @param[in] id2 - size distribution ID
        !> @param[out] c2 - concentration
        !> @param[out] lnsigma2  - ln(sigma2), sigma2 - standard deviation
        !> @param[out] rmean2 - mean radius in micrometers

! df/dlnr = c / (ln(sigma)*sqrt(2.*PI)) *  &
!           exp(-(ln(r)-ln(rm))**2 / (2.*ln(sigma)*ln(sigma)))

! ID of Size Distribution :
! - id_sd = 0 number
! - id_sd = 1 radius
! - id_sd = 2 surface
! - id_sd = 3 volume

      subroutine convert_lognormal_sd_parameters( id_sd1, c1, lnsigma1, rmean1, &
                                                  id_sd2, c2, lnsigma2, rmean2 )
      implicit none
!	----------------------------------------------------------------------
      integer, intent(in) :: id_sd1
      real, intent(in) :: c1, lnsigma1, rmean1
      integer, intent(in) :: id_sd2
      real, intent(out) :: c2, lnsigma2, rmean2
!	----------------------------------------------------------------------
      integer :: id_sd0
      real :: c0, lnsigma0, rmean0 ! number SD parameters
      real :: pi, ss
!	----------------------------------------------------------------------
      pi = acos( -1.0 )
      lnsigma2 = lnsigma1
      ss = lnsigma1*lnsigma1
      c2 = -999.0
      rmean2 = -999.0

! Convert input parameners of SD to parameters of number SD
      call lognormal_number_sd_parameters( id_sd1, c1, lnsigma1, rmean1, &
                                           id_sd0, c0, lnsigma0, rmean0)

! Convert parameters of number SD to parameters of requested SD
      select case(id_sd2)
        case(0) ! number SD
            c2 = c0
            rmean2 = rmean0
        case(1) ! radius SD
            c2 = c0 * rmean0 * exp( 0.5*ss )
            rmean2 = exp( log( rmean0 ) + ss )
        case(2) ! surface SD
            c2 = c0 * 4.*pi*rmean0*rmean0 * exp( 2.0*ss )
            rmean2 = exp( log( rmean0 ) + 2.0*ss )
        case(3) ! volume SD
            c2 = c0 * 4.0/3.0 * pi*rmean0*rmean0*rmean0 * exp( 9.0/2.0*ss )
            rmean2 = exp( log( rmean0 ) + 3.0*ss )
      end select

      return
      end subroutine convert_lognormal_sd_parameters

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine convertes parameters of lognormal number/area/volume
        !> @brief Size Distribution to parameters of lognormal number SD
        !>
        !> @author Tatsiana Lapionak
        !> @date 11 SEP 2015
        !>
        !> @param[in] id1 - size distribution
        !> @param[in] c1 - concentration
        !> @param[in] lnsigma1  - ln(sigma1), sigma1 - standard deviation
        !> @param[in] rmean1 - mean radius in micrometers
        !> @param[out] id0 - number SD
        !> @param[out] c0 - concentration of number SD
        !> @param[out] lnsigma0  - ln(sigma2), sigma2 - standard deviation
        !> @param[out] rmean0 - mean radius in micrometers

! df/dlnr = c / (ln(sigma)*sqrt(2.*PI)) *  &
!           exp(-(ln(r)-ln(rm))**2 / (2.*ln(sigma)*ln(sigma)))

! ID of Size Distribution :
! - id_sd = 0 number
! - id_sd = 1 radius
! - id_sd = 2 surface
! - id_sd = 3 volume

      subroutine lognormal_number_sd_parameters( id_sd1, c1, lnsigma1, rmean1, &
                                                 id_sd0, c0, lnsigma0, rmean0 )
      implicit none
!	----------------------------------------------------------------------
      integer, intent(in) :: id_sd1
      real, intent(in) :: c1, lnsigma1, rmean1
      integer, intent(out) :: id_sd0
      real, intent(out) :: c0, lnsigma0, rmean0

!	----------------------------------------------------------------------
      real :: pi, ss
!	----------------------------------------------------------------------
      pi = acos( -1.0 )
      lnsigma0 = lnsigma1
      ss = lnsigma1*lnsigma1
      id_sd0 = 0

      select case(id_sd1)
        case(0) ! number SD
              rmean0 = rmean1
              c0 = c1
        case(1) ! radius SD
              rmean0 = exp( log(rmean1) - ss )
              c0 = c1 / ( rmean0 * exp( 0.5*ss ) )
        case(2) ! surface SD
              rmean0 = exp( log(rmean1) - 2.0*ss )
              c0 = c1 / ( 4.*pi*rmean0*rmean0 * exp( 2.0*ss ) )
        case(3) ! volume SD
              rmean0 = exp( log(rmean1) - 3.0*ss )
              c0 = c1 * 3.0/4.0 / ( pi*rmean0*rmean0*rmean0 * exp( 9.0/2.0*ss ) )
        case default
            write(*,'(a,i0,2x,a)') 'id_sd1 = ',id_sd1,  &
            'is not valid value ( 0 <= id_sd1 <= 3 ) for aerosol particle size distribution'
            stop 'stop in lognormal_number_sd_parameters'
      end select

      return
      end subroutine lognormal_number_sd_parameters

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

end module mod_apm_transport_model
