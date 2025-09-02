! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **
#include "../constants_set/mod_globals.inc"

module mod_mixture_ref_index_chem

use mod_stop_report
use mod_globals, only  : GBL_FILE_PATH_LEN
use mod_par_OS,  only  : NMG, N_CHEM_MAX, KSD, N_RR_MAX

implicit none

TYPE CHEM_LUT
  CHARACTER(LEN=GBL_FILE_PATH_LEN)                            :: NAME
  integer                                                     :: NWL
  real,                           DIMENSION (N_RR_MAX)        :: WL
  real,                           DIMENSION (N_RR_MAX)        :: RRI
  real,                           DIMENSION (N_RR_MAX)        :: IRI
END TYPE CHEM_LUT

type(CHEM_LUT), DIMENSION(:,:),    ALLOCATABLE  :: LUT_CHEM
logical                                         :: read_chem_lut

contains


SUBROUTINE alloc_chem_lut(NSD,nfract)

implicit none

! -------------------------------
! IN :
integer,                              intent(in)      :: NSD, nfract
! -------------------------------
! LOCAL :
integer :: ierr, ISD, ifract

allocate ( LUT_CHEM(NSD,nfract), stat=ierr )
  if ( ierr/=0 ) then
  write(tmp_message,'(a)') 'Can not allocate LUT_CHEM(ISD,nfract) array.'
  G_ERROR(trim(tmp_message))
  endif
  do ISD=1,NSD
    do ifract=1,nfract
        LUT_CHEM(ISD,ifract)%WL(:)  = 0.0
        LUT_CHEM(ISD,ifract)%RRI(:) = 0.0
        LUT_CHEM(ISD,ifract)%IRI(:) = 0.0
    end do
  end do

return
END SUBROUTINE alloc_chem_lut


SUBROUTINE MIXING(RIN,WAVE,ISD,rh_mxtr,       & !IN
                  nfract,vfract,              &
                  RREAL_out,RIMAG_out)
  use mod_intrpl_linear
  use mod_par_OS,  only  : N_CHEM_MAX
  use mod_retr_settings_derived_type
  use mod_c_utils, only: cstring2fstring
  implicit none 

! -------------------------------
! IN :
  type(retr_input_settings),            intent(in)      :: RIN
  real(selected_real_kind(p=15)),                                 intent(in)      :: rh_mxtr      ! Valid range: 0--1
  REAL,                                 intent(in)      :: WAVE
  integer,                              intent(in)      :: ISD, nfract
! -------------------------------
! INOUT :
  real(selected_real_kind(p=15)), dimension(N_CHEM_MAX),          intent(inout)   :: vfract
! -------------------------------
! OUT :
  real,                                                            intent(out)     :: RREAL_out, RIMAG_out
! -------------------------------
! LOCAL :
  integer                                               :: IW, j
!tl  integer                           i_rh
  character(30)                                         :: slbl_aersl_mtrl  !  'ammnm_ntrt'  'ammnm_slft'
  character(30)                                         :: aersl_mtrl       ! iron or quartz
  real(selected_real_kind(p=15))                        :: refr_chem, refi_chem
  complex,dimension(N_CHEM_MAX)                         :: refindx_incl
  real(selected_real_kind(p=15))                        :: RREAL, RIMAG

  call cstring2fstring(RIN%chemistry%soluble, slbl_aersl_mtrl)


    do j=1,nfract-2
        !MHG Interpolate REFI LUT:
        refr_chem =  LINEAR(LUT_CHEM(ISD,j)%WL(1:LUT_CHEM(ISD,j)%NWL),LUT_CHEM(ISD,j)%RRI(1:LUT_CHEM(ISD,j)%NWL),LUT_CHEM(ISD,j)%NWL,WAVE)
        refi_chem =  LINEAR(LUT_CHEM(ISD,j)%WL(1:LUT_CHEM(ISD,j)%NWL),LUT_CHEM(ISD,j)%IRI(1:LUT_CHEM(ISD,j)%NWL),LUT_CHEM(ISD,j)%NWL,WAVE)
        refindx_incl(j) = cmplx(refr_chem, refi_chem)
    end do


    call frwrd_mdl (WAVE,                              &  !IN
                    slbl_aersl_mtrl, refindx_incl,     &
                    rh_mxtr,                           &
                    nfract,                            &  !OUT
                    vfract,                            &
                    RREAL, RIMAG)

!MH The internal calculations of mixing routine needs to be calculated in double precision
!MH but the rest of the code is written in single precision. This avoids memory problems.
RREAL_out = REAL(RREAL)
RIMAG_out = REAL(RIMAG)


end subroutine MIXING







subroutine frwrd_mdl (WAVE,                                 &
                      slbl_aersl_mtrl, refindx_incl,        &
                      rh_mxtr,                              &
                      nfract,                               &
                      vfract,                               &
                      RREAL, RIMAG)


  ! Written by Greg Schuster; initially distributed on 10/15/2008.
  ! This subroutine computes the refractive index of a 4-component aerosol mixture. 

  ! Ambient RH (rh_mxtr), fraction of insoluble aerosols (fract_inslbl), and BC fraction (fract_soot) 
  ! can be tuned to obtain a desired refractive index. 

  ! RH is used to compute the refractive index of the host solution (water and ammonium nitrate, for example)
  ! using partial molar theory (Tang papers). Once the refractive index of the host has been computed, the refractive
  ! index of the 4-component mixture is computed using the Maxwell-Garnett equations. 

  use mod_par_OS,  only  : N_CHEM_MAX
  implicit none

! -------------------------------
! IN :
  real,                              intent(in)   ::  WAVE
  integer,                           intent(in)   ::  nfract
  character(*),                      intent(in)   :: slbl_aersl_mtrl

  complex,dimension(N_CHEM_MAX),     intent(in)   :: refindx_incl

  real(selected_real_kind(p=15)),                              intent(in)   :: rh_mxtr   ! Valid
! -------------------------------
! INOUT :
  real(selected_real_kind(p=15)), dimension(N_CHEM_MAX),       intent(inout) :: vfract
! -------------------------------
! OUT :
  real(selected_real_kind(p=15)),                               intent(out)     :: RREAL, RIMAG
! -------------------------------
! LOCAL :
  integer   ::  i_wave,  i_rhmin
  real(selected_real_kind(p=15))      ::  refr_host, refi_host
  real(selected_real_kind(p=15))      ::  host_slbl_volfrac  ! Volume Fraction of host solution composed of soluble inclusions
  real(selected_real_kind(p=15))      ::  host_slbl_masfrac  ! Mass   Fraction of host solution composed of soluble inclusions
  complex   ::  refindx_host
  complex   ::  refindx_mxtr
  

  ! Note:
  ! fract_slbl + fract_wtr + frac_soot + fract_inslbl = 1
  ! host_slbl_volfrac                                 = fract_slbl / (fract_slbl + fract_wtr)


  ! Require RH above efflorescence:
  !  ammnm_slft: rh >= 0.37
  !  ammnm_ntrt: rh >= 0.28
  !    sea_salt: rh >= 0.47

   call  get_rhmin (slbl_aersl_mtrl, i_rhmin)

   if (rh_mxtr < i_rhmin/1000.) then
      write(6,*) 'FATAL: rh_mxtr < rh_min... continue w/o processing this RH'
      write(6,*) '       rh_mxtr: ', rh_mxtr
      write(6,*) '  i_rhmin/1000: ', i_rhmin/1000.
      stop
   end if


   call solution_refindx_V2 (rh_mxtr, WAVE,         & !IN
                             slbl_aersl_mtrl,       &
                             refr_host,             & !OUT
                             refi_host,             &
                             host_slbl_masfrac,     &
                             host_slbl_volfrac)

    refindx_host = cmplx(refr_host, refi_host)

    call avg_refindx_Maxwell_Garnet(nfract,           & !IN
                                    vfract,           &
                                    refindx_incl,     &
                                    refindx_host,     &
                                    refindx_mxtr)       !OUT


    RREAL =  REAL(refindx_mxtr)
    RIMAG =  AIMAG(refindx_mxtr)
    !fract_slbl =        host_slbl_volfrac  * (1.0 - fract_soot - fract_inslbl)
    !fract_wtr  = (1.0 - host_slbl_volfrac) * (1.0 - fract_soot - fract_inslbl)
    
    vfract(nfract-1) =  (1.0 - host_slbl_volfrac) * (1.0 - SUM(vfract(1:nfract-2))) !MH Water
    vfract(nfract)  = host_slbl_volfrac  * (1.0 - SUM(vfract(1:nfract-2))) !MH Soluble

end subroutine frwrd_mdl



subroutine get_rhmin (incl_aersl_mtrl, i_rhmin)

  ! This subroutine provides the minimum rh (x10) above crystallization for several aerosol species.

  implicit none

  ! Global variables 
  ! ... input ...
  character(*) incl_aersl_mtrl
  ! ... output ...
  integer i_rhmin
  


  if     (trim(incl_aersl_mtrl) == 'ammnm_slft') then
     i_rhmin = 370  
  elseif (trim(incl_aersl_mtrl) == 'ammnm_ntrt') then
     i_rhmin = 280
  elseif (trim(incl_aersl_mtrl) == 'sea_salt') then
     i_rhmin = 470
  else
     write(6,*) 'WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING'
     write(6,*) 
     write(6,*) 'incl_aersl_mtrl = '//incl_aersl_mtrl//' unrecognized by subroutines mixture_refindx_tm_host and get_rh_min'
     write(6,*) '                   ...assuming that this aerosol species is insoluble, and using i_rhmin=0'
     i_rhmin = 0
  endif


end subroutine get_rhmin


subroutine solution_refindx_V2 (rh, WAVE, slbl_aersl_mtrl, refr_solution, refi_solution, solute_masfrac, solute_volfrac)

  implicit none
! -------------------------------
! IN :
  real(selected_real_kind(p=15)),                      intent(in)  :: rh
  real,                      intent(in)  :: WAVE
  character(*),              intent(in)  :: slbl_aersl_mtrl
! -------------------------------
! OUT :
  real(selected_real_kind(p=15)),                      intent(out) :: refr_solution, refi_solution
  real(selected_real_kind(p=15)),                      intent(out) :: solute_masfrac, solute_volfrac
! -------------------------------
! LOCAL :
  real(selected_real_kind(p=15))     ::  dnsty_solute ! Density of soluble aerosol
  real(selected_real_kind(p=15))     ::  dnsty_sltn, wl_um   ! Density of solution

  wl_um = WAVE

  refr_solution  = 0.0
  refi_solution  = 0.0
  solute_masfrac = 0.0
  solute_volfrac = 0.0

  if     (trim(slbl_aersl_mtrl) == 'ammnm_slft') then
     call ammso4 (rh, wl_um, refr_solution, refi_solution, solute_masfrac)
     dnsty_solute = 0.9971 + (5.92e-3)                - (5.036e-5)                   + (1.024e-8)
     dnsty_sltn   = 0.9971 + (5.92e-3)*solute_masfrac - (5.036e-5)*solute_masfrac**2 + (1.024e-8)*solute_masfrac**3

!  else
elseif (trim(slbl_aersl_mtrl) == 'ammnm_ntrt') then
     call ammno3 (rh, wl_um, refr_solution, refi_solution, solute_masfrac)
     dnsty_solute = 0.9971 + (4.05e-3)                - (9.000e-6)
     dnsty_sltn   = 0.9971 + (4.05e-3)*solute_masfrac - (9.000e-6)*solute_masfrac**2

!
!  elseif (trim(slbl_aersl_mtrl) == 'sea_salt') then
!     call seasal (rh, wvln_um, refr_solution, refi_solution, solute_masfrac)
!     dnsty_solute = 0.9971 + (7.4100e-3) - (3.741e-5) + (2.2252e-6) - (2.060e-8)
!     dnsty_sltn   = 0.9971 + (7.4100e-3)*solute_masfrac     - (3.741e-5)*solute_masfrac**2 &
!                           + (2.2252e-6)*solute_masfrac**3  - (2.060e-8)*solute_masfrac**4

  else
     write(6,*) 'FATAL in subroutine solution_refindx; invalid slbl_aersl_mtrl: ',slbl_aersl_mtrl
     stop
  endif



  solute_volfrac = solute_masfrac*dnsty_sltn/dnsty_solute



end subroutine solution_refindx_V2



subroutine avg_refindx_Maxwell_Garnet(nfract,           &
                                      vfract,           &
                                      refindx_incl,     &
                                      refindx_host,     &
                                      refindx_avg)

    use mod_par_OS,  only  : N_CHEM_MAX
    implicit none

    ! -------------------------------
    ! IN :
    integer,                         intent(in)   :: nfract
    real(selected_real_kind(p=15)),dimension(N_CHEM_MAX),      intent(in)   :: vfract
    complex,dimension(N_CHEM_MAX),   intent(in)   :: refindx_incl
    complex,                         intent(in)   :: refindx_host                      ! refractive index of host matrix
    ! -------------------------------
    ! OUT :
    complex,                         intent(out)  :: refindx_avg                       ! avg refractive index of composite
    ! -------------------------------
    ! LOCAL :
    complex,dimension(N_CHEM_MAX)  :: diel_incl
    complex                        :: diel_host                ! dielectric constant of host matrix
    complex                        :: diel_avg                 ! average dielectric constant
    integer                        ::   j


    do j=1, nfract-2
        call refindx_to_dielectric (refindx_incl(j), diel_incl(j))
    end do

    call refindx_to_dielectric (refindx_host, diel_host)

    call avg_dielectric_n (nfract, vfract, diel_incl, diel_host, diel_avg)

    call dielectric_to_refindx (diel_avg, refindx_avg  )

    return

end subroutine avg_refindx_Maxwell_Garnet


subroutine refindx_to_dielectric (refindx, dielectric)

  ! Written 3/29/01 by GLS
  ! Given the complex refractive index, this subroutine computes
  ! the complex dielectric fcn. Ref Bohren & Huffman, p 227.

  implicit none 
! -------------------------------
! IN :
  complex,         intent(in)   :: refindx    ! Refractive index
! -------------------------------
! OUT :
  complex,         intent(out)   :: dielectric ! Dielectric constant
! -------------------------------
! LOCAL :
  real(selected_real_kind(p=15)) :: refr,  refi  ! Real and imaginary refractive index
  real(selected_real_kind(p=15)) :: dielr, dieli ! Real and imaginary dielectric constants

  ! Get real and imaginary components of refractive index
  refr =  real(refindx) 
  refi = aimag(refindx) 
  
  ! Calculate the real and imaginary components of dielectric constants
  dielr = refr**2 - refi**2
  dieli = 2.*refr*refi

  dielectric = cmplx(dielr, dieli)



end subroutine refindx_to_dielectric





subroutine dielectric_to_refindx (dielectric, refindx) 


  ! Written 3/29/01 by GLS
  ! This program converts complex dielectric constants to complex
  ! refractive index. Ref Bohren and Huffman, p 227.

  implicit none
! -------------------------------
! IN :
  complex,        intent(in)   :: dielectric ! Complex dielectric function
! -------------------------------
! OUT :
  complex,        intent(out)   :: refindx    ! Complex refractive index
! -------------------------------
! LOCAL :
  real(selected_real_kind(p=15)) ::  dielr, dieli  ! Complex dielectric function
  real(selected_real_kind(p=15)) ::  refr,  refi   ! Complex refractive index


  ! Get real and imaginary components of dielectric function
  dielr =  real(dielectric) 
  dieli = aimag(dielectric) 

  ! Calculate real and imaginary components of refractive index
  refr = (((dielr**2 + dieli**2)**0.5 + dielr) / 2.)**0.5
  refi = (((dielr**2 + dieli**2)**0.5 - dielr) / 2.)**0.5

  refindx = cmplx(refr, refi) 

end subroutine dielectric_to_refindx



subroutine avg_dielectric_n(nfract,       &
                            vfract,       &
                            diel_incl,    &
                            diel_host,    &
                            diel_avg)

  ! Written 3/29/01 by GLS
  ! This subroutine computes the average complex dielectric
  ! function for a 2-component mixture of "inclusions" embedded
  ! in an otherwise homogeneous matrix using Maxwell-Garnett.
  ! Ref Bohren and Huffman, Sec 8.5, p213, and Eq. (8.50).

  use mod_par_OS,  only  : N_CHEM_MAX

  implicit none
  
! -------------------------------
! IN :
  integer,                                  intent(in) :: nfract
  real(selected_real_kind(p=15)),            dimension(N_CHEM_MAX),   intent(in) :: vfract    ! Volume fraction of embedded inclusions
  complex,         dimension(N_CHEM_MAX) ,  intent(in) :: diel_incl ! Dielectric constant of embedded inclusions
  complex,                                  intent(in) :: diel_host ! Dielectric constant of homogeneous matrix
! -------------------------------
! OUT :
  complex,                                  intent(out):: diel_avg  ! Average dielectric constant of the mixture
! -------------------------------
! LOCAL :
  complex, dimension(N_CHEM_MAX) :: ratio
  integer                        :: j

    do j=1, nfract-2
        ratio(j)    = (diel_incl(j) - diel_host) / (diel_incl(j) + 2.*diel_host)
        ratio(j)    = ratio(j) * vfract(j)
    end do


  diel_avg = diel_host * (1. + 3.*(SUM(ratio(1:nfract-2))) / (1. - SUM(ratio(1:nfract-2)))    )

end subroutine avg_dielectric_n


subroutine linear_near_neighbor_fit_dp_index (num_knowns, xknown, yknown, &
                                        num_fits,   xfit,   yfit, i_high_out)

  ! Written 12/00 by GLS
  ! For a given number (num_knowns) of values (xknown, yknown), this program
  ! interpolates between these values with a linear fit at a desired
  ! number (num_fits) of abscissas (xfit). If unknown values are located
  ! outside of the range of known values, the slope of the nearest 2 points
  ! are extrapolated. Output is yfit.

  ! NOTE: This is not a linear regression, but rather a linear interpolation
  ! between nearest neighbors.

  implicit none

! -------------------------------
! IN :
  integer,                      intent(in)  ::  num_knowns ! # known abscissas and ordinates
  integer,                      intent(in)  ::  num_fits   ! Number of desired interpolations
  real, dimension(num_knowns),  intent(in)  :: xknown     ! Known abscissas
  real, dimension(num_knowns),  intent(in)  :: yknown     ! Known ordinates
  real, dimension(num_fits),    intent(in)  :: xfit       ! Abscissas of desired interps
! -------------------------------
! OUT :
  real, dimension(num_fits),    intent(out) :: yfit       ! Interpolated ordinates (at xfit)
  integer, dimension(num_fits), intent(out) :: i_high_out ! MH Indexes of the original grid closest to new values
! -------------------------------
! LOCAL :
  integer   :: i
  integer   :: ifit       ! Index of desired interpolations
  integer   :: i_high     ! Index of 1st known abscissa > than desired abscissa
  real      :: xdum       ! Scalar representation of a desired abscissa
  real      :: ydum       ! Scalar representation of a desired ordinate
  real, dimension(num_knowns) :: xknown_dum, yknown_dum !


   i_high_out = 0
  ! Check to be sure that abscissa values are in ascending order:
  if (xknown(num_knowns) < xknown(1)) then
     write(6,*) "SUBROUTINE LINEAR_FIT: "
     write(6,*) "xknown found to be in descending order."
     write(6,*) "This is OK.....Reassigning...."
     write(6,*)
     do i = 1,num_knowns
        xknown_dum(i) = xknown(num_knowns - (i-1))
        yknown_dum(i) = yknown(num_knowns - (i-1))
     enddo
  else
     xknown_dum = xknown
     yknown_dum = yknown
  endif



  ! Linear interpolation/extrapolation:
  yfit = 0.0
  do ifit = 1, num_fits
     
     ! Determine the location of the desired abscissas within
     ! the known abscissas
     i_high = 1

    if(num_knowns .eq. num_fits) then
         do while ( (xknown_dum(i_high) < xfit(ifit)) .and. (i_high < num_knowns) )
            i_high = i_high + 1
         enddo
    else

        do while ( (xknown_dum(i_high) < xfit(ifit)) .and. (i_high <= num_knowns) )
           i_high = i_high + 1
        enddo

    end if


     xdum = xfit(ifit)
     ! If the abscissa is located outside of the known span of abscissas,
     ! extrapolate slope of interior points. Otherwise, interpolate
     ! between nearest neighbors.
     if ( (i_high == 1) .or. (i_high == num_knowns+1) ) then
        call extrapolate_linear_fit_dp &
             (xdum, num_knowns, i_high, xknown_dum, yknown_dum, ydum)
     else
        call interpolate_linear_fit_dp (xdum, i_high, xknown_dum, yknown_dum, ydum)
     endif
     i_high_out(ifit) = i_high
     yfit(ifit) = ydum

  end do

end subroutine linear_near_neighbor_fit_dp_index



subroutine linear_near_neighbor_fit_dp (num_knowns, xknown, yknown, &
                                        num_fits,   xfit,   yfit    )

  ! Written 12/00 by GLS
  ! For a given number (num_knowns) of values (xknown, yknown), this program 
  ! interpolates between these values with a linear fit at a desired 
  ! number (num_fits) of abscissas (xfit). If unknown values are located
  ! outside of the range of known values, the slope of the nearest 2 points
  ! are extrapolated. Output is yfit.

  ! NOTE: This is not a linear regression, but rather a linear interpolation
  ! between nearest neighbors.

  implicit none

! -------------------------------
! IN :
  integer,                      intent(in)  ::  num_knowns ! # known abscissas and ordinates
  integer,                      intent(in)  ::  num_fits   ! Number of desired interpolations
  real, dimension(num_knowns),  intent(in)  :: xknown     ! Known abscissas
  real, dimension(num_knowns),  intent(in)  :: yknown     ! Known ordinates
  real, dimension(num_fits),    intent(in)  :: xfit       ! Abscissas of desired interps
! -------------------------------
! OUT :
  real, dimension(num_fits),    intent(out) :: yfit       ! Interpolated ordinates (at xfit)
! -------------------------------
! LOCAL :
  integer   :: i
  integer   :: ifit       ! Index of desired interpolations
  integer   :: i_high     ! Index of 1st known abscissa > than desired abscissa
  real      :: xdum       ! Scalar representation of a desired abscissa
  real      :: ydum       ! Scalar representation of a desired ordinate
  real, dimension(num_knowns) :: xknown_dum, yknown_dum !



  ! Check to be sure that abscissa values are in ascending order:
  if (xknown(num_knowns) < xknown(1)) then
     write(6,*) "SUBROUTINE LINEAR_FIT: "
     write(6,*) "xknown found to be in descending order."
     write(6,*) "This is OK.....Reassigning...."
     write(6,*) 
     do i = 1,num_knowns
        xknown_dum(i) = xknown(num_knowns - (i-1))
        yknown_dum(i) = yknown(num_knowns - (i-1))
     enddo
  else
     xknown_dum = xknown
     yknown_dum = yknown
  endif



  ! Linear interpolation/extrapolation:
  yfit = 0.0
  do ifit = 1, num_fits
     
     ! Determine the location of the desired abscissas within 
     ! the known abscissas
     i_high = 1

    if(num_knowns .eq. num_fits) then
         do while ( (xknown_dum(i_high) < xfit(ifit)) .and. (i_high < num_knowns) )
            i_high = i_high + 1
         enddo
    else

        do while ( (xknown_dum(i_high) < xfit(ifit)) .and. (i_high <= num_knowns) )
           i_high = i_high + 1
        enddo

    end if


     xdum = xfit(ifit) 
     ! If the abscissa is located outside of the known span of abscissas, 
     ! extrapolate slope of interior points. Otherwise, interpolate 
     ! between nearest neighbors.
     if ( (i_high == 1) .or. (i_high == num_knowns+1) ) then
        call extrapolate_linear_fit_dp &
             (xdum, num_knowns, i_high, xknown_dum, yknown_dum, ydum)
     else 
        call interpolate_linear_fit_dp (xdum, i_high, xknown_dum, yknown_dum, ydum)
     endif
     yfit(ifit) = ydum

  end do


end subroutine linear_near_neighbor_fit_dp


subroutine interpolate_linear_fit_dp (xdum, i_high, xknown, yknown, ydum)

  implicit none
! -------------------------------
! IN :
  real,               intent(in)  ::  xdum
  integer,            intent(in)  ::  i_high
  real, dimension(*), intent(in)  :: xknown
  real, dimension(*), intent(in)  :: yknown
! -------------------------------
! OUT :
  real,               intent(out)  :: ydum
! -------------------------------
! LOCAL :
  real                            :: slope


  slope = (yknown(i_high) - yknown(i_high-1)) / & 
          (xknown(i_high) - xknown(i_high-1))

  ydum = yknown(i_high) + slope*(xdum - xknown(i_high))

end subroutine interpolate_linear_fit_dp





subroutine extrapolate_linear_fit_dp &
     (xdum, num_knowns, i_high, xknown, yknown, ydum)


  implicit none
! -------------------------------
! IN :
  real,                        intent(in) :: xdum
  integer,                     intent(in) :: num_knowns
  integer,                     intent(in) :: i_high
  real, dimension(num_knowns), intent(in) :: xknown
  real, dimension(num_knowns), intent(in) :: yknown
! -------------------------------
! OUT :
  real,                         intent(out):: ydum
! -------------------------------
! LOCAL :
  real                                     :: slope
  
  
  if (i_high == 1) then
     slope = (yknown(2) - yknown(1)) / & 
             (xknown(2) - xknown(1))
     ydum  = yknown(1)  - slope*(xknown(1) - xdum)
  else if (i_high == num_knowns+1) then
     slope = (yknown(num_knowns) - yknown(num_knowns-1)) / &
             (xknown(num_knowns) - xknown(num_knowns-1)) 
     ydum  = yknown(num_knowns)  + slope*(xdum - xknown(num_knowns))
  else
     write(6,*) "FATAL ERROR: subroutine linear_fit.f90:"
     write(6,*) "i_high /= 1 or num_knowns+1 in subroutine extrapolate"
     stop
  endif

end subroutine extrapolate_linear_fit_dp


      subroutine volume_fraction_normalization(NSD,            & !IN
                                               ISD,             &
                                               nfract,          &
                                               vfract_ap,       &
                                               vfract)

      use mod_par_OS,  only  : N_CHEM_MAX
      implicit none

! -------------------------------
! IN :
      integer,                        intent(in) :: NSD, ISD, nfract
      real,dimension(N_CHEM_MAX),     intent(in) :: vfract_ap ! not normalized volume fractions of chem components
! -------------------------------
! OUT :
      real(selected_real_kind(p=15)), dimension(N_CHEM_MAX),    intent(out) :: vfract ! normalized volume fractions of chem components

! -------------------------------
! LOCAL :
      integer :: ifract
      real(selected_real_kind(p=15)) :: xnorm


      vfract(:) = 0.0

      xnorm = sum(vfract_ap(1:nfract))
      vfract(1:nfract) = vfract_ap(1:nfract)/xnorm ! normalized volume fractions of chem components

      return
      end subroutine volume_fraction_normalization

! ********************************************************************************************

      subroutine complex_refr_index_practical_mixture (ISD,            &  !IN
                                                       nfract,         &
                                                       vfract,         &
                                                       wvln_um,        &
                                                       RREAL,          &  !OUT
                                                       RIMAG)
      use mod_intrpl_linear
      use mod_par_OS,  only  : N_CHEM_MAX
      implicit none

! -------------------------------
! IN :
      integer,                        intent(in) :: ISD,nfract
      real,                           intent(in) :: wvln_um
      real(selected_real_kind(p=15)),dimension(N_CHEM_MAX),     intent(in) :: vfract
! -------------------------------
! OUT :
      real,                     intent(out) :: RREAL, RIMAG
! -------------------------------
! LOCAL :
      integer :: ifract
      real(selected_real_kind(p=15))  :: refr_chem, refi_chem

      RREAL = 0.0
      RIMAG = 0.0

      do ifract=1,nfract

        !MHG Interpolate REFI LUT:
        refr_chem =  LINEAR(LUT_CHEM(ISD,ifract)%WL(1:LUT_CHEM(ISD,ifract)%NWL),LUT_CHEM(ISD,ifract)%RRI(1:LUT_CHEM(ISD,ifract)%NWL),LUT_CHEM(ISD,ifract)%NWL,wvln_um)
        refi_chem =  LINEAR(LUT_CHEM(ISD,ifract)%WL(1:LUT_CHEM(ISD,ifract)%NWL),LUT_CHEM(ISD,ifract)%IRI(1:LUT_CHEM(ISD,ifract)%NWL),LUT_CHEM(ISD,ifract)%NWL,wvln_um)


!        call linear_near_neighbor_fit_dp (LUT_CHEM(ISD,ifract)%NWL,                             &
!                                          LUT_CHEM(ISD,ifract)%WL(1:LUT_CHEM(ISD,ifract)%NWL),  &
!                                          LUT_CHEM(ISD,ifract)%RRI(1:LUT_CHEM(ISD,ifract)%NWL), &
!                                          1,   wvln_um,   refr_chem    )
!
!        call linear_near_neighbor_fit_dp (LUT_CHEM(ISD,ifract)%NWL,                             &
!                                  LUT_CHEM(ISD,ifract)%WL(1:LUT_CHEM(ISD,ifract)%NWL),          &
!                                  LUT_CHEM(ISD,ifract)%IRI(1:LUT_CHEM(ISD,ifract)%NWL),         &
!                                  1,   wvln_um,   refi_chem    )
        RREAL = RREAL + (vfract(ifract)*refr_chem)
        RIMAG = RIMAG + (vfract(ifract)*refi_chem)
      end do
 
      end subroutine complex_refr_index_practical_mixture

! ********************************************************************************************



subroutine get_REFI_CHEM (RIN,             & !IN
                          IDIM1            &
                          )

    use mod_retr_general_output_derived_type
    use mod_retr_settings_derived_type
    use mod_globals, only  : GBL_FILE_PATH_LEN
    use mod_par_OS,  only  : N_CHEM_MAX
    use mod_c_utils, only: cstring2fstring
    implicit none

    ! -------------------------------
    ! IN :
    type(retr_input_settings),      intent(in)    ::  RIN
    integer,                        intent(in)    ::  IDIM1
    ! -------------------------------
    ! LOCAL :
    integer :: ifract, nfract, ISD
    character (len=GBL_FILE_PATH_LEN), dimension(N_CHEM_MAX)   ::  name_new_chem
    character (len=GBL_FILE_PATH_LEN)    ::  RIN_CHEM_PATH, aux, internal_files

    call cstring2fstring(RIN%chemistry%folder, RIN_CHEM_PATH)
    call cstring2fstring(RIN%DLSF%internal_file_path, internal_files)

    RIN_CHEM_PATH = TRIM(internal_files)//TRIM(RIN_CHEM_PATH)

    do ISD=1,RIN%NSD
      nfract = RIN%NDIM%n3(ISD,IDIM1)
      if ( RIN%indep_par ) then
        nfract = RIN%NDIM_plus%n3(ISD,IDIM1)
      endif
      do ifract=1,nfract
          call cstring2fstring(RIN%chemistry%species(:,ifract,ISD), LUT_CHEM(ISD,ifract)%NAME)

          call get_LUT_REFI_CHEM(RIN_CHEM_PATH,                &
                                 LUT_CHEM(ISD,ifract)%NAME,    &
                                 LUT_CHEM(ISD,ifract)%WL,      &
                                 LUT_CHEM(ISD,ifract)%NWL,     &
                                 LUT_CHEM(ISD,ifract)%RRI,     &
                                 LUT_CHEM(ISD,ifract)%IRI)
    
      end do

    end do

    return

end subroutine get_REFI_CHEM



subroutine get_LUT_REFI_CHEM (CHEM_PATH,      & !IN
                              name_chem,      &
                              WL,nlines,      & !OUT
                              refr_chem,      &
                              refi_chem)

    use mod_retr_general_output_derived_type
    use mod_retr_settings_derived_type
    use mod_globals, only  : GBL_FILE_PATH_LEN
    use mod_intrpl_linear
    use mod_par_OS,  only  : N_CHEM_MAX
    implicit none

    ! -------------------------------
    ! IN :
    character (len=GBL_FILE_PATH_LEN), intent(in)    ::  CHEM_PATH
    character (len=GBL_FILE_PATH_LEN), intent(in)    ::  name_chem
    
    ! -------------------------------
    ! OUT :
    real,                              intent(out), DIMENSION(N_RR_MAX)   ::  refr_chem, refi_chem
    real, DIMENSION(N_RR_MAX),                              intent(out)    ::  WL
    integer,                              intent(out)    ::  nlines

    ! -------------------------------
    ! LOCAL :
    integer                 :: ifract, j, b, nheaders
    logical                 :: file_exists
    character (len=GBL_FILE_PATH_LEN) :: aux

    inquire(FILE=TRIM(CHEM_PATH)//TRIM(name_chem)//TRIM('.txt'), EXIST=file_exists)

    nheaders = 2

!MH Read REFI LUT
    if(file_exists) then

        nlines = 0
        open (22, file = TRIM(CHEM_PATH)//TRIM(name_chem)//TRIM('.txt'))
        do
          read(22,*,iostat=b)
          if (b/=0) exit
          nlines = nlines + 1
        end do
        close (22)


        open(UNIT = 2,file=TRIM(CHEM_PATH)//TRIM(name_chem)//TRIM('.txt'), status = 'old', iostat=b, action='read')


        do j=1,nheaders
            read(2,*) aux
        end do

        nlines = nlines - nheaders

        do j=1,nlines
            read(2,*) WL(j), refr_chem(j), refi_chem(j)
        end do

        close(2)

    else

        write(*,*) 'ERROR: ', TRIM(CHEM_PATH)//TRIM(name_chem),  '.txt must be an existing file. Chemical LUT refractive index ', TRIM(name_chem), '.txt does not exist or is not installed in ', TRIM(CHEM_PATH), '. Please, install it correctly or choose a proper chemical LUT refractive index name.'
        stop

    end if

    return

end subroutine get_LUT_REFI_CHEM


end module mod_mixture_ref_index_chem
