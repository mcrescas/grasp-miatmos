! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

module mod_alloc_arrays

      use mod_alloc_kernels
      use mod_retr_settings_derived_type      
      
contains
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine alloc_arrays(RIN,KERNELS1,KERNELS2,US)

      use mod_par_inv, only : KMESS, KPARS, KIMAGE
      use iso_fortran_env, only: ERROR_UNIT
      implicit none

      type(retr_input_settings),  intent(in)     ::  RIN
      type(kernels_triangle_bin), intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),intent(inout)  ::  KERNELS2
      real,dimension(:,:,:),allocatable,intent(inout)  ::  US
      integer  ::  IERR

      !if (RIN%IPRI_verbose) then
      if (RIN%IPRI_additional_info) then
        write(*,*) 'alloc_arrays called'
      end if

!     Allocate array US (Jacobian matrix)
      allocate(US(KMESS, KPARS, KIMAGE),stat=IERR)
      if ( IERR/=0 ) then
          write(ERROR_UNIT, *) &
          'error while trying to allocate US (Jacobian matrix) in alloc_arrays'
          stop 12 ! ENOMEM
      end if

#if defined(SPHEROID)
!     Allocate arrays for spheroid package (DLS_bin)
      call alloc_kernel_arrays(RIN%DLSF%key,RIN%DLSF%keyEL,KERNELS1,KERNELS2)
#endif 
      
      return
      end subroutine alloc_arrays

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine dealloc_arrays(RIN,KERNELS1,KERNELS2,US)
      
      implicit none      

      type(retr_input_settings),  intent(in)     ::  RIN
      type(kernels_triangle_bin), intent(inout)  ::  KERNELS1
      type(kernels_lognormal_bin),intent(inout)  ::  KERNELS2
      real,dimension(:,:,:),allocatable,intent(inout)  ::  US
      integer  ::  IERR

      !if (RIN%IPRI_verbose) then
      if (RIN%IPRI_additional_info) then
        write(*,*) 'dealloc_arrays called'
      end if

!     Deallocate array US (Jacobian matrix)
      deallocate(US,stat=IERR)
      if ( IERR/= 0 ) stop &
      'error while trying to deallocate US (Jacobian matrix) in dealloc_arrays'

#if defined(SPHEROID)        
!     Deallocate arrays for spheroid package (DLS_bin)
      call dealloc_kernel_arrays(RIN%DLSF%key,RIN%DLSF%keyEL,KERNELS1,KERNELS2)
#endif 
      return
      end subroutine dealloc_arrays
      
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
end module mod_alloc_arrays
