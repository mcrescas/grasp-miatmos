! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

#include "../constants_set/mod_globals.inc"
#include "solver_macros.inc"
module mod_fisher_matrix_ccs

      !use mod_par_inv, only : KPAR, nnz_par
      use mod_stop_report

      implicit none
! -----------------------------------------------------------------------
! Compressed Column Storage (CCS)
! The CCS format is specified by the arrays {val, row, nval}. 
! nnz - number of nonzeros 
! val - nonzeros 
! row stores the row indices of each nonzero
! nval stores the index of the elements in val which start 
! a column of UF matrix
! nval(ncol+1)=nnz+1 where ncol - number of columns in matrix
!
! in addition to CCS, col stores column index of each nonzero

      !type nonzero
         !integer, dimension(nnz)         :: row,col
         !real*8,  dimension(nnz)         :: val
         !integer, dimension(npar+1)      :: nval
      !end type nonzero    
      type nonzero
         integer, dimension(:), allocatable  :: row,col
         real*8,  dimension(:), allocatable  :: val
         integer, dimension(:), allocatable  :: nval
      end type nonzero    

      integer(kind=c_intptr_t), private :: superlu_factors
! -----------------------------------------------------------------------

      contains

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      !subroutine allocate_sparse_matrix_storage ( UFNZ_CCS )
      subroutine allocate_sparse_matrix_storage ( UFNZ_CCS, nnz, npar )

      implicit none
! ----------------------------------------------------------------------- 
!     Compressed Column Storage (CCS)
      type(nonzero), intent(inout)  :: UFNZ_CCS
!     number of nonzero matrix elements
! NOTE: 
!     current value of nnz is bigger than number of non zero matrix elements
!     because we do not know the number at the time of CCS allocation ?
      integer,       intent(in)  :: nnz  ! nnz_par
!     number of elements in solution vector
      integer,       intent(in)  :: npar ! KPAR
! ----------------------------------------------------------------------- 
      integer  ::  alloc_stat
! ----------------------------------------------------------------------- 
       
      if(.not. allocated(UFNZ_CCS%row)) then
        allocate(UFNZ_CCS%row(1:nnz),stat=alloc_stat)
        if (alloc_stat /= 0) then
        !write(*,*) 'size(UFNZ_CCS%row)*4',size(UFNZ_CCS%row)*4,'  nnz=',nnz
        write(tmp_message,'(2(a,i0))') &
        'error while trying to allocate UFNZ_CCS%row(1:nnz)'
        G_ERROR(trim(tmp_message))
        endif
      endif
      if(.not. allocated(UFNZ_CCS%col)) then
        allocate(UFNZ_CCS%col(1:nnz),stat=alloc_stat)
        if (alloc_stat /= 0) then
        !write(*,*) 'size(UFNZ_CCS%col)*4',size(UFNZ_CCS%col)*4,'  nnz=',nnz
        write(tmp_message,'(2(a,i0))') &
        'error while trying to allocate UFNZ_CCS%col(1:nnz)'
        G_ERROR(trim(tmp_message))
        endif
      endif
      if(.not. allocated(UFNZ_CCS%val)) then
        allocate(UFNZ_CCS%val(1:nnz),stat=alloc_stat)
        if (alloc_stat /= 0) then
        !write(*,*) 'size(UFNZ_CCS%val)*8',size(UFNZ_CCS%val)*8,'  nnz=',nnz
        write(tmp_message,'(2(a,i0))') &
        'error while trying to allocate UFNZ_CCS%val(1:nnz)'
        G_ERROR(trim(tmp_message))
        endif
      endif
      if(.not. allocated(UFNZ_CCS%nval)) then
        allocate(UFNZ_CCS%nval(1:npar+1),stat=alloc_stat)
        if (alloc_stat /= 0) then
        !write(*,*) 'size(UFNZ_CCS%val)*4',size(UFNZ_CCS%nval)*4,'  npar+1=',npar+1
        write(tmp_message,'(2(a,i0))') &
        'error while trying to allocate UFNZ_CCS%nval(1:npar+1)'
        G_ERROR(trim(tmp_message))
        endif
      endif
      
      return
      end subroutine allocate_sparse_matrix_storage

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine deallocate_sparse_matrix_storage ( UFNZ_CCS )

      implicit none
! ----------------------------------------------------------------------- 
!     Compressed Column Storage (CCS)

      type(nonzero),    intent(inout)  :: UFNZ_CCS 
! ----------------------------------------------------------------------- 
      integer  ::  alloc_stat
! ----------------------------------------------------------------------- 
       
         if(allocated(UFNZ_CCS%row)) then
            deallocate(UFNZ_CCS%row,stat=alloc_stat)
            if (alloc_stat /= 0) then
            write(tmp_message,'(2(a,i0))') &
            'error while trying to deallocate UFNZ_CCS%row(1:nnz)'
            G_ERROR(trim(tmp_message))
            endif
         endif
         if(allocated(UFNZ_CCS%col)) then 
            deallocate(UFNZ_CCS%col,stat=alloc_stat)
            if (alloc_stat /= 0) then
            write(tmp_message,'(2(a,i0))') &
            'error while trying to deallocate UFNZ_CCS%col(1:nnz)'
            G_ERROR(trim(tmp_message))
            endif
         endif
         if(allocated(UFNZ_CCS%val)) then 
            deallocate(UFNZ_CCS%val,stat=alloc_stat)
            if (alloc_stat /= 0) then
            write(tmp_message,'(2(a,i0))') &
            'error while trying to deallocate UFNZ_CCS%val(1:nnz)'
            G_ERROR(trim(tmp_message))
            endif
         endif
         if(allocated(UFNZ_CCS%nval)) then
            deallocate(UFNZ_CCS%nval,stat=alloc_stat)
            if (alloc_stat /= 0) then
            write(tmp_message,'(2(a,i0))') &
            'error while trying to deallocate UFNZ_CCS%nval(1:KPAR+1)'
            G_ERROR(trim(tmp_message))
            endif
         endif
         
      return
      end subroutine deallocate_sparse_matrix_storage

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine UFNZ_symmetry_check ( KN,nnz,UFNZ,  & ! IN
                                       UF            & ! OUT
                                     )
	  	  
      use mod_par_inv, only : KPAR

      implicit none
 
! ----------------------------------------------------------------------- 
      integer,                    intent(in)  :: KN,nnz
      type(nonzero),              intent(in)  :: UFNZ
      real, dimension(KPAR,KPAR), intent(out) :: UF
! -----------------------------------------------------------------------      
      integer                       :: i,j  											   
! -----------------------------------------------------------------------

! CHECK out a symmetry of UFNZ
write(*,*) 'test in ufnz_symmetry_check; nnz=',nnz,'  KN=',KN
      UF(:,:) = 0.0
      do i=1, nnz
        UF(UFNZ%row(i),UFNZ%col(i))=UFNZ%val(i)
write(*,*) 'i=',i
      enddo ! i
      do j=1,KN
        do i=1,KN
          if(UF(i,j) .ne. UF(i,j)) then
            write(*,*) 'in UFNZ_symmetry_check j=',j,'  i=',i,'  UF(i,j)=',UF(i,j),'  UF(j,i)=',UF(j,i)
          endif 
        enddo ! i
      enddo  ! j

      return
      end subroutine UFNZ_symmetry_check

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine UF_nonzero_one_pixel (                 &
                                          KNSINGF,UFS,  & ! IN
                                          UFNZ,nnz      & ! OUT
                                      )

      use mod_par_inv, only : KPARS,KPAR
	  	  
      implicit none
 
! ----------------------------------------------------------------------- 
      integer,                      intent(in)    :: KNSINGF
      real, dimension(KPARS,KPARS), intent(in)    :: UFS

      type(nonzero),                intent(inout) :: UFNZ
      integer,                      intent(out)   :: nnz
! -----------------------------------------------------------------------      
      real                          :: XA
      integer                       :: I,J,irow_mtr,irow_vec  											   
      integer, dimension(KPAR)      :: UF_nrow_col
! -----------------------------------------------------------------------
   
      UF_nrow_col(:) = 0

      UFNZ%row(:) = 0
      UFNZ%col(:) = 0
      UFNZ%val(:) = 0.0	

      nnz=0
      do J=1,KNSINGF
         do I=1,KNSINGF
            XA=UFS(I,J)
		      if(XA .eq. 0.) cycle
            nnz=nnz+1
            irow_mtr=I
            irow_vec=nnz
            UFNZ%col(irow_vec) = J
            UFNZ%row(irow_vec) = irow_mtr				 
            UFNZ%val(irow_vec) = XA             
            UF_nrow_col(J)=UF_nrow_col(J)+1
            if(irow_mtr .eq. 0) then
               write(tmp_message,'(a,i0,2x,a)') &
               '1: irow_mtr = ',irow_mtr,'can not be 0.'
               G_ERROR(trim(tmp_message))
            endif
!                 UF((IIMAGEQ1-1)*KNSINGF+I,(IIMAGEQ1-1)*KNSINGF+I1)=  &
!                 UFS(IIMAGEQ1,I,I1)
         enddo ! I
      UFNZ%nval(J) = sum(UF_nrow_col(1:J))-UF_nrow_col(J)+1          	
      enddo ! J

      UFNZ%nval(KNSINGF+1) = nnz+1
	  
      return
      end subroutine UF_nonzero_one_pixel
      
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine UF_nonzero (                         &
                              KNSINGF,npixels,UFS,    & ! IN
                              TCCORG,                 &
                              smim,                   &
                              UFNZ,                   & ! INOUT
                              nnz                     & ! OUT
                            )

      use mod_par_inv, only : KIMAGE,KPARS,KPAR
      use mod_index_cloud
	  	  
      implicit none
 
! ----------------------------------------------------------------------- 
      integer,                             intent(in)  :: KNSINGF,npixels
      real, dimension(KPARS,KPARS,KIMAGE), intent(in)  :: UFS
      real,                                intent(in)  :: TCCORG
      real, dimension(KIMAGE,KPARS,KIMAGE),intent(in)  :: smim

      type(nonzero),                       intent(inout) :: UFNZ
      integer,                             intent(out)   :: nnz
! -----------------------------------------------------------------------      
      real                                 :: XA
      integer                              :: I,J,IS,IS1,IS2
      integer                              :: ipix,jpix,KNF,KN1
      integer                              :: icol,irow_mtr,irow_vec
      integer, dimension(KPAR)             :: UF_nrow_col
! -----------------------------------------------------------------------

      KN1=KNSINGF+npixels-1
      KNF =npixels*KNSINGF
	  	  
      UF_nrow_col(:)=0

      UFNZ%row(:)=0
      UFNZ%col(:)=0
      UFNZ%val(:)=0.0	

!  J  jpix   - UF matrix columns 
!  I  ipix   - UF matrix rows
      nnz=0
      do jpix=1,npixels
      do J=1,KNSINGF
      icol=(jpix-1)*KNSINGF+J
      do ipix=1,npixels
         if(ipix .eq. jpix) then
         do I=1,KNSINGF
            XA=UFS(I,J,ipix)
		      if(XA .eq. 0.) cycle
            nnz=nnz+1
            irow_mtr=(ipix-1)*KNSINGF+I
            irow_vec=nnz
            UFNZ%col(irow_vec) = icol
            UFNZ%row(irow_vec) = irow_mtr				 
            UFNZ%val(irow_vec) = XA             
            UF_nrow_col(icol)=UF_nrow_col(icol)+1
            if(irow_mtr .eq. 0) then
            write(tmp_message,'(a,i0,2x,a)') &
            '1: irow_mtr = ',irow_mtr,'can not be 0.'
            G_ERROR(trim(tmp_message))
            endif
!                 UF((IIMAGEQ1-1)*KNSINGF+I,(IIMAGEQ1-1)*KNSINGF+I1)=  &
!                 UFS(IIMAGEQ1,I,I1)
         enddo ! I
         else
            if(ipix .lt. jpix) then
            XA=TCCORG*smim(jpix,J,ipix)
            else 
            XA=TCCORG*smim(ipix,J,jpix)
            endif ! ipix.lt.jpix
		      if(XA .eq. 0.) cycle
            nnz=nnz+1          
            irow_mtr=(ipix-1)*KNSINGF+J
            irow_vec=nnz
            UFNZ%col(irow_vec) = icol
            UFNZ%row(irow_vec) = irow_mtr				 
            UFNZ%val(irow_vec) = XA
            UF_nrow_col(icol)=UF_nrow_col(icol)+1
            if(irow_mtr .eq. 0) then
            write(tmp_message,'(a,i0,2x,a)') &
            '2: irow_mtr=',irow_mtr,'can not be 0.'
            G_ERROR(trim(tmp_message))
            endif
!               UF((IMSM(IS,IS1)-1)*KNSINGF+I,(IMSM(IS,IS2)-1)*KNSINGF+I)=  & 
!               TCCORG*SMIM(IS,I,IS1,IS2)
		    endif ! ipix .eq. jpix
      enddo ! ipix
      UFNZ%nval(icol) = sum(UF_nrow_col(1:icol))-UF_nrow_col(icol)+1
      enddo ! J
      enddo ! jpix  

      UFNZ%nval(KNF+1) = nnz+1

      return
      end subroutine UF_nonzero

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

#ifdef SUPERLU
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! solver_SuperLU
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! solver_init_SuperLU
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine solver_init_SuperLU(KN, nnz, UFNZ, b)

        use iso_c_binding ! c_intptr_t
        use mod_par_inv, only : KPAR

        implicit none
! ----------------------------------------------------------------------
        integer,                  intent(in)    :: KN, nnz
        type(nonzero),            intent(in)    :: UFNZ
        real*8, dimension(KPAR),  intent(inout) :: b
! ----------------------------------------------------------------------
        integer, parameter :: iopt = 1
        integer :: nrhs
        integer :: n
        integer :: ldb
        integer :: info
! ----------------------------------------------------------------------
        nrhs = 1
        n = KN
        ldb = KN
!*
!* First, factorize the matrix. The factors are stored in *factors* handle.
        call c_fortran_dgssv (                                                     &
                              iopt, n, nnz, nrhs,                                  &
                              UFNZ%val(1:nnz), UFNZ%row(1:nnz), UFNZ%nval(1:KN+1), &
                              b(1:KN), ldb, superlu_factors, info                  &
                             )
!*
        if (info .eq. 0) then
!tl           write (*,*) 'Factorization succeeded'
        else
           write(tmp_message,'(a,i0)') 'INFO from factorization = ',info
           G_ERROR(trim(tmp_message))
        endif

      end subroutine solver_init_SuperLU

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! solver_cleanup_SuperLU
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine solver_cleanup_SuperLU(KN, nnz, UFNZ, b)

        use iso_c_binding ! c_intptr_t
        use mod_par_inv, only : KPAR

        implicit none
! ----------------------------------------------------------------------
        integer,                  intent(in)    :: KN, nnz
        type(nonzero),            intent(in)    :: UFNZ
        real*8, dimension(KPAR),  intent(inout) :: b
! ----------------------------------------------------------------------
        integer, parameter :: iopt = 3
        integer :: nrhs
        integer :: n
        integer :: ldb
        integer :: info
! ----------------------------------------------------------------------
        nrhs = 1
        n = KN
        ldb = KN
!*
        call c_fortran_dgssv (                                                     &
                              iopt, n, nnz, nrhs,                                  &
                              UFNZ%val(1:nnz), UFNZ%row(1:nnz), UFNZ%nval(1:KN+1), &
                              b(1:KN), ldb, superlu_factors, info                  &
                             )
!*

      end subroutine solver_cleanup_SuperLU

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! solver_SuperLU
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine solver_SuperLU ( KN, nnz, UFNZ, b )

      use mod_par_inv, only : KPAR
      use iso_c_binding ! c_intptr_t

!     iso_c_binding is only needed to get the type integer(kind=c_intptr_t)
!     integer(kind=c_intptr_t) is the official integer type to hold pointers
!     in fortran 2003+.
!     When iso_c_binding is not available (especially with old compilers),
!     one can remove the 'use iso_c_binding' statement. One has then to replace
!     the type integer(kind=c_intptr_t) with;
!     * integer*4 on 32-bits configurations (32-bits pointers)
!     * integer*8 on 64-bits configurations (64-bits pointers)

      implicit none
 
! ----------------------------------------------------------------------
      integer,                  intent(in)    :: KN, nnz
      type(nonzero),            intent(in)    :: UFNZ
      real*8, dimension(KPAR),  intent(inout) :: b
!	----------------------------------------------------------------------
! PhF
      integer  ::  n, nrhs, ldb, info, iopt
!      integer*8  ::  factors
! FD
!      integer(kind=c_intptr_t)  ::  factors
!	----------------------------------------------------------------------
!	----------------------------------------------------------------------
      nrhs = 1
      n    = KN
      ldb  = KN
!*
!* Second, solve the system using the existing factors.
        iopt = 2
        call c_fortran_dgssv (                                                       &
                                iopt, n, nnz, nrhs,                                  &
                                UFNZ%val(1:nnz), UFNZ%row(1:nnz), UFNZ%nval(1:KN+1), &
                                b(1:KN), ldb, superlu_factors, info                  &
                             )
!*
        if (info .eq. 0) then
          !write (*,*) 'SuperLU:  Solve succeeded'
!tl         write (*,*) (b(i1), i1=1,10)
!           write (*,'(10e14.4)') (b(i1), i1=1,n )
        else
          write(tmp_message,'(a,i0)') 'INFO from triangular solve = ',info
          G_ERROR(trim(tmp_message))
        endif
!*
      return
      end subroutine solver_SuperLU
#endif

#ifdef SUPERLU_MT
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! solver_SuperLU_MT
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine solver_SuperLU_MT (  KN,nnz,UFNZ,    & ! IN
                                      b               & ! INOUT
                                   )
      use mod_par_inv, only : KPAR
      use iso_c_binding ! c_intptr_t
	  	  
      implicit none
 !   ----------------------------------------------------------------------- 
      integer,                  intent(in)    :: KN,nnz
      type(nonzero),            intent(in)    :: UFNZ
      real*8, dimension(KPAR),  intent(inout) :: b
!	------------------------------------------------------------------------------------------
      integer n, nrhs, ldb, info, iopt
      integer(kind=c_intptr_t) factors
!	------------------------------------------------------------------------------------------
!   ind <= 1 - factorize the matrix 
!        > 1 - use previous factorization  
!	------------------------------------------------------------------------------------------

      nrhs = 1
      n    = KN
      ldb  = KN
	  
! Unlike the regular SuperLU 4.3, the mt version does not separate factorization
! from solving. both is done at once.

!*
!* First, factorize & solve the matrix. The factors are stored in *factors* handle.
      iopt = 1
      call c_bridge_pdgssv ( 6,                                                   &
                             iopt, n, nnz, nrhs,                                  &
	                           UFNZ%val(1:nnz), UFNZ%row(1:nnz), UFNZ%nval(1:KN+1), &
                             b(1:KN), ldb, factors, info                          &
						               )
!*
      if (info .eq. 0) then
!tl         write (*,*) 'Factorization & solve succeeded'
      else
         write(tmp_message,'(a,i0)') 'INFO from factorization & solve = ',info
         G_ERROR(trim(tmp_message))
      endif

!* Last, free the storage allocated inside SuperLU
      iopt = 3
      call c_bridge_pdgssv ( 6,                                                   &
	                           iopt, n, nnz, nrhs,                                  &
	                           UFNZ%val(1:nnz), UFNZ%row(1:nnz), UFNZ%nval(1:KN+1), &
                             b(1:KN), ldb, factors, info                          &
						               )
!*
      return
      end subroutine solver_SuperLU_MT
#endif

#ifdef VIENNA_CL
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! solver_viennacl
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine solver_viennacl (  KN,nnz,UFNZ,     & ! IN
                                    b                & ! INOUT
                                 )

        use mod_par_inv, only : KPAR
        implicit none
					  		
      interface
			subroutine viennacl_solve(m, nnz, vals, rowidx, colptr, b, info) bind (C, name="viennacl_solve")
			use, intrinsic :: ISO_C_BINDING
			implicit none
			
			integer(C_INT), value			:: m, nnz
			real(C_DOUBLE), dimension(nnz)	:: vals
			integer(C_INT), dimension(nnz)	:: rowidx
			integer(C_INT), dimension(m+1)	:: colptr
			real(C_DOUBLE), dimension(m)	:: b
			integer(C_INT) 					:: info
			
			end subroutine viennacl_solve
		end interface

!   --------------------------------------------------------------------
        integer,                  intent(in)    :: KN,nnz
        type(nonzero),            intent(in)    :: UFNZ
        real*8, dimension(KPAR),  intent(inout) :: b
!	----------------------------------------------------------------------
        integer	:: info = 0
!	----------------------------------------------------------------------
        call viennacl_solve (                                                      &
                              KN, nnz,                                             &
                              UFNZ%val(1:nnz), UFNZ%row(1:nnz), UFNZ%nval(1:KN+1), &
                              b, info                                              &
                            )
        if (info .ne. 0) then
            write (tmp_message,'(a,i0)') "cusparse failed: info = ", info
            G_ERROR(trim(tmp_message))
        endif
        
        return
    end subroutine solver_viennacl
#endif

#ifdef MUMPS
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! solver_MUMPS
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine solver_MUMPS ( KN,nnz,UFNZ,     & ! IN
                                b                & ! INOUT
                              )

      use mod_par_inv, only : KPAR
      use mod_index_cloud

      implicit none

      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'	  	  

!	----------------------------------------------------------------------
      TYPE (DMUMPS_STRUC) :: mumps_par
      INTEGER             :: IERR
!	----------------------------------------------------------------------
!   --------------------------------------------------------------------
      integer,                  intent(in)    :: KN,nnz
      type(nonzero),            intent(in)    :: UFNZ
      real*8, dimension(KPAR),  intent(inout) :: b

!	----------------------------------------------------------------------
      integer            :: i,i1,nnz_diag
      logical            :: lsym = .true.
!	----------------------------------------------------------------------

!*

!C Define a communicator for the package.                                                                                                                                                                                
      mumps_par%COMM = MPI_COMM_WORLD
!C  Initialize an instance of the package                                                                                                                                                                                
!C  for L U factorization (sym = 0, with working host)                                                                                                                                                                   
      mumps_par%JOB = -1
      mumps_par%PAR =  1
      if(lsym) then
      mumps_par%SYM =  2  ! 2 - A is general symmetric                          
      else
      mumps_par%SYM =  0  ! 0 - A is unsymmetric
      endif ! lsym
	  
      CALL DMUMPS(mumps_par)

!C  Define problem on the host (processor 0)                                                                                                                                                                             
      IF ( mumps_par%MYID .eq. 0 ) THEN
         mumps_par%N    = KN		
         mumps_par%NRHS = 1 !mumps_par%N
         if(lsym) then
            nnz_diag=0
            do i=1,nnz
            if(UFNZ%col(i).eq.UFNZ%row(i)) nnz_diag=nnz_diag+1
            enddo ! i
            mumps_par%NZ   = (nnz-nnz_diag)/2+nnz_diag  
            !write(*,*) '** symmetric matrix option used'
         else
            mumps_par%NZ   = nnz  
            !write(*,*) '** unsymmetric matrix option used'
         endif ! lsym
		
         mumps_par%ICNTL(1)=0 ! 0 NO ERRORS
         mumps_par%ICNTL(2)=0 ! 0 NO WARNINGS
         mumps_par%ICNTL(3)=0 ! 0 REMOVE MUMPS MAIN VERBOSE

         ALLOCATE( mumps_par%IRN ( mumps_par%NZ ) )
         ALLOCATE( mumps_par%JCN ( mumps_par%NZ ) )
         ALLOCATE( mumps_par%A   ( mumps_par%NZ ) )

         if(lsym) then
! Lower triangle of symmetric matrix; mumps_par%SYM =  2 - A is general symmetric
            i1=0
            do i=1,nnz
            if(UFNZ%row(i).le.UFNZ%col(i)) then
            i1=i1+1
            mumps_par%IRN(i1) = UFNZ%col(i) ! CCS to MUMPS storage conversion (matrix is symmetric)  
            mumps_par%JCN(i1) = UFNZ%row(i) ! CCS to MUMPS storage conversion (matrix is symmetric)
            mumps_par%A  (i1) = UFNZ%val(i)
            endif
            enddo ! i
            !WRITE( 6, * ) ' i1=', i1 ,' mumps_par%NZ=',mumps_par%NZ,'  in solver_MUMPS'
            if(i1.ne.mumps_par%NZ) then
            write(tmp_message,'(2(a,i0,3x),2a)') &
            'i1 = ', i1 ,'mumps_par%NZ = ',mumps_par%NZ, &
            NEW_LINE('A'), &
            'i1 .ne. mumps_par%NZ'
            G_ERROR(trim(tmp_message))
            endif
         else
!  Unsymmetric matrix; mumps_par%SYM =  0 - A is unsymmetric
            mumps_par%IRN(1:nnz) = UFNZ%col(1:nnz) ! CCS to MUMPS storage conversion (matrix is symmetric)  
            mumps_par%JCN(1:nnz) = UFNZ%row(1:nnz) ! CCS to MUMPS storage conversion (matrix is symmetric)
            mumps_par%A  (1:nnz) = UFNZ%val(1:nnz)
         endif ! lsym

         ALLOCATE( mumps_par%RHS(mumps_par%N*mumps_par%NRHS))
         mumps_par%RHS(1:mumps_par%N)  = b(1:mumps_par%N)                                                                                                                                                                                                        

      END IF ! mumps_par%MYID .eq. 0

!C  Call package for solution                                                                                                                                                                                            
      mumps_par%JOB = 6
      CALL DMUMPS(mumps_par)
!C  Solution has been assembled on the host                                                                                                                                                                              
      IF ( mumps_par%MYID .eq. 0 ) THEN
         !WRITE( 6, * ) ' MUMPS:  Solve succeeded'
         !WRITE( 6, * ) ' Solution is ',(mumps_par%RHS(I),I=1,mumps_par%N*mumps_par%NRHS)
         b(1:mumps_par%N) = mumps_par%RHS(1:mumps_par%N)
      END IF ! mumps_par%MYID .eq. 0
	  
!tl	  IF ( mumps_par%MYID .eq. 0 ) THEN
!tl       b(1:mumps_par%N) = mumps_par%RHS(1:mumps_par%N)
!tl	  ENDIF

!tl	  CALL MPI_BCAST(b,KN,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,IERR)  ! not sure if it is necessary ???
	  
!C  Deallocate user data                                                                                                                                                                                                 
      IF ( mumps_par%MYID .eq. 0 )THEN
         DEALLOCATE( mumps_par%IRN )
         DEALLOCATE( mumps_par%JCN )
         DEALLOCATE( mumps_par%A   )
         DEALLOCATE( mumps_par%RHS )
      END IF ! mumps_par%MYID .eq. 0
!C  Destroy the instance (deallocate internal data structures)                                                                                                                                                           
      mumps_par%JOB = -2
      CALL DMUMPS(mumps_par)
!* 
	  
      return
      end subroutine solver_MUMPS

#endif

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
     subroutine sparse_solver( iu_main_output, & ! IN
                               KN,nnz,UFNZ,    &
                               b,              & ! INOUT
                               solver_timer    &
                               )
         use mod_par_inv, only : KPAR

! ----------------------------------------------------------------------
         integer,                 intent(in)    :: iu_main_output,  &
                                                   KN,nnz
         type(nonzero),           intent(in)    :: UFNZ
         real*8, dimension(KPAR), intent(inout) :: b
         real,                    intent(inout) :: solver_timer
! ----------------------------------------------------------------------
         real*4                        ::  time_start,time_finish
! ----------------------------------------------------------------------

        call CPU_TIME(time_start)

        call sparse_solver_aux ( KN, nnz, UFNZ, b )
        if ( error_present() ) return

        call CPU_TIME(time_finish)
        solver_timer = solver_timer + (time_finish-time_start)

     end subroutine sparse_solver


! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

end module mod_fisher_matrix_ccs





