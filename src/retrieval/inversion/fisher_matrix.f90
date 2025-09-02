! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! file contains :

! SUBROUTINE FISHMX
! subroutine UF_matrix
!

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      SUBROUTINE FISHMX (                   &
                           KM,KN,           &
                           FS,FPS,CS,US,    & 
                           UFS,FFS          &
                        )
!C**************************************************
!C  THIS SUBROUTINE CALCULATES "FISHER MATRIX":
!C            (U)**T (C) (U)
!C                and
!C  "gradient" (U)**T (C) (F(p)-F(*))
!C                and
!C   "residual" (F(p)-F(*))**T (C) (F(p)-F(*))
!C**************************************************
!C  INPUT:
!C        KN  I        - number of lines
!C        KM  I        - number of columns
!C        US   R(KM,KN) - (derivatives) matrix
!C        CS   R(KM)    - matrix inverse to covariance
!C        FPS  R(KM)    - P-th approximation of vector F
!C        FS   R(KM)    - "measurements" vector 
!C  OUTPUT:
!C         UFS R(KN,KN) -"Fisher matrix" normalized by
!C                      maximum diagonal ellement
!C         FFS R(KN)    - "gradient" vector normalized by
!C                      maximum diagonal ellement of UF  
!C***************************************************
      USE mod_par_inv, only : KMESS,KPARS
	  
      IMPLICIT NONE	  

! ----------------------------------------------------------
! IN :
      INTEGER,                    INTENT(IN)  :: KM,KN	  
      REAL,DIMENSION(KMESS,KPARS),INTENT(IN)  :: US
      REAL,DIMENSION(KMESS),      INTENT(IN)  :: CS
      REAL,DIMENSION(KMESS),      INTENT(IN)  :: FS,FPS
! ----------------------------------------------------------
! OUT :
      REAL,DIMENSION(KPARS,KPARS),INTENT(OUT) :: UFS
      REAL,DIMENSION(KPARS),      INTENT(OUT) :: FFS
! ----------------------------------------------------------
! LOCAL :	  
      INTEGER :: I,I1
! ----------------------------------------------------------

!C*** calculating "Fisher matrix" 
!write(*,*) 'in FISHMX'
!      write(*,*) 'US(1:13,14)', US(1:13,14)
!write(*,*) 'CS', CS(1:13)
!write(*,*) 'KM, KN', KM, KN
      DO I=1,KN
      DO I1=1,KN
         UFS(I,I1)=SUM(CS(1:KM)*US(1:KM,I)*US(1:KM,I1))
      ENDDO ! I1
      ENDDO ! I

      DO I=1,KN
         FFS(I)=SUM(US(1:KM,I)*CS(1:KM)*(FPS(1:KM)-FS(1:KM)))
      ENDDO ! I
!write(*,*) 'UFS(14,1:32)', UFS(14,1:32)
      RETURN
      END SUBROUTINE FISHMX 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
	 
      subroutine UF_matrix (                              & ! IN
                              RIN,                        &
                              KN,npixels,UFS,             &
                              TCCORG,SMIM1,NISM,KSM,IMSM, &	
                              UF,nnz                      & ! OUT 
                           )
	 
      use mod_par_inv, only : KIMAGE,KMPSM,KPARS,KPAR
      use mod_retr_settings_derived_type

      implicit none

! ---------------------------------------------------	  
! IN :
      type(retr_input_settings),          intent(in)  :: RIN
      integer,                            intent(in)  :: KN,npixels,NISM      
      integer,dimension(KMPSM),           intent(in)  :: KSM
      integer,dimension(KMPSM,KIMAGE),    intent(in)  :: IMSM 
      real,dimension(KPARS,KPARS,KIMAGE), intent(in)  :: UFS
      real,                               intent(in)  :: TCCORG      
      real,dimension(KIMAGE,KPARS,KIMAGE),intent(in)  :: SMIM1
! ---------------------------------------------------
! OUT :
      real, dimension(KPAR,KPAR),         intent(out) :: UF
      integer,                            intent(out) :: nnz      
! ---------------------------------------------------
! LOCAL :
      integer :: ipix,I,I1,IS,IS1,IS2
! ---------------------------------------------------	  
      UF(:,:) = 0.0
      nnz = 0
      do ipix=1,npixels
        do I1=1,RIN%KNSING
          do I=1,RIN%KNSING
               UF((ipix-1)*RIN%KNSING+I,(ipix-1)*RIN%KNSING+I1) =  &
               UFS(I,I1,ipix)
               if(UFS(I,I1,ipix) .ne. 0) &
               nnz = nnz+1
          enddo ! I
        enddo ! I1
      enddo ! ipix      
      do IS=1,NISM
        do I=1,RIN%KNSING
          do IS1=1,KSM(IS)
            do IS2=1,KSM(IS)
              if(IS1 .ne. IS2) then
                 UF((IMSM(IS,IS1)-1)*RIN%KNSING+I,(IMSM(IS,IS2)-1)*RIN%KNSING+I) =  & 
                 TCCORG*SMIM1(IMSM(IS,IS1),I,IMSM(IS,IS2))
              endif
            enddo ! IS2
          enddo ! IS1      
        enddo ! I
      enddo ! IS

    return
    end subroutine UF_matrix 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss      

