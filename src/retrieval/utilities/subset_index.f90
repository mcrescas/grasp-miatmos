! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
        !> @brief Routine returns indices in real array a for elements of array b (b is a subset of a)
        !>
        !> @param[in]  tiny - tiny value
        !> @param[in]  abs_diff - flag for used differences (.T. - absolute, .F. - relative)
        !> @param[in]  na  - number of elements in array a
        !> @param[in]  a   - ascending order array
        !> @param[in]  nb  - number of elements in array b (b is a subset of a)
        !> @param[in]  b   - ascending order array (subset of array a)
        !> @param[out] index - indices of array b elements in array a
        !> @param[out] status - status of return (false or true)


            subroutine R_subset_index ( tiny, abs_diff, na, a, nb, b, index, status )

            implicit none
      !  ---------------------------------------------------------------------------------
            real,    intent(in)  :: tiny
            logical, intent(in)  :: abs_diff
            integer, intent(in)  :: na, nb
            real,    intent(in)  :: a(na), b(nb)
            integer, intent(out) :: index(nb)
            logical, intent(out) :: status
      !  ---------------------------------------------------------------------------------
            integer :: i, i1, icount
            real :: d, d1, d2
      !  ---------------------------------------------------------------------------------
            index(1:nb) = -999
            status = .false.

      ! Validate size of arrays: size( a ) must be >= size( b )
            if ( nb .gt. na ) then
              write(*,'(/,2(a,i0,x),x,a)') 'nb = ',nb,'> na = ',na,'in R_subset_index'
              return
            endif

      ! Search for indices
            icount = 0
            if (na .eq. 1 .and. nb .eq. 1 ) then
              d1 = abs(b(1)-a(1))
              if ( d1 .le. tiny ) then
                index(1) = 1
                icount = 1
              endif
            else
              i1 = 1
              do while ( i1 .le. nb )
              do i=2,na
              if ( b(i1) .ge. a(i-1) .and. b(i1) .le. a(i) ) then
                d1 = abs(b(i1)-a(i-1))
                d2 = abs(a(i) -b(i1) )
                if ( d1 .lt. d2 ) then
                  d = d1
                  if ( .not. abs_diff ) then
                  d = d1/b(i1)
                  endif
                  if ( d .le. tiny ) then
                    index(i1) = i-1
                    icount = icount + 1
                  endif
                elseif ( d2 .lt. d1 ) then
                  d = d2
                  if ( .not. abs_diff ) then
                  d = d2/b(i1)
                  endif
                  if ( d .le. tiny ) then
                    index(i1) = i
                    icount = icount + 1
                  endif
                endif
              exit
              endif
              enddo ! i
              i1 = i1 + 1
              enddo ! while
              ! if element of array b is outside of range array a values
              if ( b(1) .lt. a(1) ) then
                d1 = a(1) - b(1)
                d = d1
                if ( .not. abs_diff ) then
                d = d1/b(1)
                endif
                if ( d .le. tiny ) then
                  index(1) = 1
                  icount = icount + 1
                endif
              endif
              if ( b(nb) .gt. a(na) ) then
                d1 = b(nb) - a(na)
                d = d1
                if ( .not. abs_diff ) then
                d = d1/b(nb)
                endif
                if ( d .le. tiny ) then
                  index(nb) = na
                  icount = icount + 1
                endif
              endif
            endif

      ! Validate index values
            do i=1,nb
              if(index(i) .eq. -999) then
                !commented write statements were moved to calling routines
                !write(*,'(/,2(a,i0,2x),a)') 'i = ',i,'index = ',index(i),'in R_subset_index'
                !write(*,'(a,/)') ' !!!  Bad index for array b element  !!!'
                return
              endif
            enddo
            if ( nb .gt. 1 ) then
              do i=2,nb
                if ( index(i-1) .eq. index(i) ) then
                  write(*,'(/,a,i0,2x,a)') 'Array b, found repeated index i = ',index(i),'in R_subset_index'
                  write(*,'(a,10i5)') index(1:nb)
                  write(*,'(a,/)') '!!! Indices can not be repeated in R_subset_index'
                  return
                endif
              enddo
            endif

      ! Validate if indices found for all array b elements
            if ( icount .ne. nb ) then
              write(*,'(/,2(a,i0,2x),a,/)') 'nb = ',nb,'icount = ',icount, &
              'in R_subset_index !!!  Can not find index for array b element  !!!'
              return
            endif

            status = .true.
      !      write(*,*) index
            return
            end subroutine R_subset_index
! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

