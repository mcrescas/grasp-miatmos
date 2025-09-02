! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **


module mod_c_utils

  use iso_fortran_env
  use iso_c_binding
  implicit none

  ! interface to some C standard routines (shall be extended later)
  interface
     function strlen(cstring) bind(c)
       use iso_c_binding
       ! IMPORTANT: one must use cstring(*) and not cstring(:) here;
       ! an assumed shape array would corrupt the stack of strlen;
       ! this rule applies to any parameter of a foreign (non Fortran)
       ! routine or function
       character(kind=C_CHAR),  intent(in) :: cstring(*)
       integer(kind=C_SIZE_T) :: strlen
     end function strlen
  end interface

  contains
    subroutine print_cstring(cstring, with_quotes, with_newline, file_unit)
      character(kind=C_CHAR),  intent(in) :: cstring(*)
      logical, intent(in), optional :: with_quotes
      logical, intent(in), optional :: with_newline
      integer, intent(in), optional :: file_unit
      logical :: with_quotes_
      logical :: with_newline_
      integer :: file_unit_
      integer(kind=C_SIZE_T) :: length
      integer :: i

      if (present(with_quotes)) then
        with_quotes_ = with_quotes
      else
        with_quotes_ = .false.
      end if

      if (present(file_unit)) then
        file_unit_ = file_unit
      else
        file_unit_ = output_unit
      end if

      if (with_quotes_) write(file_unit_, '("""")', advance='no')

      length = strlen(cstring)
      
      do i = 1, length
        write(file_unit_, '(A)', advance='no') cstring(i)
      end do

      if (with_quotes_) then
        write(file_unit_, '(""" (",I0,")")', advance='no') length
      else
        write(file_unit_, '(" (",I0,")")', advance='no') length
      end if

      if (.not. present(with_newline)) then
        with_newline_ = .true.
      else
        with_newline_ = with_newline
      end if

      if (with_newline_) then
        write(file_unit_, '("")')
      end if

    end subroutine print_cstring

    ! Copyright (C) 2012, 2013 Fabrice Ducos, fabrice.ducos@univ-lille1.fr
    ! F. Ducos, 30 August 2013: fstring is no longer allocatable in cstring2fstring    
    ! a routine to convert cstrings (char *) into fortran strings
    subroutine cstring2fstring(cstring, fstring)
      character(kind=C_CHAR),  intent(in) :: cstring(*)
      character(kind=C_CHAR,len=*), intent(out) :: fstring

      integer(kind=C_SIZE_T) :: length
      integer(kind=C_SIZE_T) :: i

      length = strlen(cstring)

      if (length > len(fstring)) then
        write(error_unit, '("mod_c_utils.f90: cstring2fstring: the Fortran destination string (",I0," characters) is too small for the C source string (",I0," characters)")') len(fstring), length
        write(error_unit, '("mod_c_utils.f90: cstring2fstring: cstring = ")', advance = 'no')
        call print_cstring(cstring, with_quotes = .true., with_newline = .true., file_unit = error_unit)
        write(error_unit, '("mod_c_utils.f90: cstring2fstring: it looks like no NULL character was found in the first ",I0," characters of the cstring")') len(fstring)
        stop 1
      end if

      do i = 1, length
         fstring(i:i) = cstring(i)
      end do

      do i = length + 1, len(fstring)
         fstring(i:i) = ' '
      end do
      
    end subroutine cstring2fstring
    
    subroutine fstring2cstring(fstring, cstring_size, cstring, keep_trailing_spaces)
      character(kind=C_CHAR, len=*), intent(in) :: fstring
      integer(kind=C_SIZE_T), intent(in), value :: cstring_size ! includes the terminating NULL char
      character(kind=C_CHAR), intent(out) :: cstring(*)
      logical, intent(in), optional :: keep_trailing_spaces

      integer(kind=C_SIZE_T) :: i
      integer(kind=C_SIZE_T) :: fstring_len
      integer(kind=C_SIZE_T) :: min_len
      logical :: keep_trailing_spaces_

      keep_trailing_spaces_ = .false.
      if ( present(keep_trailing_spaces) ) then
         keep_trailing_spaces_ = keep_trailing_spaces
      end if

      if (keep_trailing_spaces_ .eqv. .false.) then
         fstring_len = len_trim(fstring)
      else
         fstring_len = len(fstring)
      end if

      min_len=min(fstring_len, cstring_size - 1)

      do i = 1, min_len
         cstring(i) = fstring(i:i)
      end do
      
      cstring(min_len + 1) = C_NULL_CHAR
      
    end subroutine fstring2cstring    


end module mod_c_utils
