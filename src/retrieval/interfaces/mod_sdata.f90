! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

#include "../constants_set/mod_globals.inc"
module mod_sdata

      use mod_time_utils
      use mod_index_cloud
      use mod_sdata_meas_type
      use mod_sdata_derived_type
      use mod_stop_report

      implicit none

contains

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

   subroutine print_array_int(array, element_format, from_element, to_element)
     integer, intent(in) :: array(:)
     character(*), intent(in) :: element_format
     integer, intent(in), optional :: from_element
     integer, intent(in), optional :: to_element
     integer :: i
     integer :: from_element_
     integer :: to_element_
     integer :: nelements

     if (present(from_element)) then
        from_element_ = from_element
     else
        from_element_ = 1
     end if

     if (present(to_element)) then
        to_element_ = to_element
     else
        to_element_ = size(array)
     end if

     nelements = to_element_ - from_element_ + 1
     write(*, '("[",I0,"/",I0,"]")', advance='no') nelements, size(array)

     
     do i = from_element_, to_element_
        write(*,'(" ")', advance='no')
        write(*,fmt=element_format, advance='no') array(i)
        write(*,'(" ")', advance='no')
     end do
     write(*,*)
     
   end subroutine print_array_int

   subroutine print_array_real(array, element_format, from_element, to_element)
     real(kind=C_FLOAT), intent(in) :: array(:)
     character(*), intent(in) :: element_format
     integer, intent(in), optional :: from_element
     integer, intent(in), optional :: to_element
     integer :: i
     integer :: from_element_
     integer :: to_element_
     integer :: nelements

     if (present(from_element)) then
        from_element_ = from_element
     else
        from_element_ = 1
     end if

     if (present(to_element)) then
        to_element_ = to_element
     else
        to_element_ = size(array)
     end if

     nelements = to_element_ - from_element_ + 1
     write(*, '("[",I0,"/",I0,"]")', advance='no') nelements, size(array)
     do i = from_element_, to_element_
        write(*,'(" ")', advance='no')
        write(*,fmt=element_format, advance='no') array(i)
        write(*,'(" ")', advance='no')
     end do
     write(*,*)
     
   end subroutine print_array_real

   subroutine print_data_wl(label, dwl)
     character(*), intent(in) :: label
     type(data_wl), intent(in) :: dwl
     integer :: ip
     !integer :: ivm

     write(*,'(A,"%NIP: ",I0)') trim(label), dwl%NIP

     write(*,'(A,"%NBVM(ip=1:",I0,"): ")', advance='no') trim(label), dwl%NIP
     call print_array_int(dwl%NBVM, "(I0)", 1, dwl%NIP)

     
     write(*,'(A,"%meas_type(ip=1:",I0,"): ")', advance='no') trim(label), dwl%NIP
     call print_array_int(dwl%meas_type,"(I0)", 1, dwl%NIP)

     write(*,'(A,"%wl: ",F9.3)') trim(label), dwl%wl
     write(*,'(A,"%ind_wl: ",I0)') trim(label), dwl%ind_wl
     write(*,'(A,"%sza: ",F9.3)') trim(label), dwl%sza

     do ip = 1, dwl%NIP
        if(dwl%meas_type(ip) .lt. meas_type_lid_beg .or. dwl%meas_type(ip) .gt. meas_type_lid_end ) then
          write(*,'(A,"%thetav(ivm=1:",I0,",ip=",I0,"): ")', advance='no') trim(label), dwl%NBVM(ip), ip
          call print_array_real(dwl%thetav(:,ip),"(E12.3)", 1, dwl%NBVM(ip))
        !else
          !write(*,'(A,"%HVP(ivm=1:",I0,",ip=",I0,"): ")', advance='no') trim(label), dwl%NBVM(ip), ip
          !call print_array_real(dwl%thetav(:,ip),"(E12.3)", 1, dwl%NBVM(ip))
        end if        
     end do     

     do ip = 1, dwl%NIP
        if(dwl%meas_type(ip) .lt. meas_type_lid_beg .or. dwl%meas_type(ip) .gt. meas_type_lid_end ) then
          write(*,'(A,"%phi(ivm=1:",I0,",ip=",I0,"): ")', advance='no') trim(label), dwl%NBVM(ip), ip
          call print_array_real(dwl%phi(:,ip),"(E12.3)", 1, dwl%NBVM(ip))
        endif
     end do

     write(*,'(A,"%Nsurf: ",I0)') trim(label), dwl%Nsurf
     write(*,'(A,"%groundpar(isurf=1:",I0,"): ")', advance='no') trim(label), dwl%Nsurf
     call print_array_real(dwl%groundpar,"(E12)", 1, dwl%Nsurf)
     
     write(*,'(A,"%gaspar: ",F9.3)') trim(label), dwl%gaspar

     do ip = 1, dwl%NIP
        select case (dwl%meas_type(ip))
        case(MEAS_TYPE_TOD)
           write(*,'(A,"%tod: ")', advance='no') trim(label)
           call print_array_real(dwl%tod, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_HTOD)
           write(*,'(A,"%htod: ")', advance='no') trim(label)
           call print_array_real(dwl%htod, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_AOD)
           write(*,'(A,"%aod: ")', advance='no') trim(label)
           call print_array_real(dwl%aod, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_AAOD)
           write(*,'(A,"%abs: ")', advance='no') trim(label)
           call print_array_real(dwl%aaod, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P11)
           write(*,'(A,"%p11: ")', advance='no') trim(label)
           call print_array_real(dwl%p11, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P11_REL_ANG)
           write(*,'(A,"%p11_rel_ang: ")', advance='no') trim(label)
           call print_array_real(dwl%p11_rel_ang, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P11_INTD)
           write(*,'(A,"%p11_intd: ")', advance='no') trim(label)
           call print_array_real(dwl%p11_intd, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P11_INTD_CUT_OFF_1)
           write(*,'(A,"%p11_intd_cut_off_1: ")', advance='no') trim(label)
           call print_array_real(dwl%p11_intd_cut_off_1, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P11_INTD_CUT_OFF_2)
           write(*,'(A,"%p11_intd_cut_off_2: ")', advance='no') trim(label)
           call print_array_real(dwl%p11_intd_cut_off_2, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P11_INTD_CUT_OFF_3)
           write(*,'(A,"%p11_intd_cut_off_3: ")', advance='no') trim(label)
           call print_array_real(dwl%p11_intd_cut_off_3, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P11_INTD_CUT_OFF_4)
           write(*,'(A,"%p11_intd_cut_off_4: ")', advance='no') trim(label)
           call print_array_real(dwl%p11_intd_cut_off_4, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P12)
           write(*,'(A,"%p12: ")', advance='no') trim(label)
           call print_array_real(dwl%p12, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P12_REL)
           write(*,'(A,"%p12_rel: ")', advance='no') trim(label)
           call print_array_real(dwl%p12, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P22)
           write(*,'(A,"%p22: ")', advance='no') trim(label)
           call print_array_real(dwl%p22, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P33)
           write(*,'(A,"%p33: ")', advance='no') trim(label)
           call print_array_real(dwl%p33, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P34)
           write(*,'(A,"%p34: ")', advance='no') trim(label)
           call print_array_real(dwl%p34, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P44)
           write(*,'(A,"%p44: ")', advance='no') trim(label)
           call print_array_real(dwl%p44, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_LS)
           write(*,'(A,"%LS: ")', advance='no') trim(label)
           call print_array_real(dwl%LS, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_RL)
           write(*,'(A,"%RL: ")', advance='no') trim(label)
           call print_array_real(dwl%RL, "(E12.3)", 1, dwl%NBVM(ip))
           case(MEAS_TYPE_VEXT)
        case(MEAS_TYPE_DPAR)
           write(*,'(A,"%DPAR: ")', advance='no') trim(label)
           call print_array_real(dwl%DPAR, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_DPER)
           write(*,'(A,"%DPER: ")', advance='no') trim(label)
           call print_array_real(dwl%DPER, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_DP)
           write(*,'(A,"%DP: ")', advance='no') trim(label)
           call print_array_real(dwl%DP, "(E12.3)", 1, dwl%NBVM(ip))
           write(*,'(A,"%VEXT: ")', advance='no') trim(label)
           call print_array_real(dwl%VEXT, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_VBS)
            write(*,'(A,"%VBS: ")', advance='no') trim(label)
            call print_array_real(dwl%VBS, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_I)
           write(*,'(A,"%I: ")', advance='no') trim(label)
           call print_array_real(dwl%I, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_I_REL_SUM)
           write(*,'(A,"%I_rel_sum: ")', advance='no') trim(label)
           call print_array_real(dwl%I_rel_sum, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_Q)
           write(*,'(A,"%Q: ")', advance='no') trim(label)
           call print_array_real(dwl%Q, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_U)
           write(*,'(A,"%U: ")', advance='no') trim(label)
           call print_array_real(dwl%U, "(E12.3)", 1, dwl%NBVM(ip))
        case(MEAS_TYPE_P)
           write(*,'(A,"%P: ")', advance='no') trim(label)
           call print_array_real(dwl%P, "(E12.3)", 1, dwl%NBVM(ip))   
        case(MEAS_TYPE_P_REL)
           write(*,'(A,"%P_rel: ")', advance='no') trim(label)
           call print_array_real(dwl%P_rel, "(E12.3)", 1, dwl%NBVM(ip))
        case default
           write(tmp_message,'(a,i4)') &
           "Fatal error. Unexpected value for meas_type (I0). Please check the code.", &
           dwl%meas_type(ip)
           G_ERROR(trim(tmp_message))
        end select
     end do ! ip
     
     write(*,'(A,"%IFCOV(ip=1:",I0,"): ")', advance='no') trim(label), dwl%NIP
     call print_array_int(dwl%IFCOV, "(I0)", 1, dwl%NIP)

     write(*,'(A,"%IFMP(ip=1:",I0,"): ")', advance='no') trim(label), dwl%NIP
     call print_array_int(dwl%IFMP, "(I0)", 1, dwl%NIP)

     do ip = 1, dwl%NIP
        write(*,'(A,"%CMTRX(ivm=1:",I0,",ip=",I0,"): ")', advance='no') trim(label), &
             dwl%NBVM(ip)*dwl%IFCOV(ip), ip
        call print_array_real(dwl%CMTRX(:,ip),"(E12.3)", 1, dwl%NBVM(ip)*dwl%IFCOV(ip))
     end do

     do ip = 1, dwl%NIP
        write(*,'(A,"%MPROF(ivm=1:",I0,",ip=",I0,"): ")', advance='no') trim(label), &
             dwl%NBVM(ip)*dwl%IFMP(ip), ip
        call print_array_real(dwl%MPROF(:,ip),"(E12.3)", 1, dwl%NBVM(ip)*dwl%IFMP(ip))
     end do

   end subroutine print_data_wl

   subroutine print_pixel(label, one_pixel)
     character(*), intent(in) :: label
     type(pixel), intent(in) :: one_pixel
     integer :: iwl
     character(256) :: dwl_label
     character(20) :: readable_time
     
     call convert_time_to_string(one_pixel%t, '%FT%H:%M:%SZ', readable_time)

     write(*,'(A,"%HOBS: ",F11.3)') trim(label), one_pixel%HOBS
     write(*,'(A,"%nwl: ",I0)') trim(label), one_pixel%nwl
     write(*,'(A,"%cloudy: ",I0)') trim(label), one_pixel%cloudy
     write(*,'(A,"%x: ",F9.3)') trim(label), one_pixel%x
     write(*,'(A,"%y: ",F9.3)') trim(label), one_pixel%y
     write(*,'(A,"%t: ",I0," (",A,")")') trim(label), one_pixel%t, trim(readable_time)
     write(*,'(A,"%ix: ",I0)') trim(label), one_pixel%ix
     write(*,'(A,"%iy: ",I0)') trim(label), one_pixel%iy
     write(*,'(A,"%it: ",I0)') trim(label), one_pixel%it
     write(*,'(A,"%MASL: ",F9.3)') trim(label), one_pixel%MASL
     write(*,'(A,"%land_percent: ",F9.3)') trim(label), one_pixel%land_percent
     write(*,'(A,"%irow: ",I0)') trim(label), one_pixel%irow
     write(*,'(A,"%icol: ",I0)') trim(label), one_pixel%icol
     write(*,'(A,"%IFGAS: ",I0)') trim(label), one_pixel%IFGAS
     write(*,'(A,"%meas(iwl=1:",I0,"): ")', advance='no') trim(label), one_pixel%nwl
     write(*,*)
     do iwl = 1, one_pixel%nwl
        write(dwl_label, '(A,"%meas(",I0,")")') trim(label), iwl
        call print_data_wl(dwl_label, one_pixel%meas(iwl))
     end do
     write(*,'(A,"%HVP(KVERTM=1:",I0,"): ")', advance='no') trim(label), KVERTM
     call print_array_real(one_pixel%HVP, "(F0.1)")
     
   end subroutine print_pixel

   subroutine print_segment(label, segment)
     character(*), intent(in) :: label
     type(segment_data), intent(in) :: segment
     integer :: ipixel
     character(256) :: pixel_label

     write(*,'(A," npixels: ",I0," NX: ",I0," NY: ",I0," NT: ",I0," [KIMAGE: ",I0,"]")') &
          label, segment%npixels, segment%NX, segment%NY, segment%NT, KIMAGE
     do ipixel = 1, segment%npixels
        write(pixel_label, '(A,"%pixels(",I0,")")') trim(label), ipixel
        call print_pixel(pixel_label, segment%pixels(ipixel))
     end do
     
   end subroutine print_segment

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
      subroutine init_segment_vec ( npixels, segment_vec  )
	
!  ------------------------------------------------------------------------------------------------------
      implicit none
!  ------------------------------------------------------------------------------------------------------
      integer,                             intent(in)     ::  npixels
      type(pixel_vector),dimension(KIMAGE),intent(inout)  ::  segment_vec
!  ------------------------------------------------------------------------------------------------------
      integer                    ::  ipix
!  ------------------------------------------------------------------------------------------------------
      do ipix=1,npixels
         segment_vec(ipix)%nFS(:)  = 0
         segment_vec(ipix)%FS(:)   = 0.0
         segment_vec(ipix)%KMIMAGE = 0
      enddo ! iw

      return
      end subroutine init_segment_vec

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
   subroutine set_pixel_meas (            & ! IN
                              iw,         &
                              NBVM,       &
                              meas_type,  &
                              ind,        &
                              meas,	      & ! INOUT
                              pixel_cont, &
                              cutoff_meas_diff  &
                             )
	
      implicit none
!  ---------------------------------------------------------------------------------------
      integer,intent(in)					::	iw
      integer,intent(in)					::	NBVM
      integer,intent(in)					::	meas_type
      logical,intent(in),optional	 ::  cutoff_meas_diff
      integer,intent(in)					::	ind ! ind=1 meas  => pixel (set);
                                          ! ind=2 pixel => meas (get)
!  ---------------------------------------------------------------------------------------
      type(pixel),intent(inout)           :: pixel_cont
      real,dimension(NBVM),intent(inout)    :: meas
!	----------------------------------------------------------------------------------------
      integer :: i
      logical, save :: prnt = .true.
!	----------------------------------------------------------------------------------------
	
	  !if(size(meas).LT.NBVM) then
        !write(*,*) 'Size(meas)=',Size(meas),' .LT.  NBVM=',NBVM 
        !stop 'stop in set_pixel_meas' 
	  !endif

      select case(meas_type)
      case(meas_type_aod)
        if(ind .eq. 1) then
        pixel_cont%meas(iw)%aod(1:NBVM) = meas(1:NBVM)
        else
        meas(1:NBVM) = pixel_cont%meas(iw)%aod(1:NBVM)
        endif ! ind
      case(meas_type_tod)
        if(ind .eq. 1) then
        pixel_cont%meas(iw)%tod(1:NBVM) = meas(1:NBVM)
        else
        meas(1:NBVM) = pixel_cont%meas(iw)%tod(1:NBVM)
        endif ! ind
      case(meas_type_htod)
        if(ind .eq. 1) then
        pixel_cont%meas(iw)%htod(1:NBVM) = meas(1:NBVM)
        else
        meas(1:NBVM) = pixel_cont%meas(iw)%htod(1:NBVM)
        endif ! ind
      case(meas_type_aaod)
        if(ind .eq. 1) then
        pixel_cont%meas(iw)%aaod(1:NBVM) = meas(1:NBVM)
        else
        meas(1:NBVM) = pixel_cont%meas(iw)%aaod(1:NBVM)
        endif ! ind
      case(meas_type_p11)
        if(ind .eq. 1) then
        pixel_cont%meas(iw)%p11(1:NBVM) = meas(1:NBVM)
        else
        meas(1:NBVM) = pixel_cont%meas(iw)%p11(1:NBVM)
        endif ! ind
      case(meas_type_p11_rel_ang)
        if(ind .eq. 1) then
        pixel_cont%meas(iw)%p11_rel_ang(1:NBVM) = meas(1:NBVM)
        else
        meas(1:NBVM) = pixel_cont%meas(iw)%p11_rel_ang(1:NBVM)
        endif ! ind
      case(meas_type_p11_intd)
        if(ind .eq. 1) then
        pixel_cont%meas(iw)%p11_intd(1:NBVM) = meas(1:NBVM)
        else
        meas(1:NBVM) = pixel_cont%meas(iw)%p11_intd(1:NBVM)
        endif ! ind
      case(meas_type_p11_intd_cut_off_1)
        if(ind .eq. 1) then
        pixel_cont%meas(iw)%p11_intd_cut_off_1(1:NBVM) = meas(1:NBVM)
        else
        meas(1:NBVM) = pixel_cont%meas(iw)%p11_intd_cut_off_1(1:NBVM)
        endif ! ind
      case(meas_type_p11_intd_cut_off_2)
        if(ind .eq. 1) then
          pixel_cont%meas(iw)%p11_intd_cut_off_2(1:NBVM) = meas(1:NBVM)
          if (present(cutoff_meas_diff) .and. cutoff_meas_diff .eqv. .true.) then
          pixel_cont%meas(iw)%p11_intd_cut_off_2(1:NBVM) = meas(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_1(1:NBVM)
            do i=1,NBVM
            if (pixel_cont%meas(iw)%p11_intd_cut_off_2(i) .le. 1e-30) then
            write(tmp_message,'(3(a,i0,3x),a,es12.4,a)') &
            'iw = ',iw,'i = ',i,'meas_type = ',meas_type,'p11_intd_cut_off_2 =', &
            pixel_cont%meas(iw)%p11_intd_cut_off_2(i),' is too small'
            G_ERROR(trim(tmp_message))
            endif
            enddo ! i
            if ( prnt ) then
            write(*,'(/,a)') '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            write(*,'(a)')   'P11_CUT_OFF_N measurements for N>2 are calculated for radius range rmax_(N-1) rmax_(N)'
            write(*,'(a)')   'as p11_integrated_cutoff_N = p11_integrated_cutoff_N - '
            write(*,'(a)')   '                             sum(p11_integrated_cutoff_1:p11_integrated_cutoff_(N-1)))'
            write(*,'(a)')   '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            prnt = .false.
            endif
          endif
        else
          meas(1:NBVM) = pixel_cont%meas(iw)%p11_intd_cut_off_2(1:NBVM)
          if (present(cutoff_meas_diff) .and. cutoff_meas_diff .eqv. .true.) then
          meas(1:NBVM) = pixel_cont%meas(iw)%p11_intd_cut_off_2(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_1(1:NBVM)
            do i=1,NBVM
            if (meas(i) .le. 1e-30) then
            write(tmp_message,'(3(a,i0,3x),a,es12.4,a)') &
            'iw = ',iw,'i = ',i,'meas_type = ',meas_type,'meas =', &
            meas(i),' is too small'
            G_ERROR(trim(tmp_message))
            endif
            enddo ! i
            if ( prnt ) then
            write(*,'(/,a)') '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            write(*,'(a)')   'P11_CUT_OFF_N measurements for N>2 are calculated for radius range rmax_(N-1) rmax_(N)'
            write(*,'(a)')   'as p11_integrated_cutoff_N = p11_integrated_cutoff_N - '
            write(*,'(a)')   '                             sum(p11_integrated_cutoff_1:p11_integrated_cutoff_(N-1)))'
            write(*,'(a)')   '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            prnt = .false.
            endif
          endif
        endif ! ind
      case(meas_type_p11_intd_cut_off_3)
        if(ind .eq. 1) then
          pixel_cont%meas(iw)%p11_intd_cut_off_3(1:NBVM) = meas(1:NBVM)
          if (present(cutoff_meas_diff) .and. cutoff_meas_diff .eqv. .true.) then
          pixel_cont%meas(iw)%p11_intd_cut_off_3(1:NBVM) = meas(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_1(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_2(1:NBVM)
            do i=1,NBVM
            if (pixel_cont%meas(iw)%p11_intd_cut_off_3(i) .le. 1e-30) then
            write(tmp_message,'(4(a,i0,3x),a,es12.4,a)') 'ind = ',ind, &
            'i = ',i,'meas_type = ',meas_type,'p11_intd_cut_off_3 =', &
            pixel_cont%meas(iw)%p11_intd_cut_off_3(i),' is too small'
            G_ERROR(trim(tmp_message))
            endif
            enddo ! i
          endif
        else
          meas(1:NBVM) = pixel_cont%meas(iw)%p11_intd_cut_off_3(1:NBVM)
          if (present(cutoff_meas_diff) .and. cutoff_meas_diff .eqv. .true.) then
          meas(1:NBVM) = pixel_cont%meas(iw)%p11_intd_cut_off_3(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_1(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_2(1:NBVM)
            do i=1,NBVM
            if (meas(i) .le. 1e-30) then
            write(tmp_message,'(3(a,i0,3x),a,es12.4,a)') 'ind = ',ind, &
            'iw = ',iw,'i = ',i,'meas_type = ',meas_type,'p11_intd_cut_off_3 =', &
            meas(i),' is too small'
            G_ERROR(trim(tmp_message))
            endif
            enddo ! i
          endif
        endif ! ind
      case(meas_type_p11_intd_cut_off_4)
        if(ind .eq. 1) then
          pixel_cont%meas(iw)%p11_intd_cut_off_4(1:NBVM) = meas(1:NBVM)
          if (present(cutoff_meas_diff) .and. cutoff_meas_diff .eqv. .true.) then
          pixel_cont%meas(iw)%p11_intd_cut_off_4(1:NBVM) = meas(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_1(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_2(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_3(1:NBVM)
            do i=1,NBVM
            if (pixel_cont%meas(iw)%p11_intd_cut_off_4(i) .le. 1e-30) then
            write(tmp_message,'(4(a,i0,3x),a,es12.4,a)') 'ind = ',ind, &
            'iw = ',iw,'i = ',i,'meas_type = ',meas_type,'p11_intd_cut_off_4 =', &
            pixel_cont%meas(iw)%p11_intd_cut_off_4(i),' is too small'
            G_ERROR(trim(tmp_message))
            endif
            enddo ! i
          endif
        else
          meas(1:NBVM) = pixel_cont%meas(iw)%p11_intd_cut_off_4(1:NBVM)
          if (present(cutoff_meas_diff) .and. cutoff_meas_diff .eqv. .true.) then
          meas(1:NBVM) = pixel_cont%meas(iw)%p11_intd_cut_off_4(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_1(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_2(1:NBVM) - &
          pixel_cont%meas(iw)%p11_intd_cut_off_3(1:NBVM)
            do i=1,NBVM
            if (meas(i) .le. 1e-30) then
            write(tmp_message,'(4(a,i0,3x),a,es12.4,a)') 'ind = ',ind, &
            'iw = ',iw,'i = ',i,'meas_type = ',meas_type,'p11_intd_cut_off_4 =', &
            meas(i),' is too small'
            G_ERROR(trim(tmp_message))
            endif
            enddo ! i
          endif
        endif ! ind
      case(meas_type_p12)
         if(ind .eq. 1) then         
         pixel_cont%meas(iw)%p12(1:NBVM) = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%p12(1:NBVM)      
         endif ! ind
      case(meas_type_p12_rel)
         if(ind .eq. 1) then         
         pixel_cont%meas(iw)%p12_rel(1:NBVM) = meas(1:NBVM)
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%p12_rel(1:NBVM)
         endif ! ind
      case(meas_type_p22)
         if(ind .eq. 1) then         
         pixel_cont%meas(iw)%p22(1:NBVM) = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%p22(1:NBVM)      
         endif ! ind
      case(meas_type_p33)
         if(ind .eq. 1) then         
         pixel_cont%meas(iw)%p33(1:NBVM) = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%p33(1:NBVM)      
         endif ! ind
      case(meas_type_p34)
         if(ind .eq. 1) then         
         pixel_cont%meas(iw)%p34(1:NBVM) = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%p34(1:NBVM)      
         endif ! ind
      case(meas_type_p44)
         if(ind .eq. 1) then         
         pixel_cont%meas(iw)%p44(1:NBVM) = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%p44(1:NBVM)      
         endif ! ind
      case(meas_type_LS)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%LS(1:NBVM)  = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%LS(1:NBVM)      
         endif ! ind
      case(meas_type_DPAR)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%DPAR(1:NBVM)  = meas(1:NBVM)
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%DPAR(1:NBVM)
         endif ! ind
      case(meas_type_DPER)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%DPER(1:NBVM)  = meas(1:NBVM)
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%DPER(1:NBVM)
         endif ! ind
      case(meas_type_DP)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%DP(1:NBVM)  = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%DP(1:NBVM)      
         endif ! ind
      case(meas_type_RL)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%RL(1:NBVM)  = meas(1:NBVM)
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%RL(1:NBVM)
         endif ! ind
      case(meas_type_VEXT)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%VEXT(1:NBVM)  = meas(1:NBVM)
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%VEXT(1:NBVM)
         endif ! ind
      case(meas_type_VBS)
         if(ind .eq. 1) then
         pixel_cont%meas(iw)%VBS(1:NBVM)  = meas(1:NBVM)
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%VBS(1:NBVM)
         endif ! ind
      case(meas_type_I)
         if(ind .eq. 1) then  
         pixel_cont%meas(iw)%I(1:NBVM)   = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%I(1:NBVM)         
         endif ! ind
      case(meas_type_I_rel_sum)
         if(ind .eq. 1) then
         pixel_cont%meas(iw)%I_rel_sum(1:NBVM)   = meas(1:NBVM)
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%I_rel_sum(1:NBVM)
         endif ! ind
      case(meas_type_Q)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%Q(1:NBVM)   = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%Q(1:NBVM)      
         endif ! ind
      case(meas_type_U)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%U(1:NBVM)   = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%U(1:NBVM)      
         endif ! ind
      case(meas_type_P)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%P(1:NBVM)   = meas(1:NBVM)      
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%P(1:NBVM)      
         endif ! ind
      case(meas_type_P_rel)
         if(ind .eq. 1) then 
         pixel_cont%meas(iw)%P_rel(1:NBVM)   = meas(1:NBVM)
         else
         meas(1:NBVM) = pixel_cont%meas(iw)%P_rel(1:NBVM)
         endif ! ind
      case default
         write(tmp_message,'(2(a,i0,3x),a)') &
         'iw = ',iw,'meas_type = ',meas_type,'- Unknown value of meas_type'
         G_ERROR(trim(tmp_message))
      end select
            
   end subroutine set_pixel_meas

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Assign general wave length indices to pixel wave lengths
      subroutine set_segment_pixel_wl_index ( iu_main_output, RIN, segment )

      use mod_retr_settings_derived_type

      implicit none
!	---------------------------------------------------------------------------------------
      integer,                   intent(in)     :: iu_main_output
      type(retr_input_settings), intent(in)     :: RIN
      type(segment_data),        intent(inout)	:: segment
!  --------------------------------------------------------------------------------------
!  --------------------------------------------------------------------------------------
      integer :: nwl
      !real,   dimension(KWM)           :: wl   
      !integer,dimension(KWM)           :: ind_wl   
      real,   dimension(KW) :: wl
      integer,dimension(KW) :: ind_wl
      integer,dimension(KW) :: ind_wl_i ! AL initial wavelenght relative to raman
      integer :: iw,ipix
      real :: tiny = 1e-4
      logical :: abs_diff = .true.
      logical :: status
      character(len=20) :: str1, str2
      character(len=150) :: CFMT
!  --------------------------------------------------------------------------------------

      do ipix=1,segment%npixels

      call get_pixel_wl (  segment%pixels(ipix),  & ! IN
                           nwl,wl                 & ! out
                        )

      ! search if pixel wavelengths present in wavelength set for
      ! retrieved parameters RIN%WAVE(1:RIN%NW) and define their indices 
      ! ind_wl(1:nwl) in RIN%WAVE(1:RIN%NW) array
      call R_subset_index ( tiny, abs_diff,             &
                            RIN%NW, RIN%WAVE(1:RIN%NW), &
                            nwl, wl(1:nwl),             &
                            ind_wl(1:nwl), status       & ! OUT
                          )
      if(.not. status) then
        !do iw=1,nwl
          !if(ind_wl(iw) .eq. -999) then
            !write(*,'(/,2(a,i0,2x),a)') 'i = ',iw,'index = ',ind_wl(iw),'in R_subset_index'
            !write(*,'(a,/)') ' !!!  Bad index for array b element  !!!'
          !endif
        !enddo
        write(str1,*) RIN%NW
        write(str2,*) nwl
        CFMT = '(a,i0,2a,'//trim(adjustl(str1))//'es12.4,2a,'//trim(adjustl(str2))//'es12.4)'
        write(tmp_message,trim(CFMT)) &
        'Can not fill in indices for pixel wavelengths, ipix = ',ipix, &
        NEW_LINE('A'),'WL:      ',RIN%WAVE(1:RIN%NW), &
        NEW_LINE('A'),'wl_ipix: ',wl(1:nwl)
        G_ERROR(trim(tmp_message))
      endif

      if ( RIN%IPRI_additional_info ) then
      !if ( RIN%IPRI_verbose ) then
        if ( ipix .eq. 1 ) then
              write(iu_main_output,'(/,a)') 'in set_segment_pixel_wl_index:'
              write(iu_main_output,'(a)') 'Wavelengths for pixels from a set of wavelengths for retrieved parameters:'
        endif
        write(iu_main_output,'(a,i0)') 'ipix = ', ipix
        write(iu_main_output,'(10es12.4)')  'wl_ipix: ',wl(1:nwl)
        write(iu_main_output,'(10es12.4 )') 'WL:      ',(RIN%WAVE(ind_wl(iw)), iw=1,nwl)
      endif

      call set_pixel_wl_i (segment%pixels(ipix),&
                          ind_wl,               &
                          ind_wl_i, status      & ! OUT
                          )

      if(.not. status) then
        write(tmp_message,'(a,i0,4a)') &
        'set_pixel_wl_i failed at ipix = ',ipix, &
        NEW_LINE('A'),'Elastic lidar signal is not found for every raman signal', &
        NEW_LINE('A'),'Please, check data input'
        G_ERROR(trim(tmp_message))
      endif

      do iw=1,nwl
        if ( iw .gt. 1 ) then
        if ( ind_wl(iw) .le. ind_wl(iw-1) ) then
          if ( RIN%IPRI_verbose ) then
          write(iu_main_output,'(a)') 'in set_segment_pixel_wl_index'
          write(iu_main_output,'(4x,a)') 'Pixel wavelength indices in segment wavelength array:'
          write(iu_main_output,'(4x,a,i0)') 'pixel # ',ipix
          write(iu_main_output,'(10i10)') ind_wl(1:nwl)
          endif
          write(tmp_message,'(4a,2(a,i0,2x))') &
          'Problem assigning general wave length indices to pixel wave lengths;', &
          NEW_LINE('A'), &
          'wavelength index array is not in ascending order:', &
          NEW_LINE('A'), &
          'ind_wl(iw-1) = ',ind_wl(iw-1),'ind_wl(iw) = ',ind_wl(iw)
          G_ERROR(trim(tmp_message))
        endif
        endif
        segment%pixels(ipix)%meas(iw)%ind_wl = ind_wl(iw)
        segment%pixels(ipix)%meas(iw)%ind_wl_i = ind_wl_i(ind_wl(iw))
        !write(*,*) 'iw,ind_wl',iw,ind_wl(iw)
      enddo ! iw                              
!      stop 'in segment_pixel_wl_index'
      enddo ! ipix

      return
      end subroutine set_segment_pixel_wl_index

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

	subroutine get_pixel_wl (                  &
                              pixel_cont,    & ! IN
                              nwl,           & ! OUT
                              wl_val,        & 
                              ind_wl         &   
                           )
      implicit none
!	---------------------------------------------------------------------------------------
      type(pixel),intent(in)	         :: pixel_cont
!  --------------------------------------------------------------------------------------
      integer,             intent(out)            :: nwl
      real,dimension(:),   intent(out), optional  :: wl_val   
      integer,dimension(:),intent(out), optional  :: ind_wl   

!	----------------------------------------------------------------------------------------
      integer             ::  iw
!	  character(LEN=100)  ::  msg
!	----------------------------------------------------------------------------------------

      nwl = pixel_cont%nwl
      !if(size(wl_val).LT.nwl) then
         !write(*,*) 'Size(wl_val) .LT. NWL'
         !stop 'stop in get_pixel_wl'
      !endif

      do iw=1,nwl						
         if(present(wl_val)) wl_val(iw) = pixel_cont%meas(iw)%wl
         if(present(ind_wl)) ind_wl(iw) = pixel_cont%meas(iw)%ind_wl
      enddo ! iw
      
	end subroutine get_pixel_wl

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

	subroutine set_pixel_wl_i (              &
                              pixel_cont,    & ! IN
                              ind_wl,        &
                              ind_wl_i,      & ! OUT
                              status         &
                           )
      implicit none
! AL this function looks for an index of a shorter (or same) wavelenght corresponding to
! AL      elastic pair of every raman signal in pixel, and creates an indexing array, 
! AL      having index of the elastic of pair at place of raman 
! AL      so every time for iw of raman ind_wl_i(ind_wl(iw)) will point to a shifted (or at least same) wavelength
!	---------------------------------------------------------------------------------------
      type(pixel),intent(in)	         :: pixel_cont
      integer,dimension(:),intent(in)    :: ind_wl
!  --------------------------------------------------------------------------------------
!      integer,             intent(out)            :: nwl
!      real,dimension(:),   intent(out), optional  :: wl_val
      integer,dimension(:),intent(out):: ind_wl_i
      logical, intent(out)            :: status

!	----------------------------------------------------------------------------------------
      integer             :: iw, iw_i
      integer             :: nip, nwl
      integer             :: last_found_iwl_i
      integer             :: nRL, nLS
!	----------------------------------------------------------------------------------------

      nwl = pixel_cont%nwl
      ind_wl_i(1:nwl)=ind_wl(1:nwl) ! initialization if no raman shift occured it will have same values
      last_found_iwl_i=1 ! initialization
      nRL=0
      nLS=0

      do iw=1,nwl
         nip = pixel_cont%meas(iw)%NIP
         if (ANY(pixel_cont%meas(iw)%meas_type(1:nip) .EQ. meas_type_RL )) then ! counting all raman iw
            nRL=nRL+1
         elseif ((ANY(pixel_cont%meas(iw)%meas_type(1:nip) .EQ. meas_type_LS ))) then ! counting all lidar iw
            nLS=nLS+1
         endif
      enddo

 ! checking if everything correct
    if (nRL .GT. nLS) then
       write(*,*) 'nRL=', nRL
       write(*,*) 'nLS=', nLS
       status= .false.
       return
    endif

      do iw=1,nwl						
         if (ANY(pixel_cont%meas(iw)%meas_type(1:nip) .EQ. meas_type_RL )) then ! looking for raman iw
            do iw_i=iw,last_found_iwl_i,-1 ! going backwards
               if ((ANY(pixel_cont%meas(iw_i)%meas_type(1:nip) .EQ. meas_type_LS ))) then! looking for elastic at same or shorter wl
                  last_found_iwl_i=iw ! next time won't search whole array
                  ind_wl_i(iw)=ind_wl(iw_i)   ! placing index of elastic (initial) at place of raman
                  exit
                endif
            enddo !iw_i
          endif
      enddo ! iw
    status= .true.
	end subroutine set_pixel_wl_i

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

	subroutine get_pixel_geom (  iw,ip,        & ! IN
	                             pixel_cont,   &
	                             NBVM,         & ! OUT
                               sza,          & 
	                             thetav,       & 
                               phi           &  
                             )
      implicit none
!	---------------------------------------------------------------------------------------
      type(pixel),intent(in)   :: pixel_cont
      integer,intent(in)               :: iw,ip
!  --------------------------------------------------------------------------------------
      integer,intent(out)              :: NBVM
      real,   intent(out)              :: sza
      real,dimension(:),intent(out)    :: thetav   
      real,dimension(:),intent(out)    :: phi   
!	---------------------------------------------------------------------------------------
      integer             ::  iv
!	    character(LEN=100)  ::  msg
!	---------------------------------------------------------------------------------------
      thetav(:) = 0.
      phi(:)    = 0.
	  
      NBVM = pixel_cont%meas(iw)%NBVM(ip)	  
      !if(size(thetav).LT.NBVM) then
         !write(*,*) 'Size(thetav) .LT. NBVML'
         !stop 'stop in get_pixel_geom'
      !endif
      !if(size(phi).LT.NBVM) then
         !write(*,*) 'Size(phi) .LT. NBVML'
         !stop 'stop in get_pixel_geom'
      !endif
	  
      sza = pixel_cont%meas(iw)%sza
      do iv=1,NBVM
      thetav(iv) = pixel_cont%meas(iw)%thetav(iv,ip)
      phi(iv)    = pixel_cont%meas(iw)%phi(iv,ip)
      enddo ! iv

	end subroutine get_pixel_geom

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Fill in measurement vector for segment

      subroutine set_segment_meas_vector_FS ( RIN, INVSING, segment_meas, segment_vec_meas )
	  
      use mod_retr_settings_derived_type

      implicit none
!	----------------------------------------------------------------------------
! IN :	
      type(retr_input_settings), intent(in) ::  RIN
      integer,                   intent(in) ::  INVSING
      type(segment_data),        intent(in) ::  segment_meas
!	----------------------------------------------------------------------------
! INOUT :	
!	----------------------------------------------------------------------------
! OUT :
      type(pixel_vector),dimension(KIMAGE),intent(out) ::  segment_vec_meas
!	-----------------------------------------------------------------------------
! LOCAL :
      integer                              ::  IWb,IWe,ipix
      integer                              ::  npixels,KMIMAGE
!	-----------------------------------------------------------------------------
      npixels = segment_meas%npixels
      
      call init_segment_vec ( npixels, segment_vec_meas )

      IWB = 1
      do ipix=1,npixels
        IWE = segment_meas%pixels(ipix)%nwl
! Fill in pixel measurement vector
        call set_pixel_meas_vector_FS ( RIN,IWB,IWE,ipix,           & ! IN
                                        segment_meas%pixels(ipix),  & 
                                        segment_vec_meas(ipix)      & ! INOUT
                                      )
        if ( error_present() ) return
! Calculate the number of measurements in each pixel
        segment_vec_meas(ipix)%KMIMAGE = SUM(segment_vec_meas(ipix)%nFS(IWB:IWE))           
      enddo ! ipix

      return
      end subroutine set_segment_meas_vector_FS

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Add random noise to segment measurement vector
! INOISE  - the number of different noise sources              
! SGMS(I) - std of noise in i -th source                      
! INN(I)  - EQ.1.THEN error is absolute with                  
!         - EQ.0 THEN error assumed relative
! DNN(I)  - variation of the noise of the I-th source

      subroutine add_rnoise_segment ( RIN,deep_random_switch,  & ! IN
                                      segment_meas,            &  
                                      segment_vec_meas,        & ! INOUT
                                      MNOISEI                  & ! IN                                      
                                    ) 
	  
      use mod_retr_settings_derived_type
      
      implicit none
!	----------------------------------------------------------------------------
! IN :	
      type(retr_input_settings),        intent(in) ::  RIN
      logical,                          intent(in) ::  deep_random_switch
      type(segment_data),               intent(in) ::  segment_meas
      integer,dimension(KIP,KWM,KIMAGE),intent(in) ::  MNOISEI   
!	----------------------------------------------------------------------------
! INOUT :	
      type(pixel_vector),dimension(KIMAGE), intent(inout) ::  segment_vec_meas
!	----------------------------------------------------------------------------
! LOCAL :
      integer                              ::  ipix, JJS, i
      integer                              ::  npixels
!	------------------------------------------------------------------------------------------
      npixels = segment_meas%npixels

! Disturb measurements with random noise
      do ipix=1,npixels
        JJS = SUM(segment_vec_meas(ipix)%nFS(1:segment_meas%pixels(ipix)%nwl))      
        if(RIN%KL .eq. 1) &
        segment_vec_meas(ipix)%FS(1:JJS) = EXP(segment_vec_meas(ipix)%FS(1:JJS))
        call add_rnoise_pixel ( deep_random_switch,          & ! IN
                                RIN, MNOISEI(:,:,ipix),      &
                                segment_meas%pixels(ipix),   & 
                                segment_vec_meas(ipix)       & ! INOUT
                              )
        if(RIN%KL .eq. 1) then
        if(RIN%IPRI_verbose .eqv. .true.) then
          do i=1,JJS
            if(segment_vec_meas(ipix)%FS(i) .le. 0.0) then
              write(tmp_message,'(2(a,i0,2x),a,es12.4,2(x,a))') &
              'ipix = ',ipix,'i_meas = ',i, &
              'FS_meas =',segment_vec_meas(ipix)%FS(i),'<=0 after add_rnoise_pixel;', &
              'minimization_convention: logarithm'
              G_ERROR(trim(tmp_message))
            endif
          enddo
        endif
        segment_vec_meas(ipix)%FS(1:JJS) = LOG(segment_vec_meas(ipix)%FS(1:JJS))
        endif
      enddo ! ipix

      return
      end subroutine add_rnoise_segment

! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss 

! ***************************************************
! **       Calculating the number of 
! **       the measurements KM,
! **       preparing vector of measurements FP 
! ***************************************************

      subroutine set_pixel_meas_vector_FS ( RIN, IWb, IWe, ipix, & ! IN
                                            pixel_cont,          & ! IN
                                            pixel_vec            & ! INOUT
                                          )
	  
      use mod_retr_settings_derived_type
      use mod_inversion_utils, only  : check_nan

      implicit none
!	----------------------------------------------------------------------------
! IN :	
      type(retr_input_settings),  intent(in) :: RIN
      integer,                    intent(in) :: IWb,IWe,ipix
      type(pixel),                intent(in) :: pixel_cont
!	----------------------------------------------------------------------------
! INOUT :	
      type(pixel_vector),         intent(inout) :: pixel_vec
!	----------------------------------------------------------------------------
! OUT :		
!	-----------------------------------------------------------------------------
! LOCAL :
      integer             ::  NWL, NIP, meas_type
      integer             ::  JJS, IW, IV, IP, JJSb
      real                ::  meas, I, Q2, U2 
      integer             ::  nFS
      logical             ::  LERR
!	-----------------------------------------------------------------------------
! iPOBS = 
!           1    I, Q, U  or P11,P12,P22,P33,P34,P44                              - absolute_polarization_components
!           2    I, Q/I, U/I  or p11,-P12/P11,P22/p11,P33/P11,P34/P11,P44/P11     - relative_polarization_components
!           3    I, P    or sqrt(Q*Q+U*U)                                         - polarized_reflectance
!           4    I, P/I  or sqrt(Q*Q+U*U)/I                                       - degree_of_polarization
! delete?   5    I, P/I meas or p11, -P12/P11, P22/p11, P33/P11, P34/P11, P44/P11 - relative_phase_matrix_meas
!
! Types of measurements :
! 11 - tod(wl)       - total optical depth (aer+mol+gas)
! 12 - aod(wl)       - aerosol optical depth
! 13 - aaod(wl)      - aerosol absorption optical depth
! 14 - htod(wl)      - hyperspectral total optical depth (aer+mol+gas)
!
! 21 - p11(angle,wl)  - phase matrix element p11
! 22 - p12(angle,wl)  - phase matrix element p12
! 23 - p22(angle,wl)  - phase matrix element p22
! 24 - p33(angle,wl)  - phase matrix element p33
! 25 - p34(angle,wl)  - phase matrix element p34
! 26 - p44(angle,wl)  - phase matrix element p44
! 27 - p11_rel_ang(angle,wl)  - phase matrix element p11 devided by P11 at given angle
! 28 - p12_rel(angle,wl)  - (-p12(angle,wl)/p11(angle,wl))

! 31  - LS(height,wl)  - lidar signal
! 32  - LR(height,wl)  - Raman lidar signal
! 33  - DPAR(height,wl)  - depolarization ratio
! 34  - DPER(height,wl)  - depolarization ratio
! 35  - DP(height,wl)  - depolarization ratio
! 36  - VEXT(height,wl)   - Vertical EXTinctin profile
! 39  - VBS(height,wl)    - Vertical BackScatter profile (e.g. from sondes)

! 41 - I(angle,wl)    - Stokes parameter I
! 42 - Q(angle,wl)    - Stokes parameter Q
! 43 - U(angle,wl)    - Stokes parameter U
! 44 - P(angle,wl)    - polarization sqrt(Q*Q+U*U) or P/I(iPOBS=5) 
! 45 - I_rel_sum(angle,wl)    - Stokes parameter I(angle,wl)/sum(I(:,wl) )
! 46 - P_rel(angle,wl)    - P_rel(angle,wl)/I(angle,wl) if set of angles for P is different from one for I

! 51 - p11_intd(angle,wl)   - p11 phase matrix element integrated in scattering angle range [ang1,ang2]
! 52 - p11_intd_cut_off_1(angle,wl) - p11 phase matrix element integrated in scattering angle range [ang1,ang2] for rmax1
! 53 - p11_intd_cut_off_2(angle,wl) - p11 phase matrix element integrated in scattering angle range [ang1,ang2] for rmax2
! 54 - p11_intd_cut_off_3(angle,wl) - p11 phase matrix element integrated in scattering angle range [ang1,ang2] for rmax3
! 55 - p11_intd_cut_off_4(angle,wl) - p11 phase matrix element integrated in scattering angle range [ang1,ang2] for rmax4

!	-----------------------------------------------------------------------------

      JJS  = SUM(pixel_vec%nFS(1:IWb))-pixel_vec%nFS(IWb)
      JJSb = JJS

      NWL = pixel_cont%NWL
      LOOP_WL : do iw=IWb,IWe
      nFS = 0
      NIP = pixel_cont%meas(iw)%NIP
      LOOP_meas_type : do IP=1,NIP
         meas_type = pixel_cont%meas(iw)%meas_type(IP)
         if(RIN%iPOBS .ge. 3 .and. meas_type .eq. meas_type_U) &
         exit LOOP_meas_type
         do IV=1,pixel_cont%meas(iw)%NBVM(IP)
         JJS=JJS+1 
         select case(meas_type)
         case(meas_type_aod)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%aod(IV)
            nFS = nFS+1
         case(meas_type_tod)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%tod(IV)
            nFS = nFS+1
         case(meas_type_htod)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%htod(IV)
            nFS = nFS+1
         case(meas_type_aaod)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%aaod(IV)
            nFS = nFS+1
         case(meas_type_p11)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p11(IV)  
            nFS = nFS+1
         case(meas_type_p11_rel_ang)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p11_rel_ang(IV)
            nFS = nFS+1
         case(meas_type_p11_intd)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p11_intd(IV)
            nFS = nFS+1
         case(meas_type_p11_intd_cut_off_1)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p11_intd_cut_off_1(IV)
            nFS = nFS+1
         case(meas_type_p11_intd_cut_off_2)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p11_intd_cut_off_2(IV)
            nFS = nFS+1
         case(meas_type_p11_intd_cut_off_3)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p11_intd_cut_off_3(IV)
            nFS = nFS+1
         case(meas_type_p11_intd_cut_off_4)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p11_intd_cut_off_4(IV)
            nFS = nFS+1
         case(meas_type_p12)
            select case(RIN%iPOBS)
            case(1,5)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p12(IV) + RIN%SHIFT
            case(2)
              pixel_vec%FS(JJS) = -pixel_cont%meas(iw)%p12(IV)/pixel_cont%meas(iw)%p11(IV) + RIN%SHIFT
            end select
            nFS = nFS+1
         case(meas_type_p12_rel)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p12_rel(IV) + RIN%SHIFT
            nFS = nFS+1
         case(meas_type_p22)
            select case(RIN%iPOBS)
            case(1,5)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p22(IV)
            case(2)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p22(IV)/pixel_cont%meas(iw)%p11(IV)
            end select
            nFS = nFS+1
         case(meas_type_p33)
            select case(RIN%iPOBS)
            case(1,5)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p33(IV) + RIN%SHIFT
            case(2)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p33(IV)/pixel_cont%meas(iw)%p11(IV) + RIN%SHIFT
            end select
            nFS = nFS+1
         case(meas_type_p34)
            select case(RIN%iPOBS)
            case(1,5)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p33(IV) + RIN%SHIFT
            case(2)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p33(IV)/pixel_cont%meas(iw)%p11(IV) + RIN%SHIFT
            end select
            nFS = nFS+1
         case(meas_type_p44)
            select case(RIN%iPOBS)
            case(1,5)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p44(IV) + RIN%SHIFT
            case(2)
              pixel_vec%FS(JJS) = pixel_cont%meas(iw)%p44(IV)/pixel_cont%meas(iw)%p11(IV) + RIN%SHIFT
            end select
            nFS = nFS+1
         case(meas_type_LS)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%LS(IV)
            nFS = nFS+1
         case(meas_type_RL)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%RL(IV)
            nFS = nFS+1
         case(meas_type_DPAR)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%DPAR(IV)
            nFS = nFS+1
         case(meas_type_DPER)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%DPER(IV)
            nFS = nFS+1
         case(meas_type_DP)
 !      WRITE(*,*),'select nFS, meas_type=',meas_type
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%DP(IV)
    !        WRITE(*,*),'meas=',pixel_cont%meas(iw)%DP(IV)
            nFS = nFS+1
         case(meas_type_VEXT)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%VEXT(IV)
            nFS = nFS+1
         case(meas_type_VBS)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%VBS(IV)
            nFS = nFS+1
         case(meas_type_I)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%I(IV)
            nFS = nFS+1
         case(meas_type_I_rel_sum)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%I_rel_sum(IV)
            nFS = nFS+1
         case(meas_type_Q)
            select case(RIN%iPOBS)
            case(1) 
               pixel_vec%FS(JJS) = pixel_cont%meas(iw)%Q(IV) + RIN%SHIFT
            case(2) 
               I = pixel_cont%meas(iw)%I(IV)
               pixel_vec%FS(JJS) = pixel_cont%meas(iw)%Q(IV)/I + RIN%SHIFT
            case(3) 
               meas = pixel_cont%meas(iw)%Q(IV)
               Q2 = meas*meas
               meas = pixel_cont%meas(iw)%U(IV)
               U2 = meas*meas
               pixel_vec%FS(JJS) = sqrt(Q2+U2)
            case(4)
               I = pixel_cont%meas(iw)%I(IV)
               meas = pixel_cont%meas(iw)%Q(IV)
               Q2 = meas*meas
               meas = pixel_cont%meas(iw)%U(IV)
               U2 = meas*meas
               pixel_vec%FS(JJS) = sqrt(Q2+U2)/I
            end select
            nFS = nFS+1
         case(meas_type_U)
            select case(RIN%iPOBS)
            case(1) 
               pixel_vec%FS(JJS) = pixel_cont%meas(iw)%U(IV) + RIN%SHIFT
            case(2) 
               I = pixel_cont%meas(iw)%I(IV)
               pixel_vec%FS(JJS) = pixel_cont%meas(iw)%U(IV)/I + RIN%SHIFT
            end select
            nFS = nFS+1
         case(meas_type_P)
            select case(RIN%iPOBS)
            case(3)
!tl 25-02-2015
! in case when number of angles or angle values for I differe from ones for P
! linear polarization has been divided by I at the time of setting
! modeled measurements in subroutine set_pixel_Stokes_vec_fit
               pixel_vec%FS(JJS) = pixel_cont%meas(iw)%P(IV)
            case(4) 
               I = pixel_cont%meas(iw)%I(IV)
               pixel_vec%FS(JJS) = pixel_cont%meas(iw)%P(IV)/I
            case default
               write(tmp_message,'(a,i0,a)') 'RIN%iPOBS = ',RIN%iPOBS,'  - value is not valid'
               G_ERROR(trim(tmp_message))
            end select
            nFS = nFS+1
         case(meas_type_P_rel)
            pixel_vec%FS(JJS) = pixel_cont%meas(iw)%P_rel(IV)
            nFS = nFS+1
         case default
            write(tmp_message,'(a,i0,a)') 'meas_type = ',meas_type,'  - unknown value of meas_type'
            G_ERROR(trim(tmp_message))
         end select ! meas_type
         if(RIN%KL .eq. 1) then
            if(pixel_vec%FS(JJS) .le. 0.0) then
              if( (meas_type .ge. meas_type_Q   .and. meas_type .le. meas_type_U .and. RIN%iPOBS .le. 2) .or.  &
                  (meas_type .eq. meas_type_p12) .or. (meas_type .eq. meas_type_p12_rel) .or. &
                  (meas_type .ge. meas_type_p33 .and. meas_type .le. meas_type_p44)     &
                ) then
                write(tmp_message,'(a,2(2x,a,es11.4),a,4(a,i0))') &
                '!!! Measurement vector problem  FS <=0 !!!','  FS=F+shift =',pixel_vec%FS(JJS), &
                'shift for applying logarithm to negative measurements =',RIN%SHIFT, &
                NEW_LINE('A'), &
                'pixel # ',ipix,'  wl # ',iw,'  meas type # ',ip,'  meas # ',iv
                G_ERROR(trim(tmp_message))
              else
                write(tmp_message,'(a,2x,a,es11.4,a,4(a,i0))') '!!! Measurement vector problem FS<=0 !!!', &
                'FS =',pixel_vec%FS(JJS), &
                NEW_LINE('A'), &
                'pixel # ',ipix,'  wl # ',iw,'  meas type # ',ip,'  meas # ',iv
                G_ERROR(trim(tmp_message))
              endif
            endif
         endif ! KL .eq. 1
         enddo ! IV
      enddo LOOP_meas_type
      pixel_vec%nFS(iw) = nFS
      enddo LOOP_WL
      if(RIN%KL .eq. 1) then 
        !if(any(pixel_vec%FS(JJSb+1:JJS) .le. 0.0)) then
          !write(*,*) 'FS:'
          !write(*,'(10e13.5)') pixel_vec%FS(JJSb+1:JJS)
          !write(*,*) '!!! Meas vector FS<=0 problem !!! ipix=',ipix,'  in set_pixel_meas_vector_FS'
          !stop 'stop in set_pixel_meas_vector_FS'
        !endif
        iv = SUM(pixel_vec%nFS(IWb:IWe))
        LERR = check_nan(iv,pixel_vec%FS(JJSb+1:JJS))
        if(.not. LERR) then
          if(RIN%IPRI_verbose .eqv. .true.) then
            write(*,*) 'FS:'
            write(*,'(10e13.5)') pixel_vec%FS(JJSb+1:JJS)
          endif
          write(tmp_message,'(a,i0,2x,a)') '!!! Meas vector ln(FS)=NaN problem !!! ipix=',ipix, &
          'in set_pixel_meas_vector_FS'
          G_ERROR(trim(tmp_message))
        endif
      pixel_vec%FS(JJSb+1:JJS) = LOG(pixel_vec%FS(JJSb+1:JJS))
      endif ! RIN%KL .eq. 1
      return
      end subroutine set_pixel_meas_vector_FS

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Add random noise to measurements (simulated measurements)
      subroutine add_rnoise_pixel ( deep_random_switch, & ! IN
                                    RIN, MNOISEI,       &
                                    pixel_cont,         & 
                                    pixel_vec           & ! INOUT
                                  )
      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------
! IN :
      logical,                   intent(in) :: deep_random_switch	   
      type(retr_input_settings), intent(in) :: RIN
      type(pixel),               intent(in) :: pixel_cont
      integer,dimension(KIP,KWM),intent(in) :: MNOISEI   

! -----------------------------------------------------------------
! INOUT :	  	  
      type(pixel_vector), intent(inout) :: pixel_vec
! -----------------------------------------------------------------
! LOCAL :
      integer      :: iw,IP,iv,iMN,NWL,NIP,meas_type,JJS
! ----------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------
! Modeling RANDOM NOISE                      
! INOISE  - the number of different noise sources           
! SGMS(I) - std of noise in i -th source                    

      JJS = 0
      NWL = pixel_cont%nwl
LOOP_WL : do iw=1,NWL
      NIP = pixel_cont%meas(iw)%NIP
LOOP_meas_type : do IP=1,NIP
         meas_type = pixel_cont%meas(iw)%meas_type(IP)
         iMN = MNOISEI(IP,iw)  
         if(iMN .lt. 1 .or. iMN .gt. RIN%NOISE%INOISE) then
            write(tmp_message,'(2(a,i0),2a)') 'iMN = ',iMN,'  INOISE = ',RIN%NOISE%INOISE, &
            NEW_LINE('A'), &
            'Noise index is not in valid range 1<iMN<INOISE'
            G_ERROR(trim(tmp_message))
         endif !
         if(RIN%iPOBS .ge. 3 .and. meas_type .eq. meas_type_U) & ! fit sqrt(Q*Q+U*U), sqrt(Q*Q+U*U)P/I
         exit LOOP_meas_type

         if(RIN%NOISE%SGMS(iMN) .gt. 0.0) then
           do IV=1,pixel_cont%meas(iw)%NBVM(IP)
           JJS = JJS+1
             if( (meas_type .ge. meas_type_p33 .and. meas_type .le. meas_type_p44) .or. &
                  meas_type .eq. meas_type_p12 .or. meas_type .eq. meas_type_p12_rel) then
               pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
               call rnoise (deep_random_switch,RIN%NOISE%SGMS(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS))
               pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
             elseif( meas_type .eq. meas_type_Q .and. RIN%iPOBS .lt. 3) then
               pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
               call rnoise (deep_random_switch,RIN%NOISE%SGMS(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS))
               pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
             elseif( meas_type .eq. meas_type_U .and. RIN%iPOBS .lt. 3) then
               pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
               call rnoise (deep_random_switch,RIN%NOISE%SGMS(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS))
               pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
             else
!               WRITE(*,*) '----RANDOM NOISE ----',IP,iw,JJS
               call rnoise (deep_random_switch,RIN%NOISE%SGMS(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS))
             endif
           enddo ! IV
         else
           do IV=1,pixel_cont%meas(iw)%NBVM(IP)
           JJS = JJS+1
           enddo ! IV
         endif ! RIN%NOISE%SGMS(iMN) .gt. 0.0
enddo LOOP_meas_type
enddo LOOP_WL

      return
      end subroutine add_rnoise_pixel

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine rnoise (  deep_random_switch, & ! IN
                          SGMS,INN,           &
                          FS                  & ! INOUT
                        )
      implicit none
! -----------------------------------------------------------------
! IN :
      logical, intent(in) :: deep_random_switch
      real,    intent(in) :: SGMS
      integer, intent(in) :: INN	   
! -----------------------------------------------------------------
! INOUT :
      real,    intent(inout) :: FS
! -----------------------------------------------------------------
! LOCAL :
      real              :: EMG=0.
      real,dimension(1)  :: RDM
      real*8            :: RDM0(1)=0.4
      real              :: FS0
! ----------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------
! Modeling   RANDOM NOISE                       
! SGMS - std of noise in i -th source                 
! INN  - EQ.1.THEN error is absolute with             

         if (deep_random_switch) then
!  Random number generator to provide more deep randomization	
            call RDMU2(1,RDM0(1))
         endif ! end of if (deep_random_switch)
            
         call RDMG(RDM0(1), 1, EMG, SGMS, RDM(1))
         FS0 = FS
         select case(INN)
         case(0)
            FS = FS * (1.0+RDM(1))
         case(1)
            FS = FS + RDM(1)
         end select
         !write(*,'(2(a,i3),4(a,e12.5))') 'in rnoise: INN=',INN, &
         !               '  RDM0=',RDM0(1),'  RDM=',RDM(1),'  FS0=',FS0,'  FS=',FS
         
      return
      end subroutine rnoise

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
	function write_sdata_pixels (							      &
                                 sdata_sim_file,	&
                                 npixels,         &
                                 segment					&
                               )
		use m_inssor
!	**************************************************************************************
		character(*),            intent(in)		  ::	sdata_sim_file
		integer,                 intent(in)		  ::	npixels
		type(segment_data),      intent(inout)	::	segment
		logical		         ::	write_sdata_pixels
		integer					   ::	ii,jj,kk	
		integer					   ::	io_status
		integer,dimension(npixels)	                ::  ix_pool,iy_pool,it_pool 
		integer,dimension(npixels,npixels,npixels)  :: 	index_3d ! *** (IT,IX,IY)

		character(LEN=25)		::	iso8601_string
    integer(KIND_TIME)   :: time_sec
		integer					::	Nsurf,IFGAS
 		integer					::  NX,NY,NT
		logical         ::  status_funct 
!		real,dimension(max_num_wave_length_v2,max_num_valid_directions_v2)		::	FPSW
!	------------------------------------------------------------------------------------------------------
    integer  ::  id_sdata_like_file
		integer,dimension(:),allocatable			::	num_el_time_gr		! *** number of elements in each timegroup
!	------------------------------------------------------------------------------------------------------
		integer			::	it_group_begin,it_group_end
		integer			::	ix_group_begin,ix_group_end
		integer			::	iy_group_begin,iy_group_end

		integer			::	it,it_shift,icount_time_gr
!	------------------------------------------------------------------------------------------------------
		character(LEN=255),parameter 	::	timegroup_header =	&
						"   : NPIXELS  TIMESTAMP  HEIGHT_OBS(m)  NSURF  IFGAS"
		character(LEN=64) 	::	format_timegroup_hdr =	&
						'(/,i3,3x,a20,f11.2,2i4,3x,a,i5)'

!			trim(format_timegroup_hdr)
!	------------------------------------------------------------------------------------------------------
		integer 		:: dbg_msg_level
!	------------------------------------------------------------------------------------------------------
		dbg_msg_level 	= 12
!	------------------------------------------------------------------------------------------------------
		ix_pool	= 0
		iy_pool	= 0
		it_pool	= 0
!	------------------------------------------------------------------------------------------------------
		Nsurf = segment%pixels(1)%meas(1)%Nsurf
		IFGAS  = segment%pixels(1)%IFGAS

    NT = segment%NT
    if(.NOT. allocated(num_el_time_gr)) then
			allocate(num_el_time_gr(1:NT))
		endif
    NX = segment%NX
    NY = segment%NY
!	------------------------------------------------------------------------------------------------------
      open (newunit=id_sdata_like_file, FILE=trim(sdata_sim_file), &
      STATUS='replace',FORM='formatted')
!	------------------------------------------------------------------------------------------------------
!tl If time group consists of 2 pixels (ix=2 iy=1; ix=1 iy=2) sorting indexes does not
!tl work properly. The problem is not fixed (goto 333 - 333 continue).

		ii = 0
    num_el_time_gr(:) = 0
    icount_time_gr    = 1  
      
		do  ii = 1,segment%npixels
			it_pool(ii) = segment%pixels(ii)%it
			ix_pool(ii) = segment%pixels(ii)%ix
			iy_pool(ii) = segment%pixels(ii)%iy
			if (segment%pixels(ii)%it .EQ. 0 ) then
				write(*,*) "Error. it(",ii,") = ",segment%pixels(ii)%it
			endif
			if (segment%pixels(ii)%ix .EQ. 0 ) then
				write(*,*) "Error. ix(",ii,") = ",segment%pixels(ii)%ix
			endif
			if (segment%pixels(ii)%iy .EQ. 0 ) then
				write(*,*) "Error. iy(",ii,") = ",segment%pixels(ii)%iy
			endif
			! *** forming it,ix,iy matrix with numbers of corresponding
			! 	linear pixels set. Then we will use it,ix,iy from corresponding 
			!	i?_pool arrays to address pixels in any desired it,ix,iy order	
			index_3d (                        &
                segment%pixels(ii)%it,  &
                segment%pixels(ii)%ix,  &
                segment%pixels(ii)%iy   &
               ) = ii 
!write(*,*) ii,segment%pixels(ii)%it,segment%pixels(ii)%ix,segment%pixels(ii)%iy,index_3d &
!											  (					     &
!												 segment%pixels(ii)%it,	 &
!												 segment%pixels(ii)%ix,	 &
!												 segment%pixels(ii)%iy	 &
!											  ),					   &
!'  ii,it,ix,iy,index_3d'
!write(*,*) ii,it_pool(ii),ix_pool(ii),iy_pool(ii),'  ii,it_pool(ii),ix_pool(ii),iy_pool(ii)'
!write(*,*) ii,segment%pixels(ii)%it,segment%pixels(ii)%ix,segment%pixels(ii)%iy,index_3d &
!											  (					     &
!												 it_pool(ii),	 &
!												 ix_pool(ii),	 &
!												 iy_pool(ii)	 &
!											  ),					   &
!'  ii,it(ii),ix(ii),iy(ii),index_3d'

      if(icount_time_gr .ne. segment%pixels(ii)%it) icount_time_gr = icount_time_gr + 1
      num_el_time_gr(icount_time_gr) = num_el_time_gr(icount_time_gr) + 1
!write(*,*) ii,icount_time_gr,num_el_time_gr(icount_time_gr),'  ii,icount_time_gr,num_el_time_gr(icount_time_gr)'
		enddo  !  ii
!write(*,*) 
!stop		

goto 333
		it_group_begin = 1
		it = 0
		do  ii = 1,segment%npixels - 1
			if(it_pool(ii+1) .gt. it_pool(ii) .or. ii+1 .eq. segment%npixels) then ! *** looking for new timegroup or for the last element of array
				if(ii+1 .eq. segment%npixels .and. it_pool(ii+1) .eq. it_pool(ii)) then
					it_group_end = ii + 1
				else
					it_group_end = ii
				endif
				if(allocated(num_el_time_gr)) then
					it = it + 1
					num_el_time_gr(it) = it_group_end - it_group_begin +1 
					if (dbg_msg_level .ge. 12) then
						write (*,*) "num_el_time_gr(",it,") =", num_el_time_gr(it) 
						write (*,*) "it_group_end = ",it_group_end
						write (*,*) "it_group_begin = ",it_group_begin
					endif
				endif
				call inssor(iy_pool(it_group_begin:it_group_end)) ! *** sorting part of iy indexes' array
				iy_group_begin = it_group_begin
				do jj = it_group_begin,it_group_end -1
					if(iy_pool(jj+1) .gt. iy_pool(jj) .or. jj+1 .eq. it_group_end) then ! *** looking for new timegroup or for the last element of array
						if(jj+1 .eq. it_group_end ) then
							iy_group_end = jj + 1
						else
							iy_group_end = jj
						endif
						call inssor(iy_pool(iy_group_begin:iy_group_end)) ! *** sorting part of iy indexes' array
						ix_group_begin = iy_group_begin
						do kk = iy_group_begin,iy_group_end -1
							if(iy_pool(kk+1) .gt. iy_pool(kk) .or. kk+1 .eq. iy_group_end) then ! *** looking for new timegroup or for the last element of array
								ix_group_end = kk
								if(kk+1 .eq. iy_group_end ) then
									ix_group_end = kk + 1
								else
									ix_group_end = kk
								endif
								call inssor(ix_pool(ix_group_begin:ix_group_end)) ! *** sorting part of ix indexes' array
								ix_group_begin = ix_group_end + 1
							endif
							iy_group_begin = iy_group_end + 1
						enddo ! end of do  kk = iy_group_begin,iy_group_end -1
						it_group_begin = it_group_end + 1
					endif ! end of if	(	iy_pool(jj+1) .GT.iy_pool(jj) .OR.jj+1 .EQ. it_group_end ) then ! *** looking for new timegroup or for the last element of array
				enddo ! end of do  jj = it_group_begin,it_group_end -1
!				if(it_pool(ii+1) .gt. it_pool(ii) .and.	ii+1 .eq. segment%npixels) then ! *** in this case we have ONLY ONE PIXEL in timgroup i.e. no need to sort anything over ix and iy. We need just to output as it is.
				if(it_pool(ii+1) .gt. it_pool(ii)) then ! *** in this case we have ONLY ONE PIXEL in timgroup i.e. no need to sort anything over ix and iy. We need just to output as it is.
					it_group_begin = it_group_end + 1
					if(ii+1 .eq. segment%npixels) then
						if(allocated(num_el_time_gr)) then
							it = it + 1
							num_el_time_gr(it) = 1 
							if(dbg_msg_level .ge. 12) then
								write (*,*) "num_el_time_gr(",it,") =", num_el_time_gr(it) 
								write (*,*) "Near the bottom"
							endif
						endif
					endif
				endif  !  if	(	it_pool(ii+1) .GT.it_pool(ii) .AND. ii+1 .EQ. segment%npixels) then ! *** looking for new timegroup or for the last element of array
			endif ! end of if	(it_pool(ii+1) .GT.it_pool(ii).OR.ii+1 .EQ. segment%npixels ) then ! *** looking for new timegroup or for the last element of array
		enddo ! end of do  ii = 1,segment%npixels - 1

		if  (segment%npixels .EQ. 1) then
			num_el_time_gr(1) = 1 ! in case there is only one pixel in the set
		endif
333 continue

!do ii=1,segment%npixels
!write(*,*) ii,it_pool(ii),ix_pool(ii),iy_pool(ii),'  ii,it_pool(ii),ix_pool(ii),iy_pool(ii)'
!enddo 

#ifdef WARN_DRY
#warning "__SDATA_VERSION__ duplicated"
#endif           

		write (id_sdata_like_file,'(a)',IOSTAT= io_status) 'SDATA version 2.0'
		write (id_sdata_like_file,'(i3,2i4,a)',IOSTAT= io_status) NX,NY,NT,'  : NX NY NT'
		it = 1
!write(*,*) it,it_pool(it),ix_pool(it),iy_pool(it),index_3d &
!											  (					     &
!												 it_pool(it),	 &
!												 ix_pool(it),	 &
!												 iy_pool(it)	 &
!											  ),					   &
!'  1: ii,it_pool(ii),ix_pool(ii),iy_pool(ii),index_3d'

			time_sec = segment%pixels            &	
                         (                 &
                           index_3d        &
                            (              &
                              it_pool(1),  &
                              ix_pool(1),  &
                              iy_pool(1)   &
                            )              &
                         )%t				


!		write(id_sdata_like_file,* ,IOSTAT= io_status) 	&
		!character(LEN=64) 	::	format_timegroup_hdr =	&
		!				'(/,i3,i8,3x,a,3x,a,f9.2,3i4,a)'
    call convert_time_to_string(time_sec, '%FT%H:%M:%SZ', iso8601_string)
    !write(*,'("time2: ", A)') iso8601_string
		write(id_sdata_like_file,trim(format_timegroup_hdr) ,IOSTAT= io_status) &
            num_el_time_gr(it),                 &
            trim(adjustl(iso8601_string)),		  &
            segment%pixels(1)%HOBS,             & 
            segment%pixels(1)%meas(1)%Nsurf,    &
            segment%pixels(1)%IFGAS,            & 
            trim(adjustl(timegroup_header)),    &
            1
               				
!				segment%pixels(1)%time_string,segment%pixels(1)%data_filename
		it_shift = 0
		do  ii = 1,segment%npixels

			if(num_el_time_gr(it) + it_shift + 1 .EQ. ii ) then

!	------------------------------------------------------------------------------------------------------
! *** unpacking date/time and checking them if they are present
!write(*,*) ii,it_pool(ii),ix_pool(ii),iy_pool(ii),index_3d &
!											  (					     &
!												 it_pool(ii),	 &
!												 ix_pool(ii),	 &
!												 iy_pool(ii)	 &
!											  ),					   &
!'  2: ii,it_pool(ii),ix_pool(ii),iy_pool(ii),index_3d'
            time_sec = segment%pixels	 &	
								 (							       &
									 index_3d 				   &
											  (					     &
												 it_pool(ii),	 &
												 ix_pool(ii),	 &
												 iy_pool(ii)	 &
											  )					     &
								 )%t		
								                   
! *** END unpacking date/time and checking them if they are present
!	------------------------------------------------------------------------------------------------------
				it_shift = it_shift + num_el_time_gr(it)
				it = it + 1
!				write(id_sdata_like_file,*,IOSTAT= io_status)
            
            call convert_time_to_string(time_sec, '%FT%H:%M:%SZ', iso8601_string)
            !write(*,'("time3: ",A)') iso8601_string
				write(id_sdata_like_file,trim(format_timegroup_hdr) ,IOSTAT= io_status) &
				num_el_time_gr(it),                    &
            iso8601_string,                    &
            segment%pixels(ii)%HOBS,           & 
            segment%pixels(ii)%meas(1)%Nsurf,  &
            segment%pixels(ii)%IFGAS,          & 
				trim(adjustl(timegroup_header)),       &
				it_pool(ii)
!						segment%pixels(ii)%time_string,segment%pixels(ii)%data_filename
			endif ! end of if(num_el_time_gr(it) + it_shift + 1 .EQ. ii )
		
			status_funct =                &
				write_one_pixel_sdata       &
						(                       &
						id_sdata_like_file,     &
						segment%pixels          &
							(                     &	
                index_3d            &
                  (                 &
                    it_pool(ii),    &
                    ix_pool(ii),    &
                    iy_pool(ii)     &
                  )                 &
							)                     &
						)
      if ( .not. status_funct ) then
        write(tmp_message,'(a)') &
        "write_one_pixel_sdata function status = .false."
        G_ERROR(trim(tmp_message))
      endif
		enddo ! ii

!		READ (id_sdata_like_file,*,IOSTAT= io_status) NX,NY,NT,HOBS,IPLANE,Nsurf,IFGAS,IP_ANGLE
      close(id_sdata_like_file)			

!	------------------------------------------------------------------------------------------------------
		if(allocated(num_el_time_gr)) then
			deallocate(num_el_time_gr)
		endif
!	------------------------------------------------------------------------------------------------------
		write_sdata_pixels 	= .true. 

	end function write_sdata_pixels

!ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

	function write_one_pixel_sdata (							        &
                                    id_sim_sdata_file,	&
                                    one_pixel				    &
                                 )
!	**************************************************************************************
!	------------------------------------------------------------------------------------------------------
		integer,intent(in)		    ::  id_sim_sdata_file
		type(pixel),intent(inout) ::  one_pixel
!	------------------------------------------------------------------------------------------------------
		logical		                ::  write_one_pixel_sdata
!	------------------------------------------------------------------------------------------------------
		integer					          ::  iw, ip, iv, ibrf, k, ind
    integer                   ::  nvalid_meas, meas_type
!	------------------------------------------------------------------------------------------------------
    real,dimension(KNBVM,KIP,KWM)  ::  thetav
		real,dimension(KNBVM,KIP,KWM)  ::  phi
    real,dimension(KNBVM,KIP,KWM)  ::  fpsw
!	------------------------------------------------------------------------------------------------------
!	------------------------------------------------------------------------------------------------------
                    
      ind = 2 ! pixel => meas
      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      meas_type   = one_pixel%meas(iw)%meas_type(ip)
      nvalid_meas = one_pixel%meas(iw)%nbvm(ip)
      call set_pixel_meas (                           & ! IN
                           iw,                        &
                           nvalid_meas,               &
                           meas_type,                 &
                           ind,                       &
                           fpsw(1:nvalid_meas,ip,iw), & ! INOUT
                           one_pixel                  &
                          )
      if(meas_type .gt. meas_type_lid_beg .and. meas_type .lt. meas_type_lid_end) then
         thetav(1:nvalid_meas,ip,iw) = one_pixel%HVP(1:nvalid_meas)
         phi   (1:nvalid_meas,ip,iw) = 0.0
      else
         thetav(1:nvalid_meas,ip,iw) = one_pixel%meas(iw)%thetav(1:nvalid_meas,ip)
         phi   (1:nvalid_meas,ip,iw) = one_pixel%meas(iw)%phi   (1:nvalid_meas,ip)
      endif ! meas_type
      enddo ! IP
      enddo ! IW

      write(id_sim_sdata_file,'(3i4,2i6)',advance='no') &
      one_pixel%ix,     &
      one_pixel%iy,     &
      one_pixel%cloudy, &
      one_pixel%irow,   &
      one_pixel%icol

      write(id_sim_sdata_file,'(4es15.6)',advance='no') &
			one_pixel%x,		&
			one_pixel%y,		&
			one_pixel%MASL, &
			one_pixel%land_percent

      write(id_sim_sdata_file,'(i4)',advance='no') &
			one_pixel%nwl
      do iw=1,one_pixel%nwl
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
			one_pixel%meas(iw)%wl
      enddo

      do iw=1,one_pixel%nwl
      write(id_sim_sdata_file,'(i4)',advance='no') &
      one_pixel%meas(iw)%nip
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      write(id_sim_sdata_file,'(i4)',advance='no') &
      one_pixel%meas(iw)%meas_type(ip)
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      write(id_sim_sdata_file,'(i4)',advance='no') &
			one_pixel%meas(iw)%nbvm(ip)
      enddo
      enddo

      do iw=1,one_pixel%nwl
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
			one_pixel%meas(IW)%sza
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      do iv=1,one_pixel%meas(iw)%nbvm(ip)
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
      thetav(iv,ip,iw)
      enddo
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      do iv=1,one_pixel%meas(iw)%nbvm(ip)
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
      phi(iv,ip,iw)
      enddo
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      do iv=1,one_pixel%meas(iw)%nbvm(ip)
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
      fpsw(iv,ip,iw)
      enddo
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do ibrf=1,one_pixel%meas(iw)%Nsurf
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
      one_pixel%meas(iw)%groundpar(ibrf)
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do k=1,one_pixel%IFGAS
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
      one_pixel%meas(iw)%gaspar
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      write(id_sim_sdata_file,'(i4)',advance='no') &
      one_pixel%meas(iw)%IFCOV(ip)
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      do iv=1,one_pixel%meas(iw)%nbvm(ip)*one_pixel%meas(iw)%IFCOV(ip)
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
      one_pixel%meas(iw)%CMTRX(iv,ip)
      enddo
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      write(id_sim_sdata_file,'(i4)',advance='no') &
      one_pixel%meas(iw)%IFMP(ip)
      enddo
      enddo

      do iw=1,one_pixel%nwl
      do ip=1,one_pixel%meas(iw)%nip
      do iv=1,one_pixel%meas(iw)%nbvm(ip)*one_pixel%meas(iw)%IFMP(ip)
      write(id_sim_sdata_file,'(es15.6)',advance='no') &
      one_pixel%meas(iw)%MPROF(iv,ip)
      enddo
      enddo
      enddo
      write(id_sim_sdata_file,'(/)')
!	------------------------------------------------------------------------------------------------------
      write_one_pixel_sdata = .true. 
!	------------------------------------------------------------------------------------------------------
      end function write_one_pixel_sdata

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine get_vert_prof_h (  iw,ip,    	 &
                                    pixel_cont,  & ! IN
                                    NBVM,HVP		 & ! OUT
                                 )

      implicit none
!	------------------------------------------------------------------------------------------------------
      integer,           intent(in)  :: iw,ip
      type(pixel),       intent(in)  :: pixel_cont
!  ------------------------------------------------------------------------------------------------------
      integer,           intent(out) :: NBVM
      real,dimension(:), intent(out) :: HVP   
!	------------------------------------------------------------------------------------------------------
      integer  ::  iv
!	------------------------------------------------------------------------------------------------------
      HVP(:) = 0.0
	  
      NBVM = pixel_cont%meas(iw)%NBVM(ip)	  
      if(size(HVP) .LT. NBVM) then
        write(tmp_message,'(2(a,i0,x))') 'Size(HVP)=',Size(HVP),'.LT. NBVM = ',NBVM
        G_ERROR(trim(tmp_message))
      endif
	  
      do iv=1,NBVM
      HVP(iv) = pixel_cont%HVP(iv)
      enddo ! iv

	end subroutine get_vert_prof_h

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine get_HVP_lidar ( segment,NHVP,HVP )

      implicit none
!	------------------------------------------------------------------------------------------------------
      type(segment_data),  intent(in)     ::  segment
      integer,             intent(inout)  ::  NHVP
      real,dimension(:,:), intent(inout)  ::  HVP
!	------------------------------------------------------------------------------------------------------
      integer :: meas_type,ipix,iw,ip
!	------------------------------------------------------------------------------------------------------
      NHVP = 1
      HVP(:,:) = 0.0

      do ipix=1,segment%npixels
      do iw=1,segment%pixels(ipix)%nwl
      do ip=1,segment%pixels(ipix)%meas(iw)%NIP
         meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
         if(meas_type .ge. meas_type_lid_beg) then
         if(meas_type .le. meas_type_lid_end) then
            NHVP = segment%pixels(ipix)%meas(iw)%NBVM(ip) 
            HVP(1:NHVP,ipix) = segment%pixels(ipix)%HVP(1:NHVP)
            !write(*,*) 'In get_HVP: IW,IP, NBVM(IP)',IW,IP,NHVP
            !write(*,*) 'In get_HVP: IPIX,IW,IP, HVP',ipix,IW,IP,segment%pixels(ipix)%HVP(1:NHVP)
         endif
         endif
      enddo ! ip
      enddo ! iw
      enddo ! ipix 

      return
      end subroutine get_HVP_lidar

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

      subroutine get_MASL ( segment, MASL )

      implicit none
!	------------------------------------------------------------------------------------------------------
      type(segment_data),  intent(in)    ::  segment
      real,dimension(:),    intent(inout)  ::  MASL
!	------------------------------------------------------------------------------------------------------
      integer :: ipix
!	------------------------------------------------------------------------------------------------------
      MASL(:) = 0.0

      do ipix=1,segment%npixels
         MASL(ipix) = segment%pixels(ipix)%MASL
      enddo ! ipix

      return
      end subroutine get_MASL

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

! Assign noise number to every measurement types in each pixel

      subroutine assign_noise_index(RIN,segment_meas,MNOISEI)
      
      use mod_par_inv,    only : KW,KIMAGE
      use mod_retr_settings_derived_type
      
      implicit none
!	-------------------------------------------------------------------------
      type(retr_input_settings),        intent(in)     ::  RIN
      type(segment_data),               intent(in)     ::  segment_meas
      integer,dimension(KIP,KWM,KIMAGE),intent(inout)  ::  MNOISEI
!	-------------------------------------------------------------------------            
      integer                :: nwl, nip
      real,   dimension(KW)  :: wl
      integer,dimension(KW)  :: ind_wl
      integer                :: ipix,ip,iw,ip1,iw1,iin
!	-------------------------------------------------------------------------
!	-------------------------------------------------------------------------
      do ipix=1,segment_meas%npixels
      call get_pixel_wl ( segment_meas%pixels(ipix),  &
                          nwl,wl,ind_wl               &
                        )
if(ipix .eq. -1) then
write(*,'(a)') 'ind_wl: '
write(*,'(10i5)') ind_wl(1:nwl)
stop 'stop in assign_noise_index'
endif
      do iw=1,nwl
      nip = segment_meas%pixels(ipix)%meas(iw)%NIP
      do ip=1,nip
         do iin=1,RIN%NOISE%INOISE
         do ip1=1,RIN%NOISE%NMT(iin)
         do iw1=1,RIN%NOISE%NWLP(ip1,iin)               
if(iin .eq. -2) then
write(*,'(2a,6i5,a)') 'ip1, iw1, ind_wl(iw), IWLP(iw1,ip1,iin),', &
' meas_type(ip), MT(ip1,iin) - ',ip1,iw1,ind_wl(iw),RIN%NOISE%IWLP(iw1,ip1,iin), &
segment_meas%pixels(ipix)%meas(iw)%meas_type(ip),RIN%NOISE%MT(ip1,iin), &
'  -  in assign_noise_index'
endif
            if ( ind_wl(iw) .eq. RIN%NOISE%IWLP(iw1,ip1,iin)  .and.  &
                 segment_meas%pixels(ipix)%meas(iw)%meas_type(ip) .eq. RIN%NOISE%MT(ip1,iin) &
               ) then
               MNOISEI(ip,iw,ipix)=iin

!               write(*,*) 'ipix=',ipix,'  ind_wl(iw)=',ind_wl(iw),'  ip=',ip, &
!               '  iin=',iin,'  ip1=',ip1,'  iw1=',iw1,  &
!               '  MNOISE=',MNOISEI(ip,iw,ipix)
            endif
         enddo ! iw1
         enddo ! ip1
         enddo ! iin
      enddo ! ip
      enddo ! iw
      if(RIN%IPRI_verbose) then
         if(ipix .eq. 1) write(*,'(a)') 'in assign_noise_index:'
         !write(*,'(4x,a,i0,3x,a,100i5)') 'pixel # ',ipix,'MNOISE:  ', &
         !( (MNOISEI(ip,iw,ipix),ip=1,segment_meas%pixels(ipix)%meas(iw)%NIP), &
         !iw=1,segment_meas%pixels(ipix)%nwl )
        write(*,'(4x,a,i0,3x,a)') 'pixel # ',ipix,'MNOISE:'
        do iw=1,nwl
        nip = segment_meas%pixels(ipix)%meas(iw)%NIP
        do ip=1,nip
        write(*,'(3x,i0)',advance='no') MNOISEI(ip,iw,ipix)
        enddo
        if (iw .gt. 1 .and. mod(iw,10) .eq. 0 ) write(*,*)
        enddo
        write(*,*)
      endif !  RIN%IPRI_verbose
      enddo ! ipix

      return
      end subroutine assign_noise_index

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss 
! Fill out an array of wave lengths for retrieved parameters (important if pixeles have different neither numbers 
!                                                        or values of wave lengths)

      subroutine set_RIN_wave_length_array (  iu_main_output,  & ! IN
                                              segment_meas,    &
                                              RIN              & ! INOUT
                                           )
      
      use mod_retr_settings_derived_type
      use m_inssor
      use mod_par_inv, only: KW

      implicit none
!	-------------------------------------------------------------------------
      integer,                      intent(in)     :: iu_main_output
      type(segment_data),           intent(in)     :: segment_meas
      type(retr_input_settings),    intent(inout)  :: RIN
!	-------------------------------------------------------------------------
      integer :: ipix, i, ip, iw
      integer :: IDIM1, IDIM2
      integer :: nwl_temp, nwl
      real, dimension(1) :: wl
      integer, dimension(1) :: iwl_temp
      logical :: add_wl
      character(len=20) :: str
      character(len=150) :: CFMT
      logical :: status
      logical, parameter :: abs_diff = .true.
      real, parameter :: tiny = 1e-4
!	-------------------------------------------------------------------------
 ! Calculate RIN%NW - number of wavelengths from wavelength dependent characteristics ( refractive index or surface )
      RIN%NW = 0
goto 111
      do IDIM1=1,RIN%NDIM%n1
        if(RIN%NDIM%par_type(IDIM1) .eq. par_type_RERI_spect) then  
          RIN%NW = 0
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            RIN%NW = max( RIN%NW,RIN%NDIM%n3(IDIM2,IDIM1) )
          enddo
        endif
        if(RIN%NDIM%par_type(IDIM1) .gt. par_type_SURF1_land_beg .and. & 
           RIN%NDIM%par_type(IDIM1) .lt. par_type_SURF_water_end) then
          RIN%NW = 0
          do IDIM2=1,RIN%NDIM%n2(IDIM1)
            RIN%NW = max( RIN%NW,RIN%NDIM%n3(IDIM2,IDIM1) )
          enddo       
        endif ! RIN%NDIM%par_type(IDIM1) .eq. par_type_RERI_spect
      enddo ! IDIM1
111 continue
      if ( RIN%NW .eq. 0 ) then
        do i=1,RIN%NOISE%INOISE
        do ip=1,RIN%NOISE%NMT(i)
        do iw=1,RIN%NOISE%NWLP(ip,i)
          RIN%NW = max( RIN%NW,RIN%NOISE%IWLP(iw,ip,i) )
        enddo
        enddo
        enddo
      endif

! A set of wavelengths from measurement data
loop_pix :  do ipix=1,segment_meas%npixels
      if ( ipix .eq. 1 ) then
        nwl = 0
        RIN%WAVE(:) = 0.0
        nwl = segment_meas%pixels(ipix)%nwl
        if(nwl .gt. KW) then
        write(tmp_message,'(2(a,i0),a)') &
        'Number of wavelengths in segment data nwl = ',nwl, &
        '  is bigger than constant KW = ',KW,'  in module mod_par_inv'
        G_ERROR(trim(tmp_message))
        endif
        RIN%WAVE(1:nwl) = segment_meas%pixels(ipix)%meas(1:nwl)%wl
        nwl_temp = nwl
        cycle loop_pix
      endif
      do iw=1,segment_meas%pixels(ipix)%nwl
        add_wl = .true.
        wl(1) = segment_meas%pixels(ipix)%meas(iw)%wl
        ! search if given wavelength wl present in wavelength set array
        !print*,'in set_RIN_wave_length_array: ipix, iw, nwl_temp, wave: ',ipix,iw,nwl_temp,RIN%WAVE(1:nwl_temp), &
        !'  - input for R_subset_index()'
        call R_subset_index ( tiny, abs_diff,                 & ! IN
                              nwl_temp, RIN%WAVE(1:nwl_temp), &
                              1, wl(1),                       &
                              iwl_temp(1), status             & ! OUT
                            )
        !print*,'ipix, iw, nwl_temp, wave: ',ipix,iw,nwl_temp,RIN%WAVE(1:nwl_temp), &
        !'  - input for R_subset_index()'
        !print*,'wl(1) =',wl(1),'iwl_temp =',iwl_temp(1),'status =',status, &
        !'  - output from R_subset_index() for given wavelength'

        if ( status ) then
          add_wl = .false.
        endif

        if ( add_wl ) then
          nwl = nwl+1
          if(nwl .gt. KW) then
            write(tmp_message,'(2(a,i0),a)') &
            'Number of wavelengths in segment data nwl = ',nwl, &
            '  is bigger than constant KW = ',KW,'  in module mod_par_inv'
            G_ERROR(trim(tmp_message))
          endif
          if(nwl .gt. RIN%NW) then
            write(tmp_message,'(2(a,i0),a)') &
            'Number of wavelengths in segment data nwl = ',nwl, &
            '  is bigger than number of wavelengths NW = ',RIN%NW,'  in settings'
            G_ERROR(trim(tmp_message))
          endif ! nwl .gt. RIN%NW
          RIN%WAVE(nwl) = segment_meas%pixels(ipix)%meas(iw)%wl
          nwl_temp = nwl
          call inssor(RIN%WAVE(1:nwl))
        endif ! add_wl

      enddo ! iw
      
      enddo loop_pix

      if(nwl .ne. RIN%NW) then
        write(tmp_message,'(2(a,i0,x),a)') &
        'Number of wavelengths in segment data nwl = ',nwl, &
        'must be equal to number of wavelengths NW = ',RIN%NW,'in settings'
        G_ERROR(trim(tmp_message))
      endif ! nwl .ne. RIN%NW

! Sort wavelengths into increasing order (Insertion sort)
      
      !write(*,*) 'wave lengths:'
      !write(*,*) RIN%WAVE(1:nwl)
      !write(*,*)

      !write(*,*) 'sub set_RIN_wave_length_array: wave lengths from SDATA   : ',RIN%WAVE(1:RIN%NW)
      if(RIN%IPRI_additional_info) then
        write(str,*) RIN%NW
        CFMT = '('//trim(adjustl(str))//'(es12.4))'
        write(iu_main_output,'(a,i0,2x,a)') 'NW = ',RIN%NW,'(RIN%WAVE(IW),IW=1,NW) in set_RIN_wave_length_array'
        write(iu_main_output,trim(CFMT)) (RIN%WAVE(IW),IW=1,RIN%NW)
      endif

      return
      end subroutine set_RIN_wave_length_array

! ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss 

    subroutine set_index_clouds(RIN, segment, index_clouds)

        use mod_retr_settings_derived_type

        implicit none
!	-------------------------------------------------------------------------        
		  type(retr_input_settings),  intent(in)  :: RIN
        type(segment_data),       intent(in)  :: segment
        type(ind_clouds),         intent(out) :: index_clouds
!	-------------------------------------------------------------------------        
        integer :: i,ipix,IX,IY,IT
!	-------------------------------------------------------------------------
        
        ! Initialize
         index_clouds%NX=segment%NX
         index_clouds%NY=segment%NY
         index_clouds%NT=segment%NT
         index_clouds%ITIMAGE(:)   = 0
         index_clouds%IXIMAGE(:)   = 0
         index_clouds%IYIMAGE(:)   = 0
         index_clouds%INIMAGE(:,:,:) = 0
         index_clouds%T(:,:,:)       = 0
         index_clouds%X(:,:,:)       = 0.0
         index_clouds%Y(:,:,:)       = 0.0
         index_clouds%ICLOUD(:,:,:)  = 0

        
        ! Set values
        DO ipix=1, segment%npixels
            IX=segment%pixels(ipix)%IX
            IY=segment%pixels(ipix)%IY
            IT=segment%pixels(ipix)%IT
            index_clouds%INIMAGE(IX,IY,IT)=ipix
            !index_clouds%ICLOUD(IX,IY,IT)=segment%pixels(ipix)%cloudy
            index_clouds%ICLOUD(IX,IY,IT)=1
            index_clouds%ITIMAGE(ipix)=IT
            index_clouds%IYIMAGE(ipix)=IY
            index_clouds%IXIMAGE(ipix)=IX
            index_clouds%X(IX,IY,IT)=segment%pixels(ipix)%X
            index_clouds%Y(IX,IY,IT)=segment%pixels(ipix)%Y
            index_clouds%T(IX,IY,IT)=segment%pixels(ipix)%T
        ENDDO        

      if(RIN%IPRI_verbose) then
         write(*,'(a)') 'in set_index_clouds:'
         write(*,'(4x,3(a,i4))') 'NT=',index_clouds%NT,'  NX=',index_clouds%NX,'  NY=',index_clouds%NY
         i=0
         do IT=1,index_clouds%NT
         do IY=1,index_clouds%NY
         do IX=1,index_clouds%NX
          if(index_clouds%INIMAGE(IX,IY,IT) .gt. 0) then 	  
            i=i+1
            !write(*,'(2(a,i5),a,i16,2(a,f10.3),4(a,i4))') 'i=',i,'  IIMAGE=',  &
            write(*,'(4x,a,i0,3x,a,i0,2(3x,a,f9.3),3x,a,f13.1,7(3x,a,i0))') 'i = ',i,'pixel # ',  &
            index_clouds%INIMAGE(IX,IY,IT),        &
            'X = ',index_clouds%X(IX,IY,IT),       &
            'Y = ',index_clouds%Y(IX,IY,IT),       &
            'T = ',dble(index_clouds%T(IX,IY,IT)), &
            'ix = ',index_clouds%IXIMAGE(index_clouds%INIMAGE(IX,IY,IT)),    &
            'iy = ',index_clouds%IYIMAGE(index_clouds%INIMAGE(IX,IY,IT)),    &
            'it = ',index_clouds%ITIMAGE(index_clouds%INIMAGE(IX,IY,IT)),    &
            'icloud = ',index_clouds%ICLOUD(IX,IY,IT),                       &
            'out_x = ',segment%pixels(index_clouds%INIMAGE(IX,IY,IT))%out_x, &
            'out_y = ',segment%pixels(index_clouds%INIMAGE(IX,IY,IT))%out_y, &
            'out_t = ',segment%pixels(index_clouds%INIMAGE(IX,IY,IT))%out_t
          endif
         enddo
         enddo
         enddo
      endif ! IPRI_mode .eq. 1

    return
    end subroutine set_index_clouds

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss


!!!! MEH:
! sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Add bias to segment measurement vector
! INOISE  - the number of different bias sources --> MEH: I need a new value for the bias? I am using the same than random..
! BIAS(I) - bias in i -th source
! INN(I)  - EQ.1.THEN error is absolute with
!         - EQ.0 THEN error assumed relative


      !subroutine add_bias_segment ( RIN,bias_value,  & ! IN   !deep_bias_switch,
      subroutine add_bias_segment ( RIN,  & ! IN
                                      segment_meas,            &
                                      segment_vec_meas,        & ! INOUT
                                      MNOISEI,                  & ! IN
                                      option_bias)
      
      use mod_retr_settings_derived_type
      
      implicit none
!    ----------------------------------------------------------------------------
! IN :
      type(retr_input_settings),        intent(in) ::  RIN
      !logical,                          intent(in) ::  deep_bias_switch
      type(segment_data),               intent(in) ::  segment_meas
      integer,dimension(KIP,KWM,KIMAGE),intent(in) ::  MNOISEI
      !real,                     intent(inout):: bias_value

    !MEH
      integer, optional, intent(in) :: option_bias
!    ----------------------------------------------------------------------------
! INOUT :
      type(pixel_vector),dimension(KIMAGE), intent(inout) ::  segment_vec_meas
!    ----------------------------------------------------------------------------
! LOCAL :
      integer                              ::  ipix, JJS, i
      integer                              ::  npixels
!    ------------------------------------------------------------------------------------------
      npixels = segment_meas%npixels

! Add bias in the measurements
      do ipix=1,npixels
        JJS = SUM(segment_vec_meas(ipix)%nFS(1:segment_meas%pixels(ipix)%nwl))
        if(RIN%KL .eq. 1) &
        segment_vec_meas(ipix)%FS(1:JJS) = EXP(segment_vec_meas(ipix)%FS(1:JJS))
        !call add_bias_pixel (RIN,bias_value, MNOISEI(:,:,ipix),      & !deep_bias_switch,          & ! IN
        call add_bias_pixel (RIN, MNOISEI(:,:,ipix),      & !deep_bias_switch,
                                segment_meas%pixels(ipix),   &
                                segment_vec_meas(ipix),       & ! INOUT
                                option_bias)
        if(RIN%KL .eq. 1) then
        if(RIN%IPRI_verbose .eqv. .true.) then
          do i=1,JJS
            if(segment_vec_meas(ipix)%FS(i) .le. 0.0) then
              write(tmp_message,'(2(a,i0,2x),a,es12.4,2(x,a))') &
              'ipix = ',ipix,'i_meas = ',i, &
              'FS_meas =',segment_vec_meas(ipix)%FS(i),'<=0 after add_bias_pixel;', &
              'minimization_convention: logarithm'
              G_ERROR(trim(tmp_message))
            endif
          enddo
        endif
        segment_vec_meas(ipix)%FS(1:JJS) = LOG(segment_vec_meas(ipix)%FS(1:JJS))
        endif
      enddo ! ipix

      return
      end subroutine add_bias_segment

!    sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!MEH: Add bias to the measurements (simulated measurements and also option to add bias in the  systematic component of errors)
      subroutine add_bias_pixel (RIN,  MNOISEI,  &  !deep_bias_switch, & ! IN
                                    pixel_cont,         &
                                    pixel_vec,           & ! INOUT
                                    option_bias)
      use mod_retr_settings_derived_type

      implicit none
! -----------------------------------------------------------------
! IN :
      type(retr_input_settings), intent(in) :: RIN
      type(pixel),               intent(in) :: pixel_cont
      integer,dimension(KIP,KWM),intent(in) :: MNOISEI
    ! MEH
    integer, optional, intent(in) :: option_bias
! -----------------------------------------------------------------
! INOUT :
      type(pixel_vector), intent(inout) :: pixel_vec
! -----------------------------------------------------------------
! LOCAL :
      integer      :: iw,IP,iv,iMN,NWL,NIP,meas_type,JJS
! ----------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------
! Adding bias
! INOISE  - the number of different noise sources
! BIAS(I) - bias in i -th source

      JJS = 0
      NWL = pixel_cont%nwl
LOOP_WL : do iw=1,NWL
      NIP = pixel_cont%meas(iw)%NIP
LOOP_meas_type : do IP=1,NIP
         meas_type = pixel_cont%meas(iw)%meas_type(IP)
         iMN = MNOISEI(IP,iw)
         if(iMN .lt. 1 .or. iMN .gt. RIN%NOISE%INOISE) then
            write(tmp_message,'(2(a,i0),2a)') 'iMN = ',iMN,'  INOISE = ',RIN%NOISE%INOISE, &
            NEW_LINE('A'), &
            'Noise index is not in valid range 1<iMN<INOISE'
            G_ERROR(trim(tmp_message))
         endif !
         if(RIN%iPOBS .ge. 3 .and. meas_type .eq. meas_type_U) & ! fit sqrt(Q*Q+U*U), sqrt(Q*Q+U*U)P/I
         exit LOOP_meas_type

!         if(RIN%NOISE%BIAS(iMN) .ne. 0.0) then
         if(present(option_bias)) then
            if(option_bias .EQ. 1) then
            !positive bias
               do IV=1,pixel_cont%meas(iw)%NBVM(IP)
               JJS = JJS+1
                 if( (meas_type .ge. meas_type_p33 .and. meas_type .le. meas_type_p44) .or. &
                      meas_type .eq. meas_type_p12 .or. meas_type .eq. meas_type_p12_rel) then
                   pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
                   call bias_val (RIN%NOISE%BIAS_EQ(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS), option_bias)
                   pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
                 elseif( meas_type .eq. meas_type_Q .and. RIN%iPOBS .lt. 3) then
                   pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
                   call bias_val (RIN%NOISE%BIAS_EQ(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS), option_bias)
                   pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
                 elseif( meas_type .eq. meas_type_U .and. RIN%iPOBS .lt. 3) then
                   pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
                   call bias_val (RIN%NOISE%BIAS_EQ(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS), option_bias)
                   pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
                 else
                   !WRITE(*,*)'BIAS information',RIN%NOISE%INN(iMN),iMN,JJS
                   call bias_val (RIN%NOISE%BIAS_EQ(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS), option_bias)
                 endif
               enddo ! IV
            else if(option_bias .eq. 2) then
            !negative bias
            do IV=1,pixel_cont%meas(iw)%NBVM(IP)
            JJS = JJS+1
              if( (meas_type .ge. meas_type_p33 .and. meas_type .le. meas_type_p44) .or. &
                   meas_type .eq. meas_type_p12 .or. meas_type .eq. meas_type_p12_rel) then
                pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
                call bias_val (RIN%NOISE%BIAS_EQ(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS), option_bias)
                pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
              elseif( meas_type .eq. meas_type_Q .and. RIN%iPOBS .lt. 3) then
                pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
                call bias_val (RIN%NOISE%BIAS_EQ(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS), option_bias)
                pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
              elseif( meas_type .eq. meas_type_U .and. RIN%iPOBS .lt. 3) then
                pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
                call bias_val (RIN%NOISE%BIAS_EQ(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS), option_bias)
                pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
              else
                !WRITE(*,*)'BIAS information',RIN%NOISE%INN(iMN),iMN,JJS
                call bias_val (RIN%NOISE%BIAS_EQ(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS), option_bias)
              endif
            enddo ! IV
            endif
        else
        !bias in the measurement: can be negative or positive
        do IV=1,pixel_cont%meas(iw)%NBVM(IP)
        JJS = JJS+1
          if( (meas_type .ge. meas_type_p33 .and. meas_type .le. meas_type_p44) .or. &
               meas_type .eq. meas_type_p12 .or. meas_type .eq. meas_type_p12_rel) then
            pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
            call bias_val (RIN%NOISE%BIAS(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS))
            pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
          elseif( meas_type .eq. meas_type_Q .and. RIN%iPOBS .lt. 3) then
            pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
            call bias_val (RIN%NOISE%BIAS(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS))
            pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
          elseif( meas_type .eq. meas_type_U .and. RIN%iPOBS .lt. 3) then
            pixel_vec%FS(JJS) = pixel_vec%FS(JJS) - RIN%SHIFT
            call bias_val (RIN%NOISE%BIAS(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS))
            pixel_vec%FS(JJS) = pixel_vec%FS(JJS) + RIN%SHIFT
          else
            !WRITE(*,*)'BIAS information',RIN%NOISE%INN(iMN),iMN,JJS
            call bias_val (RIN%NOISE%BIAS(iMN),RIN%NOISE%INN(iMN),pixel_vec%FS(JJS))
          endif
        enddo ! IV
        endif ! option_bias
!         else
!           do IV=1,pixel_cont%meas(iw)%NBVM(IP)
!           JJS = JJS+1
!           enddo ! IV
!        endif ! RIN%NOISE%BIAS(iMN) .ne. 0.0
enddo LOOP_meas_type
enddo LOOP_WL

      return
      end subroutine add_bias_pixel

!    sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! MEH:
    subroutine bias_val (BIAS,INN,           & ! IN
                          FS,                & ! INOUT
                          option_bias)
      implicit none
! -----------------------------------------------------------------
! IN :
      real,    intent(in) :: BIAS
      integer, intent(in) :: INN

    ! MEH
    integer, optional, intent(in) :: option_bias
! -----------------------------------------------------------------
! INOUT :
      real,    intent(inout) :: FS
! -----------------------------------------------------------------
! LOCAL :
      real              :: FS0
! ----------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------
! Adding Bias
! BIAS - Bias in i -th source
! INN  - EQ.1.THEN error is absolute

        if (present(option_bias)) then
            if (option_bias .eq. 1) then
             FS0 = FS
             select case(INN)
             case(0)
                FS = FS * (1.0+BIAS)
             case(1)
                FS = FS + BIAS
             end select
             !write(*,'(2(a,i3),4(a,e12.5))') 'in bias: INN=',INN, &
             !               '  BIAS=',BIAS,'  FS0=',FS0,'  FS=',FS
            else if (option_bias .eq. 2) then
             FS0 = FS
             select case(INN)
             case(0)
                FS = FS * (1.0-BIAS)
             case(1)
                FS = FS - BIAS
             end select
            endif
        else
        FS0 = FS
        select case(INN)
        case(0)
           FS = FS * (1.0+BIAS)
        case(1)
           FS = FS + BIAS
        end select
      endif
      return
      end subroutine bias_val


end module mod_sdata

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
! Assign general wave length indices to pixel wave lengths.
! For real Parasol data radiance measurements at wavelength #iw can be corrected.
      subroutine set_segment_meas ( iu_main_output,    & ! IN
                                    RIN,               & 
                                    segment_meas       & ! INOUT
                                  )
      use mod_sdata
      use mod_retr_settings_derived_type

      implicit none
!	---------------------------------------------------------------------------------------
      integer,                   intent(in)     :: iu_main_output
      type(retr_input_settings), intent(in)     :: RIN
      type(segment_data),        intent(inout)	:: segment_meas
! -----------------------------------------------------------------------------------------
! Assign general wave length indices to pixel wave lengths.
      call set_segment_pixel_wl_index (  iu_main_output, & ! IN
                                         RIN,               & 
                                         segment_meas       & ! INOUT
                                      )
!! For real Parasol data radiance measurements at wavelength #iw can be corrected.
!      if(RIN%I_corr)  &
!      call radiance_correction (  iu_main_output,  & ! IN
!                                  RIN,             &
!                                  segment_meas     & ! INOUT
!                               )

      return
      end subroutine set_segment_meas

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

              
!> This function has to be called just before the inversion subroutine in order to
!> prepare segment and settings. Settings will be filled with some segment information 
!> like for example, wavelengths
subroutine prepare_segment_settings(iu_main_output, & ! IN
                                    segment_meas,   & ! INOUT
                                    RIN             ) ! INOUT
      use mod_sdata
      use mod_retr_settings_derived_type
      
      implicit none
! -----------------------------------------------------------------------------------------
      integer,                    intent(in)    ::  iu_main_output
      type(segment_data),         intent(inout) ::  segment_meas
      type(retr_input_settings),  intent(inout) ::  RIN
              
! Set RIN fields (radii, surface model flags for RT_OSH, number of retrieved parameters ).
! Set RIN with wave length array for retrieved parameters (combined array of wavelengths in case  
!     different pixels contain niether diff numbers or diff values of wavelengths).
! Set RIN with NDVI wavelength indices.
      call set_input_settings ( iu_main_output, & ! IN
                                segment_meas,   &
                                RIN             & ! INOUT
                              )
      if ( error_present() ) return
! Set segment_meas. Assign general wave length indices to pixel wave lengths.
! Set segment_meas. For real Parasol data, radiance measurements at wavelength 
!                   RIN%I_corr_iwl can be corrected.
      call set_segment_meas ( iu_main_output, & ! IN
                              RIN,            & 
                              segment_meas    & ! INOUT
                            )              
              
end subroutine prepare_segment_settings

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
!> Setup flag for SOS RT routine to control if downward and upward measurement
!> pixels are present in segment

subroutine SETUP_RT_SOS_CNTRL_ICMB(iu_main_output,RIN,segment)

      use mod_globals, only : sp, pi_sp
      use mod_sdata_meas_type
      use mod_sdata
      use mod_retr_settings_derived_type
      use MOD_RT_SOS_SETUP, ONLY : RT_SOS_CTL
      use mod_par_inv, only : KIP
      use mod_par_OS,  only : NBVM
      use mod_stop_report

      implicit none
! ............................................................................
      integer, intent(in) :: iu_main_output
      type(retr_input_settings), intent(in) :: RIN
      type(segment_data),        intent(in) :: segment
! ............................................................................
      integer :: npixels, nmeas_type, meas_type, nbv
      integer :: ipix, iw, ip
      logical :: IDN, IUP
      integer :: ipix_dn, ipix_up
      real (sp) :: sza
      real (sp), dimension(NBVM,KIP) :: vis, fiv
      real (sp) :: rad_sp
! ............................................................................
      RT_SOS_CTL%ICMB = .false.

      npixels = segment%npixels

      rad_sp = pi_sp/180._sp
      IDN = .false. ! Satellite/Airborne downward observations
      IUP = .false. ! Ground based/Airborne upward observations
      loop_pix: do ipix=1,npixels
      do iw=1,segment%pixels(ipix)%nwl
      nmeas_type = segment%pixels(ipix)%meas(iw)%nip
      do ip=1,nmeas_type
        meas_type = segment%pixels(ipix)%meas(iw)%meas_type(ip)
        if ( meas_type .gt. meas_type_SvR_beg ) then
        if ( meas_type .lt. meas_type_SvR_end) then
          call get_pixel_geom ( iw, ip, segment%pixels(ipix), nbv, sza, vis(:,ip), fiv(:,ip) )
          if (any(cos(vis(1:nbv,ip)*rad_sp) .lt. 0.)) then
          !Ground based observations
            IUP = .true.
            ipix_up = ipix
          elseif (any(cos(vis(1:nbv,ip)*rad_sp) .gt. 0.)) then
          !Airborne observations
            IDN = .true.
            ipix_dn = ipix
          end if
          if ( IDN .and. IUP ) then
            RT_SOS_CTL%ICMB = .true.
            if ( ipix_dn .eq. ipix_up ) then
            write(tmp_message,'(a,i0,2x,2a)') 'Pixel # ',ipix, &
            'containts measurements from both upward and downward instruments.', &
            NEW_LINE('A'), &
            'The measurements have to be in different pixels.'
            G_ERROR(trim(tmp_message))
            endif
            return
          endif
        endif
        endif ! meas_type .gt. meas_type_lid_beg .and.
      enddo ! ip
      enddo ! iw
      enddo loop_pix

      if ( RIN%IPRI_verbose ) then
      if ( RT_SOS_CTL%ICMB ) then
        write(iu_main_output,'(4x,a)') 'Measurements of both upward and downward looking instruments are inverted.'
      else
        if ( IUP ) then
        write(iu_main_output,'(4x,a)') 'Measurements of upward looking instruments are inverted.'
        elseif ( IDN ) then
        write(iu_main_output,'(4x,a)') 'Measurements of downward looking instruments are inverted.'
        endif
      endif
      endif

return
end subroutine SETUP_RT_SOS_CNTRL_ICMB

!	sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss



