! **
! **  Copyright 2016 CNRS & Universite Lille 1. All rights reserved.  
! **  Licensed under the GRASP Open Source License V1.0 (see LICENSE file)
! **

module mo_grasp_controller
    use iso_fortran_env ! input_unit, output_unit, error_unit
    use iso_c_binding
    use mod_retr_settings_derived_type
    !use mod_retr_general_output
    use mod_sdata
    use mod_alloc_arrays
    use mod_par_inv
    use mod_grasp_inversion
    use inversion_subsystem
    use mod_edges
    use mod_stop_report
    use mod_alloc_gas_lut
    use mod_mixture_ref_index_chem, only: LUT_CHEM, read_chem_lut, alloc_chem_lut
    !private
    implicit none   

    type(kernels_triangle_bin)      ::  KERNELS1
    type(kernels_lognormal_bin)     ::  KERNELS2
    real,dimension(:,:,:),allocatable ::  US

    contains
        

    subroutine grasp_prepare_segment_settings(RIN, segment_meas) bind(C)
      type(retr_input_settings), intent(inout) :: RIN
      type(segment_data), intent(inout) :: segment_meas
      character (len=GBL_FILE_PATH_LEN)   ::  distname_O
 
      call cstring2fstring(RIN%DLSF%distname_O, distname_O)
      
      RIN%main_output_file = " "
      RIN%main_output_file(1) = "-"
      RIN%main_output_file(2) = C_NULL_CHAR
      RIN%plotting_output_file(1) = C_NULL_CHAR
      ! TEMPORAL SOLUTION: Retrieval code need two paths to kernels but control 
      ! unit reads just one. Temporally here it is duplicated the path read in settings
      ! Also we add a '/' symbol at the end in case it is not so now it is optional to put it in settings file
      if (distname_O(LEN_TRIM(distname_O):LEN_TRIM(distname_O)) .ne. '/') then
        distname_O=trim(distname_O)//'/'
      endif
      call fstring2cstring(distname_O, int(GBL_FILE_PATH_LEN, kind=c_size_t), RIN%DLSF%distname_O)
      RIN%DLSF%distname_N=RIN%DLSF%distname_O
      call prepare_segment_settings(6, segment_meas, RIN )
    end subroutine grasp_prepare_segment_settings
    

    function grasp_init_inversion(RIN) bind(C)
        type(retr_input_settings),         intent(inout)  :: RIN
        integer                                           :: grasp_init_inversion, j, IW , IS, II, NWL_LUT,NP_LUT,NT_LUT, par_type, IDIM1, ISD, nfract
        character (len=GBL_FILE_PATH_LEN)                 :: internal_file_path,lut_path, lut_name, filters_file

        ! Validate constants
        if (KNBVM .ne. max(KVERTM,NBVM)) then
            write(*,*) 'Constant bad defined: KNBVM must be the maximum between KVERT and NBVM'
            grasp_init_inversion=-1
            return
        endif
        if (KDIF .ne. max( KPARS, KITIME, KIX, KIY)) then
            write(*,*) 'Constant bad defined: KDIF must be the maximum between KVERT, KPARS, KITIME, KIX and KIY'
            grasp_init_inversion=-2
            return
        endif
                       
        ! Init inversion
        call alloc_arrays(RIN,KERNELS1,KERNELS2,US)


        if(RIN%gases%integration_method .eq. -1)then
            RIN%gases%igab = .FALSE.
        else
            RIN%gases%igab = .TRUE.
        end if

        if(RIN%gases%igab .or. RIN%emission%planck) then
            call alloc_atm_prof()
            read_gas_abs = .true.
        end if

        if(RIN%gases%igab) then

            if(RIN%gases%integration_method .eq. 0) then

                call cstring2fstring(RIN%DLSF%internal_file_path, internal_file_path)
                call cstring2fstring( RIN%gases%path_to_luts, lut_path)

                do j =1, RIN%gases%nlut_name
                    call cstring2fstring( RIN%gases%lut_name(:,j), lut_name)
                    call read_lut_head(TRIM(internal_file_path)//TRIM(lut_path), &
                                       TRIM(lut_name),NWL_LUT,NP_LUT,NT_LUT)
!                    call alloc_lut_gas_header(j,NWL_LUT,NP_LUT,NT_LUT)
                    call alloc_lut_gas_fixed(j,NWL_LUT,NP_LUT,NT_LUT)
                end do

            end if

            if(RIN%gases%integration_method .eq. 1) then
                !MH The total number of channels is the maximum size
                
                do j=1,RIN%NW
                    call alloc_kd_gas(j)
                end do
            end if

!MHG If there is at least one filter with a defined shape we allocate the full matrix for all channels. This is done to keep consistency with teh index IW used in the rest of the forward model

            do IW=1,RIN%NW
                do IS=1,RIN%gases%ngas_filters

                    do II = 1,RIN%gases%nfilters_index_of_wavelength_involved(IS)
                     
                          if(RIN%gases%filters_index_of_wavelength_involved(II,IS) == IW) then
                              if(RIN%gases%filters_meas_type(IS) .eq. meas_type_tod .or. &    !MH In near future this will be restricted just to htod
                                 RIN%gases%filters_meas_type(IS) .eq. meas_type_htod .or. &
                                 RIN%gases%filters_meas_type(IS) .eq. meas_type_I) then

                                   call cstring2fstring(RIN%gases%filters_file(:,II, IS), filters_file)

                                   if(TRIM(filters_file) .ne. '.') then

                                        do j=1,RIN%NW
                                            call alloc_filter_shape(j)
                                        end do

                                        read_filter_shape = .true.
                                        filter_exists = .true.

                                        exit

                                   end if

                              end if
                          end if
                        end do

                    end do
            end do

            read_gas_abs = .true.


        end if

!MHG Check if componentes approach is present in settings and alloc Refractive index luts
        par_type = 0
        nfract = 0
        do IDIM1=1,RIN%NDIM%n1
            par_type = RIN%NDIM%par_type(IDIM1)
            if( (par_type .EQ. par_type_CXRI_nmix) .OR. (par_type .EQ. par_type_CXRI_chem))then
                   
                    !MHG We are going to allocate chem luts with the bigger amount of components of all aerosol modes
                    do ISD=1,RIN%NSD
                        if(RIN%NDIM%n3(ISD,IDIM1) > nfract)then
                            nfract = RIN%NDIM%n3(ISD,IDIM1)
                        end if
                        if ( RIN%indep_par ) then
                            if(RIN%NDIM_plus%n3(ISD,IDIM1) > nfract)then
                                nfract = RIN%NDIM_plus%n3(ISD,IDIM1)
                            end if
                        endif
                    end do

                    call alloc_chem_lut(RIN%NSD,nfract)
            
                    read_chem_lut = .true.
                    exit
            end if

        end do

        ! Initialize inversion subsystem
        call inversion_init(RIN)

        grasp_init_inversion=0
        return
    end function grasp_init_inversion
    


    
    function grasp_input_inversion(RIN,sdata, iguess, edges, ROUT) bind(C)                              
        type(retr_input_settings),         intent(inout)  :: RIN
        type(segment_data),                intent(inout)  :: sdata
        real(C_FLOAT),                     intent(in)     :: iguess(KPARS,KIMAGE)
        type(segment_edges),               intent(in)     :: edges   
        type(output_segment_general),      intent(out)    :: ROUT
        integer                                           :: grasp_input_inversion
        
        grasp_input_inversion=0
        ! here is how to display a segment from the FORTRAN side (for debugging)
        call initialize_stop_report()
        call set_segment_for_stop_report(sdata)
        call grasp_prepare_segment_settings(RIN, sdata)
        if (error_present()) then
            call print_stop_report()
            grasp_input_inversion=-2
            return
        endif
        call inversion(RIN, 6, sdata, iguess, edges, ROUT, KERNELS1, KERNELS2, US)
        if (error_present()) then
            call print_stop_report()
            grasp_input_inversion=-1
            return
        endif
        return 
    end function grasp_input_inversion
    
    
    subroutine grasp_finalize_inversion(RIN) bind(C)
        type(retr_input_settings),     intent(in)  :: RIN
        integer                                    :: j,size_kd,c_sub
        ! Finalize inversion subsystem
        call inversion_finalize()
        ! dealloc kernels and Jacobian matrix
        call dealloc_arrays(RIN,KERNELS1,KERNELS2,US)

        !MH dealloc gas look-up-tables

        if(RIN%gases%igab .and. (RIN%gases%integration_method .eq. 0)) then
                call dealloc_lut_gas(RIN%gases%nlut_name)
        end if

        !MH dealloc gas k-distribution data

        if(RIN%gases%igab .and. (RIN%gases%integration_method .eq. 1)) then
                call dealloc_kd_gas(RIN%NW)
        end if

        !MH dealloc atmospheric information data

        if(RIN%gases%igab .or. RIN%emission%planck) then
                call dealloc_atm_prof()
        end if

        !MH dealloc gas k-distribution data

        if(RIN%gases%igab .and. (filter_exists)) then
                call dealloc_filter_shape(RIN%NW)
        end if

    end subroutine grasp_finalize_inversion
    

end module mo_grasp_controller


    

