program main

    character(len=256) :: src_nc_file, &     ! Name of source mesh netCDF file
                          src_part_prefix, & ! Name of source mesh partition file prefix
                          dst_nc_file, &     ! Name of destination mesh netCDF file
                          dst_part_prefix    ! Name of destination mesh partition file prefix

    !
    ! Retrieve and validate command-line arguments
    !
    if (get_args(src_nc_file, src_part_prefix, dst_nc_file, dst_part_prefix) /= 0) then
        stop 1
    end if

    write(0,*) 'Hello!'

    stop

contains

    !-----------------------------------------------------------------------
    !
    ! get_args
    !
    ! Retrieve and validate command-line arguments
    !
    ! This routine first checks that the number of command-line arguments is
    ! correct, and, if it is, the command-line arguments are read and returned
    ! in the output arguments.
    !
    !-----------------------------------------------------------------------
    function get_args(src_nc_file, src_part_prefix, dst_nc_file, dst_part_prefix) result(stat)

        implicit none

        ! Arguments
        character(len=*), intent(out) :: src_nc_file
        character(len=*), intent(out) :: src_part_prefix
        character(len=*), intent(out) :: dst_nc_file
        character(len=*), intent(out) :: dst_part_prefix

        ! Return value
        integer :: stat

        ! Local variables
        logical :: exists

        stat = 0

        if (command_argument_count() /= 4) then
            write(0,*) ''
            write(0,*) 'Usage: mrtb <src netCDF mesh> <src partition prefix> <dst netCDF mesh> <dst partition prefix>'
            write(0,*) ''

            stat = 1
            return
        end if

        call get_command_argument(1, src_nc_file)
        call get_command_argument(2, src_part_prefix)
        call get_command_argument(3, dst_nc_file)
        call get_command_argument(4, dst_part_prefix)

        inquire(file=trim(src_nc_file), exist=exists)
        if (.not. exists) then
            write(0,*) ''
            write(0,*) 'Error: The source netCDF mesh file '//trim(src_nc_file)//' does not exist'
            write(0,*) ''
            stat = stat + 1
        end if

        inquire(file=trim(dst_nc_file), exist=exists)
        if (.not. exists) then
            write(0,*) ''
            write(0,*) 'Error: The destination netCDF mesh file '//trim(dst_nc_file)//' does not exist'
            write(0,*) ''
            stat = stat + 1
        end if

        if (stat == 0) then
            write(0,*) ''
            write(0,*) 'Source netCDF mesh file:           '//trim(src_nc_file)
            write(0,*) 'Source partition file prefix:      '//trim(src_part_prefix)
            write(0,*) 'Destination netCDF mesh file:      '//trim(dst_nc_file)
            write(0,*) 'Destination partition file prefix: '//trim(dst_part_prefix)
            write(0,*) ''
        end if

    end function get_args

end program main
