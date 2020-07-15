program main

    use mpi

    integer :: ierr
    integer :: comm_size, comm_rank

    character(len=256) :: src_nc_file, &     ! Name of source mesh netCDF file
                          src_part_prefix, & ! Name of source mesh partition file prefix
                          dst_nc_file, &     ! Name of destination mesh netCDF file
                          dst_part_prefix    ! Name of destination mesh partition file prefix

    !
    ! Initialize MPI
    !
    call MPI_Init(ierr)
    if (ierr /= MPI_SUCCESS) then
        write(0,*) ''
        write(0,*) 'Error: MPI_Init failed ', ierr
        write(0,*) ''
        stop 1
    end if

    call MPI_Comm_rank(MPI_COMM_WORLD, comm_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierr)

    !
    ! Retrieve and validate command-line arguments
    !
    if (get_args(src_nc_file, src_part_prefix, dst_nc_file, dst_part_prefix) /= 0) then
        call fatal_error()
    end if

    write(0,*) 'Hello!'

    !
    ! Finalize MPI
    !
    call MPI_Finalize(ierr)
    if (ierr /= MPI_SUCCESS) then
        write(0,*) ''
        write(0,*) 'Error: MPI_Finalize failed ', ierr
        write(0,*) ''
        stop 1
    end if

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


    !-----------------------------------------------------------------------
    !
    ! fatal_error
    !
    ! Terminates execution via a call to MPI_Abort
    !
    ! A call to this routine will terminate execution in MPI_COMM_WORLD
    ! via a call to MPI_Abort. Accordingly, a call to this routine should
    ! never return.
    !
    !-----------------------------------------------------------------------
    subroutine fatal_error()

        implicit none

        ! Local variables
        integer :: ierr

        call MPI_Abort(MPI_COMM_WORLD, 1, ierr)

    end subroutine fatal_error

end program main
