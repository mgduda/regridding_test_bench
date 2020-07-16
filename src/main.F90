program main

    use mpi
    use iso_c_binding, only : c_char, c_null_char, c_int, c_ptr, c_f_pointer

    implicit none

    interface 
        subroutine read_element_decomp(fcomm, filename, n_elems_global, n_elems, elems) bind(c)
            use iso_c_binding, only : c_char, c_int, c_ptr
            implicit none
            integer, value :: fcomm
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(kind=c_int), intent(inout) :: n_elems_global
            integer(kind=c_int), intent(inout) :: n_elems
            type (c_ptr), intent(inout) :: elems
        end subroutine read_element_decomp

        subroutine free_element_decomp(elems) bind(c)
            use iso_c_binding, only : c_ptr
            implicit none
            type (c_ptr), intent(inout) :: elems
        end subroutine free_element_decomp
    end interface

    integer :: i
    integer :: ierr
    integer :: comm_size, comm_rank

    character(kind=c_char), dimension(:), allocatable, target :: c_filename
    integer(kind=c_int) :: n_elems_global
    integer(kind=c_int) :: n_elems
    type (c_ptr) :: elems_ptr
    integer(kind=c_int), dimension(:), pointer :: elems

    character(len=256) :: src_nc_file, &   ! Name of source mesh netCDF file
                          src_part_file, & ! Name of source mesh partition file
                          dst_nc_file, &   ! Name of destination mesh netCDF file
                          dst_part_file    ! Name of destination mesh partition file

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
    ! Retrieve and validate command-line arguments, converting partition file prefixes
    ! into full partition filenames based on comm_size
    !
    if (get_args(comm_size, comm_rank, &
                 src_nc_file, src_part_file, &
                 dst_nc_file, dst_part_file) /= 0) then
        call fatal_error()
    end if

    write(0,*) 'Hello!'

    allocate(c_filename(len_trim(src_nc_file)+1))
    do i = 1, len_trim(src_nc_file)
        c_filename(i) = src_nc_file(i:i)
    end do
    c_filename(size(c_filename)) = c_null_char

    call read_element_decomp(MPI_COMM_WORLD, c_filename, n_elems_global, n_elems, elems_ptr)
    call c_f_pointer(elems_ptr, elems, [n_elems])

    deallocate(c_filename)

    call free_element_decomp(elems_ptr)

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
    ! Two of the command-line arguments -- the source mesh partition file prefix
    ! and the destination mesh partition file prefix -- are converted into
    ! the complete filenames for the source and destination mesh partition files
    ! using comm_size, the size of the MPI communicator.
    !
    !-----------------------------------------------------------------------
    function get_args(comm_size, comm_rank, &
                      src_nc_file, src_part_file, dst_nc_file, dst_part_file) result(stat)

        implicit none

        ! Arguments
        integer, intent(in) :: comm_size
        integer, intent(in) :: comm_rank
        character(len=*), intent(out) :: src_nc_file
        character(len=*), intent(out) :: src_part_file
        character(len=*), intent(out) :: dst_nc_file
        character(len=*), intent(out) :: dst_part_file

        ! Return value
        integer :: stat

        ! Local variables
        logical :: exists
        character(len=256) :: src_part_prefix, &
                              dst_part_prefix

        stat = 0

        if (command_argument_count() /= 4) then
            if (comm_rank == 0) then
                write(0,*) ''
                write(0,*) 'Usage: mrtb <src netCDF mesh> <src partition prefix> <dst netCDF mesh> <dst partition prefix>'
                write(0,*) ''
            end if

            ! Ensure that rank 0 has been able to print error messages before returning
            call MPI_Barrier(MPI_COMM_WORLD, ierr)

            stat = 1
            return
        end if

        call get_command_argument(1, src_nc_file)
        call get_command_argument(2, src_part_prefix)
        call get_command_argument(3, dst_nc_file)
        call get_command_argument(4, dst_part_prefix)

        !
        ! Build filenames of partition files
        !
        if (comm_size < 10) then
            write(src_part_file, '(a,i1.1)') trim(src_part_prefix), comm_size
            write(dst_part_file, '(a,i1.1)') trim(dst_part_prefix), comm_size
        else if (comm_size < 100) then
            write(src_part_file, '(a,i2.2)') trim(src_part_prefix), comm_size
            write(dst_part_file, '(a,i2.2)') trim(dst_part_prefix), comm_size
        else if (comm_size < 1000) then
            write(src_part_file, '(a,i3.3)') trim(src_part_prefix), comm_size
            write(dst_part_file, '(a,i3.3)') trim(dst_part_prefix), comm_size
        else if (comm_size < 10000) then
            write(src_part_file, '(a,i4.4)') trim(src_part_prefix), comm_size
            write(dst_part_file, '(a,i4.4)') trim(dst_part_prefix), comm_size
        else if (comm_size < 100000) then
            write(src_part_file, '(a,i5.5)') trim(src_part_prefix), comm_size
            write(dst_part_file, '(a,i5.5)') trim(dst_part_prefix), comm_size
        else if (comm_size < 1000000) then
            write(src_part_file, '(a,i6.6)') trim(src_part_prefix), comm_size
            write(dst_part_file, '(a,i6.6)') trim(dst_part_prefix), comm_size
        end if

        inquire(file=trim(src_nc_file), exist=exists)
        if (.not. exists) then
            if (comm_rank == 0) then
                write(0,*) ''
                write(0,*) 'Error: The source netCDF mesh file '//trim(src_nc_file)//' does not exist'
                write(0,*) ''
            end if
            stat = stat + 1
        end if

        inquire(file=trim(src_part_file), exist=exists)
        if (.not. exists) then
            if (comm_rank == 0) then
                write(0,*) ''
                write(0,*) 'Error: The source mesh partition file '//trim(src_part_file)//' does not exist'
                write(0,*) ''
            end if
            stat = stat + 1
        end if

        inquire(file=trim(dst_nc_file), exist=exists)
        if (.not. exists) then
            if (comm_rank == 0) then
                write(0,*) ''
                write(0,*) 'Error: The destination netCDF mesh file '//trim(dst_nc_file)//' does not exist'
                write(0,*) ''
            end if
            stat = stat + 1
        end if

        inquire(file=trim(dst_part_file), exist=exists)
        if (.not. exists) then
            if (comm_rank == 0) then
                write(0,*) ''
                write(0,*) 'Error: The destination mesh partition file '//trim(dst_part_file)//' does not exist'
                write(0,*) ''
            end if
            stat = stat + 1
        end if

        if (stat == 0 .and. comm_rank == 0) then
            write(0,*) ''
            write(0,*) 'Source netCDF mesh file:      '//trim(src_nc_file)
            write(0,*) 'Source partition file:        '//trim(src_part_file)
            write(0,*) 'Destination netCDF mesh file: '//trim(dst_nc_file)
            write(0,*) 'Destination partition file:   '//trim(dst_part_file)
            write(0,*) ''
        end if

        ! Ensure that rank 0 has been able to print error messages before returning
        call MPI_Barrier(MPI_COMM_WORLD, ierr)

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
