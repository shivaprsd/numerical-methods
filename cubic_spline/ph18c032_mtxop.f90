! File: ph18c032_mtxop.f90
! Description: Module containing mathematical functions for linear matrix
!              operations and other subroutines for matrix manipulations.
! Language: Fortran 90
! Version: 2.0
! Author: Shivaprasad V
! Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
! Date: 08 Mar 2019
! Comments: This is a Fortran module file with no main(). Compiling will produce
!           only a .mod (module) file. Use in conjunction with a main program.

module mtxop
    character, parameter :: TAB = achar(9)  ! '\t'
    type Matrix     ! structure for storing a 2D real matrix and its attributes
        integer :: rows, cols
        real, allocatable, dimension(:,:) :: elems
        logical :: is_singular
    end type

    contains
        ! Initialise a Matrix instance.
        subroutine init_mtx(rows, cols, mtx)
            implicit none
            integer, intent(in) :: rows, cols
            type(Matrix), intent(out) :: mtx

            allocate(mtx%elems(rows, cols))
            mtx%elems = 0.0    ! zero out all the elements
            mtx%rows = rows; mtx%cols = cols
            mtx%is_singular = .false.
        end subroutine

        ! Finalise a Matrix instance and deallocate acquired memory.
        subroutine cleanup(mtx)
            implicit none
            type(Matrix), intent(inout) :: mtx

            deallocate(mtx%elems)
            mtx%rows = 0; mtx%cols = 0;
        end subroutine

        ! Read elements into a Matrix row wise from stdin or (optionally) from
        ! a file specified by <file_name>.
        ! <row_label> is an optional str to be printed at the beginning of each
        ! row of input, followed by the row index (e.g. row1:, row2:, etc)
        subroutine read_mtx(mtx, row_label, file_name)
            implicit none
            type(Matrix), intent(inout) :: mtx
            character(len=*), optional, intent(in) :: row_label, file_name
            integer :: i, j, io, f = 5

            if(present(file_name)) then
                f = 10      ! arbitrary unit number
                open(f, file=file_name, action="read")
            end if

            do i = 1, mtx%rows
                if (present(row_label)) write (*, "(1x, a, i0, ':', a)",&
                                              &advance="no") row_label, i, TAB
                do
                    read (f, *, iostat=io) (mtx%elems(i, j), j = 1, mtx%cols)
                    if (io == 0) exit
                end do
            end do
            if (f == 5) then
                write (*,*)     ! append '\n' if stdin
            else
                close(f)        ! close if file
            end if
        end subroutine

        ! Print elements of a Matrix as a table to stout.
        ! <offset> is an optional string by which each row after the first is to
        ! be offsetted (e.g. for printing the matrix at the end of a line).
        ! <as_sqr> is an optional Boolean to tell whether to print the matrix as
        ! a square matrix (i.e., omitting any augmented part) or not; setting it
        ! to .true. *assumes* that mtx%cols >= mtx%rows; default is .false.
        subroutine print_mtx(mtx, offset, as_sqr)
            implicit none
            type(Matrix), intent(in) :: mtx
            character(len=*), optional, intent(in) :: offset
            logical, optional, intent(in) :: as_sqr
            integer :: i, j, j_max

            if (present(as_sqr)) then
                j_max = mtx%rows    ! no. of cols = no. of rows for square mtx
            else
                j_max = mtx%cols
            end if
            do i = 1, (mtx%rows)
                ! Print row as: | a1  a2  a3 ... |
                write (*, "(a)", advance="no") "|   "
                write (*, "(*(g10.3))", advance='no') (mtx%elems(i, j), j = 1, j_max)
                print *, "|"
                ! Print <offset> to the next line
                if (present(offset)) write (*, "(a)", advance='no') offset
            end do
            write (*,*)     ! append '\n'
        end subroutine

        ! Rotate the elements of given 1-D array <vec> upto <n>th index one step
        ! left or right. If <n> is not given, rotate the entire array. Direction
        ! can be specified by the optional Boolean <left>; default is .true.
        function rotate(vec, n, left)
            implicit none
            real, dimension(:), intent(in) :: vec
            integer, optional, intent(in) :: n
            logical, optional, intent(in) :: left
            real, dimension(size(vec)) :: rotate
            integer :: i

            if (present(n) .and. n < size(vec)) then
                i = n           ! rotate upto n
            else
                i = size(vec)   ! rotate full array
            end if

            ! Rotating is done by slicing and creating a temporary array
            if (.not. present(left) .or. left) then
                rotate = (/ vec(2 : i), vec(1), vec(i + 1 :) /)
            else
                rotate = (/ vec(i), vec(1 : i - 1), vec(i + 1 :) /)
            end if
        end function

        ! Solve a tridiagonal system of equations using Thomas algorithm.
        ! <mtx> is expected to be an Nx4 matrix in the format [a|b|c|d] where
        ! a, b and c are column vectors containing respectively the subdiagonal,
        ! diagonal, superdiagonal elements of the original tridiagonal matrix
        ! (with a[0] = c[n] = 0) and d is the column vector of the constants.
        subroutine thomas(mtx)
            implicit none
            type(Matrix), target, intent(inout) :: mtx
            real, dimension(:,:), pointer :: m
            integer :: i

            m => mtx%elems
            do i = 2, mtx%rows
                ! Eliminate all a[i]'s, operating row-wise
                m(i, :) = m(i, :) * m(i - 1, 2) - rotate(m(i - 1, :), 3) * m(i, 1)
            end do
            do i = 1, mtx%rows
                ! If any b[i] is zero, the matrix is singular
                if (m(i, 2) == 0) then
                    mtx%is_singular = .true.
                    return
                end if
                m(i, :) = m(i, :) / m(i, 2)     ! Divide out the b[i]'s
            end do
            ! Back substitute row-wise & obtain the solution in the last column
            do i = mtx%rows - 1, 1, -1          ! last row is already solved
                m(i, :) = m(i, :) * m(i + 1, 2) - rotate(m(i + 1, :), 3, .false.) * m(i, 3)
            end do
        end subroutine
end module mtxop
