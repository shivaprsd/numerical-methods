! File: ph18c032_mtxop.f90
! Description: Module containing mathematical functions for linear matrix
!              operations and other subroutines for matrix manipulations.
! Language: Fortran 90
! Version: 1.0
! Author: Shivaprasad V
! Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
! Date: 19 Feb 2019
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

        ! Read elements into a Matrix row wise.
        ! <row_label> is an optional str to be printed at the beginning of each
        ! row of input, followed by the row index (e.g. row1:, row2:, etc)
        subroutine read_mtx(mtx, row_label)
            implicit none
            type(Matrix), intent(inout) :: mtx
            character(len=*), optional, intent(in) :: row_label
            integer :: i, j

            do i = 1, mtx%rows
                if (present(row_label)) write (*, "(1x, a, i0, ':', a)",&
                                              &advance="no") row_label, i, TAB
                read *, (mtx%elems(i, j), j = 1, mtx%cols)
            end do
            write (*,*)     ! append '\n'
        end subroutine

        ! Print elements of a Matrix as a table.
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

        ! Partially pivot a Matrix at its <i>th row & column.
        subroutine pivot(mtx, i)
            implicit none
            type(Matrix), intent(inout) :: mtx
            integer, intent(in) :: i
            integer :: piv

            ! Slice the column down the pivot point, and find the location of
            ! the largest (absolute) value (relative to i)
            piv = (i - 1) + maxloc(abs(mtx%elems(i:, i)), 1)
            ! Swap rows if a larger value was found
            if (piv /= i) call swap(mtx, i, piv)
        end subroutine

        ! Swap <row1> with <row2> of the given Matrix.
        subroutine swap(mtx, row1, row2)
            implicit none
            type(Matrix), intent(inout) :: mtx
            integer, intent(in) :: row1, row2
            real, dimension(mtx%cols) :: tmp

            tmp = mtx%elems(row1, :)
            mtx%elems(row1, :) = mtx%elems(row2, :)
            mtx%elems(row2, :) = tmp
        end subroutine

        ! Augment the <trg> Matrix with the augmented part of the <src> Matrix.
        ! This subroutine assumes that the arguments are augmented matrices, and
        ! just copies the last column of <src> into the last column of <trg>
        subroutine augment(src, trg)
            implicit none
            type(Matrix), intent(in) :: src
            type(Matrix), intent(inout) :: trg
            integer :: i

            if (src%rows == trg%rows) then  ! copy only if the no. of rows match
                trg%elems(:, trg%cols) = src%elems(:, src%cols)
            end if
        end subroutine

        ! Convert a Matrix into the row reduced form by Gauss elimination.
        ! If it cannot be row reduced, the <is_singular> flag of the Matrix
        ! is set to .true.
        subroutine row_reduce(mtx)
            implicit none
            type(Matrix), target, intent(inout) :: mtx
            integer :: i, j
            real, dimension(:,:), pointer :: a

            a => mtx%elems
            do i = 1, mtx%rows
                call pivot(mtx, i)  ! pivot the matrix at the current row & col
                do j = i + 1, mtx%rows
                    ! If the row leading element is zero even after pivoting,
                    ! the Matrix is singular and cannot be row-reduced.
                    if (a(i, i) == 0) then
                        mtx%is_singular = .true.
                        return
                    end if
                    ! Gauss elimination method: row_j = row_j - m * row_i
                    a(j, :) = a(j, :) - a(i, :) * a(j, i) / a(i, i)
                end do
            end do
        end subroutine

        ! Perform an in-place back (or forward) substitution on an (augmented)
        ! upper (or lower) triangular matrix; i.e., convert it into [I|x] form.
        ! The Boolean argument <back> determines whether the substitution is
        ! back or forward. If substitution is impossible, the <is_singular> flag
        ! of the Matrix is set to .true.
        subroutine subst(mtx, back)
            implicit none
            type(Matrix), target, intent(inout) :: mtx
            logical, intent(in) :: back
            integer :: i, j, start_row, end_row, stride
            real, dimension(:,:), pointer :: a

            if (back) then
                start_row = mtx%rows; end_row = 1; stride = -1  ! bottom-up
            else
                start_row = 1; end_row = mtx%rows; stride = +1  ! top-down
            end if

            a => mtx%elems
            do i = start_row, end_row, stride
                ! If the diagonal element of the triangular matrix is 0,
                ! it is singular, and cannot be back substituted.
                if (a(i, i) == 0) then
                    mtx%is_singular = .true.
                    return
                end if
                do j = 1, mtx%rows
                    if (back .and. j <= i) cycle        ! j > i for back sub.
                    if (.not. back .and. j >= i) exit   ! j < i for forward sub.
                    ! Substitution algorithm: row_i = row_i - sum(row_j * a_ij)
                    a(i, :) = a(i, :) - a(j, :) * a(i, j)
                end do

                ! Divide the entire row by a(i, i) to make the latter 1
                a(i, :) = a(i, :) / a(i, i)
            end do
        end subroutine

        ! Decompose A as the product of L (unit lower triangular) and U (upper
        ! triangular) matrices. If A is augmented matrix, only the square part 
        ! is decomposed (assumes a%cols >= a%rows). Returns without completing
        ! if decomposition is not possible (A is singular).
        subroutine ludec(a, l, u)
            implicit none
            type(Matrix), intent(in) :: a
            type(Matrix), intent(inout) :: l, u
            integer :: i, j
            real :: s

            ! Crout's method (actually Doolittle's algorithm):
            do j = 1, a%rows
                ! For i < j: u_ij = a_ij - sum(l_ik * u_kj)
                do i = 1, j
                    if (i == j) l%elems(i, j) = 1   ! l_ii = 1
                    s = dot_product(l%elems(i, :i-1), u%elems(:i-1, j))
                    u%elems(i, j) = a%elems(i, j) - s
                end do
                if (u%elems(j, j) == 0) return  ! early return
                ! For i > j: l_ij = (a_ij - sum(l_ik * u_kj)) / u_jj
                do i = j + 1, a%rows
                    s = dot_product(l%elems(i, :i-1), u%elems(:i-1, j))
                    l%elems(i, j) = (a%elems(i, j) - s) / u%elems(j, j)
                end do
            end do
        end subroutine
end module mtxop
