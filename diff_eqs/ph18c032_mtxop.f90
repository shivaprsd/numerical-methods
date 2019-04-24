! File: ph18c032_mtxop.f90
! Description: Module containing mathematical functions for linear matrix
!              operations and other subroutines for matrix manipulations.
! Language: Fortran 90
! Version: 2.1
! Author: Shivaprasad V
! Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
! Date: 08 Apr 2019
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
