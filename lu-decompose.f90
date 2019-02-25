! File: ph18c032_lu-decompose.f90
! Description: Program to solve a system of linear equations by LU decomposition
!              of the equation matrix.
! Language: Fortran 90
! Version: 1.0
! Author: Shivaprasad V
! Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
! Date: 19 Feb 2019
! Comments: This program is organised into two files: the main program (this
!           file) and a separate functions module (ph18c032_mtxop.f90). Both
!           of them should be compiled and linked together. e.g.:
!           $ gfortran ph18c032_mtxop.f90 ph18c032_lu-decompose.f90

program lu_decomposition
    use mtxop
    implicit none
    type(Matrix) :: mtx, low, up
    integer :: n, i

    print *, "Hello, this program will solve a system of linear equations by &
             &LU decomposition method."
    write (*, "(1x, a)", advance="no") "Enter the number of unknowns: "
    read *, n
    print *, "Enter the coefficients of the unknowns equation-wise:"
    write (*, 490, advance="no") (TAB, i, i = 1, n)     ! print column titles
    print *, TAB // "RHS"
 
    ! All matrices are n * (n + 1) sized to give space for augmentation
    call init_mtx(n, n + 1, mtx); call init_mtx(n, n + 1, low); call init_mtx(n, n + 1, up)
    call read_mtx(mtx, "Eqn")   ! read the augmented matrix [A|b]
    ! Partially pivot the whole matrix to get larger elements to the diagonal
    do i = 1, n
        call pivot(mtx, i)
    end do
    call ludec(mtx, low, up)    ! decompose: A = LU
    ! Print L and U omitting their augmented parts (which is yet empty)
    write (*, "(1x, a)", advance="no") "L = "
    call print_mtx(low, "     ", .true.)
    write (*, "(1x, a)", advance="no") "U = "
    call print_mtx(up, "     ", .true.)

    call augment(mtx, low)      ! copy b from [A|b] to L
    call subst(low, .false.)    ! solve Ly = b
    if (.not. low%is_singular) then
        call augment(low, up)   ! copy the solution of Ly = b to U
        call subst(up, .true.)  ! solve Ux = y
    end if
    if (low%is_singular .or. up%is_singular) then
        print *, "No unique solution exists."
    else
        write (*, 491) (i, up%elems(i, n + 1), i = 1, n)    ! print x
    end if
    call cleanup(mtx); call cleanup(low); call cleanup(up)

490 format ( 1x, *(a, "C(x", i0, ")") )                 ! C(x1) C(x2) ...
491 format ( 1x, *("x", i0, " = ", f0.3, : ", ") )      ! x1 = _, x2 = _, ...
end program lu_decomposition
