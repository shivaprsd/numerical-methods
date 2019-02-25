! Program to solve a system of linear equations by Gauss elimination method
! Language: Fortran 90
! Author: Shivaprasad V
! Credentials: PH18C032, IIT Madras

program gauss_elimination
    use mtxop
    implicit none
    type(Matrix) :: aug_mtx
    integer :: n, i

    print *, "Hello, this program will solve a system of linear equations by &
             &Gauss elimination method."
    write (*, "(1x, a)", advance="no") "Enter the number of unknowns: "
    read *, n
    print *, "Enter the coefficients of the unknowns equation-wise:"
    write (*, 490, advance="no") (TAB, i, i = 1, n)
    print *, TAB // "RHS"

    call init_mtx(n, n + 1, aug_mtx)
    call read_mtx(aug_mtx, "Eqn")
    call row_reduce(aug_mtx)
    if (.not. aug_mtx%is_singular) call subst(aug_mtx, .true.)

    if (aug_mtx%is_singular) then
        print *, "No unique solution exists."
    else
        write (*, 491) (i, aug_mtx%elems(i, n + 1), i = 1, n)
    end if
    call cleanup(aug_mtx)

490 format ( 1x, *(a, "C(x", i0, ")") )
491 format ( 1x, *("x", i0, " = ", f0.3, : ", ") )
end program gauss_elimination
