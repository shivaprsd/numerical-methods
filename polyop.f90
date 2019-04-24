! File: polyop.f90
! Description: Module containing mathematical functions for polynomial operations
!              like Interpolation, Splining and Numerical Integration.
! Language: Fortran 90
! Version: 1.0
! Author: Shivaprasad V
! Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
! Date: 26 Feb 2019
! Comments: This is a Fortran module file with no main(). Compiling will produce
!           only a .mod (module) file. Use in conjunction with a main program.

module polyop
    use mtxop
    implicit none

    contains
        real function interpol(mtx, x)
            implicit none
            type(Matrix), target, intent(in) :: mtx
            real, intent(in) :: x
            real, dimension(:,:), pointer :: a
            real :: s, p
            integer :: i, j

            a => mtx%elems
            s = 0.0;
            do i = 1, mtx%rows
                p = 1.0;
                do j = 1, mtx%rows
                    if (j == i) cycle
                    p = p * (x - a(j, 1)) / (a(i, 1) - a(j, 1))
                end do
                s = s + a(i, 2) * p
                !s = s + a(1, i) * product((/ (x - a(1, i), i = 1, j - 1) /)) *&
                    !&product((/ (x - a(1, i), i = j + 1, mtx%rows) /)) /&
                    !&product((/ (a(1, j) - a(1, i), i = 1, j - 1) /)) /&
                    !&product((/ (a(1, j) - a(1, i), i = j + 1, mtx%rows) /))
            end do
            interpol = s
        end function
end module
