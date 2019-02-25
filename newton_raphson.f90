! Program to find the root of a function by Newton-Raphson method
! Language: Fortran 90
! Author: Shivaprasad V
! Credentials: PH18C032, IIT Madras

program newton_raphson
    implicit none
    real, parameter :: EPS = 0.0001
    real :: x0, dx, df

    print *, "Hello, this program will find the roots of x^2 - x - 6 to three &
             &decimals, starting from an initial guess."
    print *, "Enter initial guess, x0:"
    read *, x0

    do
        df = deriv(f, x0)
        if (df == 0) then
            print *, "Cannot proceed! Hit local extremum..."
            exit
        end if
        dx = -f(x0) / df
        if (abs(dx) < EPS) then
            print "(1x, a, f6.3)", "Root near the guess = ", x0
            exit
        end if
        x0 = x0 + dx
    end do

    contains
        real function f(x)
            implicit none
            real, intent(in) :: x
            f = x ** 2 - x - 6
        end function f

        real function deriv(f, x0)
            implicit none
            real, external :: f
            real, intent(in) :: x0
            real :: e

            e = x0 / 1000.0 + EPS   ! adding EPS to handle case: x0 = 0
            deriv = (f(x0 + e) - f(x0 - e)) / (2 * e)
        end function deriv
end program newton_raphson
