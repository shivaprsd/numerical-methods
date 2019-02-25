! Program to find the root of a function by the Secant Method
program secant
    implicit none
    real :: n, x1, x2, eps
    real :: x3, f1, f2, f3

    print *, "This program will output the root of the function f(x) = x^2 - x - 6"
    print *, "given two initial guesses close to the root."
    write (*, *)
    print *, "Enter the guesses (x1 and x2):"
    read *, x1, x2
    print *, "Enter the required precision:"
    read *, eps
    eps = abs(eps)

    f1 = f(x1); f2 = f(x2)
    do
        x3 = x2 - f(x2) * (x2 - x1) / (f2 - f1)
        if (abs(x3 - x2) <= eps) then
            print *, "Root =", x3, "+/-", eps
            exit
        else
            f3 = f(x3)
            x1 = x2; x2 = x3
            f1 = f2; f2 = f3
        end if
    end do

    contains
        real function f(x)
            implicit none
            real, intent(in) :: x
            f = x ** 2 - x - 6
        end function
end program secant
