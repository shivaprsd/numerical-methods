! Program to find the square root of a number by the Method of Bisection
program bisection
    implicit none
    real :: n, x1, x2, eps
    real :: x3, f1, f2, f3

    print *, "This program will output the square root of the given number"
    print *, "if it lies within the given lower and upper bounds."
    write (*, *)
    print *, "Enter the number and the bounds:"
    read *, n, x1, x2
    print *, "Enter the required precision:"
    read *, eps
    eps = abs(eps)

    f1 = f(x1, n); f2 = f(x2, n)
    if (abs(f1) <= eps) then
        print *, "sqrt", n, "=", x1
    else if (abs(f2) <= eps) then
        print *, "sqrt", n, "=", x2
    else if (f1 * f2 > 0) then
        print *, "No single root within the given bounds!"
    else
        do
            x3 = (x1 + x2) / 2.0    ! Regula falsi: replace by (f1 * x2 - f2 * x1) / (f1 - f2)
            f3 = f(x3, n)
            if (abs(f3) <= eps) then
                print *, "sqrt", n, "=", x3
                exit
            else if(f1 * f3 < 0) then
                x2 = x3
                f2 = f3
            else
                x1 = x3
                f1 = f3
            end if
        end do
    endif

    contains
        real function f(x, n)
            implicit none
            real, intent(in) :: x, n
            f = x ** 2 - n
        end function
end program bisection
