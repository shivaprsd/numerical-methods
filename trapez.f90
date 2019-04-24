program integrate
    implicit none
    real :: a, b
    integer :: n

    print *, "Enter a, b, n:"
    read *, a, b, n
    print *, trapez(func, a, b, n)

    contains
        real function func(x)
            implicit none
            real, intent(in) :: x
            func = sin(x)
        end function

        real function trapez(f, a, b, n)
            implicit none
            real, external :: f
            real, intent(in) :: a, b
            integer, intent(in) :: n
            integer :: i
            real :: s, h

            h = (b - a) / n
            s = sum((/ (f(a + i * h), i = 1, n - 1) /))
            trapez = h * (0.5 * (f(a) + f(b)) + s)
        end function
end program
