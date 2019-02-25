! Program to print all the primes between 1 and n, where n is supplied by the
! user
program primes
    implicit none
    integer :: n, i, j

    print *, "Hello, this program will print all the primes between 1 and n."
    print *, "Supply the value of n:"
    read *, n

    do i = 2, n, 1
        do j = 2, i / 2, 1
            if (mod(i, j) == 0) exit
        end do
        if (j > i / 2) print *, i
    end do
end program primes
