! Program to print the n-th Fibonacci number
program fibonacci
    implicit none
    integer :: n, i, n1, n2, temp
    n1 = 0
    n2 = 1

    print *, "Hello, this program will print the n-th Fibonacci number."
    print *, "Supply the value of n:"
    read *, n

    ! n1 already contains the 1st term
    ! so we have to iterate only (n - 1) times
    do i = 1, n - 1
        temp = n1 + n2
        n1 = n2
        n2 = temp
    end do
    print *, n1
end program fibonacci
