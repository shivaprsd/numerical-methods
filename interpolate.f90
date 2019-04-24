! File: interpol.f90
! Description: Interpolate a given data set.
! Language: Fortran 90
! Version: 1.0
! Author: Shivaprasad V
! Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
! Date: 26 Feb 2019

program interpolate
    use mtxop
    use polyop
    implicit none
    type(Matrix) :: data_mtx, tmp_mtx
    integer :: i, x, n

    print *, "Enter n"
    read *, n
    print *, "Enter data:"
    print "(*(a))", TAB, "X", TAB, "Y"

    call init_mtx(n, 2, data_mtx)
    call init_mtx(3, 2, tmp_mtx)
    call read_mtx(data_mtx, "P")
    call print_mtx(data_mtx)

    print *, "Interpolated data:"
    print "(*(a))", "X", TAB, "Full order", TAB, "Piecewice quadratic"
    do i = 1, n - 2, 2
        tmp_mtx%elems = data_mtx%elems(i : i + 2, :)
        do x = int(tmp_mtx%elems(1, 1)), int(tmp_mtx%elems(3, 1)) - 1, 5
            print "(i0, a, g10.3, a, g10.3)", x, TAB, interpol(data_mtx, real(x)), TAB, interpol(tmp_mtx, real(x))
        end do
    end do
    call cleanup(data_mtx)
end program
