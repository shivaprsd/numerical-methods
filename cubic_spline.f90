! Program to plot cubic spline
program cubic_spline
    use mtxop
    implicit none
    type(Matrix), target :: data_mtx, eqn_mtx, curv_mtx
    real, dimension(:,:), pointer :: d, e, M
    character(len=30) :: data_file
    real h1, h2, x, sp
    integer :: n, i, io, f = 6
10  format (a)

    print 10, "Hello, this program will generate a natural cubic spline through the given data"
    print 10, "points. Data can be fed from a data file. Invalid lines (like comments starting"
    print 10, "with #, !, //, etc) will be ignored. Leave the filename empty to enter the data"
    print 10, "manually via stdin. Interpolated points can be written to given file or stdout."
    write (*,*)
    write (*, 10, advance="no") "Data file name > "
    read 10, data_file

    if (len_trim(data_file) /= 0) then
        n = count_dat(trim(data_file))
        if (n < 0) then
            print 10, "File does not exist!"
            return
        end if
        call init_mtx(n, 2, data_mtx)
        write (*, 10, advance="no") "Reading file... "
        call read_mtx(data_mtx, file_name=trim(data_file))
        print 10, "done"
        write (*,*)
    else
        write (*, 10, advance="no") "Number of datapoints > "
        read *, n
        call init_mtx(n, 2, data_mtx)
        print 10, "Enter the datapoints:"
        print "(*(a))", TAB, "X", TAB, "Y"
        call read_mtx(data_mtx, "P")
    end if

    call init_mtx(n - 2, 4, eqn_mtx)
    call init_mtx(n, 1, curv_mtx)
    d => data_mtx%elems
    e => eqn_mtx%elems
    M => curv_mtx%elems

    do i = 2, n - 1
        h1 = d(i, 1) - d(i - 1, 1)
        h2 = d(i + 1, 1) - d(i, 1)
        e(i - 1, 1:3) = (/ h1, 2 * (h1 + h2), h2 /)
        e(i - 1, 4) = 6 * ((d(i + 1, 2) - d(i, 2)) / h2&
                        &- (d(i, 2) - d(i - 1, 2)) / h1)
    end do
    e(1, 1) = 0; e(n - 2, 3) = 0

    call thomas(eqn_mtx)
    if (eqn_mtx%is_singular) return
    M(2: n - 1, 1) = e(:, 4)
    print 10, "Calculated second derivatives (last column):"
    call print_mtx(eqn_mtx)

    write (*, 10, advance="no") "Required interpolation spacing > "
    read *, sp
    write (*, 10, advance="no") "Output file name (leave empty for stdout) > "
    read "(a)", data_file
    if (len_trim(data_file) /= 0) then
        open(20, file=data_file, action="write", iostat=io)
        if (io == 0) f = 20
        write (*, 10, advance="no") "Writing output... "
    end if

    do i = 1, n - 1
        x = d(i, 1)
        do
            if (x >= d(i + 1, 1)) exit
            write (f, *) x, cbx(d(i: i + 1, 1), d(i: i + 1, 2), M(i: i + 1, 1), x)
            x = x + sp
        end do
    end do
    print 10, "done"

    contains
        integer function count_dat(in_file)
            implicit none
            character(len=*), intent(in) :: in_file
            integer :: io, f = 30
            real :: tmp

            open(f, file=in_file, action="read", iostat=io)
            if (io /= 0) then
                count_dat = -1
                close(f)
                return
            end if

            count_dat = 0
            do
                read (f, *, iostat=io) tmp
                if (io < 0) then
                    close(f)
                    return
                end if
                if (io == 0) count_dat = count_dat + 1
            end do
        end function

        real function cbx(xi, yi, Mi, x)
            implicit none
            real, dimension(2), intent(in) :: xi, yi, Mi
            real, intent(in) :: x
            real :: h

            h = xi(2) - xi(1)
            cbx = ((xi(2) - x) ** 3 * Mi(1) / 6 + (x - xi(1)) ** 3 * Mi(2) / 6&
                 &+ (yi(1) - h ** 2 * Mi(1) / 6) * (xi(2) - x)&
                 &+ (yi(2) - h ** 2 * Mi(2) / 6) * (x - xi(1))) / h
         end function
end program
