! File: ph18c032_cspline.f90
! Description: Program to generate a natural cubic spline through the given data points.
! Language: Fortran 90
! Version: 1.0
! Author: Shivaprasad V
! Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
! Date: 08 Mar 2019
! Comments: This program is organised into two files: the main program (this
!           file) and a separate functions module (ph18c032_mtxop.f90). Both
!           of them should be compiled and linked together. e.g.:
!           $ gfortran ph18c032_mtxop.f90 ph18c032_cspline.f90

program cubic_spline
    use mtxop
    implicit none
    type(Matrix), target :: data_mtx, eqn_mtx, curv_mtx
    real, dimension(:,:), pointer :: d, e, M    ! aliases to matrix elements
    character(len=30) :: data_file              ! str to store file_names
    real h1, h2, x, sp                  ! intervals, data point, spacing
    integer :: n, i, io, f = 6          ! unit number for stdout
10  format (a)                          ! print string
11  format (f0.3, a, f0.7)              ! print x \t y

    print 10, "Hello, this program will generate a natural cubic spline through the given data"
    print 10, "points. Data can be fed from a data file. Invalid lines (like comments starting"
    print 10, "with #, !, //, etc) will be ignored. Leave the filename empty to enter the data"
    print 10, "manually via stdin. Interpolated points can be written to given file or stdout."
    write (*,*)                         ! print \n
    write (*, 10, advance="no") "Data file name > "
    read 10, data_file

    ! Read data from file if a file name is provided
    if (len_trim(data_file) /= 0) then
        n = count_dat(trim(data_file))  ! count number of data points
        if (n < 0) then
            print 10, "File does not exist!"
            return
        end if
        call init_mtx(n, 2, data_mtx)
        write (*, 10, advance="no") "Reading file... "
        call read_mtx(data_mtx, file_name=trim(data_file))
        print 10, "done"
        write (*,*)                     ! append '\n'
    ! Read manually from stdin in file name is empty
    else
        write (*, 10, advance="no") "Number of datapoints > "
        read *, n
        call init_mtx(n, 2, data_mtx)
        print 10, "Enter the datapoints:"
        print "(*(a))", TAB, "X", TAB, "Y"
        call read_mtx(data_mtx, "P")
    end if

    call init_mtx(n - 2, 4, eqn_mtx)    ! tridiagonal matrix for n - 2 equations
    call init_mtx(n, 1, curv_mtx)       ! column matrix for n second derivatives
    d => data_mtx%elems
    e => eqn_mtx%elems
    M => curv_mtx%elems

    ! Calculate elements of the tridiagonal matrix, and the constants
    do i = 2, n - 1
        h1 = d(i, 1) - d(i - 1, 1)
        h2 = d(i + 1, 1) - d(i, 1)
        e(i - 1, 1:3) = (/ h1, 2 * (h1 + h2), h2 /)
        e(i - 1, 4) = 6 * ((d(i + 1, 2) - d(i, 2)) / h2&
                        &- (d(i, 2) - d(i - 1, 2)) / h1)
    end do
    e(1, 1) = 0; e(n - 2, 3) = 0        ! a[0] = c[n - 2] = 0

    call thomas(eqn_mtx)                ! solve the tridiagonal system
    if (eqn_mtx%is_singular) then
        print 10, "Cannot generate spline! Check your data..."
        call cleanup(data_mtx); call cleanup(eqn_mtx); call cleanup(curv_mtx)
        return
    end if
    M(2: n - 1, 1) = e(:, 4)            ! M[0] = M[n] = 0; populate the rest
    print 10, "Calculated second derivatives (last column):"
    call print_mtx(eqn_mtx)

    write (*, 10, advance="no") "Required interpolation spacing > "
    read *, sp
    write (*, 10, advance="no") "Output file name (leave empty for stdout) > "
    read "(a)", data_file
    ! Open file for output if file name is provided
    if (len_trim(data_file) /= 0) then
        open(20, file=data_file, action="write", iostat=io)
        if (io == 0) f = 20             ! out file opened successfully
        write (*, 10, advance="no") "Writing output... "
    end if

    ! Calculate and print the interpolated points, starting from x[0]
    x = d(1, 1)
    do i = 1, n - 1                     ! lower index of the interval
        do
            ! Calculate s(x) using parameters for the interval (i, i + 1)
            write (f, 11) x, TAB, cbx(d(i: i + 1, 1), d(i: i + 1, 2), M(i: i + 1, 1), x)
            x = x + sp                  ! increment x
            if (x >= d(i + 1, 1)) exit  ! reached the next interval, break & increment i
        end do
    end do
    ! Print s(x[n]) using the parameters for the interval (n - 1, n)
    write (f, 11) d(n, 1), TAB, cbx(d(i - 1: i, 1), d(i - 1: i, 2), M(i - 1: i, 1), x)
    print 10, "done"
    call cleanup(data_mtx); call cleanup(eqn_mtx); call cleanup(curv_mtx)

    contains
        ! Count the number of lines with numerical data in <in_file>
        integer function count_dat(in_file)
            implicit none
            character(len=*), intent(in) :: in_file
            integer :: io, f = 30
            real :: tmp

            open(f, file=in_file, action="read", iostat=io)
            if (io /= 0) then           ! file does not exist
                count_dat = -1
                close(f)
                return
            end if

            count_dat = 0               ! start counting
            do
                read (f, *, iostat=io) tmp
                if (io < 0) then        ! EOF
                    close(f)
                    return
                end if
                ! If no error, a numerical value was read, so count
                if (io == 0) count_dat = count_dat + 1
            end do
        end function

        ! Calculate the cubic spline at x in the interval bound by the points (xi[1], yi[1])
        ! and (xi[2], yi[2]), with second derivatives M[1] and M[2] at the end points.
        real function cbx(xi, yi, Mi, x)
            implicit none
            real, dimension(2), intent(in) :: xi, yi, Mi
            real, intent(in) :: x
            real :: h

            h = xi(2) - xi(1)           ! interval size
            ! Equation of cubic spline, given parameters:
            cbx = ((xi(2) - x) ** 3 * Mi(1) / 6 + (x - xi(1)) ** 3 * Mi(2) / 6&
                 &+ (yi(1) - h ** 2 * Mi(1) / 6) * (xi(2) - x)&
                 &+ (yi(2) - h ** 2 * Mi(2) / 6) * (x - xi(1))) / h
         end function
end program
