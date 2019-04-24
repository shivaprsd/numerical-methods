! File: ph18c032_pdesolver.f90
! Description: Program to solve PDE using Implicit method.
! Language: Fortran 90
! Version: 2.1
! Author: Shivaprasad V
! Credentials: PH18C032, M.Sc. Physics '18-'20, IIT Madras
! Date: 08 Apr 2019
! Comments: This program is organised into two files: the main program (this
!           file) and a separate functions module (ph18c032_mtxop.f90). Both
!           of them should be compiled and linked together. e.g.:
!           $ gfortran ph18c032_mtxop.f90 ph18c032_pdesolver.f90

program pde_solver
    use mtxop
    implicit none
    real, parameter :: PI = 4.d0 * atan(1.d0)
    real, parameter :: dt = 0.01, dx = 0.05
    real, parameter :: alpha = dt / (dx ** 2)
    integer, parameter :: M = nint(1 / dx) + 1, N = nint(1 / dt) + 1
    integer :: i, j
    type(Matrix), target :: eqn_mtx, sol_mtx, tmp_mtx
    real, dimension(:,:), pointer :: e, s, t    ! aliases to matrix elements

    call init_mtx(M - 2, 4, eqn_mtx); call init_mtx(M - 2, 4, tmp_mtx);
    call init_mtx(M, N, sol_mtx)
    e => eqn_mtx%elems
    s => sol_mtx%elems
    t => tmp_mtx%elems

    s(:, 1) = (/ (sin(PI * i * dx), i = 0, M) /)    ! initial boundary value
    do i = 1, M - 2
        e(i, 1:3) = (/ -alpha, 1 + 2 * alpha, -alpha /)     ! tridiagonal matrix
    end do
    e(1, 1) = 0; e(M - 2, 3) = 0        ! a[0] = c[n - 2] = 0

    do j = 2, N
        t(:, 1:3) = e(:, 1:3)           ! make copy of eqn matrix
        t(:, 4) = s(2: M - 1, j - 1)    ! copy column of previous iteration
        call thomas(tmp_mtx)
        s(: M - 1, j) = (/ 0.0, t(:, 4), 0.0 /) ! store iteration
    end do
    ! print result for plotting
    print *, "# Solution of 1D Diffusion Equation"
    print *, "x", TAB, "t", TAB, "T(x, t)"
    do j = 1, N
        do i = 1, M
            print *, (i - 1) * dx, (j - 1) * dt, s(i, j)
        end do 
    end do
    call cleanup(eqn_mtx); call cleanup(sol_mtx); call cleanup(tmp_mtx);
end program
