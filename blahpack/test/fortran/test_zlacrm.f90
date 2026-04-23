program test_zlacrm
  use test_utils
  implicit none

  ! Test arrays sized exactly to the test cases (declared per case via subroutine)
  call run_case_3x3()
  call run_case_4x2()
  call run_case_2x4()
  call run_case_1x1()
  call run_case_m0()
  call run_case_n0()

contains

  subroutine run_case_3x3()
    integer, parameter :: m = 3, n = 3
    complex*16 :: A(m, n), C(m, n)
    double precision :: A_r(2*m*n), C_r(2*m*n)
    double precision :: B(n, n)
    double precision :: RWORK(2*m*n)
    integer :: i, j
    equivalence (A, A_r)
    equivalence (C, C_r)

    do j = 1, n
      do i = 1, m
        A(i, j) = dcmplx( dble(i + 2*j), dble(3*i - j) )
      end do
    end do
    do j = 1, n
      do i = 1, n
        B(i, j) = dble(i) + 0.5d0*dble(j) - 0.25d0
      end do
    end do
    C = (0.0d0, 0.0d0)

    call ZLACRM(m, n, A, m, B, n, C, m, RWORK)

    call begin_test('basic_3x3')
    call print_int('M', m)
    call print_int('N', n)
    call print_array('A', A_r, 2*m*n)
    call print_array('B', B, n*n)
    call print_array('C', C_r, 2*m*n)
    call end_test()
  end subroutine

  subroutine run_case_4x2()
    integer, parameter :: m = 4, n = 2
    complex*16 :: A(m, n), C(m, n)
    double precision :: A_r(2*m*n), C_r(2*m*n)
    double precision :: B(n, n)
    double precision :: RWORK(2*m*n)
    integer :: i, j
    equivalence (A, A_r)
    equivalence (C, C_r)

    do j = 1, n
      do i = 1, m
        A(i, j) = dcmplx( dble(i*3 - j), dble(2*i + j) )
      end do
    end do
    do j = 1, n
      do i = 1, n
        B(i, j) = dble(i*2 - j)
      end do
    end do
    C = (0.0d0, 0.0d0)

    call ZLACRM(m, n, A, m, B, n, C, m, RWORK)

    call begin_test('rect_4x2')
    call print_int('M', m)
    call print_int('N', n)
    call print_array('A', A_r, 2*m*n)
    call print_array('B', B, n*n)
    call print_array('C', C_r, 2*m*n)
    call end_test()
  end subroutine

  subroutine run_case_2x4()
    integer, parameter :: m = 2, n = 4
    complex*16 :: A(m, n), C(m, n)
    double precision :: A_r(2*m*n), C_r(2*m*n)
    double precision :: B(n, n)
    double precision :: RWORK(2*m*n)
    integer :: i, j
    equivalence (A, A_r)
    equivalence (C, C_r)

    do j = 1, n
      do i = 1, m
        A(i, j) = dcmplx( dble(i + j), dble(i - j) )
      end do
    end do
    do j = 1, n
      do i = 1, n
        B(i, j) = dble(i) - 0.5d0*dble(j)
      end do
    end do
    C = (0.0d0, 0.0d0)

    call ZLACRM(m, n, A, m, B, n, C, m, RWORK)

    call begin_test('rect_2x4')
    call print_int('M', m)
    call print_int('N', n)
    call print_array('A', A_r, 2*m*n)
    call print_array('B', B, n*n)
    call print_array('C', C_r, 2*m*n)
    call end_test()
  end subroutine

  subroutine run_case_1x1()
    integer, parameter :: m = 1, n = 1
    complex*16 :: A(m, n), C(m, n)
    double precision :: A_r(2*m*n), C_r(2*m*n)
    double precision :: B(n, n)
    double precision :: RWORK(2*m*n)
    equivalence (A, A_r)
    equivalence (C, C_r)

    A(1, 1) = dcmplx(2.5d0, -1.25d0)
    B(1, 1) = 4.0d0
    C = (0.0d0, 0.0d0)

    call ZLACRM(m, n, A, m, B, n, C, m, RWORK)

    call begin_test('one_by_one')
    call print_int('M', m)
    call print_int('N', n)
    call print_array('A', A_r, 2*m*n)
    call print_array('B', B, n*n)
    call print_array('C', C_r, 2*m*n)
    call end_test()
  end subroutine

  subroutine run_case_m0()
    integer, parameter :: m = 0, n = 3
    complex*16 :: A(1, n), C(1, n)
    double precision :: B(n, n)
    double precision :: RWORK(1)

    A = (7.0d0, 7.0d0)
    C = (9.0d0, 9.0d0)
    B = 1.0d0

    call ZLACRM(m, n, A, 1, B, n, C, 1, RWORK)

    call begin_test('m_zero')
    call print_int('M', m)
    call print_int('N', n)
    call end_test()
  end subroutine

  subroutine run_case_n0()
    integer, parameter :: m = 3, n = 0
    complex*16 :: A(m, 1), C(m, 1)
    double precision :: B(1, 1)
    double precision :: RWORK(1)

    A = (7.0d0, 7.0d0)
    C = (9.0d0, 9.0d0)
    B = 1.0d0

    call ZLACRM(m, n, A, m, B, 1, C, m, RWORK)

    call begin_test('n_zero')
    call print_int('M', m)
    call print_int('N', n)
    call end_test()
  end subroutine

end program
