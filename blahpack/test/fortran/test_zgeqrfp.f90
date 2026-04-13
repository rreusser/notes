program test_zgeqrfp
  use test_utils
  implicit none

  ! Large arrays for blocked test
  double precision :: a_r(20000), tau_r(400), work_r(40000)
  complex*16 :: a(10000), tau(200), work(20000)
  equivalence (a, a_r)
  equivalence (tau, tau_r)
  integer :: info, lwork, i, j, M, N, LDA

  lwork = 20000

  ! Test 1: 4x3 matrix
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0)
  a(2) = (3.0d0, 4.0d0)
  a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0)
  a(5) = (9.0d0, 1.0d0)
  a(6) = (2.0d0, 3.0d0)
  a(7) = (4.0d0, 5.0d0)
  a(8) = (6.0d0, 7.0d0)
  a(9) = (8.0d0, 9.0d0)
  a(10) = (1.0d0, 2.0d0)
  a(11) = (3.0d0, 4.0d0)
  a(12) = (5.0d0, 6.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeqrfp(4, 3, a, 4, tau, work, lwork, info)
  call begin_test('basic_4x3')
  call print_int('info', info)
  call print_array('a', a_r, 24)
  call print_array('tau', tau_r, 6)
  call end_test()

  ! Test 2: 3x3 matrix
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0)
  a(2) = (0.0d0, 1.0d0)
  a(3) = (1.0d0, 0.0d0)
  a(4) = (2.0d0, 0.5d0)
  a(5) = (1.0d0, 1.0d0)
  a(6) = (0.5d0, 0.5d0)
  a(7) = (0.0d0, 1.0d0)
  a(8) = (1.0d0, 0.0d0)
  a(9) = (2.0d0, 2.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeqrfp(3, 3, a, 3, tau, work, lwork, info)
  call begin_test('square_3x3')
  call print_int('info', info)
  call print_array('a', a_r, 18)
  call print_array('tau', tau_r, 6)
  call end_test()

  ! Test 3: M=0
  a = (0.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeqrfp(0, 3, a, 1, tau, work, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0
  a = (0.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeqrfp(4, 0, a, 4, tau, work, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x4 wide
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0)
  a(2) = (2.0d0, 0.0d0)
  a(3) = (3.0d0, 2.0d0)
  a(4) = (4.0d0, 1.0d0)
  a(5) = (5.0d0, 3.0d0)
  a(6) = (6.0d0, 0.5d0)
  a(7) = (7.0d0, 1.0d0)
  a(8) = (8.0d0, 2.0d0)
  a(9) = (9.0d0, 0.0d0)
  a(10) = (0.0d0, 1.0d0)
  a(11) = (1.5d0, 2.5d0)
  a(12) = (3.5d0, 4.5d0)
  tau = (0.0d0, 0.0d0)
  call zgeqrfp(3, 4, a, 3, tau, work, lwork, info)
  call begin_test('wide_3x4')
  call print_int('info', info)
  call print_array('a', a_r, 24)
  call print_array('tau', tau_r, 6)
  call end_test()

  ! Test 6: 40x35 large matrix (exercises blocked path with NB=32)
  M = 40
  N = 35
  LDA = M
  a = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      a((j-1)*LDA + i) = dcmplx( &
        dble(mod(i*7 + j*13, 100)) / 10.0d0, &
        dble(mod(i*11 + j*3, 100)) / 10.0d0 &
      )
    end do
  end do
  tau = (0.0d0, 0.0d0)
  call zgeqrfp(M, N, a, LDA, tau, work, lwork, info)
  call begin_test('large_40x35')
  call print_int('info', info)
  call print_array('a', a_r, 2*M*N)
  call print_array('tau', tau_r, 2*N)
  call end_test()

  ! Test 7: 65x65 square (exercises >NB block plus cleanup)
  M = 65
  N = 65
  LDA = M
  a = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      a((j-1)*LDA + i) = dcmplx( &
        dble(mod(i*5 + j*17, 97)) / 10.0d0, &
        dble(mod(i*13 + j*7, 97)) / 10.0d0 &
      )
    end do
  end do
  tau = (0.0d0, 0.0d0)
  call zgeqrfp(M, N, a, LDA, tau, work, lwork, info)
  call begin_test('large_65x65')
  call print_int('info', info)
  call print_array('a', a_r, 2*M*N)
  call print_array('tau', tau_r, 2*N)
  call end_test()

end program
