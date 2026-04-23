program test_zgelqf
  use test_utils
  implicit none

  ! Large arrays for blocked test
  double precision :: a_r(6000), tau_r(200), work_r(20000)
  complex*16 :: a(3000), tau(100), work(10000)
  equivalence (a, a_r)
  equivalence (tau, tau_r)
  integer :: info, lwork, i, j, M, N, LDA

  lwork = 10000

  ! Test 1: 3x5 matrix (M < N, typical LQ case)
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
  a(13) = (7.0d0, 1.0d0)
  a(14) = (2.0d0, 3.0d0)
  a(15) = (4.0d0, 5.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelqf(3, 5, a, 3, tau, work, lwork, info)
  call begin_test('basic_3x5')
  call print_int('info', info)
  call print_array('a', a_r, 30)
  call print_array('tau', tau_r, 6)
  call end_test()

  ! Test 2: 4x4 square matrix
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0)
  a(2) = (0.0d0, 1.0d0)
  a(3) = (1.0d0, 0.0d0)
  a(4) = (2.0d0, 0.5d0)
  a(5) = (2.0d0, 0.5d0)
  a(6) = (1.0d0, 1.0d0)
  a(7) = (0.5d0, 0.5d0)
  a(8) = (3.0d0, 1.0d0)
  a(9) = (0.0d0, 1.0d0)
  a(10) = (1.0d0, 0.0d0)
  a(11) = (2.0d0, 2.0d0)
  a(12) = (1.0d0, 1.0d0)
  a(13) = (3.0d0, 0.0d0)
  a(14) = (0.5d0, 1.5d0)
  a(15) = (1.0d0, 2.0d0)
  a(16) = (4.0d0, 0.5d0)
  tau = (0.0d0, 0.0d0)
  call zgelqf(4, 4, a, 4, tau, work, lwork, info)
  call begin_test('square_4x4')
  call print_int('info', info)
  call print_array('a', a_r, 32)
  call print_array('tau', tau_r, 8)
  call end_test()

  ! Test 3: M=0
  a = (0.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelqf(0, 5, a, 1, tau, work, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0
  a = (0.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelqf(3, 0, a, 3, tau, work, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: 1x1 matrix
  a(1) = (5.0d0, 3.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelqf(1, 1, a, 1, tau, work, lwork, info)
  call begin_test('one_by_one')
  call print_int('info', info)
  call print_array('a', a_r, 2)
  call print_array('tau', tau_r, 2)
  call end_test()

  ! Test 6: 2x5 wide (M < N, simple unblocked)
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
  tau = (0.0d0, 0.0d0)
  call zgelqf(2, 5, a, 2, tau, work, lwork, info)
  call begin_test('wide_2x5')
  call print_int('info', info)
  call print_array('a', a_r, 20)
  call print_array('tau', tau_r, 4)
  call end_test()

  ! Test 7: 4x3 tall matrix (M > N)
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
  call zgelqf(4, 3, a, 4, tau, work, lwork, info)
  call begin_test('tall_4x3')
  call print_int('info', info)
  call print_array('a', a_r, 24)
  call print_array('tau', tau_r, 6)
  call end_test()

  ! Test 8: 35x50 large matrix (exercises blocked path with NB=32)
  M = 35
  N = 50
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
  call zgelqf(M, N, a, LDA, tau, work, lwork, info)
  call begin_test('large_35x50')
  call print_int('info', info)
  call print_array('a', a_r, 2*M*N)
  call print_array('tau', tau_r, 2*M)
  call end_test()

end program
