program test_zgeql2
  use test_utils
  implicit none

  double precision :: a_r(50), tau_r(10), work_r(20)
  complex*16 :: a(25), tau(5), work(10)
  equivalence (a, a_r)
  equivalence (tau, tau_r)
  integer :: info

  ! Test 1: 3x2 matrix (tall, M > N)
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (2.0d0, 0.0d0); a(3) = (3.0d0, 0.0d0)
  a(4) = (4.0d0, 1.0d0); a(5) = (5.0d0, 1.0d0); a(6) = (6.0d0, 1.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeql2(3, 2, a, 3, tau, work, info)
  call begin_test('basic_3x2')
  call print_int('info', info)
  call print_array('a', a_r, 12)
  call print_array('tau', tau_r, 4)
  call end_test()

  ! Test 2: 2x2 square matrix
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0); a(2) = (0.0d0, 1.0d0)
  a(3) = (1.0d0, 0.0d0); a(4) = (1.0d0, 1.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeql2(2, 2, a, 2, tau, work, info)
  call begin_test('square_2x2')
  call print_int('info', info)
  call print_array('a', a_r, 8)
  call print_array('tau', tau_r, 4)
  call end_test()

  ! Test 3: M=0
  call zgeql2(0, 2, a, 1, tau, work, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0
  call zgeql2(3, 0, a, 3, tau, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: 1x1
  a(1) = (5.0d0, 3.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeql2(1, 1, a, 1, tau, work, info)
  call begin_test('one_by_one')
  call print_int('info', info)
  call print_array('a', a_r, 2)
  call print_array('tau', tau_r, 2)
  call end_test()

  ! Test 6: 4x3 matrix (tall, M > N)
  a = (0.0d0, 0.0d0)
  a(1)  = (2.0d0, 1.0d0); a(2)  = (1.0d0, 0.0d0)
  a(3)  = (3.0d0, 2.0d0); a(4)  = (1.0d0, 1.0d0)
  a(5)  = (1.0d0, 0.0d0); a(6)  = (4.0d0, 1.0d0)
  a(7)  = (2.0d0, 1.0d0); a(8)  = (3.0d0, 0.0d0)
  a(9)  = (3.0d0, 1.0d0); a(10) = (2.0d0, 0.0d0)
  a(11) = (5.0d0, 2.0d0); a(12) = (1.0d0, 1.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeql2(4, 3, a, 4, tau, work, info)
  call begin_test('rect_4x3')
  call print_int('info', info)
  call print_array('a', a_r, 24)
  call print_array('tau', tau_r, 6)
  call end_test()

  ! Test 7: 2x3 matrix (wide, M < N)
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (4.0d0, 1.0d0)
  a(3) = (2.0d0, 1.0d0); a(4) = (5.0d0, 0.0d0)
  a(5) = (3.0d0, 2.0d0); a(6) = (6.0d0, 1.0d0)
  tau = (0.0d0, 0.0d0)
  call zgeql2(2, 3, a, 2, tau, work, info)
  call begin_test('wide_2x3')
  call print_int('info', info)
  call print_array('a', a_r, 12)
  call print_array('tau', tau_r, 4)
  call end_test()

end program
