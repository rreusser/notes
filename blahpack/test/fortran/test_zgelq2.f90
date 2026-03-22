program test_zgelq2
  use test_utils
  implicit none

  double precision :: a_r(100), tau_r(10), work_r(20)
  complex*16 :: a(50), tau(5), work(10)
  equivalence (a, a_r)
  equivalence (tau, tau_r)
  integer :: info

  ! Test 1: 2x3 matrix (M < N, typical LQ case)
  ! A = [ (1,0)  (2,1)  (3,0) ]
  !     [ (4,1)  (5,0)  (6,-1)]
  ! Column-major storage: a(1)=A(1,1), a(2)=A(2,1), a(3)=A(1,2), a(4)=A(2,2), ...
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (4.0d0, 1.0d0)
  a(3) = (2.0d0, 1.0d0); a(4) = (5.0d0, 0.0d0)
  a(5) = (3.0d0, 0.0d0); a(6) = (6.0d0, -1.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelq2(2, 3, a, 2, tau, work, info)
  call begin_test('basic_2x3')
  call print_int('info', info)
  call print_array('a', a_r, 12)
  call print_array('tau', tau_r, 4)
  call end_test()

  ! Test 2: 3x4 matrix (M < N)
  ! A = [ (1,1)  (2,0)  (3,-1) (4,0)  ]
  !     [ (5,0)  (6,1)  (7,0)  (8,-1) ]
  !     [ (9,1)  (10,0) (11,1) (12,0) ]
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0);  a(2) = (5.0d0, 0.0d0);  a(3) = (9.0d0, 1.0d0)
  a(4) = (2.0d0, 0.0d0);  a(5) = (6.0d0, 1.0d0);  a(6) = (10.0d0, 0.0d0)
  a(7) = (3.0d0, -1.0d0); a(8) = (7.0d0, 0.0d0);  a(9) = (11.0d0, 1.0d0)
  a(10) = (4.0d0, 0.0d0); a(11) = (8.0d0, -1.0d0); a(12) = (12.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelq2(3, 4, a, 3, tau, work, info)
  call begin_test('basic_3x4')
  call print_int('info', info)
  call print_array('a', a_r, 24)
  call print_array('tau', tau_r, 6)
  call end_test()

  ! Test 3: M=0 (quick return)
  call zgelq2(0, 3, a, 1, tau, work, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 (quick return)
  call zgelq2(2, 0, a, 2, tau, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: 1x1 matrix
  a(1) = (5.0d0, 3.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelq2(1, 1, a, 1, tau, work, info)
  call begin_test('one_by_one')
  call print_int('info', info)
  call print_array('a', a_r, 2)
  call print_array('tau', tau_r, 2)
  call end_test()

  ! Test 6: 2x2 square matrix
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0); a(2) = (0.0d0, 1.0d0)
  a(3) = (1.0d0, 0.0d0); a(4) = (1.0d0, 1.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelq2(2, 2, a, 2, tau, work, info)
  call begin_test('square_2x2')
  call print_int('info', info)
  call print_array('a', a_r, 8)
  call print_array('tau', tau_r, 4)
  call end_test()

  ! Test 7: 3x3 square matrix with complex entries
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0);  a(2) = (1.0d0, -1.0d0); a(3) = (0.0d0, 0.5d0)
  a(4) = (1.0d0, 0.0d0);  a(5) = (3.0d0, 2.0d0);  a(6) = (1.0d0, -1.0d0)
  a(7) = (-1.0d0, 1.0d0); a(8) = (0.0d0, 1.0d0);  a(9) = (4.0d0, 0.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelq2(3, 3, a, 3, tau, work, info)
  call begin_test('square_3x3')
  call print_int('info', info)
  call print_array('a', a_r, 18)
  call print_array('tau', tau_r, 6)
  call end_test()

end program
