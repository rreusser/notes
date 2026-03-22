program test_zgelq2_5x3
  use test_utils
  implicit none

  double precision :: a_r(30), tau_r(6), work_r(200)
  complex*16 :: a(15), tau(3), work(100)
  equivalence (a, a_r)
  equivalence (tau, tau_r)
  integer :: info

  ! 5x3 case: same data as test_zgelqf tall_5x3
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.5d0)
  a(2) = (2.0d0, 1.0d0)
  a(3) = (3.0d0, 1.5d0)
  a(4) = (4.0d0, 2.0d0)
  a(5) = (5.0d0, 2.5d0)
  a(6) = (0.5d0, 1.0d0)
  a(7) = (1.5d0, 2.0d0)
  a(8) = (2.5d0, 3.0d0)
  a(9) = (3.5d0, 4.0d0)
  a(10) = (4.5d0, 5.0d0)
  a(11) = (1.0d0, 0.0d0)
  a(12) = (2.0d0, 1.0d0)
  a(13) = (3.0d0, 2.0d0)
  a(14) = (4.0d0, 3.0d0)
  a(15) = (5.0d0, 4.0d0)
  tau = (0.0d0, 0.0d0)
  call zgelq2(5, 3, a, 5, tau, work, info)
  call begin_test('zgelq2_5x3')
  call print_int('info', info)
  call print_array('a', a_r, 30)
  call print_array('tau', tau_r, 6)
  call end_test()

end program
