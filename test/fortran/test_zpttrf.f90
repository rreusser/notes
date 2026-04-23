program test_zpttrf
  use test_utils
  implicit none
  double precision :: d(10)
  complex*16 :: e(10)
  double precision :: e_real(20)
  equivalence (e, e_real)
  integer :: info

  ! Test 1: basic 5x5 Hermitian positive definite tridiagonal
  ! D = [4, 4, 4, 4, 4], E = [(1+i), (1+i), (1+i), (1+i)]
  ! This is diagonally dominant so positive definite.
  d(1:5) = (/4.0d0, 4.0d0, 4.0d0, 4.0d0, 4.0d0/)
  e(1) = (1.0d0, 1.0d0)
  e(2) = (1.0d0, 1.0d0)
  e(3) = (1.0d0, 1.0d0)
  e(4) = (1.0d0, 1.0d0)
  call zpttrf(5, d, e, info)
  call begin_test('basic_5x5')
  call print_array('d', d, 5)
  call print_array('e', e_real, 8)
  call print_int('info', info)
  call end_test()

  ! Test 2: n=1 (single element)
  d(1) = 3.0d0
  call zpttrf(1, d, e, info)
  call begin_test('n_one')
  call print_array('d', d, 1)
  call print_int('info', info)
  call end_test()

  ! Test 3: n=0 (quick return)
  call zpttrf(0, d, e, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: non-positive-definite (d(1) <= 0)
  d(1:3) = (/-1.0d0, 4.0d0, 4.0d0/)
  e(1) = (1.0d0, 0.0d0)
  e(2) = (1.0d0, 0.0d0)
  call zpttrf(3, d, e, info)
  call begin_test('not_posdef_first')
  call print_array('d', d, 3)
  call print_array('e', e_real, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: non-positive-definite (fails mid-factorization)
  ! d(1) = 1, e(1) = (2+0i) => d(2) = 1 - (2^2 + 0^2)/1 = 1 - 4 = -3
  d(1:3) = (/1.0d0, 1.0d0, 4.0d0/)
  e(1) = (2.0d0, 0.0d0)
  e(2) = (1.0d0, 0.0d0)
  call zpttrf(3, d, e, info)
  call begin_test('not_posdef_mid')
  call print_array('d', d, 3)
  call print_array('e', e_real, 4)
  call print_int('info', info)
  call end_test()

  ! Test 6: larger 8x8 to exercise the 4-unrolled loop
  d(1:8) = (/5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0/)
  e(1) = (1.0d0, 0.5d0)
  e(2) = (0.5d0, 1.0d0)
  e(3) = (1.0d0, -0.5d0)
  e(4) = (-0.5d0, 1.0d0)
  e(5) = (1.0d0, 1.0d0)
  e(6) = (0.5d0, -0.5d0)
  e(7) = (-1.0d0, 0.5d0)
  call zpttrf(8, d, e, info)
  call begin_test('unrolled_8x8')
  call print_array('d', d, 8)
  call print_array('e', e_real, 14)
  call print_int('info', info)
  call end_test()

  ! Test 7: n=2
  d(1:2) = (/4.0d0, 4.0d0/)
  e(1) = (1.0d0, 1.0d0)
  call zpttrf(2, d, e, info)
  call begin_test('n_two')
  call print_array('d', d, 2)
  call print_array('e', e_real, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: pure imaginary off-diagonal
  d(1:3) = (/4.0d0, 4.0d0, 4.0d0/)
  e(1) = (0.0d0, 1.0d0)
  e(2) = (0.0d0, 1.0d0)
  call zpttrf(3, d, e, info)
  call begin_test('pure_imag')
  call print_array('d', d, 3)
  call print_array('e', e_real, 4)
  call print_int('info', info)
  call end_test()

end program
