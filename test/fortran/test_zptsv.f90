program test_zptsv
  use test_utils
  implicit none
  double precision :: d(10)
  complex*16 :: e(10), b(10, 4)
  double precision :: e_real(20), b_real(20)
  equivalence (e, e_real)
  integer :: info, i

  ! Test 1: basic 5x5, single RHS
  ! Hermitian positive definite tridiagonal with complex off-diagonals
  d(1:5) = (/4.0d0, 4.0d0, 4.0d0, 4.0d0, 4.0d0/)
  e(1) = (1.0d0, 1.0d0)
  e(2) = (1.0d0, 1.0d0)
  e(3) = (1.0d0, 1.0d0)
  e(4) = (1.0d0, 1.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (2.0d0, 1.0d0)
  b(3,1) = (3.0d0, -1.0d0)
  b(4,1) = (4.0d0, 2.0d0)
  b(5,1) = (5.0d0, 0.0d0)
  call zptsv(5, 1, d, e, b, 10, info)
  call begin_test('basic_5x5_single_rhs')
  call print_array('d', d, 5)
  call print_array('e', e_real, 8)
  do i = 1, 5
    b_real(2*i-1) = dble(b(i,1))
    b_real(2*i) = dimag(b(i,1))
  end do
  call print_array('b', b_real, 10)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 with 2 RHS
  d(1:4) = (/3.0d0, 3.0d0, 3.0d0, 3.0d0/)
  e(1) = (0.5d0, 0.5d0)
  e(2) = (0.5d0, -0.5d0)
  e(3) = (0.5d0, 0.5d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(3,1) = (1.0d0, 1.0d0)
  b(4,1) = (0.0d0, 0.0d0)
  b(1,2) = (2.0d0, 1.0d0)
  b(2,2) = (1.0d0, -1.0d0)
  b(3,2) = (0.0d0, 2.0d0)
  b(4,2) = (3.0d0, 0.0d0)
  call zptsv(4, 2, d, e, b, 10, info)
  call begin_test('multi_rhs')
  call print_array('d', d, 4)
  call print_array('e', e_real, 6)
  do i = 1, 4
    b_real(2*i-1) = dble(b(i,1))
    b_real(2*i) = dimag(b(i,1))
  end do
  call print_array('b1', b_real, 8)
  do i = 1, 4
    b_real(2*i-1) = dble(b(i,2))
    b_real(2*i) = dimag(b(i,2))
  end do
  call print_array('b2', b_real, 8)
  call print_int('info', info)
  call end_test()

  ! Test 3: n=1
  d(1) = 5.0d0
  b(1,1) = (10.0d0, -5.0d0)
  call zptsv(1, 1, d, e, b, 10, info)
  call begin_test('n_one')
  call print_array('d', d, 1)
  b_real(1) = dble(b(1,1))
  b_real(2) = dimag(b(1,1))
  call print_array('b', b_real, 2)
  call print_int('info', info)
  call end_test()

  ! Test 4: n=0 (quick return)
  call zptsv(0, 1, d, e, b, 10, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: non-positive-definite (d(1) < 0)
  d(1:3) = (/-1.0d0, 4.0d0, 4.0d0/)
  e(1) = (1.0d0, 0.0d0)
  e(2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (2.0d0, 0.0d0)
  b(3,1) = (3.0d0, 0.0d0)
  call zptsv(3, 1, d, e, b, 10, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

end program
