program test_dlapll
  use test_utils
  implicit none
  double precision :: X(10), Y(10), SSMIN

  ! Test 1: parallel vectors (linearly dependent)
  X(1) = 1.0d0; X(2) = 2.0d0; X(3) = 3.0d0; X(4) = 4.0d0
  Y(1) = 2.0d0; Y(2) = 4.0d0; Y(3) = 6.0d0; Y(4) = 8.0d0
  call DLAPLL(4, X, 1, Y, 1, SSMIN)
  call begin_test('parallel')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 2: orthogonal vectors
  X(1) = 1.0d0; X(2) = 0.0d0; X(3) = 0.0d0
  Y(1) = 0.0d0; Y(2) = 1.0d0; Y(3) = 0.0d0
  call DLAPLL(3, X, 1, Y, 1, SSMIN)
  call begin_test('orthogonal')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 3: general vectors
  X(1) = 1.0d0; X(2) = 2.0d0; X(3) = 3.0d0
  Y(1) = 4.0d0; Y(2) = 5.0d0; Y(3) = 6.0d0
  call DLAPLL(3, X, 1, Y, 1, SSMIN)
  call begin_test('general')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 4: N=1 (quick return)
  X(1) = 5.0d0
  Y(1) = 3.0d0
  call DLAPLL(1, X, 1, Y, 1, SSMIN)
  call begin_test('n_equals_1')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 5: N=2
  X(1) = 3.0d0; X(2) = 4.0d0
  Y(1) = 1.0d0; Y(2) = 2.0d0
  call DLAPLL(2, X, 1, Y, 1, SSMIN)
  call begin_test('n_equals_2')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 6: Nearly parallel vectors
  X(1) = 1.0d0; X(2) = 2.0d0; X(3) = 3.0d0; X(4) = 4.0d0; X(5) = 5.0d0
  Y(1) = 1.0d0; Y(2) = 2.0d0; Y(3) = 3.0d0; Y(4) = 4.0d0; Y(5) = 5.001d0
  call DLAPLL(5, X, 1, Y, 1, SSMIN)
  call begin_test('nearly_parallel')
  call print_scalar('ssmin', SSMIN)
  call end_test()

end program
