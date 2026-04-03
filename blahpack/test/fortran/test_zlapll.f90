program test_zlapll
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  double precision :: zx_r(2*NMAX), zy_r(2*NMAX)
  complex*16 :: zx(NMAX), zy(NMAX)
  equivalence (zx, zx_r)
  equivalence (zy, zy_r)
  double precision :: SSMIN
  integer :: i

  ! Test 1: parallel complex vectors (linearly dependent)
  ! Y = 2*X, so ssmin should be ~ 0
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (5.0d0, 6.0d0)
  zx(4) = (7.0d0, 8.0d0)
  zy(1) = (2.0d0, 4.0d0)
  zy(2) = (6.0d0, 8.0d0)
  zy(3) = (10.0d0, 12.0d0)
  zy(4) = (14.0d0, 16.0d0)
  call ZLAPLL(4, zx, 1, zy, 1, SSMIN)
  call begin_test('parallel')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 2: orthogonal complex vectors
  zx(1) = (1.0d0, 0.0d0)
  zx(2) = (0.0d0, 0.0d0)
  zx(3) = (0.0d0, 0.0d0)
  zy(1) = (0.0d0, 0.0d0)
  zy(2) = (1.0d0, 0.0d0)
  zy(3) = (0.0d0, 0.0d0)
  call ZLAPLL(3, zx, 1, zy, 1, SSMIN)
  call begin_test('orthogonal')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 3: general complex vectors
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (3.0d0, -1.0d0)
  zx(3) = (0.5d0, 4.0d0)
  zy(1) = (2.0d0, -3.0d0)
  zy(2) = (-1.0d0, 5.0d0)
  zy(3) = (4.0d0, 0.5d0)
  call ZLAPLL(3, zx, 1, zy, 1, SSMIN)
  call begin_test('general')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 4: N=1 quick return (ssmin = 0)
  zx(1) = (5.0d0, 3.0d0)
  zy(1) = (2.0d0, 7.0d0)
  call ZLAPLL(1, zx, 1, zy, 1, SSMIN)
  call begin_test('n_equals_1')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 5: N=2
  zx(1) = (3.0d0, 1.0d0)
  zx(2) = (4.0d0, -2.0d0)
  zy(1) = (1.0d0, 0.5d0)
  zy(2) = (2.0d0, 3.0d0)
  call ZLAPLL(2, zx, 1, zy, 1, SSMIN)
  call begin_test('n_equals_2')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 6: nearly parallel complex vectors
  zx(1) = (1.0d0, 1.0d0)
  zx(2) = (2.0d0, 2.0d0)
  zx(3) = (3.0d0, 3.0d0)
  zx(4) = (4.0d0, 4.0d0)
  zx(5) = (5.0d0, 5.0d0)
  zy(1) = (1.0d0, 1.0d0)
  zy(2) = (2.0d0, 2.0d0)
  zy(3) = (3.0d0, 3.0d0)
  zy(4) = (4.0d0, 4.0d0)
  zy(5) = (5.0d0, 5.001d0)
  call ZLAPLL(5, zx, 1, zy, 1, SSMIN)
  call begin_test('nearly_parallel')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 7: purely imaginary vectors
  zx(1) = (0.0d0, 1.0d0)
  zx(2) = (0.0d0, 2.0d0)
  zx(3) = (0.0d0, 3.0d0)
  zy(1) = (0.0d0, 4.0d0)
  zy(2) = (0.0d0, 5.0d0)
  zy(3) = (0.0d0, 6.0d0)
  call ZLAPLL(3, zx, 1, zy, 1, SSMIN)
  call begin_test('imaginary')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 8: identical complex vectors (ssmin ~ 0)
  zx(1) = (1.0d0, 1.0d0)
  zx(2) = (2.0d0, 2.0d0)
  zx(3) = (3.0d0, 3.0d0)
  zx(4) = (4.0d0, 4.0d0)
  zy(1) = (1.0d0, 1.0d0)
  zy(2) = (2.0d0, 2.0d0)
  zy(3) = (3.0d0, 3.0d0)
  zy(4) = (4.0d0, 4.0d0)
  call ZLAPLL(4, zx, 1, zy, 1, SSMIN)
  call begin_test('identical')
  call print_scalar('ssmin', SSMIN)
  call end_test()

  ! Test 9: large N with sin/cos
  ! Use transcendental values to avoid ties
  do i = 1, 10
    zx(i) = dcmplx(sin(dble(i)), cos(dble(i)))
    zy(i) = dcmplx(cos(dble(i) * 0.7d0), sin(dble(i) * 1.3d0))
  end do
  call ZLAPLL(10, zx, 1, zy, 1, SSMIN)
  call begin_test('large_n')
  call print_scalar('ssmin', SSMIN)
  call end_test()

end program
