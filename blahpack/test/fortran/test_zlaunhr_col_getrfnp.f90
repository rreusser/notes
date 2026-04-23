program test_zlaunhr_col_getrfnp
  use test_utils
  implicit none
  double precision :: a_r(8000)
  complex*16 :: a(4000)
  equivalence (a, a_r)
  double precision :: d_r(160)
  complex*16 :: d(80)
  equivalence (d, d_r)
  integer :: info
  integer :: i

  ! Test 1: 3x3 small (unblocked path)
  a = (0.0d0, 0.0d0)
  d = (0.0d0, 0.0d0)
  a(1) = (0.5d0, 0.3d0); a(2) = (0.2d0, -0.1d0); a(3) = (-0.3d0, 0.2d0)
  a(4) = (-0.1d0, 0.4d0); a(5) = (0.6d0, -0.2d0); a(6) = (0.1d0, 0.3d0)
  a(7) = (0.2d0, -0.3d0); a(8) = (-0.4d0, 0.1d0); a(9) = (0.5d0, 0.2d0)
  call zlaunhr_col_getrfnp(3, 3, a, 3, d, info)
  call begin_test('3x3')
  call print_array('a', a_r, 18)
  call print_array('d', d_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x3 (M>N)
  a = (0.0d0, 0.0d0)
  d = (0.0d0, 0.0d0)
  a(1) = (0.3d0, 0.2d0); a(2) = (0.1d0, -0.4d0); a(3) = (-0.2d0, 0.3d0); a(4) = (0.4d0, 0.1d0)
  a(5) = (-0.1d0, 0.5d0); a(6) = (0.5d0, -0.2d0); a(7) = (0.2d0, 0.3d0); a(8) = (-0.3d0, 0.1d0)
  a(9) = (0.2d0, -0.2d0); a(10) = (-0.4d0, 0.2d0); a(11) = (0.6d0, 0.1d0); a(12) = (0.1d0, -0.3d0)
  call zlaunhr_col_getrfnp(4, 3, a, 4, d, info)
  call begin_test('4x3')
  call print_array('a', a_r, 24)
  call print_array('d', d_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x4 (M<N)
  a = (0.0d0, 0.0d0)
  d = (0.0d0, 0.0d0)
  a(1) = (0.4d0, 0.2d0); a(2) = (0.1d0, -0.3d0); a(3) = (-0.2d0, 0.4d0)
  a(4) = (-0.1d0, 0.3d0); a(5) = (0.5d0, -0.2d0); a(6) = (0.2d0, 0.1d0)
  a(7) = (0.2d0, -0.1d0); a(8) = (-0.3d0, 0.2d0); a(9) = (0.6d0, 0.3d0)
  a(10) = (0.1d0, 0.4d0); a(11) = (-0.2d0, -0.1d0); a(12) = (0.3d0, 0.2d0)
  call zlaunhr_col_getrfnp(3, 4, a, 3, d, info)
  call begin_test('3x4')
  call print_array('a', a_r, 24)
  call print_array('d', d_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 4: 1x1
  a(1) = (0.7d0, 0.3d0)
  d(1) = (0.0d0, 0.0d0)
  call zlaunhr_col_getrfnp(1, 1, a, 1, d, info)
  call begin_test('1x1')
  call print_array('a', a_r, 2)
  call print_array('d', d_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 5: 5x5
  a = (0.0d0, 0.0d0)
  d = (0.0d0, 0.0d0)
  a(1)  = (0.50d0, 0.30d0); a(2)  = (0.20d0,-0.10d0); a(3)  = (-0.30d0, 0.20d0); a(4)  = (0.10d0, 0.40d0); a(5)  = (-0.20d0,-0.30d0)
  a(6)  = (-0.10d0, 0.40d0); a(7)  = (0.60d0,-0.20d0); a(8)  = (0.10d0, 0.30d0); a(9)  = (-0.20d0,-0.40d0); a(10) = (0.30d0, 0.10d0)
  a(11) = (0.20d0,-0.30d0); a(12) = (-0.40d0, 0.10d0); a(13) = (0.50d0, 0.20d0); a(14) = (0.30d0,-0.10d0); a(15) = (-0.10d0, 0.40d0)
  a(16) = (-0.30d0, 0.10d0); a(17) = (0.20d0, 0.30d0); a(18) = (-0.10d0,-0.40d0); a(19) = (0.60d0, 0.20d0); a(20) = (0.40d0,-0.10d0)
  a(21) = (0.40d0,-0.20d0); a(22) = (0.10d0, 0.30d0); a(23) = (-0.20d0, 0.40d0); a(24) = (-0.30d0,-0.10d0); a(25) = (0.50d0, 0.30d0)
  call zlaunhr_col_getrfnp(5, 5, a, 5, d, info)
  call begin_test('5x5')
  call print_array('a', a_r, 50)
  call print_array('d', d_r, 10)
  call print_int('info', info)
  call end_test()

  ! Test 6: 8x8 (still small but exercises typical block size in JS impl)
  a = (0.0d0, 0.0d0)
  d = (0.0d0, 0.0d0)
  do i = 1, 64
    a(i) = cmplx( 0.05d0*mod(i,7) - 0.15d0, 0.04d0*mod(i,5) - 0.1d0, kind=8 )
  end do
  ! Boost diagonal so |Re| >= sum of off-diagonal magnitudes, ensuring well-conditioned
  do i = 1, 8
    a((i-1)*8 + i) = cmplx( 1.5d0 + 0.05d0*i, 0.0d0, kind=8 )
  end do
  call zlaunhr_col_getrfnp(8, 8, a, 8, d, info)
  call begin_test('8x8')
  call print_array('a', a_r, 128)
  call print_array('d', d_r, 16)
  call print_int('info', info)
  call end_test()

  ! Test 7: 40x40 (exercises blocked path with NB=32 in JS impl)
  a = (0.0d0, 0.0d0)
  d = (0.0d0, 0.0d0)
  do i = 1, 1600
    a(i) = cmplx( 0.03d0*mod(i,9) - 0.12d0, 0.025d0*mod(i,11) - 0.125d0, kind=8 )
  end do
  ! Boost diagonal so factorization is well-conditioned
  do i = 1, 40
    a((i-1)*40 + i) = cmplx( 2.0d0 + 0.01d0*i, 0.01d0*i, kind=8 )
  end do
  call zlaunhr_col_getrfnp(40, 40, a, 40, d, info)
  call begin_test('40x40')
  call print_array('a', a_r, 3200)
  call print_array('d', d_r, 80)
  call print_int('info', info)
  call end_test()
end program
