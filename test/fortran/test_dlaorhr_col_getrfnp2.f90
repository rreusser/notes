program test_dlaorhr_col_getrfnp2
  use test_utils
  implicit none
  double precision :: a(100), d(20)
  integer :: info

  ! Test 1: 3x3 matrix
  a = 0.0d0
  d = 0.0d0
  a(1) = 0.5d0; a(2) = 0.3d0; a(3) = -0.2d0
  a(4) = -0.4d0; a(5) = 0.6d0; a(6) = 0.1d0
  a(7) = 0.2d0; a(8) = -0.1d0; a(9) = 0.7d0
  call dlaorhr_col_getrfnp2(3, 3, a, 3, d, info)
  call begin_test('3x3')
  call print_array('a', a, 9)
  call print_array('d', d, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x3 tall (M > N)
  a = 0.0d0
  d = 0.0d0
  a(1) = 0.7d0; a(2) = -0.3d0; a(3) = 0.2d0; a(4) = -0.1d0
  a(5) = 0.1d0; a(6) = 0.5d0; a(7) = -0.4d0; a(8) = 0.3d0
  a(9) = -0.2d0; a(10) = 0.4d0; a(11) = 0.6d0; a(12) = -0.5d0
  call dlaorhr_col_getrfnp2(4, 3, a, 4, d, info)
  call begin_test('4x3')
  call print_array('a', a, 12)
  call print_array('d', d, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x4 wide (M < N)
  a = 0.0d0
  d = 0.0d0
  a(1) = 0.8d0; a(2) = 0.1d0; a(3) = -0.2d0
  a(4) = -0.3d0; a(5) = 0.7d0; a(6) = 0.2d0
  a(7) = 0.4d0; a(8) = -0.5d0; a(9) = 0.6d0
  a(10) = 0.1d0; a(11) = 0.3d0; a(12) = -0.4d0
  call dlaorhr_col_getrfnp2(3, 4, a, 3, d, info)
  call begin_test('3x4')
  call print_array('a', a, 12)
  call print_array('d', d, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 1x1
  a = 0.0d0; d = 0.0d0
  a(1) = 0.6d0
  call dlaorhr_col_getrfnp2(1, 1, a, 1, d, info)
  call begin_test('1x1')
  call print_array('a', a, 1)
  call print_array('d', d, 1)
  call print_int('info', info)
  call end_test()

  ! Test 5: 1x1 negative
  a = 0.0d0; d = 0.0d0
  a(1) = -0.4d0
  call dlaorhr_col_getrfnp2(1, 1, a, 1, d, info)
  call begin_test('1x1_neg')
  call print_array('a', a, 1)
  call print_array('d', d, 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: column vector (Nx1)
  a = 0.0d0; d = 0.0d0
  a(1) = 0.7d0; a(2) = -0.3d0; a(3) = 0.5d0; a(4) = -0.2d0
  call dlaorhr_col_getrfnp2(4, 1, a, 4, d, info)
  call begin_test('col_vec')
  call print_array('a', a, 4)
  call print_array('d', d, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: row vector (1xN)
  a = 0.0d0; d = 0.0d0
  a(1) = 0.4d0; a(2) = -0.6d0; a(3) = 0.3d0
  call dlaorhr_col_getrfnp2(1, 3, a, 1, d, info)
  call begin_test('row_vec')
  call print_array('a', a, 3)
  call print_array('d', d, 1)
  call print_int('info', info)
  call end_test()

  ! Test 8: M=0 quick return
  call dlaorhr_col_getrfnp2(0, 3, a, 1, d, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: N=0 quick return
  call dlaorhr_col_getrfnp2(3, 0, a, 3, d, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 10: 5x5 (exercises recursion deeper, n1=2)
  a = 0.0d0; d = 0.0d0
  a(1)  =  0.7d0; a(2)  = -0.2d0; a(3)  =  0.1d0; a(4)  = -0.3d0; a(5)  =  0.4d0
  a(6)  = -0.1d0; a(7)  =  0.6d0; a(8)  =  0.2d0; a(9)  = -0.4d0; a(10) =  0.3d0
  a(11) =  0.3d0; a(12) = -0.2d0; a(13) =  0.8d0; a(14) =  0.1d0; a(15) = -0.5d0
  a(16) = -0.4d0; a(17) =  0.5d0; a(18) = -0.1d0; a(19) =  0.7d0; a(20) =  0.2d0
  a(21) =  0.2d0; a(22) = -0.3d0; a(23) =  0.4d0; a(24) = -0.6d0; a(25) =  0.9d0
  call dlaorhr_col_getrfnp2(5, 5, a, 5, d, info)
  call begin_test('5x5')
  call print_array('a', a, 25)
  call print_array('d', d, 5)
  call print_int('info', info)
  call end_test()

end program
