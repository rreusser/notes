program test_dlaorhr_col_getrfnp
  use test_utils
  implicit none
  double precision :: a(400), d(20)
  integer :: info, i, j

  ! Test 1: 3x3 (small, unblocked path likely)
  a = 0.0d0; d = 0.0d0
  a(1) = 0.5d0; a(2) = 0.3d0; a(3) = -0.2d0
  a(4) = -0.4d0; a(5) = 0.6d0; a(6) = 0.1d0
  a(7) = 0.2d0; a(8) = -0.1d0; a(9) = 0.7d0
  call dlaorhr_col_getrfnp(3, 3, a, 3, d, info)
  call begin_test('3x3')
  call print_array('a', a, 9)
  call print_array('d', d, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x3
  a = 0.0d0; d = 0.0d0
  a(1) = 0.7d0; a(2) = -0.3d0; a(3) = 0.2d0; a(4) = -0.1d0
  a(5) = 0.1d0; a(6) = 0.5d0; a(7) = -0.4d0; a(8) = 0.3d0
  a(9) = -0.2d0; a(10) = 0.4d0; a(11) = 0.6d0; a(12) = -0.5d0
  call dlaorhr_col_getrfnp(4, 3, a, 4, d, info)
  call begin_test('4x3')
  call print_array('a', a, 12)
  call print_array('d', d, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 1x1
  a = 0.0d0; d = 0.0d0
  a(1) = 0.6d0
  call dlaorhr_col_getrfnp(1, 1, a, 1, d, info)
  call begin_test('1x1')
  call print_array('a', a, 1)
  call print_array('d', d, 1)
  call print_int('info', info)
  call end_test()

  ! Test 4: 5x5 (small recursive)
  a = 0.0d0; d = 0.0d0
  a(1)  =  0.7d0; a(2)  = -0.2d0; a(3)  =  0.1d0; a(4)  = -0.3d0; a(5)  =  0.4d0
  a(6)  = -0.1d0; a(7)  =  0.6d0; a(8)  =  0.2d0; a(9)  = -0.4d0; a(10) =  0.3d0
  a(11) =  0.3d0; a(12) = -0.2d0; a(13) =  0.8d0; a(14) =  0.1d0; a(15) = -0.5d0
  a(16) = -0.4d0; a(17) =  0.5d0; a(18) = -0.1d0; a(19) =  0.7d0; a(20) =  0.2d0
  a(21) =  0.2d0; a(22) = -0.3d0; a(23) =  0.4d0; a(24) = -0.6d0; a(25) =  0.9d0
  call dlaorhr_col_getrfnp(5, 5, a, 5, d, info)
  call begin_test('5x5')
  call print_array('a', a, 25)
  call print_array('d', d, 5)
  call print_int('info', info)
  call end_test()

  ! Test 5: 20x20 (definitely exercises blocked path: NB defaults to 32, so blocked branch only if NB<min(M,N))
  ! With NB=32 default, 20x20 will go unblocked. Use 40x40 to force blocked path.
  a = 0.0d0; d = 0.0d0
  do j = 1, 20
    do i = 1, 20
      a((j-1)*20 + i) = sin(real(i + 3*j, 8)) * 0.5d0
      if (i == j) a((j-1)*20 + i) = a((j-1)*20 + i) + 1.5d0
    end do
  end do
  call dlaorhr_col_getrfnp(20, 20, a, 20, d, info)
  call begin_test('20x20')
  call print_array('a', a, 400)
  call print_array('d', d, 20)
  call print_int('info', info)
  call end_test()

  ! Test 6: M=0
  call dlaorhr_col_getrfnp(0, 3, a, 1, d, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0
  call dlaorhr_col_getrfnp(3, 0, a, 3, d, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: 3x4 (M < N)
  a = 0.0d0; d = 0.0d0
  a(1) = 0.8d0; a(2) = 0.1d0; a(3) = -0.2d0
  a(4) = -0.3d0; a(5) = 0.7d0; a(6) = 0.2d0
  a(7) = 0.4d0; a(8) = -0.5d0; a(9) = 0.6d0
  a(10) = 0.1d0; a(11) = 0.3d0; a(12) = -0.4d0
  call dlaorhr_col_getrfnp(3, 4, a, 3, d, info)
  call begin_test('3x4')
  call print_array('a', a, 12)
  call print_array('d', d, 4)
  call print_int('info', info)
  call end_test()

end program
