program test_dlatrs
  use test_utils
  implicit none
  double precision :: a(100), x(10), cnorm(10), scale
  integer :: info, n

  ! Test 1: Upper triangular, no transpose, non-unit diagonal, 3x3
  ! A = [[2, 1, 1], [0, 3, 2], [0, 0, 4]] (column-major)
  ! b = [1, 2, 3]
  n = 3
  a = 0.0d0
  a(1) = 2.0d0; a(4) = 1.0d0; a(7) = 1.0d0
  a(5) = 3.0d0; a(8) = 2.0d0
  a(9) = 4.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatrs('U', 'N', 'N', 'N', n, a, n, x, scale, cnorm, info)
  call begin_test('upper_N_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: Lower triangular, no transpose, non-unit diagonal, 3x3
  ! A = [[2, 0, 0], [1, 3, 0], [1, 2, 4]] (column-major)
  ! b = [1, 2, 3]
  a = 0.0d0
  a(1) = 2.0d0
  a(2) = 1.0d0; a(5) = 3.0d0
  a(3) = 1.0d0; a(6) = 2.0d0; a(9) = 4.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatrs('L', 'N', 'N', 'N', n, a, n, x, scale, cnorm, info)
  call begin_test('lower_N_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: Upper triangular, transpose, non-unit diagonal, 3x3
  a = 0.0d0
  a(1) = 2.0d0; a(4) = 1.0d0; a(7) = 1.0d0
  a(5) = 3.0d0; a(8) = 2.0d0
  a(9) = 4.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatrs('U', 'T', 'N', 'N', n, a, n, x, scale, cnorm, info)
  call begin_test('upper_T_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 4: Lower triangular, transpose, non-unit diagonal, 3x3
  a = 0.0d0
  a(1) = 2.0d0
  a(2) = 1.0d0; a(5) = 3.0d0
  a(3) = 1.0d0; a(6) = 2.0d0; a(9) = 4.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatrs('L', 'T', 'N', 'N', n, a, n, x, scale, cnorm, info)
  call begin_test('lower_T_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 5: Upper triangular, unit diagonal
  a = 0.0d0
  a(1) = 99.0d0; a(4) = 1.0d0; a(7) = 1.0d0
  a(5) = 99.0d0; a(8) = 2.0d0
  a(9) = 99.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatrs('U', 'N', 'U', 'N', n, a, n, x, scale, cnorm, info)
  call begin_test('upper_N_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0
  call dlatrs('U', 'N', 'N', 'N', 0, a, 1, x, scale, cnorm, info)
  call begin_test('n_zero')
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1
  a(1) = 5.0d0
  x(1) = 10.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatrs('U', 'N', 'N', 'N', 1, a, 1, x, scale, cnorm, info)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 upper, non-transpose, with normin='Y' (pre-computed norms)
  n = 4
  a = 0.0d0
  a(1) = 3.0d0; a(5) = 1.0d0; a(9) = 2.0d0; a(13) = 1.0d0
  a(6) = 4.0d0; a(10) = 1.0d0; a(14) = 2.0d0
  a(11) = 2.0d0; a(15) = 1.0d0
  a(16) = 5.0d0
  x(1) = 1.0d0; x(2) = 1.0d0; x(3) = 1.0d0; x(4) = 1.0d0
  ! Pre-compute norms manually
  cnorm(1) = 0.0d0
  cnorm(2) = 1.0d0
  cnorm(3) = 3.0d0
  cnorm(4) = 4.0d0
  scale = 0.0d0
  call dlatrs('U', 'N', 'N', 'Y', n, a, n, x, scale, cnorm, info)
  call begin_test('upper_N_normin_Y')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 9: Lower triangular, transpose, unit diagonal
  n = 3
  a = 0.0d0
  a(1) = 99.0d0
  a(2) = 1.0d0; a(5) = 99.0d0
  a(3) = 2.0d0; a(6) = 3.0d0; a(9) = 99.0d0
  x(1) = 6.0d0; x(2) = 5.0d0; x(3) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatrs('L', 'T', 'U', 'N', n, a, n, x, scale, cnorm, info)
  call begin_test('lower_T_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 10: Identity matrix (upper)
  n = 3
  a = 0.0d0
  a(1) = 1.0d0
  a(5) = 1.0d0
  a(9) = 1.0d0
  x(1) = 7.0d0; x(2) = 8.0d0; x(3) = 9.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatrs('U', 'N', 'N', 'N', n, a, n, x, scale, cnorm, info)
  call begin_test('identity')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

end program
