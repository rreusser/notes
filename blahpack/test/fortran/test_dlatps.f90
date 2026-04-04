program test_dlatps
  use test_utils
  implicit none
  double precision :: ap(100), x(10), cnorm(10), scale
  integer :: info, n

  ! Test 1: Upper triangular, no transpose, non-unit diagonal, 3x3
  ! A = [[2, 1, 1], [0, 3, 2], [0, 0, 4]] packed upper col-major: [2, 1, 3, 1, 2, 4]
  ! b = [1, 2, 3]
  n = 3
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 1.0d0; ap(5) = 2.0d0; ap(6) = 4.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatps('U', 'N', 'N', 'N', n, ap, x, scale, cnorm, info)
  call begin_test('upper_N_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: Lower triangular, no transpose, non-unit diagonal, 3x3
  ! A = [[2, 0, 0], [1, 3, 0], [1, 2, 4]] packed lower col-major: [2, 1, 1, 3, 2, 4]
  ! b = [1, 2, 3]
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 1.0d0
  ap(4) = 3.0d0; ap(5) = 2.0d0
  ap(6) = 4.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatps('L', 'N', 'N', 'N', n, ap, x, scale, cnorm, info)
  call begin_test('lower_N_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: Upper triangular, transpose, non-unit diagonal, 3x3
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 3.0d0
  ap(4) = 1.0d0; ap(5) = 2.0d0; ap(6) = 4.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatps('U', 'T', 'N', 'N', n, ap, x, scale, cnorm, info)
  call begin_test('upper_T_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 4: Lower triangular, transpose, non-unit diagonal, 3x3
  ap = 0.0d0
  ap(1) = 2.0d0; ap(2) = 1.0d0; ap(3) = 1.0d0
  ap(4) = 3.0d0; ap(5) = 2.0d0
  ap(6) = 4.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatps('L', 'T', 'N', 'N', n, ap, x, scale, cnorm, info)
  call begin_test('lower_T_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 5: Upper triangular, unit diagonal
  ap = 0.0d0
  ap(1) = 99.0d0; ap(2) = 1.0d0; ap(3) = 99.0d0
  ap(4) = 1.0d0; ap(5) = 2.0d0; ap(6) = 99.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatps('U', 'N', 'U', 'N', n, ap, x, scale, cnorm, info)
  call begin_test('upper_N_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0
  x(1) = 99.0d0
  scale = 0.0d0
  call dlatps('U', 'N', 'N', 'N', 0, ap, x, scale, cnorm, info)
  call begin_test('n_zero')
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1
  ap(1) = 5.0d0
  x(1) = 10.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatps('U', 'N', 'N', 'N', 1, ap, x, scale, cnorm, info)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 upper, non-transpose, with normin='Y' (pre-computed norms)
  ! A = [[3, 1, 2, 1], [0, 4, 1, 2], [0, 0, 2, 1], [0, 0, 0, 5]]
  ! packed upper col-major: [3, 1, 4, 2, 1, 2, 1, 2, 1, 5]
  n = 4
  ap = 0.0d0
  ap(1) = 3.0d0; ap(2) = 1.0d0; ap(3) = 4.0d0
  ap(4) = 2.0d0; ap(5) = 1.0d0; ap(6) = 2.0d0
  ap(7) = 1.0d0; ap(8) = 2.0d0; ap(9) = 1.0d0; ap(10) = 5.0d0
  x(1) = 1.0d0; x(2) = 1.0d0; x(3) = 1.0d0; x(4) = 1.0d0
  ! Pre-compute norms manually (1-norm of off-diagonal part of each column)
  cnorm(1) = 0.0d0
  cnorm(2) = 1.0d0
  cnorm(3) = 3.0d0
  cnorm(4) = 4.0d0
  scale = 0.0d0
  call dlatps('U', 'N', 'N', 'Y', n, ap, x, scale, cnorm, info)
  call begin_test('upper_N_normin_Y')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 9: Lower triangular, transpose, unit diagonal
  n = 3
  ap = 0.0d0
  ap(1) = 99.0d0; ap(2) = 1.0d0; ap(3) = 2.0d0
  ap(4) = 99.0d0; ap(5) = 3.0d0
  ap(6) = 99.0d0
  x(1) = 6.0d0; x(2) = 5.0d0; x(3) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatps('L', 'T', 'U', 'N', n, ap, x, scale, cnorm, info)
  call begin_test('lower_T_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 10: Identity matrix (upper, packed: [1, 0, 1, 0, 0, 1])
  n = 3
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 0.0d0; ap(3) = 1.0d0
  ap(4) = 0.0d0; ap(5) = 0.0d0; ap(6) = 1.0d0
  x(1) = 7.0d0; x(2) = 8.0d0; x(3) = 9.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatps('U', 'N', 'N', 'N', n, ap, x, scale, cnorm, info)
  call begin_test('identity')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

end program
