program test_zlanht
  use test_utils
  implicit none
  double precision :: d(10)
  complex*16 :: e(10)
  double precision :: zlanht
  external zlanht
  double precision :: result

  ! Test 1: N=0 (quick return)
  result = zlanht('M', 0, d, e)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: N=1 (diagonal only)
  d(1) = -3.0d0
  result = zlanht('M', 1, d, e)
  call begin_test('max_norm_n1')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('1', 1, d, e)
  call begin_test('one_norm_n1')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('I', 1, d, e)
  call begin_test('inf_norm_n1')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('F', 1, d, e)
  call begin_test('frob_norm_n1')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: N=2, simple
  d(1) = 3.0d0; d(2) = -4.0d0
  e(1) = (1.0d0, 2.0d0)

  result = zlanht('M', 2, d, e)
  call begin_test('max_norm_n2')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('1', 2, d, e)
  call begin_test('one_norm_n2')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('I', 2, d, e)
  call begin_test('inf_norm_n2')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('F', 2, d, e)
  call begin_test('frob_norm_n2')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: N=5, Hermitian tridiagonal
  ! D (real diagonal): [2, -4, 6, -1, 3]
  ! E (complex off-diagonal): [(1+2i), (-2+3i), (3-1i), (5+4i)]
  d(1) = 2.0d0; d(2) = -4.0d0; d(3) = 6.0d0; d(4) = -1.0d0; d(5) = 3.0d0
  e(1) = (1.0d0, 2.0d0); e(2) = (-2.0d0, 3.0d0); e(3) = (3.0d0, -1.0d0)
  e(4) = (5.0d0, 4.0d0)

  result = zlanht('M', 5, d, e)
  call begin_test('max_norm_5x5')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('O', 5, d, e)
  call begin_test('one_norm_O_5x5')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('1', 5, d, e)
  call begin_test('one_norm_1_5x5')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('I', 5, d, e)
  call begin_test('inf_norm_5x5')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('F', 5, d, e)
  call begin_test('frob_norm_5x5')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('E', 5, d, e)
  call begin_test('frob_norm_E_5x5')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: N=4, different values
  d(1) = 10.0d0; d(2) = -20.0d0; d(3) = 30.0d0; d(4) = -40.0d0
  e(1) = (5.0d0, 6.0d0); e(2) = (7.0d0, 8.0d0); e(3) = (9.0d0, 10.0d0)

  result = zlanht('M', 4, d, e)
  call begin_test('max_norm_4x4')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('1', 4, d, e)
  call begin_test('one_norm_4x4')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('I', 4, d, e)
  call begin_test('inf_norm_4x4')
  call print_scalar('result', result)
  call end_test()

  result = zlanht('F', 4, d, e)
  call begin_test('frob_norm_4x4')
  call print_scalar('result', result)
  call end_test()

end program
