program test_dznrm2
  use test_utils
  implicit none
  double precision :: dznrm2, result
  complex*16 :: x(10)

  ! Test 1: basic — |(3+4i)| = 5, so norm of single element = 5
  x(1) = (3.0d0, 4.0d0)
  result = dznrm2(1, x, 1)
  call begin_test('single')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: two elements — |(1+0i), (0+1i)| = sqrt(2)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  result = dznrm2(2, x, 1)
  call begin_test('two_elements')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: n=0 → 0
  result = dznrm2(0, x, 1)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: three elements
  x(1) = (1.0d0, 2.0d0)
  x(2) = (3.0d0, 4.0d0)
  x(3) = (5.0d0, 6.0d0)
  result = dznrm2(3, x, 1)
  call begin_test('three_elements')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: stride=2
  x(1) = (1.0d0, 0.0d0)
  x(2) = (99.0d0, 99.0d0)
  x(3) = (0.0d0, 1.0d0)
  result = dznrm2(2, x, 2)
  call begin_test('stride_2')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: large values — triggers abig (overflow-safe) path
  x(1) = (1.0d300, 1.0d300)
  x(2) = (1.0d300, 1.0d300)
  result = dznrm2(2, x, 1)
  call begin_test('large_values')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: small values — triggers asml (underflow-safe) path
  x(1) = (1.0d-300, 1.0d-300)
  x(2) = (1.0d-300, 1.0d-300)
  result = dznrm2(2, x, 1)
  call begin_test('small_values')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: mixed large and medium values — abig > 0 with amed > 0
  x(1) = (1.0d300, 1.0d300)
  x(2) = (1.0d0, 1.0d0)
  result = dznrm2(2, x, 1)
  call begin_test('large_and_medium')
  call print_scalar('result', result)
  call end_test()

  ! Test 9: mixed small and medium values — asml > 0, amed > 0, asml < amed
  x(1) = (1.0d-300, 1.0d-300)
  x(2) = (1.0d0, 1.0d0)
  result = dznrm2(2, x, 1)
  call begin_test('small_and_medium')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: mixed small and medium with small dominant (asml > amed after scaling)
  ! Need: amed > 0, asml > 0, and sqrt(asml)/SSML > sqrt(amed)
  ! TSML ~ 1.49e-154. Values below are "small", values above are "medium".
  ! Use large small values (just below threshold) and a tiny medium value.
  ! For asml to dominate: need many small contributions to outweigh the medium one.
  x(1) = (1.0d-154, 1.0d-154)
  x(2) = (1.0d-154, 1.0d-154)
  x(3) = (1.0d-154, 1.0d-154)
  x(4) = (1.0d-154, 1.0d-154)
  x(5) = (1.0d-154, 1.0d-154)
  x(6) = (1.0d-154, 1.0d-154)
  x(7) = (1.0d-154, 1.0d-154)
  x(8) = (1.0d-154, 1.0d-154)
  x(9) = (1.0d-154, 1.0d-154)
  x(10) = (1.5d-154, 0.0d0)
  result = dznrm2(10, x, 1)
  call begin_test('small_dominant')
  call print_scalar('result', result)
  call end_test()

end program
