program test_dladiv
  use test_utils
  implicit none
  double precision :: a, b, c, d, p, q

  ! Test 1: (4+2i)/(1+1i) = (4+2i)(1-1i)/((1+1i)(1-1i)) = (4+2+(-4+2)i)/2 = (6-2i)/2 = 3-1i
  a = 4.0d0; b = 2.0d0; c = 1.0d0; d = 1.0d0
  call dladiv(a, b, c, d, p, q)
  call begin_test('dladiv_basic')
  call print_scalar('p', p)
  call print_scalar('q', q)
  call end_test()

  ! Test 2: (1+0i)/(0+1i) = (1)(0-1i)/((0+1i)(0-1i)) = (0-1i)/1 = -i = 0-1i
  a = 1.0d0; b = 0.0d0; c = 0.0d0; d = 1.0d0
  call dladiv(a, b, c, d, p, q)
  call begin_test('dladiv_pure_imag_denom')
  call print_scalar('p', p)
  call print_scalar('q', q)
  call end_test()

  ! Test 3: (1+0i)/(1+0i) = 1+0i
  a = 1.0d0; b = 0.0d0; c = 1.0d0; d = 0.0d0
  call dladiv(a, b, c, d, p, q)
  call begin_test('dladiv_real_div')
  call print_scalar('p', p)
  call print_scalar('q', q)
  call end_test()

  ! Test 4: (0+0i)/(1+1i) = 0+0i
  a = 0.0d0; b = 0.0d0; c = 1.0d0; d = 1.0d0
  call dladiv(a, b, c, d, p, q)
  call begin_test('dladiv_zero_numer')
  call print_scalar('p', p)
  call print_scalar('q', q)
  call end_test()

  ! Test 5: (3+4i)/(1-2i) = (3+4i)(1+2i)/((1-2i)(1+2i)) = (3-8+(6+4)i)/5 = (-5+10i)/5 = -1+2i
  a = 3.0d0; b = 4.0d0; c = 1.0d0; d = -2.0d0
  call dladiv(a, b, c, d, p, q)
  call begin_test('dladiv_neg_denom')
  call print_scalar('p', p)
  call print_scalar('q', q)
  call end_test()

  ! Test 6: large values (overflow-safe)
  a = 1.0d300; b = 1.0d300; c = 1.0d300; d = 1.0d300
  call dladiv(a, b, c, d, p, q)
  call begin_test('dladiv_large')
  call print_scalar('p', p)
  call print_scalar('q', q)
  call end_test()

  ! Test 7: small values
  a = 1.0d-300; b = 1.0d-300; c = 1.0d-300; d = 1.0d-300
  call dladiv(a, b, c, d, p, q)
  call begin_test('dladiv_small')
  call print_scalar('p', p)
  call print_scalar('q', q)
  call end_test()

end program
