program test_zlassq
  use test_utils
  implicit none
  complex*16 :: x(10)
  double precision :: scl, sumsq

  ! Test 1: basic accumulation from zero
  ! x = [(3,4), (1,2)] => |x1|^2 + |x2|^2 = 9+16 + 1+4 = 30
  ! scale=0, sumsq=0 initially => after: scale=1, sumsq=30 (approximately)
  x = (0.0d0, 0.0d0)
  x(1) = (3.0d0, 4.0d0)
  x(2) = (1.0d0, 2.0d0)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(2, x, 1, scl, sumsq)
  call begin_test('zlassq_basic')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 2: accumulate onto existing sum
  ! x = [(1,0)] => sum of squares = 1
  ! Start with scl=2, sumsq=3 => represents 2^2*3 = 12
  ! After: should represent 12 + 1 = 13
  x(1) = (1.0d0, 0.0d0)
  scl = 2.0d0
  sumsq = 3.0d0
  call zlassq(1, x, 1, scl, sumsq)
  call begin_test('zlassq_accumulate')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 3: N=0 quick return
  scl = 2.0d0
  sumsq = 5.0d0
  call zlassq(0, x, 1, scl, sumsq)
  call begin_test('zlassq_n_zero')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 4: stride=2
  x = (0.0d0, 0.0d0)
  x(1) = (3.0d0, 0.0d0)
  x(2) = (99.0d0, 99.0d0)
  x(3) = (4.0d0, 0.0d0)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(2, x, 2, scl, sumsq)
  call begin_test('zlassq_stride')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 5: single element purely imaginary
  x(1) = (0.0d0, 5.0d0)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(1, x, 1, scl, sumsq)
  call begin_test('zlassq_imag')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 6: very large values (overflow path, abig accumulator)
  x(1) = (1.0d300, 1.0d300)
  x(2) = (2.0d300, 0.0d0)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(2, x, 1, scl, sumsq)
  call begin_test('zlassq_big')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 7: very small values (underflow path, asml accumulator)
  x(1) = (1.0d-300, 1.0d-300)
  x(2) = (2.0d-300, 0.0d0)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(2, x, 1, scl, sumsq)
  call begin_test('zlassq_small')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 8: mix of big and normal values (abig + amed combination)
  x(1) = (1.0d300, 0.0d0)
  x(2) = (1.0d0, 0.0d0)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(2, x, 1, scl, sumsq)
  call begin_test('zlassq_big_normal')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 9: mix of small and normal values (asml + amed combination)
  x(1) = (1.0d-300, 0.0d0)
  x(2) = (1.0d0, 0.0d0)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(2, x, 1, scl, sumsq)
  call begin_test('zlassq_small_normal')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 10: negative stride
  x = (0.0d0, 0.0d0)
  x(1) = (3.0d0, 0.0d0)
  x(2) = (4.0d0, 0.0d0)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(2, x, -1, scl, sumsq)
  call begin_test('zlassq_neg_stride')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 11: accumulate big existing sum onto big values
  ! scale=1e200, sumsq=1.0 => represents (1e200)^2 * 1 = 1e400
  ! Then add big x values
  x(1) = (1.0d300, 0.0d0)
  scl = 1.0d200
  sumsq = 1.0d0
  call zlassq(1, x, 1, scl, sumsq)
  call begin_test('zlassq_big_existing')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 12: accumulate small existing sum onto small values
  ! scale=1e-200, sumsq=1.0
  x(1) = (1.0d-300, 0.0d0)
  scl = 1.0d-200
  sumsq = 1.0d0
  call zlassq(1, x, 1, scl, sumsq)
  call begin_test('zlassq_small_existing')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 13: pure small values only (asml only, no amed), tests the asml-only branch
  x(1) = (1.0d-300, 2.0d-300)
  scl = 1.0d0
  sumsq = 0.0d0
  call zlassq(1, x, 1, scl, sumsq)
  call begin_test('zlassq_pure_small')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 14: sumsq=0, scale=0 (tests the scale=0, sumsq=0 paths)
  x(1) = (3.0d0, 4.0d0)
  scl = 0.0d0
  sumsq = 0.0d0
  call zlassq(1, x, 1, scl, sumsq)
  call begin_test('zlassq_scale_zero')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 15: big existing sum with scale > 1 (tests scale>1 in big sumsq branch)
  x(1) = (1.0d300, 0.0d0)
  scl = 2.0d0
  sumsq = 1.0d308
  call zlassq(1, x, 1, scl, sumsq)
  call begin_test('zlassq_big_existing_scale_gt1')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

  ! Test 16: small existing sum with scale < 1
  x(1) = (1.0d-300, 0.0d0)
  scl = 0.5d0
  sumsq = 1.0d-300
  call zlassq(1, x, 1, scl, sumsq)
  call begin_test('zlassq_small_existing_scale_lt1')
  call print_scalar('scl', scl)
  call print_scalar('sumsq', sumsq)
  call end_test()

end program
