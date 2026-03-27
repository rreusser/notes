program test_zsyr2k
  use test_utils
  implicit none
  complex*16 :: a(16), b(16), c(16)
  double precision :: a_r(32), b_r(32), c_r(32)
  equivalence (a, a_r)
  equivalence (b, b_r)
  equivalence (c, c_r)

  ! Test 1: UPLO='U', TRANS='N', N=3, K=2
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.5d0); a(2) = (2.0d0, -1.0d0); a(3) = (3.0d0, 1.0d0)
  a(4) = (4.0d0, 2.0d0); a(5) = (5.0d0, 0.0d0);  a(6) = (6.0d0, -0.5d0)

  b = (0.0d0, 0.0d0)
  b(1) = (0.5d0, 1.0d0); b(2) = (1.5d0, -0.5d0); b(3) = (2.5d0, 0.0d0)
  b(4) = (3.0d0, 1.5d0); b(5) = (4.0d0, -1.0d0); b(6) = (5.0d0, 0.5d0)

  c = (0.0d0, 0.0d0)
  call zsyr2k('U', 'N', 3, 2, (1.0d0,0.0d0), a, 3, b, 3, &
              (0.0d0,0.0d0), c, 3)
  call begin_test('upper_no_trans')
  call print_array('C', c_r, 18)
  call end_test()

  ! Test 2: UPLO='L', TRANS='N', N=3, K=2
  c = (0.0d0, 0.0d0)
  call zsyr2k('L', 'N', 3, 2, (1.0d0,0.0d0), a, 3, b, 3, &
              (0.0d0,0.0d0), c, 3)
  call begin_test('lower_no_trans')
  call print_array('C', c_r, 18)
  call end_test()

  ! Test 3: UPLO='U', TRANS='T', N=2, K=3
  ! A is 3x2 (column-major), B is 3x2, C is 2x2
  c = (0.0d0, 0.0d0)
  call zsyr2k('U', 'T', 2, 3, (1.0d0,0.0d0), a, 3, b, 3, &
              (0.0d0,0.0d0), c, 2)
  call begin_test('upper_trans')
  call print_array('C', c_r, 8)
  call end_test()

  ! Test 4: UPLO='L', TRANS='T', N=2, K=3
  c = (0.0d0, 0.0d0)
  call zsyr2k('L', 'T', 2, 3, (1.0d0,0.0d0), a, 3, b, 3, &
              (0.0d0,0.0d0), c, 2)
  call begin_test('lower_trans')
  call print_array('C', c_r, 8)
  call end_test()

  ! Test 5: complex alpha=(2,1), complex beta=(0.5,-0.5)
  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 1.0d0); c(4) = (2.0d0, -1.0d0)
  c(5) = (0.5d0, 0.5d0); c(8) = (1.0d0, 0.0d0)
  c(9) = (3.0d0, -1.0d0)
  call zsyr2k('U', 'N', 3, 2, (2.0d0,1.0d0), a, 3, b, 3, &
              (0.5d0,-0.5d0), c, 3)
  call begin_test('complex_alpha_beta')
  call print_array('C', c_r, 18)
  call end_test()

  ! Test 6: alpha=0, beta=(2,0) -- just scale C
  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 2.0d0); c(2) = (3.0d0, 4.0d0)
  c(3) = (5.0d0, 6.0d0); c(4) = (7.0d0, 8.0d0)
  call zsyr2k('U', 'N', 2, 2, (0.0d0,0.0d0), a, 2, b, 2, &
              (2.0d0,0.0d0), c, 2)
  call begin_test('alpha_zero')
  call print_array('C', c_r, 8)
  call end_test()

  ! Test 7: N=0 quick return
  c(1) = (99.0d0, 0.0d0)
  call zsyr2k('U', 'N', 0, 2, (1.0d0,0.0d0), a, 1, b, 1, &
              (0.0d0,0.0d0), c, 1)
  call begin_test('n_zero')
  call print_array('C', c_r, 2)
  call end_test()

  ! Test 8: alpha=0, beta=0 -- zero out C
  c = (0.0d0, 0.0d0)
  c(1) = (99.0d0, 88.0d0); c(2) = (77.0d0, 66.0d0)
  c(3) = (55.0d0, 44.0d0); c(4) = (33.0d0, 22.0d0)
  call zsyr2k('L', 'N', 2, 2, (0.0d0,0.0d0), a, 2, b, 2, &
              (0.0d0,0.0d0), c, 2)
  call begin_test('alpha_zero_beta_zero')
  call print_array('C', c_r, 8)
  call end_test()

  ! Test 9: alpha=0, beta=1 -- quick return (no-op)
  c = (0.0d0, 0.0d0)
  c(1) = (42.0d0, 13.0d0); c(2) = (7.0d0, 8.0d0)
  c(3) = (9.0d0, 10.0d0);  c(4) = (11.0d0, 12.0d0)
  call zsyr2k('L', 'N', 2, 2, (0.0d0,0.0d0), a, 2, b, 2, &
              (1.0d0,0.0d0), c, 2)
  call begin_test('alpha_zero_beta_one')
  call print_array('C', c_r, 8)
  call end_test()

  ! Test 10: Lower with nonzero beta
  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 1.0d0); c(2) = (2.0d0, -1.0d0); c(3) = (0.5d0, 0.5d0)
  c(5) = (0.0d0, 2.0d0); c(6) = (3.0d0, -1.0d0)
  c(9) = (1.0d0, 0.0d0)
  call zsyr2k('L', 'N', 3, 2, (1.0d0,0.0d0), a, 3, b, 3, &
              (0.5d0,0.0d0), c, 3)
  call begin_test('lower_nonzero_beta')
  call print_array('C', c_r, 18)
  call end_test()

  ! Test 11: 1x1 case
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); c = (0.0d0, 0.0d0)
  a(1) = (3.0d0, 2.0d0)
  b(1) = (1.0d0, -1.0d0)
  call zsyr2k('U', 'N', 1, 1, (2.0d0,1.0d0), a, 1, b, 1, &
              (0.0d0,0.0d0), c, 1)
  call begin_test('scalar')
  call print_array('C', c_r, 2)
  call end_test()

end program
