program test_dspr
  use test_utils
  implicit none
  double precision :: ap(20), x(20)

  ! Test 1: upper packed, alpha=1, n=3
  ! Initial AP = upper triangle of I (identity)
  ap = 0.0d0
  ap(1) = 1.0d0; ap(3) = 1.0d0; ap(6) = 1.0d0  ! diag elements at 1,3,6
  x(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  call dspr('U', 3, 1.0d0, x, 1, ap)
  call begin_test('upper_basic')
  call print_array('ap', ap, 6)
  call end_test()

  ! Test 2: lower packed, alpha=1, n=3
  ap = 0.0d0
  ap(1) = 1.0d0; ap(4) = 1.0d0; ap(6) = 1.0d0  ! diag at 1,4,6
  x(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  call dspr('L', 3, 1.0d0, x, 1, ap)
  call begin_test('lower_basic')
  call print_array('ap', ap, 6)
  call end_test()

  ! Test 3: alpha=2
  ap = 0.0d0
  x(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  call dspr('U', 3, 2.0d0, x, 1, ap)
  call begin_test('alpha2')
  call print_array('ap', ap, 6)
  call end_test()

  ! Test 4: n=0
  ap(1) = 99.0d0
  call dspr('U', 0, 1.0d0, x, 1, ap)
  call begin_test('n_zero')
  call print_array('ap', ap, 1)
  call end_test()

  ! Test 5: alpha=0
  ap = 0.0d0; ap(1) = 5.0d0
  x(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  call dspr('U', 3, 0.0d0, x, 1, ap)
  call begin_test('alpha_zero')
  call print_array('ap', ap, 6)
  call end_test()

  ! Test 6: stride=2
  ap = 0.0d0
  x = 0.0d0
  x(1) = 1.0d0; x(3) = 2.0d0; x(5) = 3.0d0
  call dspr('U', 3, 1.0d0, x, 2, ap)
  call begin_test('stride')
  call print_array('ap', ap, 6)
  call end_test()

end program
