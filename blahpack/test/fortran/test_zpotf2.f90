program test_zpotf2
  use test_utils
  implicit none
  complex*16 :: A(25)
  double precision :: A_r(50)
  equivalence (A, A_r)
  integer :: info

  ! Test 1: upper, 2x2 HPD matrix
  ! A = [4  (2+i)]
  !     [.   5   ]
  ! A^H*A sense: diag must be real
  A = (0.0d0, 0.0d0)
  A(1) = (4.0d0, 0.0d0)
  A(3) = (2.0d0, 1.0d0)
  A(4) = (5.0d0, 0.0d0)
  call zpotf2('U', 2, A, 2, info)
  call begin_test('upper_2x2')
  call print_int('info', info)
  call print_array('A', A_r, 8)
  call end_test()

  ! Test 2: lower, 2x2 HPD matrix
  ! A = [4     .   ]
  !     [(2-i) 5   ]
  A = (0.0d0, 0.0d0)
  A(1) = (4.0d0, 0.0d0)
  A(2) = (2.0d0, -1.0d0)
  A(4) = (5.0d0, 0.0d0)
  call zpotf2('L', 2, A, 2, info)
  call begin_test('lower_2x2')
  call print_int('info', info)
  call print_array('A', A_r, 8)
  call end_test()

  ! Test 3: N=0 quick return
  info = -1
  call zpotf2('U', 0, A, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=1
  A(1) = (9.0d0, 0.0d0)
  call zpotf2('U', 1, A, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 5: upper, 3x3 HPD matrix
  ! A = [10    (2+i)    (3-2i) ]
  !     [.      8       (1+i)  ]
  !     [.      .        6     ]
  A = (0.0d0, 0.0d0)
  A(1) = (10.0d0, 0.0d0)
  A(4) = (2.0d0, 1.0d0)
  A(5) = (8.0d0, 0.0d0)
  A(7) = (3.0d0, -2.0d0)
  A(8) = (1.0d0, 1.0d0)
  A(9) = (6.0d0, 0.0d0)
  call zpotf2('U', 3, A, 3, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('A', A_r, 18)
  call end_test()

  ! Test 6: lower, 3x3 HPD matrix
  ! Same as above but lower stored
  A = (0.0d0, 0.0d0)
  A(1) = (10.0d0, 0.0d0)
  A(2) = (2.0d0, -1.0d0)
  A(3) = (3.0d0, 2.0d0)
  A(5) = (8.0d0, 0.0d0)
  A(6) = (1.0d0, -1.0d0)
  A(9) = (6.0d0, 0.0d0)
  call zpotf2('L', 3, A, 3, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('A', A_r, 18)
  call end_test()

  ! Test 7: not positive definite (upper)
  ! A = [1   (2+i)]
  !     [.    1   ]
  ! |a12|^2 = 5 > a11*a22 = 1, so not HPD
  A = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)
  A(3) = (2.0d0, 1.0d0)
  A(4) = (1.0d0, 0.0d0)
  call zpotf2('U', 2, A, 2, info)
  call begin_test('not_hpd')
  call print_int('info', info)
  call end_test()

  ! Test 8: not positive definite (lower)
  A = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)
  A(2) = (2.0d0, -1.0d0)
  A(4) = (1.0d0, 0.0d0)
  call zpotf2('L', 2, A, 2, info)
  call begin_test('not_hpd_lower')
  call print_int('info', info)
  call end_test()

end program
