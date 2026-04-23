program test_zpstrf
  use test_utils
  implicit none

  complex*16 :: a(16)
  double precision :: a_r(32)
  equivalence (a, a_r)
  double precision :: work(8)
  integer :: piv(4), rank, info

  ! Test 1: upper, 3x3 Hermitian positive definite
  ! A = [10    (2+i)    (3-2i) ]
  !     [.      8       (1+i)  ]
  !     [.      .        6     ]
  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(4) = (2.0d0, 1.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(7) = (3.0d0, -2.0d0)
  a(8) = (1.0d0, 1.0d0)
  a(9) = (6.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('U', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 18)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 2: lower, 3x3 Hermitian positive definite
  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(2) = (2.0d0, -1.0d0)
  a(3) = (3.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(6) = (1.0d0, -1.0d0)
  a(9) = (6.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('L', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 18)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 3: upper, 4x4 Hermitian positive definite
  a = (0.0d0, 0.0d0)
  a(1)  = (20.0d0, 0.0d0)
  a(5)  = (1.0d0, 2.0d0)
  a(6)  = (15.0d0, 0.0d0)
  a(9)  = (3.0d0, -1.0d0)
  a(10) = (4.0d0, 2.0d0)
  a(11) = (18.0d0, 0.0d0)
  a(13) = (2.0d0, 3.0d0)
  a(14) = (1.0d0, -1.0d0)
  a(15) = (5.0d0, 1.0d0)
  a(16) = (12.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('U', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 32)
  call print_int_array('piv', piv, 4)
  call end_test()

  ! Test 4: lower, 4x4 Hermitian positive definite (same matrix)
  a = (0.0d0, 0.0d0)
  a(1)  = (20.0d0, 0.0d0)
  a(2)  = (1.0d0, -2.0d0)
  a(3)  = (3.0d0, 1.0d0)
  a(4)  = (2.0d0, -3.0d0)
  a(6)  = (15.0d0, 0.0d0)
  a(7)  = (4.0d0, -2.0d0)
  a(8)  = (1.0d0, 1.0d0)
  a(11) = (18.0d0, 0.0d0)
  a(12) = (5.0d0, -1.0d0)
  a(16) = (12.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('L', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 32)
  call print_int_array('piv', piv, 4)
  call end_test()

  ! Test 5: rank-deficient 3x3, upper
  ! A = v * v^H where v = [1, (1+i), (2-i)] => rank 1
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0)
  a(4) = (1.0d0, -1.0d0)
  a(5) = (2.0d0, 0.0d0)
  a(7) = (2.0d0, 1.0d0)
  a(8) = (1.0d0, 3.0d0)
  a(9) = (5.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('U', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_upper')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 18)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 6: rank-deficient 3x3, lower (same matrix)
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0)
  a(2) = (1.0d0, 1.0d0)
  a(3) = (2.0d0, -1.0d0)
  a(5) = (2.0d0, 0.0d0)
  a(6) = (1.0d0, -3.0d0)
  a(9) = (5.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('L', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_lower')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 18)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 7: N=0
  call zpstrf('U', 0, a, 1, piv, rank, -1.0d0, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1
  a(1) = (9.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('U', 1, a, 1, piv, rank, -1.0d0, work, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 2)
  call print_int_array('piv', piv, 1)
  call end_test()

  ! Test 9: rank-deficient 4x4, upper (rank 2)
  a = (0.0d0, 0.0d0)
  a(1)  = (3.0d0, 0.0d0)
  a(5)  = (2.0d0, 0.0d0)
  a(6)  = (3.0d0, 0.0d0)
  a(9)  = (5.0d0, -1.0d0)
  a(10) = (4.0d0, -1.0d0)
  a(11) = (9.0d0, 0.0d0)
  a(13) = (1.0d0, -1.0d0)
  a(14) = (0.0d0, -2.0d0)
  a(15) = (2.0d0, -2.0d0)
  a(16) = (2.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('U', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_4x4_upper')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 32)
  call print_int_array('piv', piv, 4)
  call end_test()

  ! Test 10: rank-deficient 4x4, lower (same matrix)
  a = (0.0d0, 0.0d0)
  a(1)  = (3.0d0, 0.0d0)
  a(2)  = (2.0d0, 0.0d0)
  a(3)  = (5.0d0, 1.0d0)
  a(4)  = (1.0d0, 1.0d0)
  a(6)  = (3.0d0, 0.0d0)
  a(7)  = (4.0d0, 1.0d0)
  a(8)  = (0.0d0, 2.0d0)
  a(11) = (9.0d0, 0.0d0)
  a(12) = (2.0d0, 2.0d0)
  a(16) = (2.0d0, 0.0d0)
  work = 0.0d0
  call zpstrf('L', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_4x4_lower')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 32)
  call print_int_array('piv', piv, 4)
  call end_test()

end program
