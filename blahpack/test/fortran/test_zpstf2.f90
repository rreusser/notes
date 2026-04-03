program test_zpstf2
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
  ! Stored upper only (column-major): A(1,1)=10, A(1,2)=(2+i), A(2,2)=8,
  !   A(1,3)=(3-2i), A(2,3)=(1+i), A(3,3)=6
  ! Full Hermitian:
  ! [10      (2+i)   (3-2i)]
  ! [(2-i)    8      (1+i) ]
  ! [(3+2i)  (1-i)    6    ]
  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(4) = (2.0d0, 1.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(7) = (3.0d0, -2.0d0)
  a(8) = (1.0d0, 1.0d0)
  a(9) = (6.0d0, 0.0d0)
  work = 0.0d0
  call zpstf2('U', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 18)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 2: lower, 3x3 Hermitian positive definite
  ! Same matrix, lower storage
  ! A(1,1)=10, A(2,1)=(2-i), A(3,1)=(3+2i), A(2,2)=8, A(3,2)=(1-i), A(3,3)=6
  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(2) = (2.0d0, -1.0d0)
  a(3) = (3.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(6) = (1.0d0, -1.0d0)
  a(9) = (6.0d0, 0.0d0)
  work = 0.0d0
  call zpstf2('L', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 18)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 3: upper, 4x4 Hermitian positive definite
  ! A = [20     (1+2i)   (3-i)    (2+3i) ]
  !     [.       15      (4+2i)   (1-i)  ]
  !     [.       .        18      (5+i)  ]
  !     [.       .        .        12    ]
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
  call zpstf2('U', 4, a, 4, piv, rank, -1.0d0, work, info)
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
  call zpstf2('L', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 32)
  call print_int_array('piv', piv, 4)
  call end_test()

  ! Test 5: rank-deficient 3x3, upper
  ! A = v * v^H where v = [1, (1+i), (2-i)]
  ! A(1,1) = 1, A(1,2) = (1+i), A(2,2) = |1+i|^2 = 2, A(1,3) = (2-i),
  ! A(2,3) = (1+i)*(2+i) = (1*2+i*1+2*i+i*i*1) = (2+i+2i-1) = (1+3i),
  ! wait let me recompute: conj(v1)*v3 = (1-i)*(2-i) = 2-i-2i+i^2 = 1-3i
  ! v*v^H: A(i,j) = v(i)*conj(v(j))
  ! (1,1)=1*1=1, (1,2)=1*conj(1+i)=(1-i), (1,3)=1*conj(2-i)=(2+i)
  ! (2,2)=(1+i)*conj(1+i)=(1+i)*(1-i)=2
  ! (2,3)=(1+i)*conj(2-i)=(1+i)*(2+i)=2+i+2i+i^2=1+3i
  ! (3,3)=(2-i)*conj(2-i)=(2-i)*(2+i)=5
  ! Upper storage only:
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0)
  a(4) = (1.0d0, -1.0d0)
  a(5) = (2.0d0, 0.0d0)
  a(7) = (2.0d0, 1.0d0)
  a(8) = (1.0d0, 3.0d0)
  a(9) = (5.0d0, 0.0d0)
  work = 0.0d0
  call zpstf2('U', 3, a, 3, piv, rank, -1.0d0, work, info)
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
  call zpstf2('L', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_lower')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 18)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 7: N=0
  call zpstf2('U', 0, a, 1, piv, rank, -1.0d0, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1
  a(1) = (9.0d0, 0.0d0)
  work = 0.0d0
  call zpstf2('U', 1, a, 1, piv, rank, -1.0d0, work, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 2)
  call print_int_array('piv', piv, 1)
  call end_test()

  ! Test 9: rank-deficient 4x4, upper
  ! A = v1*v1^H + v2*v2^H where v1=[1,(1+i),2,(1-i)], v2=[(1+i),1,(2+i),0]
  ! This gives rank 2
  ! Compute manually:
  ! v1*v1^H:
  !   (1,1)=1*1=1, (1,2)=1*(1-i)=(1-i), (1,3)=1*2=2, (1,4)=1*(1+i)=(1+i)
  !   (2,2)=(1+i)*(1-i)=2, (2,3)=(1+i)*2=2+2i, (2,4)=(1+i)*(1+i)=2i
  !   (3,3)=2*2=4, (3,4)=2*(1+i)=(2+2i)
  !   (4,4)=(1-i)*(1+i)=2
  ! v2*v2^H:
  !   (1,1)=(1+i)*(1-i)=2, (1,2)=(1+i)*1=(1+i), (1,3)=(1+i)*(2-i)=2-i+2i+1=(3+i), (1,4)=0
  !   (2,2)=1*1=1, (2,3)=1*(2-i)=(2-i), (2,4)=0
  !   (3,3)=(2+i)*(2-i)=5, (3,4)=0
  !   (4,4)=0
  ! A = v1v1^H + v2v2^H:
  !   (1,1)=3, (1,2)=(1-i)+(1+i)=2, (1,3)=2+(3+i)=(5+i), (1,4)=(1+i)
  !   (2,2)=3, (2,3)=(2+2i)+(2-i)=(4+i), (2,4)=2i
  !   (3,3)=9, (3,4)=(2+2i)
  !   (4,4)=2
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
  call zpstf2('U', 4, a, 4, piv, rank, -1.0d0, work, info)
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
  call zpstf2('L', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_4x4_lower')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a_r, 32)
  call print_int_array('piv', piv, 4)
  call end_test()

end program
