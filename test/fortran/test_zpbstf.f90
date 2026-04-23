program test_zpbstf
  use test_utils
  implicit none

  ! Band storage: LDAB = KD+1 rows, N columns (each element is complex*16)
  ! Upper: AB(KD+1+i-j, j) = A(i,j) for max(1,j-kd)<=i<=j
  ! Lower: AB(1+i-j, j) = A(i,j) for j<=i<=min(n,j+kd)

  complex*16 :: ab(100)
  double precision :: ab_r(200)
  equivalence (ab, ab_r)
  integer :: info, ldab

  ! ===== Test 1: UPLO='U', N=5, KD=1 (tridiagonal) =====
  ! Hermitian PD tridiagonal: A(i,i)=4, A(i,i+1)=(-1+0.5i)
  ! Band storage (upper): LDAB=2, rows = [superdiag, diag]
  ldab = 2
  ab = (0.0d0, 0.0d0)
  ! Col 1
  ab(2) = (4.0d0, 0.0d0)
  ! Col 2
  ab(3) = (-1.0d0, 0.5d0)
  ab(4) = (4.0d0, 0.0d0)
  ! Col 3
  ab(5) = (-1.0d0, 0.5d0)
  ab(6) = (4.0d0, 0.0d0)
  ! Col 4
  ab(7) = (-1.0d0, 0.5d0)
  ab(8) = (4.0d0, 0.0d0)
  ! Col 5
  ab(9) = (-1.0d0, 0.5d0)
  ab(10) = (4.0d0, 0.0d0)

  call zpbstf('U', 5, 1, ab, ldab, info)
  call begin_test('upper_tridiag_5')
  call print_int('info', info)
  call print_array('ab', ab_r, 20)
  call end_test()

  ! ===== Test 2: UPLO='L', N=5, KD=1 (tridiagonal) =====
  ! Same matrix, lower band storage: A(i,i+1)=(-1+0.5i) means subdiag = conj = (-1-0.5i)
  ldab = 2
  ab = (0.0d0, 0.0d0)
  ! Col 1
  ab(1) = (4.0d0, 0.0d0)
  ab(2) = (-1.0d0, -0.5d0)
  ! Col 2
  ab(3) = (4.0d0, 0.0d0)
  ab(4) = (-1.0d0, -0.5d0)
  ! Col 3
  ab(5) = (4.0d0, 0.0d0)
  ab(6) = (-1.0d0, -0.5d0)
  ! Col 4
  ab(7) = (4.0d0, 0.0d0)
  ab(8) = (-1.0d0, -0.5d0)
  ! Col 5
  ab(9) = (4.0d0, 0.0d0)

  call zpbstf('L', 5, 1, ab, ldab, info)
  call begin_test('lower_tridiag_5')
  call print_int('info', info)
  call print_array('ab', ab_r, 20)
  call end_test()

  ! ===== Test 3: UPLO='U', N=4, KD=2 =====
  ! HPD matrix with bandwidth 2
  ! A = [10    (1+i)   (0.5-0.5i)   0       ]
  !     [(1-i)  8      (2+0.5i)    (1-0.5i) ]
  !     [(0.5+0.5i)(2-0.5i) 6      (1+i)    ]
  !     [  0   (1+0.5i)(1-i)        7       ]
  ldab = 3
  ab = (0.0d0, 0.0d0)
  ! Col 1: AB(3,1)=10
  ab(3) = (10.0d0, 0.0d0)
  ! Col 2: AB(2,2)=(1+i), AB(3,2)=8
  ab(5) = (1.0d0, 1.0d0)
  ab(6) = (8.0d0, 0.0d0)
  ! Col 3: AB(1,3)=(0.5-0.5i), AB(2,3)=(2+0.5i), AB(3,3)=6
  ab(7) = (0.5d0, -0.5d0)
  ab(8) = (2.0d0, 0.5d0)
  ab(9) = (6.0d0, 0.0d0)
  ! Col 4: AB(2,4)=(1-0.5i), AB(3,4)=7
  ab(11) = (1.0d0, -0.5d0)
  ab(12) = (7.0d0, 0.0d0)

  call zpbstf('U', 4, 2, ab, ldab, info)
  call begin_test('upper_penta_4')
  call print_int('info', info)
  call print_array('ab', ab_r, 24)
  call end_test()

  ! ===== Test 4: UPLO='L', N=4, KD=2 (same matrix as test 3) =====
  ldab = 3
  ab = (0.0d0, 0.0d0)
  ! Col 1: AB(1,1)=10, AB(2,1)=(1-i), AB(3,1)=(0.5+0.5i)
  ab(1) = (10.0d0, 0.0d0)
  ab(2) = (1.0d0, -1.0d0)
  ab(3) = (0.5d0, 0.5d0)
  ! Col 2: AB(1,2)=8, AB(2,2)=(2-0.5i), AB(3,2)=(1+0.5i)
  ab(4) = (8.0d0, 0.0d0)
  ab(5) = (2.0d0, -0.5d0)
  ab(6) = (1.0d0, 0.5d0)
  ! Col 3: AB(1,3)=6, AB(2,3)=(1-i)
  ab(7) = (6.0d0, 0.0d0)
  ab(8) = (1.0d0, -1.0d0)
  ! Col 4: AB(1,4)=7
  ab(10) = (7.0d0, 0.0d0)

  call zpbstf('L', 4, 2, ab, ldab, info)
  call begin_test('lower_penta_4')
  call print_int('info', info)
  call print_array('ab', ab_r, 24)
  call end_test()

  ! ===== Test 5: N=1 =====
  ab(1) = (9.0d0, 0.0d0)
  call zpbstf('U', 1, 0, ab, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('ab', ab_r, 2)
  call end_test()

  ! ===== Test 6: N=0 (quick return) =====
  call zpbstf('L', 0, 0, ab, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 7: Not HPD (upper) =====
  ! 2x2: A(1,1)=1, A(2,2)=1, A(1,2)=(2+i), |a12|^2=5 > 1*1
  ldab = 2
  ab = (0.0d0, 0.0d0)
  ab(2) = (1.0d0, 0.0d0)
  ab(3) = (2.0d0, 1.0d0)
  ab(4) = (1.0d0, 0.0d0)
  call zpbstf('U', 2, 1, ab, ldab, info)
  call begin_test('not_hpd_upper')
  call print_int('info', info)
  call end_test()

  ! ===== Test 8: Not HPD (lower) =====
  ab = (0.0d0, 0.0d0)
  ab(1) = (1.0d0, 0.0d0)
  ab(2) = (2.0d0, -1.0d0)
  ab(3) = (1.0d0, 0.0d0)
  call zpbstf('L', 2, 1, ab, ldab, info)
  call begin_test('not_hpd_lower')
  call print_int('info', info)
  call end_test()

  ! ===== Test 9: UPLO='U', N=7, KD=2 =====
  ! Strongly diagonally dominant: A(i,i)=10, A(i,i+1)=(-1+0.5i), A(i,i+2)=(0.3-0.2i)
  ldab = 3
  ab = (0.0d0, 0.0d0)
  ! Col 1
  ab(3) = (10.0d0, 0.0d0)
  ! Col 2
  ab(5) = (-1.0d0, 0.5d0)
  ab(6) = (10.0d0, 0.0d0)
  ! Col 3
  ab(7) = (0.3d0, -0.2d0)
  ab(8) = (-1.0d0, 0.5d0)
  ab(9) = (10.0d0, 0.0d0)
  ! Col 4
  ab(10) = (0.3d0, -0.2d0)
  ab(11) = (-1.0d0, 0.5d0)
  ab(12) = (10.0d0, 0.0d0)
  ! Col 5
  ab(13) = (0.3d0, -0.2d0)
  ab(14) = (-1.0d0, 0.5d0)
  ab(15) = (10.0d0, 0.0d0)
  ! Col 6
  ab(16) = (0.3d0, -0.2d0)
  ab(17) = (-1.0d0, 0.5d0)
  ab(18) = (10.0d0, 0.0d0)
  ! Col 7
  ab(19) = (0.3d0, -0.2d0)
  ab(20) = (-1.0d0, 0.5d0)
  ab(21) = (10.0d0, 0.0d0)

  call zpbstf('U', 7, 2, ab, ldab, info)
  call begin_test('upper_7x7_kd2')
  call print_int('info', info)
  call print_array('ab', ab_r, 42)
  call end_test()

  ! ===== Test 10: UPLO='L', N=7, KD=2 (same matrix as test 9) =====
  ldab = 3
  ab = (0.0d0, 0.0d0)
  ! Col 1
  ab(1) = (10.0d0, 0.0d0)
  ab(2) = (-1.0d0, -0.5d0)
  ab(3) = (0.3d0, 0.2d0)
  ! Col 2
  ab(4) = (10.0d0, 0.0d0)
  ab(5) = (-1.0d0, -0.5d0)
  ab(6) = (0.3d0, 0.2d0)
  ! Col 3
  ab(7) = (10.0d0, 0.0d0)
  ab(8) = (-1.0d0, -0.5d0)
  ab(9) = (0.3d0, 0.2d0)
  ! Col 4
  ab(10) = (10.0d0, 0.0d0)
  ab(11) = (-1.0d0, -0.5d0)
  ab(12) = (0.3d0, 0.2d0)
  ! Col 5
  ab(13) = (10.0d0, 0.0d0)
  ab(14) = (-1.0d0, -0.5d0)
  ab(15) = (0.3d0, 0.2d0)
  ! Col 6
  ab(16) = (10.0d0, 0.0d0)
  ab(17) = (-1.0d0, -0.5d0)
  ! Col 7
  ab(19) = (10.0d0, 0.0d0)

  call zpbstf('L', 7, 2, ab, ldab, info)
  call begin_test('lower_7x7_kd2')
  call print_int('info', info)
  call print_array('ab', ab_r, 42)
  call end_test()

end program
