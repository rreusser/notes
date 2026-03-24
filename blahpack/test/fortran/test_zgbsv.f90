program test_zgbsv
  use test_utils
  implicit none
  double precision :: AB4_r(80), B_r(20), AB2_r(16)
  complex*16 :: AB4(4, 10), B(10, 2), AB2(2, 4)
  equivalence (AB4, AB4_r)
  equivalence (B, B_r)
  equivalence (AB2, AB2_r)
  integer :: ipiv(10), info

  ! ============================================================
  ! Test 1: 4x4 tridiagonal (KL=1, KU=1), single RHS
  ! A = [4+i   -1      0       0    ]
  !     [-1     4+i    -1       0    ]
  !     [ 0    -1       4+i    -1    ]
  !     [ 0     0      -1       4+i  ]
  ! b = [1, 2, 3, 4]
  ! LDAB = 2*KL+KU+1 = 4
  AB4 = (0.0d0, 0.0d0)
  AB4(2,1) = (0.0d0,0.0d0);  AB4(3,1) = (4.0d0,1.0d0);  AB4(4,1) = (-1.0d0,0.0d0)
  AB4(2,2) = (-1.0d0,0.0d0); AB4(3,2) = (4.0d0,1.0d0);  AB4(4,2) = (-1.0d0,0.0d0)
  AB4(2,3) = (-1.0d0,0.0d0); AB4(3,3) = (4.0d0,1.0d0);  AB4(4,3) = (-1.0d0,0.0d0)
  AB4(2,4) = (-1.0d0,0.0d0); AB4(3,4) = (4.0d0,1.0d0);  AB4(4,4) = (0.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 0.0d0)
  B(3,1) = (3.0d0, 0.0d0)
  B(4,1) = (4.0d0, 0.0d0)
  ipiv = 0
  call ZGBSV(4, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('tridiag_4x4_1rhs')
  call print_array('x', B_r, 8)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! ============================================================
  ! Test 2: 4x4 tridiagonal, 2 RHS (with complex RHS)
  AB4 = (0.0d0, 0.0d0)
  AB4(2,1) = (0.0d0,0.0d0);  AB4(3,1) = (4.0d0,1.0d0);  AB4(4,1) = (-1.0d0,0.0d0)
  AB4(2,2) = (-1.0d0,0.0d0); AB4(3,2) = (4.0d0,1.0d0);  AB4(4,2) = (-1.0d0,0.0d0)
  AB4(2,3) = (-1.0d0,0.0d0); AB4(3,3) = (4.0d0,1.0d0);  AB4(4,3) = (-1.0d0,0.0d0)
  AB4(2,4) = (-1.0d0,0.0d0); AB4(3,4) = (4.0d0,1.0d0);  AB4(4,4) = (0.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (5.0d0, 1.0d0)
  B(2,1) = (2.0d0, 0.0d0); B(2,2) = (6.0d0, 2.0d0)
  B(3,1) = (3.0d0, 0.0d0); B(3,2) = (7.0d0, 3.0d0)
  B(4,1) = (4.0d0, 0.0d0); B(4,2) = (8.0d0, 4.0d0)
  ipiv = 0
  call ZGBSV(4, 1, 1, 2, AB4, 4, ipiv, B, 10, info)
  call begin_test('tridiag_4x4_2rhs')
  call print_matrix('x', B_r, 20, 8, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: N=1 (scalar system)
  ! A = [3+2i], b = [6+4i], x = [2+0i]
  ! LDAB = 2*0+0+1 = 1 (KL=0, KU=0)
  ! But zgbsv needs LDAB >= 2*KL+KU+1, so use KL=0, KU=0, LDAB=1
  ! Use AB2 for small matrix
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (3.0d0, 2.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (6.0d0, 4.0d0)
  ipiv = 0
  call ZGBSV(1, 0, 0, 1, AB2, 1, ipiv, B, 10, info)
  call begin_test('n_one')
  call print_array('x', B_r, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: Singular matrix (INFO > 0)
  ! A = [1+0i  0; 0  0+0i] => U(2,2) = 0 => INFO=2
  ! KL=1, KU=1, LDAB=4
  AB4 = (0.0d0, 0.0d0)
  AB4(3,1) = (1.0d0, 0.0d0)  ! diag(1)
  AB4(3,2) = (0.0d0, 0.0d0)  ! diag(2) = 0 => singular
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 0.0d0)
  ipiv = 0
  call ZGBSV(2, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: N=0 quick return
  ipiv = 0
  B = (0.0d0, 0.0d0)
  call ZGBSV(0, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: Complex RHS with imaginary parts
  ! 4x4 tridiagonal, b has imaginary parts
  AB4 = (0.0d0, 0.0d0)
  AB4(2,1) = (0.0d0,0.0d0);  AB4(3,1) = (4.0d0,1.0d0);  AB4(4,1) = (-1.0d0,0.0d0)
  AB4(2,2) = (-1.0d0,0.0d0); AB4(3,2) = (4.0d0,1.0d0);  AB4(4,2) = (-1.0d0,0.0d0)
  AB4(2,3) = (-1.0d0,0.0d0); AB4(3,3) = (4.0d0,1.0d0);  AB4(4,3) = (-1.0d0,0.0d0)
  AB4(2,4) = (-1.0d0,0.0d0); AB4(3,4) = (4.0d0,1.0d0);  AB4(4,4) = (0.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 2.0d0)
  B(2,1) = (3.0d0, 4.0d0)
  B(3,1) = (5.0d0, 6.0d0)
  B(4,1) = (7.0d0, 8.0d0)
  ipiv = 0
  call ZGBSV(4, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('complex_rhs')
  call print_array('x', B_r, 8)
  call print_int('info', info)
  call end_test()

end program
