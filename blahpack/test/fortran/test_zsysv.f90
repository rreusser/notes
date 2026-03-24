program test_zsysv
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), B(NMAX, 4), WORK(NMAX*64)
  double precision :: A_r(2*NMAX*NMAX), B_r(2*NMAX*4)
  equivalence (A, A_r)
  equivalence (B, B_r)
  integer :: IPIV(NMAX), INFO, LWORK

  LWORK = NMAX * 64

  ! Test 1: Upper 4x4 complex symmetric, single RHS
  ! A (symmetric, upper triangle stored):
  ! [ (2,1)   (1,2)   (3,-1)  (0.5,0.5) ]
  ! [ (1,2)   (5,-1)  (2,1)   (1,-2)    ]
  ! [ (3,-1)  (2,1)   (4,2)   (3,0)     ]
  ! [ (0.5,0.5) (1,-2) (3,0)  (6,-3)    ]
  ! b = A * [1; 2; 3; 4] computed by hand
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (4.0d0, 2.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, -3.0d0)
  ! b = A * [1; 2; 3; 4]
  ! row 1: (2,1)*1 + (1,2)*2 + (3,-1)*3 + (0.5,0.5)*4 = (2+2+9+2, 1+4-3+2) = (15, 4)
  ! row 2: (1,2)*1 + (5,-1)*2 + (2,1)*3 + (1,-2)*4 = (1+10+6+4, 2-2+3-8) = (21, -5)
  ! row 3: (3,-1)*1 + (2,1)*2 + (4,2)*3 + (3,0)*4 = (3+4+12+12, -1+2+6+0) = (31, 7)
  ! row 4: (0.5,0.5)*1 + (1,-2)*2 + (3,0)*3 + (6,-3)*4 = (0.5+2+9+24, 0.5-4+0-12) = (35.5, -15.5)
  B(1,1) = (15.0d0, 4.0d0)
  B(2,1) = (21.0d0, -5.0d0)
  B(3,1) = (31.0d0, 7.0d0)
  B(4,1) = (35.5d0, -15.5d0)
  call ZSYSV('U', 4, 1, A, NMAX, IPIV, B, NMAX, WORK, LWORK, INFO)
  call begin_test('upper_4x4')
  call print_array('x', B_r, 2*4)
  call print_int_array('ipiv', IPIV, 4)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*4)
  call end_test()

  ! Test 2: Lower 4x4, single RHS (same matrix stored in lower triangle)
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(3,1) = (3.0d0, -1.0d0)
  A(3,2) = (2.0d0, 1.0d0)
  A(3,3) = (4.0d0, 2.0d0)
  A(4,1) = (0.5d0, 0.5d0)
  A(4,2) = (1.0d0, -2.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, -3.0d0)
  B(1,1) = (15.0d0, 4.0d0)
  B(2,1) = (21.0d0, -5.0d0)
  B(3,1) = (31.0d0, 7.0d0)
  B(4,1) = (35.5d0, -15.5d0)
  call ZSYSV('L', 4, 1, A, NMAX, IPIV, B, NMAX, WORK, LWORK, INFO)
  call begin_test('lower_4x4')
  call print_array('x', B_r, 2*4)
  call print_int_array('ipiv', IPIV, 4)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*4)
  call end_test()

  ! Test 3: Multiple RHS (NRHS=2), 2x2
  ! A = [(2,1) (1,0); (1,0) (3,-1)]
  ! b1 = A*[1;2] = [(4,1); (7,-1)]
  ! b2 = A*[3;1] = [(7,3); (6,-1)]
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 0.0d0)
  A(2,2) = (3.0d0, -1.0d0)
  B(1,1) = (4.0d0, 1.0d0)
  B(2,1) = (7.0d0, -1.0d0)
  B(1,2) = (7.0d0, 3.0d0)
  B(2,2) = (6.0d0, -1.0d0)
  call ZSYSV('U', 2, 2, A, NMAX, IPIV, B, NMAX, WORK, LWORK, INFO)
  call begin_test('multi_rhs')
  call print_array('x', B_r, 2*NMAX*2)
  call print_int_array('ipiv', IPIV, 2)
  call print_int('info', INFO)
  call end_test()

  ! Test 4: Singular matrix (INFO > 0)
  ! A = [(1,0) (1,0); (1,0) (1,0)] is singular
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.0d0)
  A(2,2) = (1.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 0.0d0)
  call ZSYSV('U', 2, 1, A, NMAX, IPIV, B, NMAX, WORK, LWORK, INFO)
  call begin_test('singular')
  call print_int('info', INFO)
  call end_test()

  ! Test 5: N=1
  ! A = [(3,1)], b = [(9,3)] => x = (9,3)/(3,1) = (3,0)
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 1.0d0)
  B(1,1) = (9.0d0, 3.0d0)
  call ZSYSV('U', 1, 1, A, NMAX, IPIV, B, NMAX, WORK, LWORK, INFO)
  call begin_test('n1')
  call print_array('x', B_r, 2)
  call print_int_array('ipiv', IPIV, 1)
  call print_int('info', INFO)
  call end_test()

  ! Test 6: Matrix with 2x2 pivots (zero diagonal forces pivoting)
  ! A = [(0,0) (1,1) (0,0) (0,0); (1,1) (0,0) (0,0) (0,0); (0,0) (0,0) (4,1) (1,0); (0,0) (0,0) (1,0) (4,-1)]
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(2,2) = (0.0d0, 0.0d0)
  A(3,3) = (4.0d0, 1.0d0)
  A(3,4) = (1.0d0, 0.0d0)
  A(4,4) = (4.0d0, -1.0d0)
  ! b = A * [1; 2; 3; 4]
  ! row 1: 0*1 + (1,1)*2 + 0 + 0 = (2, 2)
  ! row 2: (1,1)*1 + 0*2 + 0 + 0 = (1, 1)
  ! row 3: 0 + 0 + (4,1)*3 + (1,0)*4 = (16, 3)
  ! row 4: 0 + 0 + (1,0)*3 + (4,-1)*4 = (19, -4)
  B(1,1) = (2.0d0, 2.0d0)
  B(2,1) = (1.0d0, 1.0d0)
  B(3,1) = (16.0d0, 3.0d0)
  B(4,1) = (19.0d0, -4.0d0)
  call ZSYSV('U', 4, 1, A, NMAX, IPIV, B, NMAX, WORK, LWORK, INFO)
  call begin_test('pivot_2x2_upper')
  call print_array('x', B_r, 2*4)
  call print_int_array('ipiv', IPIV, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 7: Same 2x2 pivot matrix, lower
  A = (0.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(2,2) = (0.0d0, 0.0d0)
  A(3,3) = (4.0d0, 1.0d0)
  A(4,3) = (1.0d0, 0.0d0)
  A(4,4) = (4.0d0, -1.0d0)
  B(1,1) = (2.0d0, 2.0d0)
  B(2,1) = (1.0d0, 1.0d0)
  B(3,1) = (16.0d0, 3.0d0)
  B(4,1) = (19.0d0, -4.0d0)
  call ZSYSV('L', 4, 1, A, NMAX, IPIV, B, NMAX, WORK, LWORK, INFO)
  call begin_test('pivot_2x2_lower')
  call print_array('x', B_r, 2*4)
  call print_int_array('ipiv', IPIV, 4)
  call print_int('info', INFO)
  call end_test()

  ! Test 8: N=0 quick return
  call ZSYSV('U', 0, 1, A, NMAX, IPIV, B, NMAX, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

end program
