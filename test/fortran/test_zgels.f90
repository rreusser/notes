program test_zgels
  use test_utils
  implicit none

  double precision :: a_r(400), b_r(400)
  complex*16 :: a(200), b(200), work(5000)
  equivalence (a, a_r)
  equivalence (b, b_r)
  integer :: info, lwork, i

  lwork = 5000

  ! Test 1: Overdetermined 4x2 system, TRANS='N'
  ! Least squares: minimize || b - A*x ||
  ! A = [1+i 1+2i; 2+i 2+i; 3 3+i; 1+3i 1] (col-major), b = [1+i; 2; 3+i; 1]
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ! Column 1 (A(1:4,1))
  a(1) = (1.0d0, 1.0d0); a(2) = (2.0d0, 1.0d0); a(3) = (3.0d0, 0.0d0); a(4) = (1.0d0, 3.0d0)
  ! Column 2 (A(1:4,2))
  a(5) = (1.0d0, 2.0d0); a(6) = (2.0d0, 1.0d0); a(7) = (3.0d0, 1.0d0); a(8) = (1.0d0, 0.0d0)
  ! b
  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, 0.0d0); b(3) = (3.0d0, 1.0d0); b(4) = (1.0d0, 0.0d0)
  call ZGELS('N', 4, 2, 1, a, 4, b, 4, work, lwork, info)
  call begin_test('overdetermined_4x2')
  call print_array('x', b_r, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: Underdetermined 2x4 system, TRANS='N'
  ! Minimum norm: min || x || s.t. A*x = b
  ! A = [1+i 2 3+i 1; 0 2+i 1 3+i] (col-major), b = [10+2i; 5+i]
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ! Column 1
  a(1) = (1.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0)
  ! Column 2
  a(3) = (2.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0)
  ! Column 3
  a(5) = (3.0d0, 1.0d0); a(6) = (1.0d0, 0.0d0)
  ! Column 4
  a(7) = (1.0d0, 0.0d0); a(8) = (3.0d0, 1.0d0)
  b(1) = (10.0d0, 2.0d0); b(2) = (5.0d0, 1.0d0)
  call ZGELS('N', 2, 4, 1, a, 2, b, 4, work, lwork, info)
  call begin_test('underdetermined_2x4')
  call print_array('x', b_r, 8)
  call print_int('info', info)
  call end_test()

  ! Test 3: Square 3x3 system, TRANS='N'
  ! Diagonally dominant: A = [5+i i 0; i 5+i i; 0 i 5+i], b = [5+2i; 5+3i; 5+2i]
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (5.0d0, 1.0d0); a(2) = (0.0d0, 1.0d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (0.0d0, 1.0d0); a(5) = (5.0d0, 1.0d0); a(6) = (0.0d0, 1.0d0)
  a(7) = (0.0d0, 0.0d0); a(8) = (0.0d0, 1.0d0); a(9) = (5.0d0, 1.0d0)
  b(1) = (5.0d0, 2.0d0); b(2) = (5.0d0, 3.0d0); b(3) = (5.0d0, 2.0d0)
  call ZGELS('N', 3, 3, 1, a, 3, b, 3, work, lwork, info)
  call begin_test('square_3x3')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

  ! Test 4: TRANS='C' with M >= N (conjugate transpose, min norm via A^H)
  ! A = [2+i 1; 1 2+i; 1+i 1+i; 0 1] (4x2), TRANS='C'
  ! A^H is 2x4, underdetermined: min || x || s.t. A^H * x = b
  ! b has 2 entries
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (1.0d0, 0.0d0); a(3) = (1.0d0, 1.0d0); a(4) = (0.0d0, 0.0d0)
  a(5) = (1.0d0, 0.0d0); a(6) = (2.0d0, 1.0d0); a(7) = (1.0d0, 1.0d0); a(8) = (1.0d0, 0.0d0)
  b(1) = (3.0d0, 1.0d0); b(2) = (2.0d0, 0.0d0)
  call ZGELS('C', 4, 2, 1, a, 4, b, 4, work, lwork, info)
  call begin_test('conjtrans_mge_n_minnorm')
  call print_array('x', b_r, 8)
  call print_int('info', info)
  call end_test()

  ! Test 5: TRANS='C' with M < N (conjugate transpose, least squares)
  ! A = [1+i 2 3; 1 2+i 1+i] (2x3), TRANS='C'
  ! A^H is 3x2, overdetermined: min || b - A^H * x ||
  ! b has 3 entries
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0); a(2) = (1.0d0, 0.0d0)
  a(3) = (2.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0)
  a(5) = (3.0d0, 0.0d0); a(6) = (1.0d0, 1.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 0.0d0)
  call ZGELS('C', 2, 3, 1, a, 2, b, 3, work, lwork, info)
  call begin_test('conjtrans_mlt_n_ls')
  call print_array('x', b_r, 4)
  call print_int('info', info)
  call end_test()

  ! Test 6: Multiple RHS, overdetermined 4x2
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 0.0d0); a(2) = (0.0d0, 1.0d0); a(3) = (1.0d0, 0.0d0); a(4) = (1.0d0, 1.0d0)
  a(5) = (1.0d0, 0.0d0); a(6) = (2.0d0, 0.0d0); a(7) = (1.0d0, 1.0d0); a(8) = (0.0d0, 1.0d0)
  ! RHS 1
  b(1) = (3.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (2.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0)
  ! RHS 2 (offset by LDB=4)
  b(5) = (5.0d0, 1.0d0); b(6) = (4.0d0, 0.0d0); b(7) = (3.0d0, 1.0d0); b(8) = (2.0d0, 0.0d0)
  call ZGELS('N', 4, 2, 2, a, 4, b, 4, work, lwork, info)
  call begin_test('multi_rhs_overdetermined')
  call print_array('x1', b_r, 4)
  call print_array('x2', b_r(9:12), 4)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0 quick return
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0)
  call ZGELS('N', 3, 0, 1, a, 3, b, 3, work, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: M=0 quick return
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  call ZGELS('N', 0, 3, 1, a, 1, b, 3, work, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: NRHS=0 quick return
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (0.0d0, 0.0d0); a(4) = (1.0d0, 0.0d0)
  call ZGELS('N', 2, 2, 0, a, 2, b, 2, work, lwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 10: Larger overdetermined 6x3 with well-conditioned matrix
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  ! Column 1 (diag dominant)
  a(1) = (10.0d0, 1.0d0); a(2) = (1.0d0, 0.0d0); a(3) = (1.0d0, 0.0d0)
  a(4) = (0.0d0, 1.0d0); a(5) = (1.0d0, 0.0d0); a(6) = (0.0d0, 0.0d0)
  ! Column 2
  a(7) = (1.0d0, 0.0d0); a(8) = (10.0d0, 1.0d0); a(9) = (1.0d0, 0.0d0)
  a(10) = (1.0d0, 0.0d0); a(11) = (0.0d0, 1.0d0); a(12) = (0.0d0, 0.0d0)
  ! Column 3
  a(13) = (1.0d0, 0.0d0); a(14) = (1.0d0, 0.0d0); a(15) = (10.0d0, 1.0d0)
  a(16) = (1.0d0, 0.0d0); a(17) = (1.0d0, 0.0d0); a(18) = (0.0d0, 1.0d0)
  ! b = A * [1+0i; 2+0i; 3+0i]
  b(1) = (15.0d0, 1.0d0); b(2) = (24.0d0, 2.0d0); b(3) = (33.0d0, 3.0d0)
  b(4) = (5.0d0, 1.0d0); b(5) = (5.0d0, 2.0d0); b(6) = (0.0d0, 3.0d0)
  call ZGELS('N', 6, 3, 1, a, 6, b, 6, work, lwork, info)
  call begin_test('overdetermined_6x3')
  call print_array('x', b_r, 6)
  call print_int('info', info)
  call end_test()

end program
