program test_dgels
  use test_utils
  implicit none

  double precision :: a(200), b(200), work(2000)
  integer :: info, lwork, i

  lwork = 2000

  ! Test 1: Overdetermined 4x2 system, TRANS='N'
  ! Least squares: minimize || b - A*x ||
  ! A = [1 1; 1 2; 1 3; 1 4] (col-major), b = [1; 2; 4; 3]
  a = 0.0d0; b = 0.0d0
  ! Column 1
  a(1) = 1.0d0; a(2) = 1.0d0; a(3) = 1.0d0; a(4) = 1.0d0
  ! Column 2
  a(5) = 1.0d0; a(6) = 2.0d0; a(7) = 3.0d0; a(8) = 4.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 4.0d0; b(4) = 3.0d0
  call dgels('N', 4, 2, 1, a, 4, b, 4, work, lwork, info)
  call begin_test('overdetermined_4x2')
  call print_array('x', b, 2)
  call print_int('info', info)
  call end_test()

  ! Test 2: Underdetermined 2x4 system, TRANS='N'
  ! Minimum norm: min || x || s.t. A*x = b
  ! A = [1 2 3 4; 5 6 7 8] (col-major), b = [10; 26]
  a = 0.0d0; b = 0.0d0
  ! Column 1
  a(1) = 1.0d0; a(2) = 5.0d0
  ! Column 2
  a(3) = 2.0d0; a(4) = 6.0d0
  ! Column 3
  a(5) = 3.0d0; a(6) = 7.0d0
  ! Column 4
  a(7) = 4.0d0; a(8) = 8.0d0
  b(1) = 10.0d0; b(2) = 26.0d0
  call dgels('N', 2, 4, 1, a, 2, b, 4, work, lwork, info)
  call begin_test('underdetermined_2x4')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: Square 3x3 system, TRANS='N'
  ! Diagonally dominant: A = [5 1 1; 1 5 1; 1 1 5], b = [7; 7; 7]
  ! Solution: x = [1; 1; 1]
  a = 0.0d0; b = 0.0d0
  a(1) = 5.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 5.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 5.0d0
  b(1) = 7.0d0; b(2) = 7.0d0; b(3) = 7.0d0
  call dgels('N', 3, 3, 1, a, 3, b, 3, work, lwork, info)
  call begin_test('square_3x3')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: Overdetermined with TRANS='T' (2x4 becomes minimum norm via A^T)
  ! When TRANS='T' and M < N: least squares of A^T * x = b
  ! A = [1 2 3 4; 5 6 7 8] (2x4, col-major)
  ! This is the LS problem min || b - A^T * x || where A is 2x4, A^T is 4x2
  ! b has 4 entries
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(2) = 5.0d0
  a(3) = 2.0d0; a(4) = 6.0d0
  a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 4.0d0; a(8) = 8.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 4.0d0; b(4) = 3.0d0
  call dgels('T', 2, 4, 1, a, 2, b, 4, work, lwork, info)
  call begin_test('transpose_mlt_n_ls')
  call print_array('x', b, 2)
  call print_int('info', info)
  call end_test()

  ! Test 5: TRANS='T' with M >= N (overdetermined becomes min norm via A^T)
  ! A = [1 1; 1 2; 1 3; 1 4] (4x2), TRANS='T' -> min norm of A^T * x = b
  ! A^T is 2x4, so underdetermined: min || x || s.t. A^T * x = b
  ! b has 2 entries
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(2) = 1.0d0; a(3) = 1.0d0; a(4) = 1.0d0
  a(5) = 1.0d0; a(6) = 2.0d0; a(7) = 3.0d0; a(8) = 4.0d0
  b(1) = 10.0d0; b(2) = 30.0d0
  call dgels('T', 4, 2, 1, a, 4, b, 4, work, lwork, info)
  call begin_test('transpose_mge_n_minnorm')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 6: Multiple RHS, overdetermined 4x2
  ! A = [2 1; 0 2; 1 1; 1 0] (col-major), B has 2 columns
  a = 0.0d0; b = 0.0d0
  a(1) = 2.0d0; a(2) = 0.0d0; a(3) = 1.0d0; a(4) = 1.0d0
  a(5) = 1.0d0; a(6) = 2.0d0; a(7) = 1.0d0; a(8) = 0.0d0
  ! RHS 1
  b(1) = 3.0d0; b(2) = 2.0d0; b(3) = 2.0d0; b(4) = 1.0d0
  ! RHS 2 (offset by LDB=4)
  b(5) = 5.0d0; b(6) = 4.0d0; b(7) = 3.0d0; b(8) = 2.0d0
  call dgels('N', 4, 2, 2, a, 4, b, 4, work, lwork, info)
  call begin_test('multi_rhs_overdetermined')
  ! Print first N=2 elements of each RHS column
  call print_array('x1', b(1:2), 2)
  call print_array('x2', b(5:6), 2)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0 quick return
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0
  b(1) = 1.0d0
  call dgels('N', 3, 0, 1, a, 3, b, 3, work, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: M=0 quick return
  a = 0.0d0; b = 0.0d0
  call dgels('N', 0, 3, 1, a, 1, b, 3, work, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: NRHS=0 quick return
  a = 0.0d0; b = 0.0d0
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 1.0d0
  call dgels('N', 2, 2, 0, a, 2, b, 2, work, lwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 10: Larger overdetermined 6x3 with well-conditioned matrix
  a = 0.0d0; b = 0.0d0
  ! Column 1
  a(1) = 10.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 1.0d0; a(6) = 1.0d0
  ! Column 2
  a(7) = 1.0d0; a(8) = 10.0d0; a(9) = 1.0d0
  a(10) = 1.0d0; a(11) = 1.0d0; a(12) = 1.0d0
  ! Column 3
  a(13) = 1.0d0; a(14) = 1.0d0; a(15) = 10.0d0
  a(16) = 1.0d0; a(17) = 1.0d0; a(18) = 1.0d0
  ! b = A * [1; 2; 3] = [10+2+3; 1+20+3; 1+2+30; 1+2+3; 1+2+3; 1+2+3]
  b(1) = 15.0d0; b(2) = 24.0d0; b(3) = 33.0d0
  b(4) = 6.0d0; b(5) = 6.0d0; b(6) = 6.0d0
  call dgels('N', 6, 3, 1, a, 6, b, 6, work, lwork, info)
  call begin_test('overdetermined_6x3')
  call print_array('x', b, 3)
  call print_int('info', info)
  call end_test()

end program
