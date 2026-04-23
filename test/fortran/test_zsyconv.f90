program test_zsyconv
  use test_utils
  implicit none

  ! Arrays sized for 4x4 complex matrix
  ! A is 4x4 complex => 16 complex elements => 32 doubles
  double precision :: A_r(32), A_save_r(32), E_r(8), WORK_r(64)
  complex*16 :: A(4,4), A_save(4,4), E(4), WORK(32)
  equivalence (A, A_r)
  equivalence (A_save, A_save_r)
  equivalence (E, E_r)
  equivalence (WORK, WORK_r)

  integer :: IPIV(4), INFO, N, LDA
  integer :: i

  N = 4
  LDA = 4

  ! =====================================================
  ! Test 1: Upper, Convert (WAY='C') - diag dominant, all 1x1 pivots
  ! =====================================================
  A = (0.0d0, 0.0d0)
  ! Symmetric (not Hermitian) complex matrix, diag dominant
  ! A(i,j) = A(j,i) (symmetric, NOT conjugate-symmetric)
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.5d0);  A(2,2) = (5.0d0, 0.0d0)
  A(1,3) = (0.5d0, 0.2d0);  A(2,3) = (1.0d0, 0.3d0);  A(3,3) = (6.0d0, 0.0d0)
  A(1,4) = (0.1d0, 0.1d0);  A(2,4) = (0.5d0, 0.2d0);  A(3,4) = (1.0d0, 0.4d0)
  A(4,4) = (7.0d0, 0.0d0)

  call ZSYTRF('U', N, A, LDA, IPIV, WORK, 32, INFO)

  A_save = A

  call begin_test('upper_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONV('U', 'C', N, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call end_test()

  ! =====================================================
  ! Test 2: Upper, Revert (WAY='R')
  ! =====================================================
  call ZSYCONV('U', 'R', N, A, LDA, IPIV, E, INFO)

  call begin_test('upper_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 3: Lower, Convert (WAY='C')
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, 0.5d0);  A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (0.5d0, 0.2d0);  A(3,2) = (1.0d0, 0.3d0);  A(3,3) = (6.0d0, 0.0d0)
  A(4,1) = (0.1d0, 0.1d0);  A(4,2) = (0.5d0, 0.2d0);  A(4,3) = (1.0d0, 0.4d0)
  A(4,4) = (7.0d0, 0.0d0)

  call ZSYTRF('L', N, A, LDA, IPIV, WORK, 32, INFO)

  A_save = A

  call begin_test('lower_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONV('L', 'C', N, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call end_test()

  ! =====================================================
  ! Test 4: Lower, Revert (WAY='R')
  ! =====================================================
  call ZSYCONV('L', 'R', N, A, LDA, IPIV, E, INFO)

  call begin_test('lower_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 5: N=1 edge case (upper convert)
  ! =====================================================
  A(1,1) = (5.0d0, 1.0d0)
  call ZSYTRF('U', 1, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('n1_upper')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv', IPIV, 1)
  call print_array('a_factored', A_r, 2)

  call ZSYCONV('U', 'C', 1, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2)
  call print_array('e', E_r, 2)
  call end_test()

  ! =====================================================
  ! Test 6: N=1 edge case (lower convert)
  ! =====================================================
  A(1,1) = (3.0d0, -2.0d0)
  call ZSYTRF('L', 1, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('n1_lower')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv', IPIV, 1)
  call print_array('a_factored', A_r, 2)

  call ZSYCONV('L', 'C', 1, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2)
  call print_array('e', E_r, 2)
  call end_test()

  ! =====================================================
  ! Test 7: Force 2x2 pivots (upper)
  ! =====================================================
  A = (0.0d0, 0.0d0)
  ! Near-zero diagonals force 2x2 pivots
  A(1,1) = (1.0d-10, 0.0d0)
  A(1,2) = (3.0d0, 1.0d0);   A(2,2) = (1.0d-10, 0.0d0)
  A(1,3) = (2.0d0, 0.5d0);   A(2,3) = (4.0d0, 1.5d0);   A(3,3) = (1.0d-10, 0.0d0)
  A(1,4) = (1.0d0, 0.3d0);   A(2,4) = (2.0d0, 0.7d0);   A(3,4) = (3.0d0, 1.0d0)
  A(4,4) = (8.0d0, 0.0d0)

  call ZSYTRF('U', N, A, LDA, IPIV, WORK, 32, INFO)
  A_save = A

  call begin_test('upper_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONV('U', 'C', N, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call end_test()

  ! Revert
  call ZSYCONV('U', 'R', N, A, LDA, IPIV, E, INFO)

  call begin_test('upper_2x2_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 8: Force 2x2 pivots (lower)
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-10, 0.0d0)
  A(2,1) = (3.0d0, 1.0d0);   A(2,2) = (1.0d-10, 0.0d0)
  A(3,1) = (2.0d0, 0.5d0);   A(3,2) = (4.0d0, 1.5d0);   A(3,3) = (1.0d-10, 0.0d0)
  A(4,1) = (1.0d0, 0.3d0);   A(4,2) = (2.0d0, 0.7d0);   A(4,3) = (3.0d0, 1.0d0)
  A(4,4) = (8.0d0, 0.0d0)

  call ZSYTRF('L', N, A, LDA, IPIV, WORK, 32, INFO)
  A_save = A

  call begin_test('lower_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONV('L', 'C', N, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call end_test()

  ! Revert
  call ZSYCONV('L', 'R', N, A, LDA, IPIV, E, INFO)

  call begin_test('lower_2x2_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call end_test()

end program
