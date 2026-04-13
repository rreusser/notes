program test_zsyconvf_rook
  use test_utils
  implicit none

  ! 4x4 complex matrix: 16 complex = 32 doubles
  double precision :: A_r(32), E_r(8), WORK_r(64)
  complex*16 :: A(4,4), E(4), WORK(32)
  equivalence (A, A_r)
  equivalence (E, E_r)
  equivalence (WORK, WORK_r)

  integer :: IPIV(4), INFO, N, LDA

  N = 4
  LDA = 4

  ! =====================================================
  ! Test 1: Upper, Convert - 1x1 pivots (diag dominant)
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.5d0);  A(2,2) = (5.0d0, 0.0d0)
  A(1,3) = (0.5d0, 0.2d0);  A(2,3) = (1.0d0, 0.3d0);  A(3,3) = (6.0d0, 0.0d0)
  A(1,4) = (0.1d0, 0.1d0);  A(2,4) = (0.5d0, 0.2d0);  A(3,4) = (1.0d0, 0.4d0)
  A(4,4) = (7.0d0, 0.0d0)

  call ZSYTRF_ROOK('U', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('upper_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF_ROOK('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call end_test()

  ! =====================================================
  ! Test 2: Upper, Revert
  ! =====================================================
  call ZSYCONVF_ROOK('U', 'R', N, A, LDA, E, IPIV, INFO)
  call begin_test('upper_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 3: Lower, Convert - 1x1 pivots
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, 0.5d0);  A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (0.5d0, 0.2d0);  A(3,2) = (1.0d0, 0.3d0);  A(3,3) = (6.0d0, 0.0d0)
  A(4,1) = (0.1d0, 0.1d0);  A(4,2) = (0.5d0, 0.2d0);  A(4,3) = (1.0d0, 0.4d0)
  A(4,4) = (7.0d0, 0.0d0)

  call ZSYTRF_ROOK('L', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('lower_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF_ROOK('L', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call end_test()

  ! =====================================================
  ! Test 4: Lower, Revert
  ! =====================================================
  call ZSYCONVF_ROOK('L', 'R', N, A, LDA, E, IPIV, INFO)
  call begin_test('lower_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 5: N=1 edge case (upper)
  ! =====================================================
  A(1,1) = (5.0d0, 1.0d0)
  call ZSYTRF_ROOK('U', 1, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('n1_upper')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv', IPIV, 1)
  call print_array('a_factored', A_r, 2)

  call ZSYCONVF_ROOK('U', 'C', 1, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2)
  call print_array('e', E_r, 2)
  call end_test()

  ! =====================================================
  ! Test 6: N=1 edge case (lower)
  ! =====================================================
  A(1,1) = (3.0d0, -2.0d0)
  call ZSYTRF_ROOK('L', 1, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('n1_lower')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv', IPIV, 1)
  call print_array('a_factored', A_r, 2)

  call ZSYCONVF_ROOK('L', 'C', 1, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2)
  call print_array('e', E_r, 2)
  call end_test()

  ! =====================================================
  ! Test 7: Force 2x2 pivots (upper)
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-10, 0.0d0)
  A(1,2) = (3.0d0, 1.0d0);   A(2,2) = (1.0d-10, 0.0d0)
  A(1,3) = (2.0d0, 0.5d0);   A(2,3) = (4.0d0, 1.5d0);   A(3,3) = (1.0d-10, 0.0d0)
  A(1,4) = (1.0d0, 0.3d0);   A(2,4) = (2.0d0, 0.7d0);   A(3,4) = (3.0d0, 1.0d0)
  A(4,4) = (8.0d0, 0.0d0)

  call ZSYTRF_ROOK('U', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('upper_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF_ROOK('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call end_test()

  call ZSYCONVF_ROOK('U', 'R', N, A, LDA, E, IPIV, INFO)
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

  call ZSYTRF_ROOK('L', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('lower_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF_ROOK('L', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call end_test()

  call ZSYCONVF_ROOK('L', 'R', N, A, LDA, E, IPIV, INFO)
  call begin_test('lower_2x2_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call end_test()

end program
