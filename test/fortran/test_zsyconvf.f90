program test_zsyconvf
  use test_utils
  implicit none

  ! Arrays sized for 4x4 complex matrix
  double precision :: A_r(32), E_r(8), WORK_r(64)
  complex*16 :: A(4,4), E(4), WORK(32)
  equivalence (A, A_r)
  equivalence (E, E_r)
  equivalence (WORK, WORK_r)

  integer :: IPIV(4), INFO, N, LDA

  N = 4
  LDA = 4

  ! =====================================================
  ! Test 1: Upper, Convert (1x1 pivots)
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.5d0);  A(2,2) = (5.0d0, 0.0d0)
  A(1,3) = (0.5d0, 0.2d0);  A(2,3) = (1.0d0, 0.3d0);  A(3,3) = (6.0d0, 0.0d0)
  A(1,4) = (0.1d0, 0.1d0);  A(2,4) = (0.5d0, 0.2d0);  A(3,4) = (1.0d0, 0.4d0)
  A(4,4) = (7.0d0, 0.0d0)

  call ZSYTRF('U', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('upper_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call print_int_array('ipiv_conv', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 2: Upper, Revert (round-trip from previous)
  ! =====================================================
  call ZSYCONVF('U', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('upper_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call print_int_array('ipiv_rev', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 3: Lower, Convert (1x1 pivots)
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, 0.5d0);  A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (0.5d0, 0.2d0);  A(3,2) = (1.0d0, 0.3d0);  A(3,3) = (6.0d0, 0.0d0)
  A(4,1) = (0.1d0, 0.1d0);  A(4,2) = (0.5d0, 0.2d0);  A(4,3) = (1.0d0, 0.4d0)
  A(4,4) = (7.0d0, 0.0d0)

  call ZSYTRF('L', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('lower_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF('L', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call print_int_array('ipiv_conv', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 4: Lower, Revert (round-trip)
  ! =====================================================
  call ZSYCONVF('L', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('lower_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call print_int_array('ipiv_rev', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 5: N=1 upper
  ! =====================================================
  A(1,1) = (5.0d0, 1.0d0)
  call ZSYTRF('U', 1, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('n1_upper')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv', IPIV, 1)
  call print_array('a_factored', A_r, 2)

  call ZSYCONVF('U', 'C', 1, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2)
  call print_array('e', E_r, 2)
  call end_test()

  ! =====================================================
  ! Test 6: N=1 lower
  ! =====================================================
  A(1,1) = (3.0d0, -2.0d0)
  call ZSYTRF('L', 1, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('n1_lower')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv', IPIV, 1)
  call print_array('a_factored', A_r, 2)

  call ZSYCONVF('L', 'C', 1, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2)
  call print_array('e', E_r, 2)
  call end_test()

  ! =====================================================
  ! Test 7: Upper, 2x2 pivots, convert
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-10, 0.0d0)
  A(1,2) = (3.0d0, 1.0d0);   A(2,2) = (1.0d-10, 0.0d0)
  A(1,3) = (2.0d0, 0.5d0);   A(2,3) = (4.0d0, 1.5d0);   A(3,3) = (1.0d-10, 0.0d0)
  A(1,4) = (1.0d0, 0.3d0);   A(2,4) = (2.0d0, 0.7d0);   A(3,4) = (3.0d0, 1.0d0)
  A(4,4) = (8.0d0, 0.0d0)

  call ZSYTRF('U', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('upper_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call print_int_array('ipiv_conv', IPIV, N)
  call end_test()

  ! Revert
  call ZSYCONVF('U', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('upper_2x2_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call print_int_array('ipiv_rev', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 8: Lower, 2x2 pivots, convert
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-10, 0.0d0)
  A(2,1) = (3.0d0, 1.0d0);   A(2,2) = (1.0d-10, 0.0d0)
  A(3,1) = (2.0d0, 0.5d0);   A(3,2) = (4.0d0, 1.5d0);   A(3,3) = (1.0d-10, 0.0d0)
  A(4,1) = (1.0d0, 0.3d0);   A(4,2) = (2.0d0, 0.7d0);   A(4,3) = (3.0d0, 1.0d0)
  A(4,4) = (8.0d0, 0.0d0)

  call ZSYTRF('L', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('lower_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF('L', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call print_int_array('ipiv_conv', IPIV, N)
  call end_test()

  ! Revert
  call ZSYCONVF('L', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('lower_2x2_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call print_int_array('ipiv_rev', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 9: Upper, Convert with actual row swaps (non-identity 1x1 pivots)
  ! =====================================================
  A = (0.0d0, 0.0d0)
  ! Upper walks from col N down; put the largest off-diagonal in col 4
  ! to force ZSYTRF to pivot.
  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (0.1d0, 0.0d0);  A(2,2) = (1.0d0, 0.0d0)
  A(1,3) = (0.1d0, 0.0d0);  A(2,3) = (0.1d0, 0.0d0);  A(3,3) = (1.0d0, 0.0d0)
  A(1,4) = (9.0d0, 0.0d0);  A(2,4) = (0.1d0, 0.0d0);  A(3,4) = (0.1d0, 0.0d0)
  A(4,4) = (0.001d0, 0.0d0)

  call ZSYTRF('U', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('upper_swap_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call print_int_array('ipiv_conv', IPIV, N)
  call end_test()

  call ZSYCONVF('U', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('upper_swap_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call print_int_array('ipiv_rev', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 9b: Upper, 1x1 swap
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-3, 0.0d0)
  A(1,2) = (0.1d0, 0.0d0);  A(2,2) = (5.0d0, 0.0d0)
  A(1,3) = (0.2d0, 0.0d0);  A(2,3) = (0.3d0, 0.0d0);  A(3,3) = (6.0d0, 0.0d0)
  A(1,4) = (0.4d0, 0.0d0);  A(2,4) = (0.5d0, 0.0d0);  A(3,4) = (0.6d0, 0.0d0)
  A(4,4) = (7.0d0, 0.0d0)

  call ZSYTRF('U', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('upper_1x1_swap_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call print_int_array('ipiv_conv', IPIV, N)
  call end_test()

  call ZSYCONVF('U', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('upper_1x1_swap_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call print_int_array('ipiv_rev', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 10: Lower, with row swaps
  ! =====================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d-3, 0.0d0)
  A(2,1) = (2.0d0, 0.5d0);  A(2,2) = (1.0d-3, 0.0d0)
  A(3,1) = (3.0d0, 0.2d0);  A(3,2) = (1.5d0, 0.3d0);  A(3,3) = (5.0d0, 0.0d0)
  A(4,1) = (2.5d0, 0.1d0);  A(4,2) = (0.5d0, 0.2d0);  A(4,3) = (1.0d0, 0.4d0)
  A(4,4) = (6.0d0, 0.0d0)

  call ZSYTRF('L', N, A, LDA, IPIV, WORK, 32, INFO)

  call begin_test('lower_swap_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A_r, 2*N*N)

  E = (0.0d0, 0.0d0)
  call ZSYCONVF('L', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A_r, 2*N*N)
  call print_array('e', E_r, 2*N)
  call print_int_array('ipiv_conv', IPIV, N)
  call end_test()

  call ZSYCONVF('L', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('lower_swap_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A_r, 2*N*N)
  call print_int_array('ipiv_rev', IPIV, N)
  call end_test()

end program
