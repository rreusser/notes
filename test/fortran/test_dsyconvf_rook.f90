program test_dsyconvf_rook
  use test_utils
  implicit none

  double precision :: A(16), E(4), WORK(16)
  integer :: IPIV(4), INFO, N, LDA

  N = 4
  LDA = 4

  ! =====================================================
  ! Test 1: Upper, Convert
  ! =====================================================
  A = 0.0d0
  A(1)  = 1.0d-10; A(2)  = 3.0d0;    A(3)  = 2.0d0;    A(4)  = 1.0d0
  A(5)  = 3.0d0;   A(6)  = 1.0d-10;  A(7)  = 4.0d0;    A(8)  = 2.0d0
  A(9)  = 2.0d0;   A(10) = 4.0d0;    A(11) = 1.0d-10;  A(12) = 3.0d0
  A(13) = 1.0d0;   A(14) = 2.0d0;    A(15) = 3.0d0;    A(16) = 8.0d0

  call DSYTRF_ROOK('U', N, A, LDA, IPIV, WORK, 16, INFO)

  call begin_test('upper_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONVF_ROOK('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call end_test()

  ! =====================================================
  ! Test 2: Upper, Revert (round-trip)
  ! =====================================================
  call DSYCONVF_ROOK('U', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('upper_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call end_test()

  ! =====================================================
  ! Test 3: Lower, Convert
  ! =====================================================
  A = 0.0d0
  A(1)  = 1.0d-10; A(2)  = 3.0d0;    A(3)  = 2.0d0;    A(4)  = 1.0d0
  A(5)  = 3.0d0;   A(6)  = 1.0d-10;  A(7)  = 4.0d0;    A(8)  = 2.0d0
  A(9)  = 2.0d0;   A(10) = 4.0d0;    A(11) = 1.0d-10;  A(12) = 3.0d0
  A(13) = 1.0d0;   A(14) = 2.0d0;    A(15) = 3.0d0;    A(16) = 8.0d0

  call DSYTRF_ROOK('L', N, A, LDA, IPIV, WORK, 16, INFO)

  call begin_test('lower_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONVF_ROOK('L', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call end_test()

  ! =====================================================
  ! Test 4: Lower, Revert (round-trip)
  ! =====================================================
  call DSYCONVF_ROOK('L', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('lower_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call end_test()

  ! =====================================================
  ! Test 5: Upper, well-conditioned (likely all 1x1 pivots)
  ! =====================================================
  A = 0.0d0
  A(1) = 4.0d0;  A(2) = 2.0d0;  A(3) = 1.0d0;  A(4) = 0.0d0
  A(5) = 2.0d0;  A(6) = 5.0d0;  A(7) = 3.0d0;  A(8) = 1.0d0
  A(9) = 1.0d0;  A(10) = 3.0d0; A(11) = 6.0d0;  A(12) = 2.0d0
  A(13) = 0.0d0; A(14) = 1.0d0; A(15) = 2.0d0; A(16) = 7.0d0

  call DSYTRF_ROOK('U', N, A, LDA, IPIV, WORK, 16, INFO)

  call begin_test('upper_1x1_convert')
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONVF_ROOK('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call end_test()

  call DSYCONVF_ROOK('U', 'R', N, A, LDA, E, IPIV, INFO)
  call begin_test('upper_1x1_revert')
  call print_array('a_reverted', A, N*N)
  call end_test()

  ! =====================================================
  ! Test 6: Lower, well-conditioned
  ! =====================================================
  A = 0.0d0
  A(1) = 4.0d0;  A(2) = 2.0d0;  A(3) = 1.0d0;  A(4) = 0.0d0
  A(5) = 2.0d0;  A(6) = 5.0d0;  A(7) = 3.0d0;  A(8) = 1.0d0
  A(9) = 1.0d0;  A(10) = 3.0d0; A(11) = 6.0d0;  A(12) = 2.0d0
  A(13) = 0.0d0; A(14) = 1.0d0; A(15) = 2.0d0; A(16) = 7.0d0

  call DSYTRF_ROOK('L', N, A, LDA, IPIV, WORK, 16, INFO)

  call begin_test('lower_1x1_convert')
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONVF_ROOK('L', 'C', N, A, LDA, E, IPIV, INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call end_test()

  call DSYCONVF_ROOK('L', 'R', N, A, LDA, E, IPIV, INFO)
  call begin_test('lower_1x1_revert')
  call print_array('a_reverted', A, N*N)
  call end_test()

  ! =====================================================
  ! Test 7: N=1 upper
  ! =====================================================
  A(1) = 5.0d0
  IPIV(1) = 1
  E(1) = 99.0d0

  call DSYCONVF_ROOK('U', 'C', 1, A, 1, E, IPIV, INFO)
  call begin_test('n1_upper')
  call print_int('info', INFO)
  call print_array('a', A, 1)
  call print_array('e', E, 1)
  call end_test()

  ! =====================================================
  ! Test 8: N=1 lower
  ! =====================================================
  A(1) = 3.0d0
  IPIV(1) = 1
  E(1) = 99.0d0

  call DSYCONVF_ROOK('L', 'C', 1, A, 1, E, IPIV, INFO)
  call begin_test('n1_lower')
  call print_int('info', INFO)
  call print_array('a', A, 1)
  call print_array('e', E, 1)
  call end_test()

  ! =====================================================
  ! Test 9: N=0 quick return
  ! =====================================================
  call DSYCONVF_ROOK('U', 'C', 0, A, 1, E, IPIV, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

end program
