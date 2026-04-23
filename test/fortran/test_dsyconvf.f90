program test_dsyconvf
  use test_utils
  implicit none

  double precision :: A(25), A_save(25), E(5), WORK(25)
  integer :: IPIV(5), INFO, N, LDA

  N = 5
  LDA = 5

  ! =====================================================
  ! Test 1: Upper, Convert (WAY='C') — 5x5 with 1x1 pivots
  ! =====================================================
  A = 0.0d0
  ! Symmetric diagonally dominant 5x5 => well-conditioned => 1x1 pivots
  ! Row 1: 5  2  1  0  1
  ! Row 2: 2  6  3  1  0
  ! Row 3: 1  3  7  2  1
  ! Row 4: 0  1  2  8  3
  ! Row 5: 1  0  1  3  9
  A(1)  = 5.0d0; A(2)  = 2.0d0; A(3)  = 1.0d0; A(4)  = 0.0d0; A(5)  = 1.0d0
  A(6)  = 2.0d0; A(7)  = 6.0d0; A(8)  = 3.0d0; A(9)  = 1.0d0; A(10) = 0.0d0
  A(11) = 1.0d0; A(12) = 3.0d0; A(13) = 7.0d0; A(14) = 2.0d0; A(15) = 1.0d0
  A(16) = 0.0d0; A(17) = 1.0d0; A(18) = 2.0d0; A(19) = 8.0d0; A(20) = 3.0d0
  A(21) = 1.0d0; A(22) = 0.0d0; A(23) = 1.0d0; A(24) = 3.0d0; A(25) = 9.0d0

  call DSYTRF('U', N, A, LDA, IPIV, WORK, 25, INFO)
  A_save = A

  call begin_test('upper_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONVF('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call print_int_array('ipiv_converted', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 2: Upper, Revert — round-trip back to factored form
  ! =====================================================
  call DSYCONVF('U', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('upper_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call print_int_array('ipiv_reverted', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 3: Lower, Convert
  ! =====================================================
  A = 0.0d0
  A(1)  = 5.0d0; A(2)  = 2.0d0; A(3)  = 1.0d0; A(4)  = 0.0d0; A(5)  = 1.0d0
  A(6)  = 2.0d0; A(7)  = 6.0d0; A(8)  = 3.0d0; A(9)  = 1.0d0; A(10) = 0.0d0
  A(11) = 1.0d0; A(12) = 3.0d0; A(13) = 7.0d0; A(14) = 2.0d0; A(15) = 1.0d0
  A(16) = 0.0d0; A(17) = 1.0d0; A(18) = 2.0d0; A(19) = 8.0d0; A(20) = 3.0d0
  A(21) = 1.0d0; A(22) = 0.0d0; A(23) = 1.0d0; A(24) = 3.0d0; A(25) = 9.0d0

  call DSYTRF('L', N, A, LDA, IPIV, WORK, 25, INFO)
  A_save = A

  call begin_test('lower_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONVF('L', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call print_int_array('ipiv_converted', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 4: Lower, Revert
  ! =====================================================
  call DSYCONVF('L', 'R', N, A, LDA, E, IPIV, INFO)

  call begin_test('lower_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call print_int_array('ipiv_reverted', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 5: N=0 quick return (upper convert)
  ! =====================================================
  call begin_test('n0_upper_convert')
  call DSYCONVF('U', 'C', 0, A, LDA, E, IPIV, INFO)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 6: N=0 quick return (lower revert)
  ! =====================================================
  call begin_test('n0_lower_revert')
  call DSYCONVF('L', 'R', 0, A, LDA, E, IPIV, INFO)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 7: Upper, Convert — 2x2 pivots from small diagonals
  ! =====================================================
  A = 0.0d0
  A(1)  = 1.0d-10; A(2)  = 3.0d0;   A(3)  = 2.0d0;   A(4)  = 1.0d0;   A(5)  = 0.5d0
  A(6)  = 3.0d0;   A(7)  = 1.0d-10; A(8)  = 4.0d0;   A(9)  = 2.0d0;   A(10) = 1.0d0
  A(11) = 2.0d0;   A(12) = 4.0d0;   A(13) = 1.0d-10; A(14) = 3.0d0;   A(15) = 2.0d0
  A(16) = 1.0d0;   A(17) = 2.0d0;   A(18) = 3.0d0;   A(19) = 1.0d-10; A(20) = 4.0d0
  A(21) = 0.5d0;   A(22) = 1.0d0;   A(23) = 2.0d0;   A(24) = 4.0d0;   A(25) = 1.0d-10

  call DSYTRF('U', N, A, LDA, IPIV, WORK, 25, INFO)
  A_save = A

  call begin_test('upper_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONVF('U', 'C', N, A, LDA, E, IPIV, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call print_int_array('ipiv_converted', IPIV, N)
  call end_test()

  call DSYCONVF('U', 'R', N, A, LDA, E, IPIV, INFO)
  call begin_test('upper_2x2_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call print_int_array('ipiv_reverted', IPIV, N)
  call end_test()

  ! =====================================================
  ! Test 8: Lower, Convert — 2x2 pivot at rows (1,2) only
  ! A 2x2 pivot block at rows (i,i+1) is only safe for Lower Revert
  ! when i=1 (the I.GT.1 guard then skips the out-of-range
  ! phantom-row swap that the reference dsyconvf triggers for
  ! blocks located deeper in the matrix). Using a small matrix
  ! keeps the test focused on this one safe configuration.
  ! =====================================================
  block
    double precision :: B(9), B_save(9), Eb(3), Wb(9)
    integer :: Pb(3), Nb, LDAb
    Nb = 3
    LDAb = 3
    B = 0.0d0
    ! Small diag at top forces 2x2 pivot block at (1,2)
    B(1) = 1.0d-10; B(2) = 5.0d0;   B(3) = 1.0d0
    B(4) = 5.0d0;   B(5) = 1.0d-10; B(6) = 2.0d0
    B(7) = 1.0d0;   B(8) = 2.0d0;   B(9) = 9.0d0

    call DSYTRF('L', Nb, B, LDAb, Pb, Wb, 9, INFO)
    B_save = B

    call begin_test('lower_2x2_convert')
    call print_int('info_trf', INFO)
    call print_int_array('ipiv_trf', Pb, Nb)
    call print_array('a_factored', B, Nb*Nb)

    Eb = 0.0d0
    call DSYCONVF('L', 'C', Nb, B, LDAb, Eb, Pb, INFO)
    call print_int('info_conv', INFO)
    call print_array('a_converted', B, Nb*Nb)
    call print_array('e', Eb, Nb)
    call print_int_array('ipiv_converted', Pb, Nb)
    call end_test()

    call DSYCONVF('L', 'R', Nb, B, LDAb, Eb, Pb, INFO)
    call begin_test('lower_2x2_revert')
    call print_int('info_rev', INFO)
    call print_array('a_reverted', B, Nb*Nb)
    call print_int_array('ipiv_reverted', Pb, Nb)
    call end_test()
  end block

end program
