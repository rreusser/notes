program test_dsyconv
  use test_utils
  implicit none

  double precision :: A(16), A_save(16), E(4), WORK(16)
  integer :: IPIV(4), INFO, N, LDA
  integer :: i

  N = 4
  LDA = 4

  ! =====================================================
  ! Test 1: Upper, Convert (WAY='C')
  ! =====================================================
  ! Set up a symmetric 4x4 upper matrix and factor with dsytrf
  A = 0.0d0
  ! Column-major: A(i,j) = A((j-1)*LDA + i)
  ! Row 1: 4  2  1  0
  ! Row 2: 2  5  3  1
  ! Row 3: 1  3  6  2
  ! Row 4: 0  1  2  7
  ! (symmetric, diag dominant => well-conditioned)
  A(1) = 4.0d0;  A(2) = 2.0d0;  A(3) = 1.0d0;  A(4) = 0.0d0
  A(5) = 2.0d0;  A(6) = 5.0d0;  A(7) = 3.0d0;  A(8) = 1.0d0
  A(9) = 1.0d0;  A(10) = 3.0d0; A(11) = 6.0d0;  A(12) = 2.0d0
  A(13) = 0.0d0; A(14) = 1.0d0; A(15) = 2.0d0; A(16) = 7.0d0

  call DSYTRF('U', N, A, LDA, IPIV, WORK, 16, INFO)

  ! Save the factored matrix for revert test
  A_save = A

  call begin_test('upper_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  ! Now convert
  E = 0.0d0
  call DSYCONV('U', 'C', N, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call end_test()

  ! =====================================================
  ! Test 2: Upper, Revert (WAY='R')
  ! =====================================================
  ! Revert should undo the convert
  call DSYCONV('U', 'R', N, A, LDA, IPIV, E, INFO)

  call begin_test('upper_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call end_test()

  ! =====================================================
  ! Test 3: Lower, Convert (WAY='C')
  ! =====================================================
  A = 0.0d0
  A(1) = 4.0d0;  A(2) = 2.0d0;  A(3) = 1.0d0;  A(4) = 0.0d0
  A(5) = 2.0d0;  A(6) = 5.0d0;  A(7) = 3.0d0;  A(8) = 1.0d0
  A(9) = 1.0d0;  A(10) = 3.0d0; A(11) = 6.0d0;  A(12) = 2.0d0
  A(13) = 0.0d0; A(14) = 1.0d0; A(15) = 2.0d0; A(16) = 7.0d0

  call DSYTRF('L', N, A, LDA, IPIV, WORK, 16, INFO)

  A_save = A

  call begin_test('lower_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONV('L', 'C', N, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call end_test()

  ! =====================================================
  ! Test 4: Lower, Revert (WAY='R')
  ! =====================================================
  call DSYCONV('L', 'R', N, A, LDA, IPIV, E, INFO)

  call begin_test('lower_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call end_test()

  ! =====================================================
  ! Test 5: N=1 edge case (upper convert)
  ! =====================================================
  A(1) = 5.0d0
  IPIV(1) = 1
  E(1) = 0.0d0

  ! Factor 1x1 matrix
  call DSYTRF('U', 1, A, 1, IPIV, WORK, 16, INFO)

  call begin_test('n1_upper')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv', IPIV, 1)
  call print_array('a_factored', A, 1)

  call DSYCONV('U', 'C', 1, A, 1, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, 1)
  call print_array('e', E, 1)
  call end_test()

  ! =====================================================
  ! Test 6: N=1 edge case (lower convert)
  ! =====================================================
  A(1) = 3.0d0
  call DSYTRF('L', 1, A, 1, IPIV, WORK, 16, INFO)

  call begin_test('n1_lower')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv', IPIV, 1)
  call print_array('a_factored', A, 1)

  call DSYCONV('L', 'C', 1, A, 1, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, 1)
  call print_array('e', E, 1)
  call end_test()

  ! =====================================================
  ! Test 7: Force 2x2 pivots (upper) - use matrix that triggers them
  ! =====================================================
  A = 0.0d0
  ! Matrix with small diagonal elements to force 2x2 pivots:
  ! Row 1:  1e-10  3     2     1
  ! Row 2:  3      1e-10 4     2
  ! Row 3:  2      4     1e-10 3
  ! Row 4:  1      2     3     8
  A(1)  = 1.0d-10; A(2)  = 3.0d0;    A(3)  = 2.0d0;    A(4)  = 1.0d0
  A(5)  = 3.0d0;   A(6)  = 1.0d-10;  A(7)  = 4.0d0;    A(8)  = 2.0d0
  A(9)  = 2.0d0;   A(10) = 4.0d0;    A(11) = 1.0d-10;  A(12) = 3.0d0
  A(13) = 1.0d0;   A(14) = 2.0d0;    A(15) = 3.0d0;    A(16) = 8.0d0

  call DSYTRF('U', N, A, LDA, IPIV, WORK, 16, INFO)
  A_save = A

  call begin_test('upper_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONV('U', 'C', N, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call end_test()

  ! Revert
  call DSYCONV('U', 'R', N, A, LDA, IPIV, E, INFO)

  call begin_test('upper_2x2_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call end_test()

  ! =====================================================
  ! Test 8: Force 2x2 pivots (lower)
  ! =====================================================
  A = 0.0d0
  A(1)  = 1.0d-10; A(2)  = 3.0d0;    A(3)  = 2.0d0;    A(4)  = 1.0d0
  A(5)  = 3.0d0;   A(6)  = 1.0d-10;  A(7)  = 4.0d0;    A(8)  = 2.0d0
  A(9)  = 2.0d0;   A(10) = 4.0d0;    A(11) = 1.0d-10;  A(12) = 3.0d0
  A(13) = 1.0d0;   A(14) = 2.0d0;    A(15) = 3.0d0;    A(16) = 8.0d0

  call DSYTRF('L', N, A, LDA, IPIV, WORK, 16, INFO)
  A_save = A

  call begin_test('lower_2x2_convert')
  call print_int('info_trf', INFO)
  call print_int_array('ipiv_trf', IPIV, N)
  call print_array('a_factored', A, N*N)

  E = 0.0d0
  call DSYCONV('L', 'C', N, A, LDA, IPIV, E, INFO)
  call print_int('info_conv', INFO)
  call print_array('a_converted', A, N*N)
  call print_array('e', E, N)
  call end_test()

  ! Revert
  call DSYCONV('L', 'R', N, A, LDA, IPIV, E, INFO)

  call begin_test('lower_2x2_revert')
  call print_int('info_rev', INFO)
  call print_array('a_reverted', A, N*N)
  call end_test()

end program
