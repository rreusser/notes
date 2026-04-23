program test_dla_syrpvgrw
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  double precision :: A(NMAX, NMAX), AF(NMAX, NMAX)
  double precision :: WORK(2*NMAX), WORKF(NMAX)
  integer :: IPIV(NMAX), INFO, N, I, J
  double precision :: DLA_SYRPVGRW, RPVGRW
  external :: DSYTRF

  ! =========================================================
  ! Test 1: Upper triangle, 4x4 well-conditioned, INFO=0
  ! =========================================================
  N = 4
  ! Symmetric diagonally dominant matrix (upper storage)
  A = 0.0D+0
  A(1,1) = 10.0D+0
  A(1,2) = 1.0D+0
  A(1,3) = 2.0D+0
  A(1,4) = 0.5D+0
  A(2,2) = 8.0D+0
  A(2,3) = 1.5D+0
  A(2,4) = 0.3D+0
  A(3,3) = 6.0D+0
  A(3,4) = 1.0D+0
  A(4,4) = 5.0D+0

  ! Copy A to AF, factor
  AF = A
  call DSYTRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  RPVGRW = DLA_SYRPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('upper_4x4_info0')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 2: Lower triangle, 4x4 well-conditioned, INFO=0
  ! =========================================================
  N = 4
  A = 0.0D+0
  A(1,1) = 10.0D+0
  A(2,1) = 1.0D+0
  A(2,2) = 8.0D+0
  A(3,1) = 2.0D+0
  A(3,2) = 1.5D+0
  A(3,3) = 6.0D+0
  A(4,1) = 0.5D+0
  A(4,2) = 0.3D+0
  A(4,3) = 1.0D+0
  A(4,4) = 5.0D+0

  AF = A
  call DSYTRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  RPVGRW = DLA_SYRPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('lower_4x4_info0')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 3: N=1 edge case, upper
  ! =========================================================
  N = 1
  A = 0.0D+0
  A(1,1) = 7.0D+0

  AF = A
  call DSYTRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('upper_1x1')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 4: N=1 edge case, lower
  ! =========================================================
  N = 1
  A = 0.0D+0
  A(1,1) = 3.0D+0

  AF = A
  call DSYTRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('lower_1x1')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 5: Upper, singular matrix (INFO > 0)
  ! =========================================================
  N = 3
  A = 0.0D+0
  ! Row/col 2 is a multiple of row/col 1 => singular
  A(1,1) = 4.0D+0
  A(1,2) = 2.0D+0
  A(1,3) = 1.0D+0
  A(2,2) = 1.0D+0
  A(2,3) = 0.5D+0
  A(3,3) = 3.0D+0

  AF = A
  call DSYTRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('upper_singular')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 6: Lower, singular matrix (INFO > 0)
  ! =========================================================
  N = 3
  A = 0.0D+0
  A(1,1) = 4.0D+0
  A(2,1) = 2.0D+0
  A(2,2) = 1.0D+0
  A(3,1) = 1.0D+0
  A(3,2) = 0.5D+0
  A(3,3) = 3.0D+0

  AF = A
  call DSYTRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('lower_singular')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 7: Upper, matrix that triggers 2x2 pivots
  ! =========================================================
  N = 4
  A = 0.0D+0
  ! Small diagonal values force 2x2 pivots in Bunch-Kaufman
  A(1,1) = 0.1D+0
  A(1,2) = 5.0D+0
  A(1,3) = 0.2D+0
  A(1,4) = 0.3D+0
  A(2,2) = 0.1D+0
  A(2,3) = 0.4D+0
  A(2,4) = 0.5D+0
  A(3,3) = 8.0D+0
  A(3,4) = 1.0D+0
  A(4,4) = 7.0D+0

  AF = A
  call DSYTRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('upper_2x2_pivot')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 8: Lower, matrix that triggers 2x2 pivots
  ! =========================================================
  N = 4
  A = 0.0D+0
  A(1,1) = 0.1D+0
  A(2,1) = 5.0D+0
  A(2,2) = 0.1D+0
  A(3,1) = 0.2D+0
  A(3,2) = 0.4D+0
  A(3,3) = 8.0D+0
  A(4,1) = 0.3D+0
  A(4,2) = 0.5D+0
  A(4,3) = 1.0D+0
  A(4,4) = 7.0D+0

  AF = A
  call DSYTRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('lower_2x2_pivot')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 9: Upper, 6x6 mixed pivots
  ! =========================================================
  N = 6
  A = 0.0D+0
  A(1,1) = 0.01D+0
  A(1,2) = 9.0D+0
  A(1,3) = 0.1D+0
  A(1,4) = 0.2D+0
  A(1,5) = 0.3D+0
  A(1,6) = 0.4D+0
  A(2,2) = 0.02D+0
  A(2,3) = 0.5D+0
  A(2,4) = 0.6D+0
  A(2,5) = 0.7D+0
  A(2,6) = 0.8D+0
  A(3,3) = 12.0D+0
  A(3,4) = 1.0D+0
  A(3,5) = 0.5D+0
  A(3,6) = 0.2D+0
  A(4,4) = 10.0D+0
  A(4,5) = 2.0D+0
  A(4,6) = 0.3D+0
  A(5,5) = 11.0D+0
  A(5,6) = 1.5D+0
  A(6,6) = 9.0D+0

  AF = A
  call DSYTRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('upper_6x6_mixed')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 10: Lower, 6x6 mixed pivots
  ! =========================================================
  N = 6
  A = 0.0D+0
  A(1,1) = 0.01D+0
  A(2,1) = 9.0D+0
  A(2,2) = 0.02D+0
  A(3,1) = 0.1D+0
  A(3,2) = 0.5D+0
  A(3,3) = 12.0D+0
  A(4,1) = 0.2D+0
  A(4,2) = 0.6D+0
  A(4,3) = 1.0D+0
  A(4,4) = 10.0D+0
  A(5,1) = 0.3D+0
  A(5,2) = 0.7D+0
  A(5,3) = 0.5D+0
  A(5,4) = 2.0D+0
  A(5,5) = 11.0D+0
  A(6,1) = 0.4D+0
  A(6,2) = 0.8D+0
  A(6,3) = 0.2D+0
  A(6,4) = 0.3D+0
  A(6,5) = 1.5D+0
  A(6,6) = 9.0D+0

  AF = A
  call DSYTRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('lower_6x6_mixed')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 11: Lower with nontrivial 1x1 pivot swaps
  ! A matrix where dsytrf ('L') permutes rows for 1x1 pivots
  ! Small diagonal at position 1, large off-diagonal below => swap
  ! =========================================================
  N = 4
  A = 0.0D+0
  ! Very small A(1,1) and large A(3,1) to force pivot swap
  A(1,1) = 0.001D+0
  A(2,1) = 0.002D+0
  A(2,2) = 10.0D+0
  A(3,1) = 8.0D+0
  A(3,2) = 0.5D+0
  A(3,3) = 12.0D+0
  A(4,1) = 0.003D+0
  A(4,2) = 0.4D+0
  A(4,3) = 0.6D+0
  A(4,4) = 9.0D+0

  AF = A
  call DSYTRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('lower_1x1_swap')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 12: Lower with nontrivial 1x1 pivot swaps, larger
  ! =========================================================
  N = 5
  A = 0.0D+0
  ! Force multiple 1x1 pivot swaps in lower
  A(1,1) = 0.0001D+0
  A(2,1) = 0.0002D+0
  A(2,2) = 0.0003D+0
  A(3,1) = 7.0D+0
  A(3,2) = 6.0D+0
  A(3,3) = 20.0D+0
  A(4,1) = 0.001D+0
  A(4,2) = 0.002D+0
  A(4,3) = 0.5D+0
  A(4,4) = 15.0D+0
  A(5,1) = 0.003D+0
  A(5,2) = 0.004D+0
  A(5,3) = 0.3D+0
  A(5,4) = 1.0D+0
  A(5,5) = 12.0D+0

  AF = A
  call DSYTRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = DLA_SYRPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  call begin_test('lower_1x1_swap_5x5')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('AF', AF, NMAX, N, N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

end program
