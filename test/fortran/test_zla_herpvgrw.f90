program test_zla_herpvgrw
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX)
  double precision :: A_r(2*NMAX*NMAX), AF_r(2*NMAX*NMAX)
  double precision :: WORK(2*NMAX)
  complex*16 :: WORKF(NMAX)
  integer :: IPIV(NMAX), INFO, N, I, J
  double precision :: ZLA_HERPVGRW, RPVGRW

  ! Pack helpers: copy NxN complex submatrix to contiguous double array
  ! (avoids leading-dimension vs EQUIVALENCE stride mismatch)
  complex*16 :: Apk(NMAX*NMAX), AFpk(NMAX*NMAX)
  double precision :: Apk_r(2*NMAX*NMAX), AFpk_r(2*NMAX*NMAX)
  equivalence (Apk, Apk_r)
  equivalence (AFpk, AFpk_r)

  external :: ZHETRF

  ! =========================================================
  ! Test 1: Upper triangle, 4x4 well-conditioned, INFO=0
  ! Hermitian => diag is real, A(i,j) = conj(A(j,i))
  ! =========================================================
  N = 4
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (10.0D+0, 0.0D+0)
  A(1,2) = (1.0D+0, 0.5D+0)
  A(1,3) = (2.0D+0, -1.0D+0)
  A(1,4) = (0.5D+0, 0.3D+0)
  A(2,2) = (8.0D+0, 0.0D+0)
  A(2,3) = (1.5D+0, 0.2D+0)
  A(2,4) = (0.3D+0, -0.1D+0)
  A(3,3) = (6.0D+0, 0.0D+0)
  A(3,4) = (1.0D+0, 0.4D+0)
  A(4,4) = (5.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  RPVGRW = ZLA_HERPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  ! Pack matrices for printing
  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('upper_4x4_info0')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 2: Lower triangle, 4x4 well-conditioned, INFO=0
  ! =========================================================
  N = 4
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (10.0D+0, 0.0D+0)
  A(2,1) = (1.0D+0, -0.5D+0)
  A(2,2) = (8.0D+0, 0.0D+0)
  A(3,1) = (2.0D+0, 1.0D+0)
  A(3,2) = (1.5D+0, -0.2D+0)
  A(3,3) = (6.0D+0, 0.0D+0)
  A(4,1) = (0.5D+0, -0.3D+0)
  A(4,2) = (0.3D+0, 0.1D+0)
  A(4,3) = (1.0D+0, -0.4D+0)
  A(4,4) = (5.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  RPVGRW = ZLA_HERPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('lower_4x4_info0')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 3: N=1 edge case, upper
  ! =========================================================
  N = 1
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (7.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  Apk(1) = A(1,1)
  AFpk(1) = AF(1,1)

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
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (3.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

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
  A = (0.0D+0, 0.0D+0)
  ! Row/col 2 approximately a multiple of row/col 1 => near-singular
  A(1,1) = (4.0D+0, 0.0D+0)
  A(1,2) = (2.0D+0, 0.0D+0)
  A(1,3) = (1.0D+0, 0.5D+0)
  A(2,2) = (1.0D+0, 0.0D+0)
  A(2,3) = (0.5D+0, 0.25D+0)
  A(3,3) = (3.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('upper_singular')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 6: Lower, singular matrix (INFO > 0)
  ! =========================================================
  N = 3
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (4.0D+0, 0.0D+0)
  A(2,1) = (2.0D+0, 0.0D+0)
  A(2,2) = (1.0D+0, 0.0D+0)
  A(3,1) = (1.0D+0, -0.5D+0)
  A(3,2) = (0.5D+0, -0.25D+0)
  A(3,3) = (3.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('lower_singular')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 7: Upper, matrix that triggers 2x2 pivots
  ! =========================================================
  N = 4
  A = (0.0D+0, 0.0D+0)
  ! Small diagonal values force 2x2 pivots in Bunch-Kaufman
  A(1,1) = (0.1D+0, 0.0D+0)
  A(1,2) = (5.0D+0, 1.0D+0)
  A(1,3) = (0.2D+0, -0.1D+0)
  A(1,4) = (0.3D+0, 0.2D+0)
  A(2,2) = (0.1D+0, 0.0D+0)
  A(2,3) = (0.4D+0, 0.3D+0)
  A(2,4) = (0.5D+0, -0.2D+0)
  A(3,3) = (8.0D+0, 0.0D+0)
  A(3,4) = (1.0D+0, 0.5D+0)
  A(4,4) = (7.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('upper_2x2_pivot')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 8: Lower, matrix that triggers 2x2 pivots
  ! =========================================================
  N = 4
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (0.1D+0, 0.0D+0)
  A(2,1) = (5.0D+0, -1.0D+0)
  A(2,2) = (0.1D+0, 0.0D+0)
  A(3,1) = (0.2D+0, 0.1D+0)
  A(3,2) = (0.4D+0, -0.3D+0)
  A(3,3) = (8.0D+0, 0.0D+0)
  A(4,1) = (0.3D+0, -0.2D+0)
  A(4,2) = (0.5D+0, 0.2D+0)
  A(4,3) = (1.0D+0, -0.5D+0)
  A(4,4) = (7.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('lower_2x2_pivot')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 9: Upper, 6x6 mixed pivots
  ! =========================================================
  N = 6
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (0.01D+0, 0.0D+0)
  A(1,2) = (9.0D+0, 2.0D+0)
  A(1,3) = (0.1D+0, -0.05D+0)
  A(1,4) = (0.2D+0, 0.1D+0)
  A(1,5) = (0.3D+0, -0.15D+0)
  A(1,6) = (0.4D+0, 0.2D+0)
  A(2,2) = (0.02D+0, 0.0D+0)
  A(2,3) = (0.5D+0, 0.3D+0)
  A(2,4) = (0.6D+0, -0.2D+0)
  A(2,5) = (0.7D+0, 0.1D+0)
  A(2,6) = (0.8D+0, -0.4D+0)
  A(3,3) = (12.0D+0, 0.0D+0)
  A(3,4) = (1.0D+0, 0.5D+0)
  A(3,5) = (0.5D+0, -0.3D+0)
  A(3,6) = (0.2D+0, 0.1D+0)
  A(4,4) = (10.0D+0, 0.0D+0)
  A(4,5) = (2.0D+0, 1.0D+0)
  A(4,6) = (0.3D+0, -0.15D+0)
  A(5,5) = (11.0D+0, 0.0D+0)
  A(5,6) = (1.5D+0, 0.7D+0)
  A(6,6) = (9.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('U', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('U', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('upper_6x6_mixed')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 10: Lower, 6x6 mixed pivots
  ! =========================================================
  N = 6
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (0.01D+0, 0.0D+0)
  A(2,1) = (9.0D+0, -2.0D+0)
  A(2,2) = (0.02D+0, 0.0D+0)
  A(3,1) = (0.1D+0, 0.05D+0)
  A(3,2) = (0.5D+0, -0.3D+0)
  A(3,3) = (12.0D+0, 0.0D+0)
  A(4,1) = (0.2D+0, -0.1D+0)
  A(4,2) = (0.6D+0, 0.2D+0)
  A(4,3) = (1.0D+0, -0.5D+0)
  A(4,4) = (10.0D+0, 0.0D+0)
  A(5,1) = (0.3D+0, 0.15D+0)
  A(5,2) = (0.7D+0, -0.1D+0)
  A(5,3) = (0.5D+0, 0.3D+0)
  A(5,4) = (2.0D+0, -1.0D+0)
  A(5,5) = (11.0D+0, 0.0D+0)
  A(6,1) = (0.4D+0, -0.2D+0)
  A(6,2) = (0.8D+0, 0.4D+0)
  A(6,3) = (0.2D+0, -0.1D+0)
  A(6,4) = (0.3D+0, 0.15D+0)
  A(6,5) = (1.5D+0, -0.7D+0)
  A(6,6) = (9.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('lower_6x6_mixed')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 11: Lower with nontrivial 1x1 pivot swaps
  ! =========================================================
  N = 4
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (0.001D+0, 0.0D+0)
  A(2,1) = (0.002D+0, -0.001D+0)
  A(2,2) = (10.0D+0, 0.0D+0)
  A(3,1) = (8.0D+0, 1.0D+0)
  A(3,2) = (0.5D+0, -0.2D+0)
  A(3,3) = (12.0D+0, 0.0D+0)
  A(4,1) = (0.003D+0, 0.002D+0)
  A(4,2) = (0.4D+0, 0.1D+0)
  A(4,3) = (0.6D+0, -0.3D+0)
  A(4,4) = (9.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('lower_1x1_swap')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

  ! =========================================================
  ! Test 12: Lower with nontrivial 1x1 pivot swaps, 5x5
  ! =========================================================
  N = 5
  A = (0.0D+0, 0.0D+0)
  A(1,1) = (0.0001D+0, 0.0D+0)
  A(2,1) = (0.0002D+0, -0.0001D+0)
  A(2,2) = (0.0003D+0, 0.0D+0)
  A(3,1) = (7.0D+0, 1.0D+0)
  A(3,2) = (6.0D+0, -0.5D+0)
  A(3,3) = (20.0D+0, 0.0D+0)
  A(4,1) = (0.001D+0, 0.0005D+0)
  A(4,2) = (0.002D+0, -0.001D+0)
  A(4,3) = (0.5D+0, 0.2D+0)
  A(4,4) = (15.0D+0, 0.0D+0)
  A(5,1) = (0.003D+0, -0.002D+0)
  A(5,2) = (0.004D+0, 0.001D+0)
  A(5,3) = (0.3D+0, -0.1D+0)
  A(5,4) = (1.0D+0, 0.5D+0)
  A(5,5) = (12.0D+0, 0.0D+0)

  AF = A
  call ZHETRF('L', N, AF, NMAX, IPIV, WORKF, NMAX, INFO)

  WORK = 0.0D+0
  RPVGRW = ZLA_HERPVGRW('L', N, INFO, A, NMAX, AF, NMAX, IPIV, WORK)

  do J = 1, N
    do I = 1, N
      Apk(I + (J-1)*N) = A(I, J)
      AFpk(I + (J-1)*N) = AF(I, J)
    end do
  end do

  call begin_test('lower_1x1_swap_5x5')
  call print_int('N', N)
  call print_int('INFO', INFO)
  call print_scalar('rpvgrw', RPVGRW)
  call print_array('A', Apk_r, 2*N*N)
  call print_array('AF', AFpk_r, 2*N*N)
  call print_int_array('IPIV', IPIV, N)
  call print_array('WORK', WORK, 2*N)
  call end_test()

end program
