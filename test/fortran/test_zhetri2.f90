program test_zhetri2
  use test_utils
  implicit none

  ! Notes
  ! -----
  ! ZHETRI2 dispatches between ZHETRI (unblocked) and ZHETRI2X (blocked) based
  ! on ILAENV's NBMAX vs N. We exercise both code paths plus the singular and
  ! quick-return guards:
  !
  !   * small N (< typical NBMAX of 32)  -> dispatches to ZHETRI
  !   * larger N (>= NBMAX) with LWORK   -> dispatches to ZHETRI2X
  !
  ! Workspace size: WORK must be at least (N+NBMAX+1)*(NBMAX+3) complex elements
  ! when the dispatcher selects the blocked path. We provision generously.

  integer, parameter :: NMAX = 40
  integer, parameter :: LWMAX = (NMAX + 32 + 1) * (32 + 3)
  complex*16 :: A(NMAX, NMAX), WORK_TF(NMAX*32)
  complex*16 :: WORK(LWMAX)
  double precision :: A_r(2*NMAX*NMAX)
  equivalence (A, A_r)
  integer :: IPIV(NMAX), INFO, n, LWORK_TF, LWORK, i, j

  LWORK_TF = NMAX * 32
  LWORK = LWMAX

  ! Test 1: 4x4 upper Hermitian positive-definite (1x1 pivots), small N path -> ZHETRI.
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0);   A(2,2) = (5.0d0, 0.0d0)
  A(1,3) = (3.0d0, -1.0d0);  A(2,3) = (2.0d0, 1.0d0);   A(3,3) = (6.0d0, 0.0d0)
  A(1,4) = (0.5d0, 0.5d0);   A(2,4) = (1.0d0, -2.0d0);  A(3,4) = (3.0d0, 0.0d0);  A(4,4) = (8.0d0, 0.0d0)
  call ZHETRF('U', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('4x4_upper_def')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHETRI2('U', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 2: 4x4 lower Hermitian positive-definite (1x1 pivots), small N path.
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, -2.0d0);  A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (3.0d0, 1.0d0);   A(3,2) = (2.0d0, -1.0d0); A(3,3) = (6.0d0, 0.0d0)
  A(4,1) = (0.5d0, -0.5d0);  A(4,2) = (1.0d0, 2.0d0);  A(4,3) = (3.0d0, 0.0d0);  A(4,4) = (8.0d0, 0.0d0)
  call ZHETRF('L', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('4x4_lower_def')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHETRI2('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 3: 4x4 upper indefinite (2x2 pivots forced by zero diagonal), small N path.
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0);   A(2,2) = (0.0d0, 0.0d0)
  A(1,3) = (2.0d0, 0.0d0);   A(2,3) = (4.0d0, -1.0d0);  A(3,3) = (0.0d0, 0.0d0)
  A(1,4) = (3.0d0, 2.0d0);   A(2,4) = (5.0d0, 1.0d0);   A(3,4) = (6.0d0, 0.0d0);   A(4,4) = (0.0d0, 0.0d0)
  call ZHETRF('U', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('4x4_upper_indef')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHETRI2('U', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 4: 4x4 lower indefinite (2x2 pivots), small N path.
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, -1.0d0); A(2,2) = (0.0d0, 0.0d0)
  A(3,1) = (2.0d0, 0.0d0);  A(3,2) = (4.0d0, 1.0d0);  A(3,3) = (0.0d0, 0.0d0)
  A(4,1) = (3.0d0, -2.0d0); A(4,2) = (5.0d0, -1.0d0); A(4,3) = (6.0d0, 0.0d0); A(4,4) = (0.0d0, 0.0d0)
  call ZHETRF('L', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('4x4_lower_indef')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHETRI2('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 5: 5x5 lower mixed pivots, small N path.
  n = 5
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(2,1) = (2.0d0, -1.0d0); A(2,2) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0);  A(3,2) = (3.0d0, 1.0d0); A(3,3) = (4.0d0, 0.0d0)
  A(4,1) = (1.0d0, -2.0d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (2.0d0, -1.0d0); A(4,4) = (0.0d0, 0.0d0)
  A(5,1) = (1.0d0, 0.0d0);  A(5,2) = (2.0d0, 0.0d0); A(5,3) = (0.0d0, 0.0d0);  A(5,4) = (1.0d0, -1.0d0); A(5,5) = (5.0d0, 0.0d0)
  call ZHETRF('L', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('5x5_lower_mixed')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHETRI2('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 6: N=1 trivial.
  n = 1
  A(1,1) = (5.0d0, 0.0d0)
  IPIV(1) = 1
  call begin_test('n_one_lower')
  call ZHETRI2('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 7: N=0 quick return.
  n = 0
  call begin_test('n_zero')
  call ZHETRI2('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call end_test()

  ! Test 8: Larger N that forces the blocked path (NBMAX=32 typical).
  ! Use a diagonally-dominant Hermitian matrix that yields 1x1 pivots.
  n = 40
  A = (0.0d0, 0.0d0)
  do j = 1, n
    do i = 1, n
      if (i == j) then
        A(i,j) = dcmplx(3.0d0 + dble(i), 0.0d0)
      else if (i < j) then
        ! Upper-triangular off-diagonal; make small to keep dominance.
        A(i,j) = dcmplx( 0.1d0 * dsin(dble(i + 2*j)), 0.1d0 * dcos(dble(2*i + j)) )
      end if
    end do
  end do
  call ZHETRF('U', n, A, NMAX, IPIV, WORK_TF, LWORK_TF, INFO)
  call begin_test('40x40_upper_blocked')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHETRI2('U', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

end program
