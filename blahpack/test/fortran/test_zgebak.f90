program test_zgebak
  use test_utils
  implicit none

  complex*16 :: A(5,5), V(5,5)
  double precision :: SCALE(5)
  ! For printing complex V as interleaved re/im doubles:
  ! We extract NxM submatrix column-by-column into a flat double array
  double precision :: Vflat(80)
  integer :: ILO, IHI, INFO, N, M, LDV, i, j, idx

  ! ===========================================================
  ! Test 1: JOB='B', SIDE='R' — full round-trip with zgebal
  ! ===========================================================
  N = 4
  M = 4
  LDV = 5

  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);   A(1,2) = (0.0d0, 0.0d0);    A(1,3) = (0.0d0, 0.0d0);    A(1,4) = (1000.0d0, 500.0d0)
  A(2,1) = (0.0d0, 0.0d0);   A(2,2) = (2.0d0, 1.0d0);    A(2,3) = (0.001d0, 0.0005d0); A(2,4) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0);   A(3,2) = (1000.0d0, 500.0d0); A(3,3) = (3.0d0, 1.5d0);   A(3,4) = (0.0d0, 0.0d0)
  A(4,1) = (0.001d0, 0.0005d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (0.0d0, 0.0d0);    A(4,4) = (4.0d0, 2.0d0)

  call ZGEBAL('B', N, A, 5, ILO, IHI, SCALE, INFO)

  V = (0.0d0, 0.0d0)
  do i = 1, N
    V(i,i) = (1.0d0, 0.0d0)
  end do

  call ZGEBAK('B', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('job_B_side_R')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

  ! ===========================================================
  ! Test 2: JOB='B', SIDE='L' — left eigenvectors
  ! ===========================================================
  V = (0.0d0, 0.0d0)
  do i = 1, N
    V(i,i) = (1.0d0, 0.0d0)
  end do

  call ZGEBAK('B', 'L', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('job_B_side_L')
  call print_int('info', INFO)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

  ! ===========================================================
  ! Test 3: JOB='S' (scaling only), SIDE='R'
  ! ===========================================================
  V = (0.0d0, 0.0d0)
  do i = 1, N
    V(i,i) = (1.0d0, 0.0d0)
  end do

  call ZGEBAK('S', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('job_S_side_R')
  call print_int('info', INFO)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

  ! ===========================================================
  ! Test 4: JOB='P' (permutation only), SIDE='R'
  ! ===========================================================
  V = (0.0d0, 0.0d0)
  do i = 1, N
    V(i,i) = (1.0d0, 0.0d0)
  end do

  call ZGEBAK('P', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('job_P_side_R')
  call print_int('info', INFO)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

  ! ===========================================================
  ! Test 5: JOB='N' (no-op)
  ! ===========================================================
  V = (0.0d0, 0.0d0)
  do i = 1, N
    V(i,i) = (1.0d0, 0.0d0)
  end do

  call ZGEBAK('N', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('job_N')
  call print_int('info', INFO)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

  ! ===========================================================
  ! Test 6: N=0 (quick return)
  ! ===========================================================
  call ZGEBAK('B', 'R', 0, 1, 0, SCALE, 0, V, LDV, INFO)

  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ===========================================================
  ! Test 7: ILO=IHI (only permutation, no scaling loop)
  ! ===========================================================
  N = 3
  M = 2

  SCALE(1) = 3.0d0
  SCALE(2) = 1.0d0
  SCALE(3) = 1.0d0

  V = (0.0d0, 0.0d0)
  V(1,1) = (1.0d0, 0.5d0); V(1,2) = (2.0d0, 1.0d0)
  V(2,1) = (3.0d0, 1.5d0); V(2,2) = (4.0d0, 2.0d0)
  V(3,1) = (5.0d0, 2.5d0); V(3,2) = (6.0d0, 3.0d0)

  call ZGEBAK('B', 'R', N, 2, 2, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('ilo_eq_ihi')
  call print_int('info', INFO)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

  ! ===========================================================
  ! Test 8: JOB='S', SIDE='L' (scaling only, left eigenvectors)
  ! ===========================================================
  N = 4
  M = 4

  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);   A(1,2) = (0.0d0, 0.0d0);    A(1,3) = (0.0d0, 0.0d0);    A(1,4) = (1000.0d0, 500.0d0)
  A(2,1) = (0.0d0, 0.0d0);   A(2,2) = (2.0d0, 1.0d0);    A(2,3) = (0.001d0, 0.0005d0); A(2,4) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0);   A(3,2) = (1000.0d0, 500.0d0); A(3,3) = (3.0d0, 1.5d0);   A(3,4) = (0.0d0, 0.0d0)
  A(4,1) = (0.001d0, 0.0005d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (0.0d0, 0.0d0);    A(4,4) = (4.0d0, 2.0d0)

  call ZGEBAL('B', N, A, 5, ILO, IHI, SCALE, INFO)

  V = (0.0d0, 0.0d0)
  V(1,1) = (2.0d0, 0.1d0); V(1,2) = (0.5d0, 0.2d0); V(1,3) = (1.0d0, 0.3d0); V(1,4) = (0.0d0, 0.4d0)
  V(2,1) = (0.0d0, 0.5d0); V(2,2) = (3.0d0, 0.6d0); V(2,3) = (0.0d0, 0.7d0); V(2,4) = (1.0d0, 0.8d0)
  V(3,1) = (1.0d0, 0.9d0); V(3,2) = (0.0d0, 1.0d0); V(3,3) = (2.0d0, 1.1d0); V(3,4) = (0.5d0, 1.2d0)
  V(4,1) = (0.5d0, 1.3d0); V(4,2) = (1.0d0, 1.4d0); V(4,3) = (0.5d0, 1.5d0); V(4,4) = (2.0d0, 1.6d0)

  call ZGEBAK('S', 'L', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('job_S_side_L')
  call print_int('info', INFO)
  call print_array('scale', SCALE, N)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

  ! ===========================================================
  ! Test 9: JOB='P', SIDE='L' (permutation only, left)
  ! ===========================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);   A(1,2) = (0.0d0, 0.0d0);    A(1,3) = (0.0d0, 0.0d0);    A(1,4) = (1000.0d0, 500.0d0)
  A(2,1) = (0.0d0, 0.0d0);   A(2,2) = (2.0d0, 1.0d0);    A(2,3) = (0.001d0, 0.0005d0); A(2,4) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0);   A(3,2) = (1000.0d0, 500.0d0); A(3,3) = (3.0d0, 1.5d0);   A(3,4) = (0.0d0, 0.0d0)
  A(4,1) = (0.001d0, 0.0005d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (0.0d0, 0.0d0);    A(4,4) = (4.0d0, 2.0d0)

  call ZGEBAL('B', N, A, 5, ILO, IHI, SCALE, INFO)

  V = (0.0d0, 0.0d0)
  do i = 1, N
    V(i,i) = (1.0d0, 0.0d0)
  end do

  call ZGEBAK('P', 'L', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('job_P_side_L')
  call print_int('info', INFO)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

  ! ===========================================================
  ! Test 10: M=0 (quick return)
  ! ===========================================================
  call ZGEBAK('B', 'R', 4, 1, 4, SCALE, 0, V, LDV, INFO)

  call begin_test('m_zero')
  call print_int('info', INFO)
  call end_test()

  ! ===========================================================
  ! Test 11: Non-identity V with JOB='B', SIDE='R'
  ! ===========================================================
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0);   A(1,2) = (0.0d0, 0.0d0);    A(1,3) = (0.0d0, 0.0d0);    A(1,4) = (1000.0d0, 500.0d0)
  A(2,1) = (0.0d0, 0.0d0);   A(2,2) = (2.0d0, 1.0d0);    A(2,3) = (0.001d0, 0.0005d0); A(2,4) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0);   A(3,2) = (1000.0d0, 500.0d0); A(3,3) = (3.0d0, 1.5d0);   A(3,4) = (0.0d0, 0.0d0)
  A(4,1) = (0.001d0, 0.0005d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (0.0d0, 0.0d0);    A(4,4) = (4.0d0, 2.0d0)

  call ZGEBAL('B', N, A, 5, ILO, IHI, SCALE, INFO)

  V = (0.0d0, 0.0d0)
  V(1,1) = (2.0d0, 0.1d0); V(1,2) = (0.5d0, 0.2d0); V(1,3) = (1.0d0, 0.3d0); V(1,4) = (0.0d0, 0.4d0)
  V(2,1) = (0.0d0, 0.5d0); V(2,2) = (3.0d0, 0.6d0); V(2,3) = (0.0d0, 0.7d0); V(2,4) = (1.0d0, 0.8d0)
  V(3,1) = (1.0d0, 0.9d0); V(3,2) = (0.0d0, 1.0d0); V(3,3) = (2.0d0, 1.1d0); V(3,4) = (0.5d0, 1.2d0)
  V(4,1) = (0.5d0, 1.3d0); V(4,2) = (1.0d0, 1.4d0); V(4,3) = (0.5d0, 1.5d0); V(4,4) = (2.0d0, 1.6d0)

  call ZGEBAK('B', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call flatten_complex_matrix(V, LDV, N, M, Vflat)
  call begin_test('nonidentity_V')
  call print_int('info', INFO)
  call print_array('scale', SCALE, N)
  call print_array('V', Vflat, 2*N*M)
  call end_test()

contains

  subroutine flatten_complex_matrix(Z, ldz, rows, cols, flat)
    complex*16, intent(in) :: Z(ldz, *)
    integer, intent(in) :: ldz, rows, cols
    double precision, intent(out) :: flat(*)
    integer :: ii, jj, kk
    double precision :: zr(2)
    complex*16 :: ztmp
    kk = 1
    do jj = 1, cols
      do ii = 1, rows
        ztmp = Z(ii, jj)
        ! Use equivalence-like trick via transfer
        flat(kk)   = dble(ztmp)
        flat(kk+1) = dimag(ztmp)
        kk = kk + 2
      end do
    end do
  end subroutine

end program
