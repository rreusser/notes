program test_zgghrd
  use test_utils
  implicit none

  integer, parameter :: NMAX = 5
  complex*16 :: A(NMAX, NMAX), B(NMAX, NMAX), Q(NMAX, NMAX), Z(NMAX, NMAX)
  double precision :: A_r(2*NMAX*NMAX), B_r(2*NMAX*NMAX)
  double precision :: Q_r(2*NMAX*NMAX), Z_r(2*NMAX*NMAX)
  equivalence (A, A_r)
  equivalence (B, B_r)
  equivalence (Q, Q_r)
  equivalence (Z, Z_r)
  integer :: info, n, ilo, ihi, lda, ldb, ldq, ldz
  integer :: i, j

  lda = NMAX
  ldb = NMAX
  ldq = NMAX
  ldz = NMAX

  ! ==================================================================
  ! Test 1: Basic 4x4, COMPQ='I', COMPZ='I', ILO=1, IHI=4
  ! A is a general complex matrix, B is upper triangular
  ! ==================================================================
  n = 4
  ilo = 1
  ihi = 4

  ! Initialize A with known values (general complex)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (0.5d0, -0.5d0)
  A(1,4) = (1.0d0, 1.0d0)
  A(2,1) = (1.0d0, -1.0d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (1.0d0, 1.0d0)
  A(2,4) = (0.5d0, 0.5d0)
  A(3,1) = (0.5d0, 0.5d0)
  A(3,2) = (2.0d0, -1.0d0)
  A(3,3) = (4.0d0, 1.0d0)
  A(3,4) = (1.0d0, 0.0d0)
  A(4,1) = (0.0d0, 1.0d0)
  A(4,2) = (1.0d0, 1.0d0)
  A(4,3) = (0.5d0, -0.5d0)
  A(4,4) = (2.0d0, -1.0d0)

  ! Initialize B as upper triangular
  B = (0.0d0, 0.0d0)
  B(1,1) = (3.0d0, 0.0d0)
  B(1,2) = (1.0d0, 0.5d0)
  B(1,3) = (0.5d0, 0.5d0)
  B(1,4) = (0.0d0, 1.0d0)
  B(2,2) = (2.0d0, 1.0d0)
  B(2,3) = (1.0d0, 0.0d0)
  B(2,4) = (0.5d0, -0.5d0)
  B(3,3) = (4.0d0, -1.0d0)
  B(3,4) = (1.0d0, 1.0d0)
  B(4,4) = (1.0d0, 0.0d0)

  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)

  call zgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  call begin_test('basic_4x4')
  call print_int('info', info)
  call print_int('n', n)
  ! Print A column by column (n=4, lda=5, so each column is 5 complex = 10 doubles)
  ! Only print n rows per column
  call print_array('A_col1', A_r(1), 2*n)
  call print_array('A_col2', A_r(2*lda+1), 2*n)
  call print_array('A_col3', A_r(4*lda+1), 2*n)
  call print_array('A_col4', A_r(6*lda+1), 2*n)
  call print_array('B_col1', B_r(1), 2*n)
  call print_array('B_col2', B_r(2*lda+1), 2*n)
  call print_array('B_col3', B_r(4*lda+1), 2*n)
  call print_array('B_col4', B_r(6*lda+1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  call print_array('Q_col4', Q_r(6*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call print_array('Z_col4', Z_r(6*ldz+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 2: N=1 quick return (should be no-op after identity init)
  ! ==================================================================
  n = 1
  ilo = 1
  ihi = 1
  A(1,1) = (5.0d0, 3.0d0)
  B(1,1) = (2.0d0, 1.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)

  call zgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  call begin_test('n_equals_1')
  call print_int('info', info)
  call print_array('A', A_r(1), 2)
  call print_array('B', B_r(1), 2)
  call print_array('Q', Q_r(1), 2)
  call print_array('Z', Z_r(1), 2)
  call end_test()

  ! ==================================================================
  ! Test 3: COMPQ='N', COMPZ='N' (no orthogonal matrix accumulation)
  ! 3x3 case
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3

  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (2.0d0, 1.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(2,1) = (4.0d0, 2.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (6.0d0, 1.0d0)
  A(3,1) = (7.0d0, -1.0d0)
  A(3,2) = (8.0d0, 2.0d0)
  A(3,3) = (9.0d0, 0.0d0)

  B = (0.0d0, 0.0d0)
  B(1,1) = (2.0d0, 0.0d0)
  B(1,2) = (1.0d0, 1.0d0)
  B(1,3) = (0.5d0, 0.0d0)
  B(2,2) = (3.0d0, 0.0d0)
  B(2,3) = (1.0d0, -1.0d0)
  B(3,3) = (1.0d0, 0.0d0)

  ! Q and Z not referenced when COMPQ='N', COMPZ='N'
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)

  call zgghrd('N', 'N', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  call begin_test('no_qz_3x3')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A_col1', A_r(1), 2*n)
  call print_array('A_col2', A_r(2*lda+1), 2*n)
  call print_array('A_col3', A_r(4*lda+1), 2*n)
  call print_array('B_col1', B_r(1), 2*n)
  call print_array('B_col2', B_r(2*lda+1), 2*n)
  call print_array('B_col3', B_r(4*lda+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 4: Partial reduction: ILO=2, IHI=4 on a 5x5 matrix
  ! Only rows/cols 2:4 should be reduced
  ! ==================================================================
  n = 5
  ilo = 2
  ihi = 4

  ! Fill A with a simple pattern
  do j = 1, n
    do i = 1, n
      A(i,j) = dcmplx(dble(i + j), dble(i - j) * 0.5d0)
    end do
  end do

  ! B upper triangular
  B = (0.0d0, 0.0d0)
  do j = 1, n
    do i = 1, j
      B(i,j) = dcmplx(dble(i + j), dble(j - i) * 0.25d0)
    end do
  end do

  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)

  call zgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  call begin_test('partial_5x5')
  call print_int('info', info)
  call print_int('n', n)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('A_col1', A_r(1), 2*n)
  call print_array('A_col2', A_r(2*lda+1), 2*n)
  call print_array('A_col3', A_r(4*lda+1), 2*n)
  call print_array('A_col4', A_r(6*lda+1), 2*n)
  call print_array('A_col5', A_r(8*lda+1), 2*n)
  call print_array('B_col1', B_r(1), 2*n)
  call print_array('B_col2', B_r(2*lda+1), 2*n)
  call print_array('B_col3', B_r(4*lda+1), 2*n)
  call print_array('B_col4', B_r(6*lda+1), 2*n)
  call print_array('B_col5', B_r(8*lda+1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  call print_array('Q_col4', Q_r(6*ldq+1), 2*n)
  call print_array('Q_col5', Q_r(8*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call print_array('Z_col4', Z_r(6*ldz+1), 2*n)
  call print_array('Z_col5', Z_r(8*ldz+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 5: COMPQ='V', COMPZ='V' (accumulate into existing Q, Z)
  ! 3x3 case - initialize Q and Z to identity first
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3

  A(1,1) = (2.0d0, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.0d0, 1.0d0)
  A(2,1) = (3.0d0, -1.0d0)
  A(2,2) = (1.0d0, 0.0d0)
  A(2,3) = (2.0d0, 0.5d0)
  A(3,1) = (1.0d0, 2.0d0)
  A(3,2) = (0.0d0, -1.0d0)
  A(3,3) = (3.0d0, 1.0d0)

  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(1,2) = (0.5d0, 0.5d0)
  B(1,3) = (0.0d0, 1.0d0)
  B(2,2) = (2.0d0, 0.0d0)
  B(2,3) = (1.0d0, 0.0d0)
  B(3,3) = (3.0d0, 0.0d0)

  ! Initialize Q and Z to identity
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  do i = 1, n
    Q(i,i) = (1.0d0, 0.0d0)
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  call zgghrd('V', 'V', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  call begin_test('accumulate_3x3')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A_col1', A_r(1), 2*n)
  call print_array('A_col2', A_r(2*lda+1), 2*n)
  call print_array('A_col3', A_r(4*lda+1), 2*n)
  call print_array('B_col1', B_r(1), 2*n)
  call print_array('B_col2', B_r(2*lda+1), 2*n)
  call print_array('B_col3', B_r(4*lda+1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 6: IHI = ILO (already reduced, should be near no-op)
  ! ==================================================================
  n = 3
  ilo = 2
  ihi = 2

  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 0.0d0)
  A(2,1) = (4.0d0, 0.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (6.0d0, 0.0d0)
  A(3,1) = (7.0d0, 0.0d0)
  A(3,2) = (8.0d0, 0.0d0)
  A(3,3) = (9.0d0, 0.0d0)

  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(1,2) = (1.0d0, 0.0d0)
  B(1,3) = (1.0d0, 0.0d0)
  B(2,2) = (2.0d0, 0.0d0)
  B(2,3) = (1.0d0, 0.0d0)
  B(3,3) = (3.0d0, 0.0d0)

  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)

  call zgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  call begin_test('ilo_eq_ihi')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A_col1', A_r(1), 2*n)
  call print_array('A_col2', A_r(2*lda+1), 2*n)
  call print_array('A_col3', A_r(4*lda+1), 2*n)
  call print_array('B_col1', B_r(1), 2*n)
  call print_array('B_col2', B_r(2*lda+1), 2*n)
  call print_array('B_col3', B_r(4*lda+1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 7: IHI = ILO-1 (empty range, most trivial quick return case)
  ! ==================================================================
  n = 3
  ilo = 2
  ihi = 1

  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 0.0d0)
  A(2,1) = (4.0d0, 0.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (6.0d0, 0.0d0)
  A(3,1) = (7.0d0, 0.0d0)
  A(3,2) = (8.0d0, 0.0d0)
  A(3,3) = (9.0d0, 0.0d0)

  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,2) = (2.0d0, 0.0d0)
  B(3,3) = (3.0d0, 0.0d0)

  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)

  call zgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  call begin_test('empty_range')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A_col1', A_r(1), 2*n)
  call print_array('A_col2', A_r(2*lda+1), 2*n)
  call print_array('A_col3', A_r(4*lda+1), 2*n)
  call print_array('B_col1', B_r(1), 2*n)
  call print_array('B_col2', B_r(2*lda+1), 2*n)
  call print_array('B_col3', B_r(4*lda+1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call end_test()

end program
