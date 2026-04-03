program test_dgghrd
  use test_utils
  implicit none

  integer, parameter :: NMAX = 5
  double precision :: A(NMAX, NMAX), B(NMAX, NMAX)
  double precision :: Q(NMAX, NMAX), Z(NMAX, NMAX)
  double precision :: Apk(NMAX*NMAX), Bpk(NMAX*NMAX)
  double precision :: Qpk(NMAX*NMAX), Zpk(NMAX*NMAX)
  integer :: info, n, ilo, ihi, lda, ldb, ldq, ldz
  integer :: i, j

  lda = NMAX
  ldb = NMAX
  ldq = NMAX
  ldz = NMAX

  ! ==================================================================
  ! Test 1: Basic 4x4, COMPQ='I', COMPZ='I', ILO=1, IHI=4
  ! A is a general real matrix, B is upper triangular
  ! ==================================================================
  n = 4
  ilo = 1
  ihi = 4

  A(1,1) = 2.0d0
  A(1,2) = 1.0d0
  A(1,3) = 0.5d0
  A(1,4) = 1.0d0
  A(2,1) = 1.0d0
  A(2,2) = 3.0d0
  A(2,3) = 1.0d0
  A(2,4) = 0.5d0
  A(3,1) = 0.5d0
  A(3,2) = 2.0d0
  A(3,3) = 4.0d0
  A(3,4) = 1.0d0
  A(4,1) = 0.25d0
  A(4,2) = 1.0d0
  A(4,3) = 0.5d0
  A(4,4) = 2.0d0

  B = 0.0d0
  B(1,1) = 3.0d0
  B(1,2) = 1.0d0
  B(1,3) = 0.5d0
  B(1,4) = 0.25d0
  B(2,2) = 2.0d0
  B(2,3) = 1.0d0
  B(2,4) = 0.5d0
  B(3,3) = 4.0d0
  B(3,4) = 1.0d0
  B(4,4) = 1.0d0

  Q = 0.0d0
  Z = 0.0d0

  call dgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  ! Pack n x n submatrix into contiguous array
  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
      Bpk(i + (j-1)*n) = B(i, j)
      Qpk(i + (j-1)*n) = Q(i, j)
      Zpk(i + (j-1)*n) = Z(i, j)
    end do
  end do

  call begin_test('basic_4x4')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A', Apk, n*n)
  call print_array('B', Bpk, n*n)
  call print_array('Q', Qpk, n*n)
  call print_array('Z', Zpk, n*n)
  call end_test()

  ! ==================================================================
  ! Test 2: N=1 quick return (should be no-op after identity init)
  ! ==================================================================
  n = 1
  ilo = 1
  ihi = 1
  A(1,1) = 5.0d0
  B(1,1) = 2.0d0
  Q = 0.0d0
  Z = 0.0d0

  call dgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  call begin_test('n_equals_1')
  call print_int('info', info)
  call print_array('A', A(1,1), 1)
  call print_array('B', B(1,1), 1)
  call print_array('Q', Q(1,1), 1)
  call print_array('Z', Z(1,1), 1)
  call end_test()

  ! ==================================================================
  ! Test 3: COMPQ='N', COMPZ='N' (no orthogonal matrix accumulation)
  ! 3x3 case
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3

  A(1,1) = 1.0d0
  A(1,2) = 2.0d0
  A(1,3) = 3.0d0
  A(2,1) = 4.0d0
  A(2,2) = 5.0d0
  A(2,3) = 6.0d0
  A(3,1) = 7.0d0
  A(3,2) = 8.0d0
  A(3,3) = 9.0d0

  B = 0.0d0
  B(1,1) = 2.0d0
  B(1,2) = 1.0d0
  B(1,3) = 0.5d0
  B(2,2) = 3.0d0
  B(2,3) = 1.0d0
  B(3,3) = 1.0d0

  Q = 0.0d0
  Z = 0.0d0

  call dgghrd('N', 'N', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
      Bpk(i + (j-1)*n) = B(i, j)
    end do
  end do

  call begin_test('no_qz_3x3')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A', Apk, n*n)
  call print_array('B', Bpk, n*n)
  call end_test()

  ! ==================================================================
  ! Test 4: Partial reduction: ILO=2, IHI=4 on a 5x5 matrix
  ! Only rows/cols 2:4 should be reduced
  ! ==================================================================
  n = 5
  ilo = 2
  ihi = 4

  do j = 1, n
    do i = 1, n
      A(i,j) = dble(i + j) + 0.5d0 * dble(i - j)
    end do
  end do

  B = 0.0d0
  do j = 1, n
    do i = 1, j
      B(i,j) = dble(i + j) + 0.25d0 * dble(j - i)
    end do
  end do

  Q = 0.0d0
  Z = 0.0d0

  call dgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
      Bpk(i + (j-1)*n) = B(i, j)
      Qpk(i + (j-1)*n) = Q(i, j)
      Zpk(i + (j-1)*n) = Z(i, j)
    end do
  end do

  call begin_test('partial_5x5')
  call print_int('info', info)
  call print_int('n', n)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('A', Apk, n*n)
  call print_array('B', Bpk, n*n)
  call print_array('Q', Qpk, n*n)
  call print_array('Z', Zpk, n*n)
  call end_test()

  ! ==================================================================
  ! Test 5: COMPQ='V', COMPZ='V' (accumulate into existing Q, Z)
  ! 3x3 case - initialize Q and Z to identity first
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3

  A(1,1) = 2.0d0
  A(1,2) = 1.0d0
  A(1,3) = 0.5d0
  A(2,1) = 3.0d0
  A(2,2) = 1.0d0
  A(2,3) = 2.0d0
  A(3,1) = 1.0d0
  A(3,2) = 0.5d0
  A(3,3) = 3.0d0

  B = 0.0d0
  B(1,1) = 1.0d0
  B(1,2) = 0.5d0
  B(1,3) = 0.25d0
  B(2,2) = 2.0d0
  B(2,3) = 1.0d0
  B(3,3) = 3.0d0

  ! Initialize Q and Z to identity
  Q = 0.0d0
  Z = 0.0d0
  do i = 1, n
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  call dgghrd('V', 'V', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
      Bpk(i + (j-1)*n) = B(i, j)
      Qpk(i + (j-1)*n) = Q(i, j)
      Zpk(i + (j-1)*n) = Z(i, j)
    end do
  end do

  call begin_test('accumulate_3x3')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A', Apk, n*n)
  call print_array('B', Bpk, n*n)
  call print_array('Q', Qpk, n*n)
  call print_array('Z', Zpk, n*n)
  call end_test()

  ! ==================================================================
  ! Test 6: IHI = ILO (already reduced, should be near no-op)
  ! ==================================================================
  n = 3
  ilo = 2
  ihi = 2

  A(1,1) = 1.0d0
  A(1,2) = 2.0d0
  A(1,3) = 3.0d0
  A(2,1) = 4.0d0
  A(2,2) = 5.0d0
  A(2,3) = 6.0d0
  A(3,1) = 7.0d0
  A(3,2) = 8.0d0
  A(3,3) = 9.0d0

  B = 0.0d0
  B(1,1) = 1.0d0
  B(1,2) = 1.0d0
  B(1,3) = 1.0d0
  B(2,2) = 2.0d0
  B(2,3) = 1.0d0
  B(3,3) = 3.0d0

  Q = 0.0d0
  Z = 0.0d0

  call dgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
      Bpk(i + (j-1)*n) = B(i, j)
      Qpk(i + (j-1)*n) = Q(i, j)
      Zpk(i + (j-1)*n) = Z(i, j)
    end do
  end do

  call begin_test('ilo_eq_ihi')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A', Apk, n*n)
  call print_array('B', Bpk, n*n)
  call print_array('Q', Qpk, n*n)
  call print_array('Z', Zpk, n*n)
  call end_test()

  ! ==================================================================
  ! Test 7: IHI = ILO-1 (empty range, most trivial quick return case)
  ! ==================================================================
  n = 3
  ilo = 2
  ihi = 1

  A(1,1) = 1.0d0
  A(1,2) = 2.0d0
  A(1,3) = 3.0d0
  A(2,1) = 4.0d0
  A(2,2) = 5.0d0
  A(2,3) = 6.0d0
  A(3,1) = 7.0d0
  A(3,2) = 8.0d0
  A(3,3) = 9.0d0

  B = 0.0d0
  B(1,1) = 1.0d0
  B(2,2) = 2.0d0
  B(3,3) = 3.0d0

  Q = 0.0d0
  Z = 0.0d0

  call dgghrd('I', 'I', n, ilo, ihi, A, lda, B, ldb, Q, ldq, Z, ldz, info)

  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
      Bpk(i + (j-1)*n) = B(i, j)
      Qpk(i + (j-1)*n) = Q(i, j)
      Zpk(i + (j-1)*n) = Z(i, j)
    end do
  end do

  call begin_test('empty_range')
  call print_int('info', info)
  call print_int('n', n)
  call print_array('A', Apk, n*n)
  call print_array('B', Bpk, n*n)
  call print_array('Q', Qpk, n*n)
  call print_array('Z', Zpk, n*n)
  call end_test()

end program
