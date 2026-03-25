program test_zheevd
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  double precision :: a_r(2*NMAX*NMAX), work_r(2*(2*NMAX+NMAX*NMAX))
  double precision :: w(NMAX), rwork(1+5*NMAX+2*NMAX*NMAX)
  complex*16 :: a(NMAX*NMAX), work(2*NMAX+NMAX*NMAX)
  equivalence (a, a_r)
  equivalence (work, work_r)
  integer :: iwork(3+5*NMAX), info
  integer :: lwork, lrwork, liwork, n

  ! Test 1: JOBZ='N', UPLO='L', 3x3 Hermitian matrix (eigenvalues only)
  n = 3
  lwork = n + 1
  lrwork = n
  liwork = 1
  a = (0.0d0, 0.0d0)
  ! Hermitian: A(i,j) = conj(A(j,i))
  ! Column-major packing with LDA=3
  a(1) = (4.0d0, 0.0d0); a(2) = (1.0d0, -2.0d0); a(3) = (2.0d0, 1.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (3.0d0, 0.0d0);  a(6) = (0.0d0, -1.0d0)
  a(7) = (2.0d0, -1.0d0); a(8) = (0.0d0, 1.0d0);  a(9) = (5.0d0, 0.0d0)
  call ZHEEVD('N', 'L', n, a, n, w, work, lwork, rwork, lrwork, iwork, liwork, info)
  call begin_test('eigenvalues_only_lower_3x3')
  call print_array('w', w, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: JOBZ='N', UPLO='U', 3x3 Hermitian matrix
  n = 3
  lwork = n + 1
  lrwork = n
  liwork = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(2) = (1.0d0, -2.0d0); a(3) = (2.0d0, 1.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (3.0d0, 0.0d0);  a(6) = (0.0d0, -1.0d0)
  a(7) = (2.0d0, -1.0d0); a(8) = (0.0d0, 1.0d0);  a(9) = (5.0d0, 0.0d0)
  call ZHEEVD('N', 'U', n, a, n, w, work, lwork, rwork, lrwork, iwork, liwork, info)
  call begin_test('eigenvalues_only_upper_3x3')
  call print_array('w', w, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: JOBZ='V', UPLO='L', 3x3 Hermitian matrix (eigenvalues + eigenvectors)
  n = 3
  lwork = 2*n + n*n
  lrwork = 1 + 5*n + 2*n*n
  liwork = 3 + 5*n
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0); a(2) = (1.0d0, -2.0d0); a(3) = (2.0d0, 1.0d0)
  a(4) = (1.0d0, 2.0d0); a(5) = (3.0d0, 0.0d0);  a(6) = (0.0d0, -1.0d0)
  a(7) = (2.0d0, -1.0d0); a(8) = (0.0d0, 1.0d0);  a(9) = (5.0d0, 0.0d0)
  call ZHEEVD('V', 'L', n, a, n, w, work, lwork, rwork, lrwork, iwork, liwork, info)
  call begin_test('eigenvectors_lower_3x3')
  call print_array('w', w, n)
  call print_array('a', a_r, 2*n*n)
  call print_int('info', info)
  call end_test()

  ! Test 4: JOBZ='V', UPLO='U', 4x4 Hermitian matrix
  n = 4
  lwork = 2*n + n*n
  lrwork = 1 + 5*n + 2*n*n
  liwork = 3 + 5*n
  a = (0.0d0, 0.0d0)
  a(1) = (6.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 0.0d0); a(4) = (0.0d0, 1.0d0)
  a(5) = (2.0d0, 1.0d0); a(6) = (5.0d0, 0.0d0);  a(7) = (3.0d0, -2.0d0); a(8) = (1.0d0, 0.0d0)
  a(9) = (1.0d0, 0.0d0); a(10) = (3.0d0, 2.0d0); a(11) = (7.0d0, 0.0d0); a(12) = (2.0d0, -1.0d0)
  a(13) = (0.0d0, -1.0d0); a(14) = (1.0d0, 0.0d0); a(15) = (2.0d0, 1.0d0); a(16) = (4.0d0, 0.0d0)
  call ZHEEVD('V', 'U', n, a, n, w, work, lwork, rwork, lrwork, iwork, liwork, info)
  call begin_test('eigenvectors_upper_4x4')
  call print_array('w', w, n)
  call print_array('a', a_r, 2*n*n)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 (quick return)
  call ZHEEVD('N', 'L', 0, a, 1, w, work, 1, rwork, 1, iwork, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1 (quick return, eigenvalue = real part of A(1,1))
  n = 1
  a(1) = (7.5d0, 3.0d0)
  call ZHEEVD('V', 'L', n, a, n, w, work, 1, rwork, 1, iwork, 1, info)
  call begin_test('n_one')
  call print_array('w', w, 1)
  call print_array('a', a_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 7: 2x2 diagonal Hermitian matrix (eigenvalues only)
  n = 2
  lwork = n + 1
  lrwork = n
  liwork = 1
  a = (0.0d0, 0.0d0)
  a(1) = (3.0d0, 0.0d0); a(2) = (0.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0); a(4) = (8.0d0, 0.0d0)
  call ZHEEVD('N', 'L', n, a, n, w, work, lwork, rwork, lrwork, iwork, liwork, info)
  call begin_test('diagonal_2x2')
  call print_array('w', w, n)
  call print_int('info', info)
  call end_test()

end program
