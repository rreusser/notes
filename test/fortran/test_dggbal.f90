program test_dggbal
  use test_utils
  implicit none
  integer, parameter :: MAXN = 6
  double precision :: a(MAXN, MAXN), b(MAXN, MAXN)
  double precision :: lscale(MAXN), rscale(MAXN), work(6*MAXN)
  integer :: info, n, ilo, ihi

  ! ===== Test 1: JOB='N' — set ilo=1, ihi=n, scales = 1 =====
  n = 4
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(1,2) = 3.0d0
  a(2,1) = 5.0d0
  a(2,2) = 7.0d0
  a(3,3) = 9.0d0
  a(4,4) = 1.0d0
  b(1,1) = 1.0d0
  b(2,2) = 1.0d0
  b(3,3) = 1.0d0
  b(4,4) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('N', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_n')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call end_test()

  ! ===== Test 2: N=0 — quick return =====
  call dggbal('B', 0, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call end_test()

  ! ===== Test 3: N=1 — quick return with scales=1 =====
  n = 1
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 5.0d0
  b(1,1) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call end_test()

  ! ===== Test 4: JOB='P' — permute only =====
  ! 4x4 matrix with isolated eigenvalues
  n = 4
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(1,2) = 2.0d0
  a(2,2) = 3.0d0
  a(2,3) = 4.0d0
  a(3,2) = 5.0d0
  a(3,3) = 6.0d0
  a(4,3) = 7.0d0
  a(4,4) = 8.0d0
  b(1,1) = 1.0d0
  b(2,2) = 1.0d0
  b(3,3) = 1.0d0
  b(4,4) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('P', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_p')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_matrix('a', a, MAXN, n, n)
  call print_matrix('b', b, MAXN, n, n)
  call end_test()

  ! ===== Test 5: JOB='S' — scale only =====
  ! 3x3 pair with widely different magnitudes
  n = 3
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d3
  a(1,2) = 1.0d0
  a(1,3) = 1.0d-3
  a(2,1) = 1.0d0
  a(2,2) = 1.0d0
  a(2,3) = 1.0d0
  a(3,1) = 1.0d-3
  a(3,2) = 1.0d0
  a(3,3) = 1.0d3
  b(1,1) = 1.0d3
  b(1,2) = 1.0d0
  b(1,3) = 1.0d-3
  b(2,1) = 1.0d0
  b(2,2) = 1.0d0
  b(2,3) = 1.0d0
  b(3,1) = 1.0d-3
  b(3,2) = 1.0d0
  b(3,3) = 1.0d3
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('S', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_s')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_matrix('a', a, MAXN, n, n)
  call print_matrix('b', b, MAXN, n, n)
  call end_test()

  ! ===== Test 6: JOB='B' — both permute and scale =====
  ! 4x4 real pair: structured to exercise both phases
  n = 4
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(1,2) = 3.0d0
  a(1,3) = 0.1d0
  a(1,4) = 0.01d0
  a(2,1) = 4.0d0
  a(2,2) = 5.0d0
  a(2,3) = 2.0d0
  a(2,4) = 0.1d0
  a(3,1) = 0.1d0
  a(3,2) = 3.0d0
  a(3,3) = 6.0d0
  a(3,4) = 4.0d0
  a(4,1) = 0.01d0
  a(4,2) = 0.1d0
  a(4,3) = 3.0d0
  a(4,4) = 7.0d0
  b(1,1) = 1.0d0
  b(1,2) = 0.5d0
  b(1,3) = 0.01d0
  b(2,1) = 0.5d0
  b(2,2) = 1.0d0
  b(2,3) = 0.5d0
  b(2,4) = 0.01d0
  b(3,1) = 0.01d0
  b(3,2) = 0.5d0
  b(3,3) = 1.0d0
  b(3,4) = 0.5d0
  b(4,2) = 0.01d0
  b(4,3) = 0.5d0
  b(4,4) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_b')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_matrix('a', a, MAXN, n, n)
  call print_matrix('b', b, MAXN, n, n)
  call end_test()

  ! ===== Test 7: JOB='P' with isolated eigenvalues =====
  ! Row 3 has single nonzero in col 3 (diagonal only)
  n = 3
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(1,2) = 2.0d0
  a(2,1) = 3.0d0
  a(2,2) = 4.0d0
  a(3,3) = 5.0d0
  b(1,1) = 1.0d0
  b(1,2) = 0.5d0
  b(2,1) = 0.5d0
  b(2,2) = 1.0d0
  b(3,3) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('P', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_p_isolated')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_matrix('a', a, MAXN, n, n)
  call print_matrix('b', b, MAXN, n, n)
  call end_test()

  ! ===== Test 8: JOB='B' with larger matrix, 5x5 =====
  n = 5
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d2
  a(1,2) = 1.0d0
  a(1,3) = 1.0d-2
  a(2,1) = 1.0d0
  a(2,2) = 1.0d2
  a(2,3) = 1.0d0
  a(2,4) = 1.0d-2
  a(3,1) = 1.0d-2
  a(3,2) = 1.0d0
  a(3,3) = 1.0d2
  a(3,4) = 1.0d0
  a(3,5) = 1.0d-2
  a(4,2) = 1.0d-2
  a(4,3) = 1.0d0
  a(4,4) = 1.0d2
  a(4,5) = 1.0d0
  a(5,3) = 1.0d-2
  a(5,4) = 1.0d0
  a(5,5) = 1.0d2
  b(1,1) = 1.0d2
  b(2,2) = 1.0d2
  b(3,3) = 1.0d2
  b(4,4) = 1.0d2
  b(5,5) = 1.0d2
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_b_5x5')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_matrix('a', a, MAXN, n, n)
  call print_matrix('b', b, MAXN, n, n)
  call end_test()

  ! ===== Test 9: JOB='P' — fully diagonal (everything isolated) =====
  n = 3
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(2,2) = 2.0d0
  a(3,3) = 3.0d0
  b(1,1) = 1.0d0
  b(2,2) = 1.0d0
  b(3,3) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('P', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_p_diagonal')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call end_test()

  ! ===== Test 10: JOB='S' — ilo=ihi (trivial scaling) =====
  n = 2
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(2,2) = 2.0d0
  b(1,1) = 1.0d0
  b(2,2) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('S', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_s_trivial')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call end_test()

  ! ===== Test 11: JOB='B' with 2x2 dense =====
  n = 2
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(1,2) = 2.0d0
  a(2,1) = 4.0d0
  a(2,2) = 6.0d0
  b(1,1) = 1.0d0
  b(1,2) = 0.5d0
  b(2,1) = 0.5d0
  b(2,2) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_b_2x2')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_matrix('a', a, MAXN, n, n)
  call print_matrix('b', b, MAXN, n, n)
  call end_test()

  ! ===== Test 12: JOB='P', 5x5 with two isolated rows/cols =====
  n = 5
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(2,2) = 2.0d0
  a(2,3) = 3.0d0
  a(2,4) = 1.0d0
  a(3,2) = 4.0d0
  a(3,3) = 5.0d0
  a(3,4) = 2.0d0
  a(4,2) = 1.0d0
  a(4,3) = 3.0d0
  a(4,4) = 6.0d0
  a(5,5) = 7.0d0
  b(1,1) = 1.0d0
  b(2,2) = 1.0d0
  b(3,3) = 1.0d0
  b(4,4) = 1.0d0
  b(5,5) = 1.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('P', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_p_5x5')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_matrix('a', a, MAXN, n, n)
  call print_matrix('b', b, MAXN, n, n)
  call end_test()

  ! ===== Test 13: JOB='B', fully dense 3x3 =====
  n = 3
  a = 0.0d0
  b = 0.0d0
  a(1,1) = 1.0d0
  a(1,2) = 2.0d0
  a(1,3) = 4.0d0
  a(2,1) = 6.0d0
  a(2,2) = 8.0d0
  a(2,3) = 10.0d0
  a(3,1) = 12.0d0
  a(3,2) = 14.0d0
  a(3,3) = 16.0d0
  b(1,1) = 1.0d0
  b(1,2) = 2.0d0
  b(1,3) = 3.0d0
  b(2,1) = 4.0d0
  b(2,2) = 5.0d0
  b(2,3) = 6.0d0
  b(3,1) = 7.0d0
  b(3,2) = 8.0d0
  b(3,3) = 9.0d0
  lscale = 0.0d0
  rscale = 0.0d0
  call dggbal('B', n, a, MAXN, b, MAXN, ilo, ihi, lscale, rscale, work, info)
  call begin_test('job_b_dense')
  call print_int('info', info)
  call print_int('ilo', ilo)
  call print_int('ihi', ihi)
  call print_array('lscale', lscale, n)
  call print_array('rscale', rscale, n)
  call print_matrix('a', a, MAXN, n, n)
  call print_matrix('b', b, MAXN, n, n)
  call end_test()

end program
