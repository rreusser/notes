program test_dggbak
  use test_utils
  implicit none
  double precision :: v(6, 4)
  double precision :: lscale(6), rscale(6)
  integer :: info, n, m, ilo, ihi

  ! Test 1: JOB='N' quick return — no transformation
  n = 3
  m = 2
  ilo = 1
  ihi = 3
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(1,2) = 4.0d0
  v(2,2) = 5.0d0
  v(3,2) = 6.0d0
  lscale(1) = 2.0d0
  lscale(2) = 3.0d0
  lscale(3) = 4.0d0
  rscale(1) = 5.0d0
  rscale(2) = 6.0d0
  rscale(3) = 7.0d0
  call dggbak('N', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('job_n')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 2: JOB='S', SIDE='R' — scale right eigenvectors by RSCALE
  n = 3
  m = 2
  ilo = 1
  ihi = 3
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(1,2) = 4.0d0
  v(2,2) = 5.0d0
  v(3,2) = 6.0d0
  rscale(1) = 2.0d0
  rscale(2) = 3.0d0
  rscale(3) = 0.5d0
  call dggbak('S', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('scale_right')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 3: JOB='S', SIDE='L' — scale left eigenvectors by LSCALE
  n = 3
  m = 2
  ilo = 1
  ihi = 3
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(1,2) = 4.0d0
  v(2,2) = 5.0d0
  v(3,2) = 6.0d0
  lscale(1) = 2.0d0
  lscale(2) = 0.5d0
  lscale(3) = 3.0d0
  call dggbak('S', 'L', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('scale_left')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 4: JOB='P', SIDE='R' — permute right eigenvectors
  ! N=4, ILO=2, IHI=3 so rows 1 and 4 are permuted
  ! RSCALE(1) = 3 means swap row 1 <-> row 3
  ! RSCALE(4) = 2 means swap row 4 <-> row 2
  n = 4
  m = 2
  ilo = 2
  ihi = 3
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(4,1) = 4.0d0
  v(1,2) = 5.0d0
  v(2,2) = 6.0d0
  v(3,2) = 7.0d0
  v(4,2) = 8.0d0
  rscale(1) = 3.0d0
  rscale(2) = 0.0d0
  rscale(3) = 0.0d0
  rscale(4) = 2.0d0
  call dggbak('P', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('permute_right')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 5: JOB='P', SIDE='L' — permute left eigenvectors
  n = 4
  m = 2
  ilo = 2
  ihi = 3
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(4,1) = 4.0d0
  v(1,2) = 5.0d0
  v(2,2) = 6.0d0
  v(3,2) = 7.0d0
  v(4,2) = 8.0d0
  lscale(1) = 4.0d0
  lscale(2) = 0.0d0
  lscale(3) = 0.0d0
  lscale(4) = 1.0d0
  call dggbak('P', 'L', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('permute_left')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 6: JOB='B', SIDE='R' — both scale and permute, right
  n = 4
  m = 2
  ilo = 2
  ihi = 3
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(4,1) = 4.0d0
  v(1,2) = 5.0d0
  v(2,2) = 6.0d0
  v(3,2) = 7.0d0
  v(4,2) = 8.0d0
  rscale(1) = 3.0d0
  rscale(2) = 2.0d0
  rscale(3) = 0.5d0
  rscale(4) = 2.0d0
  call dggbak('B', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('both_right')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 7: JOB='B', SIDE='L' — both scale and permute, left
  n = 4
  m = 2
  ilo = 2
  ihi = 3
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(4,1) = 4.0d0
  v(1,2) = 5.0d0
  v(2,2) = 6.0d0
  v(3,2) = 7.0d0
  v(4,2) = 8.0d0
  lscale(1) = 4.0d0
  lscale(2) = 3.0d0
  lscale(3) = 0.25d0
  lscale(4) = 1.0d0
  call dggbak('B', 'L', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('both_left')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=0 quick return
  call dggbak('B', 'R', 0, 1, 0, lscale, rscale, 2, v, 6, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: M=0 quick return
  call dggbak('B', 'R', 3, 1, 3, lscale, rscale, 0, v, 6, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 10: ILO=IHI (skip scaling, permutation still applies)
  n = 4
  m = 2
  ilo = 2
  ihi = 2
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(4,1) = 4.0d0
  v(1,2) = 5.0d0
  v(2,2) = 6.0d0
  v(3,2) = 7.0d0
  v(4,2) = 8.0d0
  rscale(1) = 3.0d0
  rscale(2) = 2.0d0
  rscale(3) = 4.0d0
  rscale(4) = 1.0d0
  call dggbak('B', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('ilo_eq_ihi')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 11: ILO=1 (skip first permutation loop)
  n = 3
  m = 2
  ilo = 1
  ihi = 2
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(1,2) = 4.0d0
  v(2,2) = 5.0d0
  v(3,2) = 6.0d0
  rscale(1) = 1.0d0
  rscale(2) = 2.0d0
  rscale(3) = 1.0d0
  call dggbak('P', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('ilo_one_permute')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 12: IHI=N (skip second permutation loop)
  n = 3
  m = 2
  ilo = 2
  ihi = 3
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(1,2) = 4.0d0
  v(2,2) = 5.0d0
  v(3,2) = 6.0d0
  rscale(1) = 3.0d0
  rscale(2) = 2.0d0
  rscale(3) = 3.0d0
  call dggbak('P', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('ihi_n_permute')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 13: K=I (no-swap, CYCLE case)
  n = 3
  m = 2
  ilo = 2
  ihi = 2
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(1,2) = 4.0d0
  v(2,2) = 5.0d0
  v(3,2) = 6.0d0
  rscale(1) = 1.0d0
  rscale(2) = 2.0d0
  rscale(3) = 3.0d0
  call dggbak('P', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('k_eq_i')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 14: N=1 edge case
  n = 1
  m = 1
  ilo = 1
  ihi = 1
  v = 0.0d0
  v(1,1) = 5.0d0
  rscale(1) = 1.0d0
  call dggbak('B', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('n_one')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

  ! Test 15: larger matrix, JOB='B', SIDE='R'
  n = 5
  m = 3
  ilo = 2
  ihi = 4
  v = 0.0d0
  v(1,1) = 1.0d0
  v(2,1) = 2.0d0
  v(3,1) = 3.0d0
  v(4,1) = 4.0d0
  v(5,1) = 5.0d0
  v(1,2) = 6.0d0
  v(2,2) = 7.0d0
  v(3,2) = 8.0d0
  v(4,2) = 9.0d0
  v(5,2) = 10.0d0
  v(1,3) = 11.0d0
  v(2,3) = 12.0d0
  v(3,3) = 13.0d0
  v(4,3) = 14.0d0
  v(5,3) = 15.0d0
  rscale(1) = 4.0d0
  rscale(2) = 2.0d0
  rscale(3) = 0.5d0
  rscale(4) = 3.0d0
  rscale(5) = 1.0d0
  call dggbak('B', 'R', n, ilo, ihi, lscale, rscale, m, v, 6, info)
  call begin_test('larger_both_right')
  call print_matrix('v', v, 6, n, m)
  call print_int('info', info)
  call end_test()

end program
