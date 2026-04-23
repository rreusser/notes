program test_zsytri_3
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), E(NMAX), WORK_RK(NMAX*32)
  complex*16 :: WORK_3((NMAX+65)*67)
  double precision :: A_r(2*NMAX*NMAX), E_r(2*NMAX), WORK_3_r(2)
  equivalence (A, A_r)
  equivalence (E, E_r)
  equivalence (WORK_3, WORK_3_r)
  integer :: IPIV(NMAX), INFO, n, LWORK_RK, LWORK

  LWORK_RK = NMAX * 32
  LWORK = (NMAX+65)*67

  ! Test 1: 4x4 upper complex symmetric (1x1 pivots)
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0);   A(2,2) = (5.0d0, -1.0d0)
  A(1,3) = (3.0d0, -1.0d0);  A(2,3) = (2.0d0, 1.0d0);   A(3,3) = (6.0d0, 2.0d0)
  A(1,4) = (0.5d0, 0.5d0);   A(2,4) = (1.0d0, -2.0d0);  A(3,4) = (3.0d0, 0.0d0);  A(4,4) = (8.0d0, -1.0d0)
  call ZSYTRF_RK('U', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('4x4_upper_def')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call ZSYTRI_3('U', n, A, NMAX, E, IPIV, WORK_3, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 2: 4x4 lower complex symmetric
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0);   A(2,2) = (5.0d0, -1.0d0)
  A(3,1) = (3.0d0, -1.0d0);  A(3,2) = (2.0d0, 1.0d0);   A(3,3) = (6.0d0, 2.0d0)
  A(4,1) = (0.5d0, 0.5d0);   A(4,2) = (1.0d0, -2.0d0);  A(4,3) = (3.0d0, 0.0d0);  A(4,4) = (8.0d0, -1.0d0)
  call ZSYTRF_RK('L', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('4x4_lower_def')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call ZSYTRI_3('L', n, A, NMAX, E, IPIV, WORK_3, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 3: 4x4 upper indefinite (2x2 pivots)
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0);   A(2,2) = (0.0d0, 0.0d0)
  A(1,3) = (2.0d0, 0.0d0);   A(2,3) = (4.0d0, -1.0d0);  A(3,3) = (0.0d0, 0.0d0)
  A(1,4) = (3.0d0, 2.0d0);   A(2,4) = (5.0d0, 1.0d0);   A(3,4) = (6.0d0, 0.0d0);   A(4,4) = (0.0d0, 0.0d0)
  call ZSYTRF_RK('U', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('4x4_upper_indef')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call ZSYTRI_3('U', n, A, NMAX, E, IPIV, WORK_3, LWORK, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 4: N=1
  A(1,1) = (5.0d0, 2.0d0)
  E(1) = (0.0d0, 0.0d0)
  IPIV(1) = 1
  call ZSYTRI_3('L', 1, A, NMAX, E, IPIV, WORK_3, LWORK, INFO)
  call begin_test('n_one_lower')
  call print_array('a_inv', A_r, 2)
  call print_int('info', INFO)
  call end_test()

  ! Test 5: N=0
  call ZSYTRI_3('L', 0, A, NMAX, E, IPIV, WORK_3, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! Test 6: workspace query
  call ZSYTRI_3('L', 5, A, NMAX, E, IPIV, WORK_3, -1, INFO)
  call begin_test('lwork_query')
  call print_scalar('work1_re', WORK_3_r(1))
  call print_int('info', INFO)
  call end_test()

end program
