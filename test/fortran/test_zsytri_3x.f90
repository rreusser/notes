program test_zsytri_3x
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), E(NMAX), WORK_RK(NMAX*32)
  complex*16 :: WORK_3X((NMAX+8)*9)
  double precision :: A_r(2*NMAX*NMAX), E_r(2*NMAX)
  equivalence (A, A_r)
  equivalence (E, E_r)
  integer :: IPIV(NMAX), INFO, n, LWORK_RK, nb

  LWORK_RK = NMAX * 32

  ! Test 1: 4x4 upper (1x1 pivots), nb=2
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0);   A(2,2) = (5.0d0, -1.0d0)
  A(1,3) = (3.0d0, -1.0d0);  A(2,3) = (2.0d0, 1.0d0);   A(3,3) = (6.0d0, 2.0d0)
  A(1,4) = (0.5d0, 0.5d0);   A(2,4) = (1.0d0, -2.0d0);  A(3,4) = (3.0d0, 0.0d0);  A(4,4) = (8.0d0, -1.0d0)
  call ZSYTRF_RK('U', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('4x4_upper_def_nb2')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  nb = 2
  call ZSYTRI_3X('U', n, A, NMAX, E, IPIV, WORK_3X, nb, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 2: 4x4 lower, nb=2
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0);   A(2,2) = (5.0d0, -1.0d0)
  A(3,1) = (3.0d0, -1.0d0);  A(3,2) = (2.0d0, 1.0d0);   A(3,3) = (6.0d0, 2.0d0)
  A(4,1) = (0.5d0, 0.5d0);   A(4,2) = (1.0d0, -2.0d0);  A(4,3) = (3.0d0, 0.0d0);  A(4,4) = (8.0d0, -1.0d0)
  call ZSYTRF_RK('L', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('4x4_lower_def_nb2')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  nb = 2
  call ZSYTRI_3X('L', n, A, NMAX, E, IPIV, WORK_3X, nb, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 3: 4x4 upper indefinite (forces 2x2 pivots), nb=2
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, 1.0d0);   A(2,2) = (0.0d0, 0.0d0)
  A(1,3) = (2.0d0, 0.0d0);   A(2,3) = (4.0d0, -1.0d0);  A(3,3) = (0.0d0, 0.0d0)
  A(1,4) = (3.0d0, 2.0d0);   A(2,4) = (5.0d0, 1.0d0);   A(3,4) = (6.0d0, 0.0d0);   A(4,4) = (0.0d0, 0.0d0)
  call ZSYTRF_RK('U', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('4x4_upper_indef_nb2')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  nb = 2
  call ZSYTRI_3X('U', n, A, NMAX, E, IPIV, WORK_3X, nb, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 4: 4x4 lower indefinite, nb=2
  n = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(2,1) = (1.0d0, 1.0d0);  A(2,2) = (0.0d0, 0.0d0)
  A(3,1) = (2.0d0, 0.0d0);  A(3,2) = (4.0d0, -1.0d0); A(3,3) = (0.0d0, 0.0d0)
  A(4,1) = (3.0d0, 2.0d0);  A(4,2) = (5.0d0, 1.0d0);  A(4,3) = (6.0d0, 0.0d0); A(4,4) = (0.0d0, 0.0d0)
  call ZSYTRF_RK('L', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('4x4_lower_indef_nb2')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  nb = 2
  call ZSYTRI_3X('L', n, A, NMAX, E, IPIV, WORK_3X, nb, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 5: 5x5 lower mixed, nb=2
  n = 5
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(2,1) = (2.0d0, -1.0d0); A(2,2) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0);  A(3,2) = (3.0d0, 1.0d0); A(3,3) = (4.0d0, 0.0d0)
  A(4,1) = (1.0d0, -2.0d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (2.0d0, -1.0d0); A(4,4) = (0.0d0, 0.0d0)
  A(5,1) = (1.0d0, 0.0d0);  A(5,2) = (2.0d0, 0.0d0); A(5,3) = (0.0d0, 0.0d0);  A(5,4) = (1.0d0, -1.0d0); A(5,5) = (5.0d0, 0.0d0)
  call ZSYTRF_RK('L', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('5x5_lower_mixed_nb2')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  nb = 2
  call ZSYTRI_3X('L', n, A, NMAX, E, IPIV, WORK_3X, nb, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 6: 5x5 upper mixed, nb=3
  n = 5
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (2.0d0, 1.0d0);  A(2,2) = (0.0d0, 0.0d0)
  A(1,3) = (0.0d0, 0.0d0);  A(2,3) = (3.0d0, -1.0d0); A(3,3) = (4.0d0, 0.0d0)
  A(1,4) = (1.0d0, 2.0d0);  A(2,4) = (0.0d0, 0.0d0);  A(3,4) = (2.0d0, 1.0d0);  A(4,4) = (0.0d0, 0.0d0)
  A(1,5) = (1.0d0, 0.0d0);  A(2,5) = (2.0d0, 0.0d0);  A(3,5) = (0.0d0, 0.0d0);  A(4,5) = (1.0d0, 1.0d0); A(5,5) = (5.0d0, 0.0d0)
  call ZSYTRF_RK('U', n, A, NMAX, E, IPIV, WORK_RK, LWORK_RK, INFO)
  call begin_test('5x5_upper_mixed_nb3')
  call print_int('info_trf', INFO)
  call print_array('a_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  nb = 3
  call ZSYTRI_3X('U', n, A, NMAX, E, IPIV, WORK_3X, nb, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 7: N=1 trivial
  n = 1
  A(1,1) = (5.0d0, 2.0d0)
  E(1) = (0.0d0, 0.0d0)
  IPIV(1) = 1
  call begin_test('n_one_lower')
  nb = 2
  call ZSYTRI_3X('L', n, A, NMAX, E, IPIV, WORK_3X, nb, INFO)
  call print_int('info', INFO)
  call print_array('a_inv', A_r, 2*NMAX*n)
  call end_test()

  ! Test 8: N=0 quick return
  n = 0
  call begin_test('n_zero')
  nb = 2
  call ZSYTRI_3X('L', n, A, NMAX, E, IPIV, WORK_3X, nb, INFO)
  call print_int('info', INFO)
  call end_test()

end program
