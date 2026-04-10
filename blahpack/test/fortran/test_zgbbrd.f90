program test_zgbbrd
  use test_utils
  implicit none

  ! Reusable storage flat enough for all sub-tests
  complex*16 :: AB(10, 10), Q(10, 10), PT(10, 10), C(10, 10)
  complex*16 :: WORK(40)
  double precision :: AB_r(200), Q_r(200), PT_r(200), C_r(200)
  double precision :: D(10), E(10), RWORK(40)
  equivalence (AB, AB_r)
  equivalence (Q, Q_r)
  equivalence (PT, PT_r)
  equivalence (C, C_r)
  integer :: info, m, n, kl, ku, ldab, ncc

  ! ============================================================
  ! Test 1: 5x5 complex tridiagonal (kl=1, ku=1), vect='N'
  m = 5; n = 5; kl = 1; ku = 1; ldab = 3; ncc = 0
  AB = (0.0d0, 0.0d0)
  ! Col 1
  AB(2,1) = ( 4.0d0,  0.5d0); AB(3,1) = (-1.0d0,  0.2d0)
  ! Col 2
  AB(1,2) = (-1.0d0, -0.2d0); AB(2,2) = ( 4.0d0, -0.3d0); AB(3,2) = (-1.0d0,  0.1d0)
  ! Col 3
  AB(1,3) = (-1.0d0,  0.4d0); AB(2,3) = ( 4.0d0,  0.0d0); AB(3,3) = (-1.0d0, -0.5d0)
  ! Col 4
  AB(1,4) = (-1.0d0, -0.1d0); AB(2,4) = ( 4.0d0,  0.6d0); AB(3,4) = (-1.0d0,  0.3d0)
  ! Col 5
  AB(1,5) = (-1.0d0,  0.2d0); AB(2,5) = ( 4.0d0, -0.4d0)
  D = 0.0d0; E = 0.0d0
  call ZGBBRD('N', m, n, ncc, kl, ku, AB, 10, D, E, Q, 10, PT, 10, C, 10, WORK, RWORK, info)
  call begin_test('tri_5x5_N')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('AB', AB_r, 20, 2*ldab, n)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: 5x5 complex pentadiagonal (kl=2, ku=2), vect='B'
  m = 5; n = 5; kl = 2; ku = 2; ldab = 5; ncc = 0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); PT = (0.0d0, 0.0d0)
  AB(3,1) = ( 6.0d0, 0.0d0); AB(4,1) = (-2.0d0, 0.5d0); AB(5,1) = ( 1.0d0, 0.1d0)
  AB(2,2) = (-2.0d0,-0.5d0); AB(3,2) = ( 6.0d0, 0.2d0); AB(4,2) = (-2.0d0,-0.3d0); AB(5,2) = ( 1.0d0, 0.4d0)
  AB(1,3) = ( 1.0d0,-0.1d0); AB(2,3) = (-2.0d0, 0.3d0); AB(3,3) = ( 6.0d0,-0.2d0); AB(4,3) = (-2.0d0, 0.1d0); AB(5,3) = ( 1.0d0,-0.5d0)
  AB(1,4) = ( 1.0d0,-0.4d0); AB(2,4) = (-2.0d0,-0.1d0); AB(3,4) = ( 6.0d0, 0.3d0); AB(4,4) = (-2.0d0, 0.2d0)
  AB(1,5) = ( 1.0d0, 0.5d0); AB(2,5) = (-2.0d0,-0.2d0); AB(3,5) = ( 6.0d0, 0.6d0)
  D = 0.0d0; E = 0.0d0
  call ZGBBRD('B', m, n, ncc, kl, ku, AB, 10, D, E, Q, 10, PT, 10, C, 10, WORK, RWORK, info)
  call begin_test('penta_5x5_B')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('Q', Q_r, 20, 2*m, m)
  call print_matrix('PT', PT_r, 20, 2*n, n)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: 6x4 tall complex tridiagonal (kl=1, ku=1), vect='Q', ncc=2
  m = 6; n = 4; kl = 1; ku = 1; ldab = 3; ncc = 2
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); C = (0.0d0, 0.0d0)
  AB(2,1) = ( 3.0d0, 0.1d0); AB(3,1) = (-1.0d0, 0.2d0)
  AB(1,2) = (-1.0d0,-0.2d0); AB(2,2) = ( 3.0d0,-0.3d0); AB(3,2) = (-1.0d0, 0.4d0)
  AB(1,3) = (-1.0d0,-0.4d0); AB(2,3) = ( 3.0d0, 0.5d0); AB(3,3) = (-1.0d0,-0.1d0)
  AB(1,4) = (-1.0d0, 0.1d0); AB(2,4) = ( 3.0d0, 0.0d0); AB(3,4) = (-1.0d0, 0.3d0)
  C(1,1) = ( 1.0d0, 0.1d0); C(1,2) = ( 2.0d0,-0.1d0)
  C(2,1) = ( 3.0d0,-0.2d0); C(2,2) = ( 4.0d0, 0.2d0)
  C(3,1) = ( 5.0d0, 0.3d0); C(3,2) = ( 6.0d0,-0.3d0)
  C(4,1) = ( 7.0d0,-0.4d0); C(4,2) = ( 8.0d0, 0.4d0)
  C(5,1) = ( 9.0d0, 0.5d0); C(5,2) = (10.0d0,-0.5d0)
  C(6,1) = (11.0d0,-0.6d0); C(6,2) = (12.0d0, 0.6d0)
  D = 0.0d0; E = 0.0d0
  call ZGBBRD('Q', m, n, ncc, kl, ku, AB, 10, D, E, Q, 10, PT, 10, C, 10, WORK, RWORK, info)
  call begin_test('tall_6x4_Q')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('Q', Q_r, 20, 2*m, m)
  call print_matrix('C', C_r, 20, 2*m, ncc)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: 4x6 wide complex bidiagonal (kl=0, ku=1), vect='P'
  m = 4; n = 6; kl = 0; ku = 1; ldab = 2; ncc = 0
  AB = (0.0d0, 0.0d0); PT = (0.0d0, 0.0d0)
  AB(2,1) = ( 2.0d0, 0.1d0)
  AB(1,2) = ( 1.0d0,-0.2d0); AB(2,2) = ( 3.0d0, 0.3d0)
  AB(1,3) = ( 1.0d0, 0.4d0); AB(2,3) = ( 4.0d0,-0.5d0)
  AB(1,4) = ( 1.0d0,-0.1d0); AB(2,4) = ( 5.0d0, 0.2d0)
  AB(1,5) = ( 1.0d0, 0.6d0)
  AB(1,6) = ( 1.0d0,-0.3d0)
  D = 0.0d0; E = 0.0d0
  call ZGBBRD('P', m, n, ncc, kl, ku, AB, 10, D, E, Q, 10, PT, 10, C, 10, WORK, RWORK, info)
  call begin_test('wide_4x6_P')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('PT', PT_r, 20, 2*n, n)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: 4x4 complex lower bidiagonal (kl=1, ku=0), vect='B'
  m = 4; n = 4; kl = 1; ku = 0; ldab = 2; ncc = 0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); PT = (0.0d0, 0.0d0)
  AB(1,1) = ( 2.0d0, 0.1d0); AB(2,1) = (-1.0d0, 0.2d0)
  AB(1,2) = ( 3.0d0,-0.2d0); AB(2,2) = (-1.0d0, 0.3d0)
  AB(1,3) = ( 4.0d0, 0.4d0); AB(2,3) = (-1.0d0,-0.4d0)
  AB(1,4) = ( 5.0d0,-0.1d0)
  D = 0.0d0; E = 0.0d0
  call ZGBBRD('B', m, n, ncc, kl, ku, AB, 10, D, E, Q, 10, PT, 10, C, 10, WORK, RWORK, info)
  call begin_test('lower_4x4_B')
  call print_array('D', D, min(m,n))
  call print_array('E', E, min(m,n)-1)
  call print_matrix('Q', Q_r, 20, 2*m, m)
  call print_matrix('PT', PT_r, 20, 2*n, n)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: 4x4 complex diagonal only (kl=0, ku=0), vect='N'
  m = 4; n = 4; kl = 0; ku = 0; ldab = 1; ncc = 0
  AB = (0.0d0, 0.0d0)
  AB(1,1) = ( 2.5d0, 0.5d0)
  AB(1,2) = (-1.5d0,-0.3d0)
  AB(1,3) = ( 3.5d0, 0.2d0)
  AB(1,4) = ( 4.5d0,-0.6d0)
  D = 0.0d0; E = 0.0d0
  call ZGBBRD('N', m, n, ncc, kl, ku, AB, 10, D, E, Q, 10, PT, 10, C, 10, WORK, RWORK, info)
  call begin_test('diag_4x4_N')
  call print_array('D', D, min(m,n))
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: m=0 quick return
  m = 0; n = 4; kl = 1; ku = 1; ldab = 3; ncc = 0
  AB = (0.0d0, 0.0d0)
  call ZGBBRD('N', m, n, ncc, kl, ku, AB, 10, D, E, Q, 10, PT, 10, C, 10, WORK, RWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

end program
