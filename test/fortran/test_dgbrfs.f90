program test_dgbrfs
  use test_utils
  implicit none
  ! Arrays — LDAB=6 covers KL+KU+1 up to 6 (e.g. KL=2,KU=1: 4 rows needed)
  ! LDAFB=6 covers 2*KL+KU+1 up to 6 (e.g. KL=2,KU=1: 6 rows)
  ! N_MAX=4, NRHS_MAX=2
  double precision :: ab(6,4), afb(6,4), b(4,2), x(4,2)
  double precision :: ferr(4), berr(4), work(12), iwork_r(4)
  integer :: ipiv(4), iwork(4), info, n, kl, ku, nrhs

  ! ============================================================
  ! Test 1: 4x4 tridiag (KL=1, KU=1), single RHS, no-transpose
  n = 4; kl = 1; ku = 1; nrhs = 1
  ab = 0.0d0; afb = 0.0d0
  ! AB: LDAB=6, original band, row KU+1=2 is diagonal
  ab(2,1) = 4.0d0;  ab(3,1) = -1.0d0
  ab(1,2) = 0.5d0;  ab(2,2) = 4.0d0;  ab(3,2) = -1.0d0
  ab(1,3) = 0.5d0;  ab(2,3) = 4.0d0;  ab(3,3) = -1.0d0
  ab(1,4) = 0.5d0;  ab(2,4) = 4.0d0

  b = 0.0d0
  b(1,1) = 1.0d0
  b(2,1) = 2.0d0
  b(3,1) = 3.0d0
  b(4,1) = 4.0d0

  ! AFB: copy original band into factored storage
  ! For KL=1,KU=1: original rows go into AFB rows KL+1..2*KL+KU+1 = 2..4
  afb = 0.0d0
  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call DGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  x = 0.0d0
  x(1:4,1) = b(1:4,1)
  call DGBTRS('N', n, kl, ku, nrhs, afb, 6, ipiv, x, 4, info)

  call DGBRFS('N', n, kl, ku, nrhs, ab, 6, afb, 6, ipiv, b, 4, x, 4, &
              ferr, berr, work, iwork, info)
  call begin_test('tridiag_notrans')
  call print_array('x', x(1:4,1), 4)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: Same system, transpose
  ab = 0.0d0; afb = 0.0d0
  ab(2,1) = 4.0d0;  ab(3,1) = -1.0d0
  ab(1,2) = 0.5d0;  ab(2,2) = 4.0d0;  ab(3,2) = -1.0d0
  ab(1,3) = 0.5d0;  ab(2,3) = 4.0d0;  ab(3,3) = -1.0d0
  ab(1,4) = 0.5d0;  ab(2,4) = 4.0d0

  b = 0.0d0
  b(1,1) = 1.0d0
  b(2,1) = 2.0d0
  b(3,1) = 3.0d0
  b(4,1) = 4.0d0

  afb = 0.0d0
  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call DGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  x = 0.0d0
  x(1:4,1) = b(1:4,1)
  call DGBTRS('T', n, kl, ku, nrhs, afb, 6, ipiv, x, 4, info)

  call DGBRFS('T', n, kl, ku, nrhs, ab, 6, afb, 6, ipiv, b, 4, x, 4, &
              ferr, berr, work, iwork, info)
  call begin_test('tridiag_trans')
  call print_array('x', x(1:4,1), 4)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: Multiple RHS (NRHS=2)
  ab = 0.0d0; afb = 0.0d0
  ab(2,1) = 4.0d0;  ab(3,1) = -1.0d0
  ab(1,2) = 0.5d0;  ab(2,2) = 4.0d0;  ab(3,2) = -1.0d0
  ab(1,3) = 0.5d0;  ab(2,3) = 4.0d0;  ab(3,3) = -1.0d0
  ab(1,4) = 0.5d0;  ab(2,4) = 4.0d0

  b = 0.0d0
  b(1,1) = 1.0d0; b(2,1) = 2.0d0
  b(3,1) = 3.0d0; b(4,1) = 4.0d0
  b(1,2) = 0.5d0; b(2,2) = 1.5d0
  b(3,2) = -1.0d0; b(4,2) = 2.0d0

  nrhs = 2
  afb = 0.0d0
  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call DGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  x = 0.0d0
  x(1:4,1) = b(1:4,1); x(1:4,2) = b(1:4,2)
  call DGBTRS('N', n, kl, ku, nrhs, afb, 6, ipiv, x, 4, info)

  call DGBRFS('N', n, kl, ku, nrhs, ab, 6, afb, 6, ipiv, b, 4, x, 4, &
              ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x(1:4,1), 4)
  call print_array('x2', x(1:4,2), 4)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 quick return
  nrhs = 1
  ferr = 0.0d0; berr = 0.0d0
  call DGBRFS('N', 0, 0, 0, 1, ab, 6, afb, 6, ipiv, b, 4, x, 4, &
              ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: NRHS=0 quick return
  call DGBRFS('N', 4, 1, 1, 0, ab, 6, afb, 6, ipiv, b, 4, x, 4, &
              ferr, berr, work, iwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: 4x4 with KL=2, KU=1
  n = 4; kl = 2; ku = 1; nrhs = 1
  ab = 0.0d0; afb = 0.0d0
  ! AB: KL+KU+1=4 rows used. Row KU+1=2 is diagonal.
  ab(2,1) = 5.0d0; ab(3,1) = 2.0d0; ab(4,1) = 1.0d0
  ab(1,2) = 1.0d0; ab(2,2) = 6.0d0; ab(3,2) = 1.0d0; ab(4,2) = 2.0d0
  ab(1,3) = 2.0d0; ab(2,3) = 7.0d0; ab(3,3) = 3.0d0
  ab(1,4) = 1.0d0; ab(2,4) = 8.0d0

  b = 0.0d0
  b(1,1) = 1.0d0; b(2,1) = 2.0d0
  b(3,1) = 3.0d0; b(4,1) = 4.0d0

  ! AFB: LDAFB=6 = 2*KL+KU+1 = 6. Copy into rows KL+1..2*KL+KU+1 = 3..6
  afb = 0.0d0
  afb(3,1) = ab(1,1); afb(4,1) = ab(2,1); afb(5,1) = ab(3,1); afb(6,1) = ab(4,1)
  afb(3,2) = ab(1,2); afb(4,2) = ab(2,2); afb(5,2) = ab(3,2); afb(6,2) = ab(4,2)
  afb(3,3) = ab(1,3); afb(4,3) = ab(2,3); afb(5,3) = ab(3,3)
  afb(3,4) = ab(1,4); afb(4,4) = ab(2,4)

  call DGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  x = 0.0d0
  x(1:4,1) = b(1:4,1)
  call DGBTRS('N', n, kl, ku, nrhs, afb, 6, ipiv, x, 4, info)

  call DGBRFS('N', n, kl, ku, nrhs, ab, 6, afb, 6, ipiv, b, 4, x, 4, &
              ferr, berr, work, iwork, info)
  call begin_test('kl2_ku1')
  call print_array('x', x(1:4,1), 4)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: 1x1 system
  n = 1; kl = 0; ku = 0; nrhs = 1
  ab = 0.0d0; afb = 0.0d0
  ab(1,1) = 3.0d0
  afb(1,1) = 3.0d0

  b = 0.0d0
  b(1,1) = 5.0d0

  call DGBTRF(n, n, kl, ku, afb, 6, ipiv, info)
  x = 0.0d0
  x(1,1) = b(1,1)
  call DGBTRS('N', n, kl, ku, nrhs, afb, 6, ipiv, x, 4, info)

  call DGBRFS('N', n, kl, ku, nrhs, ab, 6, afb, 6, ipiv, b, 4, x, 4, &
              ferr, berr, work, iwork, info)
  call begin_test('one_by_one')
  call print_array('x', x(1:1,1), 1)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

end program
