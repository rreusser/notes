program test_dpbrfs
  use test_utils
  implicit none

  double precision :: work(100), ferr(10), berr(10)
  integer :: iwork(20), info

  ! ============================================================
  ! Test 1: upper, KD=1, 3x3, single RHS
  ! ============================================================
  ! Full SPD matrix:
  !   [4  1  0]
  !   [1  5  1]
  !   [0  1  6]
  ! Upper band storage (LDAB=2): row 0 = superdiag, row 1 = diag
  block
    double precision :: ab(2,3), afb(2,3), b(3,1), x(3,1)
    ab(1,1) = 0.0d0; ab(2,1) = 4.0d0   ! col 1
    ab(1,2) = 1.0d0; ab(2,2) = 5.0d0   ! col 2
    ab(1,3) = 1.0d0; ab(2,3) = 6.0d0   ! col 3

    afb = ab
    call dpbtrf('U', 3, 1, afb, 2, info)
    if (info /= 0) stop 'dpbtrf failed test 1'

    b(1,1) = 1.0d0; b(2,1) = 2.0d0; b(3,1) = 3.0d0

    x = b
    call dpbtrs('U', 3, 1, 1, afb, 2, x, 3, info)
    if (info /= 0) stop 'dpbtrs failed test 1'

    call dpbrfs('U', 3, 1, 1, ab, 2, afb, 2, b, 3, x, 3, &
                ferr, berr, work, iwork, info)

    call begin_test('upper_kd1_3x3')
    call print_int('info', info)
    call print_array('x', x(1:3,1), 3)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end block

  ! ============================================================
  ! Test 2: lower, KD=1, 3x3, single RHS
  ! ============================================================
  ! Same matrix, lower band storage: row 0 = diag, row 1 = subdiag
  block
    double precision :: ab(2,3), afb(2,3), b(3,1), x(3,1)
    ab(1,1) = 4.0d0; ab(2,1) = 1.0d0   ! col 1
    ab(1,2) = 5.0d0; ab(2,2) = 1.0d0   ! col 2
    ab(1,3) = 6.0d0; ab(2,3) = 0.0d0   ! col 3

    afb = ab
    call dpbtrf('L', 3, 1, afb, 2, info)
    if (info /= 0) stop 'dpbtrf failed test 2'

    b(1,1) = 1.0d0; b(2,1) = 2.0d0; b(3,1) = 3.0d0

    x = b
    call dpbtrs('L', 3, 1, 1, afb, 2, x, 3, info)
    if (info /= 0) stop 'dpbtrs failed test 2'

    call dpbrfs('L', 3, 1, 1, ab, 2, afb, 2, b, 3, x, 3, &
                ferr, berr, work, iwork, info)

    call begin_test('lower_kd1_3x3')
    call print_int('info', info)
    call print_array('x', x(1:3,1), 3)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end block

  ! ============================================================
  ! Test 3: upper, KD=2, 3x3, single RHS
  ! ============================================================
  ! Full SPD matrix:
  !   [10  2  1]
  !   [ 2 10  3]
  !   [ 1  3 10]
  ! Upper band storage (LDAB=3):
  !   row 0 = 2nd superdiag, row 1 = 1st superdiag, row 2 = diag
  block
    double precision :: ab(3,3), afb(3,3), b(3,1), x(3,1)
    ab = 0.0d0
    ab(3,1) = 10.0d0                          ! col 1: diag
    ab(2,2) = 2.0d0;  ab(3,2) = 10.0d0       ! col 2: super1, diag
    ab(1,3) = 1.0d0;  ab(2,3) = 3.0d0; ab(3,3) = 10.0d0 ! col 3: super2, super1, diag

    afb = ab
    call dpbtrf('U', 3, 2, afb, 3, info)
    if (info /= 0) stop 'dpbtrf failed test 3'

    b(1,1) = 5.0d0; b(2,1) = 7.0d0; b(3,1) = 9.0d0

    x = b
    call dpbtrs('U', 3, 2, 1, afb, 3, x, 3, info)
    if (info /= 0) stop 'dpbtrs failed test 3'

    call dpbrfs('U', 3, 2, 1, ab, 3, afb, 3, b, 3, x, 3, &
                ferr, berr, work, iwork, info)

    call begin_test('upper_kd2_3x3')
    call print_int('info', info)
    call print_array('x', x(1:3,1), 3)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end block

  ! ============================================================
  ! Test 4: upper, KD=1, 3x3, NRHS=2
  ! ============================================================
  block
    double precision :: ab(2,3), afb(2,3), b(3,2), x(3,2)
    ab(1,1) = 0.0d0; ab(2,1) = 4.0d0
    ab(1,2) = 1.0d0; ab(2,2) = 5.0d0
    ab(1,3) = 1.0d0; ab(2,3) = 6.0d0

    afb = ab
    call dpbtrf('U', 3, 1, afb, 2, info)
    if (info /= 0) stop 'dpbtrf failed test 4'

    b(1,1) = 1.0d0; b(2,1) = 2.0d0; b(3,1) = 3.0d0
    b(1,2) = 4.0d0; b(2,2) = 5.0d0; b(3,2) = 6.0d0

    x = b
    call dpbtrs('U', 3, 1, 2, afb, 2, x, 3, info)
    if (info /= 0) stop 'dpbtrs failed test 4'

    call dpbrfs('U', 3, 1, 2, ab, 2, afb, 2, b, 3, x, 3, &
                ferr, berr, work, iwork, info)

    call begin_test('upper_kd1_nrhs2')
    call print_int('info', info)
    call print_array('x', x, 6)
    call print_array('ferr', ferr, 2)
    call print_array('berr', berr, 2)
    call end_test()
  end block

  ! ============================================================
  ! Test 5: N=0 (quick return)
  ! ============================================================
  block
    double precision :: ab(1,1), afb(1,1), b(1,1), x(1,1)
    ab = 0.0d0; afb = 0.0d0; b = 0.0d0; x = 0.0d0
    ferr(1) = -1.0d0; berr(1) = -1.0d0
    call dpbrfs('U', 0, 0, 1, ab, 1, afb, 1, b, 1, x, 1, &
                ferr, berr, work, iwork, info)

    call begin_test('n_zero')
    call print_int('info', info)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end block

  ! ============================================================
  ! Test 6: N=1, KD=0
  ! ============================================================
  block
    double precision :: ab(1,1), afb(1,1), b(1,1), x(1,1)
    ab(1,1) = 4.0d0
    afb = ab
    call dpbtrf('U', 1, 0, afb, 1, info)
    if (info /= 0) stop 'dpbtrf failed test 6'

    b(1,1) = 8.0d0
    x = b
    call dpbtrs('U', 1, 0, 1, afb, 1, x, 1, info)
    if (info /= 0) stop 'dpbtrs failed test 6'

    call dpbrfs('U', 1, 0, 1, ab, 1, afb, 1, b, 1, x, 1, &
                ferr, berr, work, iwork, info)

    call begin_test('n_one')
    call print_int('info', info)
    call print_array('x', x(1:1,1), 1)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end block

  ! ============================================================
  ! Test 7: lower, KD=2, 3x3, single RHS
  ! ============================================================
  ! Same matrix as test 3 but lower storage
  ! Full SPD matrix:
  !   [10  2  1]
  !   [ 2 10  3]
  !   [ 1  3 10]
  ! Lower band storage (LDAB=3):
  !   row 0 = diag, row 1 = 1st subdiag, row 2 = 2nd subdiag
  block
    double precision :: ab(3,3), afb(3,3), b(3,1), x(3,1)
    ab = 0.0d0
    ab(1,1) = 10.0d0; ab(2,1) = 2.0d0; ab(3,1) = 1.0d0  ! col 1: diag, sub1, sub2
    ab(1,2) = 10.0d0; ab(2,2) = 3.0d0; ab(3,2) = 0.0d0  ! col 2: diag, sub1
    ab(1,3) = 10.0d0; ab(2,3) = 0.0d0; ab(3,3) = 0.0d0  ! col 3: diag

    afb = ab
    call dpbtrf('L', 3, 2, afb, 3, info)
    if (info /= 0) stop 'dpbtrf failed test 7'

    b(1,1) = 5.0d0; b(2,1) = 7.0d0; b(3,1) = 9.0d0

    x = b
    call dpbtrs('L', 3, 2, 1, afb, 3, x, 3, info)
    if (info /= 0) stop 'dpbtrs failed test 7'

    call dpbrfs('L', 3, 2, 1, ab, 3, afb, 3, b, 3, x, 3, &
                ferr, berr, work, iwork, info)

    call begin_test('lower_kd2_3x3')
    call print_int('info', info)
    call print_array('x', x(1:3,1), 3)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end block

end program
