program test_zpbrfs
  use test_utils
  implicit none

  call test_upper_kd1_3x3()
  call test_lower_kd1_3x3()
  call test_upper_kd2_3x3()
  call test_upper_kd1_nrhs2()
  call test_n_zero()
  call test_n_one()
  call test_lower_kd2_3x3()

contains

  subroutine test_upper_kd1_3x3()
    complex*16 :: ab(2,3), afb(2,3), b(3,1), x(3,1), work(200)
    double precision :: ab_r(12), afb_r(12), b_r(6), x_r(6)
    double precision :: ferr(1), berr(1), rwork(100)
    equivalence (ab, ab_r)
    equivalence (afb, afb_r)
    equivalence (b, b_r)
    equivalence (x, x_r)
    integer :: info

    ab = (0.0d0, 0.0d0)
    ab(2,1) = (4.0d0, 0.0d0)
    ab(1,2) = (1.0d0, 1.0d0); ab(2,2) = (5.0d0, 0.0d0)
    ab(1,3) = (2.0d0, -1.0d0); ab(2,3) = (6.0d0, 0.0d0)

    afb = ab
    call zpbtrf('U', 3, 1, afb, 2, info)
    if (info /= 0) stop 'zpbtrf failed test 1'

    b(1,1) = (4.0d0, 2.0d0)
    b(2,1) = (10.0d0, 2.0d0)
    b(3,1) = (13.0d0, 3.0d0)

    x = b
    call zpbtrs('U', 3, 1, 1, afb, 2, x, 3, info)
    if (info /= 0) stop 'zpbtrs failed test 1'

    call zpbrfs('U', 3, 1, 1, ab, 2, afb, 2, b, 3, x, 3, &
                ferr, berr, work, rwork, info)

    call begin_test('upper_kd1_3x3')
    call print_int('info', info)
    call print_array('ab', ab_r, 12)
    call print_array('afb', afb_r, 12)
    call print_array('b', b_r, 6)
    call print_array('x', x_r, 6)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end subroutine

  subroutine test_lower_kd1_3x3()
    complex*16 :: ab(2,3), afb(2,3), b(3,1), x(3,1), work(200)
    double precision :: ab_r(12), afb_r(12), b_r(6), x_r(6)
    double precision :: ferr(1), berr(1), rwork(100)
    equivalence (ab, ab_r)
    equivalence (afb, afb_r)
    equivalence (b, b_r)
    equivalence (x, x_r)
    integer :: info

    ab = (0.0d0, 0.0d0)
    ab(1,1) = (4.0d0, 0.0d0); ab(2,1) = (1.0d0, -1.0d0)
    ab(1,2) = (5.0d0, 0.0d0); ab(2,2) = (2.0d0, 1.0d0)
    ab(1,3) = (6.0d0, 0.0d0)

    afb = ab
    call zpbtrf('L', 3, 1, afb, 2, info)
    if (info /= 0) stop 'zpbtrf failed test 2'

    b(1,1) = (4.0d0, 2.0d0)
    b(2,1) = (10.0d0, 2.0d0)
    b(3,1) = (13.0d0, 3.0d0)

    x = b
    call zpbtrs('L', 3, 1, 1, afb, 2, x, 3, info)
    if (info /= 0) stop 'zpbtrs failed test 2'

    call zpbrfs('L', 3, 1, 1, ab, 2, afb, 2, b, 3, x, 3, &
                ferr, berr, work, rwork, info)

    call begin_test('lower_kd1_3x3')
    call print_int('info', info)
    call print_array('ab', ab_r, 12)
    call print_array('afb', afb_r, 12)
    call print_array('b', b_r, 6)
    call print_array('x', x_r, 6)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end subroutine

  subroutine test_upper_kd2_3x3()
    complex*16 :: ab(3,3), afb(3,3), b(3,1), x(3,1), work(200)
    double precision :: ab_r(18), afb_r(18), b_r(6), x_r(6)
    double precision :: ferr(1), berr(1), rwork(100)
    equivalence (ab, ab_r)
    equivalence (afb, afb_r)
    equivalence (b, b_r)
    equivalence (x, x_r)
    integer :: info

    ab = (0.0d0, 0.0d0)
    ab(3,1) = (10.0d0, 0.0d0)
    ab(2,2) = (2.0d0, 1.0d0); ab(3,2) = (10.0d0, 0.0d0)
    ab(1,3) = (1.0d0, -1.0d0); ab(2,3) = (3.0d0, 2.0d0); ab(3,3) = (10.0d0, 0.0d0)

    afb = ab
    call zpbtrf('U', 3, 2, afb, 3, info)
    if (info /= 0) stop 'zpbtrf failed test 3'

    b = (0.0d0, 0.0d0)
    b(1,1) = (16.0d0, 1.0d0)
    b(2,1) = (31.0d0, 15.0d0)
    b(3,1) = (39.0d0, 0.0d0)

    x = b
    call zpbtrs('U', 3, 2, 1, afb, 3, x, 3, info)
    if (info /= 0) stop 'zpbtrs failed test 3'

    call zpbrfs('U', 3, 2, 1, ab, 3, afb, 3, b, 3, x, 3, &
                ferr, berr, work, rwork, info)

    call begin_test('upper_kd2_3x3')
    call print_int('info', info)
    call print_array('ab', ab_r, 18)
    call print_array('afb', afb_r, 18)
    call print_array('b', b_r, 6)
    call print_array('x', x_r, 6)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end subroutine

  subroutine test_upper_kd1_nrhs2()
    complex*16 :: ab(2,3), afb(2,3), b(3,2), x(3,2), work(200)
    double precision :: ab_r(12), afb_r(12), b_r(12), x_r(12)
    double precision :: ferr(2), berr(2), rwork(100)
    equivalence (ab, ab_r)
    equivalence (afb, afb_r)
    equivalence (b, b_r)
    equivalence (x, x_r)
    integer :: info

    ab = (0.0d0, 0.0d0)
    ab(2,1) = (4.0d0, 0.0d0)
    ab(1,2) = (1.0d0, 1.0d0); ab(2,2) = (5.0d0, 0.0d0)
    ab(1,3) = (2.0d0, -1.0d0); ab(2,3) = (6.0d0, 0.0d0)

    afb = ab
    call zpbtrf('U', 3, 1, afb, 2, info)
    if (info /= 0) stop 'zpbtrf failed test 4'

    b(1,1) = (4.0d0, 2.0d0)
    b(2,1) = (10.0d0, 2.0d0)
    b(3,1) = (13.0d0, 3.0d0)
    b(1,2) = (0.0d0, 4.0d0)
    b(2,2) = (2.0d0, -2.0d0)
    b(3,2) = (6.0d0, -6.0d0)

    x = b
    call zpbtrs('U', 3, 1, 2, afb, 2, x, 3, info)
    if (info /= 0) stop 'zpbtrs failed test 4'

    call zpbrfs('U', 3, 1, 2, ab, 2, afb, 2, b, 3, x, 3, &
                ferr, berr, work, rwork, info)

    call begin_test('upper_kd1_nrhs2')
    call print_int('info', info)
    call print_array('ab', ab_r, 12)
    call print_array('afb', afb_r, 12)
    call print_array('b', b_r, 12)
    call print_array('x', x_r, 12)
    call print_array('ferr', ferr, 2)
    call print_array('berr', berr, 2)
    call end_test()
  end subroutine

  subroutine test_n_zero()
    complex*16 :: ab(1,1), afb(1,1), b(1,1), x(1,1), work(10)
    double precision :: ferr(1), berr(1), rwork(10)
    integer :: info

    ferr(1) = -1.0d0; berr(1) = -1.0d0
    ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0)
    b = (0.0d0, 0.0d0); x = (0.0d0, 0.0d0)
    call zpbrfs('U', 0, 0, 1, ab, 1, afb, 1, b, 1, x, 1, &
                ferr, berr, work, rwork, info)

    call begin_test('n_zero')
    call print_int('info', info)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end subroutine

  subroutine test_n_one()
    complex*16 :: ab(1,1), afb(1,1), b(1,1), x(1,1), work(200)
    double precision :: ab_r(2), afb_r(2), b_r(2), x_r(2)
    double precision :: ferr(1), berr(1), rwork(100)
    equivalence (ab, ab_r)
    equivalence (afb, afb_r)
    equivalence (b, b_r)
    equivalence (x, x_r)
    integer :: info

    ab(1,1) = (4.0d0, 0.0d0)
    afb = ab
    call zpbtrf('U', 1, 0, afb, 1, info)
    if (info /= 0) stop 'zpbtrf failed test 6'

    b(1,1) = (8.0d0, 4.0d0)
    x = b
    call zpbtrs('U', 1, 0, 1, afb, 1, x, 1, info)
    if (info /= 0) stop 'zpbtrs failed test 6'

    call zpbrfs('U', 1, 0, 1, ab, 1, afb, 1, b, 1, x, 1, &
                ferr, berr, work, rwork, info)

    call begin_test('n_one')
    call print_int('info', info)
    call print_array('x', x_r, 2)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end subroutine

  subroutine test_lower_kd2_3x3()
    complex*16 :: ab(3,3), afb(3,3), b(3,1), x(3,1), work(200)
    double precision :: ab_r(18), afb_r(18), b_r(6), x_r(6)
    double precision :: ferr(1), berr(1), rwork(100)
    equivalence (ab, ab_r)
    equivalence (afb, afb_r)
    equivalence (b, b_r)
    equivalence (x, x_r)
    integer :: info

    ab = (0.0d0, 0.0d0)
    ab(1,1) = (10.0d0, 0.0d0); ab(2,1) = (2.0d0, -1.0d0); ab(3,1) = (1.0d0, 1.0d0)
    ab(1,2) = (10.0d0, 0.0d0); ab(2,2) = (3.0d0, -2.0d0)
    ab(1,3) = (10.0d0, 0.0d0)

    afb = ab
    call zpbtrf('L', 3, 2, afb, 3, info)
    if (info /= 0) stop 'zpbtrf failed test 7'

    b = (0.0d0, 0.0d0)
    b(1,1) = (16.0d0, 1.0d0)
    b(2,1) = (31.0d0, 15.0d0)
    b(3,1) = (39.0d0, 0.0d0)

    x = b
    call zpbtrs('L', 3, 2, 1, afb, 3, x, 3, info)
    if (info /= 0) stop 'zpbtrs failed test 7'

    call zpbrfs('L', 3, 2, 1, ab, 3, afb, 3, b, 3, x, 3, &
                ferr, berr, work, rwork, info)

    call begin_test('lower_kd2_3x3')
    call print_int('info', info)
    call print_array('ab', ab_r, 18)
    call print_array('afb', afb_r, 18)
    call print_array('b', b_r, 6)
    call print_array('x', x_r, 6)
    call print_array('ferr', ferr, 1)
    call print_array('berr', berr, 1)
    call end_test()
  end subroutine

end program
