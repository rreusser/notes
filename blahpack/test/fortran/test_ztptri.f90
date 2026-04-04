program test_ztptri
  use test_utils
  implicit none

  call test_upper_nonunit_3()
  call test_lower_nonunit_3()
  call test_upper_unit_3()
  call test_lower_unit_3()
  call test_n0()
  call test_n1()
  call test_upper_nonunit_4()
  call test_lower_nonunit_4()
  call test_singular_upper()
  call test_singular_lower()

contains

  subroutine test_upper_nonunit_3()
    ! Upper packed 3x3, non-unit diagonal
    ! A = [2+1i  1+2i  3+0i;  0  4+1i  5-1i;  0  0  6+2i]
    ! Upper packed (col-major): [2+1i, 1+2i, 4+1i, 3+0i, 5-1i, 6+2i]
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (2.0d0, 1.0d0)
    ap(2) = (1.0d0, 2.0d0); ap(3) = (4.0d0, 1.0d0)
    ap(4) = (3.0d0, 0.0d0); ap(5) = (5.0d0, -1.0d0); ap(6) = (6.0d0, 2.0d0)
    call ztptri('U', 'N', 3, ap, info)
    call begin_test('upper_nonunit_3')
    call print_int('info', info)
    call print_array('ap', ap_r, 12)
    call end_test()
  end subroutine

  subroutine test_lower_nonunit_3()
    ! Lower packed 3x3, non-unit diagonal
    ! L = [2+1i  0  0;  1+2i  4+1i  0;  3+0i  5-1i  6+2i]
    ! Lower packed (col-major): [2+1i, 1+2i, 3+0i, 4+1i, 5-1i, 6+2i]
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (2.0d0, 1.0d0); ap(2) = (1.0d0, 2.0d0); ap(3) = (3.0d0, 0.0d0)
    ap(4) = (4.0d0, 1.0d0); ap(5) = (5.0d0, -1.0d0)
    ap(6) = (6.0d0, 2.0d0)
    call ztptri('L', 'N', 3, ap, info)
    call begin_test('lower_nonunit_3')
    call print_int('info', info)
    call print_array('ap', ap_r, 12)
    call end_test()
  end subroutine

  subroutine test_upper_unit_3()
    ! Upper packed 3x3, unit diagonal
    ! Diag values ignored; off-diag: [_, 1+2i, _, 3+0i, 5-1i, _]
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (99.0d0, 99.0d0)
    ap(2) = (1.0d0, 2.0d0); ap(3) = (99.0d0, 99.0d0)
    ap(4) = (3.0d0, 0.0d0); ap(5) = (5.0d0, -1.0d0); ap(6) = (99.0d0, 99.0d0)
    call ztptri('U', 'U', 3, ap, info)
    call begin_test('upper_unit_3')
    call print_int('info', info)
    call print_array('ap', ap_r, 12)
    call end_test()
  end subroutine

  subroutine test_lower_unit_3()
    ! Lower packed 3x3, unit diagonal
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (99.0d0, 99.0d0); ap(2) = (1.0d0, 2.0d0); ap(3) = (3.0d0, 0.0d0)
    ap(4) = (99.0d0, 99.0d0); ap(5) = (5.0d0, -1.0d0)
    ap(6) = (99.0d0, 99.0d0)
    call ztptri('L', 'U', 3, ap, info)
    call begin_test('lower_unit_3')
    call print_int('info', info)
    call print_array('ap', ap_r, 12)
    call end_test()
  end subroutine

  subroutine test_n0()
    ! N=0, quick return
    complex*16 :: ap(1)
    double precision :: ap_r(2)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (99.0d0, 99.0d0)
    info = -99
    call ztptri('U', 'N', 0, ap, info)
    call begin_test('n0')
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_n1()
    ! N=1 edge case
    complex*16 :: ap(1)
    double precision :: ap_r(2)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (3.0d0, 4.0d0)
    call ztptri('U', 'N', 1, ap, info)
    call begin_test('n1')
    call print_int('info', info)
    call print_array('ap', ap_r, 2)
    call end_test()
  end subroutine

  subroutine test_upper_nonunit_4()
    ! Upper packed 4x4, non-unit diagonal
    ! A = [1+1i  2+0i  3+1i  4+2i;  0  5+1i  6+0i  7+3i;  0  0  8+2i  9+1i;  0  0  0  10+0i]
    ! Upper packed: [1+1i, 2+0i, 5+1i, 3+1i, 6+0i, 8+2i, 4+2i, 7+3i, 9+1i, 10+0i]
    complex*16 :: ap(10)
    double precision :: ap_r(20)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (1.0d0, 1.0d0)
    ap(2) = (2.0d0, 0.0d0); ap(3) = (5.0d0, 1.0d0)
    ap(4) = (3.0d0, 1.0d0); ap(5) = (6.0d0, 0.0d0); ap(6) = (8.0d0, 2.0d0)
    ap(7) = (4.0d0, 2.0d0); ap(8) = (7.0d0, 3.0d0); ap(9) = (9.0d0, 1.0d0)
    ap(10) = (10.0d0, 0.0d0)
    call ztptri('U', 'N', 4, ap, info)
    call begin_test('upper_nonunit_4')
    call print_int('info', info)
    call print_array('ap', ap_r, 20)
    call end_test()
  end subroutine

  subroutine test_lower_nonunit_4()
    ! Lower packed 4x4, non-unit diagonal
    ! L = [1+1i  0  0  0;  2+0i  5+1i  0  0;  3+1i  6+0i  8+2i  0;  4+2i  7+3i  9+1i  10+0i]
    ! Lower packed: [1+1i, 2+0i, 3+1i, 4+2i, 5+1i, 6+0i, 7+3i, 8+2i, 9+1i, 10+0i]
    complex*16 :: ap(10)
    double precision :: ap_r(20)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (1.0d0, 1.0d0); ap(2) = (2.0d0, 0.0d0)
    ap(3) = (3.0d0, 1.0d0); ap(4) = (4.0d0, 2.0d0)
    ap(5) = (5.0d0, 1.0d0); ap(6) = (6.0d0, 0.0d0); ap(7) = (7.0d0, 3.0d0)
    ap(8) = (8.0d0, 2.0d0); ap(9) = (9.0d0, 1.0d0)
    ap(10) = (10.0d0, 0.0d0)
    call ztptri('L', 'N', 4, ap, info)
    call begin_test('lower_nonunit_4')
    call print_int('info', info)
    call print_array('ap', ap_r, 20)
    call end_test()
  end subroutine

  subroutine test_singular_upper()
    ! Singular: A(2,2) = 0, upper packed
    ! Upper packed: [2+1i, 1+2i, 0+0i, 3+0i, 5-1i, 6+2i]
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (2.0d0, 1.0d0)
    ap(2) = (1.0d0, 2.0d0); ap(3) = (0.0d0, 0.0d0)
    ap(4) = (3.0d0, 0.0d0); ap(5) = (5.0d0, -1.0d0); ap(6) = (6.0d0, 2.0d0)
    call ztptri('U', 'N', 3, ap, info)
    call begin_test('singular_upper')
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_singular_lower()
    ! Singular: L(3,3) = 0, lower packed
    ! Lower packed: [2+1i, 1+2i, 3+0i, 4+1i, 5-1i, 0+0i]
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info
    ap(1) = (2.0d0, 1.0d0); ap(2) = (1.0d0, 2.0d0); ap(3) = (3.0d0, 0.0d0)
    ap(4) = (4.0d0, 1.0d0); ap(5) = (5.0d0, -1.0d0)
    ap(6) = (0.0d0, 0.0d0)
    call ztptri('L', 'N', 3, ap, info)
    call begin_test('singular_lower')
    call print_int('info', info)
    call end_test()
  end subroutine

end program
