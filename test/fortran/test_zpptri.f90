program test_zpptri
  use test_utils
  implicit none

  call test_upper_3()
  call test_lower_3()
  call test_upper_4()
  call test_lower_4()
  call test_n0()
  call test_n1()
  call test_singular_upper()
  call test_singular_lower()

contains

  subroutine test_upper_3()
    ! Upper packed 3x3 Hermitian positive definite matrix
    ! H = [10  3-i  1+2i;  3+i  8  2-i;  1-2i  2+i  6]
    ! Upper packed (col-major): [10, 3-i, 8, 1+2i, 2-i, 6]
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info

    ap(1) = (10.0d0, 0.0d0)
    ap(2) = (3.0d0, -1.0d0); ap(3) = (8.0d0, 0.0d0)
    ap(4) = (1.0d0, 2.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (6.0d0, 0.0d0)

    call zpptrf('U', 3, ap, info)
    if (info /= 0) then
      print *, 'zpptrf failed with info=', info
      stop 1
    end if

    call zpptri('U', 3, ap, info)
    call begin_test('upper_3')
    call print_int('info', info)
    call print_array('ap', ap_r, 12)
    call end_test()
  end subroutine

  subroutine test_lower_3()
    ! Lower packed 3x3 Hermitian positive definite matrix (same matrix)
    ! H = [10  3-i  1+2i;  3+i  8  2-i;  1-2i  2+i  6]
    ! Lower packed (col-major): [10, 3+i, 1-2i, 8, 2+i, 6]
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info

    ap(1) = (10.0d0, 0.0d0); ap(2) = (3.0d0, 1.0d0); ap(3) = (1.0d0, -2.0d0)
    ap(4) = (8.0d0, 0.0d0); ap(5) = (2.0d0, 1.0d0)
    ap(6) = (6.0d0, 0.0d0)

    call zpptrf('L', 3, ap, info)
    if (info /= 0) then
      print *, 'zpptrf failed with info=', info
      stop 1
    end if

    call zpptri('L', 3, ap, info)
    call begin_test('lower_3')
    call print_int('info', info)
    call print_array('ap', ap_r, 12)
    call end_test()
  end subroutine

  subroutine test_upper_4()
    ! Upper packed 4x4 Hermitian positive definite matrix
    ! H = [14  4-2i  2+i   1-3i;
    !       4+2i  12  3-i   2+2i;
    !       2-i   3+i  10   1-i;
    !       1+3i  2-2i  1+i  9]
    ! Upper packed: [14, 4-2i, 12, 2+i, 3-i, 10, 1-3i, 2+2i, 1-i, 9]
    complex*16 :: ap(10)
    double precision :: ap_r(20)
    equivalence (ap, ap_r)
    integer :: info

    ap(1) = (14.0d0, 0.0d0)
    ap(2) = (4.0d0, -2.0d0); ap(3) = (12.0d0, 0.0d0)
    ap(4) = (2.0d0, 1.0d0); ap(5) = (3.0d0, -1.0d0); ap(6) = (10.0d0, 0.0d0)
    ap(7) = (1.0d0, -3.0d0); ap(8) = (2.0d0, 2.0d0); ap(9) = (1.0d0, -1.0d0)
    ap(10) = (9.0d0, 0.0d0)

    call zpptrf('U', 4, ap, info)
    if (info /= 0) then
      print *, 'zpptrf failed with info=', info
      stop 1
    end if

    call zpptri('U', 4, ap, info)
    call begin_test('upper_4')
    call print_int('info', info)
    call print_array('ap', ap_r, 20)
    call end_test()
  end subroutine

  subroutine test_lower_4()
    ! Lower packed 4x4 Hermitian positive definite matrix (same matrix)
    ! Lower packed: [14, 4+2i, 2-i, 1+3i, 12, 3+i, 2-2i, 10, 1+i, 9]
    complex*16 :: ap(10)
    double precision :: ap_r(20)
    equivalence (ap, ap_r)
    integer :: info

    ap(1) = (14.0d0, 0.0d0); ap(2) = (4.0d0, 2.0d0)
    ap(3) = (2.0d0, -1.0d0); ap(4) = (1.0d0, 3.0d0)
    ap(5) = (12.0d0, 0.0d0); ap(6) = (3.0d0, 1.0d0); ap(7) = (2.0d0, -2.0d0)
    ap(8) = (10.0d0, 0.0d0); ap(9) = (1.0d0, 1.0d0)
    ap(10) = (9.0d0, 0.0d0)

    call zpptrf('L', 4, ap, info)
    if (info /= 0) then
      print *, 'zpptrf failed with info=', info
      stop 1
    end if

    call zpptri('L', 4, ap, info)
    call begin_test('lower_4')
    call print_int('info', info)
    call print_array('ap', ap_r, 20)
    call end_test()
  end subroutine

  subroutine test_n0()
    ! N=0 quick return
    complex*16 :: ap(1)
    double precision :: ap_r(2)
    equivalence (ap, ap_r)
    integer :: info

    ap(1) = (99.0d0, 99.0d0)
    info = -99
    call zpptri('U', 0, ap, info)
    call begin_test('n0')
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_n1()
    ! N=1, single element HPD
    complex*16 :: ap(1)
    double precision :: ap_r(2)
    equivalence (ap, ap_r)
    integer :: info

    ap(1) = (9.0d0, 0.0d0)
    call zpptrf('U', 1, ap, info)
    call zpptri('U', 1, ap, info)
    call begin_test('n1')
    call print_int('info', info)
    call print_array('ap', ap_r, 2)
    call end_test()
  end subroutine

  subroutine test_singular_upper()
    ! Non-positive-definite (singular Cholesky factor): factor has zero on diagonal
    ! Manually set a Cholesky factor with zero diagonal
    ! Upper packed: [3+0i, 1+2i, 0+0i, 4+1i, 2-1i, 5+0i]
    ! Diagonal element (2,2) is zero -> ztptri will return info=2
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info

    ap(1) = (3.0d0, 0.0d0)
    ap(2) = (1.0d0, 2.0d0); ap(3) = (0.0d0, 0.0d0)
    ap(4) = (4.0d0, 1.0d0); ap(5) = (2.0d0, -1.0d0); ap(6) = (5.0d0, 0.0d0)

    call zpptri('U', 3, ap, info)
    call begin_test('singular_upper')
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_singular_lower()
    ! Lower packed with zero diagonal: L(3,3) = 0
    ! Lower packed: [3+0i, 1+2i, 4+1i, 5+0i, 2-1i, 0+0i]
    complex*16 :: ap(6)
    double precision :: ap_r(12)
    equivalence (ap, ap_r)
    integer :: info

    ap(1) = (3.0d0, 0.0d0); ap(2) = (1.0d0, 2.0d0); ap(3) = (4.0d0, 1.0d0)
    ap(4) = (5.0d0, 0.0d0); ap(5) = (2.0d0, -1.0d0)
    ap(6) = (0.0d0, 0.0d0)

    call zpptri('L', 3, ap, info)
    call begin_test('singular_lower')
    call print_int('info', info)
    call end_test()
  end subroutine

end program
