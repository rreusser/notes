program test_dla_gbrpvgrw
  use test_utils
  implicit none
  double precision :: result
  double precision :: DLA_GBRPVGRW
  external DLA_GBRPVGRW

  call test_no_growth()
  call test_ncols_zero()
  call test_single_element()
  call test_growth_factor()
  call test_zero_umax()
  call test_tridiagonal()
  call test_ncols_less_than_n()
  call test_wider_band()

contains

  subroutine test_no_growth()
    ! Diagonal matrix, U = A, no growth
    double precision :: AB(1, 3), AFB(1, 3)

    AB(1, 1) = 5.0d0
    AB(1, 2) = 3.0d0
    AB(1, 3) = 7.0d0

    AFB(1, 1) = 5.0d0
    AFB(1, 2) = 3.0d0
    AFB(1, 3) = 7.0d0

    result = DLA_GBRPVGRW(3, 0, 0, 3, AB, 1, AFB, 1)
    call begin_test('no_growth')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_ncols_zero()
    double precision :: AB(1, 3), AFB(1, 3)

    AB(1, 1) = 5.0d0
    AFB(1, 1) = 5.0d0

    result = DLA_GBRPVGRW(3, 0, 0, 0, AB, 1, AFB, 1)
    call begin_test('ncols_zero')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_single_element()
    ! Single element, U grew: RPVGRW = 3/6 = 0.5
    double precision :: AB(1, 1), AFB(1, 1)

    AB(1, 1) = 3.0d0
    AFB(1, 1) = 6.0d0

    result = DLA_GBRPVGRW(1, 0, 0, 1, AB, 1, AFB, 1)
    call begin_test('single_element_growth')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_growth_factor()
    ! Diagonal, U larger than A for some columns
    ! Col 1: 2/4 = 0.5, Col 2: 3/3 = 1.0, Col 3: 1/5 = 0.2
    ! RPVGRW = 0.2
    double precision :: AB(1, 3), AFB(1, 3)

    AB(1, 1) = 2.0d0
    AB(1, 2) = 3.0d0
    AB(1, 3) = 1.0d0

    AFB(1, 1) = 4.0d0
    AFB(1, 2) = 3.0d0
    AFB(1, 3) = 5.0d0

    result = DLA_GBRPVGRW(3, 0, 0, 3, AB, 1, AFB, 1)
    call begin_test('growth_factor')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_zero_umax()
    ! Col 2 has UMAX=0, should be skipped
    ! Col 1: 5/5=1.0, Col 2: skip, Col 3: 4/8=0.5
    ! RPVGRW = 0.5
    double precision :: AB(1, 3), AFB(1, 3)

    AB(1, 1) = 5.0d0
    AB(1, 2) = 3.0d0
    AB(1, 3) = 4.0d0

    AFB(1, 1) = 5.0d0
    AFB(1, 2) = 0.0d0
    AFB(1, 3) = 8.0d0

    result = DLA_GBRPVGRW(3, 0, 0, 3, AB, 1, AFB, 1)
    call begin_test('zero_umax')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_tridiagonal()
    ! kl=1, ku=1, n=4
    ! LDAB = 3, LDAFB = 4, KD = 2
    double precision :: AB(3, 4), AFB(4, 4)

    AB = 0.0d0
    AFB = 0.0d0

    ! Col 1: i=1 -> AB(2,1); i=2 -> AB(3,1)
    AB(2, 1) = 4.0d0; AB(3, 1) = 1.0d0
    ! Col 2: i=1 -> AB(1,2); i=2 -> AB(2,2); i=3 -> AB(3,2)
    AB(1, 2) = 2.0d0; AB(2, 2) = 5.0d0; AB(3, 2) = 3.0d0
    ! Col 3: i=2 -> AB(1,3); i=3 -> AB(2,3); i=4 -> AB(3,3)
    AB(1, 3) = 1.0d0; AB(2, 3) = 6.0d0; AB(3, 3) = 2.0d0
    ! Col 4: i=3 -> AB(1,4); i=4 -> AB(2,4)
    AB(1, 4) = 4.0d0; AB(2, 4) = 3.0d0

    ! U factor with KD=2, U rows: max(j-ku,1) to j
    ! Col 1: i=1 -> AFB(2,1)
    AFB(2, 1) = 4.0d0
    ! Col 2: i=1 -> AFB(1,2); i=2 -> AFB(2,2)
    AFB(1, 2) = 2.0d0; AFB(2, 2) = 10.0d0
    ! Col 3: i=2 -> AFB(1,3); i=3 -> AFB(2,3)
    AFB(1, 3) = 1.0d0; AFB(2, 3) = 6.0d0
    ! Col 4: i=3 -> AFB(1,4); i=4 -> AFB(2,4)
    AFB(1, 4) = 4.0d0; AFB(2, 4) = 7.0d0

    ! Col 1: AMAX=max(4,1)=4, UMAX=4, ratio=1
    ! Col 2: AMAX=max(2,5,3)=5, UMAX=max(2,10)=10, ratio=0.5
    ! Col 3: AMAX=max(1,6,2)=6, UMAX=max(1,6)=6, ratio=1
    ! Col 4: AMAX=max(4,3)=4, UMAX=max(4,7)=7, ratio=4/7
    ! RPVGRW = min(1, 0.5, 1, 4/7) = 0.5

    result = DLA_GBRPVGRW(4, 1, 1, 4, AB, 3, AFB, 4)
    call begin_test('tridiagonal')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_ncols_less_than_n()
    ! Only check first 2 of 4 columns
    ! Col 1: 3/6=0.5, Col 2: 4/4=1.0
    ! RPVGRW = 0.5
    double precision :: AB(1, 4), AFB(1, 4)

    AB(1, 1) = 3.0d0
    AB(1, 2) = 4.0d0
    AB(1, 3) = 1.0d0
    AB(1, 4) = 2.0d0

    AFB(1, 1) = 6.0d0
    AFB(1, 2) = 4.0d0
    AFB(1, 3) = 100.0d0
    AFB(1, 4) = 100.0d0

    result = DLA_GBRPVGRW(4, 0, 0, 2, AB, 1, AFB, 1)
    call begin_test('ncols_less_than_n')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_wider_band()
    ! kl=2, ku=2, n=5
    ! LDAB = 5, LDAFB = 7, KD = 3
    double precision :: AB(5, 5), AFB(7, 5)

    AB = 0.0d0
    AFB = 0.0d0

    ! KD = KU + 1 = 3
    ! AB(3+i-j, j) = A(i,j)
    ! Col 1: i=1(row3), i=2(row4), i=3(row5)
    AB(3, 1) = 8.0d0; AB(4, 1) = 2.0d0; AB(5, 1) = 1.0d0
    ! Col 2: i=1(row2), i=2(row3), i=3(row4), i=4(row5)
    AB(2, 2) = 3.0d0; AB(3, 2) = 7.0d0; AB(4, 2) = 4.0d0; AB(5, 2) = 1.0d0
    ! Col 3: i=1(row1), i=2(row2), i=3(row3), i=4(row4), i=5(row5)
    AB(1, 3) = 1.0d0; AB(2, 3) = 2.0d0; AB(3, 3) = 9.0d0
    AB(4, 3) = 3.0d0; AB(5, 3) = 2.0d0
    ! Col 4: i=2(row1), i=3(row2), i=4(row3), i=5(row4)
    AB(1, 4) = 5.0d0; AB(2, 4) = 1.0d0; AB(3, 4) = 6.0d0; AB(4, 4) = 4.0d0
    ! Col 5: i=3(row1), i=4(row2), i=5(row3)
    AB(1, 5) = 2.0d0; AB(2, 5) = 3.0d0; AB(3, 5) = 5.0d0

    ! AFB: U at rows with KD = 3
    ! Col 1: i=1 only -> AFB(3,1)
    AFB(3, 1) = 8.0d0
    ! Col 2: i=1 -> AFB(2,2); i=2 -> AFB(3,2)
    AFB(2, 2) = 3.0d0; AFB(3, 2) = 14.0d0
    ! Col 3: i=1 -> AFB(1,3); i=2 -> AFB(2,3); i=3 -> AFB(3,3)
    AFB(1, 3) = 1.0d0; AFB(2, 3) = 2.0d0; AFB(3, 3) = 9.0d0
    ! Col 4: i=2 -> AFB(1,4); i=3 -> AFB(2,4); i=4 -> AFB(3,4)
    AFB(1, 4) = 5.0d0; AFB(2, 4) = 1.0d0; AFB(3, 4) = 12.0d0
    ! Col 5: i=3 -> AFB(1,5); i=4 -> AFB(2,5); i=5 -> AFB(3,5)
    AFB(1, 5) = 2.0d0; AFB(2, 5) = 3.0d0; AFB(3, 5) = 10.0d0

    ! Col 1: AMAX = max(8,2,1) = 8,  UMAX = 8,              ratio = 1.0
    ! Col 2: AMAX = max(3,7,4,1) = 7, UMAX = max(3,14) = 14, ratio = 0.5
    ! Col 3: AMAX = max(1,2,9,3,2) = 9, UMAX = max(1,2,9) = 9, ratio = 1.0
    ! Col 4: AMAX = max(5,1,6,4) = 6, UMAX = max(5,1,12) = 12, ratio = 0.5
    ! Col 5: AMAX = max(2,3,5) = 5, UMAX = max(2,3,10) = 10, ratio = 0.5
    ! RPVGRW = 0.5

    result = DLA_GBRPVGRW(5, 2, 2, 5, AB, 5, AFB, 7)
    call begin_test('wider_band')
    call print_scalar('result', result)
    call end_test()
  end subroutine

end program
