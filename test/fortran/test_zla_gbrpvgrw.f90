program test_zla_gbrpvgrw
  use test_utils
  implicit none
  double precision :: result
  double precision :: ZLA_GBRPVGRW
  external ZLA_GBRPVGRW

  call test_no_growth()
  call test_ncols_zero()
  call test_single_element()
  call test_growth_factor()
  call test_zero_umax()
  call test_tridiagonal()
  call test_ncols_less_than_n()
  call test_wider_band()
  call test_complex_values()

contains

  subroutine test_no_growth()
    ! Diagonal matrix, U = A, no growth (all real values)
    complex*16 :: AB(1, 3), AFB(1, 3)

    AB(1, 1) = (5.0d0, 0.0d0)
    AB(1, 2) = (3.0d0, 0.0d0)
    AB(1, 3) = (7.0d0, 0.0d0)

    AFB(1, 1) = (5.0d0, 0.0d0)
    AFB(1, 2) = (3.0d0, 0.0d0)
    AFB(1, 3) = (7.0d0, 0.0d0)

    result = ZLA_GBRPVGRW(3, 0, 0, 3, AB, 1, AFB, 1)
    call begin_test('no_growth')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_ncols_zero()
    complex*16 :: AB(1, 3), AFB(1, 3)

    AB(1, 1) = (5.0d0, 0.0d0)
    AFB(1, 1) = (5.0d0, 0.0d0)

    result = ZLA_GBRPVGRW(3, 0, 0, 0, AB, 1, AFB, 1)
    call begin_test('ncols_zero')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_single_element()
    ! Single element with complex value:
    ! CABS1((3,4)) = |3|+|4| = 7
    ! CABS1((6,2)) = |6|+|2| = 8
    ! RPVGRW = 7/8 = 0.875
    complex*16 :: AB(1, 1), AFB(1, 1)

    AB(1, 1) = (3.0d0, 4.0d0)
    AFB(1, 1) = (6.0d0, 2.0d0)

    result = ZLA_GBRPVGRW(1, 0, 0, 1, AB, 1, AFB, 1)
    call begin_test('single_element_growth')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_growth_factor()
    ! Diagonal, with complex values
    ! Col 1: CABS1((2,1)) = 3, CABS1((4,2)) = 6, ratio = 0.5
    ! Col 2: CABS1((3,0)) = 3, CABS1((3,0)) = 3, ratio = 1.0
    ! Col 3: CABS1((1,1)) = 2, CABS1((5,3)) = 8, ratio = 0.25
    ! RPVGRW = 0.25
    complex*16 :: AB(1, 3), AFB(1, 3)

    AB(1, 1) = (2.0d0, 1.0d0)
    AB(1, 2) = (3.0d0, 0.0d0)
    AB(1, 3) = (1.0d0, 1.0d0)

    AFB(1, 1) = (4.0d0, 2.0d0)
    AFB(1, 2) = (3.0d0, 0.0d0)
    AFB(1, 3) = (5.0d0, 3.0d0)

    result = ZLA_GBRPVGRW(3, 0, 0, 3, AB, 1, AFB, 1)
    call begin_test('growth_factor')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_zero_umax()
    ! Col 2 has UMAX=0, should be skipped
    ! Col 1: CABS1((5,1)) = 6, CABS1((5,1)) = 6, ratio=1
    ! Col 2: UMAX=0, skip
    ! Col 3: CABS1((4,0)) = 4, CABS1((8,0)) = 8, ratio=0.5
    ! RPVGRW = 0.5
    complex*16 :: AB(1, 3), AFB(1, 3)

    AB(1, 1) = (5.0d0, 1.0d0)
    AB(1, 2) = (3.0d0, 2.0d0)
    AB(1, 3) = (4.0d0, 0.0d0)

    AFB(1, 1) = (5.0d0, 1.0d0)
    AFB(1, 2) = (0.0d0, 0.0d0)
    AFB(1, 3) = (8.0d0, 0.0d0)

    result = ZLA_GBRPVGRW(3, 0, 0, 3, AB, 1, AFB, 1)
    call begin_test('zero_umax')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_tridiagonal()
    ! kl=1, ku=1, n=4
    ! LDAB = 3, LDAFB = 4, KD = 2
    complex*16 :: AB(3, 4), AFB(4, 4)

    AB = (0.0d0, 0.0d0)
    AFB = (0.0d0, 0.0d0)

    ! Col 1: i=1 -> AB(2,1); i=2 -> AB(3,1)
    AB(2, 1) = (4.0d0, 1.0d0); AB(3, 1) = (1.0d0, 0.0d0)
    ! Col 2: i=1 -> AB(1,2); i=2 -> AB(2,2); i=3 -> AB(3,2)
    AB(1, 2) = (2.0d0, 0.0d0); AB(2, 2) = (5.0d0, 2.0d0); AB(3, 2) = (3.0d0, 1.0d0)
    ! Col 3: i=2 -> AB(1,3); i=3 -> AB(2,3); i=4 -> AB(3,3)
    AB(1, 3) = (1.0d0, 1.0d0); AB(2, 3) = (6.0d0, 0.0d0); AB(3, 3) = (2.0d0, 0.0d0)
    ! Col 4: i=3 -> AB(1,4); i=4 -> AB(2,4)
    AB(1, 4) = (4.0d0, 0.0d0); AB(2, 4) = (3.0d0, 1.0d0)

    ! U factor
    ! Col 1: i=1 -> AFB(2,1)
    AFB(2, 1) = (4.0d0, 1.0d0)
    ! Col 2: i=1 -> AFB(1,2); i=2 -> AFB(2,2)
    AFB(1, 2) = (2.0d0, 0.0d0); AFB(2, 2) = (10.0d0, 3.0d0)
    ! Col 3: i=2 -> AFB(1,3); i=3 -> AFB(2,3)
    AFB(1, 3) = (1.0d0, 1.0d0); AFB(2, 3) = (6.0d0, 0.0d0)
    ! Col 4: i=3 -> AFB(1,4); i=4 -> AFB(2,4)
    AFB(1, 4) = (4.0d0, 0.0d0); AFB(2, 4) = (7.0d0, 2.0d0)

    result = ZLA_GBRPVGRW(4, 1, 1, 4, AB, 3, AFB, 4)
    call begin_test('tridiagonal')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_ncols_less_than_n()
    ! Only check first 2 of 4 columns
    complex*16 :: AB(1, 4), AFB(1, 4)

    AB(1, 1) = (3.0d0, 1.0d0)
    AB(1, 2) = (4.0d0, 0.0d0)
    AB(1, 3) = (1.0d0, 0.0d0)
    AB(1, 4) = (2.0d0, 0.0d0)

    AFB(1, 1) = (6.0d0, 2.0d0)
    AFB(1, 2) = (4.0d0, 0.0d0)
    AFB(1, 3) = (100.0d0, 0.0d0)
    AFB(1, 4) = (100.0d0, 0.0d0)

    result = ZLA_GBRPVGRW(4, 0, 0, 2, AB, 1, AFB, 1)
    call begin_test('ncols_less_than_n')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_wider_band()
    ! kl=2, ku=2, n=5
    ! LDAB = 5, LDAFB = 7, KD = 3
    complex*16 :: AB(5, 5), AFB(7, 5)

    AB = (0.0d0, 0.0d0)
    AFB = (0.0d0, 0.0d0)

    ! Col 1: i=1(row3), i=2(row4), i=3(row5)
    AB(3, 1) = (8.0d0, 0.0d0); AB(4, 1) = (2.0d0, 0.0d0); AB(5, 1) = (1.0d0, 0.0d0)
    ! Col 2: i=1(row2), i=2(row3), i=3(row4), i=4(row5)
    AB(2, 2) = (3.0d0, 0.0d0); AB(3, 2) = (7.0d0, 1.0d0)
    AB(4, 2) = (4.0d0, 0.0d0); AB(5, 2) = (1.0d0, 0.0d0)
    ! Col 3
    AB(1, 3) = (1.0d0, 0.0d0); AB(2, 3) = (2.0d0, 0.0d0); AB(3, 3) = (9.0d0, 0.0d0)
    AB(4, 3) = (3.0d0, 0.0d0); AB(5, 3) = (2.0d0, 0.0d0)
    ! Col 4
    AB(1, 4) = (5.0d0, 0.0d0); AB(2, 4) = (1.0d0, 0.0d0)
    AB(3, 4) = (6.0d0, 0.0d0); AB(4, 4) = (4.0d0, 0.0d0)
    ! Col 5
    AB(1, 5) = (2.0d0, 0.0d0); AB(2, 5) = (3.0d0, 0.0d0); AB(3, 5) = (5.0d0, 0.0d0)

    ! AFB U factor
    AFB(3, 1) = (8.0d0, 0.0d0)
    AFB(2, 2) = (3.0d0, 0.0d0); AFB(3, 2) = (14.0d0, 0.0d0)
    AFB(1, 3) = (1.0d0, 0.0d0); AFB(2, 3) = (2.0d0, 0.0d0); AFB(3, 3) = (9.0d0, 0.0d0)
    AFB(1, 4) = (5.0d0, 0.0d0); AFB(2, 4) = (1.0d0, 0.0d0); AFB(3, 4) = (12.0d0, 0.0d0)
    AFB(1, 5) = (2.0d0, 0.0d0); AFB(2, 5) = (3.0d0, 0.0d0); AFB(3, 5) = (10.0d0, 0.0d0)

    result = ZLA_GBRPVGRW(5, 2, 2, 5, AB, 5, AFB, 7)
    call begin_test('wider_band')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_complex_values()
    ! Test with genuinely complex values in both real and imag parts
    ! kl=1, ku=1, n=3, LDAB=3, LDAFB=4, KD=2
    ! CABS1((a,b)) = |a| + |b|
    complex*16 :: AB(3, 3), AFB(4, 3)

    AB = (0.0d0, 0.0d0)
    AFB = (0.0d0, 0.0d0)

    ! Col 1: i=1 -> AB(2,1); i=2 -> AB(3,1)
    AB(2, 1) = (3.0d0, -4.0d0)  ! CABS1 = 7
    AB(3, 1) = (1.0d0, 2.0d0)   ! CABS1 = 3
    ! Col 2: i=1 -> AB(1,2); i=2 -> AB(2,2); i=3 -> AB(3,2)
    AB(1, 2) = (-2.0d0, 1.0d0)  ! CABS1 = 3
    AB(2, 2) = (5.0d0, -3.0d0)  ! CABS1 = 8
    AB(3, 2) = (0.0d0, 6.0d0)   ! CABS1 = 6
    ! Col 3: i=2 -> AB(1,3); i=3 -> AB(2,3)
    AB(1, 3) = (4.0d0, -1.0d0)  ! CABS1 = 5
    AB(2, 3) = (-2.0d0, -3.0d0) ! CABS1 = 5

    ! U factor
    ! Col 1: i=1 -> AFB(2,1)
    AFB(2, 1) = (3.0d0, -4.0d0)  ! CABS1 = 7
    ! Col 2: i=1 -> AFB(1,2); i=2 -> AFB(2,2)
    AFB(1, 2) = (-2.0d0, 1.0d0)  ! CABS1 = 3
    AFB(2, 2) = (10.0d0, -5.0d0) ! CABS1 = 15
    ! Col 3: i=2 -> AFB(1,3); i=3 -> AFB(2,3)
    AFB(1, 3) = (4.0d0, -1.0d0)  ! CABS1 = 5
    AFB(2, 3) = (-2.0d0, -8.0d0) ! CABS1 = 10

    ! Col 1: AMAX = max(7, 3) = 7, UMAX = 7, ratio = 1.0
    ! Col 2: AMAX = max(3, 8, 6) = 8, UMAX = max(3, 15) = 15, ratio = 8/15
    ! Col 3: AMAX = max(5, 5) = 5, UMAX = max(5, 10) = 10, ratio = 0.5
    ! RPVGRW = min(1, 8/15, 0.5) = 0.5

    result = ZLA_GBRPVGRW(3, 1, 1, 3, AB, 3, AFB, 4)
    call begin_test('complex_values')
    call print_scalar('result', result)
    call end_test()
  end subroutine

end program
