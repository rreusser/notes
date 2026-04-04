program test_dlaqgb
  use test_utils
  implicit none

  ! Band matrix: M=4, N=5, KL=1, KU=2, LDAB=KL+KU+1=4
  integer, parameter :: M = 4, N = 5, KL = 1, KU = 2, LDAB = 4
  double precision :: ab(LDAB, N), r(M), c(N)
  double precision :: rowcnd, colcnd, amax
  character :: equed
  double precision :: dlamch

  r(1) = 0.5d0; r(2) = 1.0d0; r(3) = 0.8d0; r(4) = 0.25d0
  c(1) = 0.6d0; c(2) = 1.0d0; c(3) = 0.7d0; c(4) = 0.9d0; c(5) = 0.4d0

  ! --- Test 1: No equilibration (rowcnd >= 0.1, colcnd >= 0.1, small <= amax <= large) ---
  call fill_ab(ab)
  rowcnd = 0.5d0; colcnd = 0.6d0; amax = 4.0d0
  call dlaqgb(M, N, KL, KU, ab, LDAB, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('no_equil')
  call print_array('ab', ab, LDAB * N)
  call print_char('equed', equed)
  call end_test()

  ! --- Test 2: Row scaling only (rowcnd < 0.1, colcnd >= 0.1) ---
  call fill_ab(ab)
  rowcnd = 0.01d0; colcnd = 0.6d0; amax = 4.0d0
  call dlaqgb(M, N, KL, KU, ab, LDAB, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('row_equil')
  call print_array('ab', ab, LDAB * N)
  call print_char('equed', equed)
  call end_test()

  ! --- Test 3: Column scaling only (colcnd < 0.1, rowcnd >= 0.1) ---
  call fill_ab(ab)
  rowcnd = 0.5d0; colcnd = 0.01d0; amax = 4.0d0
  call dlaqgb(M, N, KL, KU, ab, LDAB, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('col_equil')
  call print_array('ab', ab, LDAB * N)
  call print_char('equed', equed)
  call end_test()

  ! --- Test 4: Both row and column scaling ---
  call fill_ab(ab)
  rowcnd = 0.01d0; colcnd = 0.01d0; amax = 4.0d0
  call dlaqgb(M, N, KL, KU, ab, LDAB, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('both_equil')
  call print_array('ab', ab, LDAB * N)
  call print_char('equed', equed)
  call end_test()

  ! --- Test 5: Row scaling triggered by amax > large ---
  call fill_ab(ab)
  rowcnd = 0.5d0; colcnd = 0.6d0; amax = 1.0d+300
  call dlaqgb(M, N, KL, KU, ab, LDAB, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('amax_large')
  call print_array('ab', ab, LDAB * N)
  call print_char('equed', equed)
  call end_test()

  ! --- Test 6: Row scaling triggered by amax < small ---
  call fill_ab(ab)
  rowcnd = 0.5d0; colcnd = 0.6d0; amax = 1.0d-320
  call dlaqgb(M, N, KL, KU, ab, LDAB, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('amax_small')
  call print_array('ab', ab, LDAB * N)
  call print_char('equed', equed)
  call end_test()

  ! --- Test 7: Quick return M=0 ---
  call dlaqgb(0, N, KL, KU, ab, LDAB, r, c, 0.5d0, 0.6d0, 4.0d0, equed)
  call begin_test('m_zero')
  call print_char('equed', equed)
  call end_test()

  ! --- Test 8: Quick return N=0 ---
  call dlaqgb(M, 0, KL, KU, ab, LDAB, r, c, 0.5d0, 0.6d0, 4.0d0, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

contains

  subroutine fill_ab(ab)
    double precision, intent(out) :: ab(LDAB, N)
    integer :: j

    ! Initialize to zero
    ab = 0.0d0

    ! Fill band storage: AB(KU+1+i-j, j) = A(i,j)
    ! KU=2, so diagonal row index = KU+1 = 3
    ! For each column j (1-based), rows i from max(1,j-KU) to min(M,j+KL)

    ! Column 1: i=1..2, band rows KU+1+i-1 = 3,4
    ab(3, 1) = 2.0d0  ! A(1,1)
    ab(4, 1) = 1.5d0  ! A(2,1)

    ! Column 2: i=1..3, band rows KU+1+i-2 = 2,3,4
    ab(2, 2) = 3.0d0  ! A(1,2)
    ab(3, 2) = 1.0d0  ! A(2,2)
    ab(4, 2) = 0.5d0  ! A(3,2)

    ! Column 3: i=1..4, band rows KU+1+i-3 = 1,2,3,4
    ab(1, 3) = 0.8d0  ! A(1,3)
    ab(2, 3) = 2.5d0  ! A(2,3)
    ab(3, 3) = 4.0d0  ! A(3,3)
    ab(4, 3) = 1.2d0  ! A(4,3)

    ! Column 4: i=2..4, band rows KU+1+i-4 = 1,2,3
    ab(1, 4) = 0.6d0  ! A(2,4)
    ab(2, 4) = 3.5d0  ! A(3,4)
    ab(3, 4) = 2.0d0  ! A(4,4)

    ! Column 5: i=3..4, band rows KU+1+i-5 = 1,2
    ab(1, 5) = 1.0d0  ! A(3,5)
    ab(2, 5) = 0.7d0  ! A(4,5)
  end subroutine

end program
