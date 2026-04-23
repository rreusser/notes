program test_zlaqgb
  use test_utils
  implicit none
  double precision :: ab_r(200)
  complex*16 :: ab(100)
  equivalence (ab, ab_r)
  double precision :: r(10), c(10), rowcnd, colcnd, amax
  character :: equed
  integer :: m, n, kl, ku, ldab

  ! Test 1: No equilibration needed (well-scaled)
  ! 4x4 band matrix with kl=1, ku=2, ldab=kl+ku+1=4
  ! Band storage: AB(ku+1+i-j, j) = A(i,j)
  m = 4; n = 4; kl = 1; ku = 2; ldab = 4
  ab(1) = (0.0d0, 0.0d0);  ab(2) = (0.0d0, 0.0d0);  ab(3) = (1.0d0, 0.5d0);  ab(4) = (2.0d0, -1.0d0)
  ab(5) = (0.0d0, 0.0d0);  ab(6) = (3.0d0, 1.0d0);  ab(7) = (4.0d0, 2.0d0);  ab(8) = (5.0d0, -0.5d0)
  ab(9) = (6.0d0, 0.3d0);  ab(10) = (7.0d0, -0.2d0); ab(11) = (8.0d0, 1.5d0); ab(12) = (9.0d0, 0.1d0)
  ab(13) = (10.0d0, -0.4d0); ab(14) = (11.0d0, 0.7d0); ab(15) = (12.0d0, -1.0d0); ab(16) = (0.0d0, 0.0d0)
  r(1) = 1.0d0; r(2) = 1.0d0; r(3) = 1.0d0; r(4) = 1.0d0
  c(1) = 1.0d0; c(2) = 1.0d0; c(3) = 1.0d0; c(4) = 1.0d0
  rowcnd = 1.0d0; colcnd = 1.0d0; amax = 12.0d0
  call zlaqgb(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('no_equil')
  call print_char('equed', equed)
  call print_array('ab', ab_r, 2*ldab*n)
  call end_test()

  ! Test 2: Row equilibration only
  m = 4; n = 4; kl = 1; ku = 2; ldab = 4
  ab(1) = (0.0d0, 0.0d0);  ab(2) = (0.0d0, 0.0d0);  ab(3) = (1.0d0, 0.5d0);  ab(4) = (2.0d0, -1.0d0)
  ab(5) = (0.0d0, 0.0d0);  ab(6) = (3.0d0, 1.0d0);  ab(7) = (4.0d0, 2.0d0);  ab(8) = (5.0d0, -0.5d0)
  ab(9) = (6.0d0, 0.3d0);  ab(10) = (7.0d0, -0.2d0); ab(11) = (8.0d0, 1.5d0); ab(12) = (9.0d0, 0.1d0)
  ab(13) = (10.0d0, -0.4d0); ab(14) = (11.0d0, 0.7d0); ab(15) = (12.0d0, -1.0d0); ab(16) = (0.0d0, 0.0d0)
  r(1) = 0.5d0; r(2) = 2.0d0; r(3) = 1.5d0; r(4) = 0.25d0
  c(1) = 1.0d0; c(2) = 1.0d0; c(3) = 1.0d0; c(4) = 1.0d0
  rowcnd = 0.01d0; colcnd = 1.0d0; amax = 12.0d0
  call zlaqgb(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('row_equil')
  call print_char('equed', equed)
  call print_array('ab', ab_r, 2*ldab*n)
  call end_test()

  ! Test 3: Column equilibration only
  m = 4; n = 4; kl = 1; ku = 2; ldab = 4
  ab(1) = (0.0d0, 0.0d0);  ab(2) = (0.0d0, 0.0d0);  ab(3) = (1.0d0, 0.5d0);  ab(4) = (2.0d0, -1.0d0)
  ab(5) = (0.0d0, 0.0d0);  ab(6) = (3.0d0, 1.0d0);  ab(7) = (4.0d0, 2.0d0);  ab(8) = (5.0d0, -0.5d0)
  ab(9) = (6.0d0, 0.3d0);  ab(10) = (7.0d0, -0.2d0); ab(11) = (8.0d0, 1.5d0); ab(12) = (9.0d0, 0.1d0)
  ab(13) = (10.0d0, -0.4d0); ab(14) = (11.0d0, 0.7d0); ab(15) = (12.0d0, -1.0d0); ab(16) = (0.0d0, 0.0d0)
  r(1) = 1.0d0; r(2) = 1.0d0; r(3) = 1.0d0; r(4) = 1.0d0
  c(1) = 0.5d0; c(2) = 2.0d0; c(3) = 1.5d0; c(4) = 0.25d0
  rowcnd = 1.0d0; colcnd = 0.01d0; amax = 12.0d0
  call zlaqgb(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('col_equil')
  call print_char('equed', equed)
  call print_array('ab', ab_r, 2*ldab*n)
  call end_test()

  ! Test 4: Both row and column equilibration
  m = 4; n = 4; kl = 1; ku = 2; ldab = 4
  ab(1) = (0.0d0, 0.0d0);  ab(2) = (0.0d0, 0.0d0);  ab(3) = (1.0d0, 0.5d0);  ab(4) = (2.0d0, -1.0d0)
  ab(5) = (0.0d0, 0.0d0);  ab(6) = (3.0d0, 1.0d0);  ab(7) = (4.0d0, 2.0d0);  ab(8) = (5.0d0, -0.5d0)
  ab(9) = (6.0d0, 0.3d0);  ab(10) = (7.0d0, -0.2d0); ab(11) = (8.0d0, 1.5d0); ab(12) = (9.0d0, 0.1d0)
  ab(13) = (10.0d0, -0.4d0); ab(14) = (11.0d0, 0.7d0); ab(15) = (12.0d0, -1.0d0); ab(16) = (0.0d0, 0.0d0)
  r(1) = 0.5d0; r(2) = 2.0d0; r(3) = 1.5d0; r(4) = 0.25d0
  c(1) = 0.5d0; c(2) = 2.0d0; c(3) = 1.5d0; c(4) = 0.25d0
  rowcnd = 0.01d0; colcnd = 0.01d0; amax = 12.0d0
  call zlaqgb(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('both_equil')
  call print_char('equed', equed)
  call print_array('ab', ab_r, 2*ldab*n)
  call end_test()

  ! Test 5: M=0 quick return
  call zlaqgb(0, 4, 1, 2, ab, 4, r, c, 1.0d0, 1.0d0, 12.0d0, equed)
  call begin_test('m_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 6: N=0 quick return
  call zlaqgb(4, 0, 1, 2, ab, 4, r, c, 1.0d0, 1.0d0, 12.0d0, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 7: No equil due to small amax
  m = 2; n = 2; kl = 1; ku = 0; ldab = 2
  ab(1) = (1.0d-200, 0.0d0); ab(2) = (1.0d-200, 0.0d0)
  ab(3) = (1.0d-200, 0.0d0); ab(4) = (1.0d-200, 0.0d0)
  r(1) = 1.0d0; r(2) = 1.0d0
  c(1) = 1.0d0; c(2) = 1.0d0
  rowcnd = 1.0d0; colcnd = 1.0d0; amax = 1.0d-200
  call zlaqgb(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('small_amax')
  call print_char('equed', equed)
  call end_test()

  ! Test 8: Non-square matrix (M != N)
  m = 3; n = 5; kl = 1; ku = 1; ldab = 3
  ab(1) = (0.0d0, 0.0d0); ab(2) = (1.0d0, 0.5d0);  ab(3) = (2.0d0, -0.3d0)
  ab(4) = (3.0d0, 1.0d0); ab(5) = (4.0d0, -1.0d0); ab(6) = (5.0d0, 0.2d0)
  ab(7) = (6.0d0, 0.7d0); ab(8) = (7.0d0, -0.5d0); ab(9) = (8.0d0, 1.0d0)
  ab(10) = (0.0d0, 0.0d0); ab(11) = (0.0d0, 0.0d0); ab(12) = (9.0d0, -0.1d0)
  ab(13) = (0.0d0, 0.0d0); ab(14) = (0.0d0, 0.0d0); ab(15) = (0.0d0, 0.0d0)
  r(1) = 0.5d0; r(2) = 2.0d0; r(3) = 1.5d0
  c(1) = 0.5d0; c(2) = 2.0d0; c(3) = 1.5d0; c(4) = 0.25d0; c(5) = 3.0d0
  rowcnd = 0.01d0; colcnd = 0.01d0; amax = 9.0d0
  call zlaqgb(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('nonsquare_both')
  call print_char('equed', equed)
  call print_array('ab', ab_r, 2*ldab*n)
  call end_test()

end program
