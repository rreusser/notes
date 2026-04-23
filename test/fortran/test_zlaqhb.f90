program test_zlaqhb
  use test_utils
  implicit none
  integer, parameter :: NMAX = 10, LDAB = 5
  complex*16 :: ab(LDAB, NMAX)
  double precision :: ab_r(2*LDAB*NMAX)
  equivalence (ab, ab_r)
  double precision :: s(NMAX)
  double precision :: scond, amax
  character :: equed
  integer :: n, kd

  ! Test 1: Upper triangle, KD=1, N=4, equilibration needed
  ! Hermitian: diagonal is real-valued, off-diag complex
  ! Diagonal entries given with nonzero imag to verify DBLE() zeroing
  n = 4
  kd = 1
  ab = (0.0d0, 0.0d0)
  ! Diagonal: AB(KD+1, j) = A(j,j) — give imaginary parts to test zeroing
  ab(2, 1) = (4.0d0, 0.1d0)
  ab(2, 2) = (9.0d0, 0.2d0)
  ab(2, 3) = (16.0d0, 0.3d0)
  ab(2, 4) = (25.0d0, 0.4d0)
  ! Superdiagonal: AB(KD, j) = A(j-1,j) for j=2..N
  ab(1, 2) = (1.0d0, 2.0d0)
  ab(1, 3) = (3.0d0, 4.0d0)
  ab(1, 4) = (5.0d0, 6.0d0)
  s(1) = 0.5d0; s(2) = 0.25d0; s(3) = 0.2d0; s(4) = 0.1d0
  scond = 0.02d0
  amax = 25.0d0
  call zlaqhb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('upper_kd1')
  call print_array('ab', ab_r, 2 * LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: Lower triangle, KD=1, N=4, equilibration needed
  n = 4
  kd = 1
  ab = (0.0d0, 0.0d0)
  ! Diagonal: AB(1, j) = A(j,j) — with nonzero imag
  ab(1, 1) = (4.0d0, 0.1d0)
  ab(1, 2) = (9.0d0, 0.2d0)
  ab(1, 3) = (16.0d0, 0.3d0)
  ab(1, 4) = (25.0d0, 0.4d0)
  ! Subdiagonal: AB(2, j) = A(j+1, j) for j=1..N-1
  ab(2, 1) = (1.0d0, 2.0d0)
  ab(2, 2) = (3.0d0, 4.0d0)
  ab(2, 3) = (5.0d0, 6.0d0)
  s(1) = 0.5d0; s(2) = 0.25d0; s(3) = 0.2d0; s(4) = 0.1d0
  scond = 0.02d0
  amax = 25.0d0
  call zlaqhb('L', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('lower_kd1')
  call print_array('ab', ab_r, 2 * LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: No equilibration needed (good scond, amax in range)
  n = 4
  kd = 1
  ab = (0.0d0, 0.0d0)
  ab(2, 1) = (4.0d0, 0.1d0)
  ab(2, 2) = (9.0d0, 0.2d0)
  ab(2, 3) = (16.0d0, 0.3d0)
  ab(2, 4) = (25.0d0, 0.4d0)
  ab(1, 2) = (1.0d0, 2.0d0)
  ab(1, 3) = (3.0d0, 4.0d0)
  ab(1, 4) = (5.0d0, 6.0d0)
  s(1) = 1.0d0; s(2) = 1.0d0; s(3) = 1.0d0; s(4) = 1.0d0
  scond = 0.5d0
  amax = 25.0d0
  call zlaqhb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('no_equilibrate')
  call print_array('ab', ab_r, 2 * LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: N=0 quick return
  n = 0
  kd = 1
  call zlaqhb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 5: N=1, upper, KD=0, equilibration needed
  ! Single element with nonzero imaginary part
  n = 1
  kd = 0
  ab = (0.0d0, 0.0d0)
  ab(1, 1) = (100.0d0, 7.5d0)
  s(1) = 0.1d0
  scond = 0.01d0
  amax = 100.0d0
  call zlaqhb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('n_one_upper')
  call print_array('ab', ab_r, 2 * LDAB * 1)
  call print_char('equed', equed)
  call end_test()

  ! Test 6: KD=2 (wider bandwidth), upper, N=4
  n = 4
  kd = 2
  ab = (0.0d0, 0.0d0)
  ! Diagonal: AB(KD+1, j) = A(j,j) — with imaginary parts
  ab(3, 1) = (10.0d0, 0.5d0)
  ab(3, 2) = (20.0d0, 0.7d0)
  ab(3, 3) = (30.0d0, 0.9d0)
  ab(3, 4) = (40.0d0, 1.1d0)
  ! First superdiagonal: AB(KD, j) = A(j-1,j) for j>=2
  ab(2, 2) = (1.0d0, 0.5d0)
  ab(2, 3) = (2.0d0, 1.0d0)
  ab(2, 4) = (3.0d0, 1.5d0)
  ! Second superdiagonal: AB(KD-1, j) = A(j-2,j) for j>=3
  ab(1, 3) = (0.5d0, 0.25d0)
  ab(1, 4) = (1.5d0, 0.75d0)
  s(1) = 0.5d0; s(2) = 0.25d0; s(3) = 0.2d0; s(4) = 0.1d0
  scond = 0.02d0
  amax = 40.0d0
  call zlaqhb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('upper_kd2')
  call print_array('ab', ab_r, 2 * LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 7: KD=2, lower, N=4
  n = 4
  kd = 2
  ab = (0.0d0, 0.0d0)
  ! Diagonal: AB(1, j) = A(j,j) — with imaginary parts
  ab(1, 1) = (10.0d0, 0.5d0)
  ab(1, 2) = (20.0d0, 0.7d0)
  ab(1, 3) = (30.0d0, 0.9d0)
  ab(1, 4) = (40.0d0, 1.1d0)
  ! First subdiagonal: AB(2, j) = A(j+1, j)
  ab(2, 1) = (1.0d0, 0.5d0)
  ab(2, 2) = (2.0d0, 1.0d0)
  ab(2, 3) = (3.0d0, 1.5d0)
  ! Second subdiagonal: AB(3, j) = A(j+2, j)
  ab(3, 1) = (0.5d0, 0.25d0)
  ab(3, 2) = (1.5d0, 0.75d0)
  s(1) = 0.5d0; s(2) = 0.25d0; s(3) = 0.2d0; s(4) = 0.1d0
  scond = 0.02d0
  amax = 40.0d0
  call zlaqhb('L', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('lower_kd2')
  call print_array('ab', ab_r, 2 * LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 8: amax very small triggers equilibration
  n = 2
  kd = 1
  ab = (0.0d0, 0.0d0)
  ab(2, 1) = (1.0d-300, 0.5d-300)
  ab(1, 2) = (0.0d0, 0.0d0)
  ab(2, 2) = (1.0d-300, 0.3d-300)
  s(1) = 1.0d150; s(2) = 1.0d150
  scond = 1.0d0
  amax = 1.0d-300
  call zlaqhb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('small_amax')
  call print_array('ab', ab_r, 2 * LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 9: amax very large triggers equilibration
  n = 2
  kd = 1
  ab = (0.0d0, 0.0d0)
  ab(2, 1) = (1.0d300, 0.5d300)
  ab(1, 2) = (0.0d0, 0.0d0)
  ab(2, 2) = (1.0d300, 0.3d300)
  s(1) = 1.0d-150; s(2) = 1.0d-150
  scond = 1.0d0
  amax = 1.0d300
  call zlaqhb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('large_amax')
  call print_array('ab', ab_r, 2 * LDAB * n)
  call print_char('equed', equed)
  call end_test()

end program
