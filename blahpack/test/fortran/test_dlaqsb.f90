program test_dlaqsb
  use test_utils
  implicit none
  ! Band matrix: LDAB rows x N columns
  ! For upper: AB(KD+1+i-j, j) = A(i,j) for max(1,j-kd)<=i<=j
  ! For lower: AB(1+i-j, j) = A(i,j) for j<=i<=min(n,j+kd)
  integer, parameter :: NMAX = 10, LDAB = 5
  double precision :: ab(LDAB, NMAX), s(NMAX)
  double precision :: scond, amax
  character(1) :: equed
  integer :: n, kd

  ! Test 1: Upper triangle, KD=1, N=4, equilibration needed
  ! Band upper storage for 4x4 with KD=1 (tridiag upper):
  ! Row 0 (KD): superdiagonal
  ! Row 1 (KD+1): diagonal
  ! AB layout (LDAB=2 would suffice, using LDAB=5):
  !   col1: AB(2,1)=A(1,1)
  !   col2: AB(1,2)=A(1,2), AB(2,2)=A(2,2)
  !   col3: AB(1,3)=A(2,3), AB(2,3)=A(3,3)
  !   col4: AB(1,4)=A(3,4), AB(2,4)=A(4,4)
  n = 4
  kd = 1
  ab = 0.0d0
  ! Diagonal: AB(KD+1, j) = A(j,j)
  ab(2, 1) = 4.0d0
  ab(2, 2) = 9.0d0
  ab(2, 3) = 16.0d0
  ab(2, 4) = 25.0d0
  ! Superdiagonal: AB(KD, j) = A(j-1, j) for j=2..N
  ab(1, 2) = 1.0d0
  ab(1, 3) = 2.0d0
  ab(1, 4) = 3.0d0
  s(1) = 0.5d0; s(2) = 0.25d0; s(3) = 0.2d0; s(4) = 0.1d0
  scond = 0.02d0
  amax = 25.0d0
  call dlaqsb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('upper_kd1')
  ! Print the band rows that matter: rows 1..(KD+1) = rows 1..2
  call print_array('ab', ab, LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: Lower triangle, KD=1, N=4, equilibration needed
  ! Band lower storage:
  !   col1: AB(1,1)=A(1,1), AB(2,1)=A(2,1)
  !   col2: AB(1,2)=A(2,2), AB(2,2)=A(3,2)
  !   col3: AB(1,3)=A(3,3), AB(2,3)=A(4,3)
  !   col4: AB(1,4)=A(4,4)
  n = 4
  kd = 1
  ab = 0.0d0
  ! Diagonal: AB(1, j) = A(j,j)
  ab(1, 1) = 4.0d0
  ab(1, 2) = 9.0d0
  ab(1, 3) = 16.0d0
  ab(1, 4) = 25.0d0
  ! Subdiagonal: AB(2, j) = A(j+1, j) for j=1..N-1
  ab(2, 1) = 1.0d0
  ab(2, 2) = 2.0d0
  ab(2, 3) = 3.0d0
  s(1) = 0.5d0; s(2) = 0.25d0; s(3) = 0.2d0; s(4) = 0.1d0
  scond = 0.02d0
  amax = 25.0d0
  call dlaqsb('L', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('lower_kd1')
  call print_array('ab', ab, LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: No equilibration needed (good scond, amax in range)
  n = 4
  kd = 1
  ab = 0.0d0
  ab(2, 1) = 4.0d0; ab(2, 2) = 9.0d0; ab(2, 3) = 16.0d0; ab(2, 4) = 25.0d0
  ab(1, 2) = 1.0d0; ab(1, 3) = 2.0d0; ab(1, 4) = 3.0d0
  s(1) = 1.0d0; s(2) = 1.0d0; s(3) = 1.0d0; s(4) = 1.0d0
  scond = 0.5d0
  amax = 25.0d0
  call dlaqsb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('no_equilibrate')
  call print_array('ab', ab, LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: N=0 quick return
  n = 0
  kd = 1
  call dlaqsb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 5: N=1 edge case, upper, equilibration needed
  n = 1
  kd = 0
  ab = 0.0d0
  ab(1, 1) = 100.0d0
  s(1) = 0.1d0
  scond = 0.01d0
  amax = 100.0d0
  call dlaqsb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('n_one_upper')
  call print_array('ab', ab, LDAB * 1)
  call print_char('equed', equed)
  call end_test()

  ! Test 6: KD=2 (wider bandwidth), upper, N=4
  n = 4
  kd = 2
  ab = 0.0d0
  ! Diagonal: AB(KD+1, j) = A(j,j)
  ab(3, 1) = 10.0d0
  ab(3, 2) = 20.0d0
  ab(3, 3) = 30.0d0
  ab(3, 4) = 40.0d0
  ! First superdiagonal: AB(KD, j) = A(j-1,j)
  ab(2, 2) = 1.0d0
  ab(2, 3) = 2.0d0
  ab(2, 4) = 3.0d0
  ! Second superdiagonal: AB(KD-1, j) = A(j-2,j)
  ab(1, 3) = 0.5d0
  ab(1, 4) = 1.5d0
  s(1) = 0.5d0; s(2) = 0.25d0; s(3) = 0.2d0; s(4) = 0.1d0
  scond = 0.02d0
  amax = 40.0d0
  call dlaqsb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('upper_kd2')
  call print_array('ab', ab, LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 7: KD=2, lower, N=4
  n = 4
  kd = 2
  ab = 0.0d0
  ! Diagonal: AB(1, j) = A(j,j)
  ab(1, 1) = 10.0d0
  ab(1, 2) = 20.0d0
  ab(1, 3) = 30.0d0
  ab(1, 4) = 40.0d0
  ! First subdiagonal: AB(2, j) = A(j+1, j)
  ab(2, 1) = 1.0d0
  ab(2, 2) = 2.0d0
  ab(2, 3) = 3.0d0
  ! Second subdiagonal: AB(3, j) = A(j+2, j)
  ab(3, 1) = 0.5d0
  ab(3, 2) = 1.5d0
  s(1) = 0.5d0; s(2) = 0.25d0; s(3) = 0.2d0; s(4) = 0.1d0
  scond = 0.02d0
  amax = 40.0d0
  call dlaqsb('L', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('lower_kd2')
  call print_array('ab', ab, LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 8: amax very small (triggers equilibration via amax < SMALL)
  n = 2
  kd = 1
  ab = 0.0d0
  ab(2, 1) = 1.0d-300
  ab(1, 2) = 0.0d0
  ab(2, 2) = 1.0d-300
  s(1) = 1.0d150; s(2) = 1.0d150
  scond = 1.0d0
  amax = 1.0d-300
  call dlaqsb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('small_amax')
  call print_array('ab', ab, LDAB * n)
  call print_char('equed', equed)
  call end_test()

  ! Test 9: amax very large (triggers equilibration via amax > LARGE)
  n = 2
  kd = 1
  ab = 0.0d0
  ab(2, 1) = 1.0d300
  ab(1, 2) = 0.0d0
  ab(2, 2) = 1.0d300
  s(1) = 1.0d-150; s(2) = 1.0d-150
  scond = 1.0d0
  amax = 1.0d300
  call dlaqsb('U', n, kd, ab, LDAB, s, scond, amax, equed)
  call begin_test('large_amax')
  call print_array('ab', ab, LDAB * n)
  call print_char('equed', equed)
  call end_test()

end program
