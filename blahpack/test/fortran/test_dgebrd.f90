program test_dgebrd
  use test_utils
  implicit none

  integer, parameter :: BIGM = 35, BIGN = 33
  double precision :: A(5000), D(100), E(100), TAUQ(100), TAUP(100)
  double precision :: WORK(100000)
  integer :: INFO, LWORK, i, j

  LWORK = 100000

  ! Test 1: 4x3 matrix (M > N, upper bidiagonal)
  A = 0.0d0
  ! Column-major: A(i, j) at index (j-1)*LDA + i, LDA=4
  A(1) = 2.0d0; A(2) = 1.0d0; A(3) = 3.0d0; A(4) = 1.0d0
  A(5) = 1.0d0; A(6) = 4.0d0; A(7) = 2.0d0; A(8) = 3.0d0
  A(9) = 3.0d0; A(10) = 2.0d0; A(11) = 5.0d0; A(12) = 1.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(4, 3, A, 4, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call begin_test('upper_4x3')
  call print_int('INFO', INFO)
  call print_array('A', A, 12)
  call print_array('D', D, 3)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call end_test()

  ! Test 2: 3x4 matrix (M < N, lower bidiagonal)
  A = 0.0d0
  ! LDA=3
  A(1) = 2.0d0; A(2) = 4.0d0; A(3) = 1.0d0
  A(4) = 1.0d0; A(5) = 2.0d0; A(6) = 5.0d0
  A(7) = 3.0d0; A(8) = 1.0d0; A(9) = 2.0d0
  A(10) = 1.0d0; A(11) = 3.0d0; A(12) = 4.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(3, 4, A, 3, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call begin_test('lower_3x4')
  call print_int('INFO', INFO)
  call print_array('A', A, 12)
  call print_array('D', D, 3)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call end_test()

  ! Test 3: 3x3 square matrix
  A = 0.0d0
  A(1) = 5.0d0; A(2) = 3.0d0; A(3) = 1.0d0
  A(4) = 2.0d0; A(5) = 4.0d0; A(6) = 3.0d0
  A(7) = 1.0d0; A(8) = 2.0d0; A(9) = 6.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(3, 3, A, 3, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call begin_test('square_3x3')
  call print_int('INFO', INFO)
  call print_array('A', A, 9)
  call print_array('D', D, 3)
  call print_array('E', E, 2)
  call print_array('TAUQ', TAUQ, 3)
  call print_array('TAUP', TAUP, 3)
  call end_test()

  ! Test 4: 1x1 matrix
  A = 0.0d0
  A(1) = 7.0d0
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(1, 1, A, 1, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call begin_test('one_by_one')
  call print_int('INFO', INFO)
  call print_array('A', A, 1)
  call print_array('D', D, 1)
  call print_array('TAUQ', TAUQ, 1)
  call print_array('TAUP', TAUP, 1)
  call end_test()

  ! Test 5: M=0 quick return
  call DGEBRD(0, 3, A, 1, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call begin_test('m_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: N=0 quick return
  call DGEBRD(3, 0, A, 3, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: 35x33 matrix (M > N, large enough to trigger blocking with NB=32)
  ! Use a diagonally dominant matrix for numerical stability
  A = 0.0d0
  do j = 1, BIGN
    do i = 1, BIGM
      if (i .eq. j) then
        A((j-1)*BIGM + i) = dble(i+j+10) + sin(dble(i+2*j))
      else
        A((j-1)*BIGM + i) = sin(dble(i+2*j))
      end if
    end do
  end do
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(BIGM, BIGN, A, BIGM, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call begin_test('upper_35x33')
  call print_int('INFO', INFO)
  call print_array('D', D, BIGN)
  call print_array('E', E, BIGN-1)
  call print_array('TAUQ', TAUQ, BIGN)
  call print_array('TAUP', TAUP, BIGN)
  call end_test()

  ! Test 8: 33x35 matrix (M < N, large enough to trigger blocking with NB=32)
  A = 0.0d0
  do j = 1, BIGM
    do i = 1, BIGN
      if (i .eq. j) then
        A((j-1)*BIGN + i) = dble(i+j+10) + sin(dble(i+2*j))
      else
        A((j-1)*BIGN + i) = sin(dble(i+2*j))
      end if
    end do
  end do
  D = 0.0d0; E = 0.0d0; TAUQ = 0.0d0; TAUP = 0.0d0
  call DGEBRD(BIGN, BIGM, A, BIGN, D, E, TAUQ, TAUP, WORK, LWORK, INFO)
  call begin_test('lower_33x35')
  call print_int('INFO', INFO)
  call print_array('D', D, BIGN)
  call print_array('E', E, BIGN-1)
  call print_array('TAUQ', TAUQ, BIGN)
  call print_array('TAUP', TAUP, BIGN)
  call end_test()

end program
