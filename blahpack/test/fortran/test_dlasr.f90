program test_dlasr
  use test_utils
  implicit none

  integer, parameter :: LDA = 4
  double precision :: A(LDA, 4), A0(LDA, 4)
  double precision :: C(3), S(3)
  integer :: M, N, i, j

  ! Helper matrix: 4x4 with distinct values
  ! A = [1  5   9  13]
  !     [2  6  10  14]
  !     [3  7  11  15]
  !     [4  8  12  16]
  ! (column-major storage)

  ! ---- Test: M=0 quick return ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 1.0d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.0d0
  call DLASR('L', 'V', 'F', 0, 4, C, S, A, LDA)
  call begin_test('m_zero')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- Test: N=0 quick return ----
  call init_matrix(A, LDA, 4, 4)
  call DLASR('L', 'V', 'F', 4, 0, C, S, A, LDA)
  call begin_test('n_zero')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='L', PIVOT='V', DIRECT='F' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('L', 'V', 'F', 4, 4, C, S, A, LDA)
  call begin_test('L_V_F')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='L', PIVOT='V', DIRECT='B' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('L', 'V', 'B', 4, 4, C, S, A, LDA)
  call begin_test('L_V_B')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='L', PIVOT='T', DIRECT='F' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('L', 'T', 'F', 4, 4, C, S, A, LDA)
  call begin_test('L_T_F')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='L', PIVOT='T', DIRECT='B' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('L', 'T', 'B', 4, 4, C, S, A, LDA)
  call begin_test('L_T_B')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='L', PIVOT='B', DIRECT='F' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('L', 'B', 'F', 4, 4, C, S, A, LDA)
  call begin_test('L_B_F')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='L', PIVOT='B', DIRECT='B' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('L', 'B', 'B', 4, 4, C, S, A, LDA)
  call begin_test('L_B_B')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='R', PIVOT='V', DIRECT='F' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('R', 'V', 'F', 4, 4, C, S, A, LDA)
  call begin_test('R_V_F')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='R', PIVOT='V', DIRECT='B' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('R', 'V', 'B', 4, 4, C, S, A, LDA)
  call begin_test('R_V_B')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='R', PIVOT='T', DIRECT='F' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('R', 'T', 'F', 4, 4, C, S, A, LDA)
  call begin_test('R_T_F')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='R', PIVOT='T', DIRECT='B' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('R', 'T', 'B', 4, 4, C, S, A, LDA)
  call begin_test('R_T_B')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='R', PIVOT='B', DIRECT='F' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('R', 'B', 'F', 4, 4, C, S, A, LDA)
  call begin_test('R_B_F')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

  ! ---- SIDE='R', PIVOT='B', DIRECT='B' ----
  call init_matrix(A, LDA, 4, 4)
  C(1) = 0.6d0; C(2) = 0.8d0; C(3) = 0.5d0
  S(1) = 0.8d0; S(2) = 0.6d0; S(3) = 0.866025403784439d0
  call DLASR('R', 'B', 'B', 4, 4, C, S, A, LDA)
  call begin_test('R_B_B')
  call print_matrix('A', A, LDA, 4, 4)
  call end_test()

contains

  subroutine init_matrix(A, LDA, M, N)
    integer, intent(in) :: LDA, M, N
    double precision, intent(out) :: A(LDA, *)
    integer :: i, j
    do j = 1, N
      do i = 1, M
        A(i, j) = dble((j-1)*M + i)
      end do
    end do
  end subroutine

end program
