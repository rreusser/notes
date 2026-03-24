program test_dgees
  use test_utils
  implicit none

  double precision :: A(4,4), VS(4,4), WR(4), WI(4), WORK(200)
  logical :: BWORK(4)
  integer :: SDIM, INFO, LWORK
  logical, external :: sel_pos_real

  LWORK = 200

  ! Test 1: JOBVS='N', SORT='N', basic 3x3
  A = 0.0d0; VS = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0
  A(2,1) = 0.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 0.0d0; A(3,3) = 4.0d0
  call DGEES('N', 'N', sel_pos_real, 3, A, 4, SDIM, WR, WI, VS, 4, &
             WORK, LWORK, BWORK, INFO)
  call begin_test('NN basic 3x3 tri')
  call print_array('A', A, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('SDIM', SDIM)
  call print_int('info', INFO)
  call end_test()

  ! Test 2: JOBVS='V', SORT='N', 3x3 general
  A = 0.0d0; VS = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 0.0d0
  call DGEES('V', 'N', sel_pos_real, 3, A, 4, SDIM, WR, WI, VS, 4, &
             WORK, LWORK, BWORK, INFO)
  call begin_test('VN 3x3 general')
  call print_array('A', A, 16)
  call print_array('VS', VS, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('SDIM', SDIM)
  call print_int('info', INFO)
  call end_test()

  ! Test 3: JOBVS='V', SORT='S', select positive real part
  A = 0.0d0; VS = 0.0d0
  A(1,1) = 0.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0
  A(2,1) = -1.0d0; A(2,2) = 0.0d0; A(2,3) = 0.0d0
  A(3,1) = 0.0d0; A(3,2) = 0.0d0; A(3,3) = 2.0d0
  call DGEES('V', 'S', sel_pos_real, 3, A, 4, SDIM, WR, WI, VS, 4, &
             WORK, LWORK, BWORK, INFO)
  call begin_test('VS select positive real')
  call print_array('A', A, 16)
  call print_array('VS', VS, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('SDIM', SDIM)
  call print_int('info', INFO)
  call end_test()

  ! Test 4: N=0
  call DGEES('N', 'N', sel_pos_real, 0, A, 4, SDIM, WR, WI, VS, 4, &
             WORK, LWORK, BWORK, INFO)
  call begin_test('N=0')
  call print_int('SDIM', SDIM)
  call print_int('info', INFO)
  call end_test()

  ! Test 5: N=1
  A = 0.0d0; VS = 0.0d0
  A(1,1) = 5.0d0
  call DGEES('V', 'N', sel_pos_real, 1, A, 4, SDIM, WR, WI, VS, 4, &
             WORK, LWORK, BWORK, INFO)
  call begin_test('N=1')
  call print_array('A', A, 16)
  call print_array('VS', VS, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('SDIM', SDIM)
  call print_int('info', INFO)
  call end_test()

  ! Test 6: N=2 with complex eigenvalues
  A = 0.0d0; VS = 0.0d0
  A(1,1) = 0.0d0; A(1,2) = 1.0d0
  A(2,1) = -1.0d0; A(2,2) = 0.0d0
  call DGEES('V', 'N', sel_pos_real, 2, A, 4, SDIM, WR, WI, VS, 4, &
             WORK, LWORK, BWORK, INFO)
  call begin_test('N=2 complex eigs')
  call print_array('A', A, 16)
  call print_array('VS', VS, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('SDIM', SDIM)
  call print_int('info', INFO)
  call end_test()

  ! Test 7: 4x4 general matrix
  A = 0.0d0; VS = 0.0d0
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 0.0d0
  A(2,1) = 0.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0; A(2,4) = 1.0d0
  A(3,1) = 2.0d0; A(3,2) = 1.0d0; A(3,3) = 1.0d0; A(3,4) = 0.0d0
  A(4,1) = 0.0d0; A(4,2) = 0.0d0; A(4,3) = 1.0d0; A(4,4) = 2.0d0
  call DGEES('V', 'N', sel_pos_real, 4, A, 4, SDIM, WR, WI, VS, 4, &
             WORK, LWORK, BWORK, INFO)
  call begin_test('VN 4x4 general')
  call print_array('A', A, 16)
  call print_array('VS', VS, 16)
  call print_array('WR', WR, 4)
  call print_array('WI', WI, 4)
  call print_int('SDIM', SDIM)
  call print_int('info', INFO)
  call end_test()

end program

logical function sel_pos_real(wr, wi)
  implicit none
  double precision, intent(in) :: wr, wi
  sel_pos_real = (wr .gt. 0.0d0)
end function
