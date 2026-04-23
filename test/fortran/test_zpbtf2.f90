program test_zpbtf2
  use test_utils
  implicit none
  complex*16 :: AB(100)
  double precision :: AB_r(200)
  equivalence (AB, AB_r)
  integer :: info

  ! Test 1: upper, N=3, KD=1
  ! HPD tridiagonal matrix stored in banded form (LDAB=2):
  !   A = [ 4    (1+i)   0   ]
  !       [(1-i)  5    (2-i)  ]
  !       [  0   (2+i)   6   ]
  ! Band storage (upper, LDAB=2):
  !   row 1 (superdiag): *    (1+i)  (2-i)
  !   row 2 (diagonal):  4     5      6
  AB = (0.0d0, 0.0d0)
  ! Column 1: AB(1,1)=*, AB(2,1)=4
  AB(2) = (4.0d0, 0.0d0)
  ! Column 2: AB(1,2)=(1+i), AB(2,2)=5
  AB(3) = (1.0d0, 1.0d0)
  AB(4) = (5.0d0, 0.0d0)
  ! Column 3: AB(1,3)=(2-i), AB(2,3)=6
  AB(5) = (2.0d0, -1.0d0)
  AB(6) = (6.0d0, 0.0d0)
  call zpbtf2('U', 3, 1, AB, 2, info)
  call begin_test('upper_3x3_kd1')
  call print_int('info', info)
  call print_array('AB', AB_r, 12)
  call end_test()

  ! Test 2: lower, N=3, KD=1
  ! Same HPD tridiagonal matrix stored in lower banded form (LDAB=2):
  !   row 1 (diagonal):  4     5      6
  !   row 2 (subdiag):  (1-i) (2+i)    *
  AB = (0.0d0, 0.0d0)
  ! Column 1: AB(1,1)=4, AB(2,1)=(1-i)
  AB(1) = (4.0d0, 0.0d0)
  AB(2) = (1.0d0, -1.0d0)
  ! Column 2: AB(1,2)=5, AB(2,2)=(2+i)
  AB(3) = (5.0d0, 0.0d0)
  AB(4) = (2.0d0, 1.0d0)
  ! Column 3: AB(1,3)=6, AB(2,3)=*
  AB(5) = (6.0d0, 0.0d0)
  call zpbtf2('L', 3, 1, AB, 2, info)
  call begin_test('lower_3x3_kd1')
  call print_int('info', info)
  call print_array('AB', AB_r, 12)
  call end_test()

  ! Test 3: N=0 quick return
  info = -1
  call zpbtf2('U', 0, 1, AB, 2, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=1
  AB(1) = (9.0d0, 0.0d0)
  call zpbtf2('U', 1, 0, AB, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('AB', AB_r, 2)
  call end_test()

  ! Test 5: upper, N=4, KD=2
  ! HPD matrix with bandwidth 2 stored in upper band (LDAB=3):
  ! A = [10    (1+i)   (0.5-i)   0  ]
  !     [(1-i)  8      (2+i)    (1-i)]
  !     [(0.5+i)(2-i)   6       (1+i)]
  !     [  0   (1+i)   (1-i)     7   ]
  AB = (0.0d0, 0.0d0)
  ! Col 1: AB(3,1)=10
  AB(3) = (10.0d0, 0.0d0)
  ! Col 2: AB(2,2)=(1+i), AB(3,2)=8
  AB(5) = (1.0d0, 1.0d0)
  AB(6) = (8.0d0, 0.0d0)
  ! Col 3: AB(1,3)=(0.5-i), AB(2,3)=(2+i), AB(3,3)=6
  AB(7) = (0.5d0, -1.0d0)
  AB(8) = (2.0d0, 1.0d0)
  AB(9) = (6.0d0, 0.0d0)
  ! Col 4: AB(2,4)=(1-i), AB(3,4)=7
  AB(11) = (1.0d0, -1.0d0)
  AB(12) = (7.0d0, 0.0d0)
  call zpbtf2('U', 4, 2, AB, 3, info)
  call begin_test('upper_4x4_kd2')
  call print_int('info', info)
  call print_array('AB', AB_r, 24)
  call end_test()

  ! Test 6: lower, N=4, KD=2
  ! Same HPD matrix in lower band (LDAB=3):
  AB = (0.0d0, 0.0d0)
  ! Col 1: AB(1,1)=10, AB(2,1)=(1-i), AB(3,1)=(0.5+i)
  AB(1) = (10.0d0, 0.0d0)
  AB(2) = (1.0d0, -1.0d0)
  AB(3) = (0.5d0, 1.0d0)
  ! Col 2: AB(1,2)=8, AB(2,2)=(2-i), AB(3,2)=(1+i)
  AB(4) = (8.0d0, 0.0d0)
  AB(5) = (2.0d0, -1.0d0)
  AB(6) = (1.0d0, 1.0d0)
  ! Col 3: AB(1,3)=6, AB(2,3)=(1-i)
  AB(7) = (6.0d0, 0.0d0)
  AB(8) = (1.0d0, -1.0d0)
  ! Col 4: AB(1,4)=7
  AB(10) = (7.0d0, 0.0d0)
  call zpbtf2('L', 4, 2, AB, 3, info)
  call begin_test('lower_4x4_kd2')
  call print_int('info', info)
  call print_array('AB', AB_r, 24)
  call end_test()

  ! Test 7: not HPD (upper)
  AB = (0.0d0, 0.0d0)
  AB(2) = (1.0d0, 0.0d0)  ! diag A(1,1)=1
  AB(3) = (2.0d0, 1.0d0)  ! superdiag
  AB(4) = (1.0d0, 0.0d0)  ! diag A(2,2)=1
  ! |a12|^2 = 5 > a11*a22 = 1, not HPD
  call zpbtf2('U', 2, 1, AB, 2, info)
  call begin_test('not_hpd')
  call print_int('info', info)
  call end_test()

  ! Test 8: not HPD (lower)
  AB = (0.0d0, 0.0d0)
  AB(1) = (1.0d0, 0.0d0)  ! diag A(1,1)=1
  AB(2) = (2.0d0, -1.0d0) ! subdiag
  AB(3) = (1.0d0, 0.0d0)  ! diag A(2,2)=1
  call zpbtf2('L', 2, 1, AB, 2, info)
  call begin_test('not_hpd_lower')
  call print_int('info', info)
  call end_test()

end program
