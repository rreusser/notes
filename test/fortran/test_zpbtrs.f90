program test_zpbtrs
  use test_utils
  implicit none
  complex*16 :: AB(20), B(20), AB_save(20)
  double precision :: AB_r(40), B_r(40)
  equivalence (AB, AB_r)
  equivalence (B, B_r)
  integer :: info

  ! Test 1: upper, N=3, KD=1, NRHS=1
  ! HPD tridiagonal: A = [4 (1+i) 0; (1-i) 5 (2-i); 0 (2+i) 6]
  ! Band storage (LDAB=2, upper):
  !   Row 0 (superdiag): *    (1+i)  (2-i)
  !   Row 1 (diagonal):  4     5      6
  AB = (0.0d0, 0.0d0)
  AB(2) = (4.0d0, 0.0d0)
  AB(3) = (1.0d0, 1.0d0)
  AB(4) = (5.0d0, 0.0d0)
  AB(5) = (2.0d0, -1.0d0)
  AB(6) = (6.0d0, 0.0d0)
  AB_save = AB
  ! Factor first
  call zpbtrf('U', 3, 1, AB, 2, info)
  ! RHS: b = A * [1, 1+i, 2]
  ! row1: 4*1 + (1+i)(1+i) = 4 + 2i = 4+2i
  ! row2: (1-i)*1 + 5*(1+i) + (2-i)*2 = (1-i)+(5+5i)+(4-2i) = 10+2i
  ! row3: (2+i)*(1+i) + 6*2 = (2+2i+i-1) + 12 = 13+3i
  B(1) = (4.0d0, 2.0d0)
  B(2) = (10.0d0, 2.0d0)
  B(3) = (13.0d0, 3.0d0)
  call zpbtrs('U', 3, 1, 1, AB, 2, B, 3, info)
  call begin_test('upper_single_rhs')
  call print_int('info', info)
  call print_array('B', B_r, 6)
  call end_test()

  ! Test 2: lower, N=3, KD=1, NRHS=1
  ! Same HPD matrix in lower band storage (LDAB=2):
  !   Row 0 (diagonal):  4     5      6
  !   Row 1 (subdiag):  (1-i) (2+i)    *
  AB = (0.0d0, 0.0d0)
  AB(1) = (4.0d0, 0.0d0)
  AB(2) = (1.0d0, -1.0d0)
  AB(3) = (5.0d0, 0.0d0)
  AB(4) = (2.0d0, 1.0d0)
  AB(5) = (6.0d0, 0.0d0)
  call zpbtrf('L', 3, 1, AB, 2, info)
  B(1) = (4.0d0, 2.0d0)
  B(2) = (10.0d0, 2.0d0)
  B(3) = (13.0d0, 3.0d0)
  call zpbtrs('L', 3, 1, 1, AB, 2, B, 3, info)
  call begin_test('lower_single_rhs')
  call print_int('info', info)
  call print_array('B', B_r, 6)
  call end_test()

  ! Test 3: N=0 quick return
  call zpbtrs('U', 0, 1, 1, AB, 2, B, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: NRHS=0 quick return
  call zpbtrs('U', 3, 1, 0, AB, 2, B, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: upper, N=3, KD=1, NRHS=2
  AB = (0.0d0, 0.0d0)
  AB(2) = (4.0d0, 0.0d0)
  AB(3) = (1.0d0, 1.0d0)
  AB(4) = (5.0d0, 0.0d0)
  AB(5) = (2.0d0, -1.0d0)
  AB(6) = (6.0d0, 0.0d0)
  call zpbtrf('U', 3, 1, AB, 2, info)
  ! b1 = A*[1,1+i,2], b2 = A*[i, 0, 1-i]
  ! b2: row1: 4*i + 0 = 4i
  ! row2: (1-i)*i + 0 + (2-i)*(1-i) = (i+1) + (2-2i-i-1) = (i+1)+(1-3i) = 2-2i
  ! row3: 0 + 6*(1-i) = 6-6i
  B(1) = (4.0d0, 2.0d0)
  B(2) = (10.0d0, 2.0d0)
  B(3) = (13.0d0, 3.0d0)
  B(4) = (0.0d0, 4.0d0)
  B(5) = (2.0d0, -2.0d0)
  B(6) = (6.0d0, -6.0d0)
  call zpbtrs('U', 3, 1, 2, AB, 2, B, 3, info)
  call begin_test('upper_two_rhs')
  call print_int('info', info)
  call print_array('B', B_r, 12)
  call end_test()

  ! Test 6: lower, N=3, KD=1, NRHS=2
  AB = (0.0d0, 0.0d0)
  AB(1) = (4.0d0, 0.0d0)
  AB(2) = (1.0d0, -1.0d0)
  AB(3) = (5.0d0, 0.0d0)
  AB(4) = (2.0d0, 1.0d0)
  AB(5) = (6.0d0, 0.0d0)
  call zpbtrf('L', 3, 1, AB, 2, info)
  B(1) = (4.0d0, 2.0d0)
  B(2) = (10.0d0, 2.0d0)
  B(3) = (13.0d0, 3.0d0)
  B(4) = (0.0d0, 4.0d0)
  B(5) = (2.0d0, -2.0d0)
  B(6) = (6.0d0, -6.0d0)
  call zpbtrs('L', 3, 1, 2, AB, 2, B, 3, info)
  call begin_test('lower_two_rhs')
  call print_int('info', info)
  call print_array('B', B_r, 12)
  call end_test()

end program
