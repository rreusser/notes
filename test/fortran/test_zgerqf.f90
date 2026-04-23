program test_zgerqf
  use test_utils
  implicit none

  integer, parameter :: BIG = 40
  ! Use exact-size arrays for small tests
  complex*16 :: A34(3, 4), TAU34(3), WORK(6000)
  double precision :: A34_r(24), TAU34_r(6)
  equivalence (A34, A34_r)
  equivalence (TAU34, TAU34_r)

  complex*16 :: A43(4, 3), TAU43(3)
  double precision :: A43_r(24), TAU43_r(6)
  equivalence (A43, A43_r)
  equivalence (TAU43, TAU43_r)

  complex*16 :: A33(3, 3), TAU33(3)
  double precision :: A33_r(18), TAU33_r(6)
  equivalence (A33, A33_r)
  equivalence (TAU33, TAU33_r)

  complex*16 :: A11(1, 1), TAU11(1)
  double precision :: A11_r(2), TAU11_r(2)
  equivalence (A11, A11_r)
  equivalence (TAU11, TAU11_r)

  complex*16 :: A25(2, 5), TAU25(2)
  double precision :: A25_r(20), TAU25_r(4)
  equivalence (A25, A25_r)
  equivalence (TAU25, TAU25_r)

  complex*16 :: ABIG(BIG, BIG), TAUBIG(BIG), WBIG(100000)
  double precision :: ABIG_r(BIG*BIG*2), TAUBIG_r(BIG*2)
  equivalence (ABIG, ABIG_r)
  equivalence (TAUBIG, TAUBIG_r)

  integer :: info, i, j

  ! Test 1: 3x4 (M < N)
  A34 = (0.0d0, 0.0d0)
  A34(1,1) = (2.0d0, 1.0d0); A34(1,2) = (1.0d0, 2.0d0); A34(1,3) = (3.0d0, 0.0d0); A34(1,4) = (1.0d0, 1.0d0)
  A34(2,1) = (1.0d0, 0.0d0); A34(2,2) = (4.0d0, 1.0d0); A34(2,3) = (2.0d0, -1.0d0); A34(2,4) = (3.0d0, 0.0d0)
  A34(3,1) = (3.0d0, -1.0d0); A34(3,2) = (2.0d0, 0.0d0); A34(3,3) = (5.0d0, 2.0d0); A34(3,4) = (2.0d0, -2.0d0)
  call zgerqf(3, 4, A34, 3, TAU34, WORK, 6000, info)
  call begin_test('3x4')
  call print_array('a', A34_r, 2*3*4)
  call print_array('tau', TAU34_r, 2*3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x3 (M > N)
  A43 = (0.0d0, 0.0d0)
  A43(1,1) = (2.0d0, 1.0d0); A43(1,2) = (1.0d0, 0.0d0); A43(1,3) = (3.0d0, 1.0d0)
  A43(2,1) = (1.0d0, -1.0d0); A43(2,2) = (4.0d0, 1.0d0); A43(2,3) = (2.0d0, 0.0d0)
  A43(3,1) = (3.0d0, 0.0d0); A43(3,2) = (2.0d0, -1.0d0); A43(3,3) = (5.0d0, -2.0d0)
  A43(4,1) = (1.0d0, 2.0d0); A43(4,2) = (3.0d0, 0.0d0); A43(4,3) = (1.0d0, 1.0d0)
  call zgerqf(4, 3, A43, 4, TAU43, WORK, 6000, info)
  call begin_test('4x3')
  call print_array('a', A43_r, 2*4*3)
  call print_array('tau', TAU43_r, 2*3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 (square)
  A33 = (0.0d0, 0.0d0)
  A33(1,1) = (4.0d0, 1.0d0); A33(1,2) = (1.0d0, -1.0d0); A33(1,3) = (2.0d0, 0.0d0)
  A33(2,1) = (1.0d0, 0.0d0); A33(2,2) = (3.0d0, 2.0d0); A33(2,3) = (1.0d0, 1.0d0)
  A33(3,1) = (2.0d0, -1.0d0); A33(3,2) = (1.0d0, 0.0d0); A33(3,3) = (5.0d0, -2.0d0)
  call zgerqf(3, 3, A33, 3, TAU33, WORK, 6000, info)
  call begin_test('3x3')
  call print_array('a', A33_r, 2*3*3)
  call print_array('tau', TAU33_r, 2*3)
  call print_int('info', info)
  call end_test()

  ! Test 4: 1x1
  A11 = (0.0d0, 0.0d0)
  A11(1,1) = (5.0d0, 3.0d0)
  call zgerqf(1, 1, A11, 1, TAU11, WORK, 6000, info)
  call begin_test('1x1')
  call print_array('a', A11_r, 2)
  call print_array('tau', TAU11_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 5: M=0
  call zgerqf(0, 3, A33, 3, TAU33, WORK, 6000, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0
  call zgerqf(3, 0, A33, 3, TAU33, WORK, 6000, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: 2x5 (wide)
  A25 = (0.0d0, 0.0d0)
  A25(1,1) = (1.0d0, 0.0d0); A25(1,2) = (2.0d0, 1.0d0); A25(1,3) = (3.0d0, -1.0d0); A25(1,4) = (4.0d0, 0.0d0); A25(1,5) = (5.0d0, 1.0d0)
  A25(2,1) = (6.0d0, -1.0d0); A25(2,2) = (7.0d0, 0.0d0); A25(2,3) = (8.0d0, 2.0d0); A25(2,4) = (9.0d0, -1.0d0); A25(2,5) = (10.0d0, 0.0d0)
  call zgerqf(2, 5, A25, 2, TAU25, WORK, 6000, info)
  call begin_test('2x5')
  call print_array('a', A25_r, 2*2*5)
  call print_array('tau', TAU25_r, 2*2)
  call print_int('info', info)
  call end_test()

  ! Test 8: Large 40x40 matrix (triggers blocked path)
  do j = 1, BIG
    do i = 1, BIG
      ABIG(i, j) = dcmplx(dble(mod(i*7 + j*13, 97)) / 97.0d0, dble(mod(i*3 + j*11, 89)) / 89.0d0)
    end do
  end do
  call zgerqf(BIG, BIG, ABIG, BIG, TAUBIG, WBIG, 100000, info)
  call begin_test('40x40')
  call print_array('a', ABIG_r, 2*BIG*BIG)
  call print_array('tau', TAUBIG_r, 2*BIG)
  call print_int('info', info)
  call end_test()

end program
