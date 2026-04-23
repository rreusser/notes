program test_zhptrd
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: AP(NMAX*(NMAX+1)/2)
  complex*16 :: TAU(NMAX)
  double precision :: D(NMAX), E(NMAX)
  integer :: INFO

  ! EQUIVALENCE for printing interleaved re/im
  double precision :: AP_r(2*NMAX*(NMAX+1)/2)
  double precision :: TAU_r(2*NMAX)
  equivalence (AP, AP_r)
  equivalence (TAU, TAU_r)

  ! ============================================================
  ! Test 1: UPLO='U', 4x4 Hermitian matrix
  ! A = [4     1-i   -2+i   2   ]
  !     [1+i   2      0     1-i ]
  !     [-2-i  0      3    -2+i ]
  !     [2     1+i   -2-i  -1   ]
  ! Upper packed (column-major): col1=[4], col2=[1-i, 2], col3=[-2+i, 0, 3], col4=[2, 1-i, -2-i, -1]
  ! ============================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (-2.0d0, 1.0d0)
  AP(5) = (0.0d0, 0.0d0)
  AP(6) = (3.0d0, 0.0d0)
  AP(7) = (2.0d0, 0.0d0)
  AP(8) = (1.0d0, -1.0d0)
  AP(9) = (-2.0d0, -1.0d0)
  AP(10) = (-1.0d0, 0.0d0)

  call ZHPTRD('U', 4, AP, D, E, TAU, INFO)

  call begin_test('zhptrd_4x4_upper')
  call print_int('info', INFO)
  call print_array('D', D, 4)
  call print_array('E', E, 3)
  call print_array('TAU', TAU_r, 6)
  call print_array('AP', AP_r, 20)
  call end_test()

  ! ============================================================
  ! Test 2: UPLO='L', 4x4 Hermitian matrix
  ! Same matrix, lower packed
  ! Lower packed (column-major): col1=[4,1+i,-2-i,2], col2=[2,0,1-i], col3=[3,-2+i], col4=[-1]
  ! ============================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (-2.0d0, -1.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (2.0d0, 0.0d0)
  AP(6) = (0.0d0, 0.0d0)
  AP(7) = (1.0d0, -1.0d0)
  AP(8) = (3.0d0, 0.0d0)
  AP(9) = (-2.0d0, 1.0d0)
  AP(10) = (-1.0d0, 0.0d0)

  call ZHPTRD('L', 4, AP, D, E, TAU, INFO)

  call begin_test('zhptrd_4x4_lower')
  call print_int('info', INFO)
  call print_array('D', D, 4)
  call print_array('E', E, 3)
  call print_array('TAU', TAU_r, 6)
  call print_array('AP', AP_r, 20)
  call end_test()

  ! ============================================================
  ! Test 3: N=1
  ! ============================================================
  AP(1) = (5.0d0, 0.0d0)

  call ZHPTRD('U', 1, AP, D, E, TAU, INFO)

  call begin_test('zhptrd_1x1')
  call print_int('info', INFO)
  call print_array('D', D, 1)
  call end_test()

  ! ============================================================
  ! Test 4: N=0
  ! ============================================================
  call ZHPTRD('U', 0, AP, D, E, TAU, INFO)

  call begin_test('zhptrd_0x0')
  call print_int('info', INFO)
  call end_test()

end program test_zhptrd
