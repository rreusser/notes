program test_zhptri
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  complex*16 :: ap(NMAX*(NMAX+1)/2), work(NMAX)
  double precision :: ap_r(2*NMAX*(NMAX+1)/2), work_r(2*NMAX)
  equivalence (ap, ap_r)
  equivalence (work, work_r)
  integer :: ipiv(NMAX), info, nap, n

  ! Test 1: N=0 quick return
  n = 0
  info = -999
  call zhptri('U', n, ap, ipiv, work, info)
  call begin_test('n0')
  call print_int('info', info)
  call end_test()

  ! Test 2: N=1 upper
  n = 1
  nap = 1
  ap(1) = (4.0d0, 0.0d0)
  call zhptrf('U', n, ap, ipiv, info)
  call zhptri('U', n, ap, ipiv, work, info)
  call begin_test('n1_upper')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: N=1 lower
  n = 1
  nap = 1
  ap(1) = (9.0d0, 0.0d0)
  call zhptrf('L', n, ap, ipiv, info)
  call zhptri('L', n, ap, ipiv, work, info)
  call begin_test('n1_lower')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 4: 3x3 upper Hermitian positive definite
  ! A = [ 4       1+2i    3-i  ]
  !     [ 1-2i    5       2+i  ]
  !     [ 3+i     2-i     7    ]
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  n = 3
  nap = 6
  ap(1) = (4.0d0, 0.0d0)
  ap(2) = (1.0d0, 2.0d0)
  ap(3) = (5.0d0, 0.0d0)
  ap(4) = (3.0d0, -1.0d0)
  ap(5) = (2.0d0, 1.0d0)
  ap(6) = (7.0d0, 0.0d0)
  ipiv = 0
  call zhptrf('U', n, ap, ipiv, info)
  call zhptri('U', n, ap, ipiv, work, info)
  call begin_test('3x3_upper')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 5: 3x3 lower Hermitian positive definite
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  n = 3
  nap = 6
  ap(1) = (4.0d0, 0.0d0)
  ap(2) = (1.0d0, -2.0d0)
  ap(3) = (3.0d0, 1.0d0)
  ap(4) = (5.0d0, 0.0d0)
  ap(5) = (2.0d0, -1.0d0)
  ap(6) = (7.0d0, 0.0d0)
  ipiv = 0
  call zhptrf('L', n, ap, ipiv, info)
  call zhptri('L', n, ap, ipiv, work, info)
  call begin_test('3x3_lower')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 6: 4x4 upper Hermitian indefinite (forces 2x2 pivots)
  ! A = [ 0       1+i     2-i     3+0.5i ]
  !     [ 1-i     0       4+2i    5-i    ]
  !     [ 2+i     4-2i    0       6+i    ]
  !     [ 3-0.5i  5+i     6-i     0      ]
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  n = 4
  nap = 10
  ap(1)  = (0.0d0, 0.0d0)
  ap(2)  = (1.0d0, 1.0d0)
  ap(3)  = (0.0d0, 0.0d0)
  ap(4)  = (2.0d0, -1.0d0)
  ap(5)  = (4.0d0, 2.0d0)
  ap(6)  = (0.0d0, 0.0d0)
  ap(7)  = (3.0d0, 0.5d0)
  ap(8)  = (5.0d0, -1.0d0)
  ap(9)  = (6.0d0, 1.0d0)
  ap(10) = (0.0d0, 0.0d0)
  ipiv = 0
  call zhptrf('U', n, ap, ipiv, info)
  call zhptri('U', n, ap, ipiv, work, info)
  call begin_test('4x4_upper_indef')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 lower Hermitian indefinite
  ! Same matrix in lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  n = 4
  nap = 10
  ap(1)  = (0.0d0, 0.0d0)    ! A(1,1)
  ap(2)  = (1.0d0, -1.0d0)   ! A(2,1)
  ap(3)  = (2.0d0, 1.0d0)    ! A(3,1)
  ap(4)  = (3.0d0, -0.5d0)   ! A(4,1)
  ap(5)  = (0.0d0, 0.0d0)    ! A(2,2)
  ap(6)  = (4.0d0, -2.0d0)   ! A(3,2)
  ap(7)  = (5.0d0, 1.0d0)    ! A(4,2)
  ap(8)  = (0.0d0, 0.0d0)    ! A(3,3)
  ap(9)  = (6.0d0, -1.0d0)   ! A(4,3)
  ap(10) = (0.0d0, 0.0d0)    ! A(4,4)
  ipiv = 0
  call zhptrf('L', n, ap, ipiv, info)
  call zhptri('L', n, ap, ipiv, work, info)
  call begin_test('4x4_lower_indef')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 8: Singular lower matrix (INFO > 0)
  n = 2
  nap = 3
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (1.0d0, 0.0d0)
  ap(3) = (1.0d0, 0.0d0)
  ipiv = 0
  call zhptrf('L', n, ap, ipiv, info)
  call zhptri('L', n, ap, ipiv, work, info)
  call begin_test('singular_lower')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 upper with interchange (small diagonal, large off-diagonal)
  ! A(1,1) = 0.1, A(1,3) = 10+i, forces pivot swap
  n = 4
  nap = 10
  ap(1)  = (0.1d0, 0.0d0)     ! A(1,1)
  ap(2)  = (0.2d0, 0.3d0)     ! A(1,2)
  ap(3)  = (5.0d0, 0.0d0)     ! A(2,2)
  ap(4)  = (10.0d0, 1.0d0)    ! A(1,3)
  ap(5)  = (0.5d0, -0.5d0)    ! A(2,3)
  ap(6)  = (3.0d0, 0.0d0)     ! A(3,3)
  ap(7)  = (0.1d0, 0.2d0)     ! A(1,4)
  ap(8)  = (0.3d0, -0.1d0)    ! A(2,4)
  ap(9)  = (0.4d0, 0.6d0)     ! A(3,4)
  ap(10) = (4.0d0, 0.0d0)     ! A(4,4)
  ipiv = 0
  call zhptrf('U', n, ap, ipiv, info)
  call zhptri('U', n, ap, ipiv, work, info)
  call begin_test('4x4_upper_swap')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 10: 4x4 lower with interchange (small diagonal, large off-diagonal)
  ! Same matrix in lower packed
  n = 4
  nap = 10
  ap(1)  = (0.1d0, 0.0d0)     ! A(1,1)
  ap(2)  = (0.2d0, -0.3d0)    ! A(2,1) = conj(A(1,2))
  ap(3)  = (10.0d0, -1.0d0)   ! A(3,1) = conj(A(1,3))
  ap(4)  = (0.1d0, -0.2d0)    ! A(4,1) = conj(A(1,4))
  ap(5)  = (5.0d0, 0.0d0)     ! A(2,2)
  ap(6)  = (0.5d0, 0.5d0)     ! A(3,2) = conj(A(2,3))
  ap(7)  = (0.3d0, 0.1d0)     ! A(4,2) = conj(A(2,4))
  ap(8)  = (3.0d0, 0.0d0)     ! A(3,3)
  ap(9)  = (0.4d0, -0.6d0)    ! A(4,3) = conj(A(3,4))
  ap(10) = (4.0d0, 0.0d0)     ! A(4,4)
  ipiv = 0
  call zhptrf('L', n, ap, ipiv, info)
  call zhptri('L', n, ap, ipiv, work, info)
  call begin_test('4x4_lower_swap')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 11: Singular upper matrix (INFO > 0)
  ! A = [ 1       1+0i ]
  !     [ 1-0i    1    ]
  ! This is rank-1, so D(2,2) = 0 after factorization
  n = 2
  nap = 3
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (1.0d0, 0.0d0)
  ap(3) = (1.0d0, 0.0d0)
  ipiv = 0
  call zhptrf('U', n, ap, ipiv, info)
  call zhptri('U', n, ap, ipiv, work, info)
  call begin_test('singular_upper')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

end program
