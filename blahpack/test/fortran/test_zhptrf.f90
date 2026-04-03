program test_zhptrf
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  complex*16 :: ap(NMAX*(NMAX+1)/2)
  double precision :: ap_r(2*NMAX*(NMAX+1)/2)
  equivalence (ap, ap_r)
  integer :: ipiv(NMAX), info, nap

  ! Test 1: 3x3 upper Hermitian positive definite
  ! A = [ 4       1+2i    3-i  ]
  !     [ 1-2i    5       2+i  ]
  !     [ 3+i     2-i     7    ]
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  nap = 6
  ap(1) = (4.0d0, 0.0d0)   ! A(1,1)
  ap(2) = (1.0d0, 2.0d0)   ! A(1,2)
  ap(3) = (5.0d0, 0.0d0)   ! A(2,2)
  ap(4) = (3.0d0, -1.0d0)  ! A(1,3)
  ap(5) = (2.0d0, 1.0d0)   ! A(2,3)
  ap(6) = (7.0d0, 0.0d0)   ! A(3,3)
  ipiv = 0
  call zhptrf('U', 3, ap, ipiv, info)
  call begin_test('3x3_upper_hpd')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower Hermitian positive definite
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  nap = 6
  ap(1) = (4.0d0, 0.0d0)   ! A(1,1)
  ap(2) = (1.0d0, -2.0d0)  ! A(2,1) = conj(A(1,2))
  ap(3) = (3.0d0, 1.0d0)   ! A(3,1) = conj(A(1,3))
  ap(4) = (5.0d0, 0.0d0)   ! A(2,2)
  ap(5) = (2.0d0, -1.0d0)  ! A(3,2) = conj(A(2,3))
  ap(6) = (7.0d0, 0.0d0)   ! A(3,3)
  ipiv = 0
  call zhptrf('L', 3, ap, ipiv, info)
  call begin_test('3x3_lower_hpd')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 upper Hermitian indefinite (forces 2x2 pivots)
  ! A = [ 0       1+i     2-i     3+0.5i ]
  !     [ 1-i     0       4+2i    5-i    ]
  !     [ 2+i     4-2i    0       6+i    ]
  !     [ 3-0.5i  5+i     6-i     0      ]
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  nap = 10
  ap(1)  = (0.0d0, 0.0d0)    ! A(1,1)
  ap(2)  = (1.0d0, 1.0d0)    ! A(1,2)
  ap(3)  = (0.0d0, 0.0d0)    ! A(2,2)
  ap(4)  = (2.0d0, -1.0d0)   ! A(1,3)
  ap(5)  = (4.0d0, 2.0d0)    ! A(2,3)
  ap(6)  = (0.0d0, 0.0d0)    ! A(3,3)
  ap(7)  = (3.0d0, 0.5d0)    ! A(1,4)
  ap(8)  = (5.0d0, -1.0d0)   ! A(2,4)
  ap(9)  = (6.0d0, 1.0d0)    ! A(3,4)
  ap(10) = (0.0d0, 0.0d0)    ! A(4,4)
  ipiv = 0
  call zhptrf('U', 4, ap, ipiv, info)
  call begin_test('4x4_indef_upper')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 lower Hermitian indefinite
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  nap = 10
  ap(1)  = (0.0d0, 0.0d0)    ! A(1,1)
  ap(2)  = (1.0d0, -1.0d0)   ! A(2,1) = conj(A(1,2))
  ap(3)  = (2.0d0, 1.0d0)    ! A(3,1) = conj(A(1,3))
  ap(4)  = (3.0d0, -0.5d0)   ! A(4,1) = conj(A(1,4))
  ap(5)  = (0.0d0, 0.0d0)    ! A(2,2)
  ap(6)  = (4.0d0, -2.0d0)   ! A(3,2) = conj(A(2,3))
  ap(7)  = (5.0d0, 1.0d0)    ! A(4,2) = conj(A(2,4))
  ap(8)  = (0.0d0, 0.0d0)    ! A(3,3)
  ap(9)  = (6.0d0, -1.0d0)   ! A(4,3) = conj(A(3,4))
  ap(10) = (0.0d0, 0.0d0)    ! A(4,4)
  ipiv = 0
  call zhptrf('L', 4, ap, ipiv, info)
  call begin_test('4x4_indef_lower')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call zhptrf('U', 0, ap, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  ap(1) = (5.0d0, 0.0d0)
  ipiv = 0
  call zhptrf('L', 1, ap, ipiv, info)
  call begin_test('n_one')
  call print_array('ap', ap_r, 2)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: singular matrix (INFO>0)
  ap(1) = (0.0d0, 0.0d0)
  ap(2) = (0.0d0, 0.0d0)
  ap(3) = (0.0d0, 0.0d0)
  ipiv = 0
  call zhptrf('L', 2, ap, ipiv, info)
  call begin_test('singular')
  call print_array('ap', ap_r, 6)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 Hermitian tridiagonal, lower
  ! A = [ 2       -1+i    0       0     ]
  !     [-1-i      3      -1+2i   0     ]
  !     [ 0       -1-2i    4      -1+i  ]
  !     [ 0        0      -1-i    3     ]
  ! Lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  nap = 10
  ap(1)  = (2.0d0, 0.0d0)     ! A(1,1)
  ap(2)  = (-1.0d0, -1.0d0)   ! A(2,1) = conj(A(1,2))
  ap(3)  = (0.0d0, 0.0d0)     ! A(3,1)
  ap(4)  = (0.0d0, 0.0d0)     ! A(4,1)
  ap(5)  = (3.0d0, 0.0d0)     ! A(2,2)
  ap(6)  = (-1.0d0, -2.0d0)   ! A(3,2) = conj(A(2,3))
  ap(7)  = (0.0d0, 0.0d0)     ! A(4,2)
  ap(8)  = (4.0d0, 0.0d0)     ! A(3,3)
  ap(9)  = (-1.0d0, -1.0d0)   ! A(4,3) = conj(A(3,4))
  ap(10) = (3.0d0, 0.0d0)     ! A(4,4)
  ipiv = 0
  call zhptrf('L', 4, ap, ipiv, info)
  call begin_test('4x4_tridiag_lower')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 Hermitian tridiagonal, upper
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  nap = 10
  ap(1)  = (2.0d0, 0.0d0)      ! A(1,1)
  ap(2)  = (-1.0d0, 1.0d0)     ! A(1,2)
  ap(3)  = (3.0d0, 0.0d0)      ! A(2,2)
  ap(4)  = (0.0d0, 0.0d0)      ! A(1,3)
  ap(5)  = (-1.0d0, 2.0d0)     ! A(2,3)
  ap(6)  = (4.0d0, 0.0d0)      ! A(3,3)
  ap(7)  = (0.0d0, 0.0d0)      ! A(1,4)
  ap(8)  = (0.0d0, 0.0d0)      ! A(2,4)
  ap(9)  = (-1.0d0, 1.0d0)     ! A(3,4)
  ap(10) = (3.0d0, 0.0d0)      ! A(4,4)
  ipiv = 0
  call zhptrf('U', 4, ap, ipiv, info)
  call begin_test('4x4_tridiag_upper')
  call print_array('ap', ap_r, 2*nap)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1 singular
  ap(1) = (0.0d0, 0.0d0)
  ipiv = 0
  call zhptrf('U', 1, ap, ipiv, info)
  call begin_test('n_one_singular')
  call print_array('ap', ap_r, 2)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

end program
