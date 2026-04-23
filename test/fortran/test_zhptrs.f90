program test_zhptrs
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  integer, parameter :: APMAX = NMAX*(NMAX+1)/2
  complex*16 :: AP(APMAX), B(NMAX, 3)
  double precision :: AP_r(2*APMAX), B_r(2*NMAX*3)
  equivalence (AP, AP_r)
  equivalence (B, B_r)
  integer :: IPIV(NMAX), INFO, n, nrhs, nn

  ! Test 1: Upper, 3x3, 1 RHS - Hermitian packed
  n = 3
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Upper packed: (1,1), (1,2), (2,2), (1,3), (2,3), (3,3)
  AP(1) = (4.0d0, 0.0d0)    ! A(1,1)
  AP(2) = (1.0d0, 2.0d0)    ! A(1,2)
  AP(3) = (5.0d0, 0.0d0)    ! A(2,2)
  AP(4) = (2.0d0, -1.0d0)   ! A(1,3)
  AP(5) = (3.0d0, 1.0d0)    ! A(2,3)
  AP(6) = (6.0d0, 0.0d0)    ! A(3,3)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (1.0d0, -1.0d0)
  call begin_test('upper_3x3_1rhs')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_array('b', B_r, 2*n*nrhs)
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPTRS('U', n, nrhs, AP, IPIV, B, NMAX, INFO)
  call print_int('info', INFO)
  call print_array('x', B_r, 2*n*nrhs)
  call end_test()

  ! Test 2: Lower, 3x3, 2 RHS - Hermitian packed
  n = 3
  nrhs = 2
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Lower packed: (1,1), (2,1), (3,1), (2,2), (3,2), (3,3)
  AP(1) = (4.0d0, 0.0d0)    ! A(1,1)
  AP(2) = (1.0d0, -2.0d0)   ! A(2,1)
  AP(3) = (2.0d0, 1.0d0)    ! A(3,1)
  AP(4) = (5.0d0, 0.0d0)    ! A(2,2)
  AP(5) = (3.0d0, -1.0d0)   ! A(3,2)
  AP(6) = (6.0d0, 0.0d0)    ! A(3,3)
  B(1,1) = (1.0d0, 0.5d0)
  B(2,1) = (2.0d0, -1.0d0)
  B(3,1) = (0.0d0, 1.0d0)
  B(1,2) = (0.5d0, 0.0d0)
  B(2,2) = (-1.0d0, 2.0d0)
  B(3,2) = (1.0d0, -0.5d0)
  call begin_test('lower_3x3_2rhs')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_array('b', B_r, 2*NMAX*nrhs)
  call ZHPTRF('L', n, AP, IPIV, INFO)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPTRS('L', n, nrhs, AP, IPIV, B, NMAX, INFO)
  call print_int('info', INFO)
  call print_array('x', B_r, 2*NMAX*nrhs)
  call end_test()

  ! Test 3: N=1
  n = 1
  nrhs = 1
  AP(1) = (3.0d0, 0.0d0)
  B(1,1) = (1.0d0, 1.0d0)
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call begin_test('n1')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2)
  call print_int_array('ipiv', IPIV, n)
  call print_array('b', B_r, 2*n*nrhs)
  call ZHPTRS('U', n, nrhs, AP, IPIV, B, NMAX, INFO)
  call print_int('info', INFO)
  call print_array('x', B_r, 2*n*nrhs)
  call end_test()

  ! Test 4: N=0
  call ZHPTRS('U', 0, 1, AP, IPIV, B, NMAX, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 5: Upper, 4x4, 1 RHS with forced 2x2 pivots (tiny diagonal)
  n = 4
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3),(1,4),(2,4),(3,4),(4,4)
  AP(1)  = (5.0d0, 0.0d0)   ! A(1,1)
  AP(2)  = (1.0d0, 0.5d0)   ! A(1,2)
  AP(3)  = (6.0d0, 0.0d0)   ! A(2,2)
  AP(4)  = (2.0d0, -1.0d0)  ! A(1,3)
  AP(5)  = (1.0d0, 1.0d0)   ! A(2,3)
  AP(6)  = (0.01d0, 0.0d0)  ! A(3,3) - tiny, forces 2x2 pivot
  AP(7)  = (0.5d0, 0.5d0)   ! A(1,4)
  AP(8)  = (3.0d0, -2.0d0)  ! A(2,4)
  AP(9)  = (4.0d0, 1.0d0)   ! A(3,4) - large off-diag
  AP(10) = (0.02d0, 0.0d0)  ! A(4,4) - tiny
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (2.0d0, -1.0d0)
  B(4,1) = (1.0d0, 1.0d0)
  call begin_test('upper_4x4_pivot')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_array('b', B_r, 2*n*nrhs)
  call ZHPTRF('U', n, AP, IPIV, INFO)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPTRS('U', n, nrhs, AP, IPIV, B, NMAX, INFO)
  call print_int('info', INFO)
  call print_array('x', B_r, 2*n*nrhs)
  call end_test()

  ! Test 6: Lower, 4x4, 2 RHS with forced 2x2 pivots
  n = 4
  nrhs = 2
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Lower packed: (1,1),(2,1),(3,1),(4,1),(2,2),(3,2),(4,2),(3,3),(4,3),(4,4)
  AP(1)  = (0.01d0, 0.0d0)  ! A(1,1) - tiny
  AP(2)  = (5.0d0, -1.0d0)  ! A(2,1) - large off-diag
  AP(3)  = (1.0d0, 0.5d0)   ! A(3,1)
  AP(4)  = (0.5d0, -0.5d0)  ! A(4,1)
  AP(5)  = (0.02d0, 0.0d0)  ! A(2,2) - tiny
  AP(6)  = (2.0d0, -1.0d0)  ! A(3,2)
  AP(7)  = (1.0d0, 1.0d0)   ! A(4,2)
  AP(8)  = (8.0d0, 0.0d0)   ! A(3,3)
  AP(9)  = (3.0d0, 0.0d0)   ! A(4,3)
  AP(10) = (7.0d0, 0.0d0)   ! A(4,4)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (2.0d0, -1.0d0)
  B(4,1) = (1.0d0, 1.0d0)
  B(1,2) = (0.5d0, -0.5d0)
  B(2,2) = (1.0d0, 1.0d0)
  B(3,2) = (-1.0d0, 0.0d0)
  B(4,2) = (0.0d0, 2.0d0)
  call begin_test('lower_4x4_pivot')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_array('b', B_r, 2*NMAX*nrhs)
  call ZHPTRF('L', n, AP, IPIV, INFO)
  call print_int('info_trf', INFO)
  call print_array('AP_factored', AP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call ZHPTRS('L', n, nrhs, AP, IPIV, B, NMAX, INFO)
  call print_int('info', INFO)
  call print_array('x', B_r, 2*NMAX*nrhs)
  call end_test()

end program
