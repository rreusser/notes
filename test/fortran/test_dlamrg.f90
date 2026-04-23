program test_dlamrg
  use test_utils
  implicit none

  integer, parameter :: NMAX = 20
  double precision :: A(NMAX)
  integer :: INDEX(NMAX)

  ! Test 1: Both forward, n1=3 n2=3
  A(1) = 1.0d0; A(2) = 3.0d0; A(3) = 5.0d0
  A(4) = 2.0d0; A(5) = 4.0d0; A(6) = 6.0d0
  INDEX = 0
  call DLAMRG(3, 3, A, 1, 1, INDEX)
  call begin_test('fwd_fwd_3_3')
  call print_int_array('index', INDEX, 6)
  call end_test()

  ! Test 2: First forward, second backward, n1=3 n2=3
  A(1) = 1.0d0; A(2) = 3.0d0; A(3) = 5.0d0
  A(4) = 6.0d0; A(5) = 4.0d0; A(6) = 2.0d0
  INDEX = 0
  call DLAMRG(3, 3, A, 1, -1, INDEX)
  call begin_test('fwd_bwd_3_3')
  call print_int_array('index', INDEX, 6)
  call end_test()

  ! Test 3: First backward, second forward, n1=3 n2=2
  A(1) = 5.0d0; A(2) = 3.0d0; A(3) = 1.0d0
  A(4) = 2.0d0; A(5) = 4.0d0
  INDEX = 0
  call DLAMRG(3, 2, A, -1, 1, INDEX)
  call begin_test('bwd_fwd_3_2')
  call print_int_array('index', INDEX, 5)
  call end_test()

  ! Test 4: Both backward, n1=2 n2=3
  A(1) = 4.0d0; A(2) = 2.0d0
  A(3) = 6.0d0; A(4) = 3.0d0; A(5) = 1.0d0
  INDEX = 0
  call DLAMRG(2, 3, A, -1, -1, INDEX)
  call begin_test('bwd_bwd_2_3')
  call print_int_array('index', INDEX, 5)
  call end_test()

  ! Test 5: n1=1 n2=1 forward
  A(1) = 3.0d0; A(2) = 1.0d0
  INDEX = 0
  call DLAMRG(1, 1, A, 1, 1, INDEX)
  call begin_test('n1_n1')
  call print_int_array('index', INDEX, 2)
  call end_test()

  ! Test 6: Equal elements
  A(1) = 2.0d0; A(2) = 2.0d0; A(3) = 2.0d0; A(4) = 2.0d0
  INDEX = 0
  call DLAMRG(2, 2, A, 1, 1, INDEX)
  call begin_test('equal')
  call print_int_array('index', INDEX, 4)
  call end_test()

  ! Test 7: One list empty-ish (n2=0 not valid in Fortran, so test n1=4, n2=1)
  A(1) = 1.0d0; A(2) = 3.0d0; A(3) = 5.0d0; A(4) = 7.0d0; A(5) = 4.0d0
  INDEX = 0
  call DLAMRG(4, 1, A, 1, 1, INDEX)
  call begin_test('n4_n1')
  call print_int_array('index', INDEX, 5)
  call end_test()

end program
