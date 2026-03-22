program test_zlacpy
  use test_utils
  implicit none

  complex*16 :: A(5,5), B(5,5)
  double precision :: A_r(50), B_r(50)
  equivalence (A, A_r)
  equivalence (B, B_r)
  integer :: i, j, m, n

  ! ===== Test 1: Full copy 3x3 =====
  m = 3
  n = 3
  do j = 1, n
    do i = 1, m
      A(i,j) = dcmplx(dble(i + j*10), dble(i*2 + j*3))
    end do
  end do
  B = (0.0d0, 0.0d0)

  call ZLACPY('A', m, n, A, 5, B, 5)

  call begin_test('full_copy_3x3')
  call print_int('M', m)
  call print_int('N', n)
  call print_array('A', A_r, 2*5*n)
  call print_array('B', B_r, 2*5*n)
  call end_test()

  ! ===== Test 2: Upper triangle 3x3 =====
  B = (0.0d0, 0.0d0)
  call ZLACPY('U', m, n, A, 5, B, 5)

  call begin_test('upper_copy_3x3')
  call print_int('M', m)
  call print_int('N', n)
  call print_array('A', A_r, 2*5*n)
  call print_array('B', B_r, 2*5*n)
  call end_test()

  ! ===== Test 3: Lower triangle 3x3 =====
  B = (0.0d0, 0.0d0)
  call ZLACPY('L', m, n, A, 5, B, 5)

  call begin_test('lower_copy_3x3')
  call print_int('M', m)
  call print_int('N', n)
  call print_array('A', A_r, 2*5*n)
  call print_array('B', B_r, 2*5*n)
  call end_test()

  ! ===== Test 4: M=0 (quick return) =====
  B = (0.0d0, 0.0d0)
  call ZLACPY('A', 0, 3, A, 5, B, 5)

  call begin_test('m_zero')
  call print_int('M', 0)
  call print_int('N', 3)
  call print_array('B', B_r, 2*5*3)
  call end_test()

  ! ===== Test 5: N=0 (quick return) =====
  B = (0.0d0, 0.0d0)
  call ZLACPY('A', 3, 0, A, 5, B, 5)

  call begin_test('n_zero')
  call print_int('M', 3)
  call print_int('N', 0)
  call print_array('B', B_r, 2*5*0)
  call end_test()

  ! ===== Test 6: Non-square 2x4 full copy =====
  m = 2
  n = 4
  do j = 1, n
    do i = 1, m
      A(i,j) = dcmplx(dble(i*3 + j), dble(i - j*2))
    end do
  end do
  B = (0.0d0, 0.0d0)
  call ZLACPY('A', m, n, A, 5, B, 5)

  call begin_test('full_copy_2x4')
  call print_int('M', m)
  call print_int('N', n)
  call print_array('A', A_r, 2*5*n)
  call print_array('B', B_r, 2*5*n)
  call end_test()

  ! ===== Test 7: Non-square 4x2 upper copy =====
  m = 4
  n = 2
  do j = 1, n
    do i = 1, m
      A(i,j) = dcmplx(dble(i + j*5), dble(i*3 - j))
    end do
  end do
  B = (0.0d0, 0.0d0)
  call ZLACPY('U', m, n, A, 5, B, 5)

  call begin_test('upper_copy_4x2')
  call print_int('M', m)
  call print_int('N', n)
  call print_array('A', A_r, 2*5*n)
  call print_array('B', B_r, 2*5*n)
  call end_test()

  ! ===== Test 8: Non-square 4x2 lower copy =====
  B = (0.0d0, 0.0d0)
  call ZLACPY('L', m, n, A, 5, B, 5)

  call begin_test('lower_copy_4x2')
  call print_int('M', m)
  call print_int('N', n)
  call print_array('A', A_r, 2*5*n)
  call print_array('B', B_r, 2*5*n)
  call end_test()

end program
