program test_dlauum
  use test_utils
  implicit none
  double precision :: a(1225)
  integer :: info, i, j, n

  ! Test 1: upper, 4x4
  ! U = [2 1 3 2; 0 4 1 3; 0 0 5 1; 0 0 0 6]
  ! Compute U * U^T, store upper triangle in A
  a = 0.0d0
  a(1)  = 2.0d0; a(5)  = 1.0d0; a(9)  = 3.0d0; a(13) = 2.0d0
  a(6)  = 4.0d0; a(10) = 1.0d0; a(14) = 3.0d0
  a(11) = 5.0d0; a(15) = 1.0d0
  a(16) = 6.0d0
  call dlauum('U', 4, a, 4, info)
  call begin_test('upper_4')
  call print_int('info', info)
  call print_matrix('a', a, 4, 4, 4)
  call end_test()

  ! Test 2: lower, 4x4
  ! L = [2 0 0 0; 1 4 0 0; 3 1 5 0; 2 3 1 6]
  ! Compute L^T * L, store lower triangle in A
  a = 0.0d0
  a(1)  = 2.0d0; a(2)  = 1.0d0; a(3)  = 3.0d0; a(4)  = 2.0d0
  a(6)  = 4.0d0; a(7)  = 1.0d0; a(8)  = 3.0d0
  a(11) = 5.0d0; a(12) = 1.0d0
  a(16) = 6.0d0
  call dlauum('L', 4, a, 4, info)
  call begin_test('lower_4')
  call print_int('info', info)
  call print_matrix('a', a, 4, 4, 4)
  call end_test()

  ! Test 3: N=1 edge case (upper)
  a(1) = 3.0d0
  call dlauum('U', 1, a, 1, info)
  call begin_test('n1_upper')
  call print_int('info', info)
  call print_array('a', a, 1)
  call end_test()

  ! Test 4: N=1 edge case (lower)
  a(1) = 5.0d0
  call dlauum('L', 1, a, 1, info)
  call begin_test('n1_lower')
  call print_int('info', info)
  call print_array('a', a, 1)
  call end_test()

  ! Test 5: N=0 quick return
  call dlauum('U', 0, a, 1, info)
  call begin_test('n0')
  call print_int('info', info)
  call end_test()

  ! Test 6: upper, 35x35 — exercises blocked code path (NB=32)
  ! Use a diagonally dominant upper triangular matrix
  n = 35
  a = 0.0d0
  do j = 1, n
    do i = 1, j
      if (i == j) then
        a((j-1)*n + i) = dble(n + 1)
      else
        a((j-1)*n + i) = 1.0d0 / dble(j - i + 1)
      end if
    end do
  end do
  call dlauum('U', n, a, n, info)
  call begin_test('upper_35')
  call print_int('info', info)
  call print_matrix('a', a, n, n, n)
  call end_test()

  ! Test 7: lower, 35x35 — exercises blocked code path (NB=32)
  ! Use a diagonally dominant lower triangular matrix
  a = 0.0d0
  do j = 1, n
    do i = j, n
      if (i == j) then
        a((j-1)*n + i) = dble(n + 1)
      else
        a((j-1)*n + i) = 1.0d0 / dble(i - j + 1)
      end if
    end do
  end do
  call dlauum('L', n, a, n, info)
  call begin_test('lower_35')
  call print_int('info', info)
  call print_matrix('a', a, n, n, n)
  call end_test()

end program
