program test_dlag2s
  use test_utils
  implicit none
  double precision :: a(16)
  real :: sa(16)
  double precision :: sad(16)
  integer :: info, i

  ! Test 1: basic 3x3 convert
  a = 0.0d0
  sa = 0.0
  do i = 1, 9
    a(i) = dble(i) * 0.5d0
  end do
  info = 42
  call dlag2s(3, 3, a, 3, sa, 3, info)
  do i = 1, 9
    sad(i) = dble(sa(i))
  end do
  call begin_test('basic_3x3')
  call print_int('info', info)
  call print_array('sa', sad, 9)
  call end_test()

  ! Test 2: rectangular 2x4 (m < n)
  a = 0.0d0
  sa = 0.0
  do i = 1, 8
    a(i) = dble(i) - 4.5d0
  end do
  info = 42
  call dlag2s(2, 4, a, 2, sa, 2, info)
  do i = 1, 8
    sad(i) = dble(sa(i))
  end do
  call begin_test('rect_2x4')
  call print_int('info', info)
  call print_array('sa', sad, 8)
  call end_test()

  ! Test 3: rectangular 4x2 (m > n)
  a = 0.0d0
  sa = 0.0
  do i = 1, 8
    a(i) = dble(i) * 2.0d0
  end do
  info = 42
  call dlag2s(4, 2, a, 4, sa, 4, info)
  do i = 1, 8
    sad(i) = dble(sa(i))
  end do
  call begin_test('rect_4x2')
  call print_int('info', info)
  call print_array('sa', sad, 8)
  call end_test()

  ! Test 4: padded leading dimensions (LDA=4, LDSA=4, M=2, N=3)
  a = 0.0d0
  sa = 0.0
  ! col 1: rows 1..2
  a(1) = 1.25d0
  a(2) = 2.5d0
  ! col 2: rows 5..6 (padding at 3,4)
  a(5) = -3.75d0
  a(6) = 4.125d0
  ! col 3: rows 9..10
  a(9) = 5.0d0
  a(10) = -6.5d0
  info = 42
  call dlag2s(2, 3, a, 4, sa, 4, info)
  do i = 1, 12
    sad(i) = dble(sa(i))
  end do
  call begin_test('padded_lda')
  call print_int('info', info)
  call print_array('sa', sad, 12)
  call end_test()

  ! Test 5: M=0 quick-ish (loop skipped, INFO=0)
  a = 0.0d0
  sa = 0.0
  sa(1) = 99.0
  info = 42
  call dlag2s(0, 3, a, 1, sa, 1, info)
  sad(1) = dble(sa(1))
  call begin_test('m_zero')
  call print_int('info', info)
  call print_array('sa', sad, 1)
  call end_test()

  ! Test 6: N=0
  a = 0.0d0
  sa = 0.0
  sa(1) = 77.0
  info = 42
  call dlag2s(3, 0, a, 3, sa, 3, info)
  sad(1) = dble(sa(1))
  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('sa', sad, 1)
  call end_test()

  ! Test 7: overflow detected — element exceeds single precision range
  a = 0.0d0
  sa = 0.0
  a(1) = 1.0d0
  a(2) = 2.0d0
  a(3) = 1.0d40     ! > REAL overflow ~3.4e38
  a(4) = 4.0d0
  info = 42
  call dlag2s(2, 2, a, 2, sa, 2, info)
  do i = 1, 4
    sad(i) = dble(sa(i))
  end do
  call begin_test('overflow_pos')
  call print_int('info', info)
  ! SA values past the overflow point are unspecified (loop breaks)
  ! We only record info
  call end_test()

  ! Test 8: negative overflow (-inf direction)
  a = 0.0d0
  sa = 0.0
  a(1) = 1.0d0
  a(2) = -1.0d40
  a(3) = 3.0d0
  a(4) = 4.0d0
  info = 42
  call dlag2s(2, 2, a, 2, sa, 2, info)
  call begin_test('overflow_neg')
  call print_int('info', info)
  call end_test()

  ! Test 9: 1x1 normal
  a = 0.0d0
  sa = 0.0
  a(1) = 3.14159265358979d0
  info = 42
  call dlag2s(1, 1, a, 1, sa, 1, info)
  sad(1) = dble(sa(1))
  call begin_test('scalar_1x1')
  call print_int('info', info)
  call print_array('sa', sad, 1)
  call end_test()

end program
