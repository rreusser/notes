program test_dlascl
  use test_utils
  implicit none
  double precision :: a(16), ab(12)
  integer :: info, i

  ! Test 1: basic scaling of general matrix, cfrom=1, cto=2 (multiply by 2)
  ! A = [1 4; 2 5; 3 6] column-major, M=3, N=2, LDA=3
  a = 0.0d0
  do i = 1, 6
    a(i) = dble(i)
  end do
  call dlascl('G', 0, 0, 1.0d0, 2.0d0, 3, 2, a, 3, info)
  call begin_test('general_basic')
  call print_array('a', a, 6)
  call print_int('info', info)
  call end_test()

  ! Test 2: scaling by 0.5 (cfrom=2, cto=1)
  a = 0.0d0
  a(1) = 10.0d0
  a(2) = 20.0d0
  a(3) = 30.0d0
  a(4) = 40.0d0
  call dlascl('G', 0, 0, 2.0d0, 1.0d0, 2, 2, a, 2, info)
  call begin_test('general_half')
  call print_array('a', a, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: M=0 quick return
  a = 0.0d0
  a(1) = 99.0d0
  call dlascl('G', 0, 0, 1.0d0, 2.0d0, 0, 2, a, 1, info)
  call begin_test('m_zero')
  call print_array('a', a, 1)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  a = 0.0d0
  a(1) = 99.0d0
  call dlascl('G', 0, 0, 1.0d0, 2.0d0, 2, 0, a, 2, info)
  call begin_test('n_zero')
  call print_array('a', a, 1)
  call print_int('info', info)
  call end_test()

  ! Test 5: lower triangular
  ! A = [1 0 0; 2 4 0; 3 5 6] column-major, M=3, N=3, LDA=3
  a = 0.0d0
  a(1) = 1.0d0
  a(2) = 2.0d0
  a(3) = 3.0d0
  a(5) = 4.0d0
  a(6) = 5.0d0
  a(9) = 6.0d0
  call dlascl('L', 0, 0, 1.0d0, 3.0d0, 3, 3, a, 3, info)
  call begin_test('lower_tri')
  call print_array('a', a, 9)
  call print_int('info', info)
  call end_test()

  ! Test 6: upper triangular
  ! A = [1 2 3; 0 4 5; 0 0 6] column-major, M=3, N=3, LDA=3
  a = 0.0d0
  a(1) = 1.0d0
  a(4) = 2.0d0
  a(5) = 4.0d0
  a(7) = 3.0d0
  a(8) = 5.0d0
  a(9) = 6.0d0
  call dlascl('U', 0, 0, 1.0d0, 3.0d0, 3, 3, a, 3, info)
  call begin_test('upper_tri')
  call print_array('a', a, 9)
  call print_int('info', info)
  call end_test()

  ! Test 7: upper Hessenberg
  ! Hessenberg includes one sub-diagonal
  ! A = [1 3 6; 2 4 7; 0 5 8] column-major, M=3, N=3, LDA=3
  a = 0.0d0
  a(1) = 1.0d0
  a(2) = 2.0d0
  a(4) = 3.0d0
  a(5) = 4.0d0
  a(6) = 5.0d0
  a(7) = 6.0d0
  a(8) = 7.0d0
  a(9) = 8.0d0
  call dlascl('H', 0, 0, 1.0d0, 2.0d0, 3, 3, a, 3, info)
  call begin_test('hessenberg')
  call print_array('a', a, 9)
  call print_int('info', info)
  call end_test()

  ! Test 8: cfrom=cto (identity, MUL=1 quick return)
  a = 0.0d0
  a(1) = 1.0d0
  a(2) = 2.0d0
  a(3) = 3.0d0
  a(4) = 4.0d0
  call dlascl('G', 0, 0, 5.0d0, 5.0d0, 2, 2, a, 2, info)
  call begin_test('identity')
  call print_array('a', a, 4)
  call print_int('info', info)
  call end_test()

  ! Test 9: large cfrom/cto ratio - triggers iterative scaling
  ! cfrom=1e300, cto=1e-300
  a = 0.0d0
  a(1) = 1.0d0
  a(2) = 2.0d0
  call dlascl('G', 0, 0, 1.0d300, 1.0d-300, 2, 1, a, 2, info)
  call begin_test('large_ratio')
  call print_array('a', a, 2)
  call print_int('info', info)
  call end_test()

  ! Test 10: large cto/cfrom ratio - triggers mul=bignum branch
  ! cfrom=1e-150, cto=1e150
  a = 0.0d0
  a(1) = 1.0d-150
  a(2) = 2.0d-150
  call dlascl('G', 0, 0, 1.0d-150, 1.0d150, 2, 1, a, 2, info)
  call begin_test('large_ratio_inv')
  call print_array('a', a, 2)
  call print_int('info', info)
  call end_test()

  ! Test 11: lower band matrix (type='B'), kl=ku=1, M=N=4
  ! Band storage: kl+1=2 rows by N=4 columns, LDA=2
  ab = 0.0d0
  ab(1) = 1.0d0   ! diag(1)
  ab(2) = 4.0d0   ! sub(1)
  ab(3) = 2.0d0   ! diag(2)
  ab(4) = 5.0d0   ! sub(2)
  ab(5) = 3.0d0   ! diag(3)
  ab(6) = 6.0d0   ! sub(3)
  ab(7) = 7.0d0   ! diag(4) -- but k4-j=4+1-4=1, so only 1 element in col 4
  call dlascl('B', 1, 1, 1.0d0, 3.0d0, 4, 4, ab, 2, info)
  call begin_test('lower_band')
  call print_array('a', ab, 8)
  call print_int('info', info)
  call end_test()

  ! Test 12: upper band matrix (type='Q'), kl=ku=1, M=N=4
  ! Band storage: ku+1=2 rows by N=4 columns, LDA=2
  ab = 0.0d0
  ! Row 1: super-diagonal
  ab(1) = 0.0d0   ! unused for j=1
  ab(3) = 1.0d0   ! super(2)
  ab(5) = 2.0d0   ! super(3)
  ab(7) = 3.0d0   ! super(4)
  ! Row 2: diagonal
  ab(2) = 4.0d0   ! diag(1)
  ab(4) = 5.0d0   ! diag(2)
  ab(6) = 6.0d0   ! diag(3)
  ab(8) = 7.0d0   ! diag(4)
  call dlascl('Q', 1, 1, 1.0d0, 3.0d0, 4, 4, ab, 2, info)
  call begin_test('upper_band')
  call print_array('a', ab, 8)
  call print_int('info', info)
  call end_test()

  ! Test 13: full band matrix (type='Z'), kl=1, ku=1, M=N=3
  ! Band storage: 2*kl+ku+1=4 rows by N columns, LDA=4
  a = 0.0d0
  ! Column 1: rows kl+1 to 2*kl+ku+1 = rows 2..4
  a(2) = 3.0d0    ! super-diag (unused but in range)
  a(3) = 6.0d0    ! diagonal
  a(4) = 9.0d0    ! sub-diagonal
  ! Column 2: rows max(kl+ku+2-j,kl+1) to min(2*kl+ku+1, kl+ku+1+M-j)
  a(5) = 1.0d0    ! row 1: super-diag
  a(6) = 4.0d0    ! row 2: diagonal... wait
  a(7) = 7.0d0    ! row 3: sub-diag
  a(8) = 10.0d0   ! row 4: sub-sub... in range
  ! Column 3
  a(9) = 2.0d0
  a(10) = 5.0d0
  a(11) = 8.0d0
  call dlascl('Z', 1, 1, 1.0d0, 2.0d0, 3, 3, a, 4, info)
  call begin_test('full_band')
  call print_array('a', a, 12)
  call print_int('info', info)
  call end_test()

  ! Test 14: non-square general matrix (M=2, N=4)
  a = 0.0d0
  do i = 1, 8
    a(i) = dble(i)
  end do
  ! LDA=2
  call dlascl('G', 0, 0, 1.0d0, 10.0d0, 2, 4, a, 2, info)
  call begin_test('general_rect')
  call print_array('a', a, 8)
  call print_int('info', info)
  call end_test()

  ! Test 15: lower triangular with M > N (rectangular)
  a = 0.0d0
  ! 4x3 lower triangular, LDA=4
  a(1) = 1.0d0
  a(2) = 2.0d0
  a(3) = 3.0d0
  a(4) = 4.0d0
  a(6) = 5.0d0
  a(7) = 6.0d0
  a(8) = 7.0d0
  a(11) = 8.0d0
  a(12) = 9.0d0
  call dlascl('L', 0, 0, 1.0d0, 2.0d0, 4, 3, a, 4, info)
  call begin_test('lower_rect')
  call print_array('a', a, 12)
  call print_int('info', info)
  call end_test()

  ! Test 16: upper triangular with M < N (rectangular)
  a = 0.0d0
  ! 3x4 upper triangular, LDA=3
  a(1) = 1.0d0
  a(4) = 2.0d0
  a(5) = 3.0d0
  a(7) = 4.0d0
  a(8) = 5.0d0
  a(9) = 6.0d0
  a(10) = 7.0d0
  a(11) = 8.0d0
  a(12) = 9.0d0
  call dlascl('U', 0, 0, 1.0d0, 2.0d0, 3, 4, a, 3, info)
  call begin_test('upper_rect')
  call print_array('a', a, 12)
  call print_int('info', info)
  call end_test()

end program
