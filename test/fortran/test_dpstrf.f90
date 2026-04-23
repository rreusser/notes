program test_dpstrf
  use test_utils
  implicit none

  double precision :: a(16), work(8)
  integer :: piv(4), rank, info

  ! Large test arrays for blocked path (N=80 > NB=64)
  integer, parameter :: NBIG = 80
  double precision :: abig(NBIG*NBIG), workbig(2*NBIG)
  integer :: pivbig(NBIG), i, j

  ! Test 1: upper, 3x3 positive definite
  ! A = [4 2 1; 2 5 3; 1 3 6] (symmetric, column-major)
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  work = 0.0d0
  call dpstrf('U', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 9)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 2: lower, 3x3 positive definite
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  work = 0.0d0
  call dpstrf('L', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 9)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 3: upper, 4x4 positive definite
  ! A = [10 3 2 1; 3 8 4 2; 2 4 9 5; 1 2 5 7]
  a = 0.0d0
  a(1)  = 10.0d0; a(2)  = 3.0d0;  a(3)  = 2.0d0;  a(4)  = 1.0d0
  a(5)  = 3.0d0;  a(6)  = 8.0d0;  a(7)  = 4.0d0;  a(8)  = 2.0d0
  a(9)  = 2.0d0;  a(10) = 4.0d0;  a(11) = 9.0d0;  a(12) = 5.0d0
  a(13) = 1.0d0;  a(14) = 2.0d0;  a(15) = 5.0d0;  a(16) = 7.0d0
  work = 0.0d0
  call dpstrf('U', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 16)
  call print_int_array('piv', piv, 4)
  call end_test()

  ! Test 4: lower, 4x4 positive definite
  a = 0.0d0
  a(1)  = 10.0d0; a(2)  = 3.0d0;  a(3)  = 2.0d0;  a(4)  = 1.0d0
  a(5)  = 3.0d0;  a(6)  = 8.0d0;  a(7)  = 4.0d0;  a(8)  = 2.0d0
  a(9)  = 2.0d0;  a(10) = 4.0d0;  a(11) = 9.0d0;  a(12) = 5.0d0
  a(13) = 1.0d0;  a(14) = 2.0d0;  a(15) = 5.0d0;  a(16) = 7.0d0
  work = 0.0d0
  call dpstrf('L', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 16)
  call print_int_array('piv', piv, 4)
  call end_test()

  ! Test 5: rank-deficient 3x3, upper
  ! A = [1 1 1; 1 1 1; 1 1 1] — rank 1
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 1.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 1.0d0
  work = 0.0d0
  call dpstrf('U', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_upper')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 9)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 6: rank-deficient 3x3, lower
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 1.0d0; a(3) = 1.0d0
  a(4) = 1.0d0; a(5) = 1.0d0; a(6) = 1.0d0
  a(7) = 1.0d0; a(8) = 1.0d0; a(9) = 1.0d0
  work = 0.0d0
  call dpstrf('L', 3, a, 3, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_lower')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 9)
  call print_int_array('piv', piv, 3)
  call end_test()

  ! Test 7: N=0
  call dpstrf('U', 0, a, 1, piv, rank, -1.0d0, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1
  a(1) = 9.0d0
  work = 0.0d0
  call dpstrf('U', 1, a, 1, piv, rank, -1.0d0, work, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 1)
  call print_int_array('piv', piv, 1)
  call end_test()

  ! Test 9: rank-deficient 4x4 (rank 2), upper
  ! A = v1*v1^T + v2*v2^T where v1=[1,2,3,4], v2=[1,-1,1,-1]
  a = 0.0d0
  a(1)  = 2.0d0;  a(2)  = 1.0d0;  a(3)  = 4.0d0;  a(4)  = 3.0d0
  a(5)  = 1.0d0;  a(6)  = 5.0d0;  a(7)  = 5.0d0;  a(8)  = 7.0d0
  a(9)  = 4.0d0;  a(10) = 5.0d0;  a(11) = 10.0d0; a(12) = 11.0d0
  a(13) = 3.0d0;  a(14) = 7.0d0;  a(15) = 11.0d0; a(16) = 17.0d0
  work = 0.0d0
  call dpstrf('U', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_4x4_upper')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 16)
  call print_int_array('piv', piv, 4)
  call end_test()

  ! Test 10: rank-deficient 4x4 (rank 2), lower
  a = 0.0d0
  a(1)  = 2.0d0;  a(2)  = 1.0d0;  a(3)  = 4.0d0;  a(4)  = 3.0d0
  a(5)  = 1.0d0;  a(6)  = 5.0d0;  a(7)  = 5.0d0;  a(8)  = 7.0d0
  a(9)  = 4.0d0;  a(10) = 5.0d0;  a(11) = 10.0d0; a(12) = 11.0d0
  a(13) = 3.0d0;  a(14) = 7.0d0;  a(15) = 11.0d0; a(16) = 17.0d0
  work = 0.0d0
  call dpstrf('L', 4, a, 4, piv, rank, -1.0d0, work, info)
  call begin_test('rank_deficient_4x4_lower')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', a, 16)
  call print_int_array('piv', piv, 4)
  call end_test()

  ! Test 11: large N=80 upper (triggers blocked path, NB=64)
  ! Build SPD matrix: A = I + v*v^T where v = [1,2,...,N]
  abig = 0.0d0
  do i = 1, NBIG
    do j = 1, NBIG
      abig((j-1)*NBIG + i) = dble(i) * dble(j)
    end do
    abig((i-1)*NBIG + i) = abig((i-1)*NBIG + i) + dble(NBIG)
  end do
  workbig = 0.0d0
  call dpstrf('U', NBIG, abig, NBIG, pivbig, rank, -1.0d0, workbig, info)
  call begin_test('upper_large')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', abig, NBIG*NBIG)
  call print_int_array('piv', pivbig, NBIG)
  call end_test()

  ! Test 12: large N=80 lower (triggers blocked path)
  abig = 0.0d0
  do i = 1, NBIG
    do j = 1, NBIG
      abig((j-1)*NBIG + i) = dble(i) * dble(j)
    end do
    abig((i-1)*NBIG + i) = abig((i-1)*NBIG + i) + dble(NBIG)
  end do
  workbig = 0.0d0
  call dpstrf('L', NBIG, abig, NBIG, pivbig, rank, -1.0d0, workbig, info)
  call begin_test('lower_large')
  call print_int('info', info)
  call print_int('rank', rank)
  call print_array('a', abig, NBIG*NBIG)
  call print_int_array('piv', pivbig, NBIG)
  call end_test()

end program
