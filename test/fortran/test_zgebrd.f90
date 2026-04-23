program test_zgebrd
  use test_utils
  implicit none

  integer, parameter :: BIGM = 35, BIGN = 33
  double precision :: a_r(10000), tauq_r(200), taup_r(200), work_r(100000)
  complex*16 :: a(5000), tauq(100), taup(100), work(50000)
  double precision :: d(100), e(100)
  equivalence (a, a_r)
  equivalence (tauq, tauq_r)
  equivalence (taup, taup_r)
  equivalence (work, work_r)
  integer :: info, lwork, i, j

  lwork = 50000

  ! Test 1: 4x3 matrix (M > N, upper bidiagonal)
  a = (0.0d0, 0.0d0)
  a(1)  = (1.0d0, 2.0d0);  a(2)  = (3.0d0, 4.0d0);  a(3)  = (5.0d0, 6.0d0);  a(4)  = (7.0d0, 8.0d0)
  a(5)  = (9.0d0, 1.0d0);  a(6)  = (2.0d0, 3.0d0);  a(7)  = (4.0d0, 5.0d0);  a(8)  = (6.0d0, 7.0d0)
  a(9)  = (8.0d0, 9.0d0);  a(10) = (1.0d0, 2.0d0);  a(11) = (3.0d0, 4.0d0);  a(12) = (5.0d0, 6.0d0)
  d = 0.0d0; e = 0.0d0
  tauq = (0.0d0, 0.0d0); taup = (0.0d0, 0.0d0)
  call zgebrd(4, 3, a, 4, d, e, tauq, taup, work, lwork, info)
  call begin_test('upper_4x3')
  call print_int('info', info)
  call print_array('a', a_r, 24)
  call print_array('d', d, 3)
  call print_array('e', e, 2)
  call print_array('tauq', tauq_r, 6)
  call print_array('taup', taup_r, 6)
  call end_test()

  ! Test 2: 3x4 matrix (M < N, lower bidiagonal)
  a = (0.0d0, 0.0d0)
  a(1)  = (1.0d0, 2.0d0);  a(2)  = (3.0d0, 4.0d0);  a(3)  = (5.0d0, 6.0d0)
  a(4)  = (7.0d0, 8.0d0);  a(5)  = (9.0d0, 1.0d0);  a(6)  = (2.0d0, 3.0d0)
  a(7)  = (4.0d0, 5.0d0);  a(8)  = (6.0d0, 7.0d0);  a(9)  = (8.0d0, 9.0d0)
  a(10) = (1.0d0, 2.0d0);  a(11) = (3.0d0, 4.0d0);  a(12) = (5.0d0, 6.0d0)
  d = 0.0d0; e = 0.0d0
  tauq = (0.0d0, 0.0d0); taup = (0.0d0, 0.0d0)
  call zgebrd(3, 4, a, 3, d, e, tauq, taup, work, lwork, info)
  call begin_test('lower_3x4')
  call print_int('info', info)
  call print_array('a', a_r, 24)
  call print_array('d', d, 3)
  call print_array('e', e, 2)
  call print_array('tauq', tauq_r, 6)
  call print_array('taup', taup_r, 6)
  call end_test()

  ! Test 3: 6x6 matrix (well-conditioned, exercises blocking if NB < 6)
  a = (0.0d0, 0.0d0)
  a(1)  = ( 5.0d0,  1.0d0); a(2)  = ( 0.0d0,  2.0d0); a(3)  = ( 1.0d0, -1.0d0)
  a(4)  = ( 0.0d0,  0.0d0); a(5)  = ( 3.0d0,  0.0d0); a(6)  = ( 0.0d0,  1.0d0)
  a(7)  = ( 2.0d0,  0.0d0); a(8)  = ( 6.0d0, -1.0d0); a(9)  = ( 0.0d0,  3.0d0)
  a(10) = ( 1.0d0,  0.0d0); a(11) = ( 0.0d0,  0.0d0); a(12) = ( 4.0d0,  2.0d0)
  a(13) = ( 0.0d0,  1.0d0); a(14) = ( 1.0d0,  0.0d0); a(15) = ( 7.0d0,  0.0d0)
  a(16) = ( 0.0d0, -2.0d0); a(17) = ( 2.0d0,  1.0d0); a(18) = ( 0.0d0,  0.0d0)
  a(19) = ( 3.0d0,  0.0d0); a(20) = ( 0.0d0,  1.0d0); a(21) = ( 0.0d0, -1.0d0)
  a(22) = ( 8.0d0,  0.0d0); a(23) = ( 1.0d0,  0.0d0); a(24) = ( 0.0d0,  3.0d0)
  a(25) = ( 1.0d0, -1.0d0); a(26) = ( 0.0d0,  0.0d0); a(27) = ( 2.0d0,  0.0d0)
  a(28) = ( 0.0d0,  1.0d0); a(29) = ( 9.0d0,  0.0d0); a(30) = ( 1.0d0, -2.0d0)
  a(31) = ( 0.0d0,  2.0d0); a(32) = ( 1.0d0, -1.0d0); a(33) = ( 0.0d0,  0.0d0)
  a(34) = ( 3.0d0,  0.0d0); a(35) = ( 0.0d0,  1.0d0); a(36) = (10.0d0,  0.0d0)
  d = 0.0d0; e = 0.0d0
  tauq = (0.0d0, 0.0d0); taup = (0.0d0, 0.0d0)
  call zgebrd(6, 6, a, 6, d, e, tauq, taup, work, lwork, info)
  call begin_test('square_6x6')
  call print_int('info', info)
  call print_array('a', a_r, 72)
  call print_array('d', d, 6)
  call print_array('e', e, 5)
  call print_array('tauq', tauq_r, 12)
  call print_array('taup', taup_r, 12)
  call end_test()

  ! Test 4: M=0 (quick return)
  call zgebrd(0, 3, a, 1, d, e, tauq, taup, work, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 (quick return)
  call zgebrd(3, 0, a, 3, d, e, tauq, taup, work, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 matrix
  a(1) = (5.0d0, 3.0d0)
  d = 0.0d0; e = 0.0d0
  tauq = (0.0d0, 0.0d0); taup = (0.0d0, 0.0d0)
  call zgebrd(1, 1, a, 1, d, e, tauq, taup, work, lwork, info)
  call begin_test('one_by_one')
  call print_int('info', info)
  call print_array('a', a_r, 2)
  call print_array('d', d, 1)
  call print_array('tauq', tauq_r, 2)
  call print_array('taup', taup_r, 2)
  call end_test()

  ! Test 7: 35x33 matrix (M > N, large enough to trigger blocking with NB=32)
  ! Use a diagonally dominant matrix for numerical stability:
  ! A(i,j) = (delta_ij * (i+j+10) + sin(i+2*j), cos(2*i+j))
  a = (0.0d0, 0.0d0)
  do j = 1, BIGN
    do i = 1, BIGM
      if (i .eq. j) then
        a((j-1)*BIGM + i) = dcmplx(dble(i+j+10) + sin(dble(i+2*j)), cos(dble(2*i+j)))
      else
        a((j-1)*BIGM + i) = dcmplx(sin(dble(i+2*j)), cos(dble(2*i+j)))
      end if
    end do
  end do
  d = 0.0d0; e = 0.0d0
  tauq = (0.0d0, 0.0d0); taup = (0.0d0, 0.0d0)
  call zgebrd(BIGM, BIGN, a, BIGM, d, e, tauq, taup, work, lwork, info)
  call begin_test('upper_35x33')
  call print_int('info', info)
  call print_array('d', d, BIGN)
  call print_array('e', e, BIGN-1)
  call print_array('tauq', tauq_r, 2*BIGN)
  call print_array('taup', taup_r, 2*BIGN)
  call end_test()

  ! Test 8: 33x35 matrix (M < N, large enough to trigger blocking with NB=32)
  a = (0.0d0, 0.0d0)
  do j = 1, BIGM
    do i = 1, BIGN
      if (i .eq. j) then
        a((j-1)*BIGN + i) = dcmplx(dble(i+j+10) + sin(dble(i+2*j)), cos(dble(2*i+j)))
      else
        a((j-1)*BIGN + i) = dcmplx(sin(dble(i+2*j)), cos(dble(2*i+j)))
      end if
    end do
  end do
  d = 0.0d0; e = 0.0d0
  tauq = (0.0d0, 0.0d0); taup = (0.0d0, 0.0d0)
  call zgebrd(BIGN, BIGM, a, BIGN, d, e, tauq, taup, work, lwork, info)
  call begin_test('lower_33x35')
  call print_int('info', info)
  call print_array('d', d, BIGN)
  call print_array('e', e, BIGN-1)
  call print_array('tauq', tauq_r, 2*BIGN)
  call print_array('taup', taup_r, 2*BIGN)
  call end_test()

end program
