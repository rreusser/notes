program test_zgesvd
  use test_utils
  implicit none

  integer, parameter :: MAXMN = 10
  double precision :: a_r(200), u_r(200), vt_r(200), work_r(10000)
  complex*16 :: a(100), u(100), vt(100), work(5000)
  double precision :: s(MAXMN), rwork(500)
  equivalence (a, a_r)
  equivalence (u, u_r)
  equivalence (vt, vt_r)
  equivalence (work, work_r)
  integer :: info, lwork, i

  lwork = 5000

  ! Test 1: 3x3 matrix, JOBU='A', JOBVT='A' (full SVD)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 1.0d0); a(6) = (2.0d0, 3.0d0)
  a(7) = (4.0d0, 5.0d0); a(8) = (6.0d0, 7.0d0); a(9) = (8.0d0, 9.0d0)
  s = 0.0d0; rwork = 0.0d0
  u = (0.0d0, 0.0d0); vt = (0.0d0, 0.0d0)
  call zgesvd('A', 'A', 3, 3, a, 3, s, u, 3, vt, 3, work, lwork, rwork, info)
  call begin_test('full_3x3')
  call print_int('info', info)
  call print_array('s', s, 3)
  call print_array('u', u_r, 18)
  call print_array('vt', vt_r, 18)
  call end_test()

  ! Test 2: 4x3 matrix, JOBU='S', JOBVT='S' (economy SVD)
  a = (0.0d0, 0.0d0)
  a(1)  = (1.0d0, 0.0d0); a(2)  = (0.0d0, 1.0d0); a(3)  = (2.0d0, 1.0d0); a(4)  = (1.0d0, 2.0d0)
  a(5)  = (3.0d0, 1.0d0); a(6)  = (1.0d0, 0.0d0); a(7)  = (0.0d0, 2.0d0); a(8)  = (2.0d0, 0.0d0)
  a(9)  = (0.0d0, 3.0d0); a(10) = (2.0d0, 1.0d0); a(11) = (1.0d0, 0.0d0); a(12) = (0.0d0, 1.0d0)
  s = 0.0d0; rwork = 0.0d0
  u = (0.0d0, 0.0d0); vt = (0.0d0, 0.0d0)
  ! U is 4x3 (LDU=4), VT is 3x3 (LDVT=3)
  call zgesvd('S', 'S', 4, 3, a, 4, s, u, 4, vt, 3, work, lwork, rwork, info)
  call begin_test('economy_4x3')
  call print_int('info', info)
  call print_array('s', s, 3)
  call print_array('u', u_r, 24)
  call print_array('vt', vt_r, 18)
  call end_test()

  ! Test 3: 3x4 matrix, JOBU='N', JOBVT='N' (singular values only)
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 0.0d0); a(3) = (0.0d0, 1.0d0)
  a(4) = (2.0d0, 1.0d0); a(5) = (0.0d0, 3.0d0); a(6) = (1.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (1.0d0, 2.0d0); a(9) = (0.0d0, 2.0d0)
  a(10)= (0.0d0, 1.0d0); a(11)= (2.0d0, 0.0d0); a(12)= (1.0d0, 1.0d0)
  s = 0.0d0; rwork = 0.0d0
  call zgesvd('N', 'N', 3, 4, a, 3, s, u, 1, vt, 1, work, lwork, rwork, info)
  call begin_test('values_only_3x4')
  call print_int('info', info)
  call print_array('s', s, 3)
  call end_test()

  ! Test 4: 2x2 full SVD
  a = (0.0d0, 0.0d0)
  a(1) = (3.0d0, 1.0d0); a(2) = (1.0d0, 2.0d0)
  a(3) = (2.0d0, 0.0d0); a(4) = (4.0d0, 1.0d0)
  s = 0.0d0; rwork = 0.0d0
  u = (0.0d0, 0.0d0); vt = (0.0d0, 0.0d0)
  call zgesvd('A', 'A', 2, 2, a, 2, s, u, 2, vt, 2, work, lwork, rwork, info)
  call begin_test('full_2x2')
  call print_int('info', info)
  call print_array('s', s, 2)
  call print_array('u', u_r, 8)
  call print_array('vt', vt_r, 8)
  call end_test()

  ! Test 5: 1x1 matrix
  a = (0.0d0, 0.0d0)
  a(1) = (5.0d0, 3.0d0)
  s = 0.0d0; rwork = 0.0d0
  u = (0.0d0, 0.0d0); vt = (0.0d0, 0.0d0)
  call zgesvd('A', 'A', 1, 1, a, 1, s, u, 1, vt, 1, work, lwork, rwork, info)
  call begin_test('full_1x1')
  call print_int('info', info)
  call print_array('s', s, 1)
  call print_array('u', u_r, 2)
  call print_array('vt', vt_r, 2)
  call end_test()

  ! Test 6: M=0 quick return
  s = 0.0d0
  call zgesvd('N', 'N', 0, 3, a, 1, s, u, 1, vt, 1, work, lwork, rwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0 quick return
  s = 0.0d0
  call zgesvd('N', 'N', 3, 0, a, 3, s, u, 1, vt, 1, work, lwork, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: 3x5 matrix, JOBU='A', JOBVT='A' (M < N, full SVD)
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (0.0d0, 3.0d0); a(3) = (1.0d0, 0.0d0)
  a(4) = (1.0d0, 1.0d0); a(5) = (4.0d0, 0.0d0); a(6) = (0.0d0, 2.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (1.0d0, 1.0d0); a(9) = (2.0d0, 2.0d0)
  a(10)= (0.0d0, 1.0d0); a(11)= (2.0d0, 0.0d0); a(12)= (1.0d0, 3.0d0)
  a(13)= (1.0d0, 2.0d0); a(14)= (0.0d0, 1.0d0); a(15)= (3.0d0, 1.0d0)
  s = 0.0d0; rwork = 0.0d0
  u = (0.0d0, 0.0d0); vt = (0.0d0, 0.0d0)
  ! U is 3x3 (LDU=3), VT is 5x5 (LDVT=5)
  call zgesvd('A', 'A', 3, 5, a, 3, s, u, 3, vt, 5, work, lwork, rwork, info)
  call begin_test('full_3x5')
  call print_int('info', info)
  call print_array('s', s, 3)
  call print_array('u', u_r, 18)
  call print_array('vt', vt_r, 50)
  call end_test()

  ! Test 9: 5x3 matrix, JOBU='S', JOBVT='A'
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 1.0d0); a(2) = (2.0d0, 0.0d0); a(3) = (0.0d0, 1.0d0)
  a(4) = (3.0d0, 2.0d0); a(5) = (1.0d0, 1.0d0)
  a(6) = (0.0d0, 2.0d0); a(7) = (1.0d0, 0.0d0); a(8) = (3.0d0, 1.0d0)
  a(9) = (2.0d0, 0.0d0); a(10)= (0.0d0, 3.0d0)
  a(11)= (2.0d0, 1.0d0); a(12)= (0.0d0, 1.0d0); a(13)= (1.0d0, 2.0d0)
  a(14)= (1.0d0, 0.0d0); a(15)= (2.0d0, 2.0d0)
  s = 0.0d0; rwork = 0.0d0
  u = (0.0d0, 0.0d0); vt = (0.0d0, 0.0d0)
  ! U is 5x3 (LDU=5), VT is 3x3 (LDVT=3)
  call zgesvd('S', 'A', 5, 3, a, 5, s, u, 5, vt, 3, work, lwork, rwork, info)
  call begin_test('economy_u_full_vt_5x3')
  call print_int('info', info)
  call print_array('s', s, 3)
  call print_array('u', u_r, 30)
  call print_array('vt', vt_r, 18)
  call end_test()

end program
