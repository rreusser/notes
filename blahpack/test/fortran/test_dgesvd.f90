program test_dgesvd
  use test_utils
  implicit none

  double precision :: a(100), a_copy(100), s(10), u(100), vt(100), work(10000)
  integer :: info, m, n, lda, ldu, ldvt, lwork, minmn
  integer :: i, j

  lwork = 10000

  ! =====================================================================
  ! Test 1: 4x3 (M > N), JOBU='A', JOBVT='A' — full U and V^T
  ! =====================================================================
  m = 4
  n = 3
  lda = m
  ldu = m
  ldvt = n
  minmn = min(m, n)

  ! Well-conditioned, diagonally dominant matrix (column-major)
  ! A = [ 5  1  2 ]
  !     [ 1  6  1 ]
  !     [ 2  1  7 ]
  !     [ 1  1  1 ]
  a(1)=5.0d0; a(2)=1.0d0; a(3)=2.0d0; a(4)=1.0d0   ! col 1
  a(5)=1.0d0; a(6)=6.0d0; a(7)=1.0d0; a(8)=1.0d0   ! col 2
  a(9)=2.0d0; a(10)=1.0d0; a(11)=7.0d0; a(12)=1.0d0 ! col 3

  ! Save A for verification
  a_copy(1:12) = a(1:12)

  call dgesvd('A', 'A', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('4x3_full_svd')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*m)
  call print_array('vt', vt, n*n)
  call end_test()

  ! =====================================================================
  ! Test 2: 4x3, JOBU='S', JOBVT='S' — compact SVD
  ! =====================================================================
  a(1:12) = a_copy(1:12)

  call dgesvd('S', 'S', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('4x3_compact_svd')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*minmn)
  call print_array('vt', vt, minmn*n)
  call end_test()

  ! =====================================================================
  ! Test 3: 4x3, JOBU='O', JOBVT='S' — U overwrites A
  ! =====================================================================
  a(1:12) = a_copy(1:12)

  call dgesvd('O', 'S', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('4x3_overwrite_u')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('a', a, m*minmn)  ! U overwrites A
  call print_array('vt', vt, minmn*n)
  call end_test()

  ! =====================================================================
  ! Test 4: 4x3, JOBU='N', JOBVT='N' — singular values only
  ! =====================================================================
  a(1:12) = a_copy(1:12)

  call dgesvd('N', 'N', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('4x3_values_only')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call end_test()

  ! =====================================================================
  ! Test 5: 3x4 (M < N), JOBU='A', JOBVT='A'
  ! =====================================================================
  m = 3
  n = 4
  lda = m
  ldu = m
  ldvt = n
  minmn = min(m, n)

  ! A = [ 5  1  2  1 ]
  !     [ 1  6  1  2 ]
  !     [ 2  1  7  1 ]
  a(1)=5.0d0; a(2)=1.0d0; a(3)=2.0d0   ! col 1
  a(4)=1.0d0; a(5)=6.0d0; a(6)=1.0d0   ! col 2
  a(7)=2.0d0; a(8)=1.0d0; a(9)=7.0d0   ! col 3
  a(10)=1.0d0; a(11)=2.0d0; a(12)=1.0d0 ! col 4

  a_copy(1:12) = a(1:12)

  call dgesvd('A', 'A', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('3x4_full_svd')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*m)
  call print_array('vt', vt, n*n)
  call end_test()

  ! =====================================================================
  ! Test 6: 3x4, JOBU='S', JOBVT='S' — compact SVD
  ! =====================================================================
  a(1:12) = a_copy(1:12)

  call dgesvd('S', 'S', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('3x4_compact_svd')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*minmn)
  call print_array('vt', vt, minmn*n)
  call end_test()

  ! =====================================================================
  ! Test 7: 3x3 (square), JOBU='A', JOBVT='A'
  ! =====================================================================
  m = 3
  n = 3
  lda = m
  ldu = m
  ldvt = n
  minmn = min(m, n)

  ! A = [ 5  1  2 ]
  !     [ 1  6  1 ]
  !     [ 2  1  7 ]
  a(1)=5.0d0; a(2)=1.0d0; a(3)=2.0d0
  a(4)=1.0d0; a(5)=6.0d0; a(6)=1.0d0
  a(7)=2.0d0; a(8)=1.0d0; a(9)=7.0d0

  a_copy(1:9) = a(1:9)

  call dgesvd('A', 'A', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('3x3_full_svd')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*m)
  call print_array('vt', vt, n*n)
  call end_test()

  ! =====================================================================
  ! Test 8: 1x1 edge case
  ! =====================================================================
  m = 1
  n = 1
  lda = 1
  ldu = 1
  ldvt = 1
  minmn = 1

  a(1) = 3.0d0

  call dgesvd('A', 'A', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('1x1_edge_case')
  call print_int('info', info)
  call print_array('s', s, 1)
  call print_array('u', u, 1)
  call print_array('vt', vt, 1)
  call end_test()

  ! =====================================================================
  ! Test 9: M=0 quick return
  ! =====================================================================
  m = 0
  n = 3
  lda = 1
  ldu = 1
  ldvt = 1

  call dgesvd('N', 'N', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('m0_quick_return')
  call print_int('info', info)
  call end_test()

  ! =====================================================================
  ! Test 10: 4x3, JOBU='S', JOBVT='O' — right vectors overwrite A
  ! =====================================================================
  m = 4
  n = 3
  lda = m
  ldu = m
  ldvt = n
  minmn = min(m, n)

  a(1)=5.0d0; a(2)=1.0d0; a(3)=2.0d0; a(4)=1.0d0
  a(5)=1.0d0; a(6)=6.0d0; a(7)=1.0d0; a(8)=1.0d0
  a(9)=2.0d0; a(10)=1.0d0; a(11)=7.0d0; a(12)=1.0d0
  a_copy(1:12) = a(1:12)

  call dgesvd('S', 'O', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('4x3_s_o')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*minmn)
  call print_array('a', a, n*n)  ! VT overwrites first N rows of A
  call end_test()

  ! =====================================================================
  ! Test 11: 4x3, JOBU='A', JOBVT='S'
  ! =====================================================================
  a(1:12) = a_copy(1:12)

  call dgesvd('A', 'S', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('4x3_a_s')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*m)
  call print_array('vt', vt, minmn*n)
  call end_test()

  ! =====================================================================
  ! Test 12: 3x4, JOBU='N', JOBVT='S'
  ! =====================================================================
  m = 3
  n = 4
  lda = m
  ldu = 1
  ldvt = n
  minmn = min(m, n)

  a(1)=5.0d0; a(2)=1.0d0; a(3)=2.0d0
  a(4)=1.0d0; a(5)=6.0d0; a(6)=1.0d0
  a(7)=2.0d0; a(8)=1.0d0; a(9)=7.0d0
  a(10)=1.0d0; a(11)=2.0d0; a(12)=1.0d0

  call dgesvd('N', 'S', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('3x4_n_s')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('vt', vt, minmn*n)
  call end_test()

  ! =====================================================================
  ! Test 13: 3x4, JOBU='S', JOBVT='N'
  ! =====================================================================
  a(1)=5.0d0; a(2)=1.0d0; a(3)=2.0d0
  a(4)=1.0d0; a(5)=6.0d0; a(6)=1.0d0
  a(7)=2.0d0; a(8)=1.0d0; a(9)=7.0d0
  a(10)=1.0d0; a(11)=2.0d0; a(12)=1.0d0

  ldu = m

  call dgesvd('S', 'N', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('3x4_s_n')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*minmn)
  call end_test()

  ! =====================================================================
  ! Test 14: 4x3, JOBU='N', JOBVT='A'
  ! =====================================================================
  m = 4
  n = 3
  lda = m
  ldu = 1
  ldvt = n
  minmn = min(m, n)

  a(1)=5.0d0; a(2)=1.0d0; a(3)=2.0d0; a(4)=1.0d0
  a(5)=1.0d0; a(6)=6.0d0; a(7)=1.0d0; a(8)=1.0d0
  a(9)=2.0d0; a(10)=1.0d0; a(11)=7.0d0; a(12)=1.0d0

  call dgesvd('N', 'A', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('4x3_n_a')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('vt', vt, n*n)
  call end_test()

  ! =====================================================================
  ! Test 15: 3x4, JOBU='A', JOBVT='N'
  ! =====================================================================
  m = 3
  n = 4
  lda = m
  ldu = m
  ldvt = 1
  minmn = min(m, n)

  a(1)=5.0d0; a(2)=1.0d0; a(3)=2.0d0
  a(4)=1.0d0; a(5)=6.0d0; a(6)=1.0d0
  a(7)=2.0d0; a(8)=1.0d0; a(9)=7.0d0
  a(10)=1.0d0; a(11)=2.0d0; a(12)=1.0d0

  call dgesvd('A', 'N', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, info)

  call begin_test('3x4_a_n')
  call print_int('info', info)
  call print_array('s', s, minmn)
  call print_array('u', u, m*m)
  call end_test()

end program test_dgesvd
