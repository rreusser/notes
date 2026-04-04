program test_dsbgvx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: INFO, N, KA, KB, LDAB, LDBB, LDQ, LDZ, IL, IU, M, i, j
  double precision :: VL, VU, ABSTOL
  double precision :: AB(200), BB(200), Q(NMAX*NMAX), Z(NMAX*NMAX)
  double precision :: W(NMAX), WORK(7*NMAX)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', UPLO='U', N=5, KA=2, KB=1
  ! All eigenvalues with eigenvectors, upper
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = N; LDZ = N
  ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0
  WORK = 0.0d0; IWORK = 0; IFAIL = 0

  ! A diagonal (row KA+1=3)
  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  ! A superdiag 1 (row 2)
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  ! A superdiag 2 (row 1)
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  ! B diagonal (row KB+1=2)
  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  ! B superdiag 1 (row 1)
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DSBGVX('V', 'A', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_A_U_n5_ka2_kb1')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call print_array('Z', Z, N*M)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='N', RANGE='A', UPLO='U', N=5, KA=2, KB=1
  ! All eigenvalues, no vectors
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = 1; LDZ = 1
  ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0

  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DSBGVX('N', 'A', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('N_A_U_n5_ka2_kb1')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='V', RANGE='A', UPLO='L', N=5, KA=2, KB=1
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = N; LDZ = N
  ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0
  WORK = 0.0d0; IWORK = 0; IFAIL = 0

  ! Lower band: diagonal at row 1, subdiag1 at row 2, subdiag2 at row 3
  AB(1)=10.0d0; AB(4)=8.0d0; AB(7)=6.0d0; AB(10)=9.0d0; AB(13)=7.0d0
  AB(2)=1.0d0; AB(5)=2.0d0; AB(8)=1.5d0; AB(11)=1.0d0
  AB(3)=0.5d0; AB(6)=0.3d0; AB(9)=0.4d0

  BB(1)=4.0d0; BB(3)=5.0d0; BB(5)=3.0d0; BB(7)=6.0d0; BB(9)=4.0d0
  BB(2)=0.2d0; BB(4)=0.3d0; BB(6)=0.1d0; BB(8)=0.2d0

  call DSBGVX('V', 'A', 'L', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_A_L_n5_ka2_kb1')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call print_array('Z', Z, N*M)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='V', UPLO='U', N=5, KA=2, KB=1
  ! Value range: eigenvalues in (1.0, 2.5]
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = N; LDZ = N
  VL = 1.0d0; VU = 2.5d0; ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0
  WORK = 0.0d0; IWORK = 0; IFAIL = 0

  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DSBGVX('V', 'V', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_V_U_n5_ka2_kb1')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  if (M > 0) then
    call print_array('Z', Z, N*M)
  end if
  call end_test()

  ! =====================================================
  ! Test 5: JOBZ='V', RANGE='I', UPLO='U', N=5, KA=2, KB=1
  ! Index range: eigenvalues 2 through 4
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = N; LDZ = N
  IL = 2; IU = 4; ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0
  WORK = 0.0d0; IWORK = 0; IFAIL = 0

  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DSBGVX('V', 'I', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_U_n5_ka2_kb1')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call print_array('Z', Z, N*M)
  call end_test()

  ! =====================================================
  ! Test 6: JOBZ='N', RANGE='V', UPLO='L', N=5, KA=2, KB=1
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = 1; LDZ = 1
  VL = 1.0d0; VU = 2.5d0; ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0

  AB(1)=10.0d0; AB(4)=8.0d0; AB(7)=6.0d0; AB(10)=9.0d0; AB(13)=7.0d0
  AB(2)=1.0d0; AB(5)=2.0d0; AB(8)=1.5d0; AB(11)=1.0d0
  AB(3)=0.5d0; AB(6)=0.3d0; AB(9)=0.4d0

  BB(1)=4.0d0; BB(3)=5.0d0; BB(5)=3.0d0; BB(7)=6.0d0; BB(9)=4.0d0
  BB(2)=0.2d0; BB(4)=0.3d0; BB(6)=0.1d0; BB(8)=0.2d0

  call DSBGVX('N', 'V', 'L', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('N_V_L_n5_ka2_kb1')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call end_test()

  ! =====================================================
  ! Test 7: JOBZ='N', RANGE='I', UPLO='L', N=5, KA=2, KB=1
  ! Select single eigenvalue IL=IU=3
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = 1; LDZ = 1
  IL = 3; IU = 3; ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0

  AB(1)=10.0d0; AB(4)=8.0d0; AB(7)=6.0d0; AB(10)=9.0d0; AB(13)=7.0d0
  AB(2)=1.0d0; AB(5)=2.0d0; AB(8)=1.5d0; AB(11)=1.0d0
  AB(3)=0.5d0; AB(6)=0.3d0; AB(9)=0.4d0

  BB(1)=4.0d0; BB(3)=5.0d0; BB(5)=3.0d0; BB(7)=6.0d0; BB(9)=4.0d0
  BB(2)=0.2d0; BB(4)=0.3d0; BB(6)=0.1d0; BB(8)=0.2d0

  call DSBGVX('N', 'I', 'L', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('N_I_L_n5_ka2_kb1')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call end_test()

  ! =====================================================
  ! Test 8: N=0 quick return
  ! =====================================================
  AB = 0.0d0; BB = 0.0d0; W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  call DSBGVX('N', 'A', 'U', 0, 1, 0, AB, 2, BB, 1, Q, 1, &
              VL, VU, IL, IU, 0.0d0, M, W, Z, 1, WORK, IWORK, IFAIL, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_int('M', M)
  call end_test()

  ! =====================================================
  ! Test 9: N=1 trivial, JOBZ='V', RANGE='A'
  ! =====================================================
  N = 1; KA = 0; KB = 0; LDAB = 1; LDBB = 1; LDQ = 1; LDZ = 1
  ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0
  WORK = 0.0d0; IWORK = 0; IFAIL = 0
  AB(1) = 3.0d0
  BB(1) = 2.0d0

  call DSBGVX('V', 'A', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_A')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call print_array('Z', Z, N*M)
  call end_test()

  ! =====================================================
  ! Test 10: KA=KB=0, N=3 (diagonal matrices), RANGE='A'
  ! =====================================================
  N = 3; KA = 0; KB = 0; LDAB = 1; LDBB = 1; LDQ = 1; LDZ = 1
  ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0
  AB(1) = 5.0d0; AB(2) = 6.0d0; AB(3) = 7.0d0
  BB(1) = 2.0d0; BB(2) = 3.0d0; BB(3) = 4.0d0

  call DSBGVX('N', 'A', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('diag_n3')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call end_test()

  ! =====================================================
  ! Test 11: JOBZ='V', RANGE='I', UPLO='U', N=5, KA=2, KB=1
  ! IL=1, IU=N (fast path with vectors)
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = N; LDZ = N
  IL = 1; IU = 5; ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0
  WORK = 0.0d0; IWORK = 0; IFAIL = 0

  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DSBGVX('V', 'I', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_U_n5_fast')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call print_array('Z', Z, N*M)
  call end_test()

  ! =====================================================
  ! Test 12: JOBZ='N', RANGE='I', UPLO='U', N=5, KA=2, KB=1
  ! IL=1, IU=N (fast path no vectors)
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = 1; LDZ = 1
  IL = 1; IU = 5; ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; W = 0.0d0; WORK = 0.0d0; IWORK = 0; IFAIL = 0

  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DSBGVX('N', 'I', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('N_I_U_n5_fast')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call end_test()

  ! =====================================================
  ! Test 13: JOBZ='V', RANGE='I', single eigenvalue IL=IU=1
  ! =====================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDQ = N; LDZ = N
  IL = 1; IU = 1; ABSTOL = 0.0d0
  AB = 0.0d0; BB = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0
  WORK = 0.0d0; IWORK = 0; IFAIL = 0

  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DSBGVX('V', 'I', 'U', N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ, &
              VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_U_n5_single')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('W', W, M)
  call print_array('Z', Z, N*M)
  call end_test()

end program test_dsbgvx
