program test_zhbgv
  use test_utils
  implicit none

  integer :: INFO, N, KA, KB, LDAB, LDBB, LDZ, I

  ! Max sizes for our tests
  complex*16 :: AB(200), BB(200), Z(400), WORK(200)
  double precision :: AB_r(400), BB_r(400), Z_r(800), WORK_r(400)
  equivalence (AB, AB_r)
  equivalence (BB, BB_r)
  equivalence (Z, Z_r)
  equivalence (WORK, WORK_r)
  double precision :: W(50), RWORK(400)

  ! =====================================================================
  ! Test 1: UPLO='U', N=5, KA=2, KB=1, JOBZ='N' (eigenvalues only)
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDZ = 1
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  ! Diagonal of A (row KA+1 = row 3 in band storage) - must be real for Hermitian
  AB(3)=(10,0); AB(6)=(8,0); AB(9)=(6,0); AB(12)=(9,0); AB(15)=(7,0)
  ! Superdiagonal 1 (row KA = row 2) - complex
  AB(5)=(1,1); AB(8)=(2,-1); AB(11)=(1.5d0,0.5d0); AB(14)=(1,-1)
  ! Superdiagonal 2 (row KA-1 = row 1) - complex
  AB(7)=(0.5d0,-0.5d0); AB(10)=(0.3d0,0.2d0); AB(13)=(0.4d0,-0.1d0)

  ! B diagonal (row KB+1 = row 2) - must be real positive for positive definite
  BB(2)=(4,0); BB(4)=(5,0); BB(6)=(3,0); BB(8)=(6,0); BB(10)=(4,0)
  ! B superdiagonal 1 (row 1) - complex
  BB(3)=(0.2d0,0.1d0); BB(5)=(0.3d0,-0.1d0); BB(7)=(0.1d0,0.2d0); BB(9)=(0.2d0,-0.1d0)

  call ZHBGV('N', 'U', N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, &
             WORK, RWORK, INFO)
  call begin_test('upper_n5_ka2_kb1_noev')
  call print_int('info', INFO)
  call print_array('W', W, N)
  call end_test()

  ! =====================================================================
  ! Test 2: UPLO='U', N=5, KA=2, KB=1, JOBZ='V' (eigenvalues + vectors)
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDZ = 5
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(3)=(10,0); AB(6)=(8,0); AB(9)=(6,0); AB(12)=(9,0); AB(15)=(7,0)
  AB(5)=(1,1); AB(8)=(2,-1); AB(11)=(1.5d0,0.5d0); AB(14)=(1,-1)
  AB(7)=(0.5d0,-0.5d0); AB(10)=(0.3d0,0.2d0); AB(13)=(0.4d0,-0.1d0)

  BB(2)=(4,0); BB(4)=(5,0); BB(6)=(3,0); BB(8)=(6,0); BB(10)=(4,0)
  BB(3)=(0.2d0,0.1d0); BB(5)=(0.3d0,-0.1d0); BB(7)=(0.1d0,0.2d0); BB(9)=(0.2d0,-0.1d0)

  call ZHBGV('V', 'U', N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, &
             WORK, RWORK, INFO)
  call begin_test('upper_n5_ka2_kb1_ev')
  call print_int('info', INFO)
  call print_array('W', W, N)
  call print_array('Z', Z_r, N * LDZ * 2)
  call end_test()

  ! =====================================================================
  ! Test 3: UPLO='L', N=5, KA=2, KB=1, JOBZ='N'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDZ = 1
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  ! Diagonal of A (row 1 in lower band storage)
  AB(1)=(10,0); AB(4)=(8,0); AB(7)=(6,0); AB(10)=(9,0); AB(13)=(7,0)
  ! Subdiagonal 1 (row 2) - conjugate of upper superdiag
  AB(2)=(1,-1); AB(5)=(2,1); AB(8)=(1.5d0,-0.5d0); AB(11)=(1,1)
  ! Subdiagonal 2 (row 3)
  AB(3)=(0.5d0,0.5d0); AB(6)=(0.3d0,-0.2d0); AB(9)=(0.4d0,0.1d0)

  ! B diagonal (row 1)
  BB(1)=(4,0); BB(3)=(5,0); BB(5)=(3,0); BB(7)=(6,0); BB(9)=(4,0)
  ! B subdiagonal 1 (row 2) - conjugate of upper
  BB(2)=(0.2d0,-0.1d0); BB(4)=(0.3d0,0.1d0); BB(6)=(0.1d0,-0.2d0); BB(8)=(0.2d0,0.1d0)

  call ZHBGV('N', 'L', N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, &
             WORK, RWORK, INFO)
  call begin_test('lower_n5_ka2_kb1_noev')
  call print_int('info', INFO)
  call print_array('W', W, N)
  call end_test()

  ! =====================================================================
  ! Test 4: UPLO='L', N=5, KA=2, KB=1, JOBZ='V'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDZ = 5
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(1)=(10,0); AB(4)=(8,0); AB(7)=(6,0); AB(10)=(9,0); AB(13)=(7,0)
  AB(2)=(1,-1); AB(5)=(2,1); AB(8)=(1.5d0,-0.5d0); AB(11)=(1,1)
  AB(3)=(0.5d0,0.5d0); AB(6)=(0.3d0,-0.2d0); AB(9)=(0.4d0,0.1d0)

  BB(1)=(4,0); BB(3)=(5,0); BB(5)=(3,0); BB(7)=(6,0); BB(9)=(4,0)
  BB(2)=(0.2d0,-0.1d0); BB(4)=(0.3d0,0.1d0); BB(6)=(0.1d0,-0.2d0); BB(8)=(0.2d0,0.1d0)

  call ZHBGV('V', 'L', N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, &
             WORK, RWORK, INFO)
  call begin_test('lower_n5_ka2_kb1_ev')
  call print_int('info', INFO)
  call print_array('W', W, N)
  call print_array('Z', Z_r, N * LDZ * 2)
  call end_test()

  ! =====================================================================
  ! Test 5: N=0 quick return
  ! =====================================================================
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  call ZHBGV('N', 'U', 0, 1, 0, AB, 2, BB, 1, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================================
  ! Test 6: KA=KB=0, N=3 (diagonal matrices)
  ! =====================================================================
  N = 3; KA = 0; KB = 0; LDAB = 1; LDBB = 1; LDZ = 1
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(1)=(5,0); AB(2)=(6,0); AB(3)=(7,0)
  BB(1)=(2,0); BB(2)=(3,0); BB(3)=(4,0)

  call ZHBGV('N', 'U', N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, &
             WORK, RWORK, INFO)
  call begin_test('diag_n3')
  call print_int('info', INFO)
  call print_array('W', W, N)
  call end_test()

  ! =====================================================================
  ! Test 7: Larger - UPLO='U', N=8, KA=3, KB=2, JOBZ='V'
  ! =====================================================================
  N = 8; KA = 3; KB = 2; LDAB = 4; LDBB = 3; LDZ = 8
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  ! Diagonal - real for Hermitian
  do I = 1, N
    AB(4 + (I-1)*LDAB) = dcmplx(dble(10+I), 0.0d0)
  end do
  ! Superdiagonal 1 - complex
  do I = 1, N-1
    AB(3 + I*LDAB) = dcmplx(0.5d0*dble(I), 0.2d0*dble(I))
  end do
  ! Superdiagonal 2
  do I = 1, N-2
    AB(2 + (I+1)*LDAB) = dcmplx(0.2d0*dble(I), -0.1d0*dble(I))
  end do
  ! Superdiagonal 3
  do I = 1, N-3
    AB(1 + (I+2)*LDAB) = dcmplx(0.1d0*dble(I), 0.05d0*dble(I))
  end do

  ! B diagonal - real positive
  do I = 1, N
    BB(3 + (I-1)*LDBB) = dcmplx(dble(5+I), 0.0d0)
  end do
  ! B superdiagonal 1 - complex
  do I = 1, N-1
    BB(2 + I*LDBB) = dcmplx(0.1d0*dble(I), 0.05d0*dble(I))
  end do
  ! B superdiagonal 2
  do I = 1, N-2
    BB(1 + (I+1)*LDBB) = dcmplx(0.05d0*dble(I), -0.02d0*dble(I))
  end do

  call ZHBGV('V', 'U', N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, &
             WORK, RWORK, INFO)
  call begin_test('upper_n8_ka3_kb2_ev')
  call print_int('info', INFO)
  call print_array('W', W, N)
  call print_array('Z', Z_r, N * LDZ * 2)
  call end_test()

  ! =====================================================================
  ! Test 8: Larger - UPLO='L', N=8, KA=3, KB=2, JOBZ='V'
  ! =====================================================================
  N = 8; KA = 3; KB = 2; LDAB = 4; LDBB = 3; LDZ = 8
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  ! Diagonal - real
  do I = 1, N
    AB(1 + (I-1)*LDAB) = dcmplx(dble(10+I), 0.0d0)
  end do
  ! Subdiagonal 1 - conjugate of upper
  do I = 1, N-1
    AB(2 + (I-1)*LDAB) = dcmplx(0.5d0*dble(I), -0.2d0*dble(I))
  end do
  ! Subdiagonal 2
  do I = 1, N-2
    AB(3 + (I-1)*LDAB) = dcmplx(0.2d0*dble(I), 0.1d0*dble(I))
  end do
  ! Subdiagonal 3
  do I = 1, N-3
    AB(4 + (I-1)*LDAB) = dcmplx(0.1d0*dble(I), -0.05d0*dble(I))
  end do

  ! B diagonal - real positive
  do I = 1, N
    BB(1 + (I-1)*LDBB) = dcmplx(dble(5+I), 0.0d0)
  end do
  ! B subdiagonal 1 - conjugate of upper
  do I = 1, N-1
    BB(2 + (I-1)*LDBB) = dcmplx(0.1d0*dble(I), -0.05d0*dble(I))
  end do
  ! B subdiagonal 2
  do I = 1, N-2
    BB(3 + (I-1)*LDBB) = dcmplx(0.05d0*dble(I), 0.02d0*dble(I))
  end do

  call ZHBGV('V', 'L', N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, &
             WORK, RWORK, INFO)
  call begin_test('lower_n8_ka3_kb2_ev')
  call print_int('info', INFO)
  call print_array('W', W, N)
  call print_array('Z', Z_r, N * LDZ * 2)
  call end_test()

  ! =====================================================================
  ! Test 9: N=1 trivial
  ! =====================================================================
  N = 1; KA = 0; KB = 0; LDAB = 1; LDBB = 1; LDZ = 1
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); W = 0.0d0
  Z = (0.0d0, 0.0d0); WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(1)=(3,0)
  BB(1)=(2,0)

  call ZHBGV('V', 'U', N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, &
             WORK, RWORK, INFO)
  call begin_test('n1_trivial')
  call print_int('info', INFO)
  call print_array('W', W, N)
  call print_array('Z', Z_r, N * LDZ * 2)
  call end_test()

end program
