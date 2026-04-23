program test_zhbgst
  use test_utils
  implicit none

  integer :: INFO, N, KA, KB, LDAB, LDBB, LDX, I, J

  ! Max sizes for our tests
  complex*16 :: AB(200), BB(200), X(400), WORK(100)
  complex*16 :: AB_save(200)
  double precision :: AB_r(400), BB_r(400), X_r(800), WORK_r(200)
  double precision :: AB_save_r(400)
  equivalence (AB, AB_r)
  equivalence (BB, BB_r)
  equivalence (X, X_r)
  equivalence (WORK, WORK_r)
  equivalence (AB_save, AB_save_r)
  double precision :: RWORK(100)

  ! =====================================================================
  ! Test 1: UPLO='U', N=5, KA=2, KB=1, VECT='N'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDX = 1
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); X = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(3)=(10,0);AB(6)=(8,0);AB(9)=(6,0);AB(12)=(9,0);AB(15)=(7,0)
  AB(5)=(1,1);AB(8)=(2,-1);AB(11)=(1.5d0,0.5d0);AB(14)=(1,-1)
  AB(7)=(0.5d0,-0.5d0);AB(10)=(0.3d0,0.2d0);AB(13)=(0.4d0,-0.1d0)

  BB(2)=(4,0);BB(4)=(5,0);BB(6)=(3,0);BB(8)=(6,0);BB(10)=(4,0)
  BB(3)=(0.2d0,0.1d0);BB(5)=(0.3d0,-0.1d0);BB(7)=(0.1d0,0.2d0);BB(9)=(0.2d0,-0.1d0)

  call ZPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call ZHBGST('N', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, RWORK, INFO)
  call begin_test('upper_n5_ka2_kb1_none')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save_r, N * LDAB * 2)
  call print_array('BB', BB_r, N * LDBB * 2)
  call print_array('AB', AB_r, N * LDAB * 2)
  call end_test()

  ! =====================================================================
  ! Test 2: UPLO='L', N=5, KA=2, KB=1, VECT='N'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDX = 1
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); X = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(1)=(10,0);AB(4)=(8,0);AB(7)=(6,0);AB(10)=(9,0);AB(13)=(7,0)
  AB(2)=(1,-1);AB(5)=(2,1);AB(8)=(1.5d0,-0.5d0);AB(11)=(1,1)
  AB(3)=(0.5d0,0.5d0);AB(6)=(0.3d0,-0.2d0);AB(9)=(0.4d0,0.1d0)

  BB(1)=(4,0);BB(3)=(5,0);BB(5)=(3,0);BB(7)=(6,0);BB(9)=(4,0)
  BB(2)=(0.2d0,-0.1d0);BB(4)=(0.3d0,0.1d0);BB(6)=(0.1d0,-0.2d0);BB(8)=(0.2d0,0.1d0)

  call ZPBSTF('L', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call ZHBGST('N', 'L', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, RWORK, INFO)
  call begin_test('lower_n5_ka2_kb1_none')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save_r, N * LDAB * 2)
  call print_array('BB', BB_r, N * LDBB * 2)
  call print_array('AB', AB_r, N * LDAB * 2)
  call end_test()

  ! =====================================================================
  ! Test 3: UPLO='U', N=5, KA=2, KB=1, VECT='V'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDX = 5
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); X = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(3)=(10,0);AB(6)=(8,0);AB(9)=(6,0);AB(12)=(9,0);AB(15)=(7,0)
  AB(5)=(1,1);AB(8)=(2,-1);AB(11)=(1.5d0,0.5d0);AB(14)=(1,-1)
  AB(7)=(0.5d0,-0.5d0);AB(10)=(0.3d0,0.2d0);AB(13)=(0.4d0,-0.1d0)

  BB(2)=(4,0);BB(4)=(5,0);BB(6)=(3,0);BB(8)=(6,0);BB(10)=(4,0)
  BB(3)=(0.2d0,0.1d0);BB(5)=(0.3d0,-0.1d0);BB(7)=(0.1d0,0.2d0);BB(9)=(0.2d0,-0.1d0)

  call ZPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call ZHBGST('V', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, RWORK, INFO)
  call begin_test('upper_n5_ka2_kb1_vect')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save_r, N * LDAB * 2)
  call print_array('BB', BB_r, N * LDBB * 2)
  call print_array('AB', AB_r, N * LDAB * 2)
  call print_array('X', X_r, N * LDX * 2)
  call end_test()

  ! =====================================================================
  ! Test 4: UPLO='L', N=5, KA=2, KB=1, VECT='V'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDX = 5
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); X = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(1)=(10,0);AB(4)=(8,0);AB(7)=(6,0);AB(10)=(9,0);AB(13)=(7,0)
  AB(2)=(1,-1);AB(5)=(2,1);AB(8)=(1.5d0,-0.5d0);AB(11)=(1,1)
  AB(3)=(0.5d0,0.5d0);AB(6)=(0.3d0,-0.2d0);AB(9)=(0.4d0,0.1d0)

  BB(1)=(4,0);BB(3)=(5,0);BB(5)=(3,0);BB(7)=(6,0);BB(9)=(4,0)
  BB(2)=(0.2d0,-0.1d0);BB(4)=(0.3d0,0.1d0);BB(6)=(0.1d0,-0.2d0);BB(8)=(0.2d0,0.1d0)

  call ZPBSTF('L', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call ZHBGST('V', 'L', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, RWORK, INFO)
  call begin_test('lower_n5_ka2_kb1_vect')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save_r, N * LDAB * 2)
  call print_array('BB', BB_r, N * LDBB * 2)
  call print_array('AB', AB_r, N * LDAB * 2)
  call print_array('X', X_r, N * LDX * 2)
  call end_test()

  ! =====================================================================
  ! Test 5: N=0 quick return
  ! =====================================================================
  call ZHBGST('N', 'U', 0, 1, 0, AB, 2, BB, 1, X, 1, WORK, RWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================================
  ! Test 6: KA=KB=1, N=4
  ! =====================================================================
  N = 4; KA = 1; KB = 1; LDAB = 2; LDBB = 2; LDX = 1
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); X = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(2)=(5,0);AB(4)=(6,0);AB(6)=(7,0);AB(8)=(8,0)
  AB(3)=(1,0.5d0);AB(5)=(0.5d0,-1);AB(7)=(1,1)

  BB(2)=(3,0);BB(4)=(4,0);BB(6)=(5,0);BB(8)=(3,0)
  BB(3)=(0.1d0,0.2d0);BB(5)=(0.2d0,-0.1d0);BB(7)=(0.1d0,0.1d0)

  call ZPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call ZHBGST('N', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, RWORK, INFO)
  call begin_test('upper_n4_ka1_kb1')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save_r, N * LDAB * 2)
  call print_array('BB', BB_r, N * LDBB * 2)
  call print_array('AB', AB_r, N * LDAB * 2)
  call end_test()

  ! =====================================================================
  ! Test 7: KA=0, KB=0 (diagonal)
  ! =====================================================================
  N = 3; KA = 0; KB = 0; LDAB = 1; LDBB = 1; LDX = 1
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(1)=(5,0);AB(2)=(6,0);AB(3)=(7,0)
  BB(1)=(2,0);BB(2)=(3,0);BB(3)=(4,0)

  call ZPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call ZHBGST('N', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, RWORK, INFO)
  call begin_test('upper_n3_ka0_kb0')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save_r, N * LDAB * 2)
  call print_array('BB', BB_r, N * LDBB * 2)
  call print_array('AB', AB_r, N * LDAB * 2)
  call end_test()

  ! =====================================================================
  ! Test 8: Larger - UPLO='U', N=8, KA=3, KB=2, VECT='V'
  ! =====================================================================
  N = 8; KA = 3; KB = 2; LDAB = 4; LDBB = 3; LDX = 8
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); X = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  do I = 1, N
    AB(4 + (I-1)*LDAB) = dcmplx(dble(10+I), 0.0d0)
  end do
  do I = 1, N-1
    AB(3 + I*LDAB) = dcmplx(0.5d0*dble(I), 0.3d0*dble(mod(I,3)-1))
  end do
  do I = 1, N-2
    AB(2 + (I+1)*LDAB) = dcmplx(0.2d0*dble(I), -0.1d0*dble(mod(I,2)))
  end do
  do I = 1, N-3
    AB(1 + (I+2)*LDAB) = dcmplx(0.1d0*dble(I), 0.05d0*dble(mod(I,2)))
  end do

  do I = 1, N
    BB(3 + (I-1)*LDBB) = dcmplx(dble(5+I), 0.0d0)
  end do
  do I = 1, N-1
    BB(2 + I*LDBB) = dcmplx(0.1d0*dble(I), 0.05d0*dble(mod(I,2)-1))
  end do
  do I = 1, N-2
    BB(1 + (I+1)*LDBB) = dcmplx(0.05d0*dble(I), 0.02d0*dble(mod(I,3)-1))
  end do

  call ZPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call ZHBGST('V', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, RWORK, INFO)
  call begin_test('upper_n8_ka3_kb2_vect')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save_r, N * LDAB * 2)
  call print_array('BB', BB_r, N * LDBB * 2)
  call print_array('AB', AB_r, N * LDAB * 2)
  call print_array('X', X_r, N * LDX * 2)
  call end_test()

  ! =====================================================================
  ! Test 9: Larger - UPLO='L', N=8, KA=3, KB=2, VECT='V'
  ! =====================================================================
  N = 8; KA = 3; KB = 2; LDAB = 4; LDBB = 3; LDX = 8
  AB = (0.0d0, 0.0d0); BB = (0.0d0, 0.0d0); X = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  do I = 1, N
    AB(1 + (I-1)*LDAB) = dcmplx(dble(10+I), 0.0d0)
  end do
  do I = 1, N-1
    AB(2 + (I-1)*LDAB) = dcmplx(0.5d0*dble(I), -0.3d0*dble(mod(I,3)-1))
  end do
  do I = 1, N-2
    AB(3 + (I-1)*LDAB) = dcmplx(0.2d0*dble(I), 0.1d0*dble(mod(I,2)))
  end do
  do I = 1, N-3
    AB(4 + (I-1)*LDAB) = dcmplx(0.1d0*dble(I), -0.05d0*dble(mod(I,2)))
  end do

  do I = 1, N
    BB(1 + (I-1)*LDBB) = dcmplx(dble(5+I), 0.0d0)
  end do
  do I = 1, N-1
    BB(2 + (I-1)*LDBB) = dcmplx(0.1d0*dble(I), -0.05d0*dble(mod(I,2)-1))
  end do
  do I = 1, N-2
    BB(3 + (I-1)*LDBB) = dcmplx(0.05d0*dble(I), -0.02d0*dble(mod(I,3)-1))
  end do

  call ZPBSTF('L', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call ZHBGST('V', 'L', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, RWORK, INFO)
  call begin_test('lower_n8_ka3_kb2_vect')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save_r, N * LDAB * 2)
  call print_array('BB', BB_r, N * LDBB * 2)
  call print_array('AB', AB_r, N * LDAB * 2)
  call print_array('X', X_r, N * LDX * 2)
  call end_test()

end program
