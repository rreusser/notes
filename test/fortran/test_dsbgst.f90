program test_dsbgst
  use test_utils
  implicit none

  integer :: INFO, N, KA, KB, LDAB, LDBB, LDX, I
  double precision :: AB(200), BB(200), X(400), WORK(200)
  double precision :: AB_save(200)

  ! =====================================================================
  ! Test 1: UPLO='U', N=5, KA=2, KB=1, VECT='N'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDX = 1
  AB = 0.0d0; BB = 0.0d0; X = 0.0d0; WORK = 0.0d0

  ! Diagonal of A (row KA+1 = row 3 in band storage)
  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  ! Superdiagonal 1 (row KA = row 2)
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  ! Superdiagonal 2 (row KA-1 = row 1)
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  ! B diagonal (row KB+1 = row 2)
  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  ! B superdiagonal 1 (row 1)
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call DSBGST('N', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, INFO)
  call begin_test('upper_n5_ka2_kb1_none')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save, N * LDAB)
  call print_array('BB', BB, N * LDBB)
  call print_array('AB', AB, N * LDAB)
  call end_test()

  ! =====================================================================
  ! Test 2: UPLO='L', N=5, KA=2, KB=1, VECT='N'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDX = 1
  AB = 0.0d0; BB = 0.0d0; X = 0.0d0; WORK = 0.0d0

  ! Diagonal of A (row 1 in lower band storage)
  AB(1)=10.0d0; AB(4)=8.0d0; AB(7)=6.0d0; AB(10)=9.0d0; AB(13)=7.0d0
  ! Subdiagonal 1 (row 2)
  AB(2)=1.0d0; AB(5)=2.0d0; AB(8)=1.5d0; AB(11)=1.0d0
  ! Subdiagonal 2 (row 3)
  AB(3)=0.5d0; AB(6)=0.3d0; AB(9)=0.4d0

  ! B diagonal (row 1 in lower band storage)
  BB(1)=4.0d0; BB(3)=5.0d0; BB(5)=3.0d0; BB(7)=6.0d0; BB(9)=4.0d0
  ! B subdiagonal 1 (row 2)
  BB(2)=0.2d0; BB(4)=0.3d0; BB(6)=0.1d0; BB(8)=0.2d0

  call DPBSTF('L', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call DSBGST('N', 'L', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, INFO)
  call begin_test('lower_n5_ka2_kb1_none')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save, N * LDAB)
  call print_array('BB', BB, N * LDBB)
  call print_array('AB', AB, N * LDAB)
  call end_test()

  ! =====================================================================
  ! Test 3: UPLO='U', N=5, KA=2, KB=1, VECT='V'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDX = 5
  AB = 0.0d0; BB = 0.0d0; X = 0.0d0; WORK = 0.0d0

  AB(3)=10.0d0; AB(6)=8.0d0; AB(9)=6.0d0; AB(12)=9.0d0; AB(15)=7.0d0
  AB(5)=1.0d0; AB(8)=2.0d0; AB(11)=1.5d0; AB(14)=1.0d0
  AB(7)=0.5d0; AB(10)=0.3d0; AB(13)=0.4d0

  BB(2)=4.0d0; BB(4)=5.0d0; BB(6)=3.0d0; BB(8)=6.0d0; BB(10)=4.0d0
  BB(3)=0.2d0; BB(5)=0.3d0; BB(7)=0.1d0; BB(9)=0.2d0

  call DPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call DSBGST('V', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, INFO)
  call begin_test('upper_n5_ka2_kb1_vect')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save, N * LDAB)
  call print_array('BB', BB, N * LDBB)
  call print_array('AB', AB, N * LDAB)
  call print_array('X', X, N * LDX)
  call end_test()

  ! =====================================================================
  ! Test 4: UPLO='L', N=5, KA=2, KB=1, VECT='V'
  ! =====================================================================
  N = 5; KA = 2; KB = 1; LDAB = 3; LDBB = 2; LDX = 5
  AB = 0.0d0; BB = 0.0d0; X = 0.0d0; WORK = 0.0d0

  AB(1)=10.0d0; AB(4)=8.0d0; AB(7)=6.0d0; AB(10)=9.0d0; AB(13)=7.0d0
  AB(2)=1.0d0; AB(5)=2.0d0; AB(8)=1.5d0; AB(11)=1.0d0
  AB(3)=0.5d0; AB(6)=0.3d0; AB(9)=0.4d0

  BB(1)=4.0d0; BB(3)=5.0d0; BB(5)=3.0d0; BB(7)=6.0d0; BB(9)=4.0d0
  BB(2)=0.2d0; BB(4)=0.3d0; BB(6)=0.1d0; BB(8)=0.2d0

  call DPBSTF('L', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call DSBGST('V', 'L', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, INFO)
  call begin_test('lower_n5_ka2_kb1_vect')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save, N * LDAB)
  call print_array('BB', BB, N * LDBB)
  call print_array('AB', AB, N * LDAB)
  call print_array('X', X, N * LDX)
  call end_test()

  ! =====================================================================
  ! Test 5: N=0 quick return
  ! =====================================================================
  call DSBGST('N', 'U', 0, 1, 0, AB, 2, BB, 1, X, 1, WORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================================
  ! Test 6: KA=KB=1, N=4, UPLO='U'
  ! =====================================================================
  N = 4; KA = 1; KB = 1; LDAB = 2; LDBB = 2; LDX = 1
  AB = 0.0d0; BB = 0.0d0; X = 0.0d0; WORK = 0.0d0

  AB(2)=5.0d0; AB(4)=6.0d0; AB(6)=7.0d0; AB(8)=8.0d0
  AB(3)=1.0d0; AB(5)=0.5d0; AB(7)=1.0d0

  BB(2)=3.0d0; BB(4)=4.0d0; BB(6)=5.0d0; BB(8)=3.0d0
  BB(3)=0.1d0; BB(5)=0.2d0; BB(7)=0.1d0

  call DPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call DSBGST('N', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, INFO)
  call begin_test('upper_n4_ka1_kb1')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save, N * LDAB)
  call print_array('BB', BB, N * LDBB)
  call print_array('AB', AB, N * LDAB)
  call end_test()

  ! =====================================================================
  ! Test 7: KA=0, KB=0 (diagonal)
  ! =====================================================================
  N = 3; KA = 0; KB = 0; LDAB = 1; LDBB = 1; LDX = 1
  AB = 0.0d0; BB = 0.0d0; WORK = 0.0d0

  AB(1)=5.0d0; AB(2)=6.0d0; AB(3)=7.0d0
  BB(1)=2.0d0; BB(2)=3.0d0; BB(3)=4.0d0

  call DPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call DSBGST('N', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, INFO)
  call begin_test('upper_n3_ka0_kb0')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save, N * LDAB)
  call print_array('BB', BB, N * LDAB)
  call print_array('AB', AB, N * LDAB)
  call end_test()

  ! =====================================================================
  ! Test 8: Larger - UPLO='U', N=8, KA=3, KB=2, VECT='V'
  ! =====================================================================
  N = 8; KA = 3; KB = 2; LDAB = 4; LDBB = 3; LDX = 8
  AB = 0.0d0; BB = 0.0d0; X = 0.0d0; WORK = 0.0d0

  do I = 1, N
    AB(4 + (I-1)*LDAB) = dble(10+I)
  end do
  do I = 1, N-1
    AB(3 + I*LDAB) = 0.5d0*dble(I)
  end do
  do I = 1, N-2
    AB(2 + (I+1)*LDAB) = 0.2d0*dble(I)
  end do
  do I = 1, N-3
    AB(1 + (I+2)*LDAB) = 0.1d0*dble(I)
  end do

  do I = 1, N
    BB(3 + (I-1)*LDBB) = dble(5+I)
  end do
  do I = 1, N-1
    BB(2 + I*LDBB) = 0.1d0*dble(I)
  end do
  do I = 1, N-2
    BB(1 + (I+1)*LDBB) = 0.05d0*dble(I)
  end do

  call DPBSTF('U', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call DSBGST('V', 'U', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, INFO)
  call begin_test('upper_n8_ka3_kb2_vect')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save, N * LDAB)
  call print_array('BB', BB, N * LDBB)
  call print_array('AB', AB, N * LDAB)
  call print_array('X', X, N * LDX)
  call end_test()

  ! =====================================================================
  ! Test 9: Larger - UPLO='L', N=8, KA=3, KB=2, VECT='V'
  ! =====================================================================
  N = 8; KA = 3; KB = 2; LDAB = 4; LDBB = 3; LDX = 8
  AB = 0.0d0; BB = 0.0d0; X = 0.0d0; WORK = 0.0d0

  do I = 1, N
    AB(1 + (I-1)*LDAB) = dble(10+I)
  end do
  do I = 1, N-1
    AB(2 + (I-1)*LDAB) = 0.5d0*dble(I)
  end do
  do I = 1, N-2
    AB(3 + (I-1)*LDAB) = 0.2d0*dble(I)
  end do
  do I = 1, N-3
    AB(4 + (I-1)*LDAB) = 0.1d0*dble(I)
  end do

  do I = 1, N
    BB(1 + (I-1)*LDBB) = dble(5+I)
  end do
  do I = 1, N-1
    BB(2 + (I-1)*LDBB) = 0.1d0*dble(I)
  end do
  do I = 1, N-2
    BB(3 + (I-1)*LDBB) = 0.05d0*dble(I)
  end do

  call DPBSTF('L', N, KB, BB, LDBB, INFO)
  AB_save(1:N*LDAB) = AB(1:N*LDAB)

  call DSBGST('V', 'L', N, KA, KB, AB, LDAB, BB, LDBB, X, LDX, WORK, INFO)
  call begin_test('lower_n8_ka3_kb2_vect')
  call print_int('info', INFO)
  call print_array('AB_in', AB_save, N * LDAB)
  call print_array('BB', BB, N * LDBB)
  call print_array('AB', AB, N * LDAB)
  call print_array('X', X, N * LDX)
  call end_test()

end program
