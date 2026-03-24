program test_dpbcon
  use test_utils
  implicit none
  double precision :: work(100), anorm, rcond
  integer :: iwork(10), info, n, kd

  ! Test 1: 4x4 SPD banded, upper, KD=1, 1-norm
  ! Full SPD matrix:
  !   [4  1  0  0]
  !   [1  5  1  0]
  !   [0  1  6  2]
  !   [0  0  2  7]
  ! Upper Cholesky banded storage (LDAB=KD+1=2):
  !   Row 1 = superdiag, Row 2 = main diagonal
  block
    double precision :: ab1(2,4)
    n = 4; kd = 1
    ab1 = 0.0d0
    ! Col 1: main = 4
    ab1(2,1) = 4.0d0
    ! Col 2: super = 1, main = 5
    ab1(1,2) = 1.0d0; ab1(2,2) = 5.0d0
    ! Col 3: super = 1, main = 6
    ab1(1,3) = 1.0d0; ab1(2,3) = 6.0d0
    ! Col 4: super = 2, main = 7
    ab1(1,4) = 2.0d0; ab1(2,4) = 7.0d0

    ! 1-norm = infinity-norm for symmetric: max col sum
    ! Col 1: 4+1 = 5
    ! Col 2: 1+5+1 = 7
    ! Col 3: 1+6+2 = 9
    ! Col 4: 2+7 = 9
    anorm = 9.0d0

    call dpbtrf('U', n, kd, ab1, kd+1, info)
    if (info .ne. 0) then
      print *, 'dpbtrf failed: info=', info
      stop
    end if
    call dpbcon('U', n, kd, ab1, kd+1, anorm, rcond, work, iwork, info)
    call begin_test('upper_kd1')
    call print_scalar('anorm', anorm)
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 2: 4x4 SPD banded, lower, KD=1
  ! Same SPD matrix as test 1, lower Cholesky banded storage:
  !   Row 1 = main diagonal, Row 2 = subdiag
  block
    double precision :: ab2(2,4)
    n = 4; kd = 1
    ab2 = 0.0d0
    ! Col 1: main = 4, sub = 1
    ab2(1,1) = 4.0d0; ab2(2,1) = 1.0d0
    ! Col 2: main = 5, sub = 1
    ab2(1,2) = 5.0d0; ab2(2,2) = 1.0d0
    ! Col 3: main = 6, sub = 2
    ab2(1,3) = 6.0d0; ab2(2,3) = 2.0d0
    ! Col 4: main = 7
    ab2(1,4) = 7.0d0

    anorm = 9.0d0

    call dpbtrf('L', n, kd, ab2, kd+1, info)
    if (info .ne. 0) then
      print *, 'dpbtrf failed: info=', info
      stop
    end if
    call dpbcon('L', n, kd, ab2, kd+1, anorm, rcond, work, iwork, info)
    call begin_test('lower_kd1')
    call print_scalar('anorm', anorm)
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 3: 4x4 SPD banded, upper, KD=2
  ! Full SPD matrix:
  !   [10  2  1  0]
  !   [ 2 10  3  1]
  !   [ 1  3 10  2]
  !   [ 0  1  2 10]
  block
    double precision :: ab3(3,4)
    n = 4; kd = 2
    ab3 = 0.0d0
    ! Upper banded: row 1=superdiag-2, row 2=superdiag-1, row 3=main
    ! Col 1: main = 10
    ab3(3,1) = 10.0d0
    ! Col 2: super-1 = 2, main = 10
    ab3(2,2) = 2.0d0; ab3(3,2) = 10.0d0
    ! Col 3: super-2 = 1, super-1 = 3, main = 10
    ab3(1,3) = 1.0d0; ab3(2,3) = 3.0d0; ab3(3,3) = 10.0d0
    ! Col 4: super-2 = 1, super-1 = 2, main = 10
    ab3(1,4) = 1.0d0; ab3(2,4) = 2.0d0; ab3(3,4) = 10.0d0

    ! 1-norm: max col sum = max(10+2+1, 2+10+3+1, 1+3+10+2, 1+2+10) = max(13,16,16,13) = 16
    anorm = 16.0d0

    call dpbtrf('U', n, kd, ab3, kd+1, info)
    call dpbcon('U', n, kd, ab3, kd+1, anorm, rcond, work, iwork, info)
    call begin_test('upper_kd2')
    call print_scalar('anorm', anorm)
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 4: N=0 (rcond = 1)
  block
    double precision :: ab4(1,1)
    ab4 = 0.0d0
    n = 0; kd = 0; anorm = 0.0d0
    call dpbcon('U', n, kd, ab4, 1, anorm, rcond, work, iwork, info)
    call begin_test('n_zero')
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 5: N=1
  block
    double precision :: ab5(1,1)
    n = 1; kd = 0
    ab5(1,1) = 4.0d0
    anorm = 4.0d0
    call dpbtrf('U', n, kd, ab5, 1, info)
    call dpbcon('U', n, kd, ab5, 1, anorm, rcond, work, iwork, info)
    call begin_test('n_one')
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 6: anorm = 0 (rcond = 0)
  block
    double precision :: ab6(2,4)
    n = 4; kd = 1
    ab6 = 0.0d0
    ab6(2,1) = 4.0d0
    ab6(1,2) = 1.0d0; ab6(2,2) = 5.0d0
    ab6(1,3) = 1.0d0; ab6(2,3) = 6.0d0
    ab6(1,4) = 2.0d0; ab6(2,4) = 7.0d0
    call dpbtrf('U', n, kd, ab6, kd+1, info)
    anorm = 0.0d0
    call dpbcon('U', n, kd, ab6, kd+1, anorm, rcond, work, iwork, info)
    call begin_test('anorm_zero')
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

end program
