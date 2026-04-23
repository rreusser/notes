program test_dgbcon
  use test_utils
  implicit none
  double precision :: work(100), anorm, rcond
  integer :: ipiv(10), iwork(10), info, n, kl, ku

  ! Test 1: 4x4 well-conditioned tridiag, 1-norm
  ! KL=1, KU=1, LDAB = 2*KL+KU+1 = 4
  ! Full: [4 1 0 0; 1 5 2 0; 0 1 6 1; 0 0 2 7]
  block
    double precision :: ab1(4,4)
    n = 4; kl = 1; ku = 1
    ab1 = 0.0d0
    ! Row 1=fill, row 2=super(ku=1), row 3=main, row 4=sub(1)
    ab1(3,1) = 4.0d0; ab1(4,1) = 1.0d0
    ab1(2,2) = 1.0d0; ab1(3,2) = 5.0d0; ab1(4,2) = 1.0d0
    ab1(2,3) = 2.0d0; ab1(3,3) = 6.0d0; ab1(4,3) = 2.0d0
    ab1(2,4) = 1.0d0; ab1(3,4) = 7.0d0
    ! 1-norm: max col sum = max(5,7,10,8) = 10
    anorm = 10.0d0
    call dgbtrf(n, n, kl, ku, ab1, 4, ipiv, info)
    call dgbcon('1', n, kl, ku, ab1, 4, ipiv, anorm, rcond, work, iwork, info)
    call begin_test('tridiag_1norm')
    call print_scalar('anorm', anorm)
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 2: Same matrix, infinity-norm
  block
    double precision :: ab2(4,4)
    n = 4; kl = 1; ku = 1
    ab2 = 0.0d0
    ab2(3,1) = 4.0d0; ab2(4,1) = 1.0d0
    ab2(2,2) = 1.0d0; ab2(3,2) = 5.0d0; ab2(4,2) = 1.0d0
    ab2(2,3) = 2.0d0; ab2(3,3) = 6.0d0; ab2(4,3) = 2.0d0
    ab2(2,4) = 1.0d0; ab2(3,4) = 7.0d0
    ! Inf-norm: max row sum = max(5,8,8,9) = 9
    ! Actually: Row 1=[4,1,0,0]=>5, Row 2=[1,5,2,0]=>8, Row 3=[0,1,6,1]=>8, Row 4=[0,0,2,7]=>9
    anorm = 9.0d0
    call dgbtrf(n, n, kl, ku, ab2, 4, ipiv, info)
    call dgbcon('I', n, kl, ku, ab2, 4, ipiv, anorm, rcond, work, iwork, info)
    call begin_test('tridiag_Inorm')
    call print_scalar('anorm', anorm)
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 3: 4x4 with KL=2, KU=1, LDAB=6
  ! Full: [5 1 0 0; 2 6 2 0; 1 1 7 1; 0 2 3 8]
  block
    double precision :: ab3(6,4)
    n = 4; kl = 2; ku = 1
    ab3 = 0.0d0
    ! rows 1-2=fill, row 3=super(ku=1), row 4=main, row 5=sub1, row 6=sub2
    ab3(4,1) = 5.0d0; ab3(5,1) = 2.0d0; ab3(6,1) = 1.0d0
    ab3(3,2) = 1.0d0; ab3(4,2) = 6.0d0; ab3(5,2) = 1.0d0; ab3(6,2) = 2.0d0
    ab3(3,3) = 2.0d0; ab3(4,3) = 7.0d0; ab3(5,3) = 3.0d0
    ab3(3,4) = 1.0d0; ab3(4,4) = 8.0d0
    ! 1-norm: max col sum = max(8,10,12,9) = 12
    anorm = 12.0d0
    call dgbtrf(n, n, kl, ku, ab3, 6, ipiv, info)
    call dgbcon('1', n, kl, ku, ab3, 6, ipiv, anorm, rcond, work, iwork, info)
    call begin_test('kl2_ku1_1norm')
    call print_scalar('anorm', anorm)
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 4: N=0 (rcond = 1)
  block
    double precision :: ab4(1,1)
    ab4 = 0.0d0
    n = 0; kl = 0; ku = 0; anorm = 0.0d0
    call dgbcon('1', n, kl, ku, ab4, 1, ipiv, anorm, rcond, work, iwork, info)
    call begin_test('n_zero')
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 5: N=1
  block
    double precision :: ab5(1,1)
    n = 1; kl = 0; ku = 0
    ab5(1,1) = 3.0d0
    anorm = 3.0d0
    call dgbtrf(n, n, kl, ku, ab5, 1, ipiv, info)
    call dgbcon('1', n, kl, ku, ab5, 1, ipiv, anorm, rcond, work, iwork, info)
    call begin_test('n_one')
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

  ! Test 6: anorm = 0 (rcond = 0)
  block
    double precision :: ab6(4,4)
    n = 4; kl = 1; ku = 1
    ab6 = 0.0d0
    ab6(3,1) = 4.0d0; ab6(4,1) = 1.0d0
    ab6(2,2) = 1.0d0; ab6(3,2) = 5.0d0; ab6(4,2) = 1.0d0
    ab6(2,3) = 2.0d0; ab6(3,3) = 6.0d0; ab6(4,3) = 2.0d0
    ab6(2,4) = 1.0d0; ab6(3,4) = 7.0d0
    call dgbtrf(n, n, kl, ku, ab6, 4, ipiv, info)
    anorm = 0.0d0
    call dgbcon('1', n, kl, ku, ab6, 4, ipiv, anorm, rcond, work, iwork, info)
    call begin_test('anorm_zero')
    call print_scalar('rcond', rcond)
    call print_int('info', info)
    call end_test()
  end block

end program
