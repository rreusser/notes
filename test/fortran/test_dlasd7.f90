program test_dlasd7
  use test_utils
  implicit none

  integer, parameter :: NMAX = 20
  integer :: ICOMPQ, NL, NR, SQRE, K, N, M, INFO, GIVPTR
  integer :: GIVCOL(NMAX, 2), IDX(NMAX), IDXP(NMAX), IDXQ(NMAX)
  integer :: PERM(NMAX)
  double precision :: D(NMAX), Z(NMAX), ZW(NMAX)
  double precision :: VF(NMAX), VFW(NMAX), VL(NMAX), VLW(NMAX)
  double precision :: ALPHA, BETA, C, S
  double precision :: DSIGMA(NMAX), GIVNUM(NMAX, 2)
  integer :: i

  ! ================================================================
  ! Test 1: basic case with icompq=1, NL=3, NR=3, sqre=0
  ! ================================================================
  NL = 3
  NR = 3
  SQRE = 0
  ICOMPQ = 1
  N = NL + NR + 1
  M = N + SQRE
  ALPHA = 0.5D0
  BETA = 0.3D0

  ! Initialize D (singular values for two subproblems)
  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 5.0D0
  D(4) = 0.0D0  ! placeholder at NL+1
  D(5) = 2.0D0
  D(6) = 4.0D0
  D(7) = 6.0D0

  ! Initialize VF
  VF(1) = 0.1D0
  VF(2) = 0.2D0
  VF(3) = 0.3D0
  VF(4) = 0.4D0
  VF(5) = 0.5D0
  VF(6) = 0.6D0
  VF(7) = 0.7D0

  ! Initialize VL
  VL(1) = 0.7D0
  VL(2) = 0.6D0
  VL(3) = 0.5D0
  VL(4) = 0.4D0
  VL(5) = 0.3D0
  VL(6) = 0.2D0
  VL(7) = 0.1D0

  ! Initialize IDXQ (sorting perm for each subproblem, 1-based)
  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 3
  IDXQ(4) = 0  ! placeholder at NL+1
  IDXQ(5) = 1
  IDXQ(6) = 2
  IDXQ(7) = 3

  ! Zero work arrays
  do i = 1, N
    ZW(i) = 0.0D0
    VFW(i) = 0.0D0
    VLW(i) = 0.0D0
    DSIGMA(i) = 0.0D0
    IDX(i) = 0
    IDXP(i) = 0
    PERM(i) = 0
  end do
  do i = 1, NMAX
    GIVCOL(i, 1) = 0
    GIVCOL(i, 2) = 0
    GIVNUM(i, 1) = 0.0D0
    GIVNUM(i, 2) = 0.0D0
  end do

  CALL DLASD7(ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL, &
              VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ, &
              PERM, GIVPTR, GIVCOL, NMAX, GIVNUM, NMAX, &
              C, S, INFO)

  call begin_test('basic_icompq1')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_int('givptr', GIVPTR)
  call print_scalar('c', C)
  call print_scalar('s', S)
  call print_array('D', D, N)
  call print_array('Z', Z, M)
  call print_array('ZW', ZW, N)
  call print_array('VF', VF, M)
  call print_array('VL', VL, M)
  call print_array('VFW', VFW, N)
  call print_array('VLW', VLW, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('PERM', PERM, N)
  call print_int_array('GIVCOL1', GIVCOL(1:N,1), N)
  call print_int_array('GIVCOL2', GIVCOL(1:N,2), N)
  call print_array('GIVNUM1', GIVNUM(1:N,1), N)
  call print_array('GIVNUM2', GIVNUM(1:N,2), N)
  call end_test()

  ! ================================================================
  ! Test 2: icompq=0 (singular values only, no vectors)
  ! ================================================================
  NL = 3
  NR = 3
  SQRE = 0
  ICOMPQ = 0
  N = NL + NR + 1
  M = N + SQRE
  ALPHA = 0.5D0
  BETA = 0.3D0

  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 5.0D0
  D(4) = 0.0D0
  D(5) = 2.0D0
  D(6) = 4.0D0
  D(7) = 6.0D0

  VF(1) = 0.1D0
  VF(2) = 0.2D0
  VF(3) = 0.3D0
  VF(4) = 0.4D0
  VF(5) = 0.5D0
  VF(6) = 0.6D0
  VF(7) = 0.7D0

  VL(1) = 0.7D0
  VL(2) = 0.6D0
  VL(3) = 0.5D0
  VL(4) = 0.4D0
  VL(5) = 0.3D0
  VL(6) = 0.2D0
  VL(7) = 0.1D0

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 3
  IDXQ(4) = 0
  IDXQ(5) = 1
  IDXQ(6) = 2
  IDXQ(7) = 3

  do i = 1, N
    ZW(i) = 0.0D0
    VFW(i) = 0.0D0
    VLW(i) = 0.0D0
    DSIGMA(i) = 0.0D0
    IDX(i) = 0
    IDXP(i) = 0
    PERM(i) = 0
  end do
  do i = 1, NMAX
    GIVCOL(i, 1) = 0
    GIVCOL(i, 2) = 0
    GIVNUM(i, 1) = 0.0D0
    GIVNUM(i, 2) = 0.0D0
  end do

  CALL DLASD7(ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL, &
              VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ, &
              PERM, GIVPTR, GIVCOL, NMAX, GIVNUM, NMAX, &
              C, S, INFO)

  call begin_test('basic_icompq0')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, M)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('VF', VF, M)
  call print_array('VL', VL, M)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDXQ', IDXQ, N)
  call end_test()

  ! ================================================================
  ! Test 3: sqre=1 (rectangular lower block), icompq=1
  ! ================================================================
  NL = 2
  NR = 2
  SQRE = 1
  ICOMPQ = 1
  N = NL + NR + 1
  M = N + SQRE
  ALPHA = 0.8D0
  BETA = 0.6D0

  D(1) = 1.0D0
  D(2) = 4.0D0
  D(3) = 0.0D0
  D(4) = 2.0D0
  D(5) = 5.0D0

  VF(1) = 0.1D0
  VF(2) = 0.3D0
  VF(3) = 0.5D0
  VF(4) = 0.7D0
  VF(5) = 0.9D0
  VF(6) = 0.2D0  ! extra element for M = N+1

  VL(1) = 0.9D0
  VL(2) = 0.7D0
  VL(3) = 0.5D0
  VL(4) = 0.3D0
  VL(5) = 0.1D0
  VL(6) = 0.4D0  ! extra element for M = N+1

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 0
  IDXQ(4) = 1
  IDXQ(5) = 2

  do i = 1, M
    ZW(i) = 0.0D0
    VFW(i) = 0.0D0
    VLW(i) = 0.0D0
  end do
  do i = 1, N
    DSIGMA(i) = 0.0D0
    IDX(i) = 0
    IDXP(i) = 0
    PERM(i) = 0
  end do
  do i = 1, NMAX
    GIVCOL(i, 1) = 0
    GIVCOL(i, 2) = 0
    GIVNUM(i, 1) = 0.0D0
    GIVNUM(i, 2) = 0.0D0
  end do

  CALL DLASD7(ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL, &
              VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ, &
              PERM, GIVPTR, GIVCOL, NMAX, GIVNUM, NMAX, &
              C, S, INFO)

  call begin_test('sqre1_icompq1')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_int('givptr', GIVPTR)
  call print_scalar('c', C)
  call print_scalar('s', S)
  call print_array('D', D, N)
  call print_array('Z', Z, M)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('VF', VF, M)
  call print_array('VL', VL, M)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('PERM', PERM, N)
  call print_int_array('GIVCOL1', GIVCOL(1:N,1), N)
  call print_int_array('GIVCOL2', GIVCOL(1:N,2), N)
  call print_array('GIVNUM1', GIVNUM(1:N,1), N)
  call print_array('GIVNUM2', GIVNUM(1:N,2), N)
  call end_test()

  ! ================================================================
  ! Test 4: deflation case - duplicate singular values
  ! ================================================================
  NL = 3
  NR = 3
  SQRE = 0
  ICOMPQ = 1
  N = NL + NR + 1
  M = N + SQRE
  ALPHA = 0.5D0
  BETA = 0.3D0

  ! Set D so some values will be close/equal after merge
  D(1) = 1.0D0
  D(2) = 2.0D0
  D(3) = 3.0D0
  D(4) = 0.0D0
  D(5) = 1.0D0  ! duplicate of D(1) -> deflation
  D(6) = 2.0D0  ! duplicate of D(2) -> deflation
  D(7) = 4.0D0

  VF(1) = 0.1D0
  VF(2) = 0.2D0
  VF(3) = 0.3D0
  VF(4) = 0.4D0
  VF(5) = 0.5D0
  VF(6) = 0.6D0
  VF(7) = 0.7D0

  VL(1) = 0.7D0
  VL(2) = 0.6D0
  VL(3) = 0.5D0
  VL(4) = 0.4D0
  VL(5) = 0.3D0
  VL(6) = 0.2D0
  VL(7) = 0.1D0

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 3
  IDXQ(4) = 0
  IDXQ(5) = 1
  IDXQ(6) = 2
  IDXQ(7) = 3

  do i = 1, N
    ZW(i) = 0.0D0
    VFW(i) = 0.0D0
    VLW(i) = 0.0D0
    DSIGMA(i) = 0.0D0
    IDX(i) = 0
    IDXP(i) = 0
    PERM(i) = 0
  end do
  do i = 1, NMAX
    GIVCOL(i, 1) = 0
    GIVCOL(i, 2) = 0
    GIVNUM(i, 1) = 0.0D0
    GIVNUM(i, 2) = 0.0D0
  end do

  CALL DLASD7(ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL, &
              VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ, &
              PERM, GIVPTR, GIVCOL, NMAX, GIVNUM, NMAX, &
              C, S, INFO)

  call begin_test('deflation')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_int('givptr', GIVPTR)
  call print_scalar('c', C)
  call print_scalar('s', S)
  call print_array('D', D, N)
  call print_array('Z', Z, M)
  call print_array('DSIGMA', DSIGMA, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('PERM', PERM, N)
  call print_int_array('GIVCOL1', GIVCOL(1:N,1), N)
  call print_int_array('GIVCOL2', GIVCOL(1:N,2), N)
  call print_array('GIVNUM1', GIVNUM(1:N,1), N)
  call print_array('GIVNUM2', GIVNUM(1:N,2), N)
  call end_test()

  ! ================================================================
  ! Test 5: NL=1, NR=1 (minimum size)
  ! ================================================================
  NL = 1
  NR = 1
  SQRE = 0
  ICOMPQ = 1
  N = NL + NR + 1
  M = N + SQRE
  ALPHA = 1.0D0
  BETA = 2.0D0

  D(1) = 3.0D0
  D(2) = 0.0D0
  D(3) = 7.0D0

  VF(1) = 0.5D0
  VF(2) = 0.3D0
  VF(3) = 0.8D0

  VL(1) = 0.4D0
  VL(2) = 0.6D0
  VL(3) = 0.9D0

  IDXQ(1) = 1
  IDXQ(2) = 0
  IDXQ(3) = 1

  do i = 1, N
    ZW(i) = 0.0D0
    VFW(i) = 0.0D0
    VLW(i) = 0.0D0
    DSIGMA(i) = 0.0D0
    IDX(i) = 0
    IDXP(i) = 0
    PERM(i) = 0
  end do
  do i = 1, NMAX
    GIVCOL(i, 1) = 0
    GIVCOL(i, 2) = 0
    GIVNUM(i, 1) = 0.0D0
    GIVNUM(i, 2) = 0.0D0
  end do

  CALL DLASD7(ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL, &
              VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ, &
              PERM, GIVPTR, GIVCOL, NMAX, GIVNUM, NMAX, &
              C, S, INFO)

  call begin_test('min_size')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_int('givptr', GIVPTR)
  call print_scalar('c', C)
  call print_scalar('s', S)
  call print_array('D', D, N)
  call print_array('Z', Z, M)
  call print_array('DSIGMA', DSIGMA, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('PERM', PERM, N)
  call end_test()

  ! ================================================================
  ! Test 6: sqre=1 with small z component (deflation via z)
  ! ================================================================
  NL = 2
  NR = 2
  SQRE = 1
  ICOMPQ = 1
  N = NL + NR + 1
  M = N + SQRE
  ALPHA = 1.0D-20
  BETA = 0.5D0

  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 0.0D0
  D(4) = 2.0D0
  D(5) = 5.0D0

  VF(1) = 0.1D0
  VF(2) = 0.2D0
  VF(3) = 0.3D0
  VF(4) = 0.4D0
  VF(5) = 0.5D0
  VF(6) = 0.6D0

  VL(1) = 0.6D0
  VL(2) = 0.5D0
  VL(3) = 0.4D0
  VL(4) = 0.3D0
  VL(5) = 0.2D0
  VL(6) = 0.1D0

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 0
  IDXQ(4) = 1
  IDXQ(5) = 2

  do i = 1, M
    ZW(i) = 0.0D0
    VFW(i) = 0.0D0
    VLW(i) = 0.0D0
  end do
  do i = 1, N
    DSIGMA(i) = 0.0D0
    IDX(i) = 0
    IDXP(i) = 0
    PERM(i) = 0
  end do
  do i = 1, NMAX
    GIVCOL(i, 1) = 0
    GIVCOL(i, 2) = 0
    GIVNUM(i, 1) = 0.0D0
    GIVNUM(i, 2) = 0.0D0
  end do

  CALL DLASD7(ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL, &
              VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ, &
              PERM, GIVPTR, GIVCOL, NMAX, GIVNUM, NMAX, &
              C, S, INFO)

  call begin_test('small_z_deflation')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_int('givptr', GIVPTR)
  call print_scalar('c', C)
  call print_scalar('s', S)
  call print_array('D', D, N)
  call print_array('Z', Z, M)
  call print_array('DSIGMA', DSIGMA, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('PERM', PERM, N)
  call end_test()

  ! ================================================================
  ! Test 7: all z-values small (complete deflation)
  ! ================================================================
  NL = 2
  NR = 2
  SQRE = 0
  ICOMPQ = 0
  N = NL + NR + 1
  M = N + SQRE
  ALPHA = 1.0D-20
  BETA = 1.0D-20

  D(1) = 1.0D0
  D(2) = 3.0D0
  D(3) = 0.0D0
  D(4) = 2.0D0
  D(5) = 5.0D0

  VF(1) = 0.1D0
  VF(2) = 0.2D0
  VF(3) = 0.3D0
  VF(4) = 0.4D0
  VF(5) = 0.5D0

  VL(1) = 0.5D0
  VL(2) = 0.4D0
  VL(3) = 0.3D0
  VL(4) = 0.2D0
  VL(5) = 0.1D0

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 0
  IDXQ(4) = 1
  IDXQ(5) = 2

  do i = 1, N
    ZW(i) = 0.0D0
    VFW(i) = 0.0D0
    VLW(i) = 0.0D0
    DSIGMA(i) = 0.0D0
    IDX(i) = 0
    IDXP(i) = 0
    PERM(i) = 0
  end do
  do i = 1, NMAX
    GIVCOL(i, 1) = 0
    GIVCOL(i, 2) = 0
    GIVNUM(i, 1) = 0.0D0
    GIVNUM(i, 2) = 0.0D0
  end do

  CALL DLASD7(ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL, &
              VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ, &
              PERM, GIVPTR, GIVCOL, NMAX, GIVNUM, NMAX, &
              C, S, INFO)

  call begin_test('all_deflated')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, M)
  call print_array('DSIGMA', DSIGMA, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXP', IDXP, N)
  call end_test()

end program
