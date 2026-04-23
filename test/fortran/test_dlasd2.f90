program test_dlasd2
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: NL, NR, SQRE, K, LDU, LDVT, LDU2, LDVT2, INFO
  integer :: N, M, I, J
  double precision :: ALPHA, BETA
  double precision :: D(NMAX), Z(NMAX+1), DSIGMA(NMAX+1)
  double precision :: U(NMAX, NMAX), VT(NMAX+1, NMAX+1)
  double precision :: U2(NMAX, NMAX), VT2(NMAX+1, NMAX+1)
  integer :: IDXP(NMAX), IDX(NMAX), IDXC(NMAX)
  integer :: IDXQ(NMAX+1), COLTYP(NMAX)

  ! Pack arrays for printing 2D matrices
  double precision :: Upk(NMAX*NMAX), VTpk((NMAX+1)*(NMAX+1))
  double precision :: U2pk(NMAX*NMAX), VT2pk((NMAX+1)*(NMAX+1))

  ! ========================================================
  ! Test case 1: Basic NL=2, NR=2, SQRE=0
  ! N = NL+NR+1 = 5, M = N+SQRE = 5
  ! ========================================================
  NL = 2
  NR = 2
  SQRE = 0
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  ! Set up D: sorted singular values for each subproblem
  ! Upper block (NL entries): D(1)..D(NL) sorted
  D(1) = 1.0D0
  D(2) = 3.0D0
  ! Lower block (NR entries): D(NL+2)..D(N) sorted
  D(3) = 0.0D0   ! D(NLP1) is the gap/separator, will be overwritten
  D(4) = 2.0D0
  D(5) = 4.0D0

  ALPHA = 0.5D0
  BETA = 0.7D0

  ! Set up U as identity
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! Set up VT as identity
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do

  ! Set up IDXQ: permutation that sorts each sub-block
  ! For the upper block (indices 1..NL), already sorted
  IDXQ(1) = 1
  IDXQ(2) = 2
  ! For the lower block (indices NLP2..N), already sorted
  IDXQ(3) = 0   ! NLP1 position, not used initially
  IDXQ(4) = 1
  IDXQ(5) = 2

  ! Zero output arrays
  do I = 1, N
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack U (N x N) for printing
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  ! Pack VT (M x M) for printing
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  ! Pack U2 (N x N) for printing
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  ! Pack VT2 (M x M) for printing
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('basic_nl2_nr2_sqre0')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ========================================================
  ! Test case 2: SQRE=1 case, NL=2, NR=2
  ! N = 5, M = 6
  ! ========================================================
  NL = 2
  NR = 2
  SQRE = 1
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  D(1) = 1.5D0
  D(2) = 3.5D0
  D(3) = 0.0D0
  D(4) = 2.5D0
  D(5) = 5.0D0

  ALPHA = 0.3D0
  BETA = 0.4D0

  ! U = identity (N x N)
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! VT = identity (M x M)
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 0
  IDXQ(4) = 1
  IDXQ(5) = 2

  ! Zero output arrays
  do I = 1, N+1
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
  end do
  do I = 1, N
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack U (N x N)
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  ! Pack VT (M x M)
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  ! Pack U2 (N x N)
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  ! Pack VT2 (M x M)
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('sqre1_nl2_nr2')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ========================================================
  ! Test case 3: NL=3, NR=3, SQRE=0 — larger problem with
  ! non-trivial values and potential deflation
  ! ========================================================
  NL = 3
  NR = 3
  SQRE = 0
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  ! Set up D with distinct values
  D(1) = 0.5D0
  D(2) = 1.5D0
  D(3) = 2.5D0
  D(4) = 0.0D0
  D(5) = 1.0D0
  D(6) = 2.0D0
  D(7) = 3.0D0

  ALPHA = 0.6D0
  BETA = 0.8D0

  ! U = identity (N x N)
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! VT = identity (M x M)
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 3
  IDXQ(4) = 0
  IDXQ(5) = 1
  IDXQ(6) = 2
  IDXQ(7) = 3

  ! Zero output arrays
  do I = 1, N
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack U (N x N)
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  ! Pack VT (M x M)
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  ! Pack U2 (N x N)
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  ! Pack VT2 (M x M)
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('nl3_nr3_sqre0')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ========================================================
  ! Test case 4: Deflation case — duplicate singular values
  ! NL=2, NR=2, SQRE=0, with D values close enough to deflate
  ! ========================================================
  NL = 2
  NR = 2
  SQRE = 0
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  ! D values: 1.0, 2.0 | 1.0, 3.0
  ! The 1.0 values should trigger deflation (close singular values)
  D(1) = 1.0D0
  D(2) = 2.0D0
  D(3) = 0.0D0
  D(4) = 1.0D0
  D(5) = 3.0D0

  ALPHA = 0.5D0
  BETA = 0.5D0

  ! U = identity
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! VT = identity
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 0
  IDXQ(4) = 1
  IDXQ(5) = 2

  ! Zero output arrays
  do I = 1, N
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack matrices
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('deflation_close_values')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ========================================================
  ! Test case 5: NL=1, NR=1, SQRE=0 — minimal problem
  ! N = 3, M = 3
  ! ========================================================
  NL = 1
  NR = 1
  SQRE = 0
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  D(1) = 2.0D0
  D(2) = 0.0D0
  D(3) = 4.0D0

  ALPHA = 0.8D0
  BETA = 0.6D0

  ! U = identity (3x3)
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! VT = identity (3x3)
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do

  IDXQ(1) = 1
  IDXQ(2) = 0
  IDXQ(3) = 1

  ! Zero output arrays
  do I = 1, N
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack matrices
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('minimal_nl1_nr1')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ========================================================
  ! Test case 6: SQRE=1 with NL=1, NR=1
  ! N=3, M=4
  ! ========================================================
  NL = 1
  NR = 1
  SQRE = 1
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  D(1) = 3.0D0
  D(2) = 0.0D0
  D(3) = 5.0D0

  ALPHA = 0.4D0
  BETA = 0.9D0

  ! U = identity (3x3)
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! VT = identity (4x4)
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do

  IDXQ(1) = 1
  IDXQ(2) = 0
  IDXQ(3) = 1

  ! Zero output arrays
  do I = 1, N+1
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
  end do
  do I = 1, N
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack matrices
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('sqre1_nl1_nr1')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ========================================================
  ! Test case 7: Force close-values deflation (Givens rotation)
  ! Two close singular values with non-tiny z entries.
  ! Use non-identity VT so that all Z entries are non-zero.
  ! NL=2, NR=2, SQRE=0
  ! ========================================================
  NL = 2
  NR = 2
  SQRE = 0
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  ! D values with two identical values (5.0, 5.0)
  ! After sorting, they should be adjacent and within tolerance
  D(1) = 5.0D0
  D(2) = 10.0D0
  D(3) = 0.0D0
  D(4) = 5.0D0
  D(5) = 15.0D0

  ALPHA = 2.0D0
  BETA = 3.0D0

  ! U = identity
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! VT: start with identity and put non-zero entries in the key columns
  ! (NLP1=3 and NLP2=4) to ensure non-zero Z entries for all sorted positions.
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do
  ! Make all entries of column NLP1=3 non-zero (these go into Z via alpha*VT(I,NLP1))
  VT(1, 3) = 0.3D0
  VT(2, 3) = 0.4D0
  VT(3, 3) = 0.5D0
  ! Make all entries of column NLP2=4 non-zero (these go into Z via beta*VT(I,NLP2))
  VT(4, 4) = 0.6D0
  VT(5, 4) = 0.7D0

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 0
  IDXQ(4) = 1
  IDXQ(5) = 2

  ! Zero output arrays
  do I = 1, N
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack matrices
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('givens_deflation')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ========================================================
  ! Test case 8: All entries deflated (tiny alpha, beta)
  ! All Z entries will be small, leading to all deflation
  ! NL=2, NR=1, SQRE=0
  ! ========================================================
  NL = 2
  NR = 1
  SQRE = 0
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  D(1) = 1.0D0
  D(2) = 2.0D0
  D(3) = 0.0D0
  D(4) = 3.0D0

  ! Tiny alpha and beta to make Z entries close to zero
  ALPHA = 1.0D-20
  BETA = 1.0D-20

  ! U = identity (4x4)
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! VT = identity (4x4)
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 0
  IDXQ(4) = 1

  ! Zero output arrays
  do I = 1, N
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack matrices
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('all_deflated')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

  ! ========================================================
  ! Test case 9: DSIGMA(2) tiny — trigger hlftol replacement
  ! Use D values where the smallest merged value is very small
  ! NL=2, NR=1, SQRE=0
  ! ========================================================
  NL = 2
  NR = 1
  SQRE = 0
  N = NL + NR + 1
  M = N + SQRE
  LDU = NMAX
  LDVT = NMAX+1
  LDU2 = NMAX
  LDVT2 = NMAX+1

  ! D values with the smallest being very close to zero
  D(1) = 1.0D-20
  D(2) = 2.0D0
  D(3) = 0.0D0
  D(4) = 3.0D0

  ALPHA = 1.0D0
  BETA = 1.0D0

  ! U = identity
  do J = 1, N
    do I = 1, N
      U(I, J) = 0.0D0
    end do
    U(J, J) = 1.0D0
  end do

  ! VT = identity
  do J = 1, M
    do I = 1, M
      VT(I, J) = 0.0D0
    end do
    VT(J, J) = 1.0D0
  end do

  IDXQ(1) = 1
  IDXQ(2) = 2
  IDXQ(3) = 0
  IDXQ(4) = 1

  ! Zero output arrays
  do I = 1, N
    Z(I) = 0.0D0
    DSIGMA(I) = 0.0D0
    IDXP(I) = 0
    IDX(I) = 0
    IDXC(I) = 0
    COLTYP(I) = 0
  end do
  do I = 1, N
    do J = 1, N
      U2(I, J) = 0.0D0
    end do
  end do
  do I = 1, M
    do J = 1, M
      VT2(I, J) = 0.0D0
    end do
  end do

  call DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, &
               U, LDU, VT, LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, &
               IDXP, IDX, IDXC, IDXQ, COLTYP, INFO )

  ! Pack matrices
  do J = 1, N
    do I = 1, N
      Upk(I + (J-1)*N) = U(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VTpk(I + (J-1)*M) = VT(I, J)
    end do
  end do
  do J = 1, N
    do I = 1, N
      U2pk(I + (J-1)*N) = U2(I, J)
    end do
  end do
  do J = 1, M
    do I = 1, M
      VT2pk(I + (J-1)*M) = VT2(I, J)
    end do
  end do

  call begin_test('tiny_dsigma2')
  call print_int('info', INFO)
  call print_int('K', K)
  call print_array('D', D, N)
  call print_array('Z', Z, N)
  call print_array('DSIGMA', DSIGMA, N)
  call print_array('U', Upk, N*N)
  call print_array('VT', VTpk, M*M)
  call print_array('U2', U2pk, N*N)
  call print_array('VT2', VT2pk, M*M)
  call print_int_array('IDXP', IDXP, N)
  call print_int_array('IDX', IDX, N)
  call print_int_array('IDXC', IDXC, N)
  call print_int_array('IDXQ', IDXQ, N)
  call print_int_array('COLTYP', COLTYP, 4)
  call end_test()

end program
