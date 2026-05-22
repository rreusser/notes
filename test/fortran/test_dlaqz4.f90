program test_dlaqz4
  use test_utils
  implicit none

  integer, parameter :: NMAX = 12
  integer, parameter :: LWMAX = NMAX * NMAX
  double precision :: A(NMAX, NMAX), B(NMAX, NMAX)
  double precision :: Q(NMAX, NMAX), Z(NMAX, NMAX)
  double precision :: QC(NMAX, NMAX), ZC(NMAX, NMAX)
  double precision :: SR(NMAX), SI(NMAX), SS(NMAX)
  double precision :: WORK(LWMAX)
  integer :: N, ilo, ihi, nshifts, nblock_desired, info
  integer :: i, j

  ! ============================================================
  ! Test 1: Basic 8x8 sweep, ILSCHUR=T, ILQ=T, ILZ=T, NSHIFTS=2
  ! ============================================================
  N = 8
  ilo = 1
  ihi = 8
  nshifts = 2
  nblock_desired = 4
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  SR(1) = 1.5d0;  SI(1) = 0.3d0;  SS(1) = 1.0d0
  SR(2) = 1.5d0;  SI(2) = -0.3d0; SS(2) = 1.0d0
  call DLAQZ4(.TRUE., .TRUE., .TRUE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('basic_8x8_ns2')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call print_matrix('Q', Q, NMAX, N, N)
  call print_matrix('Z', Z, NMAX, N, N)
  call end_test()

  ! ============================================================
  ! Test 2: 10x10 sweep, NSHIFTS=4 (multi-shift), all updates on
  ! ============================================================
  N = 10
  ilo = 1
  ihi = 10
  nshifts = 4
  nblock_desired = 6
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  SR(1) = 2.0d0;  SI(1) = 0.5d0;  SS(1) = 1.0d0
  SR(2) = 2.0d0;  SI(2) = -0.5d0; SS(2) = 1.0d0
  SR(3) = 1.0d0;  SI(3) = 0.2d0;  SS(3) = 1.0d0
  SR(4) = 1.0d0;  SI(4) = -0.2d0; SS(4) = 1.0d0
  call DLAQZ4(.TRUE., .TRUE., .TRUE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('multi_10x10_ns4')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call print_matrix('Q', Q, NMAX, N, N)
  call print_matrix('Z', Z, NMAX, N, N)
  call end_test()

  ! ============================================================
  ! Test 3: ILSCHUR=F, ILQ=F, ILZ=F, partial window ilo=2, ihi=7
  ! ============================================================
  N = 8
  ilo = 2
  ihi = 7
  nshifts = 2
  nblock_desired = 4
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  SR(1) = 1.2d0;  SI(1) = 0.4d0;  SS(1) = 1.0d0
  SR(2) = 1.2d0;  SI(2) = -0.4d0; SS(2) = 1.0d0
  call DLAQZ4(.FALSE., .FALSE., .FALSE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('partial_window_no_updates')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call print_matrix('Q', Q, NMAX, N, N)
  call print_matrix('Z', Z, NMAX, N, N)
  call end_test()

  ! ============================================================
  ! Test 4: NSHIFTS=0 (quick return)
  ! ============================================================
  N = 6
  ilo = 1
  ihi = 6
  nshifts = 0
  nblock_desired = 4
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  call DLAQZ4(.TRUE., .TRUE., .TRUE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('nshifts_0_noop')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call print_matrix('Q', Q, NMAX, N, N)
  call print_matrix('Z', Z, NMAX, N, N)
  call end_test()

  ! ============================================================
  ! Test 5: ILO >= IHI (quick return)
  ! ============================================================
  N = 6
  ilo = 4
  ihi = 4
  nshifts = 2
  nblock_desired = 4
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  SR(1) = 1.0d0;  SI(1) = 0.1d0;  SS(1) = 1.0d0
  SR(2) = 1.0d0;  SI(2) = -0.1d0; SS(2) = 1.0d0
  call DLAQZ4(.TRUE., .TRUE., .TRUE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('ilo_eq_ihi_noop')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call end_test()

  ! ============================================================
  ! Test 6: Odd NSHIFTS=3 (gets reduced to 2)
  ! ============================================================
  N = 8
  ilo = 1
  ihi = 8
  nshifts = 3
  nblock_desired = 4
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  ! Triple of shifts: complex pair + lone real. As paired triples per
  ! the shuffle loop. Use SI(I).NE.-SI(I+1) check across (1,2): (0.5,-0.5) paired.
  SR(1) = 2.0d0;  SI(1) = 0.5d0;  SS(1) = 1.0d0
  SR(2) = 2.0d0;  SI(2) = -0.5d0; SS(2) = 1.0d0
  SR(3) = 1.0d0;  SI(3) = 0.0d0;  SS(3) = 1.0d0
  call DLAQZ4(.TRUE., .TRUE., .TRUE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('odd_nshifts_3')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call print_matrix('Q', Q, NMAX, N, N)
  call print_matrix('Z', Z, NMAX, N, N)
  call end_test()

  ! ============================================================
  ! Test 7: Shift shuffle path triggered. NSHIFTS=4, with the first
  ! pair NOT being a complex-conjugate pair so the shuffle kicks in
  ! at I=1.
  ! ============================================================
  N = 8
  ilo = 1
  ihi = 8
  nshifts = 4
  nblock_desired = 5
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  ! SI(1) != -SI(2) -> shuffle. After shuffle, the pair (SR(2),SR(3)) ends
  ! up where the original (SR(1),SR(2)) was.
  SR(1) = 1.0d0;  SI(1) = 0.0d0;  SS(1) = 1.0d0
  SR(2) = 1.5d0;  SI(2) = 0.4d0;  SS(2) = 1.0d0
  SR(3) = 1.5d0;  SI(3) = -0.4d0; SS(3) = 1.0d0
  SR(4) = 0.5d0;  SI(4) = 0.0d0;  SS(4) = 1.0d0
  call DLAQZ4(.TRUE., .TRUE., .TRUE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('shuffle_path_ns4')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call print_matrix('Q', Q, NMAX, N, N)
  call print_matrix('Z', Z, NMAX, N, N)
  call print_array('SR', SR, nshifts)
  call print_array('SI', SI, nshifts)
  call end_test()

  ! ============================================================
  ! Test 8a: ILSCHUR=T but ILO>1, IHI<N — exercises the
  ! "update A(istartm:ilo-1)" and "update A(...,ihi+1:istopm)"
  ! branches when (ISTARTM, ISTOPM) extend beyond [ILO, IHI].
  ! ============================================================
  N = 10
  ilo = 3
  ihi = 8
  nshifts = 2
  nblock_desired = 4
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  SR(1) = 1.2d0;  SI(1) = 0.4d0;  SS(1) = 1.0d0
  SR(2) = 1.2d0;  SI(2) = -0.4d0; SS(2) = 1.0d0
  call DLAQZ4(.TRUE., .TRUE., .TRUE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('ilschur_inner_window')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call print_matrix('Q', Q, NMAX, N, N)
  call print_matrix('Z', Z, NMAX, N, N)
  call end_test()

  ! ============================================================
  ! Test 8: 12x12 sweep, NSHIFTS=4, larger npos chase
  ! ============================================================
  N = 12
  ilo = 1
  ihi = 12
  nshifts = 4
  nblock_desired = 8
  call init_pencil(A, B, N)
  call eye(Q, N)
  call eye(Z, N)
  call eye(QC, N)
  call eye(ZC, N)
  SR(1) = 3.0d0;  SI(1) = 0.7d0;  SS(1) = 1.0d0
  SR(2) = 3.0d0;  SI(2) = -0.7d0; SS(2) = 1.0d0
  SR(3) = 1.5d0;  SI(3) = 0.0d0;  SS(3) = 1.0d0
  SR(4) = 0.8d0;  SI(4) = 0.0d0;  SS(4) = 1.0d0
  call DLAQZ4(.TRUE., .TRUE., .TRUE., N, ilo, ihi, nshifts, &
              nblock_desired, SR, SI, SS, A, NMAX, B, NMAX, &
              Q, NMAX, Z, NMAX, QC, NMAX, ZC, NMAX, &
              WORK, LWMAX, info)
  call begin_test('large_12x12_ns4')
  call print_int('info', info)
  call print_matrix('A', A, NMAX, N, N)
  call print_matrix('B', B, NMAX, N, N)
  call print_matrix('Q', Q, NMAX, N, N)
  call print_matrix('Z', Z, NMAX, N, N)
  call end_test()

contains

  subroutine init_pencil(A, B, N)
    integer, intent(in) :: N
    double precision, intent(out) :: A(NMAX, NMAX), B(NMAX, NMAX)
    integer :: i, j
    ! Upper-Hessenberg A (subdiagonal nonzero, zeros below)
    do j = 1, NMAX
      do i = 1, NMAX
        A(i, j) = 0.0d0
        B(i, j) = 0.0d0
      end do
    end do
    do j = 1, N
      do i = 1, N
        if (i .le. j + 1) then
          A(i, j) = 1.0d0 + 0.1d0 * dble(i + 2 * j) + 0.03d0 * dble(i * j)
        end if
      end do
    end do
    ! Upper-triangular B (strictly above diag + diag)
    do j = 1, N
      do i = 1, N
        if (i .le. j) then
          B(i, j) = 2.0d0 + 0.2d0 * dble(j - i) + 0.05d0 * dble(j)
        end if
      end do
    end do
  end subroutine

  subroutine eye(M, N)
    integer, intent(in) :: N
    double precision, intent(out) :: M(NMAX, NMAX)
    integer :: i, j
    do j = 1, NMAX
      do i = 1, NMAX
        M(i, j) = 0.0d0
      end do
    end do
    do i = 1, N
      M(i, i) = 1.0d0
    end do
  end subroutine

end program test_dlaqz4
