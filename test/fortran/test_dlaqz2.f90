program test_dlaqz2
  use test_utils
  implicit none

  integer, parameter :: N = 8
  double precision :: A(N, N), B(N, N), Q(N, N), Z(N, N)
  integer :: k, istartm, istopm, ihi, i, j

  ! ---- Test 1: normal branch, k=1, ilq=true, ilz=true ----
  call init_pencil(A, B, N)
  ! Introduce a 2x2 bulge at (k+1:k+2, k:k+1) with k=1 -> rows 2..3, cols 1..2
  A(3, 1) = 0.4d0
  A(3, 2) = 0.3d0
  B(2, 1) = 0.5d0
  B(3, 1) = 0.7d0
  B(3, 2) = 0.6d0
  call eye(Q, N)
  call eye(Z, N)
  k = 1
  istartm = 1
  istopm = N
  ihi = N
  call DLAQZ2(.TRUE., .TRUE., k, istartm, istopm, ihi, A, N, B, N, &
              N, 1, Q, N, N, 1, Z, N)
  call begin_test('normal_k1_qz')
  call print_matrix('A', A, N, N, N)
  call print_matrix('B', B, N, N, N)
  call print_matrix('Q', Q, N, N, N)
  call print_matrix('Z', Z, N, N, N)
  call end_test()

  ! ---- Test 2: normal branch, k=2, ilq=false, ilz=false ----
  call init_pencil(A, B, N)
  A(4, 2) = 0.35d0
  A(4, 3) = 0.25d0
  B(3, 2) = 0.45d0
  B(4, 2) = 0.55d0
  B(4, 3) = 0.5d0
  call eye(Q, N)
  call eye(Z, N)
  k = 2
  istartm = 1
  istopm = N
  ihi = N
  call DLAQZ2(.FALSE., .FALSE., k, istartm, istopm, ihi, A, N, B, N, &
              N, 1, Q, N, N, 1, Z, N)
  call begin_test('normal_k2_noqz')
  call print_matrix('A', A, N, N, N)
  call print_matrix('B', B, N, N, N)
  call print_matrix('Q', Q, N, N, N)
  call print_matrix('Z', Z, N, N, N)
  call end_test()

  ! ---- Test 3: edge branch, k+2 == ihi ----
  ! Pick k=3, ihi=5. Bulge rows k+1..k+2=4..5, cols k..k+1=3..4
  call init_pencil(A, B, N)
  A(5, 3) = 0.35d0
  A(5, 4) = 0.3d0
  B(4, 3) = 0.4d0
  B(5, 3) = 0.6d0
  B(5, 4) = 0.45d0
  call eye(Q, N)
  call eye(Z, N)
  k = 3
  istartm = 1
  istopm = N
  ihi = 5
  call DLAQZ2(.TRUE., .TRUE., k, istartm, istopm, ihi, A, N, B, N, &
              N, 1, Q, N, N, 1, Z, N)
  call begin_test('edge_k3_ihi5')
  call print_matrix('A', A, N, N, N)
  call print_matrix('B', B, N, N, N)
  call print_matrix('Q', Q, N, N, N)
  call print_matrix('Z', Z, N, N, N)
  call end_test()

  ! ---- Test 4: normal branch, narrow window (istartm, istopm) ----
  call init_pencil(A, B, N)
  A(3, 1) = 0.4d0
  A(3, 2) = 0.3d0
  B(2, 1) = 0.5d0
  B(3, 1) = 0.7d0
  B(3, 2) = 0.6d0
  call eye(Q, N)
  call eye(Z, N)
  k = 1
  istartm = 1
  istopm = 5
  ihi = N
  call DLAQZ2(.TRUE., .FALSE., k, istartm, istopm, ihi, A, N, B, N, &
              N, 1, Q, N, N, 1, Z, N)
  call begin_test('normal_narrow_qonly')
  call print_matrix('A', A, N, N, N)
  call print_matrix('B', B, N, N, N)
  call print_matrix('Q', Q, N, N, N)
  call print_matrix('Z', Z, N, N, N)
  call end_test()

contains

  subroutine init_pencil(A, B, N)
    integer, intent(in) :: N
    double precision, intent(out) :: A(N, N), B(N, N)
    integer :: i, j
    ! Hessenberg A
    do j = 1, N
      do i = 1, N
        if (i .le. j + 1) then
          A(i, j) = 1.0d0 + 0.1d0 * (i + 2 * j) + 0.03d0 * i * j
        else
          A(i, j) = 0.0d0
        end if
      end do
    end do
    ! Upper-triangular B
    do j = 1, N
      do i = 1, N
        if (i .le. j) then
          B(i, j) = 2.0d0 + 0.2d0 * (j - i) + 0.05d0 * j
        else
          B(i, j) = 0.0d0
        end if
      end do
    end do
  end subroutine

  subroutine eye(M, N)
    integer, intent(in) :: N
    double precision, intent(out) :: M(N, N)
    integer :: i, j
    do j = 1, N
      do i = 1, N
        if (i .eq. j) then
          M(i, j) = 1.0d0
        else
          M(i, j) = 0.0d0
        end if
      end do
    end do
  end subroutine

end program test_dlaqz2
