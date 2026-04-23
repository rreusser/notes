program test_ztfttr
  use test_utils
  implicit none
  integer, parameter :: NMAX = 8
  complex*16 :: ARF(NMAX*(NMAX+1)/2), A(NMAX, NMAX)
  double precision :: ARF_r(2*NMAX*(NMAX+1)/2)
  equivalence (ARF, ARF_r)
  ! Packed output array for printing A without LDA stride issues
  complex*16 :: Apk(NMAX*NMAX)
  double precision :: Apk_r(2*NMAX*NMAX)
  equivalence (Apk, Apk_r)
  integer :: INFO, i, j, N, NT

  ! =============================================
  ! Edge cases
  ! =============================================

  ! Test 1: N=0 quick return
  A = (0.0d0, 0.0d0)
  call ztfttr('N', 'L', 0, ARF, A, 1, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 2: N=1 quick return (normal)
  ARF(1) = (42.0d0, 7.0d0)
  A = (0.0d0, 0.0d0)
  call ztfttr('N', 'L', 1, ARF, A, NMAX, INFO)
  call begin_test('n1_N')
  call print_int('info', INFO)
  Apk(1) = A(1,1)
  call print_array('A', Apk_r, 2)
  call end_test()

  ! Test 3: N=1 quick return (conjugate-transpose)
  ARF(1) = (42.0d0, 7.0d0)
  A = (0.0d0, 0.0d0)
  call ztfttr('C', 'L', 1, ARF, A, NMAX, INFO)
  call begin_test('n1_C')
  call print_int('info', INFO)
  Apk(1) = A(1,1)
  call print_array('A', Apk_r, 2)
  call end_test()

  ! =============================================
  ! N=5 (odd) — all 4 combinations
  ! Use LDA=NMAX to match actual array layout
  ! =============================================
  N = 5
  NT = N*(N+1)/2  ! = 15

  ! Fill ARF with distinct complex values
  do i = 1, NT
    ARF(i) = dcmplx(dble(i), dble(i)*0.1d0)
  end do

  ! Test 4: N=5, TRANSR='N', UPLO='L' (odd, normal, lower)
  A = (0.0d0, 0.0d0)
  call ztfttr('N', 'L', N, ARF, A, NMAX, INFO)
  call begin_test('n5_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', Apk_r, 2*N*N)
  call end_test()

  ! Test 5: N=5, TRANSR='N', UPLO='U' (odd, normal, upper)
  A = (0.0d0, 0.0d0)
  call ztfttr('N', 'U', N, ARF, A, NMAX, INFO)
  call begin_test('n5_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', Apk_r, 2*N*N)
  call end_test()

  ! Test 6: N=5, TRANSR='C', UPLO='L' (odd, conj-trans, lower)
  A = (0.0d0, 0.0d0)
  call ztfttr('C', 'L', N, ARF, A, NMAX, INFO)
  call begin_test('n5_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', Apk_r, 2*N*N)
  call end_test()

  ! Test 7: N=5, TRANSR='C', UPLO='U' (odd, conj-trans, upper)
  A = (0.0d0, 0.0d0)
  call ztfttr('C', 'U', N, ARF, A, NMAX, INFO)
  call begin_test('n5_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', Apk_r, 2*N*N)
  call end_test()

  ! =============================================
  ! N=6 (even) — all 4 combinations
  ! Use LDA=NMAX to match actual array layout
  ! =============================================
  N = 6
  NT = N*(N+1)/2  ! = 21

  ! Fill ARF with distinct complex values
  do i = 1, NT
    ARF(i) = dcmplx(dble(i)*0.5d0, dble(i)*(-0.3d0))
  end do

  ! Test 8: N=6, TRANSR='N', UPLO='L' (even, normal, lower)
  A = (0.0d0, 0.0d0)
  call ztfttr('N', 'L', N, ARF, A, NMAX, INFO)
  call begin_test('n6_N_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', Apk_r, 2*N*N)
  call end_test()

  ! Test 9: N=6, TRANSR='N', UPLO='U' (even, normal, upper)
  A = (0.0d0, 0.0d0)
  call ztfttr('N', 'U', N, ARF, A, NMAX, INFO)
  call begin_test('n6_N_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', Apk_r, 2*N*N)
  call end_test()

  ! Test 10: N=6, TRANSR='C', UPLO='L' (even, conj-trans, lower)
  A = (0.0d0, 0.0d0)
  call ztfttr('C', 'L', N, ARF, A, NMAX, INFO)
  call begin_test('n6_C_L')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', Apk_r, 2*N*N)
  call end_test()

  ! Test 11: N=6, TRANSR='C', UPLO='U' (even, conj-trans, upper)
  A = (0.0d0, 0.0d0)
  call ztfttr('C', 'U', N, ARF, A, NMAX, INFO)
  call begin_test('n6_C_U')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('ARF', ARF_r, 2*NT)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  call print_array('A', Apk_r, 2*N*N)
  call end_test()

end program
