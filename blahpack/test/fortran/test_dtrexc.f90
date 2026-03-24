program test_dtrexc
  use test_utils
  implicit none

  integer, parameter :: N = 4
  double precision :: T(N,N), Q(N,N), WORK(N)
  integer :: IFST, ILST, INFO
  integer :: i

  ! Test 1: swap two 1x1 blocks
  T = 0.0d0
  T(1,1) = 4.0d0; T(1,2) = 1.0d0; T(1,3) = 0.5d0; T(1,4) = 0.2d0
  T(2,2) = 3.0d0; T(2,3) = 0.8d0; T(2,4) = 0.3d0
  T(3,3) = 2.0d0; T(3,4) = 0.6d0
  T(4,4) = 1.0d0

  Q = 0.0d0
  do i = 1, N
    Q(i,i) = 1.0d0
  end do

  IFST = 1
  ILST = 4
  call DTREXC('V', N, T, N, Q, N, IFST, ILST, WORK, INFO)

  call begin_test('swap 1x1 forward')
  call print_int('info', INFO)
  call print_int('ifst', IFST)
  call print_int('ilst', ILST)
  call print_array('T', T, N*N)
  call print_array('Q', Q, N*N)
  call end_test()

  ! Test 2: N=1 quick return
  call DTREXC('V', 1, T, 1, Q, 1, 1, 1, WORK, INFO)
  call begin_test('N=1 quick return')
  call print_int('info', INFO)
  call end_test()

end program
