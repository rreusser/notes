program test_zunm22
  use test_utils
  implicit none

  ! Q is block 2x2 banded structure:
  !   Q(1:N1, 1:N2)             : N1 x N2 rectangular
  !   Q(1:N1, N2+1:N2+N1)       : N1 x N1 LOWER triangular
  !   Q(N1+1:N1+N2, 1:N2)       : N2 x N2 UPPER triangular
  !   Q(N1+1:N1+N2, N2+1:N2+N1) : N2 x N1 rectangular

  complex*16 :: Q(8, 8), C(8, 8), WORK(200)
  double precision :: C_r(16, 8)
  equivalence (C, C_r)
  integer :: info, i, j, LWORK
  integer :: M, N, N1, N2, LDQ, LDC

  LWORK = 200
  LDQ = 8
  LDC = 8

  ! ===== Setup: N1=3, N2=2, NQ=5 =====
  Q = (0.0d0, 0.0d0)
  N1 = 3
  N2 = 2
  M = 5

  ! Q(1:3, 1:2): N1 x N2 rectangular block
  Q(1,1) = (0.5d0, 0.1d0); Q(1,2) = (-0.3d0, 0.2d0)
  Q(2,1) = (0.2d0, -0.1d0); Q(2,2) = (0.8d0, 0.05d0)
  Q(3,1) = (-0.4d0, 0.15d0); Q(3,2) = (0.1d0, -0.2d0)
  ! Q(1:3, 3:5): N1 x N1 lower-triangular block
  Q(1,3) = (1.1d0, 0.3d0)
  Q(2,3) = (0.7d0, -0.2d0);  Q(2,4) = (0.9d0, 0.4d0)
  Q(3,3) = (-0.5d0, 0.1d0); Q(3,4) = (0.4d0, 0.0d0); Q(3,5) = (1.2d0, -0.1d0)
  ! Q(4:5, 1:2): N2 x N2 upper-triangular block
  Q(4,1) = (0.6d0, -0.3d0);  Q(4,2) = (-0.2d0, 0.5d0)
                             Q(5,2) = (1.3d0, 0.1d0)
  ! Q(4:5, 3:5): N2 x N1 rectangular block
  Q(4,3) = (0.3d0, 0.2d0); Q(4,4) = (-0.1d0, -0.4d0); Q(4,5) = (0.5d0, 0.1d0)
  Q(5,3) = (-0.7d0, 0.3d0); Q(5,4) = (0.4d0, 0.2d0); Q(5,5) = (0.2d0, -0.3d0)

  ! ===== Test 1: LEFT / NoTrans, M=NQ=5, N=4 =====
  N = 4
  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i,j) = dcmplx(dble(i - 2 + j) * 0.25d0 + 0.1d0, dble(i + j) * 0.1d0)
    end do
  end do
  call zunm22('L', 'N', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_cmatrix('c', C_r, LDC, M, N)
  call end_test()

  ! ===== Test 2: LEFT / ConjTrans =====
  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i,j) = dcmplx(dble(i - 2 + j) * 0.25d0 + 0.1d0, dble(i + j) * 0.1d0)
    end do
  end do
  call zunm22('L', 'C', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('left_conjtrans')
  call print_int('info', info)
  call print_cmatrix('c', C_r, LDC, M, N)
  call end_test()

  ! ===== Test 3: RIGHT / NoTrans, M=4, N=NQ=5 =====
  M = 4
  N = 5
  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i,j) = dcmplx(dble(i - 2 + j) * 0.25d0 + 0.1d0, dble(i + j) * 0.1d0)
    end do
  end do
  call zunm22('R', 'N', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_cmatrix('c', C_r, LDC, M, N)
  call end_test()

  ! ===== Test 4: RIGHT / ConjTrans =====
  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i,j) = dcmplx(dble(i - 2 + j) * 0.25d0 + 0.1d0, dble(i + j) * 0.1d0)
    end do
  end do
  call zunm22('R', 'C', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('right_conjtrans')
  call print_int('info', info)
  call print_cmatrix('c', C_r, LDC, M, N)
  call end_test()

  ! ===== Test 5: N1=0 LEFT NoTrans (pure upper triangular Q) =====
  Q = (0.0d0, 0.0d0)
  N1 = 0
  N2 = 3
  M = 3
  N = 4
  Q(1,1) = (1.0d0, 0.2d0); Q(1,2) = (0.5d0, -0.1d0); Q(1,3) = (-0.2d0, 0.3d0)
                           Q(2,2) = (0.8d0, 0.0d0); Q(2,3) = (0.3d0, 0.2d0)
                                                    Q(3,3) = (1.2d0, -0.4d0)
  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i,j) = dcmplx(dble(i + j) * 0.3d0, dble(i - j) * 0.15d0)
    end do
  end do
  call zunm22('L', 'N', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('n1_zero_left_notrans')
  call print_int('info', info)
  call print_cmatrix('c', C_r, LDC, M, N)
  call end_test()

  ! ===== Test 6: N2=0 RIGHT ConjTrans (pure lower triangular Q) =====
  Q = (0.0d0, 0.0d0)
  N1 = 3
  N2 = 0
  M = 4
  N = 3
  Q(1,1) = (1.0d0, 0.1d0)
  Q(2,1) = (0.4d0, 0.2d0); Q(2,2) = (0.9d0, -0.1d0)
  Q(3,1) = (-0.3d0, 0.25d0); Q(3,2) = (0.6d0, 0.3d0); Q(3,3) = (1.1d0, -0.2d0)
  C = (0.0d0, 0.0d0)
  do j = 1, N
    do i = 1, M
      C(i,j) = dcmplx(dble(i - j) * 0.2d0 + 0.5d0, dble(i + j) * 0.1d0)
    end do
  end do
  call zunm22('R', 'C', M, N, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('n2_zero_right_conjtrans')
  call print_int('info', info)
  call print_cmatrix('c', C_r, LDC, M, N)
  call end_test()

  ! ===== Test 7: M=0 quick return =====
  call zunm22('L', 'N', 0, 4, 0, 0, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 8: N=0 quick return =====
  N1 = 3
  N2 = 2
  call zunm22('L', 'N', 5, 0, N1, N2, Q, LDQ, C, LDC, WORK, LWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

contains
  subroutine print_cmatrix(name, arr, lda_val, m_val, n_val)
    character(*), intent(in) :: name
    integer, intent(in) :: lda_val, m_val, n_val
    double precision, intent(in) :: arr(2*lda_val, *)
    integer :: i, j
    logical :: first
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    first = .true.
    do j = 1, n_val
      do i = 1, m_val
        if (.not. first) write(*, '(A)', advance='no') ','
        first = .false.
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+1, j)
        write(*, '(A)', advance='no') ','
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+2, j)
      end do
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

end program
