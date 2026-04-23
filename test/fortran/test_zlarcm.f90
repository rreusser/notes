program test_zlarcm
  use test_utils
  implicit none

  ! Test 1: 3x3 real A times 3x2 complex B
  call test_basic()

  ! Test 2: M=N=1
  call test_one()

  ! Test 3: 4x4 real A times 4x3 complex B
  call test_4x3()

  ! Test 4: M=2, N=4 (rectangular B)
  call test_2x4()

contains

  subroutine test_basic()
    integer, parameter :: m = 3, n = 2
    double precision :: a(m, m)
    complex*16 :: b(m, n), c(m, n)
    double precision :: rwork(2*m*n)
    double precision :: a_flat(m*m)
    double precision :: b_flat(2*m*n)
    double precision :: c_flat(2*m*n)
    equivalence (b, b_flat)
    equivalence (c, c_flat)
    integer :: i, j

    a = reshape([ &
      1.0d0, 2.0d0, 3.0d0, &
      4.0d0, 5.0d0, 6.0d0, &
      7.0d0, 8.0d0, 9.5d0  &
    ], [m, m])
    b(1,1) = (1.0d0, 0.5d0)
    b(2,1) = (2.0d0, -1.0d0)
    b(3,1) = (-3.0d0, 2.0d0)
    b(1,2) = (0.0d0, 1.0d0)
    b(2,2) = (4.0d0, -2.0d0)
    b(3,2) = (1.5d0, 3.0d0)
    c = (0.0d0, 0.0d0)

    call zlarcm(m, n, a, m, b, m, c, m, rwork)

    do j = 1, m
      do i = 1, m
        a_flat(i + (j-1)*m) = a(i, j)
      end do
    end do

    call begin_test('zlarcm_basic')
    call print_int('m', m)
    call print_int('n', n)
    call print_array('a', a_flat, m*m)
    call print_array('b', b_flat, 2*m*n)
    call print_array('c', c_flat, 2*m*n)
    call end_test()
  end subroutine test_basic

  subroutine test_one()
    integer, parameter :: m = 1, n = 1
    double precision :: a(m, m)
    complex*16 :: b(m, n), c(m, n)
    double precision :: rwork(2*m*n)
    double precision :: a_flat(m*m)
    double precision :: b_flat(2*m*n)
    double precision :: c_flat(2*m*n)
    equivalence (b, b_flat)
    equivalence (c, c_flat)

    a(1,1) = 2.5d0
    b(1,1) = (3.0d0, -4.0d0)
    c = (0.0d0, 0.0d0)

    call zlarcm(m, n, a, m, b, m, c, m, rwork)

    a_flat(1) = a(1,1)

    call begin_test('zlarcm_one')
    call print_int('m', m)
    call print_int('n', n)
    call print_array('a', a_flat, m*m)
    call print_array('b', b_flat, 2*m*n)
    call print_array('c', c_flat, 2*m*n)
    call end_test()
  end subroutine test_one

  subroutine test_4x3()
    integer, parameter :: m = 4, n = 3
    double precision :: a(m, m)
    complex*16 :: b(m, n), c(m, n)
    double precision :: rwork(2*m*n)
    double precision :: a_flat(m*m)
    double precision :: b_flat(2*m*n)
    double precision :: c_flat(2*m*n)
    equivalence (b, b_flat)
    equivalence (c, c_flat)
    integer :: i, j

    do j = 1, m
      do i = 1, m
        a(i, j) = sin(real(i + 2*j, kind=8))
      end do
    end do
    do j = 1, n
      do i = 1, m
        b(i, j) = cmplx(cos(real(i + j, kind=8)), sin(real(i*j, kind=8)), kind=8)
      end do
    end do
    c = (0.0d0, 0.0d0)

    call zlarcm(m, n, a, m, b, m, c, m, rwork)

    do j = 1, m
      do i = 1, m
        a_flat(i + (j-1)*m) = a(i, j)
      end do
    end do

    call begin_test('zlarcm_4x3')
    call print_int('m', m)
    call print_int('n', n)
    call print_array('a', a_flat, m*m)
    call print_array('b', b_flat, 2*m*n)
    call print_array('c', c_flat, 2*m*n)
    call end_test()
  end subroutine test_4x3

  subroutine test_2x4()
    integer, parameter :: m = 2, n = 4
    double precision :: a(m, m)
    complex*16 :: b(m, n), c(m, n)
    double precision :: rwork(2*m*n)
    double precision :: a_flat(m*m)
    double precision :: b_flat(2*m*n)
    double precision :: c_flat(2*m*n)
    equivalence (b, b_flat)
    equivalence (c, c_flat)
    integer :: i, j

    a(1,1) = 1.0d0
    a(2,1) = -2.0d0
    a(1,2) = 0.5d0
    a(2,2) = 3.0d0
    do j = 1, n
      do i = 1, m
        b(i, j) = cmplx(real(i+j, kind=8), real(i-j, kind=8), kind=8)
      end do
    end do
    c = (0.0d0, 0.0d0)

    call zlarcm(m, n, a, m, b, m, c, m, rwork)

    do j = 1, m
      do i = 1, m
        a_flat(i + (j-1)*m) = a(i, j)
      end do
    end do

    call begin_test('zlarcm_2x4')
    call print_int('m', m)
    call print_int('n', n)
    call print_array('a', a_flat, m*m)
    call print_array('b', b_flat, 2*m*n)
    call print_array('c', c_flat, 2*m*n)
    call end_test()
  end subroutine test_2x4

end program test_zlarcm
