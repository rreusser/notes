program test_zgetri
  use test_utils
  implicit none

  call test_3x3()
  call test_4x4()
  call test_n1()
  call test_3x3_pivots()

contains

  subroutine test_3x3()
    complex*16 :: a(3, 3), work(256), aorig(3, 3), c(3, 3)
    double precision :: a_r(18), c_r(18)
    equivalence (a, a_r)
    equivalence (c, c_r)
    integer :: ipiv(3), info, i, j, k
    a = (0.0d0, 0.0d0)
    a(1,1) = (2.0d0, 1.0d0)
    a(2,1) = (4.0d0, 2.0d0)
    a(3,1) = (8.0d0, 0.0d0)
    a(1,2) = (1.0d0, 0.0d0)
    a(2,2) = (3.0d0, 1.0d0)
    a(3,2) = (7.0d0, 1.0d0)
    a(1,3) = (1.0d0, 0.5d0)
    a(2,3) = (3.0d0, 0.0d0)
    a(3,3) = (9.0d0, 2.0d0)
    aorig = a
    call zgetrf(3, 3, a, 3, ipiv, info)
    call begin_test('3x3_factor')
    call print_int('info', info)
    call end_test()
    call zgetri(3, a, 3, ipiv, work, 256, info)
    call begin_test('3x3_inverse')
    call print_int('info', info)
    call print_array('a', a_r, 18)
    c = (0.0d0, 0.0d0)
    do j = 1, 3
      do i = 1, 3
        do k = 1, 3
          c(i,j) = c(i,j) + aorig(i,k) * a(k,j)
        end do
      end do
    end do
    call print_array('product', c_r, 18)
    call end_test()
  end subroutine

  subroutine test_4x4()
    complex*16 :: a(4, 4), work(256), aorig(4, 4), c(4, 4)
    double precision :: a_r(32), c_r(32)
    equivalence (a, a_r)
    equivalence (c, c_r)
    integer :: ipiv(4), info, i, j, k
    a = (0.0d0, 0.0d0)
    a(1,1) = (5.0d0, 1.0d0)
    a(2,1) = (1.0d0, 0.5d0)
    a(3,1) = (0.5d0, 0.0d0)
    a(4,1) = (0.0d0, 0.5d0)
    a(1,2) = (1.0d0, -0.5d0)
    a(2,2) = (5.0d0, 2.0d0)
    a(3,2) = (1.0d0, 1.0d0)
    a(4,2) = (0.5d0, 0.0d0)
    a(1,3) = (0.5d0, 0.0d0)
    a(2,3) = (1.0d0, -1.0d0)
    a(3,3) = (5.0d0, 0.0d0)
    a(4,3) = (1.0d0, 0.5d0)
    a(1,4) = (0.0d0, -0.5d0)
    a(2,4) = (0.5d0, 0.0d0)
    a(3,4) = (1.0d0, -0.5d0)
    a(4,4) = (5.0d0, 1.0d0)
    aorig = a
    call zgetrf(4, 4, a, 4, ipiv, info)
    call begin_test('4x4_factor')
    call print_int('info', info)
    call end_test()
    call zgetri(4, a, 4, ipiv, work, 256, info)
    call begin_test('4x4_inverse')
    call print_int('info', info)
    call print_array('a', a_r, 32)
    c = (0.0d0, 0.0d0)
    do j = 1, 4
      do i = 1, 4
        do k = 1, 4
          c(i,j) = c(i,j) + aorig(i,k) * a(k,j)
        end do
      end do
    end do
    call print_array('product', c_r, 32)
    call end_test()
  end subroutine

  subroutine test_n1()
    complex*16 :: a(1, 1), work(4)
    double precision :: a_r(2)
    equivalence (a, a_r)
    integer :: ipiv(1), info
    a(1,1) = (3.0d0, 4.0d0)
    call zgetrf(1, 1, a, 1, ipiv, info)
    call zgetri(1, a, 1, ipiv, work, 4, info)
    call begin_test('n1')
    call print_int('info', info)
    call print_array('a', a_r, 2)
    call end_test()
  end subroutine

  subroutine test_3x3_pivots()
    complex*16 :: a(3, 3), work(256), aorig(3, 3), c(3, 3)
    double precision :: a_r(18), c_r(18)
    equivalence (a, a_r)
    equivalence (c, c_r)
    integer :: ipiv(3), info, i, j, k
    a = (0.0d0, 0.0d0)
    a(1,1) = (1.0d0, 0.0d0)
    a(2,1) = (4.0d0, 1.0d0)
    a(3,1) = (7.0d0, 2.0d0)
    a(1,2) = (2.0d0, 1.0d0)
    a(2,2) = (5.0d0, 0.0d0)
    a(3,2) = (8.0d0, 1.0d0)
    a(1,3) = (3.0d0, 0.0d0)
    a(2,3) = (6.0d0, 2.0d0)
    a(3,3) = (0.0d0, 1.0d0)
    aorig = a
    call zgetrf(3, 3, a, 3, ipiv, info)
    call begin_test('3x3_pivots_factor')
    call print_int('info', info)
    call end_test()
    call zgetri(3, a, 3, ipiv, work, 256, info)
    call begin_test('3x3_pivots_inverse')
    call print_int('info', info)
    call print_array('a', a_r, 18)
    c = (0.0d0, 0.0d0)
    do j = 1, 3
      do i = 1, 3
        do k = 1, 3
          c(i,j) = c(i,j) + aorig(i,k) * a(k,j)
        end do
      end do
    end do
    call print_array('product', c_r, 18)
    call end_test()
  end subroutine

end program
