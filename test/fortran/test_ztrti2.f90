program test_ztrti2
  use test_utils
  implicit none

  ! Use subroutines to allocate different-sized local arrays
  call test_upper_nonunit_3()
  call test_lower_nonunit_3()
  call test_upper_unit_3()
  call test_lower_unit_3()
  call test_n1()
  call test_upper_nonunit_4()
  call test_lower_nonunit_4()

contains

  subroutine test_upper_nonunit_3()
    complex*16 :: a(3, 3)
    double precision :: a_r(18)
    equivalence (a, a_r)
    integer :: info
    a = (0.0d0, 0.0d0)
    a(1,1) = (2.0d0, 1.0d0)
    a(1,2) = (1.0d0, 0.5d0)
    a(1,3) = (3.0d0, 1.0d0)
    a(2,2) = (4.0d0, 2.0d0)
    a(2,3) = (5.0d0, 1.0d0)
    a(3,3) = (6.0d0, 3.0d0)
    call ztrti2('U', 'N', 3, a, 3, info)
    call begin_test('upper_nonunit')
    call print_int('info', info)
    call print_array('a', a_r, 18)
    call end_test()
  end subroutine

  subroutine test_lower_nonunit_3()
    complex*16 :: a(3, 3)
    double precision :: a_r(18)
    equivalence (a, a_r)
    integer :: info
    a = (0.0d0, 0.0d0)
    a(1,1) = (2.0d0, 1.0d0)
    a(2,1) = (1.0d0, 0.5d0)
    a(3,1) = (3.0d0, 1.0d0)
    a(2,2) = (4.0d0, 2.0d0)
    a(3,2) = (5.0d0, 1.0d0)
    a(3,3) = (6.0d0, 3.0d0)
    call ztrti2('L', 'N', 3, a, 3, info)
    call begin_test('lower_nonunit')
    call print_int('info', info)
    call print_array('a', a_r, 18)
    call end_test()
  end subroutine

  subroutine test_upper_unit_3()
    complex*16 :: a(3, 3)
    double precision :: a_r(18)
    equivalence (a, a_r)
    integer :: info
    a = (0.0d0, 0.0d0)
    a(1,1) = (99.0d0, 99.0d0)
    a(1,2) = (1.0d0, 0.5d0)
    a(1,3) = (3.0d0, 1.0d0)
    a(2,2) = (99.0d0, 99.0d0)
    a(2,3) = (5.0d0, 1.0d0)
    a(3,3) = (99.0d0, 99.0d0)
    call ztrti2('U', 'U', 3, a, 3, info)
    call begin_test('upper_unit')
    call print_int('info', info)
    call print_array('a', a_r, 18)
    call end_test()
  end subroutine

  subroutine test_lower_unit_3()
    complex*16 :: a(3, 3)
    double precision :: a_r(18)
    equivalence (a, a_r)
    integer :: info
    a = (0.0d0, 0.0d0)
    a(1,1) = (99.0d0, 99.0d0)
    a(2,1) = (1.0d0, 0.5d0)
    a(3,1) = (3.0d0, 1.0d0)
    a(2,2) = (99.0d0, 99.0d0)
    a(3,2) = (5.0d0, 1.0d0)
    a(3,3) = (99.0d0, 99.0d0)
    call ztrti2('L', 'U', 3, a, 3, info)
    call begin_test('lower_unit')
    call print_int('info', info)
    call print_array('a', a_r, 18)
    call end_test()
  end subroutine

  subroutine test_n1()
    complex*16 :: a(1, 1)
    double precision :: a_r(2)
    equivalence (a, a_r)
    integer :: info
    a(1,1) = (3.0d0, 4.0d0)
    call ztrti2('U', 'N', 1, a, 1, info)
    call begin_test('n1')
    call print_int('info', info)
    call print_array('a', a_r, 2)
    call end_test()
  end subroutine

  subroutine test_upper_nonunit_4()
    complex*16 :: a(4, 4)
    double precision :: a_r(32)
    equivalence (a, a_r)
    integer :: info
    a = (0.0d0, 0.0d0)
    a(1,1) = (1.0d0, 1.0d0)
    a(1,2) = (2.0d0, 0.0d0)
    a(1,3) = (3.0d0, 1.0d0)
    a(1,4) = (4.0d0, 2.0d0)
    a(2,2) = (5.0d0, 1.0d0)
    a(2,3) = (6.0d0, 0.0d0)
    a(2,4) = (7.0d0, 3.0d0)
    a(3,3) = (8.0d0, 2.0d0)
    a(3,4) = (9.0d0, 1.0d0)
    a(4,4) = (10.0d0, 0.0d0)
    call ztrti2('U', 'N', 4, a, 4, info)
    call begin_test('upper_nonunit_4')
    call print_int('info', info)
    call print_array('a', a_r, 32)
    call end_test()
  end subroutine

  subroutine test_lower_nonunit_4()
    complex*16 :: a(4, 4)
    double precision :: a_r(32)
    equivalence (a, a_r)
    integer :: info
    a = (0.0d0, 0.0d0)
    a(1,1) = (1.0d0, 1.0d0)
    a(2,1) = (2.0d0, 0.0d0)
    a(3,1) = (3.0d0, 1.0d0)
    a(4,1) = (4.0d0, 2.0d0)
    a(2,2) = (5.0d0, 1.0d0)
    a(3,2) = (6.0d0, 0.0d0)
    a(4,2) = (7.0d0, 3.0d0)
    a(3,3) = (8.0d0, 2.0d0)
    a(4,3) = (9.0d0, 1.0d0)
    a(4,4) = (10.0d0, 0.0d0)
    call ztrti2('L', 'N', 4, a, 4, info)
    call begin_test('lower_nonunit_4')
    call print_int('info', info)
    call print_array('a', a_r, 32)
    call end_test()
  end subroutine

end program
