program test_dlasdt
  use test_utils
  implicit none

  integer :: N, LVL, ND, MSUB
  integer :: INODE(100), NDIML(100), NDIMR(100)

  ! Test case 1: N=15, MSUB=6 (basic test)
  N = 15
  MSUB = 6
  call DLASDT(N, LVL, ND, INODE, NDIML, NDIMR, MSUB)
  call begin_test('n15_msub6')
  call print_int('N', N)
  call print_int('MSUB', MSUB)
  call print_int('LVL', LVL)
  call print_int('ND', ND)
  call print_int_array('INODE', INODE, ND)
  call print_int_array('NDIML', NDIML, ND)
  call print_int_array('NDIMR', NDIMR, ND)
  call end_test()

  ! Test case 2: N=1, MSUB=6 (smallest non-trivial)
  N = 1
  MSUB = 6
  INODE = 0
  NDIML = 0
  NDIMR = 0
  call DLASDT(N, LVL, ND, INODE, NDIML, NDIMR, MSUB)
  call begin_test('n1_msub6')
  call print_int('N', N)
  call print_int('MSUB', MSUB)
  call print_int('LVL', LVL)
  call print_int('ND', ND)
  call print_int_array('INODE', INODE, ND)
  call print_int_array('NDIML', NDIML, ND)
  call print_int_array('NDIMR', NDIMR, ND)
  call end_test()

  ! Test case 3: N=2, MSUB=6 (small)
  N = 2
  MSUB = 6
  INODE = 0
  NDIML = 0
  NDIMR = 0
  call DLASDT(N, LVL, ND, INODE, NDIML, NDIMR, MSUB)
  call begin_test('n2_msub6')
  call print_int('N', N)
  call print_int('MSUB', MSUB)
  call print_int('LVL', LVL)
  call print_int('ND', ND)
  call print_int_array('INODE', INODE, ND)
  call print_int_array('NDIML', NDIML, ND)
  call print_int_array('NDIMR', NDIMR, ND)
  call end_test()

  ! Test case 4: N=31, MSUB=6 (larger, more levels)
  N = 31
  MSUB = 6
  INODE = 0
  NDIML = 0
  NDIMR = 0
  call DLASDT(N, LVL, ND, INODE, NDIML, NDIMR, MSUB)
  call begin_test('n31_msub6')
  call print_int('N', N)
  call print_int('MSUB', MSUB)
  call print_int('LVL', LVL)
  call print_int('ND', ND)
  call print_int_array('INODE', INODE, ND)
  call print_int_array('NDIML', NDIML, ND)
  call print_int_array('NDIMR', NDIMR, ND)
  call end_test()

  ! Test case 5: N=63, MSUB=12 (different MSUB)
  N = 63
  MSUB = 12
  INODE = 0
  NDIML = 0
  NDIMR = 0
  call DLASDT(N, LVL, ND, INODE, NDIML, NDIMR, MSUB)
  call begin_test('n63_msub12')
  call print_int('N', N)
  call print_int('MSUB', MSUB)
  call print_int('LVL', LVL)
  call print_int('ND', ND)
  call print_int_array('INODE', INODE, ND)
  call print_int_array('NDIML', NDIML, ND)
  call print_int_array('NDIMR', NDIMR, ND)
  call end_test()

  ! Test case 6: N=7, MSUB=2 (small MSUB, forces deeper tree)
  N = 7
  MSUB = 2
  INODE = 0
  NDIML = 0
  NDIMR = 0
  call DLASDT(N, LVL, ND, INODE, NDIML, NDIMR, MSUB)
  call begin_test('n7_msub2')
  call print_int('N', N)
  call print_int('MSUB', MSUB)
  call print_int('LVL', LVL)
  call print_int('ND', ND)
  call print_int_array('INODE', INODE, ND)
  call print_int_array('NDIML', NDIML, ND)
  call print_int_array('NDIMR', NDIMR, ND)
  call end_test()

  ! Test case 7: N=100, MSUB=25 (larger problem)
  N = 100
  MSUB = 25
  INODE = 0
  NDIML = 0
  NDIMR = 0
  call DLASDT(N, LVL, ND, INODE, NDIML, NDIMR, MSUB)
  call begin_test('n100_msub25')
  call print_int('N', N)
  call print_int('MSUB', MSUB)
  call print_int('LVL', LVL)
  call print_int('ND', ND)
  call print_int_array('INODE', INODE, ND)
  call print_int_array('NDIML', NDIML, ND)
  call print_int_array('NDIMR', NDIMR, ND)
  call end_test()

end program
