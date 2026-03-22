program test_zlartg
  use test_utils
  implicit none
  complex*16 :: f, g, s, r
  double precision :: c
  double precision :: s_r(2), r_r(2)
  equivalence (s, s_r)
  equivalence (r, r_r)

  ! Test 1: g=0 => c=1, s=0, r=f
  f = (3.0d0, 4.0d0)
  g = (0.0d0, 0.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_g_zero')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 2: f=0 => c=0, r is real
  f = (0.0d0, 0.0d0)
  g = (3.0d0, 4.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_f_zero')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 3: both real
  f = (3.0d0, 0.0d0)
  g = (4.0d0, 0.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_real')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 4: general complex case
  f = (1.0d0, 2.0d0)
  g = (3.0d0, 4.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_general')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 5: f and g both purely imaginary
  f = (0.0d0, 2.0d0)
  g = (0.0d0, 3.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_pure_imag')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 6: f=0, g purely imaginary
  f = (0.0d0, 0.0d0)
  g = (0.0d0, 5.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_f_zero_g_imag')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 7: f=0, g purely real
  f = (0.0d0, 0.0d0)
  g = (5.0d0, 0.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_f_zero_g_real')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 8: very large f, normal g => triggers scaled algorithm
  f = (1.0d200, 1.0d200)
  g = (1.0d0, 1.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_large_f')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 9: very small f, normal g => triggers scaled algorithm, f not well-scaled
  f = (1.0d-200, 1.0d-200)
  g = (1.0d0, 1.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_small_f')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 10: both very large => scaled algorithm, same scale
  f = (3.0d200, 4.0d200)
  g = (1.0d200, 2.0d200)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_both_large')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 11: normal f, very large g => scaled algorithm
  f = (1.0d0, 1.0d0)
  g = (1.0d200, 1.0d200)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_large_g')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 12: f=0, very large g (scaled f=0 path)
  f = (0.0d0, 0.0d0)
  g = (1.0d200, 2.0d200)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_f_zero_g_large')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 13: very small f, very small g => scaled
  f = (3.0d-200, 4.0d-200)
  g = (1.0d-200, 2.0d-200)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_both_small')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 14: f large, g very small => tests f2 >= h2*SAFMIN in scaled
  f = (1.0d200, 0.0d0)
  g = (1.0d-200, 0.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_large_f_small_g')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 15: f very small, g=0 (edge: g=0 with tiny f)
  f = (1.0d-300, 1.0d-300)
  g = (0.0d0, 0.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_tiny_f_g_zero')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 16: f=0, g very small (tests scaled f=0 path with small g)
  f = (0.0d0, 0.0d0)
  g = (1.0d-200, 2.0d-200)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_f_zero_g_small')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 17: unscaled, f2 < h2*SAFMIN (tiny f, moderate g, both in range)
  ! f very small relative to g, triggering f2 < h2*SAFMIN in unscaled path
  f = (1.0d-54, 0.0d0)
  g = (1.0d100, 0.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_unscaled_tiny_f')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 18: unscaled, f2 <= rtmin (f2 small enough to hit else branch of f2>rtmin)
  ! f1 > rtmin=sqrt(SAFMIN)~1.49e-154 but f2=f1^2 < rtmin
  ! Use f=(1e-100, 0), g=(1,0) => f2=1e-200. rtmin=1.49e-154. f2 < rtmin.
  ! f1=1e-100 > rtmin, g1=1 < rtmax. Both in normal range.
  ! Also f2=1e-200, h2=1+1e-200~=1, h2*SAFMIN=2.22e-308 < f2=1e-200 => f2 >= h2*SAFMIN
  ! So this hits lines 180-185 (the else of f2>rtmin && h2<rtmax)
  f = (1.0d-100, 0.0d0)
  g = (1.0d0, 0.0d0)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_unscaled_f2_small')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 19: scaled, f very small relative to g (f2 < h2*SAFMIN in scaled path)
  ! Both large but f much smaller than g
  f = (1.0d-100, 0.0d0)
  g = (1.0d200, 1.0d200)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_scaled_tiny_f')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 20: scaled, f2 >= h2*SAFMIN but h2 >= rtmax*2 (else branch in scaled)
  ! f and g both large and similar in magnitude
  f = (5.0d152, 5.0d152)
  g = (5.0d152, 5.0d152)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_scaled_large_both')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

  ! Test 21: scaled, same scaling, f2 < h2*SAFMIN
  ! f is small enough that after same scaling, f2 < h2*SAFMIN
  ! f1 = 2e46, g1 = 1e200 => u=1e200, f1/u=2e-154 >= rtmin => same scaling
  ! fsr=2e-154, f2=4e-308, g2=2, h2=2, h2*SAFMIN=4.44e-308
  ! f2=4e-308 < 4.44e-308 => hits the f2<h2*SAFMIN branch
  f = (2.0d46, 0.0d0)
  g = (1.0d200, 1.0d200)
  call zlartg(f, g, c, s, r)
  call begin_test('zlartg_scaled_f2_lt_safmin')
  call print_scalar('c', c)
  call print_array('s', s_r, 2)
  call print_array('r', r_r, 2)
  call end_test()

end program
