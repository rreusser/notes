program test_dlasv2
  use test_utils
  implicit none
  double precision :: f, g, h, ssmin, ssmax, snr, csr, snl, csl

  ! Test 1: Diagonal matrix [3 0; 0 4] — g=0, no rotation needed
  f = 3.0d0; g = 0.0d0; h = 4.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('diagonal_no_swap')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 2: Diagonal matrix [4 0; 0 3] — g=0, swap needed (H < F)
  f = 4.0d0; g = 0.0d0; h = 3.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('diagonal_swap')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 3: Identity matrix [1 0; 0 1]
  f = 1.0d0; g = 0.0d0; h = 1.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('identity')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 4: Upper triangular [3 4; 0 0] — h=0
  f = 3.0d0; g = 4.0d0; h = 0.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('h_zero')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 5: [0 0; 0 0] — all zeros
  f = 0.0d0; g = 0.0d0; h = 0.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('all_zero')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 6: f=0, g nonzero, h nonzero — [0 5; 0 3]
  f = 0.0d0; g = 5.0d0; h = 3.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('f_zero')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 7: General case [1 2; 0 3]
  f = 1.0d0; g = 2.0d0; h = 3.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('general_1_2_3')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 8: Negative values [−2 1; 0 −3]
  f = -2.0d0; g = 1.0d0; h = -3.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('negative_vals')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 9: Very large GA — triggers gasmal=false path [1 1e20; 0 1]
  f = 1.0d0; g = 1.0d20; h = 1.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('very_large_g')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 10: Very large GA with HA > 1 — [1 1e20; 0 2]
  f = 1.0d0; g = 1.0d20; h = 2.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('very_large_g_ha_gt_1')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 11: Swap path with general — [1 2; 0 5] — h > f triggers swap
  f = 1.0d0; g = 2.0d0; h = 5.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('swap_general')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 12: Negative f, positive h — [-3 4; 0 5]
  f = -3.0d0; g = 4.0d0; h = 5.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('neg_f_pos_h')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 13: Negative g — [2 -3; 0 1]
  f = 2.0d0; g = -3.0d0; h = 1.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('neg_g')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 14: f=0, g=0, h nonzero — [0 0; 0 5]
  f = 0.0d0; g = 0.0d0; h = 5.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('f_g_zero')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 15: f nonzero, g=0, h=0 — [5 0; 0 0]
  f = 5.0d0; g = 0.0d0; h = 0.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('h_g_zero')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 16: pmax=2 path — g is largest, [0.5 10; 0 0.5]
  f = 0.5d0; g = 10.0d0; h = 0.5d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('pmax_2')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 17: d=fa case (ha very small) — [10 1; 0 1e-320]
  f = 10.0d0; g = 1.0d0; h = 1.0d-320
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('ha_very_small')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 18: mm=0, l!=0 path — [2 0; 0 1] with tiny g (triggers mm=0, l!=0 -> SIGN(D,FT)+m/t)
  ! Actually need g tiny but nonzero, and fa != ha so l != 0
  f = 2.0d0; g = 1.0d-300; h = 1.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('mm_zero_l_nonzero')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 19: mm=0, l=0 path — [2 0; 0 2] with tiny g (triggers mm=0, l=0 -> SIGN(2,ft)*SIGN(1,gt))
  ! Need fa=ha so l=0, and m very tiny so mm=0
  f = 2.0d0; g = 1.0d-300; h = 2.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('mm_zero_l_zero')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 20: mm=0, l=0, negative ft and gt
  f = -2.0d0; g = -1.0d-300; h = 2.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('mm_zero_l_zero_neg')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 21: All negative [−1 −2; 0 −3]
  f = -1.0d0; g = -2.0d0; h = -3.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('all_negative')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 22: Swap with very large g — [1 1e20; 0 5] triggers swap + pmax=2
  f = 1.0d0; g = 1.0d20; h = 5.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('swap_very_large_g')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 23: [0 1; 0 0] — f=0, h=0, g nonzero
  f = 0.0d0; g = 1.0d0; h = 0.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('f_h_zero_g_nonzero')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 24: [1 1; 0 1] — equal diagonal
  f = 1.0d0; g = 1.0d0; h = 1.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('equal_diagonal')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

  ! Test 25: gasmal=false with ha>1 after swap — [2 1e20; 0 100]
  ! After swap: ft=100, ha=2 (>1), ga=1e20, fa/ga < eps
  f = 2.0d0; g = 1.0d20; h = 100.0d0
  call dlasv2(f, g, h, ssmin, ssmax, snr, csr, snl, csl)
  call begin_test('gasmal_false_ha_gt_1')
  call print_scalar('ssmin', ssmin)
  call print_scalar('ssmax', ssmax)
  call print_scalar('snr', snr)
  call print_scalar('csr', csr)
  call print_scalar('snl', snl)
  call print_scalar('csl', csl)
  call end_test()

end program
