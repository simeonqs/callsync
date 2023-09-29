wave_1 = load.wave('files/wave_1.wav')
wave_2 = load.wave('files/wave_2.wav')

test_that('Basics.', {

  so_1 = create.spec.object(wave = wave_1, plot_it = FALSE)
  so_2 = create.spec.object(wave = wave_2, plot_it = FALSE)
  out = sliding.pixel.comparison(so_1, so_2)

  expect_true(is.numeric(out))

})
