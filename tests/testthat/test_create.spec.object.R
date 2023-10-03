wave = load.wave('files/wave_1.wav')

test_that('Basics.', {

  out = create.spec.object(wave = wave, plot_it = FALSE)

  expect_true(nrow(out) == 255)
  expect_true(ncol(out) == 102)
  expect_true(is.matrix(out))


})
