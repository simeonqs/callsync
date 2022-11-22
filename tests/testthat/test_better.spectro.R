test_that('Basics.', {

  wave = load.wave('files/wave_1.wav')
  b = better.spectro(wave = wave, ylim = c(1000, 2000), xlim = c(0, 0.2))

  expect_true(all(b$yaxt >= 1000))
  expect_true(all(b$yaxt <= 2000))
  expect_true(all(b$xaxt <= 0.2))

})

