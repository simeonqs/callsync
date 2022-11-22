test_that('Start before end.', {

  wave = load.wave('files/wave_1.wav')
  ca = calc.am(wave)

  expect_true(class(ca) == 'data.frame')

})

