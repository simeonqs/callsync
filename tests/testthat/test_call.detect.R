test_that('Start before end.', {

  wave = load.wave('files/wave_1.wav')
  cd = call.detect(wave)

  expect_true(cd$end > cd$start)

})

