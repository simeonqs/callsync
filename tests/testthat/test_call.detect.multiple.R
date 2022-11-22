test_that('Start before end.', {

  wave = load.wave('files/wave_1.wav')
  cd = call.detect.multiple(wave)

  expect_true(all(cd$end > cd$start))

})

