test_that('Class.', {

  wave = load.wave('files/wave_1.wav')

  trace = trace.fund(wave = wave)

  expect_equal(class(trace), 'data.frame')

})

test_that('Columns.', {

  wave = load.wave('files/wave_1.wav')

  trace = trace.fund(wave = wave)

  expect_equal(colnames(trace), c('time', 'fund', 'missing'))

})

test_that('Time increasing in equal steps.', {

  wave = load.wave('files/wave_1.wav')

  trace = trace.fund(wave = wave)
  d = diff(trace$time)
  dd = diff(d)

  expect_true(all(d > 0))
  expect_true(all(dd < 1e-6))

})

test_that('Fund within limits', {

  wave = load.wave('files/wave_1.wav')

  trace = trace.fund(wave = wave, freq_lim = c(1, 2))

  expect_true(all(trace$fund >= 1000))
  expect_true(all(trace$fund <= 2000))

})
