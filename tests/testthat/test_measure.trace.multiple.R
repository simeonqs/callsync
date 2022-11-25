test_that('Basics.', {

  files = list.files('files', '*wave_*', full.names = T)
  waves = lapply(files, load.wave)
  new_waves = waves
  detections = lapply(waves, call.detect)

  traces = lapply(waves, trace.fund)
  mt = measure.trace.multiple(traces = traces, waves = waves, new_waves = new_waves, detections = detections)

  expect_equal(class(mt), 'data.frame')
  expect_true(mt$mean_fund_hz[1] > 1)

})
