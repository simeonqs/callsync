files = list.files('files', '*wave_*', full.names = T)
waves = lapply(files, load.wave)
new_waves = waves
detections = lapply(waves, call.detect)
traces = lapply(waves, trace.fund)

test_that('Basics.', {

  mt = measure.trace.multiple(traces = traces, waves = waves, new_waves = new_waves, detections = detections)

  expect_equal(class(mt), 'data.frame')
  expect_true(mt$mean_fund_hz[1] > 1)
  expect_equal(names(mt), c('mean_fund_hz', 'duration_s', 'band_hz', 'max_freq_hz', 'min_freq_hz',
                            'diff_start_mean', 'diff_end_mean', 'ipi_s', 'fm_hz', 'sd_trace',
                            'prop_missing_trace', 'file', 'start', 'end', 'signal_to_noise'))

})

test_that('Test without waves.', {

  mt = measure.trace.multiple(traces = traces, sr = waves[[1]]@samp.rate)

  expect_equal(mt$file, rep(NA, 2))
  expect_equal(mt$start, rep(NA, 2))
  expect_equal(mt$end, rep(NA, 2))
  expect_equal(mt$signal_to_noise, rep(NA, 2))

})

test_that('Test without waves and sr - warning.', {

  expect_warning(measure.trace.multiple(traces = traces),
                 regexp = 'Sample rate set to 1. This will make the duration meaningless.
              Supply `sr` for meaningfull durations.')

})

test_that('PDF.', {

  path_pdf = tempfile(fileext = '.pdf')

  mt = measure.trace.multiple(traces = traces, waves = waves, new_waves = new_waves, detections = detections,
                              path_pdf = path_pdf)

  expect_equal(class(mt), 'data.frame')

  unlink(path_pdf)

})

