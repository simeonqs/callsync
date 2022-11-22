test_that('Class and end > start.', {

  all_files = list.files('files', '*chunk*', full.names = T)

  # Detect calls in each chunk
  detections = lapply(all_files, function(file){
    wave = load.wave(file, ffilter_from = 1100)
    detections = call.detect.multiple(wave, plot_it = F)
    return(detections)
  })
  names(detections) = basename(all_files)

  ca = call.assign(all_files = all_files,
                   detections = detections,
                   quiet = TRUE,
                   save_files = FALSE)

  expect_true(class(ca) == 'data.frame')
  expect_true(all(ca$end > ca$start))

})


