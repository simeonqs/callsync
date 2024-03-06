test_that('Exporting empty detections object.', {

  path_out = paste(tempdir(), 'detections.txt', sep = '/')
  export.detections(detections = data.frame(), path_out = path_out)
  expect_true(file.exists(path_out))

})

