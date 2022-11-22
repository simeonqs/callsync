test_that('Class and end > start.', {

  all_files = list.files('tests/testthat/files', '*chunk*', full.names = T)

  daa = detect.and.assign(all_files = all_files,
                          quiet = TRUE,
                          save_files = FALSE)

  expect_true(class(daa) == 'data.frame')
  expect_true(all(daa$end > daa$start))

})


