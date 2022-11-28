all_files = list.files('files', '*chunk*', full.names = T)

test_that('Finding files.', {

  expect_true(!is.null(all_files))

})

test_that('Class and end > start.', {

  daa = detect.and.assign(all_files = all_files,
                          quiet = TRUE,
                          save_files = FALSE)

  expect_true(class(daa) == 'data.frame')
  expect_true(all(daa$end > daa$start))

})
