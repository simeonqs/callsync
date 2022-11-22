test_that('Class.', {

  files = list.files('files', '*chunk*', full.names = T)

  a = align(chunk_size = 1,
            step_size = 0.1,
            all_files = files,
            keys_id = c('c', '@'),
            keys_rec = c('c', '@'),
            blank = 0,
            wing = 0,
            quiet = TRUE)

  expect_true(class(a) == 'list')
  expect_true(class(a[[1]]) == 'Wave')

})


