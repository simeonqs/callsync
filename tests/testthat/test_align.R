all_files = list.files('files', '*chunk*', full.names = T)

test_that('Finding files.', {

  expect_true(length(all_files) != 0)

})

test_that('Class.', {

  a = align(chunk_size = 2,
            step_size = 0.1,
            all_files = all_files,
            keys_id = c('c', '@'),
            keys_rec = c('c', '@'),
            blank = 0,
            wing = 0,
            quiet = TRUE)

  expect_true(class(a) == 'list')
  expect_true(class(a[[1]]) == 'Wave')

})

test_that('Save files, a is null.', {

  td = tempdir()

  a = align(chunk_size = 2,
            step_size = 0.1,
            all_files = all_files,
            keys_id = c('c', '@'),
            keys_rec = c('c', '@'),
            blank = 0,
            wing = 0,
            quiet = TRUE,
            save_pdf = TRUE,
            path_chunks = td)

  expect_null(a)

  unlink(td)

})


test_that('Warning wing.', {

  expect_error( align(chunk_size = 1,
                      step_size = 0.1,
                      all_files = files,
                      keys_id = c('c', '@'),
                      keys_rec = c('c', '@'),
                      blank = 10,
                      wing = 15,
                      quiet = TRUE),
                'Wing cannot be greater than blank.')

})

test_that('Test log.', {

  td = tempdir()
  a = align(chunk_size = 2,
            step_size = 0.1,
            all_files = all_files,
            keys_id = c('c', '@'),
            keys_rec = c('c', '@'),
            blank = 0,
            wing = 0,
            quiet = TRUE,
            save_log = TRUE,
            path_chunks = td)
  path_out = paste(tempdir(), 'align_log.csv', sep = '/')
  expect_true(file.exists(path_out))

})
