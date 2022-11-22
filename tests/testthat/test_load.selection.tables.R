test_that('Class.', {

  st = load.selection.tables(path_selection_tables = 'files')

  expect_true(class(st) == 'data.frame')

})

