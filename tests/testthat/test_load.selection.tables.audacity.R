test_that('Class.', {

  st = load.selection.tables.audacity(path_selection_tables = 'files/audacity')

  expect_true(class(st) == 'data.frame')

})

