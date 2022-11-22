test_that('Class.', {

  d = load.selection.tables.audacity(path_selection_tables = 'files/audacity')
  gt = load.selection.tables(path_selection_tables = 'files')

  out = calc.perf(d, gt)

  expect_equal(class(out), 'list')

})

