d = load.selection.tables.audacity(path_selection_tables = 'files/audacity')

test_that('Class.', {

  out = calc.perf(d, d)

  expect_equal(class(out), 'list')

})
