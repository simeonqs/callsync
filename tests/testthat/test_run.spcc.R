test_that('Class.', {

  files = list.files('files', '*wave', full.names = T)

  waves = lapply(files, load.wave)

  spcc_out = run.spcc(waves)

  expect_true(as.character(class(spcc_out))[1] == 'matrix')

})

test_that('Diagonal 0.', {

  files = list.files('files', '*wave', full.names = T)

  waves = lapply(files, load.wave)

  spcc_out = run.spcc(waves)

  expect_true(all(diag(spcc_out) == 0))

})

test_that('Mirrored.', {

  files = list.files('files', '*wave', full.names = T)

  waves = lapply(files, load.wave)

  spcc_out = run.spcc(waves)

  expect_equal(spcc_out[upper.tri(spcc_out)], spcc_out[lower.tri(spcc_out)])

})
