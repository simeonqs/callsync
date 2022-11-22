test_that('-1.', {

  s1 = c(0, 1)
  s2 = c(1)
  scc = simple.cc(s1, s2)

  expect_equal(scc, -1)

})

test_that('Normalise.', {

  s1 = c(0, 1, 2)
  s2 = c(1, 2)
  scc = simple.cc(s1, s2, norm = T)

  expect_equal(scc, -1/3)

})
