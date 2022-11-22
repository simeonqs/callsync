test_that('Symmmetric.', {

  m = matrix(1:9, nrow = 3, ncol = 3)
  o = m[lower.tri(m)]

  m_new <- o.to.m(o)

  expect_equal(m_new[lower.tri(m_new)], m_new[upper.tri(m_new)])

})

test_that('Is matrix.', {

  m = matrix(1:9, nrow = 3, ncol = 3)
  o = m[lower.tri(m)]

  m_new <- o.to.m(o)

  expect_true(class(m_new)[1] == 'matrix')

})

test_that('Diagonal 0.', {

  m = matrix(1:9, nrow = 3, ncol = 3)
  o = m[lower.tri(m)]

  m_new <- o.to.m(o)

  expect_equal(as.numeric(diag(m_new)), rep(0, 3))

})
