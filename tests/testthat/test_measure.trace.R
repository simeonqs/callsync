test_that('Basics.', {

  trace = data.frame(time = 1:10, fund = 1:10, missing = rep(T, 10))
  mt = measure.trace(trace)

  expect_equal(mt$mean_fund_hz, 5.5)
  expect_equal(mt$duration, 10)
  expect_equal(mt$prop_missing_trace, 1)

})

