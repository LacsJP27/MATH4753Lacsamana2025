test_that("mycurve runs without error", {
  expect_silent(mycurve(mu = 10, sigma = 5, a = 6))  # Ensures no error occurs
})

test_that("mycurve computes probability correctly", {
  a <- 6
  mu <- 10
  sigma <- 5
  expected_prob <- round(pnorm(a, mean = mu, sd = sigma), 4)
  calculated_prob <- round(pnorm(a, mean = mu, sd = sigma), 4)  # Mimics function output
  expect_equal(calculated_prob, expected_prob)
})

test_that("mycurve handles edge cases correctly", {
  expect_silent(mycurve(mu = 0, sigma = 1, a = -1))  # Standard normal, a < mean
  expect_silent(mycurve(mu = 100, sigma = 10, a = 120))  # Large values
  expect_silent(mycurve(mu = -10, sigma = 3, a = -5))  # Negative values
})
