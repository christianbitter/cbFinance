test_that("simple compounding", {
  p <- compound(100, .05, 2, simple = T);
  expect_equal(p, 110);
})

test_that("exponential compounding", {
  p <- compound(100, .05, 2, simple = F);
  expect_equal(p, 110.25);
})
