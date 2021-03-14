test_that("simple returns", {
  prices <- c(10, 12, 8);
  returns <- simple_return(prices);


  expect_equal(length(returns),length(prices));
  expect_equal(returns[1], 0);
  expect_equal(returns[2], (prices[2] - prices[1]) / prices[2]);
  expect_equal(returns[3], (prices[3] - prices[2]) / prices[3]);
})
