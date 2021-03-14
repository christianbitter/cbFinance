test_that("max drawdown", {
  log_returns <- c(100, 102, 98, 100, 97, 95, 99, 100, 102, 103);
  mdd <- cbFinance::max_drawdown(log_returns);
  expect_true(mdd == 7);
})

test_that("longest loss period", {
  log_returns <- c(100, 102, 98, 100, 97, 95, 99, 100, 102, 103);
  llp <- cbFinance::longest_loss_period(log_returns);
  expect_true(llp[3] == -4);
})