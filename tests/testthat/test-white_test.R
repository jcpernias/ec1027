library(ec1027)

mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)
sq_uhat <- resid(mod)^2

white1 <-  ~ sqrft + lotsize + bdrms +
  I(sqrft^2) + I(lotsize^2) + I(bdrms^2)

white2 <- update(white1,  ~ . + sqrft * lotsize + sqrft * bdrms +
                  lotsize * bdrms)

test_that("white_test full", {
  F1 <- het_test(mod, white2)
  expect_error({F2 <- white_test(mod)}, NA)
  expect_equal(F2$statistic, F1$statistic)
  expect_equal(F2$parameter, F1$parameter)
  expect_equal(F2$p.value, F1$p.value)

  X1 <- het_test(mod, white2, chisq = TRUE)
  expect_error({X2 <- white_test(mod, chisq = TRUE)}, NA)
  expect_equal(X2$statistic, X1$statistic)
  expect_equal(X2$parameter, X1$parameter)
})

test_that("white_test no cross-products", {
  F1 <- het_test(mod, white1)
  expect_error({F2 <- white_test(mod, full = FALSE)}, NA)
  expect_equal(F2$statistic, F1$statistic)
  expect_equal(F2$parameter, F1$parameter)
  expect_equal(F2$p.value, F1$p.value)

  X1 <- het_test(mod, white1, chisq = TRUE)
  expect_error({X2 <- white_test(mod,  full = FALSE, chisq = TRUE)}, NA)
  expect_equal(X2$statistic, X1$statistic)
  expect_equal(X2$parameter, X1$parameter)
})
