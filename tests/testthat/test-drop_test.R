library(ec1027)
library(sandwich)

mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)
b <- coef(mod)[-1]
V <- vcov(mod)[-1, -1]
df1 <- length(b)
df2 <- df.residual(mod)
Xstat <- sum(b * solve(V, b))
Fstat <- Xstat / df1

test_that("joint significance test: F version", {
  expect_error({f <- drop_test(mod)}, NA)
  expect_s3_class(f, "htest")
  expect_equal(f$statistic[[1]], Fstat)
  expect_equal(f$parameter, c(df1 = df1, df2 = df2))
  expect_equal(f$p.value, pf(Fstat, df1 = df1, df2 = df2, lower.tail = FALSE))
})

test_that("joint significance test: X-squared version", {
  expect_error({X <- drop_test(mod, chisq = TRUE)}, NA)
  expect_s3_class(X, "htest")
  expect_equal(X$statistic[[1]], Xstat)
  expect_equal(X$parameter, c(df = df1))
  expect_equal(X$p.value, pchisq(Xstat, df = df1, lower.tail = FALSE))
})

test_that("joint significant test: vce is a valid string", {
  VHC <- sandwich::vcovHC(mod)[-1, -1]
  Xstat <- sum(b * solve(VHC, b))
  Fstat <- Xstat / df1
  expect_error({f <- drop_test(mod, vce = "HC")}, NA)
  expect_s3_class(f, "htest")
  expect_equal(f$statistic[[1]], Fstat)
  expect_equal(f$parameter, c(df1 = df1, df2 = df2))
  expect_equal(f$p.value, pf(Fstat, df1 = df1, df2 = df2, lower.tail = FALSE))

  expect_error({X <- drop_test(mod, chisq = TRUE, vce = "HC")}, NA)
  expect_s3_class(X, "htest")
  expect_equal(X$statistic[[1]], Xstat)
  expect_equal(X$parameter, c(df = df1))
  expect_equal(X$p.value, pchisq(Xstat, df = df1, lower.tail = FALSE))
})

test_that("joint significant test: vce is a function", {
  F1 <- drop_test(mod, vce = "HC")
  expect_error({F2 <- drop_test(mod, vce = vcovHC)}, NA)
  expect_equal(F2$statistic, F1$statistic)
  expect_equal(F2$parameter, F1$parameter)
  expect_equal(F2$p.value, F1$p.value)
})

test_that("joint significant test (hccm matrix)", {
  hccm <- vcovHC(mod)
  expect_s3_class(drop_test(mod, vce = hccm), "htest")
})

test_that("significance of a subset of variables", {
  expect_s3_class(drop_test(mod, ~ sqrft + lotsize), "htest")
})

test_that("significance of a subset of variables (hccm)", {
  expect_s3_class(drop_test(mod, ~ sqrft + lotsize, vce = "HC"), "htest")
})

test_that("joint significance and *", {
  mod2 <- lm(price ~ sqrft * lotsize + bdrms, data = hprice1)
  expect_s3_class(drop_test(mod2), "htest")
})

test_that("frml with left hand side", {
  mod2 <- lm(price ~ sqrft * lotsize + bdrms, data = hprice1)
  expect_warning({f <- drop_test(mod2, frml = y ~ bdrms)})
  expect_s3_class(f, "htest")
})

test_that("drop a logical variable", {
  col_bool <- hprice1$colonial == 1
  mod3 <- lm(price ~ sqrft + bdrms + col_bool, data = hprice1)
  expect_error({f <- drop_test(mod3, ~ col_bool)}, NA)
  expect_equal(f$parameter[[1]], 1)
})

test_that("drop a logical variable (no intercept)", {
  col_bool <- hprice1$colonial == 1
  mod3 <- lm(price ~ sqrft + bdrms + col_bool - 1, data = hprice1)
  expect_error({f <- drop_test(mod3, ~ col_bool)}, NA)
  expect_equal(f$parameter[[1]], 2)
})

