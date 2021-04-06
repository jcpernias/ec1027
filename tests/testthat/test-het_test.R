library(ec1027)

mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)
sq_uhat <- resid(mod)^2


test_that("het_test all covariates", {
  aux <- update(mod, sq_uhat ~ .)
  F1 <- drop_test(aux)
  expect_error({F2 <- het_test(mod)}, NA)
  expect_equal(F2$statistic, F1$statistic)
  expect_equal(F2$parameter, F1$parameter)
  expect_equal(F2$p.value, F1$p.value)

  LM <- nobs(aux) * summary(aux)$r.squared
  expect_error({X2 <- het_test(mod, chisq = TRUE)}, NA)
  expect_equal(X2$statistic[[1]], LM)
  expect_equal(X2$parameter[[1]], length(coef(aux)) - 1)
})

test_that("het_test all covariates", {
  aux <- update(mod, sq_uhat ~ . - bdrms)
  F1 <- drop_test(aux)
  expect_error({F2 <- het_test(mod, ~ sqrft + lotsize)}, NA)
  expect_equal(F2$statistic, F1$statistic)
  expect_equal(F2$parameter, F1$parameter)
  expect_equal(F2$p.value, F1$p.value)
})
