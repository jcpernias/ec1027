library(ec1027)
library(sandwich)

mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)

test_that("OLS standard errors", {
  expect_equal(se(mod), sqrt(diag(vcov(mod))))
})

test_that("HC3 standard errors", {
  V <- vcovHC(mod, type = "HC3")
  expect_equal(se(mod, "HC"), sqrt(diag(V)))
})

test_that("function computing standard errors", {
  V <- vcovHC(mod)
  expect_equal(se(mod, vcovHC), sqrt(diag(V)))
})

test_that("passing covariance matrix", {
  V <- vcovHC(mod)
  expect_equal(se(mod, V), sqrt(diag(V)))
})

test_that("aliased coefficients OLS standard errors", {
  x <- hprice1$sqrft - hprice1$lotsize
  mod2 <- lm(price ~ sqrft + lotsize + x + bdrms, data = hprice1)
  V <- vcov(mod2)
  expect_equal(se(mod2), sqrt(diag(V)))
})

test_that("aliased coefficients HC standard errors", {
  x <- hprice1$sqrft - hprice1$lotsize
  mod2 <- lm(price ~ sqrft + lotsize + x + bdrms, data = hprice1)
  bhat <- coef(mod2)
  V <- .vcov.aliased(is.na(bhat), vcovHC(mod2, type = "HC3"))
  expect_equal(se(mod2, "HC"), sqrt(diag(V)))
})

test_that("passing invalid vce inputs", {
  expect_error(se(mod, list(1, 2)))
  expect_error(se(mod, c("HC", "HC2")))
  expect_error(se(mod, TRUE))
})


