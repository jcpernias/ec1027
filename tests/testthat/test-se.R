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
