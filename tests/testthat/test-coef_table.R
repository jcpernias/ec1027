library(ec1027)
library(sandwich)

mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)

silently <- function(expr) {
  capture.output({result <- expr})
  invisible(result)
}

test_that("coef_table works", {
  expect_error(silently(coef_table(mod)), NA)
})

test_that("coef_table with vce from sandwich packge", {
  expect_error(silently(coef_table(mod, vce = vcovHC)), NA)
})

test_that("coef_table with vce as a string", {
  expect_error(silently(coef_table(mod, vce = "HC")), NA)
})

test_that("coef_table with invalid strings for vce", {
  expect_error(silently(coef_table(mod, vce = c("HC", "HC3"))))
  expect_error(silently(coef_table(mod, vce = "Potato")))
})

test_that("coef_table with invalid vce", {
  expect_error(silently(coef_table(mod, vce = list(1, 2))))
  expect_error(silently(coef_table(mod, vce = TRUE)))
})

test_that("coef_table with a model without intercept", {
  mod2 <- update(mod, . ~ . - 1)
  expect_error(silently(coef_table(mod2)), NA)
})
