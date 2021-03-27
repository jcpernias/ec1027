library(ec1027)
library(sandwich)

mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)

test_that("coef_table works", {
  expect_error(coef_table(mod), NA)
})

test_that("coef_table with vce from sandwich packge", {
  expect_error(coef_table(mod, vce = vcovHC), NA)
})

test_that("coef_table with vce as a string", {
  expect_error(coef_table(mod, vce = "HC"), NA)
})

test_that("coef_table with invalid strings for vce", {
  expect_error(coef_table(mod, vce = c("HC", "HC3")))
  expect_error(coef_table(mod, vce = "Potato"))
})

test_that("coef_table with invalid vce", {
  expect_error(coef_table(mod, vce = list(1, 2)))
  expect_error(coef_table(mod, vce = TRUE))
})
