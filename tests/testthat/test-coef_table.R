library(ec1027)

test_that("coef_table works", {
  mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)
  expect_error(coef_table(mod), NA)
})
