library(ec1027)
library(sandwich)

mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)

test_that("joint significance test", {
  expect_s3_class(drop_test(mod), "htest")
})

test_that("joint significant test (hccm)", {
  expect_s3_class(drop_test(mod, vce = vcovHC), "htest")
})

test_that("significance of a subset of variables", {
  expect_s3_class(drop_test(mod, ~ sqrft + lotsize), "htest")
})

test_that("significance of a subset of variables (hccm)", {
  expect_s3_class(drop_test(mod, ~ sqrft + lotsize, vce = vcovHC), "htest")
})
