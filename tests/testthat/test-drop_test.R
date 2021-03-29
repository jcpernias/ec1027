library(ec1027)
library(sandwich)

mod <- lm(price ~ sqrft + lotsize + bdrms, data = hprice1)

test_that("joint significance test", {
  expect_s3_class(drop_test(mod), "htest")
})

test_that("joint significant test (hccm)", {
  expect_s3_class(drop_test(mod, vce = "HC"), "htest")
})

test_that("joint significant test (pkg sandwich hccm)", {
  expect_s3_class(drop_test(mod, vce = vcovHC), "htest")
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
  expect_warning({F <- drop_test(mod2, frml = y ~ bdrms)})
  expect_s3_class(F, "htest")
})

