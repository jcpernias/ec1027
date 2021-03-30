
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ec1027

<!-- badges: start -->

[![R-CMD-check](https://github.com/jcpernias/ec1027/workflows/R-CMD-check/badge.svg)](https://github.com/jcpernias/ec1027/actions)
[![Codecov test
coverage](https://codecov.io/gh/jcpernias/ec1027/branch/master/graph/badge.svg)](https://codecov.io/gh/jcpernias/ec1027?branch=master)
<!-- badges: end -->

This package provides datasets and some functions to be used in the
course EC1027 - Econometrics I.

## Installation

<!--
You can install the released version of ec1027 from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ec1027")
```
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jcpernias/ec1027")
```

## Datasets

The `ec1027` package provides some datasets taken from Jeffrey M.
Wooldridge (2006:) *Introductory econometrics : a modern approach*, 3rd
ed., Thomson South-Western:

-   `bwght`: Birth weight and cigarette smoking.

-   `earns`: Earnings, productivity and hours (macro).

-   `gpa1`: Gollege GPA and its predictors.

-   `hprice1`: House prices and characteristics.

-   `hseinv`: Housing investment and prices.

-   `intdef`: Interest rates and Federal budget balance.

-   `rdchem`: R & D and sales, chemical industry.

-   `traffic2`: Speed limits and highway safety.

Note that the package
[wooldridge](https://justinmshea.github.io/wooldridge/) provides access
to many more datasets. Also note that derived variables (logarithms,
lags, etc) are not included in the `ec1027` data sets.

## Example

Load the `ec1027` package and use `data` to bring one of the included
data sets to the global environment.

``` r
library(ec1027)

## data on house prices
data(hprice1)

## Show the first observations
head(hprice1)
#>     price assess bdrms lotsize sqrft colonial
#> 1 300.000  349.1     4    6126  2438        1
#> 2 370.000  351.5     3    9903  2076        1
#> 3 191.000  217.7     3    5200  1374        0
#> 4 195.000  231.8     3    4600  1448        1
#> 5 373.000  319.1     4    6095  2514        1
#> 6 466.275  414.5     5    8566  2754        1
```

## Code

### Robust standard errors

Several functions have an `vce` parameter that allows the use of
variance covariance estimators consistent in the presence of
heteroskedasticity or autocorrelation.

Output from regression models is usually examined with the `summary`
function:

``` r
mod <- lm(price ~ sqrft + bdrms + colonial, data = hprice1)

summary(mod)
#> 
#> Call:
#> lm(formula = price ~ sqrft + bdrms + colonial, data = hprice1)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -129.793  -40.167   -5.588   30.447  227.011 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -21.55241   31.21022  -0.691    0.492    
#> sqrft         0.12985    0.01395   9.310 1.41e-14 ***
#> bdrms        12.48749   10.02366   1.246    0.216    
#> colonial     13.07755   15.43591   0.847    0.399    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 63.15 on 84 degrees of freedom
#> Multiple R-squared:  0.635,  Adjusted R-squared:  0.622 
#> F-statistic: 48.72 on 3 and 84 DF,  p-value: < 2.2e-16
```

But `summary` only shows the OLS standard errors of estimates. The
`coef_table` function produces output similar to `summary` but allows
other covariance matrix estimators:

``` r
# Heteroskedasticity consistent errors
coef_table(mod, vce = "HC")
#> 
#> Call:
#> lm(price ~ sqrft + bdrms + colonial, data = hprice1)
#> 
#> Covariance matrix estimate: HC.
#> 
#> Coefficients: 
#>               Estimate Std. Error t value  Pr(>|t|)    
#> (Intercept) -21.552414  45.840628 -0.4702    0.6395    
#> sqrft         0.129849   0.021651  5.9973 4.876e-08 ***
#> bdrms        12.487493  11.018019  1.1334    0.2603    
#> colonial     13.077547  19.126077  0.6838    0.4960    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 63.1498 on 84 degrees of freedom
#> Multiple R-squared:  0.635,  Adjusted R-squared:  0.622 
#> F-statistic: 16.75 on 3 and 84 DF,  p-value: 1.2893e-08
```

Also, the `se` function computes standard errors robust to
heteroskedasticity and autocorrelation. See more details in the
documentation of this function.

### Heteroskedasticity tests

The function `white_test` computes White’s test for heteroskedasticity:

``` r
white_test(mod)
#> 
#>  White's test for heteroskedasticity
#> 
#> data:  Auxiliary regression of squared residuals from:
#>   lm(price ~ sqrft + bdrms + colonial, data = hprice1)
#> on all covariates, their squares and cross-products.
#> 
#> 
#> F = 2.6942, df1 = 8, df2 = 79, p-value = 0.01128
```

The function `het_test` computes LM tests for heteroskedasticity that
allow the user to specify the variables related to hetroskedasticity.

``` r
# Using all covariates
het_test(mod)
#> 
#>  LM test for heteroskedasticity
#> 
#> data:  Auxiliary regression of squared residuals from:
#>   lm(price ~ sqrft + bdrms + colonial, data = hprice1)
#> on all covariates.
#> 
#> 
#> F = 4.7726, df1 = 3, df2 = 84, p-value = 0.004035

# Using only sqrft and colonial
het_test(mod, ~ sqrft + colonial)
#> 
#>  LM test for heteroskedasticity
#> 
#> data:  Auxiliary regression of squared residuals from:
#>   lm(price ~ sqrft + bdrms + colonial, data = hprice1)
#> on: ~sqrft + colonial.
#> 
#> 
#> F = 5.9392, df1 = 2, df2 = 85, p-value = 0.003852
```

### Hypotheses tests

The `drop_test` function performs a Wald test of the null joint
hypotheses that the parameters of some variables are 0. The `vce`
argument allows the use of alternative covariance matrix estimators.

``` r
# Joint significant test with the OLS covariance matrix estimator
drop_test(mod)
#> 
#>  Wald test for redundant variables
#> 
#> data:  Test for redudancy of all covariates.
#> 
#> Call:
#>   lm(price ~ sqrft + bdrms + colonial, data = hprice1)
#> 
#> 
#> F = 48.72, df1 = 3, df2 = 84, p-value < 2.2e-16

# Now using a heteroskedasticity consistent covariance matrix estimator
drop_test(mod, vce = "HC")
#> 
#>  Wald test for redundant variables
#> 
#> data:  Test for redudancy of all covariates.
#> 
#> Call:
#>   lm(price ~ sqrft + bdrms + colonial, data = hprice1)
#> 
#> Covariance matrix estimate: HC.
#> 
#> 
#> F = 16.749, df1 = 3, df2 = 84, p-value = 1.289e-08

# Heteroskedasticity robust rest test on the joint signifinace of 
# bdrms and colonial
drop_test(mod, ~ bdrms + colonial, vce = "HC")
#> 
#>  Wald test for redundant variables
#> 
#> data:  Test for redudancy of ~bdrms + colonial.
#> 
#> Call:
#>   lm(price ~ sqrft + bdrms + colonial, data = hprice1)
#> 
#> Covariance matrix estimate: HC.
#> 
#> 
#> F = 1.3769, df1 = 2, df2 = 84, p-value = 0.258
```
