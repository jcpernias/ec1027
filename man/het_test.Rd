% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/het_test.R
\name{het_test}
\alias{het_test}
\title{LM test for heteroskedasticity}
\usage{
het_test(model, frml = NULL, chisq = FALSE)
}
\arguments{
\item{model}{an estimated model returned by \code{lm} or similar functions.}

\item{frml}{an one-sided with the regressors of the auxiliary regression.
If \code{NULL} all of the model regressors are used in the auxiliary regression.}

\item{chisq}{if \code{TRUE} the LM statistic of the auxiliary regression is
returned. By default is \code{FALSE} and the F statistic is computed.}
}
\value{
An object of class \code{htest} with components:
\describe{
\item{statistic}{the value of the test statistic.}
\item{p.value}{the p-value of the test.}
\item{parameter}{degrees of freedom.}
\item{method}{a character string indicating what type of test was
performed.}
\item{data.name}{a character string describing the model.}
}
}
\description{
Fit an auxiliary regression of the squared MCO residual on the covariates
specified in \code{frml}.
}
\details{
By default, the test is the F statistic of the auxiliary regression. The LM
version of the test is the product of the umber of observations and the
R-squared and it is distributed as a \eqn{\chi^2} under the null hypothesis of
homoskedasticity.
}
\examples{
data("hprice1")

mod <- lm(price ~ sqrft + bdrms, data = hprice1)

# Test using all regressorn in mod
het_test(mod)

# Using sqrft only
het_test(mod, ~ sqrft)

}
