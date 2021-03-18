#' hprice1
#'
#' Collected from the real estate pages of the Boston Globe during 1990.
#' These homes sold in the Boston, MA area.
#'
#' @section Notes:
#' Typically, it is very easy to obtain data on selling prices and
#' characteristics of homes, using publicly available databases.  It is
#' interesting to match the information on houses with other
#' information – such as local crime rates, quality of the local schools,
#' pollution levels, and so on – and estimate the effects of such variables
#' on housing prices.
#'
#' @usage data("hprice1")
#'
#' @format A data frame with 88 observations.
#' \describe{
#'   \item{price}{house price, $1000s}
#'   \item{assess}{assessed value, $1000s}
#'   \item{bdrms}{number of bedrooms}
#'   \item{lotsize}{size of lot in square feet}
#'   \item{sqrft}{size of house in square feet}
#'   \item{colonial}{=1 if home is colonial style}
#' }
#' @source Wooldrige (2009) \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"hprice1"

#' traffic2
#'
#' Data used in: P.S. McCarthy (1994), “Relaxed Speed Limits and Highway
#' Safety: New Evidence from California,” \emph{Economics Letters}
#' \strong{46}, 173--179. Professor McCarthy kindly provided the data.
#'
#' @section Notes:
#' Many states have changed maximum speed limits and imposed seat belt laws
#' over the past 25 years. Data similar to those in \code{traffic2} should be
#' fairly easy to obtain for a particular state. One should combine this
#' information with changes in a state’s blood alcohol limit and the passage
#' of per se and open container laws.
#'
#' @usage data("traffic2")
#'
#' @format A data frame with 108 observations.
#' \describe{
#'   \item{year}{1981 to 1989}
#'   \item{month}{1 to 12}
#'   \item{totacc}{statewide total accidents}
#'   \item{fatacc}{statewide fatal accidents}
#'   \item{injacc}{statewide injury accidents}
#'   \item{pdoacc}{property damage only accidents}
#'   \item{ntotacc}{noninterstate total acc.}
#'   \item{nfatacc}{noninterstate fatal acc.}
#'   \item{ninjacc}{noninterstate injur acc.}
#'   \item{npdoacc}{noninterstate property acc.}
#'   \item{rtotacc}{total acc. on rural 65 mph roads}
#'   \item{rfatacc}{fatal acc. on rural 65 mph roads}
#'   \item{rinjacc}{injury acc. on rural 65 mph roads}
#'   \item{rpdoacc}{property acc. on rural 65 mph roads}
#'   \item{ushigh}{acc. on U.S. highways}
#'   \item{cntyrds}{acc. on county roads}
#'   \item{strtes}{acc. on state routes}
#'   \item{unem}{state unemployment rate}
#'   \item{spdlaw}{=1 after 65 mph in effect}
#'   \item{beltlaw}{=1 after seatbelt law}
#'   \item{wkends}{number of weekend days in month}
#' }
#' @source Wooldrige (2009) \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"traffic2"

