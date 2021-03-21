#' earns
#'
#' Collected from the \emph{Economic Report of the President}, 1989, Table B-47.
#' The data are for the non-farm business sector.
#'
#' @section Notes:
#'These data could be usefully updated, but changes in reporting conventions
#'in more recent ERPs may make that difficult.
#'
#' @usage data("earns")
#'
#' @format A data frame with 41 observations.
#' \describe{
#'   \item{year}{1947 to 1987}
#'   \item{wkearns}{avg. real weekly earnings}
#'   \item{wkhours}{avg. weekly hours}
#'   \item{outphr}{output per labor hour}
#' }
#' @source Wooldrige (2009): \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"earns"

#' gpa1
#'
#' Christopher Lemmon, a former MSU undergraduate, collected these data
#' from a survey he took of MSU students in Fall 1994.
#'
#' @section Notes:
#' This is a nice example of how students can obtain an original data set
#' by focusing locally and carefully composing a survey.
#'
#' @usage data("gpa1")
#'
#' @format A data frame with 141 observations.
#' \describe{
#'   \item{age}{in years}
#'   \item{soph}{=1 if sophomore}
#'   \item{junior}{=1 if junior}
#'   \item{senior}{=1 if senior}
#'   \item{senior5}{=1 if fifth year senior}
#'   \item{male}{=1 if male}
#'   \item{campus}{=1 if live on campus}
#'   \item{business}{=1 if business major}
#'   \item{engineer}{=1 if engineering major}
#'   \item{colGPA}{MSU GPA}
#'   \item{hsGPA}{high school GPA}
#'   \item{ACT}{'achievement' score}
#'   \item{job19}{=1 if job <= 19 hours}
#'   \item{job20}{=1 if job >= 20 hours}
#'   \item{drive}{=1 if drive to campus}
#'   \item{bike}{=1 if bicycle to campus}
#'   \item{walk}{=1 if walk to campus}
#'   \item{voluntr}{=1 if do volunteer work}
#'   \item{PC}{=1 of pers computer at sch}
#'   \item{greek}{=1 if fraternity or sorority}
#'   \item{car}{=1 if own car}
#'   \item{siblings}{=1 if have siblings}
#'   \item{bgfriend}{=1 if boy- or girlfriend}
#'   \item{clubs}{=1 if belong to MSU club}
#'   \item{skipped}{avg lectures missed per week}
#'   \item{alcohol}{avg # days per week drink alcohol}
#'   \item{gradMI}{=1 if Michigan high school}
#'   \item{fathcoll}{=1 if father college grad}
#'   \item{mothcoll}{=1 if mother college grad}
#' }
#' @source Wooldrige (2009): \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"gpa1"

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
#' @source Wooldrige (2009): \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"hprice1"

#' hseinv
#'
#' Data from D. McFadden (1994), “Demographics, the Housing Market, and
#' the Welfare of the Elderly,” in D.A. Wise (ed.), \emph{Studies in the Economics
#' of Aging}. Chicago: University of Chicago Press, 225-285.
#'
#' @usage data("hseinv")
#'
#' @format A data frame with 42 observations.
#' \describe{
#'   \item{year}{1947-1988}
#'   \item{inv}{real housing invest., millions $}
#'   \item{pop}{population, 1000s}
#'   \item{price}{housing price index; 1982 = 1}
#' }
#' @source Wooldrige (2009): \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"hseinv"

#' intdef
#'
#' Data from \emph{Economic Report of the President}, 2004,
#' Tables B-64, B-73, and B-79.
#'
#' @usage data("intdef")
#'
#' @format A data frame with 49 observations.
#' \describe{
#'   \item{year}{1948-1996}
#'   \item{i3}{3 mo. T bill rate}
#'   \item{inf}{CPI inflation rate}
#'   \item{rec}{federal receipts, % GDP}
#'   \item{out}{federal outlays, % GDP}
#'   \item{def}{out - rec (deficit as % GDP)}
#' }
#' @source Wooldrige (2009): \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"intdef"

#' rdchem
#'
#' From \emph{Businessweek} R&D Scoreboard, October 25, 1991
#'
#' @section Notes:
#' It would be interesting to collect more recent data and see whether
#' the R&D/firm size relationship has changed over time.
#'
#' @usage data("rdchem")
#'
#' @format A data frame with 32 observations.
#' \describe{
#'   \item{rd}{R&D spending, millions $}
#'   \item{sales}{firm sales, millions $}
#'   \item{profits}{profits, millions $}
#' }
#' @source Wooldrige (2009): \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"rdchem"


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
#' @source Wooldrige (2009): \emph{Introductory Econometrics: A Modern
#' Approach}, 4th ed. South-Western, Cengage Learning.
#' @docType data
"traffic2"

