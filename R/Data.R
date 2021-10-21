#' Dataset with lurking varible
#'
#' A dataset containing two variables with are both generated from a model
#' dependted on the third variable Z.
#'
#' @docType data
#'
#' @usage data(Dataset1)
#'
#' @format A data frame with 500 rows and 3 variables:
#' \describe{
#'   \item{X}{The first variable wich is created out from Z}
#'   \item{Y}{The second variable there is created out from Z}
#'   \item{Z}{The lurking variable wich is generated from the standard normal.}
#' }
#'
"Dataset1"

#' Dataset without lurking variable
#'
#' A dataset containing three variables where X was made from Z and Y was made from X.
#' This ensure that Z is not a lurking variable.
#'
#' @docType data
#'
#' @usage data(Dataset2)
#'
#'
#' @format A data frame with 500 rows and 3 variables:
#' \describe{
#'   \item{X}{A variable which is created out from Z}
#'   \item{Y}{The second varible which is created out from X}
#'   \item{Z}{The lurking variable, with is generated from the standard normal.}
#' }
#'
"Dataset2"
