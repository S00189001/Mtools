#' Title
#'
#' @param x  A Numeric vector for calculation
#' @param f A function to be applied to x
#'
#' @return Result of calculation
#' @export
#'
#' @examples my_calc(1:10, mean)
#'
my_calc <- function(x, f)
{
  f(x)
}
