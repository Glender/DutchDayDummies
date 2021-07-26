#' Gauss Easter Algorithm to calculate Easter dates
#'
#' @param years int.
#' @export
#'
#' @examples
#' gauss_easter_algorithm(2021)
gauss_easter_algorithm <- function(years) {
  easter_dates <- as.Date(sapply(years, gauss_easter_algorithm_Cpp))
  return(easter_dates)
}
