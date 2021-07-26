#' Create dummies for World Animal Day (In dutch: 'Dierendag')
#'
#' Create a dummy variable that takes the value 0 or 1 to indicate the
#' abscence or presence of World Animal Day. The day(s) before or after World Animal Day
#' can also be recoded by using the pre and post arguments. For example, if you also
#' want to recode the days after World Animal Day, set
#' post argument to 1.
#'
#' @param dates A vector with dates in the format YYYY-MM-DD.
#' @param pre non-negative integer. To encode the days before the holiday dates.
#' @param post non-negative integer. To encode the days after the holiday dates.
#' @return A dummy (binary vector).
#' @importFrom magrittr %>%
#' @export
#' @examples
#' dates <- seq_days("2021-3-31", "2021-4-6")
#' tibble::tibble(
#'dates,
#'animal_day=world_animal_day(dates)
#')
world_animal_day <- function(dates, pre=0, post=0){
  holiday_dates <- find_dates(dates, "10-04$") %>%
    add_intervals(pre=pre, post=post)
  # create new column and recode holidays
  vector <- add_holiday_dummies(dates, holiday_dates)
  return(vector)
}
