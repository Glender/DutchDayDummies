# valide seq_days function
validate_seq_days <- function(date){
  if(!stringr::str_detect(date, "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}")){
    stop(
      glue::glue("Assure that your date ",
      date, " complies to the format yyyy-mm-dd. For example: '2021-12-25'."),
      call. = FALSE
    )
  } else if(!is.character(date)){
    stop(
      "`from` and `to` must be a date written in string characters (e.g. '2021-12-25').",
      call. = FALSE
    )
  } else if (!lubridate::is.Date(as.Date(date))){
    stop(
      glue::glue("Assure that your date ",
                 date, " complies to the format yyyy-mm-dd. For example: '2021-12-25'."),
      call. = FALSE
    )
  }
}

#' Create a time series on a daily level
#'
#' Create a vector that consists of a sequence of dates on a daily level.
#' It is created by specifying the start and end day in the format yyyy-mm-dd, for example: "2021-12-31".
#' Be mindful that other date formats are not allowed.
#'
#' @param from string The starting value of the sequence, must be in the format 'YYY-MM-DD'.
#' @param to string The end value of the sequence, must be in the format 'YYY-MM-DD'.
#' @return A vector of class date
#' @export
#' @examples
#' # Create a time series by days:
#' seq_days(from = "2020-1-1", to = "2021-12-31")
seq_days <- function(from, to){
  # stop if not in right format
  validate_seq_days(from)
  validate_seq_days(to)
  # create day vector
  date <- seq(
    from=as.Date(from, format = "%Y-%m-%d"),
    to=as.Date(to, format = "%Y-%m-%d"),
    by="days"
  )
  return(date)
}
