% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ramadan.R
\name{ramadan}
\alias{ramadan}
\title{Create dummies for Ramadan (estimated by the Umm al-Qura algorithm)}
\usage{
ramadan(dates, pre = 0, post = 0)
}
\arguments{
\item{dates}{A vector with dates in the format YYYY-MM-DD.}

\item{pre}{non-negative integer. To encode the days before the holiday dates.}

\item{post}{non-negative integer. To encode the days after the holiday dates.}
}
\value{
A dummy (binary vector).
}
\description{
Create a dummy variable that takes the value 0 or 1 to indicate the
abscence or presence of the start of Ramadan, the ninth month of the Islamic Calender.
The day(s) before or after this holiday can also be recoded by using the pre and post arguments.
For example, if you also want to recode the days after Ramadan, set
post argument to 1.
}
\examples{
dates <- seq_days("2021-4-10", "2021-4-29")
tibble::tibble(
   dates,
   start_of_ramadan = ramadan(dates)
)
}
