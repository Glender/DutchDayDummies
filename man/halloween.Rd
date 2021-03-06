% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/halloween.R
\name{halloween}
\alias{halloween}
\title{Create dummies for Halloween}
\usage{
halloween(dates, pre = 0, post = 0)
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
abscence or presence of Halloween. The day(s) before or after Halloween
can also be recoded by using the pre and post arguments. For example, if you also
want to recode the days after Halloween, set
post argument to 1.
}
\examples{
dates <- seq_days("2021-10-29", "2021-11-4")
tibble::tibble(
dates,
hween=halloween(dates)
)
}
