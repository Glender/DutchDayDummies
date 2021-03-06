% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fathers_day.R
\name{fathers_day}
\alias{fathers_day}
\title{Create dummies for Father's Day (In dutch: 'Vaderdag')}
\usage{
fathers_day(dates, pre = 0, post = 0)
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
abscence or presence of Father's Day. The day(s) before or after this holiday
can also be recoded by using the pre and post arguments. For example, if you also
want to recode the day after Father's Day, set the
post argument to 1.
}
\examples{
df <- tibble::tibble(
dates = seq_days("2022-6-14", "2022-6-24"),
dad_day = fathers_day(dates, pre=1,post=0)
)
df
}
