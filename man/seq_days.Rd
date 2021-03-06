% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_days.R
\name{seq_days}
\alias{seq_days}
\title{Create a time series on a daily level}
\usage{
seq_days(from, to)
}
\arguments{
\item{from}{string The starting value of the sequence, must be in the format 'YYY-MM-DD'.}

\item{to}{string The end value of the sequence, must be in the format 'YYY-MM-DD'.}
}
\value{
A vector of class date
}
\description{
Create a vector that consists of a sequence of dates on a daily level.
It is created by specifying the start and end day in the format yyyy-mm-dd, for example: "2021-12-31".
Be mindful that other date formats are not allowed.
}
\examples{
# Create a time series by days:
seq_days(from = "2020-1-1", to = "2021-12-31")
}
