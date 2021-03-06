% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summer_vacation.R
\name{summer_vacation}
\alias{summer_vacation}
\title{Create dummies for Summer vacation (In dutch: 'Zomervakantie')}
\usage{
summer_vacation(dates, pre = 0, post = 0)
}
\arguments{
\item{dates}{A vector with dates in the format YYYY-MM-DD}

\item{pre}{non-negative int. To encode the days before the holiday dates.}

\item{post}{non-ngevative int. To encode the days after the holiday dates.}
}
\value{
A dummy (binary vector)
}
\description{
Create a dummy variable that takes the value 0 or 1 to indicate the
abscence or presence of summer break. The day(s) before or after summer break
can also be recoded by using the pre and post arguments. For example, if you also
want to recode the days after the summer vacation period, set
post argument to 1.
}
\examples{
df <- tibble::tibble(
dates = seq_days("2021-1-1", "2021-12-31"),
summer_vac = summer_vacation(dates)
)
# Examine result
#View(df)
}
