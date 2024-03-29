% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_tools.R
\name{check_if_a_in_b}
\alias{check_if_a_in_b}
\title{Check if item a is in vector b}
\usage{
check_if_a_in_b(a, b)
}
\arguments{
\item{a}{any value}

\item{b}{list or vector}
}
\value{
Returns TRUE or FALSE
}
\description{
Returns TRUE If item a is found in vector b and FALSE otherwise.
}
\examples{
check_if_a_in_b(1, c(1, 2, 3))
check_if_a_in_b(0, c(1, 2, 3))

}
\references{
\href{https://stackoverflow.com/questions/53086053/how-to-check-if-a-list-contains-a-certain-element-in-r}{Stack Overflow}
}
