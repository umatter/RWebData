\name{dfList}
\alias{dfList}
\title{Harmonize and combine different data frames in a list}
\usage{
dfList(l)
}
\arguments{
  \item{l}{a list containing several data frames}
}
\value{
a data frame combining all individual data frames from the
input list
}
\description{
A function that harmonizes and combines different data
frames in a list to one data frame.
}
\examples{
a <- data.frame(a=c(1,2,4),b=c(5,5,6))
b <- data.frame(a=c(3,4,5,6), b=c(8,8,5,4), c=c(9,4,2,1))
x <- list(a,b)
dfList(x)
}

