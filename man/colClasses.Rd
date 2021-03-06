% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscUtils.R
\name{colClasses}
\alias{colClasses}
\title{Get classes for each column in a dataframe}
\usage{
colClasses(df)
}
\arguments{
\item{df}{a data frame}
}
\value{
a data frame containing column class information
}
\description{
Get classes for each column in a dataframe
}
\examples{
colClasses(data.frame(x = pi, y = factor(letters), z = letters, stringsAsFactors = F) )

}
