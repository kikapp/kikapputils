% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscUtils.R
\name{showNAs}
\alias{showNAs}
\title{Shows the extent of NAs in a data frame}
\usage{
showNAs(df, missing_strings = c("MISSING", "missing", ""))
}
\arguments{
\item{df}{a data frame whose missingness is a mystery}

\item{missing_strings}{an array of characters indicating which strings correspond to missing values}
}
\value{
a data frame containing count and proportion missing for each variable in the_only_argument_is_a_data_frame
}
\description{
Shows the extent of NAs in a data frame
}
\examples{
> showNAs(ChickWeight)
     var NA_count NA_mean
1 weight        0       0
2   Time        0       0
3  Chick        0       0
4   Diet        0       0

> ChickWeightNA <- ChickWeight
> ChickWeightNA$weight[1:300] <- NA
> ChickWeightNA$Time <- NA
> showNAs(ChickWeightNA)
     var NA_count   NA_mean
1 weight      300 0.5190311
2   Time      578 1.0000000
3  Chick        0 0.0000000
4   Diet        0 0.0000000

}
