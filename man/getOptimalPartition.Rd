% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factorMergerHelper.R
\name{getOptimalPartition}
\alias{getOptimalPartition}
\title{Get optimal partition (final clusters names)}
\usage{
getOptimalPartition(factorMerger, stat = "GIC", value = 2)
}
\arguments{
\item{factorMerger}{object of a class \code{factorMerger}}

\item{stat}{statistic used in the bottom-up search. Available statistics are:
\code{"loglikelihood"}, \code{"pvalue"}, \code{"GIC"}.}

\item{value}{cut threshold / GIC penalty}
}
\value{
Returns a vector with the final cluster names from the \code{factorMerger} object.
}
\description{
Splits factor levels into non-overlapping
clusters based on a \code{factorMerger} object.
If a \code{stat} is \code{"loglikelihood"} or {"p-value"}
then performs bottom-up search through models
on the merging path until spots a model scored worse than
the given threshold (\code{value}).
If \code{stat = "GIC"}, \code{value} is interpreted as
GIC penalty and optimal GIC model is returned.
}
\details{
By default, \code{cutree} returns factor partition
corresponding to the optimal GIC model (with the lowest GIC).
}
