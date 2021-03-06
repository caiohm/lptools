% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cumulative.R
\name{get_cumulative}
\alias{get_cumulative}
\title{Cumulative change of variable after episodes}
\usage{
get_cumulative(delta, dates, k)
}
\arguments{
\item{delta}{Vector with changes.}

\item{dates}{Dummy that equals 1 at dates to compute cumulative changes.}

\item{k}{Periods to compute cumulative change.}
}
\value{
Vector with cumulative changes.
}
\description{
This function computes cumulative change from t to t+k using first differences delta at dates = 1.
}
