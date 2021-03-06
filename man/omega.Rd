% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/omega_value.R
\name{omega}
\alias{omega}
\title{Finds the Omega value for a given design}
\usage{
omega(D)
}
\arguments{
\item{D}{The design matirx encoded with (0,1,2) where each run is a row.}
}
\value{
\item{Omega-value}{The value the Omega optimality criterion for this design.}
}
\description{
The function takes in a single 3-level design and returns the calculated value for Omega.
}
