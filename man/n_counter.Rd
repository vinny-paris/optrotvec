% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_counter.R
\name{n_counter}
\alias{n_counter}
\title{Design Row Calculator}
\usage{
n_counter(rotv_unq, design)
}
\arguments{
\item{rotv_unq}{A vector with the first entry being TRUE/FALSE for if the design should be unique. The rest of the vector is the rotation vector with elements 0,1,2 of length f for f being number of factors}

\item{design}{The design matrix to be expanded with elements 0,1,2}
}
\value{
number of rows
}
\description{
Finds the number of rows for an augmented design created within the fucntion
}
