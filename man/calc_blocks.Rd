% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{calc_blocks}
\alias{calc_blocks}
\title{Calculate block size options based on predetermined N}
\usage{
calc_blocks(num_blocks, levels, N)
}
\arguments{
\item{num_blocks}{Number of blocks used, only works for 1:3.}

\item{levels}{Number of treatment levels in the study.  Used to determine odd/even block sizes.}

\item{N}{The total number of randomization slots you'll need.}
}
\value{
A nx4 data.frame with all options for block sizes for your given parameters.
}
\description{
Calculate block size options based on predetermined N
}
\examples{
calc_blocks(num_blocks=3, levels=3, N=216)
}

