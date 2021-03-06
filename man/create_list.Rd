% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{create_list}
\alias{create_list}
\title{Create a randomization list for upload into Medidata}
\usage{
create_list(list_name, myseed, site_n, n, block_sizes, arm_codes, arm_names,
  out_loc)
}
\arguments{
\item{list_name}{A name to identify your list, also the filename.}

\item{myseed}{Randomization seed.}

\item{site_n}{The number of sites in your study.}

\item{n}{The number of randomization slots per site.}

\item{block_sizes}{A list of block sizes.}

\item{arm_codes}{A vector of Arm codes.}

\item{arm_names}{A vector or Arm descriptions.}

\item{out_loc}{Where the CSV will get output to.}
}
\value{
A randomization list
}
\description{
Create a randomization list for upload into Medidata
}
\examples{
create_list(list_name = "test_list",
           myseed = 213,
           site_n = 1,
           n = 216,
           block_sizes = c(3,24),
           arm_codes = c("a","b","c"),
           arm_names = c("Drug A description", "Drug B description", "Drug C description"),
           out_loc = "C:/Users/Desktop/")
}

