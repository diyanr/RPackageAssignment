% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read a data file}
\source{
The data used is from the US National Highway Traffic Safety Administration's
Fatality Analysis Reporting System (FARS), which is a nationwide census providing the
American public yearly data regarding fatal injuries suffered in motor vehicle
traffic crashes.
}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string giving the file name for the data}
}
\value{
This function returns the data in the file
}
\description{
This function reads a data file containing FARS data
}
\examples{
\dontrun{
fars_read("accident_2013.csv.bz2")
fars_read("accident_2014.csv.bz2")
fars_read("accident_2015.csv.bz2")
}

}

