% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Make a file name}
\source{
The data used is from the US National Highway Traffic Safety Administration's
Fatality Analysis Reporting System (FARS), which is a nationwide census providing the
American public yearly data regarding fatal injuries suffered in motor vehicle
traffic crashes.
}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A numeric year in YYYY format}
}
\value{
A character string of the FARS data filename for that year
}
\description{
This function takes year character string and makes a file name that contains
the FARS data for that particular year
}
\examples{
\dontrun{
make_filename(2013)
make_filename(2014)
make_filename(2015)
}

}

