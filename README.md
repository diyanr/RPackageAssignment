# RPackageAssignment
[![Build Status](https://travis-ci.org/diyanr/RPackageAssignment.svg?branch=master)](https://travis-ci.org/diyanr/RPackageAssignment)

The functions provided for you in this package will be using data from the US National Highway Traffic Safety Administration's [Fatality Analysis Reporting System](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars), which is a nationwide census providing the American public yearly data regarding fatal injuries suffered in motor vehicle traffic crashes.

## Package Info

This document goes through the following exported functions in the package:

- make_filename
- fars_read
- fars_read_years
- fars_summarize_years
- fars_map_state

## make_filename

Make a file name: This function takes year character string and makes a file name that contains the FARS data for that particular year

Input param: year A numeric year in YYYY format

Returns: A character string of the FARS data filename for that year

Examples:
```{r}
RPackageAssignment::make_filename(2013)
RPackageAssignment::make_filename(2014)
RPackageAssignment::make_filename(2015)
```

## fars_read

Read a data file: This function reads a data file containing FARS data

Input param: filename A character string giving the file name for the data

Return: a dataframe with the data in the file

Examples:
```{r}
data2013 <- RPackageAssignment::fars_read("accident_2013.csv.bz2")
## Number of records in 2013
nrow(data2013)
```

## fars_read_years

Read data for a list of years: This function takes a list of years in YYYY format and reads the FARS data for each year

Input param: years A list of years in YYYY format

Returns:

- A list of data tables for each year.
- Returns an error message "invalid year YYYY" when an error is encountered.

Examples:
```{r}
data_2013_2015 <- RPackageAssignment::fars_read_years(2013:2015)
## Number of data files between 2013 and 2015
length(data_2013_2015)
## number of records in data for 2013
nrow(data_2013_2015[[1]])
```

## fars_summarize_years

Summarize the data for each year: This function takes a list of years and returns a table of the total number of fatalities per month for each year

Input param: years A list of years in YYYY format

Returns: A table of the total fatalities per month in each year

Examples:
```{r}
RPackageAssignment::fars_summarize_years(2013:2015)
```

## fars_map_state

Map the data for a given state for a given year: This function takes a state number and a given year in YYYY format and displays a geogrphical map of fatalities accross the state for that year.

Input params:

- state.num A numeric value from 1 to 51 that represents each of the states and DC
- year A specific year in YYYY format

Returns: A geographic map of the state with points representing fatalities during that year

Examples:
```{r}
## Display data for Maryland in 2014
RPackageAssignment::fars_map_state(24, 2014)
## Display data for Virginia in 2015
RPackageAssignment::fars_map_state(51, 2015)
```
