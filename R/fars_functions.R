#' Read a data file
#'
#' This function reads a data file containing FARS data
#'
#' @source The data used is from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (FARS), which is a nationwide census providing the
#' American public yearly data regarding fatal injuries suffered in motor vehicle
#' traffic crashes.
#'
#' @param filename A character string giving the file name for the data
#'
#' @return This function returns the data in the file
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2014.csv.bz2")
#' fars_read("accident_2015.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make a file name
#'
#' This function takes year character string and makes a file name that contains
#' the FARS data for that particular year
#'
#' @source The data used is from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (FARS), which is a nationwide census providing the
#' American public yearly data regarding fatal injuries suffered in motor vehicle
#' traffic crashes.
#'
#' @param year A numeric year in YYYY format
#'
#' @return A character string of the FARS data filename for that year
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename(2014)
#' make_filename(2015)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read data for a list of years
#'
#' This function takes a list of years in YYYY format and reads the FARS data
#' for each year
#'
#' @source The data used is from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (FARS), which is a nationwide census providing the
#' American public yearly data regarding fatal injuries suffered in motor vehicle
#' traffic crashes.
#'
#' @param years A list of years in YYYY format
#'
#' @return A list of data tables for each year.
#'      Returns an error message "invalid year YYYY" when an error is encountered.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(c(2014, 2015))
#' fars_read_years(2013:2015)
#' }
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize the data for each year
#'
#' This function takes a list of years and returns a table of the total number
#' of fatalities per month for each year
#'
#' @source The data used is from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (FARS), which is a nationwide census providing the
#' American public yearly data regarding fatal injuries suffered in motor vehicle
#' traffic crashes.
#'
#' @param years A list of years in YYYY format
#'
#' @return A table of the total fatalities per month in each year
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_sumarize_years(c(2014, 2015))
#' fars_summarize_years(2013:2015)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map the data for a given state for a given year
#'
#' This function takes a state number and a given year in YYYY format
#' and displays a geogrphical map of fatalities accross the state
#' for that year
#'
#' @source The data used is from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System (FARS), which is a nationwide census providing the
#' American public yearly data regarding fatal injuries suffered in motor vehicle
#' traffic crashes.
#'
#' @param state.num A numeric value from 1 to 51 that represents each of the states and DC
#' @param year A specific year in YYYY format
#'
#' @return A geographic map of the state with points representing fatalities during that year
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(24, 2014)
#' fars_map_state(51, 2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
