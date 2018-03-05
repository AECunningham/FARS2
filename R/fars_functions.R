utils::globalVariables(c("STATE", "MONTH", "year"))

#' Read a file of FARS data
#'
#' This function reads the FARS data from a specified file, and returns it as a tibble.
#'
#' NB - in practice, this function is only likely to be called from within other functions.
#' However, it is exported and documented here for the sake of completeness.
#'
#'
#' @param filename name of the file you want to read.
#' The file must exist, otherwise an error message will result.
#' @return A tibble of the file contents
#' @examples
#' fars_read(make_filename(2013))

#' @export

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Construct a file name for the FARS data for a particular year
#'
#' This function should be supplied with a 4-digit year, and will return
#' the name of the file in which FARS data for that year can be found.
#'
#' NB - in practice, this function is only likely to be called from within other functions.
#' However, it is exported and documented here for the sake of completeness.
#'
#' @param year A 4-digit number denoting a year.
#' @return A string representing the name of the file containing FARS data for \code{year}
#' @examples
#' make_filename(2013)

#' @export

make_filename <- function(year) {
        year <- as.integer(year)
#        sprintf("accident_%d.csv.bz2", year)
        system.file("extdata",sprintf("accident_%d.csv.bz2",year),package="FARS2") # suggested by J Waugh
}

#' Read all the FARS data for specified years
#'
#' This function should be supplied with a vector of 4-digit years. It will read the FARS
#' data from all the relevant files for those years, and return one tibble per year, retaining
#' the 'MONTH' and 'year' columns only.
#'
#' NB - In practice, this function is only likely to be called from within other functions.
#' However, it is exported and documented here for the sake of completeness.
#'
#' @param years A vector of 4-digit numbers, each denoting a year.
#' Must all be years for which you have a FARS data file, otherwise an error message will result.
#' @return A list of tibbles, one for each of the specified \code{years},
#' retaining the 'MONTH' and 'year' columns only.
#' @examples
#' fars_read_years(c(2013,2014))

#' @export
#' @importFrom magrittr %>%

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

#' Creates a cross-tab with number of accidents per month and year.
#'
#' This function should be supplied with a vector of 4-digit years. It will read the FARS
#' data from all the relevant files for those years, and return a cross-tabulation showing
#' how many accidents there were in each month of each year.
#'
#' @param years A vector of 4-digit numbers, each denoting a year.
#' Must all be years for which you have a FARS data file, otherwise an error message will result.
#' @return A tibble containing the number of accidents for each combination of month and year.
#' @examples
#' fars_summarize_years(c(2013,2014))

#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr n

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Draw a dot map of FARS data for a particular year in a particular state
#'
#' This function should be supplied with a state number and a 4-digit year, and will draw
#' a dot map of all the accidents occurring that year in that state.
#'
#' @param state.num an integer representing a US state number.
#' Must be a state number which appears in the data file for the relevant year, otherwise
#' an error message will result.
#' @param year A 4-digit number denoting a year.
#' Must be a year for which you have a FARS data file, otherwise an error message will result.
#' @return A map of the accidents occuring that year in that state.
#' @examples
#' fars_map_state(40,2013)

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
