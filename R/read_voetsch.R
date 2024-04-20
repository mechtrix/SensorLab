#' read raw data from voetsch twt in the sensor lab
#'
#' @param path_to_fn path to the *.csv file from voetsch
#'
#' @return a dataframe with date_time conversion already done
#' @export
#'
#' @examples
#'\dontrun{
#' out <- read_voetsch("inst/extdata/USER-2024-04-04.csv")
#'}
read_voetsch <- function(path_to_fn){

  raw <- readr::read_delim(
    path_to_fn,
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
    ) %>%
    janitor::clean_names()

  clean_data <- raw %>%
    dplyr::mutate(
      time = lubridate::as_datetime(date_time)
    )

}
