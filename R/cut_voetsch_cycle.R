#' Cut a SensorLab Voetsch dataset into its cycles
#'
#' @param dataset a dataset as normalized by read_voetsch
#' @param voetsch_total_number_of_cycles what is the total number of cycles in the dataset? --> usually repetitions or someone canceled it
#' @param voetsch_cycle a list that specifies the programmed voetsch cycle. See default values for spec. Can probably refer to a standardized cycle at some point?
#'
#' @return a the original dataframe including the cycle number and a measurement index within that cycle
#' @export
#'
#' @examples
#'\dontrun{
#' out <- cut_voetsch_cycle(dataset, voetsch_total_number_of_cycles = 33)
#'}
cut_voetsch_cycle <- function(dataset,
                              voetsch_total_number_of_cycles,
                              voetsch_cycle = list(
                                step_no = c(1,2,3,4,5,6),
                                temperature_celsius = c(-40,-40,150,150,23,23),
                                duration_min = c(0,30,30,15,30,15))
)
{

  `%within%` <- lubridate::`%within%`

  start_time <- dataset$time[[1]] # get the first time stamp
  end_time <- utils::tail(dataset$time,1)

  voetsch_total_cycle_time <- sum(voetsch_cycle[["duration_min"]]) %>% lubridate::as.period() # calc the complete cycle time

  voetsch_cycle_index_start_time <- seq(0,voetsch_total_number_of_cycles - 1) # indices for calculating interval start
  voetsch_cycle_index_end_time <- seq(1,voetsch_total_number_of_cycles) # indices for calculating interval end

  # calc interval start and end and assign last value (if cycle is cut off)

  voetsch_cycle_df <- data.frame(
    idx_start = seq(0,voetsch_total_number_of_cycles - 1),
    idx_end = seq(1,voetsch_total_number_of_cycles)
  ) %>%
    dplyr::mutate(
      cycles_start =  start_time+idx_start*lubridate::minutes(voetsch_total_cycle_time),
      cycles_end = start_time+idx_end*lubridate::minutes(voetsch_total_cycle_time)
    )

  voetsch_cycle_df[["cycles_end"]][nrow(voetsch_cycle_df)] <- end_time

  # make the intervals for checking if timestamp is in interval

  voetsch_cycle_df <- voetsch_cycle_df %>%
    dplyr::mutate(
      int_chk =lubridate::interval(
        start = cycles_start,
        end = cycles_end)
    )

  # make list for multiple comparisons

  int_list <- voetsch_cycle_df$int_chk

  # check in which interval (cycle number) the acutal timestamp is and return index
  # also make the measurement index

  dataset <- dataset %>%
    dplyr::mutate(
      cycle_number = purrr::map_dbl(time,function(x) which(x %within% int_list))
    ) %>%
    dplyr::group_by(cycle_number) %>%
    dplyr::mutate(measurement_index = dplyr::row_number(cycle_number)) %>%
    dplyr::ungroup()

  out <- dataset

  return(out)

}
