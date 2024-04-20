#' Compute ppm from given Cpk
#'
#' @param process_cpk the process Cpk as calculated by Cpk = (min(mu - LSL | USL - mu))/(6*sd) --> classical approach
#'
#' @return A value rounded to the nearest integer giving the ppm
#' @export
#'
#' @examples
#' ppm <- cmp_ppm_cpk(2)
cmp_ppm_cpk <- function(process_cpk){

  p_process_lo <- 1-pnorm(process_cpk*3) # probability for fail part out of spec because of lower limit
  p_process_hi <- 1-pnorm(process_cpk*3,lower.tail = TRUE) # probability for fail part out of spec because of upper limit

  dpmo <- round((p_process_lo+p_process_hi)*1000000,digits = 0) # ppm calculation

  ppm <- dpmo

  return(ppm)

}
