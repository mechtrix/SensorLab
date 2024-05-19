#' compute the potential process capability (Cp)
#'
#' @param sd_values the process standard deviation
#' @param Tol the tolerance window in which to compute
#' @param num_sd the numbers of standard deviations (default = 6)
#'
#' @return the Cp values according to asmao script
#' @export
#'
#' @examples Cp(1,10)

Cp <- function(sd_values,
               Tol,
               num_sd = 6
               ){
  Cp <- (Tol)/(6*sd_values)
  return(Cp)

}


#' compute the potential gage capability (Cg)
#'
#' @param sd_values the standard deviation of the MSA1 measurements
#' @param Tol the tolerance the gage shall be used for
#' @param K the correction factor for the tolerance (usually 0.2)
#' @param num_sd the numbers of standard deviations (default = 6)
#'
#' @return the potential gage Cg according to asmao script
#' @export
#'
#' @examples Cg(1,50)

Cg <- function(sd_values,
               Tol,
               K = 0.2,
               num_sd = 6){

  Cg <- (K*Tol)/(num_sd*sd_values)

  return(Cg)

}


#' compute the actual process capability (Cpk)
#'
#' @param mean_values the mean value of the process
#' @param sd_values the standard deviation of the process
#' @param usl the upper specification limit
#' @param lsl the lower specification limit
#' @param num_sd the numbers of standard deviations (default = 6)
#'
#' @return the actual process capability Cpk according to asmao script
#' @export
#'
#' @examples Cpk(19,0.5,21,17)

Cpk <- function(mean_values,
                sd_values,
                usl,
                lsl,
                num_sd = 6
                ){

  lim_lo <- mean_values-lsl
  lim_hi <- usl-mean_values

  sd_half <- num_sd/2

  Cp_lo <- lim_lo/(sd_half*sd_values)
  Cp_hi <- lim_hi/(sd_half*sd_values)

  Cpk <- min(c(Cp_lo,Cp_hi))

  return(Cpk)

}

#' Compute the actual gage capability with systematic error (Cgk)
#'
#' @param mean_values of the MSA1
#' @param sd_values of the MSA1
#' @param ref_value the value of the reference normal
#' @param Tol the tolerance in which the gage should measure
#' @param K the tolerance correction factor (default 0.2)
#' @param num_sd the numbers of standard deviations (default = 6)
#'
#' @return the Cgk value as a number
#' @export
#'
#' @examples Cgk(19,0.5,21,17)

Cgk <- function(mean_values,
                sd_values,
                ref_value,
                Tol,
                K = 0.2,
                num_sd = 6){

  sd_half <- num_sd/2

  Cgk <- (0.5*K*Tol-abs(mean_values-ref_value))/(sd_half*sd_values)

  return(Cgk)

}
