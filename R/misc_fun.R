#' sigmoid function
#'
#' A sigmoid function of the form f(x) = 1/(1-e^-(beta_0 + beta_1*x))
#'
#' @param x independent var
#' @param beta_0 parameter 1
#' @param beta_1 parameter 2
#'
#' @return a value
#' @export
#'
#' @examples
#' y <- sig_fun(1,1,1)

sig_fun <- function(x,beta_0,beta_1){

  y <- 1/(1+exp(-beta_0+beta_1*x))

  return(y)

}
