
#' Compute Upper Control Limit (US)
#'
#' @param dataset the dataset upon which to compute
#' @param n the size of the subgroup
#'
#' @return the UCL
#' @export
#'

cmp_UCL_US <- function(dataset,n = 5){

  UCL = mean(dataset$mean_dia)+3*mean(dataset$sd_dia)/(c4(n)*sqrt(n))
}



#' Compute Upper Warning Limit
#'
#' @param dataset  dataset
#' @param n size of the subgruop
#'
#' @return the upper warning limit
#' @export
#'

cmp_UWL <- function(dataset,n = 5){
  if (length(n) == 0){
    n<- length(dataset)
  }
  out <- mean(dataset)+1.96*(stats::sd(dataset)/sqrt(n))
  return(out)
}

#' compute lower warning limit
#'
#' @param dataset dataset
#' @param n size of the subgroup
#'
#' @return the lower warning limit
#' @export
#'

cmp_LWL <- function(dataset,n = 5){
  if (length(n) == 0){
    n<- length(dataset)
  }
  out <- mean(dataset)-1.96*(stats::sd(dataset)/sqrt(n))
  return(out)
}

#' compute upper control limit
#'
#' @param dataset the data
#' @param n size of the subgroup
#'
#' @return the upper control limit
#' @export
#'

cmp_UCL <- function(dataset,n = 5){
  if (length(n) == 0){
    n<- length(dataset)
  }
  out <- mean(dataset)+2.58*(stats::sd(dataset)/sqrt(n))
  return(out)
}

#' cmoputer lower control limit
#'
#' @param dataset data
#' @param n size of the subgroup
#'
#' @return the lower control limit
#' @export
#'

cmp_LCL <- function(dataset,n = 5){
  if (length(n) == 0){
    n<- length(dataset)
  }
  out <- mean(dataset)-2.58*(stats::sd(dataset)/sqrt(n))
  return(out)
}


#' compute the c4 parameter
#'
#' @param n sample size
#'
#' @return c4 parameter
#' @export
#'

c4 <- function(n){
  out <- sqrt(2/(n-1))*factorial((n/2)-1)/factorial(((n-1)/2)-1)
  return(out)
}

#' compute the mean of s (s chart)
#'
#' @param dataset data
#' @param n size of the subgroup
#'
#' @return the s mean
#' @export
#'

cmp_s_mean <- function(dataset, n = 5){
  sd_bar <- mean(dataset)
  out <- sd_bar
  return(out)
}

#' compute upper control limit for s chart
#'
#' @param dataset data
#' @param n subgroup
#'
#' @return the upper control limit
#' @export
#'

cmp_UCL_s <- function(dataset, n = 5){
  sd_bar <- mean(dataset)
  out <- sd_bar*1.9275
  return(out)
}

#' compute the lower control limit for s chart
#'
#' @param dataset data
#' @param n size of the subgroup
#'
#' @return the lower control limit
#' @export
#'

cmp_LCL_s <- function(dataset, n = 5){
  sd_bar <- mean(dataset)
  out <- sd_bar*0.0063
  return(out)
}

#' compute the upper warning limit for s chart
#'
#' @param dataset the data
#' @param n size of the subgroup
#'
#' @return the upper warning limit
#' @export
#'

cmp_UWL_s <- function(dataset, n = 5){
  sd_bar <- mean(dataset)
  out <- sd_bar*1.6691
  return(out)
}

#' compute the lower warning limit for s chart
#'
#' @param dataset data
#' @param n size of the subgroup
#'
#' @return the lower warning limit
#' @export
#'

cmp_LWL_s <- function(dataset, n = 5){
  sd_bar <- mean(dataset)
  out <- sd_bar*0.3480
  return(out)
}
