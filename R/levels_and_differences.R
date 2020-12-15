
#' Gets the level of some data from changes
#'
#' @param delta Vector of changes in a variable of interest x (that is x(t)-x(t-1)).
#' @param dates Vector of dates associated to each value of x (it must be in ascending order).
#' @param ref Date at which we normalize the level to zero.
#' @return Vector with normalized level.
#' @export
get_level <- function(delta,dates,ref) {

  ind = match(ref,dates)
  z = dates*0

  if (ind>1){
    for (i in (ind-1):1){
      z[i] = z[i+1] - delta[i+1]
    }
  }

  if (ind<length(dates)) {
    for (i in (ind+1):length(dates)){
      z[i] = z[i-1] + delta[i]
    }
  }
  return(z)
}


#' Gets the "fake" level of some data from changes,
#'
#' This function computes the level of some data from changes. We say it is the "fake" level
#' because if there is missing data (holes in the time series), the function still computes some kind of level.
#' In particular, after each hole in the series, a 0 is assigned to the level variable.
#' If one uses the output of this function to compute first differences, we get the right first differences (other than that, the output has no meaning).
#' This function is useful to use in packages that requires some variable in level as input, but that compute first differences before using it.
#'
#' @param delta Vector with changes of data to compute fake level.
#' @return Vector with fake levels (only useful to back out first differences).
#' @export
get_fake_level <- function(delta) {

  z <- rep(NA,length(delta))
  na <- c(1,which(is.na(delta) & !is.na(get_lead(delta,1))))
  z[na] <- 0
  for (i in na) {
    for (j in (i+1):length(z)) {
      if (is.na(delta[j])) {
        break
      }
      z[j] <- z[j-1] + delta[j]
    }
  }
  return(z)
}


#' Log difference of a vector with NA in first row
#'
#' @param x Variable to compute log differences.
#' @return Log difference with NA in first row.
#' @export
get_logdif <- function(x) {
  z <- c(NA,diff(log(x)))
}


#' First difference of a vector with NA in first row
#'
#' @param x Variable to compute first difference.
#' @return First difference with NA in first row.
#' @export
get_dif <- function(x) {
  z <- c(NA,diff(x))
}
