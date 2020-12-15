
#' Classifies variables according to bins
#'
#' Returns 1 if x> inf and x<=sup and zero otherwise (and NA if the variable is NA).
#'
#' @param x Variable to be classified.
#' @param inf Lower bound of interval.
#' @param sup Upper bound of interval.
#' @return Dummy that equals 1 if variable belongs to bin.
#' @export
get_bin <- function(x,inf,sup) {
  z <- ifelse(x>inf & x<=sup,1,0)
  return(z)
}
###

#' Automatically adds binned variables to data frame
#'
#' @param df Data frame to be transformed.
#' @param x Variable to be classified in bins.
#' @param binvec Vector with increasing sequence to create bins.
#' (e.g., if binvec=c(1,2,3) two bins are created from intervals (1,2] and (2,3]).
#' @return Original data frame but with added bins.
#' @export
gen_bin <- function(df,x,binvec) {
  x <- enquo(x)
  for (i in 1:(length(binvec)-1)) {
    name <- paste(quo_name(x),"_bin",i,sep="")
    df <- mutate(df, !!name := get_bin(!!x,binvec[i],binvec[i+1]))
  }
  return(df)
}

