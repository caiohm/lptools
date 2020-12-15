
#' Computes lag of vector
#'
#' @param x Vector used to compute lags.
#' @param k Number of lags.
#' @return Vector x(t-k), with NA in the first k entries.
#' @export
get_lag <- function(x,k){
  z <- c(rep(NA,k),head(x,-k))
}


#' Computes lead of vector
#'
#' @param x Vector used to compute leads.
#' @param k Number of leads.
#' @return Vector x(t+k), with NA in the last k entries.
#' @export
get_lead <- function(x,k){
  z <- c(tail(x,-k),rep(NA,k))
}


#' Automatically adds lagged to data frame
#'
#' This function takes a data frame and returns the same data frame with the additional lagged variables.
#'
#' @param df Data frame to be transformed.
#' @param x Name of the variable used to compute the lags.
#' @param lags Maximum lag used (e.g.,lag = 2 creates variable x(t-1) and x(t-2)).
#' @param id Individual dimension of panel data (if data is not a panel a constant variable should be provided).
#' @param time Time dimension of the data.
#' @return Provided data frame df with the added lagged variables.
#' @export
gen_lag <- function(df,x,lag,id,time){
  id <- enquo(id)
  time <- enquo(time)
  x <- enquo(x)
  lags <- 1:lag
  df <- df %>%
    arrange(!!id,!!time) %>%
    group_by(!!id)
  for (k in lags) {
    name <- paste(quo_name(x),"_lag",k,sep="")
    df <- mutate(df, !!name := get_lag(!!x,k))
  }
  df <- df %>%
    ungroup() %>%
    arrange(!!id)
  return(df)
}


#' Automatically adds leads to data frame
#'
#' This function takes a data frame and returns the same data frame with the additional variables of leads.
#'
#' @param df Data frame to be transformed.
#' @param x Name of the variable used to compute the leads.
#' @param lead Maximum lead used (e.g.,lead = 2 creates variable x(t+1) and x(t+2)).
#' @param id Individual dimension of panel data (if data is not a panel a constant variable should be provided).
#' @param time Time dimension of the data.
#' @return Provided data frame df with the added lagged variables.
#' @export
gen_lead <- function(df,x,lead,id,time){
  id <- enquo(id)
  time <- enquo(time)
  x <- enquo(x)
  lags <- 1:lead
  df <- df %>%
    arrange(!!id,!!time) %>%
    group_by(!!id)
  for (k in lags) {
    name <- paste(quo_name(x),"_lead",k,sep="")
    df <- mutate(df, !!name := get_lead(!!x,k))
  }
  df <- df %>%
    ungroup() %>%
    arrange(!!id)
  return(df)
}
