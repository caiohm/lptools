
#' Cumulative change of variable after episodes
#'
#' This function computes cumulative change from t to t+k using first differences delta at dates = 1.
#'
#' @param delta Vector with changes.
#' @param dates Dummy that equals 1 at dates to compute cumulative changes.
#' @param k Periods to compute cumulative change.
#' @return Vector with cumulative changes.
#' @export
get_cumulative <- function(delta,dates,k) {

  z <- rep(NA,length(dates))
  for (i in 1:(length(dates)-k)) {
    if (coalesce(dates[i]==1,FALSE)) {
      z[i] <- sum(delta[i:(i+k)])
    }
  }
  return(z)
}


#' Automatically adds variable with cumulative change after episodes to data frame
#'
#' @param df Data frame to be transformed.
#' @param delta Vector with changes.
#' @param dates Dates with episodes (equals 1 if episode occurred).
#' @param kvec (Possibly of a vector of) steps to compute cumulative changes
#' (e.g., if k=2 it computes delta(t)+delta(t+1)+delta(t+2) for each episode).
#' @param id Individual dimension of panel (if not panel data provide constant variable).
#' @param time Time dimension of panel.
#' @return Same data frame initially provided but with added cumulative change variables.
#' @export
gen_cumulative <- function(df,delta,dates,kvec,id,time) {

  id <- enquo(id)
  time <- enquo(time)
  delta <- enquo(delta)
  dates <- enquo(dates)

  df <- df %>%
    arrange(!!id,!!time) %>%
    group_by(!!id)
  for (k in kvec) {
    name <- paste(quo_name(delta),"_",quo_name(dates),"_cum",k,sep="")
    df <- mutate(df,!!name := get_cumulative(!!delta,!!dates,k))
  }
  df <- df %>%
    ungroup() %>%
    arrange(!!id)
  return(df)
}
