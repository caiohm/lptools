
#' Computes change between (t-1) and t+k from a vector of changes delta between t-1 and t
#'
#' @param delta Vector with changes between t and (t-1) of variable.
#' @param k Steps forward,
#' @return Change between (t+k) and (t-1) of variable.
#' @export
get_forward_delta <- function(delta,k) {
  z <- rep(NA,length(delta))
  for (i in 1:length(delta)) {
    z[i] <- sum(delta[i:(i+k)])
  }
  return(z)
}


#' Automatically adds change of variable between t and t+k to data frame
#'
#' @param df Data frame that will be transformed.
#' @param delta Vector of changes in a variable of interest x (that is, x(t)-x(t-1)).
#' @param horizon Maximum horizon to compute x(t+k)-x(t-1).
#' @param id Individual dimension of panel (provide constant variable if data is not panel).
#' @param time Time dimension of the data.
#' @return Data frame provided initially, but with the added variables.
#' @export
gen_forward_delta <- function(df,delta,horizon,id,time) {

  id <- enquo(id)
  time <- enquo(time)
  delta <- enquo(delta)
  df <- df %>%
    arrange(!!id,!!time) %>%
    group_by(!!id)
  for (k in 0:horizon) {
    name <- paste(quo_name(delta),"_g",k,sep="")
    df <- mutate(df,!!name := get_forward_delta(!!delta,k))
  }
  df <- df %>%
    ungroup() %>%
    arrange(!!id)
  return(df)
}


#' Prepare data frame to run local projection
#'
#' This function prepares a data frame to run local projections.
#' It is not necessary to use this function before running a local projection, except when the argument gen_data is set to false in the function
#' local_projection(). It may be useful to use this function if one runs many local projections with the same variables.
#'
#' @param df Data frame that will be transformed.
#' @param endog String with endogenous variable. It should represent change between t and t-1 if growth = TRUE and level if growth = FALSE.
#' See explanation in growth parameter below for details.
#' @param exogl Vector with exogenous variables of LP that are lagged, as strings (default is NULL).
#' @param lags Number of lags used in LP.
#' @param horizon Horizon used in LP.
#' @param id Individual dimension of panel, as string (provide constant variable if data is not panel).
#' @param time Time dimension of the data, as string.
#' @param growth If LHS of regression is growth rate of variable of interest y between t and t+k set equal to TRUE (default).
#' If it is level set equal to FALSE. Therefore:
#' * If growth=TRUE, endogenous variables on the LHS are differences between: (t-1) and t, t(-1) and (t+1),..., (t-1) and (t+k). In this case, endog should
#' represent a vector with first differences of variable of interest y.
#' * If growth=FALSE, endogenous variables are y(t), y(t+1),...,y(t+k). In this case endog should be the level of the variable of interest y.
#' @return Data frame provided initially, but with added variables.
#' @export
gen_data_lp <- function(df,endog,exogl=NULL,lags,horizon,id,time,growth=TRUE) {
  if (lags>0 & !is.null(exogl)) {
    for (i in 1:length(exogl)) {
      df <- gen_lag(df,!!as.symbol(exogl[i]),lags,!!as.symbol(id),!!as.symbol(time))
    }
  }

  if (growth==TRUE) {
    df <- gen_forward_delta(df,!!as.symbol(endog),horizon,!!as.symbol(id),!!as.symbol(time))
  }
  else {
    df <- gen_lead(df,!!as.symbol(endog),horizon,!!as.symbol(id),!!as.symbol(time))
    name <- paste(endog,"_lead0",sep="") # Because gen_lead does not create "endog_lead0"
    df <- mutate(df,!!name := !!as.symbol(endog))
  }
  return(df)
}


#' Runs local projection
#'
#' This function runs a local projection.
#'
#' @param df Data frame with variables used.
#' @param endog String with endogenous variable. It should represent change between t and t-1 if growth = TRUE and level if growth = FALSE.
#' See explanation in growth parameter below for details.
#' @param exogl Vector with exogenous variables that are contemporaneous controls, as strings (default is NULL).
#' @param exogl Vector with exogenous variables that are lagged, as strings (default is NULL).
#' @param shock Shock variable used to report IRF (as string).
#' @param lags Number of lags used for variables in exogl.
#' @param horizon Maximum horizon used to compute IRFs.
#' @param method Function used to run regressions (e.g., lm, felm, etc). If function requires package it should be loaded.
#' @param append_formula string to append to formula of regression.
#' By default formula is (endogenous variable ~ contemporaneous controls + exogenous controls). For instance, if one uses method=felm and want
#' to add country fixed effects, one should use append_formula="| country". Similarly, one can add instrumental variables that way.
#' @param id Individual dimension of panel, as string (provide constant variable if data is not panel).
#' @param time Time dimension of the data, as string.
#' @param conf_level Confidence level for confidence bands (default is 10%).
#' @param gen_data Use TRUE if data frame was not prepared before hand using gen_data_lp().
#' @param growth If LHS of regression is growth rate of variable of interest y between t and t+k set equal to TRUE (default).
#' If it is level set equal to FALSE. Therefore:
#' * If growth=TRUE, endogenous variables on the LHS are differences between: (t-1) and t, t(-1) and (t+1),..., (t-1) and (t+k). In this case, endog should
#' represent a vector with first differences of variable of interest y.
#' * If growth=FALSE, endogenous variables are y(t), y(t+1),...,y(t+k). In this case endog should be the level of the variable of interest y.
#' @param ... Additional arguments that will be passed to regression method chosen.
#' @return A list with:
#' * Impulse response functions (irf);
#' * Lower confidence bands (conf_low);
#' * Upper confidence bands (conf_high)
#' * Vector with horizons 0:horizon (hvec);
#' * List with regressions output (e.g., to access regression of horizon k call output$regressions[[[k]]]).
#' @export
local_projection <- function(df,endog,exogc=NULL,exogl=NULL,shock,lags=0,horizon,method,append_formula=NULL,id,time,
                             conf_level=0.9, gen_data=TRUE,growth=TRUE,...) {

  if (gen_data == TRUE) {
    df <- gen_data_lp(df,endog,exogl,lags,horizon,id,time,growth)
  }

  auxstring <- ifelse(growth==TRUE,"_g","_lead")

  if (lags>0 & !is.null(exogl)) {
    lag_str <- rep(NA,length(exogl)*lags)
    for (i in (1:length(exogl))) {
      for (j in 1:lags) {
        lag_str[(i-1)*lags+j] <- paste(exogl[i],"_lag",j,sep="")
      }
    }
  }
  if (lags==0 | is.null(exogl)) {
    lag_str <- NULL
  }

  endog_str <- rep(NA,horizon+1)
  for (i in (0:horizon)) {
    endog_str[i+1] <- paste(endog,auxstring,i,sep="")
  }

  irf <- rep(NA,horizon+1)
  conf_low <- irf
  conf_high <- irf
  regressions <- vector('list', horizon+1)
  for (i in 1:(horizon+1)){
    formula_str <- paste(endog_str[i], paste(c(exogc,lag_str), collapse=" + "), sep=" ~ ")
    formula_str  <- paste(formula_str,append_formula,sep=" ")
    reg <- method(formula = as.formula(formula_str),data = df, ...)

    betas <- coef(reg)
    irf[i] <- betas[which(names(betas) == shock)]
    try(confband <- confint(reg,shock,level = conf_level), silent=TRUE)
    try(conf_low[i] <- confband[1], silent=TRUE)
    try(conf_high[i] <- confband[2], silent=TRUE)
    regressions[[i]] <- reg
  }
  return(list(irf = irf, conf_low = conf_low, conf_high = conf_high, hvec = 0:horizon, regressions = regressions))
}

