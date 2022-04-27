# functions for prediction and evaluation (HS.R,...)


##### Prediction Functions #####

garch <- function(spec, data, n.ahead = 1, forecast.length = 500, 
                  n.start = NULL, refit.every = 25, refit.window = c("recursive", "moving"), 
                  window.size = NULL, solver = "hybrid", fit.control = list(), 
                  solver.control = list(), calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), 
                  cluster = NULL, keep.coef = TRUE, ...){
  tmp <- tryCatch(
    rugarch::ugarchroll(
      spec, data, n.ahead = n.ahead, forecast.length = forecast.length, 
      n.start = n.start, refit.every = refit.every, refit.window = refit.window, 
      window.size = window.size, solver = solver, fit.control = fit.control, 
      solver.control = solver.control, calculate.VaR = calculate.VaR, VaR.alpha = VaR.alpha, 
      cluster = cluster, keep.coef = keep.coef, ...
    )@forecast$density[,c("Mu","Sigma","Shape","Realized")],
    error = function(e) tmp <- NULL
  )
  if(!is.null(tmp)){
    # try other solver options
    for(solver_option in 1:4){
      tmp <- tryCatch(
        rugarch::ugarchroll(
          spec, data, n.ahead = n.ahead, forecast.length = forecast.length, 
          n.start = n.start, refit.every = refit.every, refit.window = refit.window, 
          window.size = window.size, solver = "nloptr", fit.control = fit.control, 
          solver.control = list(solver=solver_option), calculate.VaR = calculate.VaR, VaR.alpha = VaR.alpha, 
          cluster = cluster, keep.coef = keep.coef, ...
        )@forecast$density[,c("Mu","Sigma","Shape","Realized")],
        error = function(e) tmp <- NULL
      )
      if(!is.null(tmp)) break
    }
  }
  
  return(tmp)
  
}


##### Evaluation Metrics #####

pit <- function(y, yhat) mean(yhat<y)

# this is not restricted to DRF, but then more checks needed
pitDRF <- function(y, yhat, w_yhat){
  n <- length(y)
  yMat <- array(y, dim=dim(yhat))
  logi_yhat_l_y <- yhat<yMat
  pit <- rep(NA_real_,n)
  for(i in 1:n){
    pit[i] <- sum(w_yhat[i,logi_yhat_l_y[i,]])
  }
  return(pit)
}

roll_func <- function(y, width, func){
  n <- length(y)
  nout <- n-width+1
  
  out <- rep(NA_real_,n)
  if(nout>0){
    id <- matrix(1:width, nout, width, byrow=TRUE) + 0:(nout-1)
    yWindow <- matrix(y[id], nout, width)
    
    for(i in width:n) out[i] <- func(y[i], yWindow[i-width+1,])
  }
  
  return(out)
}



##### Helper #####

# helper function to get the first non NA index of a time series
getFirstNonNaIdx <- function(df){
  # dimensions without date column
  p <- ncol(df)
  # initialize output
  out <- numeric(p)
  names(out) <- names(df)
  if(!is.data.frame(df)) df <- data.frame(df)
  # loop over all names
  for(i in 1:p){
    out[i] <- suppressWarnings(min(which( is.na(lag(df[,i])) & !is.na(df[,i]) )))
  }
  out
}