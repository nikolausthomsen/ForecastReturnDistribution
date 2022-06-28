# functions for prediction and evaluation (HS.R,Garch.R,DRF.R)



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
  if(is.null(tmp)){
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


fitDRF <- function(dataMat, yColIdx, h,
                   lag = 3,
                   num.fc = 1,
                   abs.inputs=FALSE,
                   functional=NULL,
                   quantiles=NULL,...){
  # dimension of input matrix
  d <- if(is.matrix(dataMat)) ncol(dataMat) else 1
  
  # create the lagged data
  aux <- embed(dataMat,lag+h)
  n   <- nrow(aux)
  Y   <- aux[1:(n+1-num.fc),yColIdx]
  # help random forest with absolute return values if requested
  if(abs.inputs) aux <- abs(aux)
  X   <- aux[1:(n+1-num.fc),-(1:(d*h))]
  
  # fit the model
  model.fit <- drf::drf(X=X,Y=Y,...)
  
  # prediction matrix to predict t+h
  if(num.fc==1){
    Xpred <- t(tail(aux,1)[,1:ncol(X)])
  }else{
    Xpred <- tail(aux,num.fc)[,1:ncol(X)]
  }
  
  # predict
  Ypred <- predict(model.fit,Xpred,functional = functional,quantiles=quantiles)
  
  # case distribution is requested, we also return
  # mean, sd, and quantiles if they are available
  if( is.null(functional) && !is.null(quantiles) ){
    tmp_ncol <- length(quantiles)+2
    tmp_summary <- matrix(NA,nrow = num.fc,ncol = tmp_ncol)
    colnames(tmp_summary) <- c("mean","sd",paste0("q",quantiles))
    tmp_summary[,1  ] <- predict(model.fit,Xpred, functional = "mean")$mean %>% 
      as.numeric()
    tmp_summary[,2  ] <- predict(model.fit,Xpred, functional = "sd")$sd %>% 
      as.numeric()
    tmp_summary[,3:tmp_ncol] <- predict(model.fit,Xpred, functional = "quantile",
                                        quantiles=quantiles)$quantile %>% as.numeric()
    Ypred$summary <- tmp_summary
  }
  
  
  return(list(model=model.fit,Ypred=Ypred))
}

fitroll <- function(func,data,
                    target.name,
                    lag = 3,
                    window.size = 250,
                    n.ahead = 1,
                    forecast.length = 1,
                    refit.every = 1,
                    refit.window = "moving",
                    crps = TRUE,
                    functional = NULL,
                    quantiles = NULL,
                    abs.inputs=FALSE,
                    ...){
  # split data into matrix and date
  dataDate <- data %>% arrange(date) %>% pull(date) %>% as.Date()
  dataMat  <- data %>% arrange(date) %>% select(-date) %>% as.matrix()
  # names without date
  nam <- colnames(dataMat)
  # index of the dependent column
  yColIdx <- which(nam==target.name)
  # rows in dataMat
  N <- nrow(dataMat)
  
  # initialize matrices
  p_out      <- 2+ifelse(is.null(quantiles),0,2+length(quantiles))+ifelse(crps,1,0)
  resultMat  <- matrix(NA_real_,nrow = forecast.length, ncol = p_out)
  resultDate <- character(forecast.length) %>% as.Date()
  
  # iterate through the forecast length
  for(fc_idx in seq(forecast.length,1,by=-refit.every)){
    # for the most recent fit we don't need to predict "refit.every" forecasts
    tmp_num.fc <- if(fc_idx<refit.every) fc_idx else refit.every
    # when data for forecast ends (the additional tmp_num.fc obs are not used 
    # for training, but rather for evaluation)
    tmp_endIdx <- N-n.ahead-fc_idx+tmp_num.fc
    # when data for forecast begins (we need additional lag+n.ahead-1 obs 
    # because of the lag operators in embed(.) in fitDRF)
    tmp_begIdx <- N-n.ahead-fc_idx-window.size-lag-n.ahead+3
    if(tmp_begIdx<1 || refit.window=="recursive" ) tmp_begIdx <- 1
    # prepare a slice index (we need additional lag+n.ahead+1 observations 
    # because of the lag operators in embed(.) in fitDRF)
    idx <- tmp_begIdx:tmp_endIdx
    # fill index for matrices
    tmp_fill_idx <- forecast.length-(fc_idx:(fc_idx-tmp_num.fc+1))+1
    # prediction date(s)
    resultDate[tmp_fill_idx] <- dataDate[tmp_endIdx+n.ahead-((tmp_num.fc-1):0)]
    if( crps ){
      # fit distributional random forest
      tmp <- fitDRF(dataMat[idx,],yColIdx,n.ahead,lag,tmp_num.fc,functional = NULL,
                    quantiles = quantiles,abs.inputs=abs.inputs,...)$Ypred
      # realized value
      y <- dataMat[tmp_endIdx+n.ahead-((tmp_num.fc-1):0),yColIdx]
      # real window size
      tmp_real_window <- tmp$weights@Dim[2]
      # yhat weights
      w_yhat <- matrix(tmp$weights,nrow=tmp_num.fc, ncol=tmp_real_window)
      # yhat coefs (repeat the values of yhat in each row)
      yhat <- matrix(tmp$y, nrow=tmp_num.fc, ncol=tmp_real_window, byrow=TRUE)
      # save realized values
      resultMat[tmp_fill_idx,1] <- y
      # compute and save crps
      resultMat[tmp_fill_idx,2] <- scoringRules::crps_sample(y,yhat,w=w_yhat)
      # compute the PIT values
      resultMat[tmp_fill_idx,3] <- pitDRF(y,yhat,w_yhat)
      
      # output
      if( !is.null(quantiles) ){
        resultMat[tmp_fill_idx,3+1:(2+length(quantiles))] <- tmp$summary
      }
    }else{
      stop("Not implemented yet")
    }
    
  }
  colnames(resultMat) <- c("realized",if(crps)c("crps","PIT",colnames(tmp$summary)) else colnames(tmp$summary))
  
  # output data.frame
  data.frame(resultMat, row.names = resultDate)
  
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

# helper function to get the last non NA index of a time series
getLastNonNaIdx <- function(df){
  # dimensions without date column
  p <- ncol(df)
  # initialize output
  out <- numeric(p)
  names(out) <- names(df)
  if(!is.data.frame(df)) df <- data.frame(df)
  # loop over all names
  for(i in 1:p){
    out[i] <- suppressWarnings(max(which( is.na(lead(df[,i])) & !is.na(df[,i]) )))
  }
  out
}