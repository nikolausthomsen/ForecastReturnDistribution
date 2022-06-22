# tuned DRF model to come up with advancter model than in preregistration

# - USE THE SPARSE VERSION OF INPUTS AFTER SEEING THE FEATURE IMPORTANCE FROM UNKNOWN TEST SET
# - TUNE ntree, mtry AND min.node.size ON KNOWN TEST SET

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import prediction and evaluation functions
source(file.path(path$func,"func_Pred.R"))

# tune the Model
# - slim: only the 3 most important variables are included and evaluated on UNKNOWN test set
# - tuneAll: 3 most important variables, and all grids are tuned on KNOWN test set
# - tuned: 3 most important variables, and best grid from "tuneAll" are evaluated on UNKNOWN test set
tuneModel <- "tuned"
DRF$n_fc <- 4

if(tuneModel=="tuneAll"){
  # faster performance (otherwise tuning would take much longer)
  permitSmallerW <- FALSE
  DRF <- list(
    splitting.rule = "CART",
    window.size = 1000,
    num.trees = c(500,1000,1500),
    min.node.size = c(15,20),
    mtry = c(floor(sqrt(9)),5,min(ceiling(sqrt(9) + 20), 9)),
    n_fc = 250,
    refit.every = 50,
    n_lags = 1,
    absolute.inputs = TRUE,
    corsi.freq = "m",
    q = c(.05,.95)
  )
  # read data without the unknown test set
  dat <- readRDS(paste0(str_remove(path$input,"ForecastReturnDistribution"),"2022_03_31_NAFilledData.rds"))
}else if(tuneModel=="tuned"){
  # evaluate tuned model on unknown test set
  permitSmallerW <- TRUE
  DRF$num.trees <- 1000
  DRF$min.node.size <- 20
  DRF$mtry <- floor(sqrt(9))

  # read data including unknown test set
  dat <- readRDS(paste0(creationDataDate,"NAFilledData.rds"))
}else if(tuneModel=="slim"){
  # evaluate tuned model on unknown test set
  permitSmallerW <- TRUE
  # default params of DRF function
  DRF$num.trees <- 500
  DRF$min.node.size <- 15
  DRF$mtry <-  min(ceiling(sqrt(9) + 20), 9)
  
  # read data including unknown test set
  dat <- readRDS(paste0(creationDataDate,"NAFilledData.rds"))
}else stop("tuneModel not implemented. Choose from: tuneAll, tuned, slim")


# load packages
library(RcppRoll)
library(progress)
library(foreach)



# now select only R, HML, and IXIC as predictors
name_dat <- names(dat)
dat <- dat %>% select(all_of(c("date",
                               str_subset(name_dat,"^adjusted\\_"),
                               str_subset(name_dat,"^highMlow\\_"),
                               "IXIC")))

# incorporate corsi variables
if(DRF$corsi.freq=="w"){
  agg_func <- list(w=function(x)roll_meanr(abs(x), n=5))
  dat <- dat %>% mutate(across(-date, agg_func))
}else if(DRF$corsi.freq=="m"){
  agg_func <- list(w=function(x)roll_meanr(abs(x), n=5), 
                   m=function(x)roll_meanr(abs(x), n=22))
  dat <- dat %>% mutate(across(-date, agg_func))
}

# find start of each time series
beginIdx <- getFirstNonNaIdx(dat)
name_dat <- names(dat)
# set beginning to beginning of highest corsi freq
if(DRF$corsi.freq!=""){
  corsi_v <- str_subset(name_dat,paste0("\\_",DRF$corsi.freq,"$"))
  for(v in corsi_v){
    beginIdx[str_which(name_dat,str_remove(v,paste0("\\_",DRF$corsi.freq,"$")))] <- beginIdx[v]
  }
  DRF$n_lags <- 1
}
# fine end of each time series
endIdx <- getLastNonNaIdx(dat)
# get the real number of n_fc
real_nfc <- pmin(endIdx-ifelse(permitSmallerW,0,DRF$window.size)-beginIdx+1,DRF$n_fc)
# keep all with positive number of real_nfc
dat <- dat[,real_nfc>0]
beginIdx <- beginIdx[real_nfc>0]
endIdx <- endIdx[real_nfc>0]
real_nfc <- real_nfc[real_nfc>0]


# get target variable names
name_dat <- names(dat)
Nasdaq100 <- read.csv("NASDAQ100Tickers.txt")$SYMBOL
target_idx <- which(name_dat %in% paste0("adjusted_",Nasdaq100))
name_target <- name_dat[target_idx]
p_target <- length(name_target)

# get vola variable names ONLY IXIC variable
name_vola <- paste0(rep(c("IXIC"),each=3),rep(c("","_w","_m"),1))

# set up for grid
gridRF <- expand.grid(num.trees=DRF$num.trees,
                      min.node.size=DRF$min.node.size,
                      mtry=DRF$mtry)
n_grid <- nrow(gridRF)

# initialize stuff
n_dat <- nrow(dat)
p_dat <- ncol(dat)
real_nfc_cumsum <- c(0,cumsum(real_nfc[name_target]))
real_nfc_sum <- as.numeric(real_nfc_cumsum[p_target+1])

# initialize result matrix
tmp_n_res <- real_nfc_sum*length(DRF$splitting.rule)*n_grid
fc_drfMat <- matrix(NA_real_,nrow = tmp_n_res, ncol = 6+ifelse(is.null(DRF$q),0,2+length(DRF$q)))
colnames(fc_drfMat) <- if(is.null(DRF$q)) c("num.trees","min.node.size","mtry","Realized","crps","PIT") else c("num.trees","min.node.size","mtry","Realized","crps","PIT","mean","sd",paste0("q",DRF$q))
fc_drfChar <- matrix(NA_character_,nrow = tmp_n_res, ncol = 3)
colnames(fc_drfChar) <- c("date","Name","Split")


# set up progress bar
pb <- progress_bar$new(total=length(DRF$splitting.rule)*p_target*n_grid+.001,
                       format = "[:bar]:percent, Time: :elapsed", 
                       clear = FALSE)
pb$tick(0)



# stopwatch
tic <- Sys.time()

# predict via drf
for(splitting.rule in DRF$splitting.rule){
  for(i in 1:p_target){
    target <- name_target[i]
    incl_var <- c("date",name_vola,str_subset(name_dat,str_remove(target,"^adjusted\\_")))
    for(grid_idx in 1:n_grid){
      fc_drf_roll <- tryCatch(fitroll(fitDRF,dat[beginIdx[target]:endIdx[target],incl_var],target,
                                      lag = DRF$n_lags, forecast.length = real_nfc[target],
                                      window.size = DRF$window.size, refit.every = DRF$refit.every,
                                      crps = TRUE, quantiles = DRF$q, abs.inputs = DRF$absolute.inputs,
                                      splitting.rule=splitting.rule,mtry=gridRF$mtry[grid_idx],
                                      min.node.size=gridRF$min.node.size[grid_idx],
                                      num.trees=gridRF$num.trees[grid_idx],
                                      compute.oob.predictions=FALSE),
                              error=function(e) data.frame(realized=NA_real_,
                                                           crps=NA_real_,
                                                           PIT=NA_real_,
                                                           mean=NA_real_,
                                                           sd=NA_real_,
                                                           q0.05=NA_real_,
                                                           q0.95=NA_real_))
      
      # assign to result matrix
      tmp_fill_idx <- (grid_idx-1)*real_nfc_sum+real_nfc_cumsum[i]+1:nrow(fc_drf_roll)
      fc_drfMat[tmp_fill_idx, 1] <- gridRF$num.trees[grid_idx]
      fc_drfMat[tmp_fill_idx, 2] <- gridRF$min.node.size[grid_idx]
      fc_drfMat[tmp_fill_idx, 3] <- gridRF$mtry[grid_idx]
      fc_drfMat[tmp_fill_idx,-(1:3)] <- fc_drf_roll%>% as.matrix()
      fc_drfChar[tmp_fill_idx,1] <- rownames(fc_drf_roll)
      fc_drfChar[tmp_fill_idx,2] <- target
      fc_drfChar[tmp_fill_idx,3] <- splitting.rule
      
      
      # update progress bar
      pb$tick()
    }
    
    # temporary save because sometimes R session is aborted
    if(i%%10==9) save(fc_drfMat,fc_drfChar,i,splitting.rule, file = "tmp_fc_drf.RData")
    
  }
  
  
  
  
}
# close
pb$terminate()
print(Sys.time()-tic)

# compute normal crps
fc_drf <- data.frame(fc_drfChar,fc_drfMat) %>% 
  mutate(date=as.Date(date),
         Name = str_remove(Name,"^adjusted\\_"),
         crps.norm = scoringRules::crps_norm(Realized,mean,sd)) %>% 
  na.omit

# save drf forecasts
save(fc_drf, file = paste0(creationDataDate,tuneModel,"DRF_Var14_FcW",DRF$window.size,
                           ifelse(permitSmallerW,"lower",""),
                           "Nfc",DRF$n_fc,"Corsi",DRF$corsi.freq,".RData"))
