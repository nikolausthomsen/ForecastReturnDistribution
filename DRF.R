# DRF Model

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import prediction and evaluation functions
source(file.path(func_path,"func_Pred.R"))

# load packages
library(RcppRoll)
library(progress)
library(foreach)

# read data
dat <- readRDS(paste0(creationDataDate,"NAFilledData.rds"))

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
# get the real number of n_fc
real_nfc <- pmin(nrow(dat)-DRF$window.size-beginIdx+1,DRF$n_fc)
# keep all with positive number of real_nfc
dat <- dat[,real_nfc>0]
beginIdx <- beginIdx[real_nfc>0]
real_nfc <- real_nfc[real_nfc>0]


# get target variable names
name_dat <- names(dat)
Nasdaq100 <- read.csv("NASDAQ100Tickers.txt")$SYMBOL
target_idx <- which(name_dat %in% paste0("adjusted_",Nasdaq100))
name_target <- name_dat[target_idx]
p_target <- length(name_target)

# initialize stuff
n_dat <- nrow(dat)
p_dat <- ncol(dat)
real_nfc_sum <- sum(real_nfc[name_target])

# initialize result matrix
tmp_n_res <- real_nfc_sum*length(DRF$splitting.rule)
fc_drfMat <- matrix(NA_real_,nrow = tmp_n_res, ncol = 3+ifelse(is.null(DRF$q),0,2+length(DRF$q)))
colnames(fc_drfMat) <- if(is.null(DRF$q)) c("realized","crps","PIT") else c("realized","crps","PIT","mean","sd",paste0("q",DRF$q))
fc_drfChar <- matrix(NA_character_,nrow = tmp_n_res, ncol = 3)
colnames(fc_drfChar) <- c("date","Name","Split")

# count number of nonparallel loops
cnt <- 1

# parallel computation
cl <-  makeCluster(detectCores()-4, outfile=paste0(creationDataDate,"LOG_Fc_Window",DRF$window.size,"_Nfc",DRF$n_fc,".txt"))
doSNOW::registerDoSNOW(cl)

# set up progress bar
pb <- progress_bar$new(total=length(DRF$splitting.rule)*p_target+.001,
                       format = "[:bar]:percent, Time: :elapsed", 
                       clear = FALSE)
pb$tick(0)
ots <- list(progress=function() pb$tick())

# stopwatch
tic <- Sys.time()

# predict via drf
for(splitting.rule in DRF$splitting.rule){
  fc_drf_roll <- tryCatch(
    foreach(i = 1:p_target,.packages = c("dplyr","stringr"),
            .combine = "rbind",.options.snow=ots) %dopar% {
      target <- name_target[i]
      incl_var <- c("date",str_subset(name_dat,str_remove(target,"^adjusted\\_")))
      tryCatch(data.frame(
        fitroll(fitDRF,dat[beginIdx[target]:n_dat,incl_var],target,
                lag = DRF$n_lags, forecast.length = real_nfc[target],
                window.size = DRF$window.size, refit.every = DRF$refit.every,
                crps = TRUE, quantiles = DRF$q, abs.inputs = DRF$absolute.inputs,
                splitting.rule=splitting.rule),
        Name = target),
        error=function(e){print(e); NULL}
      )
    }, error=function(e){print(e); NULL}
  )
  
  # assign to result matrix
  if(!is.null(fc_drf_roll)){
    tmp_fill_idx <- ((cnt-1)*real_nfc_sum+1):((cnt-1)*real_nfc_sum+nrow(fc_drf_roll))
    fc_drfMat[tmp_fill_idx,  ] <- fc_drf_roll %>% select(-Name) %>% as.matrix()
    fc_drfChar[tmp_fill_idx,1] <- substr(rownames(fc_drf_roll),1,10)
    fc_drfChar[tmp_fill_idx,2] <- fc_drf_roll$Name
    fc_drfChar[tmp_fill_idx,3] <- splitting.rule
  }
  
  # update counter
  cnt <- cnt + 1
}
# close
stopCluster(cl)
pb$terminate()
print(Sys.time()-tic)

# compute normal crps
fc_drf <- data.frame(fc_drfChar,fc_drfMat) %>% 
  mutate(date=as.Date(date),
         Name = str_remove(Name,"^adjusted\\_"),
         crps.norm = scoringRules::crps_norm(realized,mean,sd)) %>% 
  na.omit

# save drf forecasts
save(fc_drf, file = paste0(creationDataDate,"DRF_Var_Fc_Window",DRF$window.size,
                           "_Nfc",n_fc,"_Corsi",DRF$corsi.freq,"_Var14.RData"))