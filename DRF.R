# DRF Model

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import prediction and evaluation functions
source(file.path(func_path,"func_Pred.R"))

# load packages
library(RcppRoll)
#library(progress)
#library(foreach)

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
nfc_idx_helper <- c(0,cumsum(real_nfc[name_target]))

# initialize result matrix
tmp_n_res <- real_nfc_sum*length(DRF$splitting.rule)
fc_drfMat <- matrix(NA_real_,nrow = tmp_n_res, ncol = 3+ifelse(is.null(DRF$q),0,2+length(DRF$q)))
colnames(fc_drfMat) <- if(is.null(DRF$q)) c("realized","crps","PIT") else c("realized","crps","PIT","mean","sd",paste0("q",DRF$q))
fc_drfChar <- matrix(NA_character_,nrow = tmp_n_res, ncol = 3)
colnames(fc_drfChar) <- c("date","Name","Split")

