# Garch Models

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import prediction and evaluation functions
source(file.path(func_path,"func_Pred.R"))

# load packages
library(rugarch)
library(xts)
library(progress)
library(scoringRules)

# specify model parameters
spec_sGarch <- ugarchspec(
  mean.model = sGarch["armaOrder"],
  variance.model = sGarch[c("model","garchOrder")],
  distribution.model = "norm"
)
spec_tGarch <- ugarchspec(
  mean.model = sGarch["armaOrder"],
  variance.model = sGarch[c("model","garchOrder")],
  distribution.model = "std",
  fixed.pars = sGarch["shape"]
)
n_spec <- 2

# maximal window.size
w_max <- max(sGarch$window.size)

# read data
dat <- readRDS(paste0(creationDataDate,"NAFilledData.rds"))

# get column names
name_dat <- names(dat)
# get target variables
first_target_idx <- which("adjusted_AMD"== name_dat)
last_target_idx  <- which("adjusted_ZM" == name_dat)

# cut data to target variables
dat <- xts(x = dat %>% select(all_of(first_target_idx:last_target_idx)),
           order.by = dat$date)

# find start of each series
beginIdx <- getFirstNonNaIdx(dat)
# get the real number of n_fc
real_nfc <- pmin(nrow(dat)-w_max-beginIdx+1,sGarch$n_fc)
# keep all with positive number of real_nfc
dat <- dat[,real_nfc>0]
beginIdx <- beginIdx[real_nfc>0]
real_nfc <- real_nfc[real_nfc>0]


# initialize stuff
name_dat <- names(dat)
n_dat <- nrow(dat)
p_dat <- ncol(dat)
real_nfc_sum <- sum(real_nfc)
nfc_idx_helper <- c(0,cumsum(real_nfc))

# number of available clusters
cl <- makeCluster(detectCores()-2)
# iterate through different window sizes
for(w in sGarch$window.size){
  # initialize memory
  fc_garchMat  <- matrix(NA_real_,     nrow = real_nfc_sum*n_spec, ncol = 4)
  fc_garchChar <- matrix(NA_character_,nrow = real_nfc_sum*n_spec, ncol = 3)
  colnames(fc_garchMat)  <- c("Mu","Sigma","Shape","Realized")
  colnames(fc_garchChar) <- c("date","Model","Name")
  
  # stopwatch
  tic <- Sys.time()
  # progress bar
  pb <- progress_bar$new(total = p_dat*n_spec+.001,
                         format = "[:bar]:percent, Time: :elapsed",
                         clear = FALSE)
  pb$tick(0)
  
  for(spec_idx in 1:n_spec){
    if(spec_idx==1){
      tmp_model <- "ngarch"
      garchspec <- spec_sGarch
    }else{
      tmp_model <- "tgarch"
      garchspec <- spec_tGarch
    }
    tmp_idx_shift <- (spec_idx-1)*real_nfc_sum
    
    pb$message(paste("- ", tmp_model ," ____________________________________ ", sep=""))
    
    # loop over all target variables
    for(target_idx in 1:p_dat){
      # predict via garch
      tmp <- garch(
        spec = garchspec,
        data = dat[beginIdx[target_idx]:n_dat,target_idx],
        forecast.length = real_nfc[target_idx],
        refit.every = sGarch$refit.every,
        refit.window = sGarch$refit.window,
        window.size = w,
        calculate.VaR = FALSE,
        keep.coef = FALSE,
        cluster = cl
      )
      
      # update progress bar
      pb$tick()
      
      tmp_idx <- (nfc_idx_helper[target_idx]+1):nfc_idx_helper[target_idx+1] + tmp_idx_shift
      # set the name and the model
      fc_garchChar[tmp_idx,"Name" ] <- name_dat[target_idx]
      fc_garchChar[tmp_idx,"Model"] <- tmp_model
      if( is.null(tmp) ) next
      fc_garchChar[tmp_idx,"date"] <- rownames(tmp)
      fc_garchMat[ tmp_idx,      ] <- as.matrix(tmp)
    }
    # stopwatch per model
    pb$message(paste("Elapsed:",round(difftime(Sys.time(),tic, units = "mins"),2),"mins"))
    tic <- Sys.time()
  }
  # close progress bar
  pb$terminate()
  # combine dataframes
  fc_garch <- data.frame(fc_garchChar,fc_garchMat)
  rm(fc_garchChar,fc_garchMat)
  
  # evaluate forecasts
  fc_garch <- fc_garch %>% 
    mutate(
      date = as.Date(date),
      Name = str_remove(Name,"^adjusted\\_"),
      crps = if_else(str_detect(Model,"ngarch"),
                     crps_norm(Realized,Mu,Sigma),
                     crps_t(Realized,Shape,Mu,Sigma)),
      PIT = if_else(str_detect(Model,"ngarch"),
                    pnorm(Realized,Mu,Sigma),
                    pdist("std",Realized,Mu,Sigma,shape=Shape)),
      q1 = if_else(str_detect(Model,"ngarch"),
                   qnorm(sGarch$q[1],Mu,Sigma),
                   qdist("std",sGarch$q[1],Mu,Sigma,shape = Shape)),
      q2 = if_else(str_detect(Model,"ngarch"),
                   qnorm(sGarch$q[2],Mu,Sigma),
                   qdist("std",sGarch$q[2],Mu,Sigma,shape = Shape))
    )
  colnames(fc_garch)[ncol(fc_garch)-1:0] <- paste0("q",sGarch$q)
  
  # save garch forecasts
  save(fc_garch, file = paste0(creationDataDate,"Garch_FcW",w,"Nfc",sGarch$n_fc,".RData"))
}

# close clusters
stopCluster(cl)
