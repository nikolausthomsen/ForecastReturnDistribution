# Historic Simulation

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import prediction and evaluation functions
source(file.path(func_path,"func_Pred.R"))

# load packages
library(tidyr)
library(scoringRules)
library(RcppRoll)
library(roll)

# read data
dat <- readRDS(paste0(creationDataDate,"NAFilledData.rds"))

# get column names
name_dat <- names(dat)
# get target variables
first_target_idx <- which("adjusted_AMD"== name_dat)
last_target_idx  <- which("adjusted_ZM" == name_dat)

# cut data to target variables and transform to wide format
dat <- dat %>% 
  select(date,all_of(first_target_idx:last_target_idx)) %>% 
  pivot_longer(-date, names_to = "Name", values_to = "Realized") %>% 
  na.omit %>% 
  mutate(Name = str_remove(Name, "^adjusted\\_")) %>% 
  arrange(Name,date)

# predict via hs
for(w in HS$window.size){
  fc_HS <- dat %>% 
    group_by(Name) %>% 
    mutate(
      mean = lag(roll_meanr(Realized,n=w,fill=NA,na.rm=T),HS$h),
      sd   = lag(roll_sdr(  Realized,n=w,fill=NA,na.rm=T),HS$h),
      q1   = lag(roll_quantile(Realized, width=w, p=HS$q[1])),
      q2   = lag(roll_quantile(Realized, width=w, p=HS$q[2])),
      crps = lag(roll_func(Realized, width=w, crps_sample)),
      PIT  = lag(roll_func(Realized, width=w, pit))
    ) %>% 
    na.omit() %>% ungroup()
  colnames(fc_HS)[6:7] <- paste0("q",HS$q)
  save(fc_HS, file = paste0(creationDataDate,"HS_FcW",w,".RData"))
}
