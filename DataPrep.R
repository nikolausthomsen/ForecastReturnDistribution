# Data transformation and NA handling

# run config file
source("~/ForecastReturnDistribution/config.R")

# import preparation functions
source(file.path(path$func,"func_DataPrep.R"))

# load packages
library(tidyr)

# read data
dat <- readRDS(paste0(creationDataDate,"rawData.rds"))



##### Transform Data #####

# get column names
name_dat <- names(dat)
# get all adjusted_ ... names
name_adj <- str_subset(name_dat,"adjusted\\_")
# get all volume_ ... names
name_vol <- str_subset(name_dat,"volume\\_")

# transform with diff log x_t (t5 function)
dat <- dat %>% mutate(across(all_of(c(name_adj,name_vol)), t5))



##### NA handling #####

# get information how good is the quality of the time series
tsQuality <- TSnaInfo(dat,"date")


if(max(tsQuality$numNAbtwTSstartend)>0){
  # names of NA columns
  name_NA <- rownames(tsQuality)[tsQuality$numNAbtwTSstartend>0]
  
  # console output of quality
  cat("Fill missing values for:           ",
      paste(rownames(tsQuality)[tsQuality$numNAbtwTSstartend>0], collapse = ", "),
      "\n",
      "Maximal imputation:                ",
      max(tsQuality$numNAbtwTSstartend),
      "\n", 
      "Maximal number of consecutive NAs: ",
      max(tsQuality$numConsNAbtwTSstartend), "\n", sep="")
  
  # impute missing values with most recent observation
  dat <- dat %>% arrange(date) %>% 
    fill(all_of(name_NA))
  
  # check if imputation is complete
  tsFillQuality <- TSnaInfo(dat,"date")
  if( any(tsFillQuality$numNAbtwTSstartend!=0) ) stop("Something went wrong with NA filling")
}



##### Save Data #####

saveRDS(dat, file=paste0(creationDataDate,"NAFilledData.rds"))

