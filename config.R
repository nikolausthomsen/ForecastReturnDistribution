# Configuration file

# clear all
rm(list=ls())
graphics.off()

# set path
input_path <- "/Users/justusthomsen/Documents/Masterthesis/Data/ForecastReturnDistribution"
func_path <- "/Users/justusthomsen/Documents/Masterthesis/ForecastReturnDistribution/functions"
coding_path <- "/Users/justusthomsen/Documents/Masterthesis/ForecastReturnDistribution"

# set working directory
setwd(input_path)

# packages that are always required
library(dplyr, quietly = TRUE)
library(stringr)

# set suffix for the data that is generated
creationDataDate <- "2022_04_26_"

# set number of forecasts
n_fc <- sum(timeDate::isBizday(
  timeDate::timeDate(seq.Date(from = as.Date("2022-04-01"), 
                              to = as.Date(str_replace_all(creationDataDate,"\\_","-")),
                              by=1))))



##### Historic Simulation #####

HS <- list(
  window.size = 250,
  q = c(.05,.95),
  h = 1
)



##### GARCH Models #####

sGarch <- list(
  armaOrder = c(0,0),
  model = "sGARCH",
  garchOrder = c(1,1),
  shape = 8,
  window.size = 1e3,
  n_fc = n_fc,
  refit.every = 1,
  refit.window = "moving"
)



##### DRF Model #####

DRF <- list(
  splitting.rule = "CART",
  window.size = 1e3,
  n_fc = n_fc,
  refit.every = 1,
  n_lags = 3, # note that n_lags=1 if corsi.freq!="" automatically
  absolute.inputs = TRUE,
  corsi.freq = "m",
  q = c(.05,.95)
)