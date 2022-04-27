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
library(dplyr)
library(stringr)

# set suffix for the data that is generated
creationDataDate <- "2022_04_26_"



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
  n_fc = 10,
  refit.every = 1,
  refit.window = "moving"
)

