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