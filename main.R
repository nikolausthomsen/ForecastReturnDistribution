# main file to run all models

# path for the main files
coding_path <- "~/Documents/Masterthesis/ForecastReturnDistribution"

# scrap data
source(file.path(coding_path,"DataScrap.R"))

# prepare data
source(file.path(coding_path,"DataPrep.R"))

# predict via historic simulation
source(file.path(coding_path,"HS.R"))

# predict via garch model
source(file.path(coding_path,"Garch.R"))

# predict via drf
source(file.path(coding_path,"DRF.R"))
