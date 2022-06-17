# main file to run all models

# path for the main files
path <- list(coding="~/Documents/Masterthesis/ForecastReturnDistribution")

# scrap data
source(file.path(path$coding,"DataScrap.R"))

# prepare data
source(file.path(path$coding,"DataPrep.R"))

# predict via historic simulation
source(file.path(path$coding,"HS.R"))

# predict via garch model
source(file.path(path$coding,"Garch.R"))

# predict via drf
source(file.path(path$coding,"DRF.R"))

# evaluate results
source(file.path(path$coding,"Results.R"))
