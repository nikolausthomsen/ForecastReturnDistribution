# Data scrapping of stock prices and volatility data

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import data scrap function
source(file.path(path$func,"func_dataScrap.R"))

# load packages
library(tidyr)
library(timeDate)
library(lubridate)



##### Volatility Data ######

## 1. Oxford MAN realized vola (rv)
# url of oxford man realized vola
url_oxMAN <- "https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices.zip"
# download rv vola data
volaData_rv_long <- scrapZipCsvData(url=url_oxMAN,header = TRUE)

# consider US vola only
volaData_rv_long <-  volaData_rv_long %>% 
  filter(Symbol %in% c(".DJI",".IXIC",".RUT",".SPX")) %>% 
  mutate(date = as.Date(X),
         Symbol = str_remove(Symbol,"."), .keep="unused") %>% 
  filter(date >= as.Date("2018-01-01"))

# transform to wide format
volaData_rv <- pivot_wider(volaData_rv_long, id_cols = "date",
                           names_from = "Symbol",
                           values_from = "rv5_ss")

## 2. FRED implied option (io) vola
# download io vola data
volaData_io_long <- fredr(series = paste0(c("VIX","VXN","VXD"),"CLS"),
                          observation_start = min(volaData_rv$date),
                          observation_end = Sys.Date(),
                          frequency = "d")

# transform to wide format
volaData_io <- pivot_wider(volaData_io_long, id_cols = "date",
                           names_from = "series_id",
                           values_from = "value")

## combine vola data
volaData <- full_join(volaData_rv,volaData_io, by="date")

##### Stock Data #####

# get Nasdaq100 tickers
Nasdaq100 <- read.csv("NASDAQ100Tickers.txt")

# download stock data
stockData_long <- tq_get(Nasdaq100$SYMBOL, from=min(volaData$date), to=Sys.Date())

# consider a subset of variables
stockData_long <- stockData_long %>% 
  transmute(symbol,date,adjusted,volume,
            highMlow = log(high)-log(low))

# transform to wide format
stockData <- pivot_wider(stockData_long %>% filter(!is.na(date)),
                         id_cols = date,
                         names_from = symbol,
                         values_from = c("adjusted","volume","highMlow"))



##### Combine Data sets #####

# merge data sets
dat <- full_join(volaData,stockData, by="date")

# make sure all variables are numeric
dat <- dat %>% mutate(across(-date, as.numeric))

# consider only business days
dat <- dat %>% 
  filter(
    isBizday(timeDate(date), 
             holidays = holidayNYSE(year = seq(min(year(date)),max(year(date)))),
             wday = 1:5)
    ) %>% 
  arrange(date)

# NA fraction of dat
na_frac <- rowSums(is.na(dat))/(ncol(dat)-1)

# print critical dates
if(max(na_frac)>=1) print(data.frame(dates.missing=dat$date[na_frac>=1]))

# delete rows if they just include NAs
dat <- dat %>% filter(na_frac < 1)


##### Save Data set #####

# save data
creationDataDate <- paste0(str_replace_all(Sys.Date(),"-","_"),"_")
saveRDS(dat, file=paste0(creationDataDate,"rawData.rds"))
save(creationDataDate, file="creationDataDate")
