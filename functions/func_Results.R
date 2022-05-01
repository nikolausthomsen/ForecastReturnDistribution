# functions for Results.R

joinFcDfs <- function(creationDataDate,HS,sGarch,DRF){
  commonCols <- c("date","Name")
  cols <- c(commonCols,"Realized","crps","PIT",paste0("q",HS$q))
  files <- paste0(creationDataDate,
                  c("HS","Garch","DRF_Var14"),
                  "_FcW",
                  c(HS$window.size,sGarch$window.size,DRF$window.size),
                  c("","Nfc","Nfc"),
                  c("",sGarch$n_fc,DRF$n_fc),
                  c("","",paste0("Corsi",DRF$corsi.freq)),
                  ".RData")
  
  # load all files
  for(i in 1:length(files)) load(files[i])
  # join forecasts
  commonDateName <- inner_join(fc_HS[,commonCols],fc_garch[,commonCols], by=commonCols) %>% unique()
  commonDateName <- inner_join(commonDateName,fc_drf[,commonCols], by=commonCols) %>% unique()
  date <- dplyr::intersect(as.character(fc_HS$date),as.character(fc_garch$date))
  date <- as.Date(dplyr::intersect(date,as.character(fc_drf$date)))
  dat <- rbind(dplyr::inner_join(data.frame(commonDateName,Model="HS"),fc_HS[,cols], by=commonCols),
               dplyr::inner_join(data.frame(commonDateName),fc_garch[,c("Model",cols)], by=commonCols),
               dplyr::inner_join(data.frame(commonDateName,Model="DRF"),fc_drf[,cols], by=commonCols)
               )
  
  return(dat)
}