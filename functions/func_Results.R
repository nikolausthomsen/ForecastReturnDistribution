# functions for Results.R

joinFcDfs <- function(creationDataDate,HS,sGarch,DRF,permitSmallerW=FALSE,outputCommonDateName=FALSE){
  commonCols <- c("date","Name")
  cols <- c(commonCols,"Realized","crps","PIT",paste0("q",HS$q))
  files <- paste0(creationDataDate,
                  c("HS","Garch","DRF_Var14"),
                  "_FcW",
                  c(HS$window.size,sGarch$window.size,DRF$window.size),
                  if(permitSmallerW) c("",rep("lower",2)) else "",
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
  if(outputCommonDateName) dat <- list(data=dat,commonDateName=commonDateName)
  return(dat)
}

countCrpsPerStock <- function(dat){
  start <- as.character(min(dat$date))
  end <- as.character(max(dat$date))
  dat %>% 
    group_by(Model,Name) %>% 
    summarise(avg.crps=mean(crps)) %>% 
    tidyr::pivot_wider(id_cols="Name", names_from = "Model", values_from = "avg.crps") %>% 
    mutate(cntHS=DRF<=HS,
           cntnGarch=DRF<=ngarch,
           cnttGarch=DRF<=tgarch) %>% 
    summarise(mean(cntHS),
              mean(cntnGarch),
              mean(cnttGarch),
              n=n()) %>% 
    mutate(across(-n, ~ round(.*100, digits = 2)), start, end)
}

datPermitNoSmallerW <- function(dat){
  ticker <- c("AMD","ADBE","ALGN","AMZN","AMGN","AEP","ADI","ANSS","AAPL","AMAT","ASML","TEAM","ADSK","ATVI" 
              ,"ADP","AVGO","BIDU","BIIB","BMRN","BKNG","CDNS","CDW","CERN","CHKP","CHTR","CPRT","CTAS","CSCO" 
              ,"CMCSA","COST","CSX","CTSH","DOCU","DXCM","DLTR","EA", "EBAY","EXC","FAST","FB", "FISV","GILD" 
              ,"GOOG","GOOGL","HON","ILMN","INCY","INTC","INTU","ISRG","MRVL","IDXX","JD", "KDP","KLAC","KHC"
              ,"LRCX","LULU","MELI","MAR","MTCH","MCHP","MDLZ","MNST","MSFT","MU", "NFLX","NTES","NVDA","NXPI" 
              ,"OKTA","ORLY","PAYX","PCAR","PYPL","PEP","QCOM","REGN","ROST","SIRI","SGEN","SPLK","SWKS","SBUX" 
              ,"SNPS","TCOM","TSLA","TXN","TMUS","VRSN","VRSK","VRTX","WBA","WDAY","XEL")
  dat <- dat %>% filter(Name %in% ticker)
  return(dat)
}

evaluationTable <- function(dat){
  dat %>% group_by(Name,Model) %>% 
    summarise(crps=mean(crps),
              avg.90QR=mean(q0.95-q0.05),
              avg.90Cover=mean((q0.05<=Realized)&(Realized<=q0.95)),
              n=n(),
              start = min(date),
              end=max(date)) %>% 
    group_by(Model) %>% 
    summarise(avg.crps=mean(crps),
              median.crps=median(crps),
              avg.90QR = mean(avg.90QR),
              avg.90Cover = round(mean(avg.90Cover)*100, digits = 2),
              n = sum(n),
              start = as.character(min(start)),
              end=as.character(max(end))) %>% 
    arrange(avg.crps) %>% 
    ungroup %>% mutate(across(-c("Model","avg.90Cover","n","start","end"), function(x) round(x, digits = 5)*100))
}