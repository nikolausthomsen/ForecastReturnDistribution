# Make nice plots and tables

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import result functions
source(file.path(func_path,"func_Results.R"))

# load packages
library(ggplot2)

# get all forecasts in one table
fc <- joinFcDfs(creationDataDate,HS,sGarch,DRF,TRUE) %>% 
  filter(as.Date("2022-04-01")<=date,date<=as.Date("2022-06-15"))
if(!permitSmallerW){
  ticker <- c("AMD","ADBE","ALGN","AMZN","AMGN","AEP","ADI","ANSS","AAPL","AMAT","ASML","TEAM","ADSK","ATVI" 
    ,"ADP","AVGO","BIDU","BIIB","BMRN","BKNG","CDNS","CDW","CERN","CHKP","CHTR","CPRT","CTAS","CSCO" 
    ,"CMCSA","COST","CSX","CTSH","DOCU","DXCM","DLTR","EA", "EBAY","EXC","FAST","FB", "FISV","GILD" 
    ,"GOOG","GOOGL","HON","ILMN","INCY","INTC","INTU","ISRG","MRVL","IDXX","JD", "KDP","KLAC","KHC"
    ,"LRCX","LULU","MELI","MAR","MTCH","MCHP","MDLZ","MNST","MSFT","MU", "NFLX","NTES","NVDA","NXPI" 
    ,"OKTA","ORLY","PAYX","PCAR","PYPL","PEP","QCOM","REGN","ROST","SIRI","SGEN","SPLK","SWKS","SBUX" 
    ,"SNPS","TCOM","TSLA","TXN","TMUS","VRSN","VRSK","VRTX","WBA","WDAY","XEL")
  fc <- fc %>% filter(Name %in% ticker)
}

# make tables
countCrpsPerStock <- fc %>% 
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
  mutate(across(-n, ~ round(.*100, digits = 2)))
evaluationTable <- fc %>% group_by(Name,Model) %>% 
  summarise(crps=mean(crps),
            avg.90QR=mean(q0.95-q0.05),
            avg.90Cover=mean((q0.05<=Realized)&(Realized<=q0.95)),
            n=n()) %>% 
  group_by(Model) %>% 
  summarise(avg.crps=mean(crps),
            median.crps=median(crps),
            avg.90QR = mean(avg.90QR),
            avg.90Cover = round(mean(avg.90Cover)*100, digits = 2),
            n = sum(n)) %>% 
  arrange(avg.crps) %>% 
  ungroup %>% mutate(across(-c("Model","avg.90Cover","n"), function(x) round(x, digits = 5)*100))

# save tables
Hmisc::latex(evaluationTable,
             rowlabel="Rank",
             colheads = c("Model", "$CRPS^{avg}$","$CRPS^{med}$","$\\overline{L}_{0.9}$","$CR_{0.9}$","n"),
             caption.lot = "Evaluation of the four distributional forecast models on the new test set",
             caption = paste0("Evaluation of the four distributional forecast models on the new test set. 
                              The ranking is based on the average CRPS ($CRPS^{avg}$). 
                              $\\overline{L}_{0.9}$ indicates the average ",(q[2]-q[1])*100,"\\% length 
                              of prediction intervals and $CR_{0.9}$ describes the empirical coverage of ",
                              (q[2]-q[1])*100,"\\%. All evaluation values are given in \\%.",
                              ifelse(permitSmallerW," I allow also smaller window sizes to stick to the 102 stocks.","")),
             file=file.path(latexTab_path,paste0(creationDataDate,"EvalTab",ifelse(permitSmallerW,"Wlower",""),".tex")),
             label = paste0("Tab:FinalEval", ifelse(permitSmallerW,"Wlower","")))
Hmisc::latex(countCrpsPerStock,
             rowname=NULL,
             colheads = c("$\\Lambda^{hs}$","$\\Lambda^{ngarch}$","$\\Lambda^{tgarch}$","Number of Stocks"),
             caption = paste0("Describes the count ratio: how many time average CRPS scores of stocks are 
                              lower for DRF than for any other method. All count ratios are given in \\%. 
                              A value greater the 50\\% implys a preference for the DRF method.",
                              ifelse(permitSmallerW," I allow also smaller window sizes to stick to the 102 stocks.","")),
             file=file.path(latexTab_path,paste0(creationDataDate,"CountCrpsGtDrf",ifelse(permitSmallerW,"Wlower",""),".tex")),
             label = paste0("Tab:FinalCntCrpsPerStock", ifelse(permitSmallerW,"Wlower","")))

fc %>% ggplot(aes(x=PIT)) + geom_histogram(aes(y=..density..),breaks=seq(0,1,length.out=11), alpha=.3, size=1) + 
  geom_hline(aes(yintercept=1), color="darkgrey",lty=2) + facet_wrap(vars(Model))

ggsave(file.path(latexPic_path,paste0(creationDataDate,"PITpanel",ifelse(permitSmallerW,"Wlower",""),".pdf")),device = "pdf",height = 11, width=10, units="cm")
