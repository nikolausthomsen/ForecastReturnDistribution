# Make nice plots and tables

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import result functions
source(file.path(path$func,"func_Results.R"))

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
countCrpsPerStockLower <- countCrpsPerStock(fc)

evalTable <- evaluationTable(fc)

# save tables
Hmisc::latex(evalTable,
             rowlabel="Rank",
             colheads = c("Model", "$CRPS^{avg}$","$CRPS^{med}$","$\\overline{L}_{0.9}$","$CR_{0.9}$","n","$start$","$end$"),
             caption.lot = "Evaluation of the four distributional forecast models on the new test set",
             caption = paste0("Evaluation of the four distributional forecast models on the new test set. 
                              The ranking is based on the average CRPS ($CRPS^{avg}$). 
                              $\\overline{L}_{0.9}$ indicates the average ",(q[2]-q[1])*100,"\\% length 
                              of prediction intervals and $CR_{0.9}$ describes the empirical coverage of ",
                              (q[2]-q[1])*100,"\\%. All evaluation values are given in \\%.",
                              ifelse(permitSmallerW," I allow also smaller window sizes to stick to the 102 stocks.","")),
             file=file.path(path$latexTab,paste0(creationDataDate,"EvalTab",ifelse(permitSmallerW,"Wlower",""),".tex")),
             label = paste0("Tab:FinalEval", ifelse(permitSmallerW,"Wlower","")))
Hmisc::latex(countCrpsPerStockLower,
             rowname=NULL,
             colheads = c("$\\Lambda^{hs}$","$\\Lambda^{ngarch}$","$\\Lambda^{tgarch}$","Number of Stocks","$start$","$end$"),
             caption = paste0("Describes the count ratio: how many time average CRPS scores of stocks are 
                              lower for DRF than for any other method. All count ratios are given in \\%. 
                              A value greater the 50\\% implys a preference for the DRF method.",
                              ifelse(permitSmallerW," I allow also smaller window sizes to stick to the 102 stocks.","")),
             file=file.path(path$latexTab,paste0(creationDataDate,"CountCrpsGtDrf",ifelse(permitSmallerW,"Wlower",""),".tex")),
             label = paste0("Tab:FinalCntCrpsPerStock", ifelse(permitSmallerW,"Wlower","")))

fc %>% ggplot(aes(x=PIT)) + geom_histogram(aes(y=..density..),breaks=seq(0,1,length.out=11), alpha=.3, size=1) + 
  geom_hline(aes(yintercept=1), color="darkgrey",lty=2) + facet_wrap(vars(Model))

ggsave(file.path(path$latexPic,paste0(creationDataDate,"PITpanel",ifelse(permitSmallerW,"Wlower",""),".pdf")),device = "pdf",height = 11, width=10, units="cm")
