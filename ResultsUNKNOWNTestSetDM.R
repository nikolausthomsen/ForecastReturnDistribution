# CRPS comparison with DM tests on UNKNOWN test set

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# get latex functions
source(file.path(str_replace(path$func,
                             "ForecastReturnDistribution",
                             "ReturnDistributionViaEnsemble"),
                 "func_Latex.R"))
source(file.path(path$func,"func_Results.R"))

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
ntest <- 4
rnd_digit <- 4
fc <- fc %>% tidyr::pivot_wider(id_cols=c("date","Name"),names_from = "Model", values_from = "crps")
crps_tab <- fc %>% group_by(Name) %>% 
  summarise(across(-c("date","DRF"),
                   function(x){
                     tmp <- dmtest(x,DRF)
                     addSignificanceStars(tmp$mu*100,tmp$pval,bold.if.lower0 = T,ntest = ntest)
                   }),
            DRF=addSignificanceStars(mean(DRF)*100,1,bold.if.lower0 = T,ntest = ntest),
            start = as.character(min(date)),
            end = as.character(max(date))) %>% 
  rbind(fc %>% group_by(date) %>% summarise(across(-Name, mean)) %>% 
          summarise(across(-c("date","DRF"),
                           function(x){
                             tmp <- dmtest(x,DRF)
                             addSignificanceStars(tmp$mu*100,tmp$pval,bold.if.lower0 = T,ntest = ntest)
                           }),
                    DRF=addSignificanceStars(mean(DRF)*100,1,bold.if.lower0 = T,ntest = ntest),
                    start = as.character(min(date)),
                    end = as.character(max(date)),
                    Name="All"))


# print latex table
Hmisc::latex(crps_tab[,c("Name","DRF","tgarch","ngarch","HS","start","end")],
      rowname=NULL, 
      file = paste0(path$latexTab,"/",creationDataDate,"CRPS_AllunknownTest.tex"),
      longtable = T,
      colheads = c("Name",
                   "$DRFVar14^{cart, m}_{w=1000}$",
                   "$\\Delta tgarch^{df=8}_{w=1000}$",
                   "$\\Delta ngarch$",
                   "$\\Delta HS_{w=250}$",
                   "$start$",
                   "$end$"),
      caption.lot = "Comparison of the best DRF model with the best benchmarks on unknown test data",
      caption = paste0("Comparison of the best DRF model with the best benchmarks on unknown test data.",
                       " The numbers after DRFVar indicate the variable subsets labeled in Chapter \\ref{ch:Data}. ",
                       "$\\Delta model_2$ indicates the difference in CRPS from the closest left CRPS $model_1$, hence $\\Delta model_2= model_2-model_1$. ",
                       "All CRPS scores are multiplied by 100. A negative value means that $model_2$ performs better ",
                       "compared to $model_1$ and is printed in bold. ",
                       "I also report the significance stars arising from a Diebold-Mariano test ",
                       "($^{***}$ Pvalue$<0.1\\%$,$^{**}$ Pvalue$<1\\%$, $^{*}$ Pvalue$<5\\%$,$^{.}$ Pvalue$<10\\%$). ",
                       "Note that the Pvalues are adjusted by Bonferroni correction."),
      label = "Tab:CRPS_AllunknownTest",
      size = "footnotesize")
