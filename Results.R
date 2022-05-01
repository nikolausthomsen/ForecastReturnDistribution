# Make nice plots and tables

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import result functions
source(file.path(func_path,"func_Results.R"))

# load packages
library(ggplot2)

# get all forecasts in one table
fc <- joinFcDfs(creationDataDate,HS,sGarch,DRF)

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
            `Number of Stocks`=n()) %>% 
  mutate(across(-`Number of Stocks`, ~ round(.*100, digits = 2)))
evaluationTable <- fc %>% group_by(Name,Model) %>% 
  summarise(avg.crps=mean(crps),
            avg.90QR=mean(q0.95-q0.05),
            avg.90Cover=mean((q0.05<=Realized)&(Realized<=q0.95)),
            n=n()) %>%
  group_by(Model) %>% 
  summarise(avg.crps=mean(avg.crps),
            median.crps=median(avg.crps),
            avg.90QR = mean(avg.90QR),
            avg.90Cover = round(mean(avg.90Cover)*100, digits = 2),
            n = sum(n)) %>% 
  arrange(avg.crps) %>% 
  ungroup %>% mutate(across(-c("Model","avg.90Cover","n"), function(x) round(x, digits = 5)*100))

# save tables
Hmisc::latex(evaluationTable,
             rowlabel="Rank",
             file=file.path(latexTab_path,paste0(creationDataDate,"EvalTab.tex")))
Hmisc::latex(countCrpsPerStock,
             rowname=NULL,
             file=file.path(latexTab_path,paste0(creationDataDate,"CountCrpsGtDrf.tex")))

fc %>% ggplot(aes(x=PIT)) + geom_histogram(aes(y=..density..),breaks=seq(0,1,length.out=11), alpha=.3, size=1) + 
  geom_hline(aes(yintercept=1), color="darkgrey",lty=2) + facet_wrap(vars(Model))

ggsave(file.path(latexPic_path,"PITpanel"),device = "pdf",height = 18, width=10, units="cm")
