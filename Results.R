# Make nice plots and tables

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import result functions
source(file.path(func_path,"func_Results.R"))

# load packages
library(ggplot2)

# get all forecasts in one table
fc <- joinFcDfs(creationDataDate,HS,sGarch,DRF)

# make table
fc %>% 
  group_by(Model,Name) %>% 
  summarise(avg.crps=mean(crps)) %>% 
  tidyr::pivot_wider(id_cols="Name", names_from = "Model", values_from = "avg.crps") %>% 
  mutate(cntHS=DRF<=HS,
         cntnGarch=DRF<=ngarch,
         cnttGarch=DRF<=tgarch) %>% 
  summarise(mean(cntHS),
            mean(cntnGarch),
            mean(cnttGarch))
fc %>% group_by(Model) %>% 
  summarise(avg.crps=mean(crps),
            median.crps=median(crps),
            avg.90QR=mean(q0.95-q0.05),
            avg.90Cover=mean((q0.05<=Realized)&(Realized<=q0.95)),
            n=n()) %>% arrange(avg.crps)

fc %>% ggplot(aes(x=PIT)) + geom_histogram(aes(y=..density..),breaks=seq(0,1,length.out=11), alpha=.3, size=1) + 
  geom_hline(aes(yintercept=1), color="darkgrey",lty=2) + facet_wrap(vars(Model))

ggsave(file.path(latexPic_path,"PITpanel"),device = "pdf",height = 18, width=10, units="cm")
