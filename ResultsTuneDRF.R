# evaluate tuned DRF

# run config file
source("~/Documents/Masterthesis/ForecastReturnDistribution/config.R")

# import result functions
source(file.path(path$func,"func_Results.R"))

load("2022_06_11_tuneAllDRF_Var14_FcW1000Nfc250Corsim.RData")

rnd_digit <- 4

tune <- datPermitNoSmallerW(fc_drf) %>% filter(as.Date("2021-03-08")<date,date<as.Date("2022-03-04")) %>% 
  group_by(min.node.size,mtry,num.trees) %>% 
  summarise(`CRPS^{avg}`=round(mean(crps)*100,rnd_digit),
            `\\overline{L}_{0.9}`=round(mean(q0.95-q0.05)*100,rnd_digit),
            `CR_{0.9}`=round(mean((q0.05<=Realized)&(Realized<=q0.95))*100,rnd_digit),
            start=as.character(min(date)),
            end=as.character(max(date))
            ) %>%
  arrange(`CRPS^{avg}`)
colnames(tune) <- colnames(tune) %>% str_replace("^num.trees$","ntree")

# -> num.trees=1000, mtry=3, min.node.size=20
Hmisc::latex(tune,
             title="tune, ResultsTuneDRF.R",
             file=file.path(path$latexTab,"tuneAll.tex"),
             rowlabel="Rank",
             math.col.names=TRUE,
             size="small",
             center="center",
             label="Tab:RolePrereg:TuneSparseDRF",
             caption.lot="Tuning the sparse DRF model on the known test set",
             caption=paste0("Tuning the sparse DRF model on the known test set. ",
                            "The ranking is based on the average CRPS ($CRPS^{avg}$). ",
                            "$\\overline{L}_{0.9}$ indicates the average ",(q[2]-q[1])*100,
                            "\\% length of prediction intervals and $CR_{0.9}$ describes the empirical coverage of ",
                            (q[2]-q[1])*100,"\\%. All evaluation values are given in \\%."))


permitSmallerW <- FALSE

# get all forecasts in one table
fc <- joinFcDfs(creationDataDate,HS,sGarch,DRF,TRUE,outputCommonDateName = TRUE) %>% 
  lapply(., function(dat) dat %>% filter(as.Date("2022-04-01")<=date,date<=as.Date("2022-06-15")))
  
if(!permitSmallerW){
  fc <- lapply(fc, function(dat) datPermitNoSmallerW(dat))
}
load(paste0(creationDataDate,"tunedDRF_Var14_FcW",DRF$window.size,"lowerNfc",DRF$n_fc,"Corsi",DRF$corsi.freq,".RData"))
tmpTuned <- inner_join(data.frame(fc$commonDateName,Model="tunedDRF"),
                  fc_drf %>% select(date,Name,Realized,crps,PIT,q0.05,q0.95),
                  by=c("date","Name"))
load(paste0(creationDataDate,"slimDRF_Var14_FcW",DRF$window.size,"lowerNfc",DRF$n_fc,"Corsi",DRF$corsi.freq,".RData"))
tmpSlim <- inner_join(data.frame(fc$commonDateName,Model="slimDRF"),
                      fc_drf %>% select(date,Name,Realized,crps,PIT,q0.05,q0.95),
                      by=c("date","Name"))
fc <- rbind(fc$data,tmpTuned) %>% rbind(tmpSlim)

# make tables
countCrpsPerStockLower <- countCrpsPerStock(fc)
`%notin%` <- Negate(`%in%`)
countCrpsPerStockLower_slimDRF <- countCrpsPerStock(fc %>% filter(Model!="DRF") %>% mutate(Model=str_replace(Model,"^slimDRF$","DRF")))
countCrpsPerStockLower_tunedDRF <- countCrpsPerStock(fc %>% filter(Model!="DRF") %>% mutate(Model=str_replace(Model,"^tunedDRF$","DRF")))
countCrpsLower <- rbind(data.frame(Reference="DRF",countCrpsPerStockLower),
                        data.frame(Reference="slimDRF",countCrpsPerStockLower_slimDRF),
                        data.frame(Reference="tunedDRF",countCrpsPerStockLower_tunedDRF))

evalTable <- evaluationTable(fc)


Hmisc::latex(countCrpsLower,
             title="countCrpsLower, ResultsTuneDRF.R",
             file=file.path(path$latexTab,"countCrpsLowerTunedDRF.tex"),
             rowname=NULL,
             colheads = c("Reference","$\\Lambda^{hs}$","$\\Lambda^{ngarch}$","$\\Lambda^{tgarch}$","Number of Stocks","start","end"),
             caption.lot = "Describes the count ratio including tuned DRF models",
             caption = paste0("Describes the count ratio: how many time average CRPS scores of stocks are 
                              lower for the DRF in the Reference column than for any other method. All count ratios are given in \\%. 
                              A value greater the 50\\% implys a preference for the DRF method.",
                              ifelse(permitSmallerW," I allow also smaller window sizes to stick to the 102 stocks.","")),
             label="Tab:RolePrereg:CrpsCountLower",
             size="small")

Hmisc::latex(evalTable,
             title="evalTable, ResultsTuneDRF.R",
             file=file.path(path$latexTab,"evalTabTunedDRF.tex"),
             rowlabel="Rank",
             colheads = c("Model", "$CRPS^{avg}$","$CRPS^{med}$","$\\overline{L}_{0.9}$","$CR_{0.9}$","n"),
             caption.lot = "Evaluation of the four initial distributional forecast models and two adjusted DRF models on the new test set",
             caption = paste0("Evaluation of the four initial distributional forecast models and two adjusted DRF models on the new test set. 
                              The ranking is based on the average CRPS ($CRPS^{avg}$). 
                              $\\overline{L}_{0.9}$ indicates the average ",(q[2]-q[1])*100,"\\% length 
                              of prediction intervals and $CR_{0.9}$ describes the empirical coverage of ",
                              (q[2]-q[1])*100,"\\%. All evaluation values are given in \\%.",
                              ifelse(permitSmallerW," I allow also smaller window sizes to stick to the 102 stocks.","")),
             label="Tab:RolePrereg:EvalTunedDRF")
