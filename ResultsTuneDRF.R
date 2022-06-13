# evaluate tuned DRF

load("2022_06_11_tuneDRF_Var14_FcW1000Nfc250Corsim.RData")
load("2022_06_11_tuneExtensiveDRF_Var14_FcW1000Nfc250Corsim.RData")

fc_drf %>% filter(as.Date("2021-03-08")<date,date<as.Date("2022-03-04")) %>% 
  group_by(num.trees,min.node.size,mtry) %>% 
  summarise(CRPS=mean(crps),Coverage=mean((q0.05<=Realized)&(Realized<=q0.95)),L=mean(q0.95-q0.05),n()) %>% View

# -> num.trees=1000, mtry=3, min.node.size=20