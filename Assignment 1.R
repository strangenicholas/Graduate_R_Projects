library(dplyr) 
library(haven)

## Read in data 
wvs <- read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/wvs_dataset.sas7bdat")
as.data.frame(wvs)

wvs <- 
  wvs %>% 
  mutate(trust = if_else(A165 == 1,1,0), trust = replace(trust, A165 < 1, NA), 
         inc=X047CS, inc = replace(inc, inc < 1, NA),
         inc = if_else(X047CS >= 840001 & X047CS <= 840010, X047CS-840000 ,inc), 
         inc = if_else(X047CS >= 840011 & X047CS <= 840020, X047CS-840010 ,inc),
         inc = if_else(X047CS >= 840041 & X047CS <= 840050, X047CS-840040 ,inc), 
         inc = if_else(X047CS >= 840051 & X047CS <= 840060, X047CS-840050 ,inc), 
         hinc = if_else(inc >= 8,1,0)) %>%
  select(year, S003, trust, X047CS, inc, hinc) 

View(wvs)
   
summary(wvs)