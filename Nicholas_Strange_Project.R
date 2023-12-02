# Project 

library(dplyr)
library(haven)

## Read in data
mret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat")
as.data.frame(mret)