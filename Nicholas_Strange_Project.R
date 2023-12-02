# Project 

library(dplyr)
library(haven)

## Read in Market Return data
mret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat")
as.data.frame(mret)

mret <- filter(mret, SHRCD %in% c(10,11)) 

# Remove duplicate records
mret %>% distinct()

# Read in Names data
names <-
  read.csv("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/Names.csv")
as.data.frame(names)

# Get top 10 names

# Get distinct company names
compnames <- unique(mret[["COMNAM"]])

compnames


matches <- data_frame()

#Psudo code
# for name in names:
#   if str names = colohmn in compnames:
#   matches.append compnames[comnam]
#   if substr(STR,a,b) in colohmn in compnames:
#   matches.append compnames[comnam]:
#   exc
# 
# 
# get distinct matches
# 
# nomatch <-  unique names that are not in matches
  



