#Assignment 4
# Portfolio of 3 assets. Calculate mean and 
# standard variation – both analytically and through historical simulation. Over a period. 

# At the market closing on October 12, 2018, Fuzzy Logic Co. had the following outstanding 
# positions: 
#   • A long position in 1.212 mil. shares of IBM stock (PERMNO = 12490) 
# • A long position in 3.444 mil. shares of MSFT stock (PERMNO = 10107) 
# • A short position in 1.872 mil. shares of MSFT stock (PERMNO = 10107) 
# • A short position in 0.568 mil. shares of GM stock (PERMNO = 12369) 
# • $5.380 mil. in cash

library(dplyr) 
library(haven)
library(ggplot2) 

## Read in data
dret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/dret1519.sas7bdat")
as.data.frame(dret)
