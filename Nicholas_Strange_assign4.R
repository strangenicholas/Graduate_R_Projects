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

# Questions: What is the total value of Fuzzy Logic’s portfolio and the weights of each asset 
# in the portfolio at the end of the trading day? 
# Calculate the VaR of the porfoilo at the 5% level over the next trading day based on the 
# 1) Variance Covariance Method (pp.8–9) and 
# 2) Historical Simulation Method (p. 10). 

library(dplyr) 
library(haven)
library(ggplot2) 

## Read in data
dret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/dret1519.sas7bdat")
as.data.frame(dret)

dret <- 
  dret %>% 
  filter(SHRCD %in% c(10,11)) %>% 
  mutate(year = as.numeric(format(DATE,"%Y")), 
         month = as.numeric(format(DATE,"%m")), 
         day = as.numeric(format(DATE,"%d")), 
         qtr = floor(month/4) + 1, 
         PRC = abs(PRC))
dret


#Extract the stocks 
IBM  <- filter(dret, PERMNO== '12490')   
IBM  <- IBM %>% transmute(year=year, month=month, day=day, 
                          DATE=DATE, IBMR=RET, IBMP=PRC) 

MSFT  <- filter(dret, PERMNO== '10107')   
MSFT  <- MSFT %>% transmute(year=year, month=month, day=day, 
                          DATE=DATE, MSFTR=RET, MSFTP=PRC)

GM  <- filter(dret, PERMNO== '12369')   
GM  <- GM %>% transmute(year=year, month=month, day=day, 
                            DATE=DATE, IBMR=RET, IBMP=PRC)


#Merge Stock returns 
merged_stocks <- merge(IBM, MSFT, by = "DATE", all = FALSE)
merged_stocks <- merge(merged_stocks, GM, by = "DATE", all = FALSE)
merged_stocks <- merged_stocks[with(merged_stocks, order(DATE)), ]  

stocks <- 
  merged_stocks %>%
  filter(DATE >= "2015-01-01" & DATE <= "2018-10-12")
stocks 

# Get stock prices as of 10/12/2018
stockprices = filter(stocks, DATE == "2018-10-12")
stockprices

mv <- ((1.212 * stockprices$ibmp) + (3.444 * stockprices$msftp) - (1.872 * stockprices$msftp) + (0.568 * stockprices$gmp) +(5.380))
mv

#Construct the trading book historical returns 
stocks <- stocks %>% 
  mutate(PRET=0.85*IBMR+0.14*RM, 
         Percentile=ntile(PRET,100)) 