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
library(PerformanceAnalytics)

## Read in data
dret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/dret1519.sas7bdat")
as.data.frame(dret)

dret <-
  dret %>%
  filter(SHRCD %in% c(10, 11)) %>%
  mutate(
    year = as.numeric(format(DATE, "%Y")),
    month = as.numeric(format(DATE, "%m")),
    day = as.numeric(format(DATE, "%d")),
    qtr = floor(month / 4) + 1,
    PRC = abs(PRC)
  )
dret


#Extract the stocks
ibm <- filter(dret, PERMNO == '12490')
ibm <- ibm %>% transmute(
  year = year,
  month = month,
  day = day,
  DATE = DATE,
  IBMR = RET,
  IBMP = PRC
)

msft  <- filter(dret, PERMNO == '10107')
msft  <- msft %>% transmute(
  year = year,
  month = month,
  day = day,
  DATE = DATE,
  MSFTR = RET,
  MSFTP = PRC
)

gm  <- filter(dret, PERMNO == '12369')
gm  <- gm %>% transmute(
  year = year,
  month = month,
  day = day,
  DATE = DATE,
  GMR = RET,
  GMP = PRC
)


#Merge Stock returns
merged_stocks <- merge(ibm, msft, by = "DATE", all = FALSE)
merged_stocks <- merge(merged_stocks, gm, by = "DATE", all = FALSE)
merged_stocks <- merged_stocks[with(merged_stocks, order(DATE)),]

stocks <-
  merged_stocks %>%
  filter(DATE >= "2015-01-01" & DATE <= "2018-10-12")
stocks

# Get stock prices as of 10/12/2018
stockprices = filter(stocks, DATE == "2018-10-12")
stockprices

# Set positions and market value

mv <- data.frame(
  Stock = c("IBML", "MSFTL", "MSFTS", "GMS", "Cash"),
  Position = c(1.212, 3.444, -1.872, -.568, 5.380),
  Price = c(stockprices$IBMP, stockprices$MSFTP, stockprices$MSFTP, stockprices$GMP, 1)
)

mv$MarketValue <- mv$Position * mv$Price
mv$allocation <- mv$MarketValue / sum(mv$MarketValue)

# Questions: What is the total value of Fuzzy Logic’s portfolio and the weights of each asset
mv
sum(mv$MarketValue)

# Get allocation by stock
ibma <- mv$allocation[mv$Stock == "IBML"] 
msfta <- (mv$allocation[mv$Stock == "MSFTL"] + mv$allocation[mv$Stock == "MSFTS"]) 
gma <- mv$allocation[mv$Stock == "GMS"] 
casha <- mv$allocation[mv$Stock == "Cash"] 

ibma + msfta + gma + casha

##########################################

# Calculate the VaR of the porfoilo at the 5% level over the next trading day based on the

stocks <- stocks %>%
  mutate(PRET = ibma * IBMR + msfta * MSFTR + gma * GMR, 
         Percentile=ntile(PRET,100))

# 1) Variance Covariance Method (pp.8–9) and

## ESTIMATE MEAN AND STD
MSTD <- stocks %>%
  summarise(MRET = mean(PRET, na.rm = TRUE),
            SDRET=sd(PRET, na.rm = TRUE),
            NRET=sum(!is.na(PRET)))
MSTD

MSTD <- MSTD %>%
  mutate(Norm=qnorm(0.05, mean=MRET, sd=SDRET, lower.tail=TRUE))
MSTD

VaR1 <- round(abs(MSTD$Norm * sum(mv$MarketValue)),2)
print(paste("VaR =",VaR1, "Million"))

# 2) Historical Simulation Method (p. 10).

## HSM: Estimate 5 percentile
stocksP <- stocks %>% 
  arrange(PRET) %>% 
  mutate(count = row_number()) %>% 
  select(DATE, PRET, Percentile, count)
stocksP

pt <- stocksP$PRET[stocksP$count == "50"]

VaR2 <- round(abs(pt * sum(mv$MarketValue)),2)
print(paste("VaR =",VaR2, "Million"))




