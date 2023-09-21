library(dplyr) 
library(haven)

## Read in data 
mret <- read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat") 
as.data.frame(mret)

# Calculate EOM price
mret['Price'] <- abs(mret['PRC'])
# Adjust price for stock split
# mret['adjprice'] <- Price/CFACPR
# Get top 10 
View(head(mret, 10))

mret <-
  mret %>%
  group_by(PERMNO) %>%
  mutate(year = as.numeric(format(DATE,"%Y")),
         month = as.numeric(format(DATE,"%m")),
         PRC = abs(PRC),
         AdjPRC = PRC/CFACPR,
         mcap = PRC*SHROUT,
         mcap1 = lag(mcap, 1))

mret <- filter(mret, mcap1 != 'NA')
mret1 <- mret %>%
  arrange(month)
head(mret1)

# A) As noted above, the dataset mret7018 provides monthly stock price data over the 1970–
# 2018 (see the file mret_var for definitions). Using the price variable (find it), construct
# monthly stock returns for each stock each month based on the formula:


# Calculate return by month
mret1 <- 
  mret1 %>%
  mitate(year, )
mret1 <- mret %>%
  group_by(month,year) %>%
  summarise(EWRET = mean(RET, na.rm = TRUE),
            NRET=sum(!is.na(RET)),
            EWRETD = mean(EWRETD, na.rm = TRUE),
            VWRETD = mean(VWRETD, na.rm = TRUE),
            VWRET = weighted.mean(RET, mcap1, na.rm = TRUE))
head(mret1)



# B) Does the stock market perform differently during the month of February in a leap year
# vs. a common year? 

mret2 <- mret1 %>%
  group_by(month) %>%
  summarise(EWRETD=100*mean(EWRETD, na.rm = TRUE),
            VWRETD=100*mean(VWRETD, na.rm = TRUE))

mret2m = as.matrix(mret2)
barplot(mret2m[,2], names.arg=mret2m[,1],
        xlab="Month", ylab="Market Return", col="blue",
        main="Stock Market Seasonality", border="red")


# get leap years
leap_years <- mret %>%
  filter(format(DATE, "%m/%d") == "02/29") %>%
  distinct(year)
leap_years

leap_years = unique(leap_years$year)
leap_years


mret3 <-
  mret1 %>% filter(year  %in% (leap_years))

mret3m = as.matrix(mret3)
barplot(mret3m[,2], names.arg=mret3m[,1],
        xlab="Month", ylab="Market Return", col="blue",
        main="Stock Market Seasonality", border="red")




# B) Does the stock market perform differently during the month of February in a leap year
# vs. a common year? 