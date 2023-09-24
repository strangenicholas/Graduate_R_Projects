library(dplyr) 
library(haven)

## Read in data 
mret <- read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat") 
as.data.frame(mret)

# A) As noted above, the dataset mret7018 provides monthly stock price data over the 1970–
# 2018 (see the file mret_var for definitions). Using the price variable (find it), construct
# monthly stock returns for each stock each month based on the formula:

# Calculate real price
mret <-
  mret %>%
  group_by(PERMNO) %>%
  mutate(year = as.numeric(format(DATE,"%Y")),
         month = as.numeric(format(DATE,"%m")),
         PRC = abs(PRC),
         AdjPRC = PRC/CFACPR,
         mcap = PRC*SHROUT,
         mcap1 = lag(mcap, 1)
         )

mret <- filter(mret, mcap1 != 'NA')
mret1 <- mret %>%
  arrange(DATE)
# head(mret1)

View(mret1)



# Calculate return by month
mret1 <- mret %>%
  group_by(PERMNO, year, month) %>%
  summarise(EWRET = mean(RET, na.rm = TRUE),
            NRET=sum(!is.na(RET)),
            EWRETD = mean(EWRETD, na.rm = TRUE),
            VWRETD = mean(VWRETD, na.rm = TRUE),
            PRC0 = lag(AdjPRC,1),
            FinPrice = ((AdjPRC - PRC0)/ PRC0),
            VWRET = weighted.mean(RET, mcap1, na.rm = TRUE))
head(mret1)

# Find the correlation of your variable with the return variable in the data (RET). Why is the correlation not 1?  
cor(mret1$EWRET, mret1$EWRETD, use = "complete.obs")

cor(mret1$VWRET, mret1$VWRETD, use= "complete.obs")

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

mret3 <- mret1 %>%
  filter(year  %in% (leap_years)) %>%
  group_by(month) %>%
  summarise(EWRETD=100*mean(EWRETD, na.rm = TRUE),
            VWRETD=100*mean(VWRETD, na.rm = TRUE))

mret3m = as.matrix(mret3)
barplot(mret3m[,2], names.arg=mret3m[,1],
        xlab="Month", ylab="Market Return", col="blue",
        main="Stock Market Seasonality", border="red")



