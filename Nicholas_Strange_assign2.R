library(dplyr) 
library(haven)

## Read in data 
mret <- read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat") 
as.data.frame(mret)

mret <- filter(mret, SHRCD %in% c(10,11)) 

# Remove duplicate records
mret %>% distinct()

# A) As noted above, the dataset mret7018 provides monthly stock price data over the 1970–
# 2018 (see the file mret_var for definitions). Using the price variable (find it), construct
# monthly stock returns for each stock each month based on the formula:


# Calculate real price and portfolio returns
mret <-
  mret %>%
  group_by(PERMNO) %>%
  mutate(year = as.numeric(format(DATE,"%Y")),
         month = as.numeric(format(DATE,"%m")),
         # Get absolute value of PRC
         PRC = abs(PRC),
         # Adjust PRC for splits, spinoffs, and dividends
         AdjPRC = PRC/CFACPR,
         # Get PRC of previous month
         PRC0 = lag(AdjPRC,1),
         # Calculate return 
         FRET = ((AdjPRC - PRC0)/ PRC0),
         mcap = PRC*SHROUT,
         mcap1 = lag(mcap, 1)
         )

# Only include rows where market cap of the previous month is not NA
mret <- filter(mret, mcap1 != 'NA')
mret <- mret %>%
  arrange(DATE)

View(mret)

# Find the correlation of your variable with the return variable in the data (RET). Why is the correlation not 1?  
cor(mret$FRET,mret$RET, use="pairwise.complete.obs")


# B) Does the stock market perform differently during the month of February in a leap year
# vs. a common year? 
mret <- 
  mret %>% 
  mutate(year = as.numeric(format(DATE,"%Y")), 
         month = as.numeric(format(DATE,"%m"))) 

mret1 <- mret %>%
  group_by(month) %>%
  summarise(EWRETD=100*mean(EWRETD, na.rm = TRUE),
            VWRETD=100*mean(VWRETD, na.rm = TRUE))

mret1m = as.matrix(mret1)
barplot(mret1m[,2], names.arg=mret1m[,1],
        xlab="Month", ylab="Market Return", col="blue",
        main="Stock Market Seasonality All Years", border="red")


# get leap years
leap_years <- mret %>%
  filter(format(DATE, "%m/%d") == "02/29") %>%
  distinct(year)
leap_years

leap_years = unique(leap_years$year)
leap_years

mret3 <- mret %>%
  filter(year  %in% (leap_years)) %>%
  group_by(month) %>%
  summarise(EWRETD=100*mean(EWRETD, na.rm = TRUE),
            VWRETD=100*mean(VWRETD, na.rm = TRUE))

mret3m = as.matrix(mret3)
barplot(mret3m[,2], names.arg=mret3m[,1],
        xlab="Month", ylab="Market Return", col="blue",
        main="Stock Market Seasonality Leap Years", border="red")





