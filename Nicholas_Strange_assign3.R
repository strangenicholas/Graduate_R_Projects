library(dplyr) 
library(haven)

## Read in data 
mret <- read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat") 
as.data.frame(mret)

mret <- filter(mret, SHRCD %in% c(10,11)) 

# Remove duplicate records
mret %>% distinct()


# A) Construct a variable equal to the market cap (mcap) for each stock in the mret7018 data set. Based on this variable, create a data set that lists the largest firm in the market at the end of the previous month (date PERMNO company name mcap). Which company has held the #1 spot for the longest time?  How many months?

# Calculate market cap
mret <-
  mret %>%
  group_by(PERMNO) %>%
  mutate(year = as.numeric(format(DATE,"%Y")),
         month = as.numeric(format(DATE,"%m")),
         # Get absolute value of PRC
         PRC = abs(PRC),
         mcap = PRC*SHROUT,
         mcap1 = lag(mcap, 1)
  ) %>%
select(DATE, PERMNO, COMNAM, mcap1, VWRET)
# deduplicate
mret %>% distinct()

mret %>% filter(mret,mcap1 != 'NA')

head(mret)

# Get top 1 by mcap1 by month
mret1 <-
  mret%>%
  group_by(DATE) %>%
  slice_max(order_by = mcap1, n = 1)

head(mret1)

top_mcap <- table(mret1$PERMNO)
top_mcap_sorted <- sort(top_mcap, decreasing = TRUE)
print(top_mcap_sorted)

# B) Construct the monthly returns of 10 value-weighted dynamic portfolios consisting of the (10, 20, ...,100) stocks with the largest market capitalization as of the end of the previous month. Calculate the correlations of the returns of these portfolios with the returns of the value-weighted market index (VWRETD) over the 1970 –2018 period. Visualize the 10 correlation coefficients (e.g., via histogram). Interpret the results.    

port1 <-
  mret%>%
  group_by(DATE) %>%
  slice_max(order_by = mcap1, n = 10) %>%
  summarize(VWRET = mean((VWRET, na.rm = TRUE)))

View(port1)
