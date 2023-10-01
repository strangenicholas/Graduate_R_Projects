library(dplyr)
library(haven)
library(ggplot2)

## Read in data
mret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat")
as.data.frame(mret)

mret <- filter(mret, SHRCD %in% c(10, 11))

# Remove duplicate records
mret %>% distinct()


# A) Construct a variable equal to the market cap (mcap) for each stock in the mret7018 data set. Based on this variable, create a data set that lists the largest firm in the market at the end of the previous month (date PERMNO company name mcap). Which company has held the #1 spot for the longest time?  How many months?

# Calculate market cap
mret <-
  mret %>%
  group_by(PERMNO) %>%
  mutate(
    year = as.numeric(format(DATE, "%Y")),
    month = as.numeric(format(DATE, "%m")),
    # Get absolute value of PRC
    PRC = abs(PRC),
    mcap = PRC * SHROUT,
    mcap1 = lag(mcap, 1)
  ) %>%
  select(DATE, PERMNO, COMNAM, mcap1, RET, VWRETD)
# deduplicate
mret %>% distinct()

mret %>% filter(mret, mcap1 != 'NA')

# View(mret)

# Get top 1 by mcap1 by month
mret1 <-
  mret %>%
  group_by(DATE) %>%
  slice_max(order_by = mcap1, n = 1)

head(mret1)

top_mcap <- table(mret1$PERMNO)
top_mcap_sorted <- sort(top_mcap, decreasing = TRUE)
print(top_mcap_sorted)

# B) Construct the monthly returns of 10 value-weighted dynamic portfolios consisting of the (10, 20, ...,100) stocks with the largest market capitalization as of the end of the previous month. Calculate the correlations of the returns of these portfolios with the returns of the value-weighted market index (VWRETD) over the 1970 –2018 period. Visualize the 10 correlation coefficients (e.g., via histogram). Interpret the results.

port1 <-
  mret %>%
  group_by(DATE) %>%
  slice_max(order_by = mcap1, n = 10) %>%
  summarise(ARET = mean(RET, na.rm = TRUE))

# View(port1)
port2 <-
  mret %>%
  group_by(DATE) %>%
  slice_max(order_by = mcap1, n = 20) %>%
  summarise(ARET = mean(RET, na.rm = TRUE))




#mkt return
mret2 <-
  mret %>%
  group_by(DATE) %>%
  summarize(VWRETD = mean(VWRETD, na.rm = TRUE))

mret2


# Store Portfolio Results
portfolios <- list()

n_values <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# Loop through portfolios
for (n in n_values) {
  port <- mret %>%
    group_by(DATE) %>%
    slice_max(order_by = mcap1, n = n) %>%
    summarise(ARET = mean(RET, na.rm = TRUE))
  
  # Store results
  portfolios[[paste0("port", n)]] <- port
}


# get coorelation
cor(portfolios$port10$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port20$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port30$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port40$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port50$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port60$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port70$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port80$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port90$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
cor(portfolios$port100$ARET, mret2$VWRETD, use = "pairwise.complete.obs")

# Visualize



# Calculate correlations for all portfolios
correlations <- sapply(portfolios, function(port) {
  cor(port$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
})

# Create a histogram of the correlation coefficients
hist(
  correlations,
  main = "Correlation Coefficients with VWRETD",
  xlab = "Correlation Coefficient",
  col = "skyblue",
  border = "black",
  breaks = 20
)



# store correlations for all portfolios
correlations <-
  c(
    cor(portfolios$port10$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port20$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port30$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port40$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port50$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port60$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port70$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port80$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port90$ARET, mret2$VWRETD, use = "pairwise.complete.obs"),
    cor(portfolios$port100$ARET, mret2$VWRETD, use = "pairwise.complete.obs")
  )

# create df with portfolio numbers and correlations
correlation_data <-
  data.frame(Portfolio_Number = n_values, Correlation = correlations)

# create bar plot
barplot(
  correlation_data$Correlation,
  names.arg = correlation_data$Portfolio_Number,
  main = "Correlations with VWRETD by Portfolio Number",
  xlab = "Portfolio Number",
  ylab = "Correlation Coefficient",
  col = "skyblue",
  border = "black",
)
