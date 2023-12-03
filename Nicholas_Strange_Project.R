# Project 

library(dplyr)
library(haven)
library(stringr)
library(ggplot2)

## Read in Market Return data
mret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat")
as.data.frame(mret)

mret <- mret %>%
  mutate(
    year = as.numeric(format(DATE, "%Y")),
    month = as.numeric(format(DATE, "%m"))
  ) %>%
  filter(SHRCD %in% c(10, 11)) 

# Remove duplicate records
mret %>% distinct()

# Filter years -- TBD(haven't decided on this yet)
mret <- filter(mret, year >= 1990 & year <= 2000 )

# Read in Names data
names <-
  read.csv("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/Names.csv")
as.data.frame(names)

# Get top 10 names
names10 <- head(names, 10)

# Get distinct company names
compnames <- distinct(mret,PERMCO, COMNAM)


#Psuedo code
# for name in names:
#   if str names = colomn in compnames:
#   matches.append compnames[comnam]
#   if substr(STR,a,b) in colohmn in compnames:
#   matches.append compnames[comnam]:
#   exc
# 
# 


# Create empty listy to store matches
matches <- list()

# Find exact matches in compnames
for (i in 1:nrow(names10)) {
  for (c in 1:nrow(compnames)) {
    if (compnames[c, "COMNAM"] == names10[i, "name"]) {
      matches <- append(matches, compnames[c, "COMNAM"])
    }
  }
} 


# Find strings in compnames
for (i in 1:nrow(names10)) {
  for (c in 1:nrow(compnames)) {
    if (str_detect(compnames[c, "COMNAM"], names10[i, "name"])) {
      matches <- append(matches, compnames[c, "COMNAM"])
    }
  }
}             

# Find substrings in compnames TBD

# # Print matches
# View(matches)

# Matching names mret
namesmret <- filter(mret, COMNAM %in% matches)

# Non-matching names mret
nonamesmret <- mret[!(mret$COMNAM %in% matches), ]


# Calculate Returns -- Need to fix, all return same values
mret2 <- mret %>%
  group_by(DATE) %>%
  summarise(VWRETD = mean(VWRETD, na.rm = TRUE))

namesmret2 <- namesmret %>%
  group_by(DATE) %>%
  summarise(VWRETD = mean(VWRETD, na.rm = TRUE))

nonamesmret2 <- nonamesmret %>%
  group_by(DATE) %>%
  summarise(VWRETD = mean(VWRETD, na.rm = TRUE))

# Get avg return
average_VWRETD <- mean(mret2$VWRETD, na.rm = TRUE)
print(average_VWRETD)

average_VWRETD2 <- mean(namesmret2$VWRETD, na.rm = TRUE)
print(average_VWRETD2)

average_VWRETD3 <- mean(nonamesmret2$VWRETD, na.rm = TRUE)
print(average_VWRETD3)

# Combine the df's for plotting
combined_data <- bind_rows(
  mutate(mret2, Group = "All Companies"),
  mutate(namesmret2, Group = "Named Companies"),
  mutate(nonamesmret2, Group = "Unnamed Companies")
)

# Plotting
ggplot(combined_data, aes(x = DATE, y = VWRETD, color = Group)) +
  geom_line() +
  labs(title = "Mean VWRETD Over Time",
       x = "Date",
       y = "Mean VWRETD") +
  theme_minimal()





  



