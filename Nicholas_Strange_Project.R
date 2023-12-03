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
  group_by(PERMNO) %>%
  mutate(
    year = as.numeric(format(DATE, "%Y")),
    month = as.numeric(format(DATE, "%m"))
  ) %>%
  filter(SHRCD %in% c(10, 11)) 

# Remove duplicate records
mret %>% distinct()

# # Filter years 
mret <- filter(mret, year >= 2000 & year <= 2010 )

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

# Find substrings in compnames
for (i in 1:nrow(names10)) {
  for (c in 1:nrow(compnames)) {
    if (str_detect(compnames[c, "COMNAM"], substr(names10[i, "name"],0,5))) {
      matches <- append(matches, compnames[c, "COMNAM"])
    }
  }
}      

# # Print matches
View(matches)

# Matching names mret
namesmret <- filter(mret, COMNAM %in% matches)

# Non-matching names mret
nonamesmret <- mret[!(mret$COMNAM %in% matches), ]
# Calculate Returns 
mret2 <- mret %>%
  group_by(DATE, COMNAM) %>%
  summarise(ARET = mean(RET, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(DATE) %>%
  summarise(ARET = mean(ARET, na.rm = TRUE))

namesmret2 <- namesmret %>%
  group_by(DATE, COMNAM) %>%
  summarise(ARET = mean(RET, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(DATE) %>%
  summarise(ARET = mean(ARET, na.rm = TRUE))

nonamesmret2 <- nonamesmret %>%
  group_by(DATE, COMNAM) %>%
  summarise(ARET = mean(RET, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(DATE) %>%
  summarise(ARET = mean(ARET, na.rm = TRUE))

# Combine the df's for plotting hist return 
combined_data <- bind_rows(
  mutate(mret2, Group = "All Companies"),
  mutate(namesmret2, Group = "Named Companies"),
  mutate(nonamesmret2, Group = "Unnamed Companies")
)

# Plot
ggplot(combined_data, aes(x = DATE, y = ARET, color = Group)) +
  geom_line() +
  labs(title = "Mean VWRETD Over Time",
       x = "Date",
       y = "Mean VWRETD") +
  theme_minimal()

# Get avg return
avg_RET1 <- mean(mret2$ARET, na.rm = TRUE)
print(avg_RET1)

avg_RET2 <- mean(namesmret2$ARET, na.rm = TRUE)
print(avg_RET2)

avg_RET3 <- mean(nonamesmret2$ARET, na.rm = TRUE)
print(avg_RET3)

# DF for avg_RET
avg_returns_data <- data.frame(
  Dataset = c("Market", "Names", "NoNames"),
  Average_RET = c(avg_RET1, avg_RET2, avg_RET3)
)

# Bar plot
ggplot(avg_returns_data, aes(x = Dataset, y = Average_RET, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.4f", Average_RET)), vjust = -0.5) +  
  labs(title = "Average Returns",
       y = "Average RET") +
  theme_minimal()





