# Project 

library(dplyr)
library(haven)
library(stringr)

## Read in Market Return data
mret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat")
as.data.frame(mret)

mret <- filter(mret, SHRCD %in% c(10,11)) 

# Remove duplicate records
mret %>% distinct()

mret

# Read in Names data
names <-
  read.csv("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/Names.csv")
as.data.frame(names)

# Get top 10 names
names10 <- head(names, 10)
  
# Get distinct company names
compnames <- distinct(mret,PERMCO, COMNAM)

compnames


matches <- data_frame()

#Psudo code
# for name in names:
#   if str names = colomn in compnames:
#   matches.append compnames[comnam]
#   if substr(STR,a,b) in colohmn in compnames:
#   matches.append compnames[comnam]:
#   exc
# 
# 


# Initialize an empty list to store matches
matches <- logical(nrow(names10))

# Loop through each row in 'names'
for (i in 1:nrow(names10)) {
  # Check if any keyword is present in the current row
  if (names10[i, "name"] %in% compnames) {
    matches[i] <- TRUE
  }
}

# Loop through each row in 'names10'
for (i in 1:nrow(names10)) { 
  if (str_detect(names10[i, "name"],compnames) = TRUE) {
  matches[i] <- TRUE
}
}

  # Check if any keyword is present in 'compnames'
  matches <- compnames[str_detect(compnames, names10[i, "name"])]
  matches[i] <- compnames$comnam

# Subset df1 based on matches
result_df <- names10[matches, ]

# Print the result
print(result_df)

  



