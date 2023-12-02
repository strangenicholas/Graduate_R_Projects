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

# Read in Names data
names <-
  read.csv("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/Names.csv")
as.data.frame(names)

# Get top 10 names
names
# Get distinct company names
compnames <- unique(mret[["COMNAM"]])

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
matches <- logical(nrow(names))

# Loop through each row in 'names'
for (i in 1:nrow(names)) {
  # Check if any keyword is present in the current row
  if (names[i, "name"] %in% compnames) {
    matches[i] <- TRUE
  }
}

# Subsstring
for (i in 1:nrow(names)) {
  # Check if any keyword is present in the current row
  if (names[i, "name"] %in% str_detect(compnames$COMNAM, name)) {
    matches[i] <- TRUE
  }
}

# Subset df1 based on matches
result_df <- names[matches, ]

# Print the result
print(result_df)





# get distinct matches
# 
# nomatch <-  unique names that are not in matches
  



