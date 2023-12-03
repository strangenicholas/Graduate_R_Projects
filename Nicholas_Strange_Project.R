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
matches <- list()

# Find perfect matches in compnames
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


# Print the result
View(matches)





  



