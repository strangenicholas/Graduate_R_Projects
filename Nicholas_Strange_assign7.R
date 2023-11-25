# The file nj2000.sas7bdat contains variables for a sample of household
# heads from the state of NJ. Definitions of the variables could be found in the file nj2000_var. 
  #Based on this information:
# 1. Filter out observations for the Philadelphia, PA/NJ metropolitan area (METAREA = 616)
# 2. Using the variable OWNERSHIP, construct a variable equal to 1 for owning a home, vs. 0 for renting
# 3. Predict homeownership based on the available variables and your knowledge acquired in class. 
  # Pick up your best model based on AUC. Describe the model. 


library(dplyr) 
library(haven)

## Read in data
demog <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/nj2000.sas7bdat")
as.data.frame(dems)

# Remove duplicate records
demog %>% distinct()

demog <- filter(demog, METAREA == 616 & OWNERSHP != 0) #filter to PA/NJ and remove NA ownership

demog <- 
  dems %>%
  mutate(homeown = if_else(OWNERSHP == 1,1,0))
  

# View(dems)

# Predict Home Ownership
