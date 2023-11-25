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
as.data.frame(demog)

# Remove duplicate records
demog %>% distinct()

demog <- filter(demog, METAREA == 616 & OWNERSHP != 0) #filter to PA/NJ and remove NA ownership

demog <-
  demog %>%
  mutate(
    homeown = if_else(OWNERSHP == 1, 1, 0),
    iscitizen = if_else(CITIZEN == 0, NA, CITIZEN),
    iscitizen = if_else(iscitizen  %in% c(1, 2), 1, iscitizen),
    iscitizen = if_else(iscitizen  %in% c(3, 4, 5), 0, iscitizen),
    edulvl = if_else(EDUC == 0, NA, EDUC),
    ancestry = if_else(
      ANCESTR1 >= 1 & ANCESTR1 <= 98,
      'Western Europe',
      if_else(
        ancestry >= 100 & ancestry <= 179,
        'Eastern Europe',
        if_else(
          ancestry >= 181 & ancestry <= 195,
          'Europe NEC',
          if_else(
            ancestry >= 200 & ancestry <= 296,
            'Hispanic',
            if_else(
              ancestry >= 300 & ancestry <= 337,
              'West Indies',
              if_else(
                ancestry >= 360 & ancestry <= 380,
                'Central & South America',
                if_else(
                  ancestry >= 400 & ancestry <= 496,
                  'North Africa & SW Asia',
                  if_else(
                    ancestry >= 500 & ancestry <= 599,
                    'Subsaharan Africa',
                    if_else(
                      ancestry >= 600 & ancestry <= 695,
                      'South Asia',
                      if_else(
                        ancestry >= 700 & ancestry <= 796,
                        'Other Asia',
                        if_else(
                          ancestry >= 800 & ancestry <= 870,
                          'Pacific',
                          if_else(
                            ancestry >= 900 & ancestry <= 994,
                            'North America',
                            if_else(ancestry >= 995, 'Other', ancestry)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
  ) %>%
         # )
select(homeown, iscitizen, edulvl, ancestry)  

View(demog)

# Predict Home Ownership
