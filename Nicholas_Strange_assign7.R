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
        ANCESTR1 >= 100 & ANCESTR1 <= 179,
        'Eastern Europe',
        if_else(
          ANCESTR1 >= 181 & ANCESTR1 <= 195,
          'Europe NEC',
          if_else(
            ANCESTR1 >= 200 & ANCESTR1 <= 296,
            'Hispanic',
            if_else(
              ANCESTR1 >= 300 & ANCESTR1 <= 337,
              'West Indies',
              if_else(
                ANCESTR1 >= 360 & ANCESTR1 <= 380,
                'Central & South America',
                if_else(
                  ANCESTR1 >= 400 & ANCESTR1 <= 496,
                  'North Africa & SW Asia',
                  if_else(
                    ANCESTR1 >= 500 & ANCESTR1 <= 599,
                    'Subsaharan Africa',
                    if_else(
                      ANCESTR1 >= 600 & ANCESTR1 <= 695,
                      'South Asia',
                      if_else(
                        ANCESTR1 >= 700 & ANCESTR1 <= 796,
                        'Other Asia',
                        if_else(
                          ANCESTR1 >= 800 & ANCESTR1 <= 870,
                          'Pacific',
                          if_else(
                            ANCESTR1 >= 900 & ANCESTR1 <= 994,
                            'North America',
                            if_else(ANCESTR1 >= 995, 'Other', as.character(ANCESTR1))
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
    employed = if_else(EMPSTAT == 0, NA, EMPSTAT),
    employed = if_else(employed == 1, 1, 0), 
    hhincome = if_else(HHINCOME == 9999999,NA,HHINCOME),
    totincome = if_else(INCTOT == 9999999,NA,INCTOT),
    industry = if_else(
      IND1990 == 0, NA,
      if_else(IND1990 >= 1 & IND1990 <= 32, 'Agriculture',
              if_else(IND1990 >= 40 & IND1990 <= 50, 'Mining',
                      if_else(IND1990 == 60, 'Construction',
                              if_else(IND1990 >= 100 & IND1990 <= 392, 'Manufacturing',
                                      if_else(IND1990 >= 400 & IND1990 <= 472, 'Transportation',
                                              if_else(IND1990 >= 500 & IND1990 <= 571, 'Wholesale',
                                                      if_else(IND1990 >= 580 & IND1990 <= 691, 'Retail',
                                                              if_else(IND1990 >= 700 & IND1990 <= 712, 'Finance',
                                                                      if_else(IND1990 >= 721 & IND1990 <= 760, 'Business',
                                                                              if_else(IND1990 >= 761 & IND1990 <= 791, 'Personal Services',
                                                                                      if_else(IND1990 >= 800 & IND1990 <= 893, 'Professional Services',
                                                                                              if_else(IND1990 >= 900 & IND1990 <= 932, 'Public Admin',
                                                                                                      if_else(IND1990 >= 940 & IND1990 <= 960, 'Military', 'Other')
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
      )
    ),
    
    
  ) %>%
select(homeown, iscitizen, edulvl, ancestry, employed, hhincome, totincome, industry)  

View(demog)

# Predict Home Ownership
