library(dplyr)
library(haven)

# Instructions
# A) Expand the mutate-statement by creating additional variables corresponding to the
# following columns: A008, E179, A030, A038, C011-C021, E045, E039, X001, X003,
# X007, X025, X028. Chose names of the variables that are both concise and informative.
# Make sure the values are aligned with the concept being represented and NA-values are
# coded correctly.
# B) Based on the variable X001, construct averages of all other variables across male and
# female respondents (summarize).
# C) Tabulate the averages from B. You could do it in MS Word. Visualize the averages
# from B using some of the R tools (you need to do your own research for this part).

## Read in data
wvs <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/wvs_dataset.sas7bdat")
as.data.frame(wvs)

wvs <-
  wvs %>%
  mutate(
    trust = if_else(A165 == 1, 1, 0),
    trust = replace(trust, A165 < 1, NA),
    inc = X047CS,
    inc = replace(inc, inc < 1, NA),
    inc = if_else(X047CS >= 840001 &
                    X047CS <= 840010, X047CS - 840000, inc),
    inc = if_else(X047CS >= 840011 &
                    X047CS <= 840020, X047CS - 840010, inc),
    inc = if_else(X047CS >= 840041 &
                    X047CS <= 840050, X047CS - 840040, inc),
    inc = if_else(X047CS >= 840051 &
                    X047CS <= 840060, X047CS - 840050, inc),
    hinc = if_else(inc >= 8, 1, 0),
    happy = if_else(A008 >= 1, 1, 0),
    happy = replace(happy, A165 < 1, NA),
    vote = if_else(E179 >= 5 | E179 == 3, 1, 0),
    vote = replace(vote, E179 <= 1, NA),
    hardwork = if_else(A030 == 1, 1, 0),
    hardwork = replace(hardwork, A030 < 1, NA),
    thrift = if_else(A038 == 1, 1, 0),
    thrift = replace(thrift, A038 < 1, NA),
    jobpay = if_else(C011 == 1, 1, 0),
    jobpay = replace(jobpay, C011 < 1, NA),
    jobpressure = if_else(C012 == 1, 1, 0),
    jobpressure = replace(jobpressure, C012 < 1, NA),
    jobsecurity = if_else(C013 == 1, 1, 0),
    jobsecurity = replace(jobsecurity, C013 < 1, NA),
    jobrespected = if_else(C014 == 1, 1, 0),
    jobrespected = replace(jobrespected, C014 < 1, NA),
    jobhours = if_else(C015 == 1, 1, 0),
    jobhours = replace(jobhours, C015 < 1, NA),
    jobinitiative = if_else(C016 == 1, 1, 0),
    jobinitiative = replace(jobinitiative, C016 < 1, NA),
    jobholidays = if_else(C017 == 1, 1, 0),
    jobholidays = replace(jobholidays, C017 < 1, NA),
    jobachieve = if_else(C018 == 1, 1, 0),
    jobachieve = replace(jobachieve, C018 < 1, NA),
    jobresponsible = if_else(C019 == 1, 1, 0),
    jobresponsible = replace(jobresponsible, C019 < 1, NA),
    jobinteresting = if_else(C020 == 1, 1, 0),
    jobinteresting = replace(jobinteresting, C020 < 1, NA),
    jobdoable = if_else(C021 == 1, 1, 0),
    jobdoable = replace(jobdoable, C021 < 1, NA),
    risktolerance = E045,
    risktolerance = replace(risktolerance, risktolerance < 1, NA),
    competitionharm = E045,
    competitionharm = replace(competitionharm, competitionharm < 1, NA),
    sex = X001,
    sex = replace(sex, sex < 1, NA),
    sex = replace(sex, sex == 1, "Male"),
    sex = replace(sex, sex == 2, "Female"),
    age = X003,
    age = replace(age, age < 1, NA),
    relationship = if_else(X007 %in% c(1, 2, 8), 1, 0),
    relationship = replace(relationship, X007 < 1, NA),
    higheducation = if_else(X025 >= 6, 1, 0),
    higheducation = replace(higheducation,X007 < 1, NA),
    employed = if_else(X028 %in% c(1, 2, 3), 1, 0),
    employed = replace(employed, X028 < 1, NA)
  ) %>%
  select(
    year,
    S003,
    trust,
    X047CS,
    inc,
    hinc,
    happy,
    vote,
    hardwork,
    thrift,
    jobpay,
    jobpressure,
    jobsecurity,
    jobrespected,
    jobhours,
    jobinitiative,
    jobholidays,
    jobachieve,
    jobresponsible,
    jobdoable, 
    risktolerance,
    competitionharm,
    sex,
    age,
    relationship,
    higheducation,
    employed
  )

View(wvs)

# summary(wvs)

# wvs <-
#   read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/wvs_dataset.sas7bdat")
# as.data.frame(wvs)
# unique(wvs$X025)
