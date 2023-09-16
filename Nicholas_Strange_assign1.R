library(dplyr)
library(haven)


## Read in data
wvs <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/wvs_dataset.sas7bdat")
as.data.frame(wvs)

# A) Expand the mutate-statement by creating additional variables corresponding to the
# following columns: A008, E179, A030, A038, C011-C021, E045, E039, X001, X003,
# X007, X025, X028. Chose names of the variables that are both concise and informative.
# Make sure the values are aligned with the concept being represented and NA-values are
# coded correctly.

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
    happy = if_else(A008 %in% c(1, 2), 1, 0),
    happy = replace(happy, A165 < 1, NA),
    vote = if_else(E179 >= 5 | E179 == 3, 1, 0),
    vote = replace(vote, E179 <= 1, NA),
    republican = if_else(E179 == 840001, 1, 0),
    democrat = if_else(E179 == 840002, 1, 0),
    independent = if_else(E179 == 840003, 1, 0),
    otherparty = if_else(vote == 1 &
                           republican == 0 &
                           democrat == 0 & independent == 0 , 1, 0),
    hardwork = replace(A030, A030 < 0, NA),
    thrift = replace(A038, A038 < 0, NA),
    jobpay = replace(C011, C011 < 0, NA),
    jobpressure = replace(C012, C012 < 0, NA),
    jobsecurity = replace(C013, C013 < 0, NA),
    jobrespected = replace(C014, C014 < 0, NA),
    jobhours = replace(C015, C015 < 0, NA),
    jobinitiative = replace(C016, C016 < 0, NA),
    jobholidays = replace(C017, C017 < 0, NA),
    jobachieve = replace(C018, C018 < 0, NA),
    jobresponsible = replace(C019, C019 < 0, NA),
    jobinteresting = replace(C020, C020 < 0, NA),
    jobdoable = replace(C021, C021 < 0, NA),
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
    higheducation = replace(higheducation, X007 < 1, NA),
    employed = if_else(X028 %in% c(1, 2, 3), 1, 0),
    employed = replace(employed, X028 < 1, NA)
  ) %>%
  select(
    year,
    trust,
    inc,
    hinc,
    happy,
    vote,
    republican,
    democrat,
    independent,
    otherparty,
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

summary(wvs)

# B) Based on the variable X001, construct averages of all other variables across male and
# female respondents (summarize).

SummarybySex <- wvs %>%
  group_by(sex) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

View(SummarybySex)


# C) Tabulate the averages from B. You could do it in MS Word. Visualize the averages
# from B using some of the R tools (you need to do your own research for this part).

library(ggplot2)

# General Plot
dx <-
  as.matrix(SummarybySex[-c(1, 2, 4, 24, 25, 26, 27)])  # Exclude outlier columns
rownames(dx) <- SummarybySex$sex[-c(1, 2, 4, 24, 25, 26, 27)]

# Define colors
colours <- c("pink", "blue")

# Create the barplot
barplot(
  dx,
  main = 'Summary by Sex',
  ylab = 'Results',
  beside = TRUE,
  col = colours,
  ylim = c(0, max(dx) * 1.3),
  names.arg = rownames(dx),
  las = 2
)

box()


# D) Regress the income variable (inc) on each one of these variables in separate regressions:

# run regression on all features and 

feature_names <- setdiff(names(wvs), c("inc", "sex", "hinc", "year"))
model_summaries <-
  data.frame(Feature = character(), R_squared = numeric())

for (feature in feature_names) {
  fit <- lm(wvs[[feature]] ~ inc, data = wvs)
  model_summary <-
    data.frame(Feature = feature, R_squared = summary(fit)$r.squared)
  model_summaries <- rbind(model_summaries, model_summary)
}

# select top 3 r-squared descending
sorted_summaries <-
  model_summaries[order(-model_summaries$R_squared),]

top_3_features <- head(sorted_summaries, 3)

top_3_features

# combined top 3 features
top_3_comb <-
  lm(inc ~ wvs$vote + wvs$relationship + wvs$higheducation, data = wvs)

# print the summary of the regression model
summary(top_3_comb)$r.squared
