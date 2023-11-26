# The file nj2000.sas7bdat contains variables for a sample of household
# heads from the state of NJ. Definitions of the variables could be found in the file nj2000_var. 
  #Based on this information:
# 1. Filter out observations for the Philadelphia, PA/NJ metropolitan area (METAREA = 616)
# 2. Using the variable OWNERSHIP, construct a variable equal to 1 for owning a home, vs. 0 for renting
# 3. Predict homeownership based on the available variables and your knowledge acquired in class. 
  # Pick up your best model based on AUC. Describe the model. 


library(dplyr) 
library(haven)
library(ggplot2) 
library(caret) 
library(pROC) 
library(randomForest)

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
    occupation = if_else(OCC1990 >= 3 & OCC1990 <= 200, 'Managerial & Professional', 
      if_else(OCC1990 >= 203 & OCC1990 <= 389, 'Technical, Sales & Admin',
              if_else(OCC1990 >= 405 & OCC1990 <= 469, 'Service',
                      if_else(OCC1990 >= 473 & OCC1990 <= 498, 'Farming, Forestry & Fishing',
                              if_else(OCC1990 >= 503 & OCC1990 <= 699, 'Production, Craft & Repair',
                                      if_else(OCC1990 >= 703 & OCC1990 <= 889, 'Operators, Fabricators & Laborers',
                                              if_else(OCC1990 == 905 , 'Military','Other'))))))),
    married = if_else(MARST %in% c(1,2), 1, 0), #may add divorced, in case home purchased when married
    # migrated = 
    travtime = if_else(TRANTIME == 000,NA,TRANTIME),
    
    
    
  ) %>%
select(homeown, iscitizen, edulvl, ancestry, employed, hhincome, totincome, industry, occupation, married, travtime, SEX)  

View(demog)

# Predict Home Ownership

## Non-linear regression
fit <-
  lm(
    homeown ~  iscitizen + edulvl + ancestry + employed + hhincome 
    + totincome + industry + occupation + married + travtime + SEX,
    data = demog
  )
summary(fit) # show results 

fitNLR <- lm(homeown ~  iscitizen + edulvl + ancestry + hhincome + married,
             data = demog)
summary(fitNLR) # show results 

####### Logistic regression   
fitLgR <- glm(homeown ~  iscitizen + edulvl + ancestry + employed + hhincome + totincome + industry + occupation + married + travtime + SEX, 
              family = "binomial", data = demog)
summary(fitLgR)  


####### Decision tree  
library(rpart)
library(rpart.plot)

fit3 <- lm(    homeown ~  iscitizen + edulvl + ancestry + employed + hhincome 
               + totincome + industry + occupation + married + travtime + SEX,
               data = demog)
summary(fit3) # show results 

dt <- rpart(    homeown ~  iscitizen + edulvl + ancestry + employed + hhincome 
                + totincome + industry + occupation + married + travtime + SEX,
                data = demog)
summary(dt) 
# Plot the decision tree 
prp(dt, space=4, split.cex=1.5, nn.border.col=0)


####### CARET: Predicting homeownership
demog2 <- 
  demog %>% 
  select(homeown, iscitizen, edulvl, ancestry, employed, hhincome, totincome, industry, occupation, married, travtime, SEX) %>% 
  mutate(homeown = as.factor(homeown))
summary(demog2) 

# removing rows with missing values
demog2 <- na.omit(demog2)

### Random number generation
set.seed(2)
rnorm(5)
rnorm(5)

set.seed(2)
rnorm(5)


### Data splitting 
set.seed(123)

inTrain <- createDataPartition(demog2$homeown, p = .70, list = FALSE)
inTrain[1:10]

Train <- demog2[ inTrain,]
Test  <- demog2[-inTrain,]
summary(Train)
summary(Test)

### Train a model 
glm <- train(homeown ~  iscitizen + edulvl + ancestry + employed + hhincome 
             + totincome + industry + occupation + married + travtime + SEX,
             family=binomial(link='logit'), 
             method = "glm", data = Train)
glm 


# Obtaining details about each model 
getModelInfo("glm") 

varimpGLM = varImp(glm)
varimpGLM
plot(varimpGLM, main = "Mret Variable Importance: GLM") 

### Creating ROC curves 
# Output the probabilities for 1   
fitProb <- predict(glm, Test, type = "prob")
head(fitProb)

fitProb1 <- fitProb$"1"
par(pty = "s") 

roc(Test$homeown, fitProb1, plot=TRUE, legacy.axes=TRUE, col="#377eb8", 
    print.auc=TRUE)

# Explore different thresholds with CARET      
fitProbT80 <- factor(ifelse(fitProb1 >= 0.8, 1, 0))

confusionMatrix(reference = Test$homeown, data = fitProbT80, 
                mode = "everything",
                positive = "1") 
# Explore different thresholds with CARET      
fitProbT80 <- factor(ifelse(fitProb1 >= 0.8, 1, 0))

confusionMatrix(reference = Test$homeown, data = fitProbT80, 
                mode = "everything",
                positive = "1") 

# Explore different thresholds with pROC 
par(pty = "s") 
roc.out <- roc(Test$homeown, fitProb1, plot=TRUE)
summary(roc.out) 

roc.df <- data.frame(
  Sensitivity=roc.out$sensitivities,
  Specificity=roc.out$specificities, 
  fpp=1-roc.out$specificities, 
  Threshold=roc.out$thresholds) 

roc.dfT <- roc.df %>% 
  filter(Threshold > 0.79 & Threshold < 0.81)
roc.dfT 


### Choosing an optimal threshold 
# Extracting Accuracy form the CM 
Accuracy <- function(t){
  fitProbT <- factor(ifelse(fitProb1 >= t, 1, 0))
  cm <- caret::confusionMatrix(reference = Test$homeown, 
                               data = fitProbT, 
                               mode = "everything",
                               positive = "1") 
  a <- c(cm$overall[['Accuracy']]) 
  return(a)
}

T=0
acc=Accuracy(0.001) 

for(i in seq(1,9)){
  if(acc < Accuracy(i/10)){
    T=i/10  
    acc=Accuracy(i/10)} 
} 

Tacc <- c(T,acc)
Tacc

### Once you choose a threshold, you can classify new observations 
# Simulate new observations 

demog2new <- demog2 %>% 
  select(homeown, iscitizen, edulvl, ancestry, employed, hhincome, totincome, industry, occupation, married, travtime, SEX) %>% 
  sample_frac(0.003)
demog2new 

# Classify the observations @ T=0.4 
fitProbNew <- predict(glm, demog2new, type = "prob")

fitProbNew <- fitProbNew  %>% 
  mutate(homeown = if_else(fitProbNew$"1">0.4,1,0))
fitProbNew 

# CARET finds optimal thresholds  
fit <- predict(glm, Test)
fit[1:10]

confusionMatrix(reference = Test$homeown, data = fit, 
                mode = "everything",
                positive = "1") 


### Comparing models: GLM vs. Random forest
rf <- train(homeown ~  iscitizen + edulvl + ancestry + employed + hhincome 
            + totincome + industry + occupation + married + travtime + SEX,
            method = "rf", data=Train, ntree = 100)
rf

fitProbRF <- predict(rf, Test, type = "prob")
head(fitProbRF)

fitProbRF1 <- fitProbRF$"1"

par(pty = "s")
roc(Test$homeown, fitProbRF1, plot=TRUE, legacy.axes=TRUE, col="#377eb8",
    print.auc=TRUE)


## ROC: GLM & RF
par(pty = "s")
roc(Test$homeown, fitProb1, plot=TRUE, legacy.axes=TRUE, col="#377eb8",
    print.auc=TRUE)

roc(Test$homeown, fitProbRF1, plot=TRUE, legacy.axes=TRUE,  col="#4daf4a",
    add=TRUE, print.auc=TRUE, print.auc.y=0.4)

legend("bottomright", legend=c("GLM Model",
                               "RF Model"),
       col=c("#377eb8", "#4daf4a"), lwd=3)


### Comparing models: GLM vs. GLM2

glm <- train(homeown ~  iscitizen + edulvl + ancestry + employed + hhincome 
             + totincome + industry + occupation + married + travtime + SEX,
             method = "glm", data=Train)
glm

varimpGLM = varImp(glm)
varimpGLM
plot(varimpGLM, main = "MRET Variable Importance: GLM")


# Drop unimportant variables
glm2 <- train(homeown ~  iscitizen +  ancestry + hhincome + married,
              method = "glm", data=Train)
glm2

fitProb2 <- predict(glm2, Test, type = "prob")
fitProb21 <- fitProb2$"1"

## ROC: GLM & GLM2
par(pty = "s")
roc(Test$homeown, fitProb1, plot=TRUE, legacy.axes=TRUE, col="#377eb8",
    print.auc=TRUE)

roc(Test$homeown, fitProb21, plot=TRUE, legacy.axes=TRUE,  col="#4daf4a",
    add=TRUE, print.auc=TRUE, print.auc.y=0.4)

legend("bottomright", legend=c("GLM Model",
                               "NL GLM Model"),
       col=c("#377eb8", "#4daf4a"), lwd=3)