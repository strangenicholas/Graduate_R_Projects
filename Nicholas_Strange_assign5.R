# Predicting crashes in the stock market is notoriously difficult. Based on the mret7018 data
# set, construct a variable CRASH equal to 1 if the return of a stock in a given month is less
# than –8% (–0.08), and equal to 0, otherwise. Build a predictive model of CRASH based on
# the stock market variables in the data set. To reduce computational time, focus on the last
# 5 years of data.
# Make sure that all predictive variables are lagged (1,2,3,.. months), so that this is indeed a
# predictive model. Select the variables carefully. Some possible predictors include past
# returns, absolute values of returns (volatility), market caps, turnover (trading volume/shares outstanding), 
# etc. You could also use interactions of some of the predictors.
# Explore different predictive variables and models, as discussed in this chapter. What is the
# best AUC that your model achieves? Summarize your best model in a short write up and
# email to me both the R-code and the write up. 

library(dplyr) 
library(haven)
library(ggplot2) 
library(caret) 
library(pROC) 
library(randomForest)

## Read in data
mret <-
  read_sas("C:/Users/nicho/OneDrive/UCF MS - FinTech/FIN 6779/mret7018.sas7bdat")
as.data.frame(mret)

mret <- filter(mret, SHRCD %in% c(10, 11))


# Remove duplicate records
mret %>% distinct()

mret <- 
  mret %>% 
  group_by(PERMNO) %>%
  mutate(year = as.numeric(format(DATE,"%Y")), 
         month = as.numeric(format(DATE,"%m")), 
         PRC = abs(PRC), 
         sz = PRC*SHROUT, 
         mcap = lag(sz, 1),
         r1m = lag(RET,1), #Past return 1 month
         r2m = lag(RET,2), #Past return 2 month
         r3m = lag(RET,3), #Past return 3 month
         r4m = lag(RET,4), #Past return 4 month
         r5m = lag(RET,5), #Past return 5 month
         r6m = lag(RET,6), #Past return 6 month
         v1m = abs(r1m), #Volatility 1 month prior
         v2m = abs(r2m), #Volatility 2 month prior
         v3m = abs(r3m), #Volatility 3 month prior
         v4m = abs(r4m), #Volatility 4 month prior
         v5m = abs(r5m), #Volatility 5 month prior
         v6m = abs(r6m), #Volatility 6 month prior
         mcap1m = lag(sz, 2), #mcap 1 month prior
         mcap2m = lag(sz, 3), #mcap 2 month prior
         mcap3m = lag(sz, 4), #mcap 3 month prior
         mcap4m = lag(sz, 5), #mcap 4 month prior
         mcap5m = lag(sz, 6), #mcap 5 month prior
         mcap6m = lag(sz, 7), #mcap 6 month prior
         turn = (VOL/SHROUT),
         turn1m = lag(turn, 1), # turn 1 month prior
         turn2m = lag(turn, 2), # turn 2 month prior
         turn3m = lag(turn, 3), # turn 3 month prior
         turn4m = lag(turn, 4), # turn 4 month prior
         turn5m = lag(turn, 5), # turn 5 month prior
         turn6m = lag(turn, 6), # turn 6 month prior
         CRASH = if_else(RET <= -.08, 1,0))%>% 
  ungroup %>% 
  select(PERMNO, year, month, mcap, 
         r1m, r2m, r3m, r4m, r5m, r6m, 
         v1m, v2m, v3m, v4m, v5m, v6m, 
         mcap1m, mcap2m, mcap3m, mcap4m, mcap5m, mcap6m,
         turn1m, turn2m, turn3m, turn4m, turn5m, turn6m,
         CRASH)

mret <- na.omit(mret)
mret <- filter(mret, mcap != 'NA') 
mret <- filter(mret, year >= (max(year)-5))

View(mret)
# Coorelation
colnames(mret)

## Non-linear regression
fit <-
  lm(
    CRASH ~ r1m + r2m + r3m + r4m + r5m + r6m + v1m 
    + v2m + v3m + v4m + v5m + v6m + mcap1m + mcap2m 
    + mcap3m + mcap4m + mcap5m + mcap6m + turn1m + turn2m
    + turn3m + turn4m + turn5m + turn6m,
    data = mret
  )
summary(fit) # show results 

fit1 <- lm(CRASH ~  v1m + v2m + v3m 
           + v4m + v5m + v6m,
           data=mret)
summary(fit1) # show results 


####### Logistic regression   
fit <- glm(CRASH ~ v1m + v2m + v3m 
           + v4m + v5m + v6m, 
           family = "binomial", data=mret)
summary(fit)  

####### Decision tree  
library(rpart)
library(rpart.plot)

fit <- lm(CRASH ~ v1m + v2m + v3m 
          + v4m + v5m + v6m, data=mret)
summary(fit) # show results 

dt <- rpart(CRASH ~ v1m + v2m + v3m 
            + v4m + v5m + v6m, data=mret)
summary(dt) 
# Plot the decision tree 
prp(dt, space=4, split.cex=1.5, nn.border.col=0)


####### CARET: Predicting Crashes
mret2 <- 
  mret %>% 
  select(PERMNO, year, month, mcap, 
         r1m, r2m, r3m, r4m, r5m, r6m, 
         v1m, v2m, v3m, v4m, v5m, v6m, 
         mcap1m, mcap2m, mcap3m, mcap4m, mcap5m, mcap6m,
         turn1m, turn2m, turn3m, turn4m, turn5m, turn6m,
         CRASH) %>% 
  mutate(CRASH = as.factor(CRASH))
summary(mret2) 

# Identify variables with limited variation 
nearZeroVar(mret2, freqCut = 70/30, uniqueCut = 10)

### Random number generation  
set.seed(2)
rnorm(5)
rnorm(5) 

set.seed(2)
rnorm(5)


### Data splitting 
set.seed(123)

inTrain <- createDataPartition(mret2$CRASH, p = .70, list = FALSE)
inTrain[1:10]

Train <- mret2[ inTrain,]
Test  <- mret2[-inTrain,]
summary(Train)
summary(Test)

### Train a model 
glm <- train(CRASH ~ v1m + v2m + v3m 
             + v4m + v5m + v6m, 
             family=binomial(link='logit'), 
             method = "glm", data=Train)
glm 

# Obtaining details about each model 
getModelInfo("glm") 

varimpGLM = varImp(glm)
varimpGLM
plot(varimpGLM, main = "Mret Variable Importance: GLM") 

### Comparing models: GLM vs. Neural network 
nn <- train(CRASH ~ v1m + v2m + v3m 
            + v4m + v5m + v6m, 
            method = "nnet", data=Train)
nn

fitProbNN <- predict(nn, Test, type = "prob") 
head(fitProbNN)

fitProbNN1 <- fitProbNN$"1"

par(pty = "s") 
roc(Test$CRASH, fitProbNN1, plot=TRUE, legacy.axes=TRUE, col="#377eb8", 
    print.auc=TRUE)


## ROC: GLM & NN 
par(pty = "s") 
roc(Test$CRASH, fitProb1, plot=TRUE, legacy.axes=TRUE, col="#377eb8", 
    print.auc=TRUE)

roc(Test$CRASH, fitProbNN1, plot=TRUE, legacy.axes=TRUE,  col="#4daf4a", 
    add=TRUE, print.auc=TRUE, print.auc.y=0.4)

legend("bottomright", legend=c("GLM Model", 
                               "NN Model"), 
       col=c("#377eb8", "#4daf4a"), lwd=3)

