setwd("/Users/tz240/Desktop/Tims/School/Regis/MSDS_692")
Fraud_Data <- read.csv("Fraud_Data.CSV")
library(stringr)
library(caret)
library(C50)
library(gmodels)
library(rpart)
library(reshape)
library(rpart.plot)
library(dplyr)
library(pROC)
#create data frame
svm_fraud <- data.frame(hours = Fraud_Data$step,
                        type = as.numeric(Fraud_Data$type),
                        amount = Fraud_Data$amount,
                        transaction_start = str_sub(Fraud_Data$nameOrig,1,1),
                        old_balance_orig = Fraud_Data$oldbalanceOrg,
                        new_balance_orig = Fraud_Data$newbalanceOrig,
                        transaction_end = str_sub(Fraud_Data$nameDest,1,1),
                        old_balance_dest = Fraud_Data$oldbalanceDest,
                        new_balance_dest = Fraud_Data$newbalanceDest,
                        fraud = factor(Fraud_Data$isFraud, levels = c("0", "1"), labels = c("No", "Yes")),
                        fraud_flag = factor(Fraud_Data$isFlaggedFraud, levels = c("0", "1"), labels = c("No", "Yes")))
svm_fraud$transaction_end <- as.numeric(svm_fraud$transaction_end)
svm_fraud$transaction_start <- as.numeric(svm_fraud$transaction_start)

svm_fraud_2 <- svm_fraud[,-11]
set.seed(123)
ind = createDataPartition(svm_fraud_2$fraud, p = .8, list = FALSE)
trainset = svm_fraud_2[ind,]
testset = svm_fraud_2[-ind,]

dim(trainset)
dim(testset)

#Check for and remove highly correlated predictors
corFraud <- Fraud_Data[,c(3,5,6,8,9)]
roundFraud <- round(cor(corFraud),2)
roundFraud

get_lower_tri<-function(roundFraud){
  roundFraud[upper.tri(roundFraud)] <- NA
  return(roundFraud)
}
  # Get upper triangle of the correlation matrix
get_upper_tri <- function(roundFraud){
  roundFraud[lower.tri(roundFraud)]<- NA
  return(roundFraud)
}

upper_tri <- get_upper_tri(roundFraud)

melted_fraud <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_fraud, aes(X2, X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "Dark Red", high = "Dark Green", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
#################################################
#### REMOVE TIME FROM DATASET TO RUN MODEL  ####
################################################
#This was the best model using all data.

#model using 10-fold cross validation
rpart_model_notime = train(fraud ~ ., 
                           data = trainset_notime, 
                           method = "rpart", 
                           tuneLength = 10, 
                           metric = "ROC", 
                           trControl = control) 
#Predict on testset
rpart_train_pred_notime <- predict(rpart_model_notime, testset_notime)
confusionMatrix(testset_notime$fraud, rpart_train_pred_notime, positive = "Yes")

#ROC
rpart_model_notime_prob <- predict(rpart_model_notime, testset_notime, type = "prob")
rpart_model_notime_ROC <- roc(response = testset_notime$fraud, predictor = rpart_model_notime_prob$Yes,
                              levels = levels(testset_notime$fraud))

plot(rpart_model_notime_ROC, col = "red")
auc(rpart_model_notime_ROC)

###############################################
### Downsampling and removing time         ###
#############################################
#This was the best model from downsampling data

#downsampling non-fraud data points to run SVM and increase fraud percentage
downSampleFraudno <- svm_fraud_2 %>% filter(fraud == "No")
fraud_only <- svm_fraud_2 %>% filter(fraud == "Yes")
#check
table(Fraud_Data$isFraud)
#creating new dataset
set.seed(456)
downSampleFraud <- sample_n(downSampleFraudno, 50000)
downSampleFraud <- rbind(downSampleFraud, fraud_only)
dim(downSampleFraud)

#Split downsample data
set.seed(123)
ind = createDataPartition(downSampleFraud$fraud, p = .8, list = FALSE)
trainset_DS = downSampleFraud[ind,]
testset_DS = downSampleFraud[-ind,]
#check
dim(trainset_DS)
dim(testset_DS)

#model using 10-fold cross validation
rpart_model_DS_notime = train(fraud ~ ., 
                              data = trainsetDS_notime, 
                              method = "rpart", 
                              tuneLength = 10, 
                              metric = "ROC", 
                              trControl = control) 
#Predict on testset
rpart_train_pred_DS_notime <- predict(rpart_model_DS_notime, testsetDS_notime)
confusionMatrix(testsetDS_notime$fraud, rpart_train_pred_DS_notime, positive = "Yes")

#ROC
rpart_model_DS_prob_notime <- predict(rpart_model_DS_notime, testsetDS_notime, type = "prob")
rpart_model_DS_ROC_notime <- roc(response = testsetDS_notime$fraud, predictor = rpart_model_DS_prob$Yes,
                                 levels = levels(testsetDS_notime$fraud))

plot(rpart_model_DS_ROC_notime, col = "red")
auc(rpart_model_DS_ROC_notime)


#Appendix of models trying to obtain optimal metrics.

#################################################
#### RUNNING MODEL WITH DATA AS IS          ####
################################################

#Decision model for visual and logic
Fraud_DT <- rpart(fraud ~., data = trainset, cp = .02)
Fraud_DT
  #plot tree
fraud_DT_plot <- rpart.plot(Fraud_DT, digits = 3)
  #Accuracy metrics
rpart_pred <- predict(Fraud_DT, testset, type = "class")
confusionMatrix(testset$fraud,rpart_pred, positive = "Yes")

  #rpart model using 10-fold cross validation
control <- trainControl(method = "repeatedcv", #resampling method
                        number = 10, # number of folds
                        repeats = 3, # Number of complete sets of folds to compute
                        classProbs = TRUE, # Logical, should probabilities be computed for classification
                        summaryFunction = twoClassSummary) # compute peformance metrics
rpart_model = train(fraud ~ ., #variables
                    data = trainset, #Data
                    method = "rpart", #Model type
                    tuneLength = 10, # number of levels for each tuning parameters that should be generated
                    metric = "ROC", #to measure accuracy
                    trControl = control) # list of values to define how function acts

rpart_model$bestTune
  #Predict on testset
rpart_train_pred <- predict(rpart_model, testset)
confusionMatrix(testset$fraud, rpart_train_pred,positive = "Yes")
#Plot ROC
library(pROC)
rpart_model_prob <- predict(rpart_model, testset, type = "prob")
rpart_model_ROC <- roc(response = testset$fraud, predictor = rpart_model_prob$Yes,
                    levels = levels(testset$fraud))
plot(rpart_model_ROC, col = "red")

auc(rpart_model_ROC)
  #model using C50
c50model <- C5.0(trainset[-10], trainset$fraud)
c50model

summary(c50model)
  #validation of C50
c50model_pred <- predict(c50model, testset)
CrossTable(testset$fraud, c50model_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testset$fraud, c50model_pred)
  #Boosting C50 model
fraud_boost <- C5.0(trainset[-10], trainset$fraud, trials = 10)
fraud_boost

summary(fraud_boost)
  #validation of boosting c50 model
fraud_boost_pred <- predict(fraud_boost, testset)
CrossTable(testset$fraud, fraud_boost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testset$fraud, fraud_boost_pred, positive = "Yes")
#################################################
#### REMOVE TIME FROM DATASET TO RUN MODEL  ####
################################################

trainset_notime <- trainset[-c(1)]
testset_notime <- testset[-c(1)]
  #rpart model
Fraud_DT_notime <- rpart(fraud ~., data = trainset_notime, cp = .02)
Fraud_DT_notime
  #Accuracy metrics
rpart_pred_notime <- predict(Fraud_DT_notime, testset_notime, type = "class")
confusionMatrix(testset_notime$fraud,rpart_pred_notime, positive = "Yes")
  #plot model
rpart.plot(Fraud_DT_notime, digits = 3)

  #model using 10-fold cross validation
rpart_model_notime = train(fraud ~ ., 
                    data = trainset_notime, 
                    method = "rpart", 
                    tuneLength = 10, 
                    metric = "ROC", 
                    trControl = control) 
  #Predict on testset
rpart_train_pred_notime <- predict(rpart_model_notime, testset_notime)
confusionMatrix(testset_notime$fraud, rpart_train_pred_notime, positive = "Yes")

  #ROC
rpart_model_notime_prob <- predict(rpart_model_notime, testset_notime, type = "prob")
rpart_model_notime_ROC <- roc(response = testset_notime$fraud, predictor = rpart_model_notime_prob$Yes,
                              levels = levels(testset_notime$fraud))

plot(rpart_model_notime_ROC, col = "red")
auc(rpart_model_notime_ROC)
  #C50 Model for removal of time dataset:
c50model_notime <- C5.0(trainset_notime[,-9], trainset_notime$fraud)
c50model_notime

summary(c50model_notime)
  #validation of C50
c50model_notime_pred <- predict(c50model_notime, testset_notime)
CrossTable(testset_notime$fraud, c50model_notime_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testset_notime$fraud,c50model_notime_pred, positive = "Yes")
  #boosting C50 model
fraud_boost_notime <- C5.0(trainset_notime[-9], trainset_notime$fraud, trials = 10)
fraud_boost_notime

summary(fraud_boost_notime)
  #validation of boosting
fraud_boost_notime_pred <- predict(fraud_boost_notime, testset_notime)
CrossTable(testset_notime$fraud, fraud_boost_notime_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testset_notime$fraud, fraud_boost_notime_pred, positive = "Yes")
############################################
### REMOVING HIGHLY CORRELATED FEATURES ###
##########################################

trainset_noOld <- trainset_notime[-c(4,7)]
testset_noOld <- testset_notime[-c(4,7)]


#rpart model
Fraud_DT_noOld <- rpart(fraud ~., data = trainset_noOld, cp = .02)
Fraud_DT_noOld
#Accuracy metrics
rpart_pred_noOld <- predict(Fraud_DT_noOld, testset_noOld, type = "class")
confusionMatrix(testset_noOld$fraud,rpart_pred_noOld, positive = "Yes")
#plot model
rpart.plot(Fraud_DT_noOld, digits = 3)

#model using 10-fold cross validation
rpart_model_noOld = train(fraud ~ ., 
                           data = trainset_noOld, 
                           method = "rpart", 
                           tuneLength = 10, 
                           metric = "ROC", 
                           trControl = control) 
#Predict on testset
rpart_train_pred_noOld <- predict(rpart_model_noOld, testset_noOld)
confusionMatrix(testset_noOld$fraud, rpart_train_pred_noOld, positive = "Yes")

#ROC
rpart_model_noOld_prob <- predict(rpart_model_noOld, testset_noOld, type = "prob")
rpart_model_noOld_ROC <- roc(response = testset_noOld$fraud, predictor = rpart_model_noOld_prob$Yes,
                              levels = levels(testset_noOld$fraud))

plot(rpart_model_noOld_ROC, col = "red")
auc(rpart_model_noOld_ROC)
#C50 Model for removal of time dataset:
c50model_noOld <- C5.0(trainset_noOld[-9], trainset_noOld$fraud)
c50model_noOld

summary(c50model_noOld)
#validation of C50
c50model_noOld_pred <- predict(c50model_noOld, testset_noOld)
CrossTable(testset_noOld$fraud, c50model_noOld_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testset_noOld$fraud,c50model_noOld_pred, positive = "Yes")
#boosting C50 model
fraud_boost_noOld <- C5.0(trainset_noOld[-9], trainset_noOld$fraud, trials = 10)
fraud_boost_noOld

summary(fraud_boost_noOld)
#validation of boosting
fraud_boost_noOld_pred <- predict(fraud_boost_noOld, testset_noOld)
CrossTable(testset_noOld$fraud, fraud_boost_noOld_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testset_noOld$fraud, fraud_boost_noOld_pred, positive = "Yes")
###############################################
### Downsampling to 58K records to compare ###
#############################################

#downsampling non-fraud data points to run SVM and increase fraud percentage
downSampleFraudno <- svm_fraud_2 %>% filter(fraud == "No")
fraud_only <- svm_fraud_2 %>% filter(fraud == "Yes")
#check
table(Fraud_Data$isFraud)
#creating new dataset
set.seed(456)
downSampleFraud <- sample_n(downSampleFraudno, 50000)
downSampleFraud <- rbind(downSampleFraud, fraud_only)
dim(downSampleFraud)

#Split downsample data
set.seed(123)
ind = createDataPartition(downSampleFraud$fraud, p = .8, list = FALSE)
trainset_DS = downSampleFraud[ind,]
testset_DS = downSampleFraud[-ind,]
#check
dim(trainset_DS)
dim(testset_DS)

#rpart model
Fraud_DT_DS <- rpart(fraud ~., data = trainset_DS, cp = .02)
Fraud_DT_DS

#Accuracy metrics
rpart_pred_DS <- predict(Fraud_DT_DS, testset_DS, type = "class")
confusionMatrix(testset_DS$fraud,rpart_pred_DS, positive = "Yes")
#plot model
rpart.plot(Fraud_DT_DS, digits = 3)

#model using 10-fold cross validation
rpart_model_DS = train(fraud ~ ., 
                          data = trainset_DS, 
                          method = "rpart", 
                          tuneLength = 10, 
                          metric = "ROC", 
                          trControl = control) 
#Predict on testset
rpart_train_pred_DS <- predict(rpart_model_DS, testset_DS)
confusionMatrix(testset_DS$fraud, rpart_train_pred_DS, positive = "Yes")

#ROC
rpart_model_DS_prob <- predict(rpart_model_DS, testset_DS, type = "prob")
rpart_model_DS_ROC <- roc(response = testset_DS$fraud, predictor = rpart_model_DS_prob$Yes,
                             levels = levels(testset_DS$fraud))

plot(rpart_model_DS_ROC, col = "red")
auc(rpart_model_DS_ROC)
#C50 Model:
c50model_DS <- C5.0(trainset_DS[-9], trainset_DS$fraud)
c50model_DS

summary(c50model_DS)
#validation of C50
c50model_DS_pred <- predict(c50model_DS, testset_DS)
CrossTable(testset_DS$fraud, c50model_DS_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testset_DS$fraud,c50model_DS_pred, positive = "Yes")
#boosting C50 model
fraud_boost_DS <- C5.0(trainset_DS[-9], trainset_DS$fraud, trials = 10)
fraud_boost_DS

summary(fraud_boost_DS)
#validation of boosting
fraud_boost_DS_pred <- predict(fraud_boost_DS, testset_DS)
CrossTable(testset_DS$fraud, fraud_boost_DS_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testset_DS$fraud, fraud_boost_DS_pred, positive = "Yes")

###############################################
### Downsampling and removing time         ###
#############################################
trainsetDS_notime <- trainset_DS[,-1]
testsetDS_notime <- testset_DS[,-1]

#rpart model
Fraud_DS_notime <- rpart(fraud ~., data = trainsetDS_notime, cp = .02)
Fraud_DS_notime

#Accuracy metrics
rpart_pred_DS_notime <- predict(Fraud_DS_notime, testsetDS_notime, type = "class")
confusionMatrix(testsetDS_notime$fraud,rpart_pred_DS_notime, positive = "Yes")
#plot model
rpart.plot(Fraud_DS_notime, digits = 3)

#model using 10-fold cross validation
rpart_model_DS_notime = train(fraud ~ ., 
                       data = trainsetDS_notime, 
                       method = "rpart", 
                       tuneLength = 10, 
                       metric = "ROC", 
                       trControl = control) 
#Predict on testset
rpart_train_pred_DS_notime <- predict(rpart_model_DS_notime, testsetDS_notime)
confusionMatrix(testsetDS_notime$fraud, rpart_train_pred_DS_notime, positive = "Yes")

#ROC
rpart_model_DS_prob_notime <- predict(rpart_model_DS_notime, testsetDS_notime, type = "prob")
rpart_model_DS_ROC_notime <- roc(response = testsetDS_notime$fraud, predictor = rpart_model_DS_prob$Yes,
                          levels = levels(testsetDS_notime$fraud))

plot(rpart_model_DS_ROC_notime, col = "red")
auc(rpart_model_DS_ROC_notime)
#C50 Model:
c50model_DS_notime <- C5.0(trainsetDS_notime[-9], trainsetDS_notime$fraud)
c50model_DS_notime

summary(c50model_DS_notime)
#validation of C50
c50model_DS_pred_notime <- predict(c50model_DS_notime, testsetDS_notime)
CrossTable(testsetDS_notime$fraud, c50model_DS_pred_notime,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testsetDS_notime$fraud,c50model_DS_pred_notime, positive = "Yes")
#boosting C50 model
fraud_boost_DS_notime <- C5.0(trainsetDS_notime[-9], trainsetDS_notime$fraud, trials = 10)
fraud_boost_DS_notime

summary(fraud_boost_DS_notime)
#validation of boosting
fraud_boost_DS_pred_notime <- predict(fraud_boost_DS_notime, testsetDS_notime)
CrossTable(testsetDS_notime$fraud, fraud_boost_DS_pred_notime,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testsetDS_notime$fraud, fraud_boost_DS_pred_notime, positive = "Yes")

#################################################
#### RUNNING MODEL NO DUMMY VARIABLES       ####
###############################################
LTrain <- trainset[-c(1,2,4,7)]
LTest <- testset[-c(1,2,4,7)]

#Decision model for visual and logic
Fraud_DT_noDum <- rpart(fraud ~., data = LTrain, cp = .02)
Fraud_DT_noDum
#plot tree
fraud_DT_plot_noDum <- rpart.plot(Fraud_DT_noDum, digits = 3)
fraud_DT_plot_noDum
#Accuracy metrics
rpart_pred_noDum <- predict(Fraud_DT_noDum, LTest, type = "class")
confusionMatrix(LTest$fraud,rpart_pred_noDum, positive = "Yes")

#rpart model using 10-fold cross validation
rpart_model_noDum = train(fraud ~ ., #variables
                    data = LTrain, #Data
                    method = "rpart", #Model type
                    tuneLength = 10, # number of levels for each tuning parameters that should be generated
                    metric = "ROC", #to measure accuracy
                    trControl = control) # list of values to define how function acts
#Predict on testset
rpart_train_pred_noDum <- predict(rpart_model_noDum, LTest)
confusionMatrix(LTest$fraud, rpart_train_pred_noDum,positive = "Yes")
#Plot ROC
rpart_model_prob_noDum <- predict(rpart_model_noDum, LTest, type = "prob")
rpart_model_ROC_noDum <- roc(response = LTest$fraud, predictor = rpart_model_prob_noDum$Yes,
                       levels = levels(LTest$fraud))
plot(rpart_model_ROC_noDum, col = "red")

auc(rpart_model_ROC_noDum)
#model using C50
c50model_noDum <- C5.0(LTrain[-6], LTrain$fraud)
c50model_noDum

summary(c50model_noDum)
#validation of C50
c50model_pred_noDUm <- predict(c50model_noDum, LTest)
CrossTable(LTest$fraud, c50model_pred_noDUm,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(LTest$fraud, c50model_pred_noDUm)
#Boosting C50 model
fraud_boost_noDum <- C5.0(LTrain[-6], LTrain$fraud, trials = 10)
fraud_boost_noDum

summary(fraud_boost_noDum)
#validation of boosting c50 model
fraud_boost_pred_noDum <- predict(fraud_boost_noDum, LTest)
CrossTable(LTest$fraud, fraud_boost_pred_noDum,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(LTest$fraud, fraud_boost_pred_noDum, positive = "Yes")

###############################################
### Downsampling no dummy variables        ###
#############################################
trainsetDS_noDum <- trainset_DS[-c(1,2,4,7)]
testsetDS_noDum <- testset_DS[-c(1,2,4,7)]

#rpart model
Fraud_DS_noDum <- rpart(fraud ~., data = trainsetDS_noDum, cp = .02)
Fraud_DS_noDum

#Accuracy metrics
rpart_pred_DS_noDum <- predict(Fraud_DS_notime, testsetDS_noDum, type = "class")
confusionMatrix(testsetDS_noDum$fraud,rpart_pred_DS_noDum, positive = "Yes")
#plot model
rpart.plot(rpart_pred_DS_noDum, digits = 3)

#model using 10-fold cross validation
rpart_model_DS_noDum = train(fraud ~ ., 
                              data = trainsetDS_noDum, 
                              method = "rpart", 
                              tuneLength = 10, 
                              metric = "ROC", 
                              trControl = control) 
#Predict on testset
rpart_train_pred_DS_noDum <- predict(rpart_model_DS_noDum, testsetDS_noDum)
confusionMatrix(testsetDS_noDum$fraud, rpart_train_pred_DS_noDum, positive = "Yes")

#ROC
rpart_model_DS_prob_noDum <- predict(rpart_model_DS_noDum, testsetDS_noDum, type = "prob")
rpart_model_DS_ROC_noDum <- roc(response = testsetDS_noDum$fraud, predictor = rpart_model_DS_prob_noDum$Yes,
                                 levels = levels(testsetDS_noDum$fraud))

plot(rpart_model_DS_ROC_noDum, col = "red")
auc(rpart_model_DS_ROC_noDum)
#C50 Model:
c50model_DS_noDum <- C5.0(trainsetDS_noDum[-6], trainsetDS_noDum$fraud)
c50model_DS_noDum

summary(c50model_DS_noDum)
#validation of C50
c50model_DS_pred_noDum <- predict(c50model_DS_noDum, testsetDS_noDum)
CrossTable(testsetDS_noDum$fraud, c50model_DS_pred_noDum,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testsetDS_noDum$fraud,c50model_DS_pred_noDum, positive = "Yes")
#boosting C50 model
fraud_boost_DS_noDum <- C5.0(trainsetDS_noDum[-6], trainsetDS_noDum$fraud, trials = 10)
fraud_boost_DS_noDum

summary(fraud_boost_DS_noDum)
#validation of boosting
fraud_boost_DS_pred_noDum <- predict(fraud_boost_DS_noDum, testsetDS_noDum)
CrossTable(testsetDS_noDum$fraud, fraud_boost_DS_pred_noDum,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Fraud','Predicted Fraud'))
confusionMatrix(testsetDS_noDum$fraud, fraud_boost_DS_pred_noDum, positive = "Yes")


########################
### Random Forest   ###
######################
library(randomForest)
rpart_model_DS_notime = train(fraud ~ ., 
                              data = trainsetDS_notime, 
                              method = "rf", 
                              tuneLength = 10, 
                              metric = "ROC", 
                              trControl = control) 
plot(rpart_model_DS_notime$finalModel)
rf_train_pred <- predict(rpart_model_DS_notime, testset_notime)
confusionMatrix(testset_notime$fraud, rf_train_pred, positive = "Yes")
