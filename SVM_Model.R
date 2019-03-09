head(Fraud_Data,10)
library(stringr)

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

View(svm_fraud)
str(svm_fraud)
summary(svm_fraud)

svm_fraud_2 <- svm_fraud[,-11]
head(svm_fraud_2,1)
#Percent of Fraud
fraud_table <- table(svm_fraud_2$fraud)
8213/6354407 # .1% of data is fraud, may have to remove records to increase the fraud record percentage



#Training Data model with all data
library(caret)
set.seed(123)
ind = createDataPartition(svm_fraud_2$fraud, p = .8, list = FALSE)
trainset = svm_fraud_2[ind,-10]
testset = svm_fraud_2[-ind,-10]

dim(trainset)
dim(testset)

#creating new dataset (trying more test samples)
set.seed(456)
downSampleFraud50 <- sample_n(downSampleFraudno, 50000)
downSampleFraud50 <- rbind(downSampleFraud50, fraud_only)
dim(downSampleFraud50)
table(downSampleFraud50$fraud)
8213/58213

#training/test set
set.seed(123)
ind50K = createDataPartition(downSampleFraud50$fraud, p = .8, list = FALSE)
trainset50K = downSampleFraud50[ind50K,]
testset50K = downSampleFraud50[-ind50K,]


dim(trainset50K)
dim(testset50K)

## Trying Z-NORM ### Min/MAX NORM provided great results but z-normalization was better ###
trainNormZ <- as.data.frame(scale(trainset50K[c(3,5,6,8,9)]))
bindNormZ <- cbind(trainNormZ,
                   type = trainset50K[,2], 
                   transaction_end = trainset50K[,7],
                   fraud = trainset50K[,10])
testNormZ <- as.data.frame(scale(testset50K[c(3,5,6,8,9)]))
testbindNormZ <- cbind(testNormZ,
                       type = testset50K[,2],
                       transaction_end = testset50K[,7],
                       fraud = testset50K[,10])


### Polynomial 97.43%
svm_fraud_linear1000Zpoly = svm(fraud ~., 
                                data = bindNormZ, 
                                type = "C-classification",
                                cost = 1000,
                                kernel = "polynomial",
                                scale = FALSE)
svm_fraud_linear1000Zpoly
test_pred_linear1000Zpoly <- predict(svm_fraud_linear1000Zpoly, newdata = testbindNormZ)
confusionMatrix(test_pred_linear1000Zpoly, testbindNormZ$fraud)



#Model - issues running all data points
library(e1071)
svm_fraud_model = svm(fraud ~., data = svm_fraud_2, scale = FALSE)

#downsampling non-fraud data points to run SVM and increase fraud percentage
library(dplyr)
head(svm_fraud_2,1)
downSampleFraudno <- svm_fraud_2 %>% filter(fraud == "No")
fraud_only <- svm_fraud_2 %>% filter(fraud == "Yes")
dim(fraud_only)
dim(downSampleFraudno)
  #check
table(Fraud_Data$isFraud)
  #creating new dataset
set.seed(456)
downSampleFraud <- sample_n(downSampleFraudno, 25000)
downSampleFraud <- rbind(downSampleFraud, fraud_only)
dim(downSampleFraud)
  #EDA on new data set
summary(downSampleFraud)


  #creating new training data
set.seed(123)
ind = createDataPartition(downSampleFraud$fraud, p = .8, list = FALSE)
trainset = downSampleFraud[ind,]
testset = downSampleFraud[-ind,]

table(testset$fraud)

dim(trainset)
dim(testset)
length(yvar)

#Model 2 - Linear model with cost = 1 --- 81.57%
svm_fraud_linear1 = svm(fraud ~., 
                      data = trainset, 
                      type = "C-classification",
                      cost = 1,
                      kernel = "linear",
                      scale = FALSE)
svm_fraud_linear1
test_pred_linear1 <- predict(svm_fraud_linear1, newdata = testset)
confusionMatrix(test_pred_linear1, testset$fraud)

#Model 3 - Linear model with cost = 100
svm_fraud_linear100 = svm(fraud ~., 
                       data = trainset, 
                       type = "C-classification",
                       cost = 100,
                       kernel = "linear",
                       scale = FALSE)
svm_fraud_linear100
test_pred_linear100 <- predict(svm_fraud_linear100, newdata = testset)
confusionMatrix(test_pred_linear100, testset$fraud)

#Model 4 - polynomial model with cost = 1
svm_fraud_poly1 = svm(fraud ~., 
                          data = trainset, 
                          type = "C-classification",
                          cost = 1,
                          kernel = "polynomial",
                          scale = FALSE)
svm_fraud_poly1
test_pred_poly1 <- predict(svm_fraud_poly1, newdata = testset)
confusionMatrix(test_pred_poly1, testset$fraud)

#Model 5 - polynomial model with cost = 100
svm_fraud_poly100 = svm(fraud ~., 
                      data = trainset, 
                      type = "C-classification",
                      cost = 100,
                      kernel = "polynomial",
                      scale = FALSE)
svm_fraud_poly100
test_pred_poly100 <- predict(svm_fraud_poly100, newdata = testset)
confusionMatrix(test_pred_poly100, testset$fraud)

#Model 6 - Radial model with cost = 1
svm_fraud_radial1 = svm(fraud ~., 
                        data = trainset, 
                        type = "C-classification",
                        cost = 1,
                        kernel = "radial",
                        scale = FALSE)
svm_fraud_radial1
test_pred_radial1 <- predict(svm_fraud_radial1, newdata = testset)
confusionMatrix(test_pred_radial1, testset$fraud)

#Model 7 - Radial model with cost = 100
svm_fraud_radial100 = svm(fraud ~., 
                        data = trainset, 
                        type = "C-classification",
                        cost = 100,
                        kernel = "radial",
                        scale = FALSE)
svm_fraud_radial100
test_pred_radial100 <- predict(svm_fraud_radial100, newdata = testset)
confusionMatrix(test_pred_radial100, testset$fraud)

#Model 8 - Sigmoid model with cost = 1
svm_fraud_sigmoid1 = svm(fraud ~., 
                          data = trainset, 
                          type = "C-classification",
                          cost = 1,
                          kernel = "sigmoid",
                          scale = FALSE)
svm_fraud_sigmoid1
test_pred_sigmoid1 <- predict(svm_fraud_sigmoid1, newdata = testset)
confusionMatrix(test_pred_sigmoid1, testset$fraud)

#Model 9 - Sigmoid model with cost = 100
svm_fraud_sigmoid100 = svm(fraud ~., 
                         data = trainset, 
                         type = "C-classification",
                         cost = 100,
                         kernel = "sigmoid",
                         scale = FALSE)
svm_fraud_sigmoid100
test_pred_sigmoid100 <- predict(svm_fraud_sigmoid100, newdata = testset)
confusionMatrix(test_pred_sigmoid100, testset$fraud)
plot(svm_fraud_linear1$SV)
svm_fraud_linear1$SV
plot(svm_fraud_linear1$coefs)
plot(svm_fraud_linear1$decision.values)

tune_fraud = tune(svm, fraud ~., 
                  data = trainset,
                  kernel = "linear",
                  ranges = list(cost = c(.1,1,10,100,1000), 
                                gamma = c(.5,1,2,3,4)))
summary(tune_fraud)

#creating new dataset (trying more test samples)
set.seed(456)
downSampleFraud50 <- sample_n(downSampleFraudno, 50000)
downSampleFraud50 <- rbind(downSampleFraud50, fraud_only)
dim(downSampleFraud50)
table(downSampleFraud50$fraud)
8213/58213

  #training/test set
set.seed(123)
ind50K = createDataPartition(downSampleFraud50$fraud, p = .8, list = FALSE)
trainset50K = downSampleFraud50[ind50K,]
testset50K = downSampleFraud50[-ind50K,]


dim(trainset50K)
dim(testset50K)

#Test model with more data
svm_fraud_linear50K = svm(fraud ~., 
                              data = trainset50K, 
                              type = "C-classification",
                              cost = 1,
                              kernel = "linear",
                              scale = FALSE)
svm_fraud_linear50K
test_pred_linear50K <- predict(svm_fraud_linear50K, newdata = testset50K)
confusionMatrix(test_pred_linear50K, testset50K$fraud, positive = "Yes")

#Tuning the 58K data model
svm_fraud_tune = tune.svm(fraud ~., 
                          data = trainset50K, 
                          type = "C-classification",
                          gamma = 10^(-6:-1), 
                          cost = 10^(3:5),
                          kernel = "linear",
                          scale = FALSE)
svm_fraud_tune
test_pred_tune <- predict(svm_fraud_tune, newdata = testset50K)
confusionMatrix(test_pred_tune, testset50K$fraud, positive = "Yes")

#Test model with more data
svm_fraud_linear50K0 = svm(fraud ~., 
                          data = trainset50K, 
                          type = "C-classification",
                          cost = 100,
                          kernel = "linear",
                          scale = FALSE)
svm_fraud_linear50K0
test_pred_linear50K0 <- predict(svm_fraud_linear50K0, newdata = testset50K)
confusionMatrix(test_pred_linear50K0, testset50K$fraud, positive = "Yes")

#Test model with more data 87% accuracy 21.39 Kappa
svm_fraud_linear50K10 = svm(fraud ~., 
                           data = trainset50K, 
                           type = "C-classification",
                           cost = 10,
                           kernel = "linear",
                           scale = FALSE)
svm_fraud_linear50K10
test_pred_linear50K10 <- predict(svm_fraud_linear50K10, newdata = testset50K)
confusionMatrix(test_pred_linear50K10, testset50K$fraud, positive = "Yes")

#min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

trainNorm <- as.data.frame(lapply(trainset50K[c(3,5,6,8,9)], normalize))
bind <- cbind(trainNorm,
              type = trainset50K[,2], 
              transaction_end = trainset50K[,7],
              fraud = trainset50K[,10])
testNorm <- as.data.frame(lapply(testset50K[c(3,5,6,8,9)], normalize))
testbind <- cbind(testNorm,
                  type = testset50K[,2],
                  transaction_end = testset50K[,7],
                  fraud = testset50K[,10])
dim(bind)

View(bind)
View(testbind)

#Creating new data frame with normalized data above
#Running model with normalized data ######## BEST MODEL YET

svm_fraud_linear1mmnorm = svm(fraud ~., 
                              data = bind, 
                              type = "C-classification",
                              cost = 1,
                              kernel = "linear",
                              scale = FALSE)
svm_fraud_linear1mmnorm
test_pred_linear1mmnorm <- predict(svm_fraud_linear1mmnorm, newdata = testbind)
confusionMatrix(test_pred_linear1mmnorm, testbind$fraud)

#### Even Better   ### Still better  ### Removed time and transaction start (all cust) as well
svm_fraud_linear100mmnorm = svm(fraud ~., 
                              data = bind, 
                              type = "C-classification",
                              cost = 100,
                              kernel = "sigmoid",
                              scale = FALSE)
svm_fraud_linear100mmnorm
test_pred_linear100mmnorm <- predict(svm_fraud_linear100mmnorm, newdata = testbind)
confusionMatrix(test_pred_linear100mmnorm, testbind$fraud)


svm_fraud_linear100normNoTime = svm(fraud ~., 
                                data = bind, 
                                type = "C-classification",
                                cost = 100,
                                kernel = "linear",
                                scale = FALSE)
svm_fraud_linear100normNoTime
test_pred_linear100normNoTime <- predict(svm_fraud_linear100normNoTime, newdata = testbind)
confusionMatrix(test_pred_linear100normNoTime, testbind$fraud)
#Try Z-Norm after 

#### Model with z normalization 96.2% !!!!
svm_fraud_linear100Znorm = svm(fraud ~., 
                                    data = bindNormZ, 
                                    type = "C-classification",
                                    cost = 100,
                                    kernel = "linear",
                                    scale = FALSE)
svm_fraud_linear100Znorm
test_pred_linear100Znorm <- predict(svm_fraud_linear100Znorm, newdata = testbindNormZ)
confusionMatrix(test_pred_linear100Znorm, testbindNormZ$fraud)


### 96.42% best so far.
svm_fraud_linear10Znorm = svm(fraud ~., 
                               data = bindNormZ, 
                               type = "C-classification",
                               cost = 10,
                               kernel = "linear",
                               scale = FALSE)
svm_fraud_linear10Znorm
test_pred_linear10Znorm <- predict(svm_fraud_linear10Znorm, newdata = testbindNormZ)
confusionMatrix(test_pred_linear10Znorm, testbindNormZ$fraud)

### sigmoid
svm_fraud_linear10Zsig = svm(fraud ~., 
                              data = bindNormZ, 
                              type = "C-classification",
                              cost = 10,
                              kernel = "sigmoid",
                              scale = FALSE)
svm_fraud_linear10Zsig
test_pred_linear10Zsig <- predict(svm_fraud_linear10Zsig, newdata = testbindNormZ)
confusionMatrix(test_pred_linear10Zsig, testbindNormZ$fraud)

### radial 96.56%
svm_fraud_linear10Zrad = svm(fraud ~., 
                             data = bindNormZ, 
                             type = "C-classification",
                             cost = 10,
                             kernel = "radial",
                             scale = FALSE)
svm_fraud_linear10Zrad
test_pred_linear10Zrad <- predict(svm_fraud_linear10Zrad, newdata = testbindNormZ)
confusionMatrix(test_pred_linear10Zrad, testbindNormZ$fraud)


#try to remove a few features
trainsetRem <- trainset[,c(2,3,5,6,7,8,9,10)]
testsetRem  <- testset[,c(2,3,5,6,7,8,9,10)]
svm_fraud_linear1r = svm(fraud ~., 
                        data = trainsetRem, 
                        type = "C-classification",
                        cost = 100,
                        kernel = "linear",
                        scale = FALSE)
svm_fraud_linear1r
test_pred_linear1r <- predict(svm_fraud_linear1r, newdata = testsetRem)
confusionMatrix(test_pred_linear1r, testsetRem$fraud)

