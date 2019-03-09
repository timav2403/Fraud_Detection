setwd("/Users/tz240/Desktop/Tims/School/Regis/MSDS_692")
Fraud_Data <- read.csv("Fraud_Data.CSV")
library(dplyr)
library(ggplot2)
library(ggExtra)
######## EDA of Data ########
View(Fraud_Data)
#Null data?
is.na(Fraud_Data) #Provides list of null values
sum(is.na(Fraud_Data)) #Provides count of null values

sum(complete.cases(Fraud_Data)) #Provides count of non-null values
sum(!complete.cases(Fraud_Data)) #Provides count of null values

#Data types of columns
str(Fraud_Data)

#High Level Overview
JustFraud <- subset(Fraud_Data, isFraud == "1")
JustNonFraud <- subset(Fraud_Data, isFraud == "0")

summary(Fraud_Data)
summary(JustFraud)
summary(JustNonFraud)

madfigures <- data.frame(MADstep = mad(Fraud_Data$step),
                         MADamount = mad(Fraud_Data$amount),
                         MADoldBalanceOrg = mad(Fraud_Data$oldbalanceOrg),
                         MADnewBalanceOrig = mad(Fraud_Data$newbalanceOrig),
                         MADoldBalanceDest = mad(Fraud_Data$oldbalanceDest),
                         MADnewBalanceDest = mad(Fraud_Data$newbalanceDest))
madJustFraud <- data.frame(MADstep = mad(JustFraud$step),
                         MADamount = mad(JustFraud$amount),
                         MADoldBalanceOrg = mad(JustFraud$oldbalanceOrg),
                         MADnewBalanceOrig = mad(JustFraud$newbalanceOrig),
                         MADoldBalanceDest = mad(JustFraud$oldbalanceDest),
                         MADnewBalanceDest = mad(JustFraud$newbalanceDest))
madJustNonFraud <- data.frame(MADstep = mad(JustNonFraud$step),
                         MADamount = mad(JustNonFraud$amount),
                         MADoldBalanceOrg = mad(JustNonFraud$oldbalanceOrg),
                         MADnewBalanceOrig = mad(JustNonFraud$newbalanceOrig),
                         MADoldBalanceDest = mad(JustNonFraud$oldbalanceDest),
                         MADnewBalanceDest = mad(JustNonFraud$newbalanceDest))



madfigures
madJustFraud
madJustNonFraud

isfraud <- table(Fraud_Data$isFraud)
isflagfraud <- table(Fraud_Data$isFraud)
#Clean Summary
library(stringr)

Cleaning_Fraud_Data <- data.frame(hours = Fraud_Data$step,
                        type = Fraud_Data$type,
                        amount = Fraud_Data$amount,
                        transaction_start = str_sub(Fraud_Data$nameOrig,1,1),
                        old_balance_orig = Fraud_Data$oldbalanceOrg,
                        new_balance_orig = Fraud_Data$newbalanceOrig,
                        transaction_end = str_sub(Fraud_Data$nameDest,1,1),
                        old_balance_dest = Fraud_Data$oldbalanceDest,
                        new_balance_dest = Fraud_Data$newbalanceDest,
                        fraud = factor(Fraud_Data$isFraud, levels = c("0", "1"), labels = c("No", "Yes")),
                        fraud_flag = factor(Fraud_Data$isFlaggedFraud, levels = c("0", "1"), labels = c("No", "Yes")))

Clean_Fraud <- subset(Cleaning_Fraud_Data, fraud == "Yes")
Clean_NonFraud <- subset(Cleaning_Fraud_Data, fraud == "No")
summary(Cleaning_Fraud_Data)
summary(Clean_Fraud)
summary(Clean_NonFraud)

#Count of fraudulent transactions
isflagfraud
isfraud


#Visual of fraud against amount transferred
ggplot(Fraud_Data, aes(x = newbalanceOrig, 
                       y = amount, 
                       color = isFraud, 
                       shape = type)) +
  geom_point()

#view fraud only records
amountFraud <- subset(Fraud_Data, isFraud == "1")
plot(Fraud_Data$amount, color = Fraud_Data$type)

### Hours of transactions ###
step <- table(Fraud_Data$step)
step <- as.data.frame(step)
step <- data.frame(hours = step$Var1,
                   frequency = step$Freq)
step
str(step)
plot(step)
hist(Fraud_Data$step, 
     main = "# of Transactions per Hr", 
     xlab = "Hour", 
     ylab = "Frequency", 
     col = "Dark Green", 
     labels = TRUE,
     breaks = c(0,24,48,72,96,120,144,168,192,216,240,264,288,
                312,336,360,384,408,432,456,480,504,528,552,
                576,600,624,648,672,696,720,744))

### Type ###
Type <- table(Fraud_Data$type)
Type
plot(Type)
percent <- round(100*Type/sum(Type),1)
pie(Type, labels = percent, main = "Transaction Type", col = rainbow(length(Type)))
legend("topright", c("Cash IN", "Cash Out", "Debit", "Payment", "Transfers"), cex = 1, fill = rainbow(length(Type)))

### Amount ###
Amount <- table(Fraud_Data$amount)
Amount
Amount100 <- subset(Fraud_Data, amount <= 100000)
summary(Fraud_Data$amount)
hist(Amount100$amount, main = "Amounts")
boxplot(Fraud_Data$amount)

##Amount, Fraud, Type ##
sp <- ggplot(Amount100, aes(Amount100$amount)) +
  geom_histogram(col = "red",
                 aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red")
sp + facet_grid(isFraud ~ type, scales = "free")

## Old to New Bal Original ##
ap <- ggplot(Fraud_Data, aes(x = newbalanceOrig, y = oldbalanceOrg)) +
  geom_point(shape = 1)
ap + facet_grid(isFraud ~ type, scales = "free")

## Old to New Bal destination ##
bp <- ggplot(Fraud_Data, aes(x = newbalanceDest, y = oldbalanceDest)) +
  geom_point(shape = 1)
bp + facet_grid(isFraud ~ type, scales = "free")


### Customer Name ###
Start_Cust <- table(Fraud_Data$nameOrig)
Start_Cust <- as.data.frame(Start_Cust)
str(Start_Cust)
sort(Start_Cust$Freq, decreasing = TRUE)

### Initial Balance ###
Beg_Balance <- table(Fraud_Data$oldbalanceOrg)
Beg_Balance
hist(Fraud_Data$oldbalanceOrg, main = "Initial Balance Before Transaction")

### Balance After Transaction ###
Bal_Aft_Trans <- table(Fraud_Data$newbalanceOrig)
Bal_Aft_Trans
hist(Fraud_Data$newbalanceOrig, main = "Balance After Transaction")


#see how many accounts were cleaned out.
zero <- New_Data %>% filter(New_Data$Balance == 0 & New_Data$newbalanceOrig != 0 & New_Data$oldbalanceOrg != 0)
count(zero)

#attempting 3d plot
library(plot3D)
scatter3D(fraud_only$new_balance_orig,
          fraud_only$new_balance_dest, 
          fraud_only$amount, 
          colvar = NULL,
          theta = 15, 
          phi = 20,
          bty = "g",
          main = "Fraud Transfer Amonts",
          zlab = "Transferred Amount",
          ylab = "New Balance Fraud Account",
          xlab = "New Balance of Victim",
          col = fraud_only$type,
          ticktype = "detailed")

#correlation
library(reshape)
corFraud <- Fraud_Data[,c(3,5,6,8,9)]
cor(corFraud)
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
upper_tri

melted_fraud <- melt(upper_tri, na.rm = TRUE)
View(melted_fraud)
ggplot(data = melted_fraud, aes(X2, X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "Dark Red", high = "Dark Green", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#correlation did not look right, trying another correlation plot
install.packages("corrplot")
library("corrplot")
cor_Fraud <- cor(corFraud)
corrplot(cor_Fraud, type="upper", order="hclust", tl.col="black", tl.srt=45)

plot(Fraud_Data$oldbalanceOrg, Fraud_Data$newbalanceOrig)


##Amount, Fraud, Type for amounts > 100K ##
AmountPlus <- subset(Fraud_Data, amount > 100000)
sp <- ggplot(AmountPlus, aes(AmountPlus$amount)) +
  geom_histogram(col = "red",
                 aes(fill = ..count..),
                 bins = 10) +
  scale_fill_gradient("Count", low = "green", high = "red")
sp + facet_grid(isFraud ~ type, scales = "free")


#Review Old to new balance
scatter <- ggplot(data = Fraud_Data, aes(x = oldbalanceOrg, y = newbalanceOrig)) +
  geom_point(shape = Fraud_Data$isFraud)
scatter

#conclusion
results <- data.frame(NN_notime_norm = c(99.94, 70.44, 98.11),
                      NN_DS_notime_norm = c(97.82, 90.59, 99.59),
                      DT_CV_Rpart_all = c(99.7, 85.39, 94.59),
                      DT_DS_Boost_noDum = c(99.46,97.78,NA),
                      SVM_norm_10R = c(96.58, 85.15, NA),
                      SVM_norm_1000P = c(97.43, 88.91, NA))

results <- data.frame(NN_notime_norm = c(99.94, 70.44, 98.11),
                      NN_DS_notime_norm = c(97.82, 90.59, 99.59),
                      DT_CV_Rpart_all = c(99.7, 85.39, 94.59),
                      DT_DS_Boost_noDum = c(99.46,97.78,97.3),
                      SVM_norm_10R = c(96.58, 85.15, 81.9),
                      SVM_norm_1000P = c(97.43, 88.91, 85.5))

rownames(results) <- c("Accuracy", "Kappa", "ROC")



colours <- c("cyan4", "brown1", "darkgoldenrod2")


barplot(as.matrix(results), 
        main="Accuracy Metrics", 
        ylab = "Percentage", 
        cex.lab = 1.5, 
        cex.main = 1.4, 
        beside=TRUE, 
        col=colours)
legend("topright", 
       c("Accuracy","Kappa","ROC"), 
       cex=.85, 
       bty="n", 
       fill=colours)

