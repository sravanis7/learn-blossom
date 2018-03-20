# Logistic Model for predicting which customer will by term deposit-Version 1 by Gopesh Dwivedi

#1 importing packages
install.packages("pacman")
pacman::p_load(tidyr,devtools,MASS,caret,caTools,ROCR,pROC,VIF,car)

#2 reading data from the file
directory <- "/Users/gopeshdwivedi/Desktop/EBAC5/EB5101-FBA/Datasets/"
filename <- "bank-additional/bank-additional-full.csv"
setwd(directory)
data <- read.csv(filename)
data2 <- data # saving original dataset in data2

#3 Exploration
dim(data) #checking the dimensions of data
str(data) # structure of the data
summary(data)  # statistical summary of data
round(cor(data[sapply(data, is.numeric)]),2) # correlation among numeric fields

#4 Visualization

#5 Cleaning and Transforming

### Combining all different basic education levels ###
data$education <- as.character(data$education)
data$education[which(data$education=="basic.4y" | data$education=="basic.6y" | data$education=="basic.9y")] <- "basic"
data$education <- as.factor(data$education) 
summary(data$education)

data$loan <- as.character(data$loan)
data$housing <- as.character(data$housing)
data$loan[which(data$loan=="yes")] <- 1
data$loan[which(data$loan=="no")] <- 0
data$housing[which(data$housing=="yes")] <- 1
data$housing[which(data$housing=="no")] <- 0
data$loan <- as.factor(data$loan)
data$housing <- as.factor(data$housing)

### -end- ###

### finding missing values and treatment ###

# Contingency Matrices for variables where unknown
tab1<- with(warpbreaks, table(data$default,data$y))
tab2<- with(warpbreaks, table(data$education,data$y))
tab3<- with(warpbreaks, table(data$marital,data$y))
tab4<- with(warpbreaks, table(data$loan,data$y))
tab5<- with(warpbreaks, table(data$housing,data$y))
tab6<- with(warpbreaks, table(data$job,data$y))
tab1
tab2
tab3
tab4
tab5
tab6
# Chi-squared tests
chisq.test(tab1,simulate.p.value = TRUE) 
chisq.test(tab2,simulate.p.value = TRUE) 
chisq.test(tab3,simulate.p.value = TRUE)
chisq.test(tab4,simulate.p.value = TRUE)
chisq.test(tab5,simulate.p.value = TRUE)
chisq.test(tab6,simulate.p.value = TRUE)

# Missing Value Treatment for Marital, Job- Remove rows with "unknown" 
data$marital <- as.character(data$marital) # convert to strings for removing unknown
data$job <- as.character(data$job)
data$loan <- as.character(data$loan)
data$housing <- as.character(data$housing)
data <- data[!(data$marital=="unknown"),] # remove rows where value is unknown
data <- data[!(data$job=="unknown"),]
data <- data[!(data$loan=="unknown"),]
data <- data[!(data$housing=="unknown"),]
data$marital <- as.factor(data$marital) # convert back to factors for modelling
data$job <- as.factor(data$job)
data$loan <- as.factor(data$loan)
data$housing <- as.factor(data$housing)
dim(data)
summary(data$marital)
summary(data$job)
summary(data$loan)
summary(data$housing)

write.csv(data,"bank_cleaned.csv")
### -end- ###

#6 Data Preparation
set.seed(88)
splitr <- sample.split(data$y, SplitRatio = 0.7)
train <- subset( data ,splitr == "TRUE")
test <- subset( data , splitr == "FALSE")
write.csv(train,"train.csv")
write.csv(test,"test.csv")

#8 Model Preparation

basemodel <- glm(train$y ~ .-duration,data = train, family = binomial, x=TRUE)
finalmodel <- glm(train$y ~ month +default+job + campaign +contact + poutcome + cons.conf.idx + nr.employed + pdays, data = train, family = binomial, x=TRUE)
summary(basemodel)
summary(finalmodel)
train$prob_train_base <- predict(basemodel,train,type = "response")
train$prob_train_final <- predict(finalmodel,train,type = "response")
conf_base_1 <- with(warpbreaks,table(actualvalue=train$y,predictedvalue=train$prob_train_base>0.5))
conf_final_1 <- with(warpbreaks,table(actualvalue=train$y,predictedvalue=train$prob_train_final>0.5))
conf_base_2 <- with(warpbreaks,table(actualvalue=train$y,predictedvalue=train$prob_train_base>0.25))
conf_final_2 <- with(warpbreaks,table(actualvalue=train$y,predictedvalue=train$prob_train_final>0.25))
train <- train %>% mutate(y = 1*(y == "yes") + 0)
train <- train  %>% mutate(base_pred = 1*(prob_train_base > .5) + 0)
train <- train  %>% mutate(final_pred = 1*(prob_train_final > .25) + 0)
train <- train %>% mutate(accurate_base = 1*(base_pred == y))
train <- train %>% mutate(accurate_final = 1*(final_pred == y))
summary(train)
base_train_accuracy <- sum(train$accurate_base)/nrow(train)
final_train_accuracy <- sum(train$accurate_final)/nrow(train)
base_train_accuracy
final_train_accuracy

ROCRPred_base <- prediction(train$prob_train_base,train$y)
ROCRPerf_base <- performance(ROCRPred_base,"tpr","fpr")

ROCRPred_final <- prediction(train$prob_train_final,train$y)
ROCRPerf_final <- performance(ROCRPred_final,"tpr","fpr")


auc_base_train <- auc(train$y,train$prob_train_base)
auc_final_train <- auc(train$y,train$prob_train_final)
auc_base_train
auc_final_train

plot(ROCRPerf_base,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
plot(ROCRPerf_final,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

#10 Cross Validation

#11 Results
test$prob_test_base <- predict(basemodel,test,type = "response")
test$prob_test_final <- predict(finalmodel,test,type = "response")
with(warpbreaks,table(actualvalue=test$y,predictedvalue=test$prob_test_base>0.5))
with(warpbreaks,table(actualvalue=test$y,predictedvalue=test$prob_test_final>0.1))
test <- test %>% mutate(y = 1*(y == "yes") + 0)
test <- test  %>% mutate(base_pred = 1*(prob_test_base > .5) + 0)
test <- test  %>% mutate(final_pred = 1*(prob_test_final > .1) + 0)
test <- test %>% mutate(accurate_base = 1*(base_pred == y))
test <- test %>% mutate(accurate_final = 1*(final_pred == y))
auc_base_test <- auc(test$y,test$prob_test_base)
auc_final_test <- auc(test$y,test$prob_test_final)
auc_base_test
auc_final_test

base_test_accuracy <- sum(test$accurate_base)/nrow(test)
final_test_accuracy <- sum(test$accurate_final)/nrow(test)
base_test_accuracy
final_test_accuracy
