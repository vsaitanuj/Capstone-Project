###Installation and Importing of requiered libraries ####
install.packages("readxl")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("corrgram")
install.packages("corrplot")
install.packages("DMwR")
install.packages("caTools")
install.packages("forcats")
install.packages("ineq")
install.packages("memisc")
install.packages("psych")
install.packages("Prediction")
install.packages("ROCR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("class")
install.packages("ipred")
install.packages("xgboost")
install.packages("gbm")
library(Hmisc)
library(readxl)
library(ggplot2)
library(corrgram)
library(corrplot)
library(DMwR)
library(caTools)
library(forcats)
library(randomForest)
library(InformationValue)
library(ineq)
library(memisc)
library(psych)
library(Prediction)
library(ROCR)
library(rpart)
library(rpart.plot)
library(class)
library(ipred)
library(xgboost)
library(gbm)
#### Setting Working directory and importing dataset ####
setwd("C:/greatlakes/capstone/Loan Default")
loan = readxl::read_xlsx("loan default_data.xlsx")
### Initial Exploration ####
class(loan)
##Changing the class of the dataset to data frame ####
loan = as.data.frame(loan)
head(loan)
tail(loan)
str(loan)
dim(loan)
### Conversion of variables to desired data types ####
loan$home_ownership = as.factor(loan$home_ownership)
loan$annual_inc = as.numeric(loan$annual_inc)
loan$verification_status = as.factor(loan$verification_status)
loan$issue_d = as.factor(loan$issue_d)
loan$pymnt_plan = as.factor(loan$pymnt_plan)
loan$home_ownership = as.factor(loan$home_ownership)
loan$verification_status = as.factor(loan$verification_status)
loan$purpose = as.factor(loan$purpose)
loan$addr_state = as.factor(loan$purpose)
loan$next_pymnt_d = as.factor(loan$next_pymnt_d)
loan$last_credit_pull_d = as.factor(loan$last_credit_pull_d)
loan$last_pymnt_d = as.factor(loan$last_pymnt_d)
loan$application_type = as.factor(loan$application_type)
loan$loan_status = as.factor(loan$loan_status)
loan$emp_length = as.factor(loan$emp_length)
loan$grade = as.factor(loan$grade)
loan$earliest_cr_line = as.factor(loan$earliest_cr_line)
loan$term = as.factor(loan$term)
### Removal of DESC and ID variables ####
loan = loan[,-c(1,15)]
###Uni- Variate Analyisis ####
summary(loan)
### Numeric Variable Analysis ####
hist(loan[,c(1,2,3,5,6)])
hist(loan[,c(10,15:18)])
hist(loan[,c(19:25)])
hist(loan[,c(25:31)])
### Categorical Variable Analysis ####
table(loan$grade)
table(loan$addr_state)
table(loan$emp_length)
table(loan$home_ownership)
table(loan$verification_status)
table(loan$issue_d)
table(loan$pymnt_plan)
table(loan$purpose)
table(loan$collection_recovery_fee)
table(loan$recoveries)
table(loan$application_type)
table(loan$loan_status)
### Multi-Variate Analysis ####
## Numerical Variable with numerical variable ###
qplot(loan$delinq_2yrs,loan$funded_amnt_inv,geom = "bin2d",col = loan$total_acc,
      xlab = "Number of 30+ days without being delinquent",
      ylab = "Loan amount approved by investors",
      main = "Delinquency vs Loan Amount approved by investors")
qplot(loan$total_acc,loan$total_rec_prncp,geom = "jitter",col = loan$total_acc,
      xlab = "Current number of Credit Lines",
      ylab = "Principal Received to date",
      main = "Number of Credit lines vs Principal Received")
qplot(loan$int_rate,loan$installment,col = loan$int_rate,
      xlab = "Interest Rate",
      ylab = "Installment Amount",
      main = "Interest Rate vs. Installment Amount")
## Numerical Variable against Categorical Variable ####
qplot(loan$funded_amnt,fill = loan$home_ownership,
      xlab = "Loan Amount required by the burrower",
      ylab = "Frequency of the burrowers",
      main = "Loan amount required vs Ownership of home")+labs(fill = "Home Ownership")
qplot(loan$grade,loan$funded_amnt_inv,fill = loan$loan_status,geom = "boxplot",
      xlab = "Grade assigned to loan",
      ylab = "Loan amount approved by the investors",
      main = "Seggregation of Grades according to Loan amount",)+labs(fill = "Grade")
qplot(loan$funded_amnt_inv,fill = loan$loan_status,geom = "histogram",main = "Amount Sanctioned vs Loan Status")+
   scale_fill_manual(values = c("Red","Blue"))+labs(fill = "Loan status",x = "Amount funded by investors",y = "Frequency")
## Categorical Variable against Categorical variables ####
qplot(loan$grade,fill = loan$application_type,geom = "bar",
      xlab = "Loan Grade",
      ylab = "Frequency",
      main = "Loan Grade vs Application type")+labs(fill = "Application Type")
table(loan$purpose,loan$pymnt_plan)
table(loan$loan_status,loan$verification_status)
## Correlation Plot ####
corp = loan[,-c(4,7,8,9,11,12,13,14,15,18,34,36,37,38,39)]
corp = as.data.frame(corp)
corm = cor(corp)
corrplot(corm,number.digits = TRUE)

### Treatment of NAs ####
colSums(is.na(loan))
loan$mths_since_last_delinq[is.na(loan$mths_since_last_delinq)] = median(loan$mths_since_last_delinq,na.rm = TRUE)
loan$revol_util[is.na(loan$revol_util)] = median(loan$revol_util,na.rm = TRUE)
colSums(is.na(loan))

### Treating Outliers ####
boxplot(loan[,c(1,2,3,5,6,10,16,17,19:33,35)])
q = c(1,2,3,5,6,10,16,17,19:33,35)
for(i in q){
   iq = IQR(loan[,i])
   z = quantile(loan[,i])
   z= as.data.frame(z)
   range1 = z[2,1] - 1.5*iq
   range2 = z[4,1] + 1.5*iq
   loan[,i][loan[,i]<range1] = range1
   loan[,i][loan[,i]>range2] = range2
}
boxplot(loan[,c(1,2,3,5,6,10,16,17,19:33,35)])

### Removal of Unwanted variables ####
str(loan)
colnames(loan)
loan = loan[,-c(37,36,34,12,18)]
colSums(is.na(loan))
summary(loan)
### SMOTE ####
table(loan$loan_status)
prop.table(table(loan$loan_status))
dim(loan)
set.seed(77)
s.loan =SMOTE(loan_status~.,loan,perc.over = 494.8, perc.under = 121)
table(s.loan$loan_status)
prop.table(table(s.loan$loan_status))

#### PROJECT NOTES PART 2 #####
### Conversion ####
loan1 = s.loan
loan1$loan_status = ifelse(loan1$loan_status == "Default","1","0")
loan1$loan_status = as.factor(loan1$loan_status)
table(s.loan$loan_status)
table(loan1$loan_status)

### splitting of dataset ####
set.seed(77)
split <- sample.split(loan1$loan_status, SplitRatio = 0.70)
train<- subset(loan1, split == TRUE)
test<- subset(loan1, split == FALSE)
dim(train)
dim(test)



### Logistic Regression ####
log.model = glm(loan_status~.,train,family = "binomial")
summary(log.model)
log.model2 = glm(loan_status~loan_amnt+funded_amnt+funded_amnt_inv+int_rate+
             term+
             inq_last_6mths+open_acc+total_pymnt+
             total_pymnt_inv+total_rec_prncp+last_pymnt_amnt,train,family = "binomial")
summary(log.model2)
### making predictions with Logistic Regression ####
prob = predict(log.model2,test,type = "response")
logplot = qplot(prob,test$loan_status,geom = "boxplot")
logplot+geom_vline(xintercept = 0.55,col = "Red")
pred = ifelse(prob < 0.55,"0","1")
table(pred)
table(test$loan_status)
test$loan_status = as.factor(test$loan_status)
pred = as.factor(pred)
plot(test$loan_status,pred,xlab = "Actuals",ylab = "Predicted",
     col = c("Violet","Red"),main = "Actuals vs Predicted")
###Confusion Matrix (Logistic Regresssion) ####
caret::confusionMatrix(pred,test$loan_status,positive = "1")
## Concordance Ratio (Logistic Regression) ##
a = test$loan_status
b = prob
Concordance(actuals = a,predictedScores = b)
## Error Rate (Logistic Regression) ## 
conf.table = table(test$loan_status,pred)
errrate = (conf.table[2,1]+conf.table[1,2])/nrow(test)
print(errrate)
## ROC (Logistic Regresssion) ##
pred.obj = prediction(prob,test$loan_status)
perf = performance(pred.obj,"tpr","fpr")
plot(perf,main = "ROC Curve")
## KS VALUE (Logistic Regression) ##
print(max(perf@y.values[[1]] - perf@x.values[[1]]))
#### AUC (Logistic Regression) ####
auc = performance(pred.obj,"auc")
auc = as.numeric(auc@y.values)
print(auc)
#### GINI (Logistic Regression) ####
gini = ineq(prob,"gini")
print(gini)


### Naive Bayes ####  
naivebayes = e1071::naiveBayes(train$loan_status~.,data = train)
print(naivebayes)
### Making Predictions with Naive Bayes ####
naive.predict = predict(naivebayes,test,type = "class")
naive.prob = predict(naivebayes,test,type = "raw")
naive.prob = as.data.frame(naive.prob)
naive.prob = naive.prob[,2]
naive.predict = as.factor(naive.predict)
test$loan_status = as.factor(test$loan_status)
plot(test$loan_status,naive.predict,xlab = "Actuals",ylab = "Predicted",
     col = c("Red","Green"),main = "Actuals vs Predicted")
### Confusion Matrix (Naive Bayes) ####
caret::confusionMatrix(naive.predict,test$loan_status,positive = "1")
## Concordance Ratio (Naive Bayes) ##
a1 = test$loan_status
b1 = naive.prob
Concordance(actuals = a1,predictedScores = b1)
## Error Rate (Naive Bayes) ## 
conf.table1 = table(test$loan_status,naive.predict)
errrate1 = (conf.table1[2,1]+conf.table1[1,2])/nrow(test)
print(errrate1)
## ROC (Naive Bayes) ##
pred.obj1 = prediction(prob,test$loan_status)
perf1 = performance(pred.obj1,"tpr","fpr")
plot(perf1,main = "ROC Curve")
## KS VALUE (Naive Bayes) ##
print(max(perf1@y.values[[1]] - perf1@x.values[[1]]))
#### AUC (Naive Bayes) ####
auc1 = performance(pred.obj1,"auc")
auc1 = as.numeric(auc1@y.values)
print(auc1)
#### GINI (Naive Bayes) ####
gini1 = ineq(prob,"gini")
print(gini1)

### KNN ####
knn.train = train[,c(1,2,3,5,6,10,15:32,34)]
knn.test = test[,c(1,2,3,5,6,10,15:32,34)]
knn.cl = train$loan_status
knn.np = knn(train = knn.train,test = knn.test,cl = knn.cl,k = 486)
knn = knn(train = knn.train,test = knn.test,cl = knn.cl,k = 486,prob = TRUE)
### Making predictions with KNN ####
knn.prob = attributes(knn)$prob
knn.prob.df = data.frame(knn.prob,knn.np)
knn.prob.df$knn.train.prob[knn.np == "0"] = 1 - knn.prob.df$knn.prob[knn.np == "0"]
knn.train.predict = knn.prob.df$knn.train.prob
knn.train.response = knn.prob.df$knn.train.c
plot(test$loan_status,knn.np,xlab = "Actuals",ylab = "Predicted",
     col = c("Red","Blue"),main = "Actuals vs Predicted")
### Confusion Matrix (KNN) ####
caret::confusionMatrix(knn.np,test$loan_status,positive = "1")
## Concordance Ratio (KNN) ##
a2 = test$loan_status
b2 = knn.train.predict
Concordance(actuals = a2,predictedScores = b2)
## Error Rate (KNN) ## 
conf.table2 = table(test$loan_status,knn.np)
errrate2 = (conf.table2[2,1]+conf.table2[1,2])/nrow(test)
print(errrate1)
## ROC (KNN) ##
pred.obj2 = prediction(knn.train.predict,test$loan_status)
perf2 = performance(pred.obj2,"tpr","fpr")
plot(perf2,main = "ROC Curve")
## KS VALUE (KNN) ##
print(max(perf2@y.values[[1]] - perf2@x.values[[1]]))
#### AUC (KNN) ####
auc2 = performance(pred.obj2,"auc")
auc2 = as.numeric(auc2@y.values)
print(auc2)
#### GINI (KNN) ####
gini2 = ineq(rf.prob,"gini")
print(gini2)

### CART Model ####
rtree = rpart(loan_status~.,data = train,minsplit = 1000,minbucket = 1000)
print(rtree)
rpart.plot(rtree)
### Making Predictions with CART Model ####
rtree.predict = predict(rtree,test,type = "class")
rtree.prob= predict(rtree,test,type = "prob")[,1]
rtree.predict = as.factor(rtree.predict)
plot(test$loan_status,rtree.predict,xlab = "Actuals",ylab = "Predicted",
     col = c("Orange","Purple"),main = "Actuals vs Predicted")
### Confusion Matrix (CART Model) ####
caret::confusionMatrix(rtree.predict,test$loan_status,positive = "1")
## Concordance Ratio (CART Model) ##
a3 = test$loan_status
b3 = 1-rtree.prob
Concordance(actuals = a3,predictedScores = b3)
## Error Rate (CART Model) ## 
conf.table3 = table(test$loan_status,rtree.predict)
errrate3 = (conf.table2[2,1]+conf.table2[1,2])/nrow(test)
print(errrate3)
## ROC (CART Model) ##
pred.obj3 = prediction(1-rtree.prob,test$loan_status)
perf3 = performance(pred.obj3,"tpr","fpr")
plot(perf3)
## KS VALUE (CART Model) ##
print(max(perf3@y.values[[1]] - perf3@x.values[[1]]))
#### AUC (CART Model) ####
auc3 = performance(pred.obj3,"auc")
auc3 = as.numeric(auc3@y.values)
print(auc3)
#### GINI (CART Model) ####
gini3 = ineq(rtree.prob,"gini")
print(gini3)

### Random Forest ####
set.seed(77)
rf = randomForest(loan_status~.,data = train,
                  ntree =300,mtry = 3,nodesize = 10,importance = TRUE,do.trace = TRUE)
plot(rf,main = "Random Forest (OOB error vs. Number of trees)")
print(rf)
rf$importanceSD
### Making predictions with Random Forest ####
rf.pred = predict(rf,test,type = "class")
rf.prob = predict(rf,test,type = "prob")
rf.prob = as.data.frame(rf.prob)
rf.prob = rf.prob[,2]
plot(test$loan_status,rf.pred,xlab = "Actuals",ylab = "Predicted",
     col = c("Orange","Blue"),main = "Actuals vs Predicted")
### Confusion Matrix (Random Forest) ####
caret::confusionMatrix(rf.pred,test$loan_status,positive = "1")
## Concordance Ratio (Random Forest) ##
a4 = test$loan_status
b4 = rf.prob
Concordance(actuals = a4,predictedScores = b4)
## Error Rate (Random Forest) ## 
conf.table4 = table(test$loan_status,rf.pred)
errrate4 = (conf.table4[2,1]+conf.table4[1,2])/nrow(test)
print(errrate4)
## ROC (Random Forest) ###
pred.obj4 = prediction(rf.prob,test$loan_status)
perf4 = performance(pred.obj4,"tpr","fpr")
plot(perf4)
## KS VALUE (Random Forest) ###
print(max(perf4@y.values[[1]] - perf4@x.values[[1]]))
#### AUC (Random Forest) ####
auc4 = performance(pred.obj4,"auc")
auc4 = as.numeric(auc4@y.values)
print(auc4)
#### GINI (Random Forest) ####
gini4 = ineq(rf.prob,"gini")
print(gini4)

### Bagging ####
set.seed(77)
bag.model = bagging(loan_status~.,data = train,coob = TRUE,
                    control = rpart.control(maxdepth = 10,minsplit = 3))
summary(bag.model)
## Making predictions with Bagging model ####
set.seed(77)
bag.pred = predict(bag.model,test,type = "class")
bag.prob = predict(bag.model,test,type = "prob")
bag.prob = bag.prob[,2]
bag.pred = as.factor(bag.pred)
plot(test$loan_status,gbm.pred,xlab = "Actuals",ylab = "Predicted",
     col = c("Green","Blue"),main = "Actuals vs Predicted")
## Confusion Matrix (Bagging Model)
caret::confusionMatrix(test$loan_status,bag.pred,positive = "1")
## Concordance Ratio (Bagging Model) ##
a5 = test$loan_status
b5 = bag.prob
Concordance(actuals = a5,predictedScores = b5)
## Error Rate (Bagging Model) ## 
conf.table5 = table(test$loan_status,bag.pred)
errrate5 = (conf.table5[2,1]+conf.table5[1,2])/nrow(test)
print(errrate5)
## ROC (Bagging Model) ##
pred.obj5 = prediction(bag.prob,test$loan_status)
perf5 = performance(pred.obj5,"tpr","fpr")
plot(perf5)
## KS VALUE (Bagging Model) ##
print(max(perf5@y.values[[1]] - perf5@x.values[[1]]))
#### AUC (Bagging Model) ####
auc5 = performance(pred.obj5,"auc")
auc5 = as.numeric(auc5@y.values)
print(auc5)
#### GINI (Bagging Model) ####
gini5 = ineq(bag.prob,"gini")
print(gini5)
### Adaptive Boosting ####
gbm <- gbm(
   formula = loan_status~.,
   data = train[,c(1,2,3,5,6,10,15,17,19:22,25:28,32,34)],
   distribution = "multinomial",
   n.trees = 500,
   interaction.depth = 1,
   shrinkage = 0.1,
   cv.folds = 0,
   n.cores = 2, # will use all cores by default
   verbose = FALSE
)
summary(gbm)
### Making predictions with Adaptive Boosting ####
gbm.prob = predict(gbm,test,type = "response",n.trees = 500)
gbm.prob = as.data.frame(gbm.prob)
gbmplot = qplot(gbm.prob[,c(2)],test$loan_status,geom = "boxplot")
gbmplot+geom_vline(xintercept = 0.61)
gbm.prob= gbm.prob[,c(2)]
gbm.pred = ifelse(gbm.prob > 0.61,"1","0")
gbm.pred = as.factor(gbm.pred)
plot(test$loan_status,gbm.pred,xlab = "Actuals",ylab = "Predicted",
     col = c("Pink","Violet"),main = "Actuals vs Predicted")
### Confusion Matrix (AdaBoost) ####
caret::confusionMatrix(gbm.pred,test$loan_status,positive = "1")
## Concordance Ratio (AdaBoost) ##
a6 = test$loan_status
b6 = gbm.prob
Concordance(actuals = a6,predictedScores = b6)
## Error Rate (AdaBoost) ## 
conf.table6 = table(test$loan_status,gbm.pred)
errrate6 = (conf.table6[2,1]+conf.table6[1,2])/nrow(test)
print(errrate6)
## ROC (AdaBoost) ##
pred.obj6 = prediction(gbm.prob,test$loan_status)
perf6 = performance(pred.obj6,"tpr","fpr")
plot(perf6)
## KS VALUE (AdaBoost) ##
print(max(perf6@y.values[[1]] - perf6@x.values[[1]]))
#### AUC (AdaBoost) ####
auc6 = performance(pred.obj6,"auc")
auc6 = as.numeric(auc6@y.values)
print(auc6)
#### GINI (AdaBoost) ####
gini6 = ineq(gbm.prob,"gini")
print(gini6)

### XG Boost ####
set.seed(77)
m = model.matrix(train$loan_status~.,train)
n = train[,34]
n = as.matrix(n)
n = as.character(n)
n = as.numeric(n)
xg.train = xgb.DMatrix(data = m,label = n)
t = model.matrix(test$loan_status~.,test)
i = test[,34]
i = as.matrix(i)
i = as.character(i)
i = as.numeric(i)
xg.test = xgb.DMatrix(data = t,label = i)
set.seed(77)
xgb <- xgboost(
   data = xg.train,
   eta = 0.05,
   max_depth = 4,
   nrounds = 200,
   nfold = 0,
   objective = "binary:logistic",  # for regression models
   verbose = 1,               # silent,
   early_stopping_rounds = 50 # stop if no improvement for 10 consecutive trees
)
print(xgb)
### Making Predictions with XG Boost ####
xgb.prob = predict(xgb,xg.test)
bp = qplot(xgb.prob,test$loan_status,geom = "boxplot"
           ,xlab = "Probabilities",ylab = "Loan Status")
bp+geom_vline(xintercept = 0.49,color = "Red",size = 1)
xgb.pred = ifelse(xgb.prob > 0.49,"1","0")
xgb.pred = as.factor(xgb.pred)
test$loan_status = as.factor(test$loan_status)
plot(test$loan_status,xgb.pred,xlab = "Actuals",ylab = "Predicted",
     col = c("Brown","Yellow"),main = "Actuals vs Predicted")
### Confusion Matrix (XG Boost) ####
caret::confusionMatrix(xgb.pred,test$loan_status,positive = "1")
## Concordance Ratio (XG boost) ##
a7 = test$loan_status
b7 = xgb.prob
Concordance(actuals = a7,predictedScores = b7)
## Error Rate (XG boost) ## 
conf.table7 = table(test$loan_status,xgb.pred)
errrate7 = (conf.table7[2,1]+conf.table7[1,2])/nrow(test)
print(errrate7)
## ROC (XG Boost) ##
pred.obj7 = prediction(xgb.prob,test$loan_status)
perf7 = performance(pred.obj7,"tpr","fpr")
plot(perf7)
## KS VALUE (XG boost) ##
print(max(perf7@y.values[[1]] - perf7@x.values[[1]]))
#### AUC (XG boost) ####
auc7 = performance(pred.obj7,"auc")
auc7 = as.numeric(auc7@y.values)
print(auc7)
#### GINI (XG boost) ####
gini7 = ineq(xgb.prob,"gini")
print(gini7)



### Ensembling Methods (Weighted Average) ####
wap = (rf.prob*0.25)+(prob*0.25)+(xgb.prob*0.50)
qplot(test$loan_status,wap,geom = "boxplot")
wa = ifelse(wap >0.45,"1","0")
wa = as.factor(wa)
### Confusion Matrix (Weighted Average) ####
caret::confusionMatrix(wa,test$loan_status,positive = "1")
## Concordance Ratio (Weighted Average) ##
a8 = test$loan_status
b8 = wap
Concordance(actuals = a8,predictedScores = b8)
## Error Rate (Weighted Average) ## 
conf.table8 = table(test$loan_status,wa)
errrate8 = (conf.table8[2,1]+conf.table8[1,2])/nrow(test)
print(errrate8)
## ROC (Weighted Average) ##
pred.obj8 = prediction(wap,test$loan_status)
perf8 = performance(pred.obj8,"tpr","fpr")
plot(perf8)
## KS VALUE (Weighted Average) ##
print(max(perf8@y.values[[1]] - perf8@x.values[[1]]))
#### AUC (Weighted Average) ####
auc8 = performance(pred.obj8,"auc")
auc8 = as.numeric(auc8@y.values)
print(auc8)
#### GINI (Weighted Average) ####
gini8 = ineq(wap,"gini")
print(gini8)

### Model Tuning ####
xgb.tuned <- xgboost(
   data = xg.train,
   eta = 0.005,
   max_depth = 5,
   nrounds = 1000,
   nfold = 2,
   objective = "binary:logistic",  # for regression models
   verbose = 1,               # silent,
   early_stopping_rounds = 50 # stop if no improvement for 10 consecutive trees
)
print(xgb.tuned)
### Making Predictions with tuned xg boost ####
xgb.tuned.prob = predict(xgb.tuned,xg.test)
bp = qplot(xgb.tuned.prob,test$loan_status,geom = "boxplot"
           ,xlab = "Probabilities",ylab = "Loan Status",xintercept = 0.63)
bp+geom_vline(xintercept = 0.42,color = "Red",size = 1)
xgb.tuned.pred = ifelse(xgb.tuned.prob > 0.42,"1","0")
xgb.tuned.pred = as.factor(xgb.tuned.pred)
test$loan_status = as.factor(test$loan_status)
### Confusion Matrix (Tuned XG Boost) ####
caret::confusionMatrix(xgb.tuned.pred,test$loan_status,positive = "1")


### Model Comparision Graphs ####
## Specificity ####
log.spec = specificity(test$loan_status,prob,threshold = 0.55)
nb.spec = specificity(test$loan_status,naive.prob,threshold = 0.50)
knn.spec = specificity(test$loan_status,knn.train.predict,threshold = 0.50)
cart.spec = 1-specificity(test$loan_status,rtree.prob,threshold = 0.50)
rf.spec = specificity(test$loan_status,rf.prob,threshold = 0.50)
bag.spec = specificity(test$loan_status,bag.prob,threshold = 0.50)
adb.spec = specificity(test$loan_status,gbm.prob,threshold = 0.61)
xgb.spec = specificity(test$loan_status,xgb.prob,threshold = 0.49)
## Sensitivity ####
log.sens = sensitivity(test$loan_status,prob,threshold = 0.55)
nb.sens = sensitivity(test$loan_status,naive.prob,threshold = 0.50)
knn.sens = 1-sensitivity(test$loan_status,knn.train.predict,threshold = 0.50)
cart.sens = 1-sensitivity(test$loan_status,rtree.prob,threshold = 0.50)
rf.sens = sensitivity(test$loan_status,rf.prob,threshold = 0.50)
bag.sens = sensitivity(test$loan_status,bag.prob,threshold = 0.50)
adb.sens = sensitivity(test$loan_status,gbm.prob,threshold = 0.61)
xgb.sens = sensitivity(test$loan_status,xgb.prob,threshold = 0.49)
## Accuracy ####
log.acc = (conf.table[1,1]+conf.table[2,2])/nrow(test)
nb.acc = (conf.table1[1,1]+conf.table1[2,2])/nrow(test)
knn.acc = (conf.table2[1,1]+conf.table2[2,2])/nrow(test)
cart.acc = (conf.table3[1,1]+conf.table3[2,2])/nrow(test)
rf.acc = (conf.table4[1,1]+conf.table4[2,2])/nrow(test)
bag.acc = (conf.table5[1,1]+conf.table5[2,2])/nrow(test)
adb.acc = (conf.table6[1,1]+conf.table6[2,2])/nrow(test)
xgb.acc = (conf.table7[1,1]+conf.table7[2,2])/nrow(test)
### Specificity Plot ####
spec1 = c("Logistic Regression","Naive Bayes","KNN","CART","RF","Bagging","Adaboost","XGboost")
spec2 = c(log.spec,nb.spec,knn.spec,cart.spec,rf.spec,bag.spec,adb.spec,xgb.spec)
spec1 = as.factor(spec1)
plot(spec1,spec2,xlab = "Model",ylab = "Specificity",main = "Specificity Comparison")
### Sensitivity Plot ####
sens1 = c("Logistic Regression","Naive Bayes","KNN","CART","RF","Bagging","Adaboost","XGboost")
sens2 = c(log.sens,nb.sens,knn.sens,cart.sens,rf.sens,bag.sens,adb.sens,xgb.sens)
sens1 = as.factor(sens1)
plot(sens1,sens2,xlab = "Model",ylab = "Sensitivity",main = "Sensitivity Comparision")
### Accuracy Plot ####
acc1 = c("Logistic Regression","Naive Bayes","KNN","CART","RF","Bagging","Adaboost","XGboost")
acc2 = c(log.acc,nb.acc,knn.acc,cart.acc,rf.acc,bag.acc,adb.acc,xgb.acc)
acc1 = as.factor(acc1)
plot(acc1,acc2,xlab = "Model",ylab = "Accuracy",main = "Accuracy Comparison")
