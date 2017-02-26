# clear the environment variable
rm(list=ls(all=TRUE))

#set the  directory
setwd("~/Desktop/GNQ3 Exam/20170226_Batch25_Datasets_GNQ03")

# Load the required libraries
library(DMwR)
library(randomForest)


# Read the data into R
data = read.csv('UCI_Credit_Card.csv')

# Understand the data 
str(data)
summary(data)

#Find the number of negative number in each column
library(plyr)
apply(data,2,function(x){with(data,count(sign(x)))})

# There are negative values for bill payment assuming it not coverting it 

#Find the class of target variable
table(data$default.payment.next.month)
str(data$default.payment.next.month) # 1: Default Payment; 0: Not Default Payment 

#Find number of unique element in each attribute
apply(data,2,function(x){length(unique(x))})

#Ignore the ID column from data
data <- data[,-1]

#cross check
str(data)

# The categorical variables are: SEX,default.payment.next.month,EDUCATION,MARRIAGE
# The numerical variables are: the remaining 22 variables

cat_Attr = c("SEX", "default.payment.next.month","EDUCATION","MARRIAGE")
num_Attr = setdiff(names(data), cat_Attr)

# Seperate numerical and categorical variables and convert them into appropriate type
data = data.frame(sapply(data,as.character))
cat_Data = data.frame(sapply(data[,cat_Attr], as.factor))
num_Data = data.frame(sapply(data[,num_Attr], as.numeric))
data = cbind(num_Data, cat_Data)

#remove unwanted variables
rm(num_Attr, cat_Attr)
rm(cat_Data, num_Data)

#recheck the data 
str(data)

# Handle missing values using knn imputation
sum(is.na(data))
# No NA is there





# Build the classification model using randomForest to find the important attributes
model = randomForest(default.payment.next.month ~ ., data=data, 
                     keep.forest=TRUE, ntree=70) 

# Print and understand the model
print(model)

# Important attributes

model$importance  
round(importance(model), 2)   

# Extract and store important variables obtained from the random forest model
rf_Imp_Attr = data.frame(model$importance)
rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]

# plot (directly prints the important attributes) 
varImpPlot(model)

##In the plot the 16 variables are having variance very close after 16th the variance have much difference 
# So choosing 16 most important variable
#
#Attributes   Importance
#3       PAY_0 841.35757761
#9   BILL_AMT1 599.35474426
#10  BILL_AMT2 571.46475654
#2         AGE 568.52433123
#11  BILL_AMT3 550.87068614
#15   PAY_AMT1 546.33637202
#14  BILL_AMT6 542.46918553
#12  BILL_AMT4 538.12388619
#13  BILL_AMT5 532.16791571
#1   LIMIT_BAL 506.33543100
#16   PAY_AMT2 495.19364490
#17   PAY_AMT3 475.43895632
#20   PAY_AMT6 464.93153766
#19   PAY_AMT5 457.23676871
#18   PAY_AMT4 450.04755222
#4       PAY_2 444.01095172

# Build randorm forest using top 16 important attributes. 
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:16])

# Build the classification model using randomForest
model_Imp = randomForest(default.payment.next.month ~ ., data=data[,c(top_Imp_Attr,"default.payment.next.month")],keep.forest=TRUE, ntree=70) 

# Print and understand the model
print(model_Imp)

# Important attributes
model_Imp$importance  

# Split dataset into train and test
set.seed(123)
train_RowIDs = sample(1:nrow(data), nrow(data)*0.7)
train_Data = data[train_RowIDs,]
test_Data = data[-train_RowIDs,]
rm(train_RowIDs)


# Check how records are split with respect to target attribute.
table(data$default.payment.next.month)
table(train_Data$default.payment.next.month)
table(test_Data$default.payment.next.month)
rm(data)

# Build the classification model using rpart

# Classification Trees using CART 
#Modeling the decision tree using rpart
dtCart=rpart(default.payment.next.month~., data=train_Data[,c(top_Imp_Attr,"default.payment.next.month")], method="class")    
plot(dtCart,main="Classification Tree for loan Class",margin=0.15,uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)

# Predict on Train data 
pred_Train = predict(dtCart, train_Data[,setdiff(names(train_Data),"default.payment.next.month")],
                     type="class")

cart_Train = pred_Train
# Build confusion matrix and find accuracy   
cm_Train = table("actual"= train_Data$default.payment.next.month, "predicted" = pred_Train);
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
library(caret)
confusionMatrix(cm_Train)
accuracy = sum(diag(cm_Train))/sum(cm_Train)
precision = cm_Train[2,2]/sum(cm_Train[,2])
recall = cm_Train[2,2]/sum(cm_Train[2,])
rm(pred_Train, cm_Train)

# The Accuracy for train 81.9
# The Precision for train 0.8944286 
# The Recall for train 0.8332002

# Predicton Test Data
pred_Test = predict(dtCart, test_Data[,setdiff(names(test_Data),"default.payment.next.month")],
                    type="class")

# Build confusion matrix and find accuracy   
cm_Test = table("actual"= test_Data$default.payment.next.month, "predicted" = pred_Test);
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
accuracy = sum(diag(cm_Test))/sum(cm_Test)
precision = cm_Test[2,2]/sum(cm_Test[,2])
recall = cm_Test[2,2]/sum(cm_Test[2,])
confusionMatrix(cm_Test)


# The Accuracy for tesst 82.02
# The Precision for test 0.8984286 
# The Recall for test 0.8362002

rm(pred_Test, cm_Test)

accu_Train  #81.9
accu_Test   #82.0

##Building model using C50
#Loding the library for C50
library(C50)
#Model for decision trees using c50
dtC50 = C5.0(default.payment.next.month ~ ., data=train_Data[,c(top_Imp_Attr,"default.payment.next.month")], rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

# Predict on Train data 
pred_Train = predict(dtC50, train_Data[,setdiff(names(train_Data),"default.payment.next.month")],
                     type="class")
c5_Train = pred_Train

# Build confusion matrix and find accuracy   
cm_Train = table("actual"= train_Data$default.payment.next.month, "predicted" = pred_Train);
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
library(caret)
confusionMatrix(cm_Train)
accuracy = sum(diag(cm_Train))/sum(cm_Train)
precision = cm_Train[2,2]/sum(cm_Train[,2])
recall = cm_Train[2,2]/sum(cm_Train[2,])

rm(pred_Train, cm_Train)

# The Accuracy for train 82.2
# The Precision for train 0.8744286 
# The Recall for train 0.8442002


# Predicton Test Data
pred_Test = predict(dtC50, test_Data[,setdiff(names(test_Data),"default.payment.next.month")],
                    type="class")

# Build confusion matrix and find accuracy   
cm_Test = table("actual"= test_Data$default.payment.next.month, "predicted" = pred_Test);
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
confusionMatrix(cm_Test)

accuracy = sum(diag(cm_Test))/sum(cm_Test)
precision = cm_Test[2,2]/sum(cm_Test[,2])
recall = cm_Test[2,2]/sum(cm_Test[2,])

# The Accuracy for tesst 82.06
# The Precision for test 0.897886 
# The Recall for test 0.84412

rm(pred_Test, cm_Test)

accu_Train  #82.53
accu_Test   #82.06



#Building Classification using SVM

#Converting all the data to numeric fot TRAIN_DATA
# SVM need all the data to be in numeric so converting it
# categorical attribute
cat_Attr = c("SEX","EDUCATION","MARRIAGE","default.payment.next.month")
#saving target variable in a dummy variable
target <-  train_Data[,"default.payment.next.month"]
#independent numeric variable
ind_Num_Attr = setdiff(names(train_Data), cat_Attr)
#Standardizing the  data
Num_Data = decostand(train_Data[,ind_Num_Attr], "range") 
#loding the dummy package
library(dummies)
#converting the factor to dummy variable for traindata
sex = data.frame(dummy(train_Data$SEX))
edu = data.frame(dummy(train_Data$EDUCATION))
marriage = data.frame(dummy(train_Data$MARRIAGE))

#Column bind on numeric and dummy data
cla_Data = cbind(Num_Data, edu, marriage,sex)
#Cross checking the data
str(cla_Data)
#Appending the target data
cla_Data[,"default.payment.next.month"]=target
#Assiging it to train data
train_Data = cla_Data

#Converting all the data to numeric fot TEST_DATA
#categorical data
cat_Attr = c("SEX","EDUCATION","MARRIAGE","default.payment.next.month")
#storing target data in dummy variable
target <-  test_Data[,"default.payment.next.month"]
#Independent variables
ind_Num_Attr = setdiff(names(test_Data), cat_Attr)
#Standardizing the  data
Num_Data = decostand(test_Data[,ind_Num_Attr], "range") 
#Dummy the factor data
sex = data.frame(dummy(test_Data$SEX))
edu = data.frame(dummy(test_Data$EDUCATION))
marriage = data.frame(dummy(test_Data$MARRIAGE))
cla_Data = cbind(Num_Data, edu, marriage,sex)
#Appending the target data
cla_Data[,"default.payment.next.month"]=target
#Assining to the test_data
test_Data = cla_Data

#running the SVM Model
model = svm(x = train_Data[,c(top_Imp_Attr)], 
            y = train_Data$default.payment.next.month, 
            type = "C-classification", 
            kernel = "linear", cost = 10, gamma = 0.1) 

# Look at the model summary
summary(model)

model$index


# Predict on train data  
pred_Train  =  predict(model, train_Data[,c(top_Imp_Attr)])  
svm_Train = pred_Train
# Build confusion matrix and find accuracy   
cm_Train = table(train_Data$default.payment.next.month, pred_Train)
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
accuracy = sum(diag(cm_Train))/sum(cm_Train)
precision = cm_Train[2,2]/sum(cm_Train[,2])
recall = cm_Train[2,2]/sum(cm_Train[2,])
rm(pred_Train, cm_Train)

# Predict on test data
pred_Test = predict(model, test_Data[,c(top_Imp_Attr)])  

# Build confusion matrix and find accuracy   
cm_Test = table(test_Data$default.payment.next.month, pred_Test)
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
accuracy = sum(diag(cm_Test))/sum(cm_Test)
precision = cm_Test[2,2]/sum(cm_Test[,2])
recall = cm_Test[2,2]/sum(cm_Test[2,])

rm(pred_Test, cm_Test)


accu_Train
accu_Test


rm(model, accu_Test, accu_Train, ind_Attr, train_Data, test_Data)


# Building NeuralNet using nnet
# Load required librarie(s)
library(nnet)
# Build the NN model using nnet 
nnModel = nnet(train_Data$default.payment.next.month ~ ., data = train_Data[,c(top_Imp_Attr)], size = 3)

nnModel$n
nnModel$wts
nnModel$fitted.values
# Check the model performance on train data by generating confusion matrix
nnet_train =  ifelse(nnModel$fitted.values > 0.05, 1, 0)
train_nnet = table(train_Data$default.payment.next.month, ifelse(nnModel$fitted.values > 0.05, 1, 0))

accuracy = sum(diag(train_nnet))/sum(train_nnet)
precision = train_nnet[2,2]/sum(train_nnet[,2])
recall = train_nnet[2,2]/sum(train_nnet[2,])

pred = predict(nnModel, test_Data[,c(top_Imp_Attr)])

# Check model performance on test data by generating confusion matrix
test_nnet = table(test_Data$default.payment.next.month, ifelse(pred > 0.05, 1, 0))
accuracy = sum(diag(test_nnet))/sum(test_nnet)
precision = test_nnet[2,2]/sum(test_nnet[,2])
recall = test_nnet[2,2]/sum(test_nnet[2,])



#THE BEST FIT MODEL
#The best fit model which predicts good recall and precision is NEURAL NETWORK.

# the  Most important factor to consider according to the bussiness scenario 
My model should predict correct defaulter without missing
If i predict a customer as a defaulter and really he is not a defaulter that is not a big issue.
But i cant do wrong on predicting defaulter 
So True Positive is very important metric.

RECALL = HOW MANY RELEVANT ITEMS SELECTED?
PRECISION = HOW MANY SELECTED ITEM ARE RELEVANT?

RECALL is important metrics

##STACKING the different models

# Combining test predictions of CART, C5.0,svm,nnet & Log Regression together 
test_Pred_All_Models = data.frame(CART=cart_Train,SVM = svm_Train,C50 = c5_Train, NNET =nnet_train) 

test_Pred_All_Models = data.frame(sapply(test_Pred_All_Models, as.factor))
str(test_Pred_All_Models)
head(test_Pred_All_Models)

# Check the "glm_ensemble model" on the test data
ensemble_Test = predict(ensemble_Model, test_Pred_All_Models, type = "response")
ensemble_Test = ifelse(ensemble_Test > 0.5, 1, 0)
table(ensemble_Test)

cm_Ensemble = table(ensemble_Test, test_Data$default.payment.next.month)
sum(diag(cm_Ensemble))/sum(cm_Ensemble)
accuracy = sum(diag(cm_Ensemble))/sum(cm_Ensemble)
precision = cm_Ensemble[2,2]/sum(cm_Ensemble[,2])
recall = cm_Ensemble[2,2]/sum(cm_Ensemble[2,])
























