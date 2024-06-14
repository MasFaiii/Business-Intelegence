#DIARY BI - SVM


#===================================LOGISTIC-REGRESSION=====================================



df <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(df)

sum(is.na(df))

summary(df)

xtabs(~ admit +rank ,data=df)

df$rank <- as.factor(df$rank)
logit <- glm(admit ~ gre+gpa+rank,data=df,family="binomial")
summary(logit)

x <- data.frame(gre=790,gpa=3.8,rank=as.factor(1))
p<- predict(logit,x)
p

#install.packages("caTools")
par(mfrow=c(1,1))
library(caTools)

sim = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    
    dataset = df
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    # Fitting Logistic Regression to the Training set
    glm.r= glm(formula = admit ~ ., data = trainingset, family = "binomial")
    
    # Predicting the Test set results
    ypred = predict(glm.r, newdata = testset)
    ypred = round(1/(1+exp(-ypred)))
    cm = table(ypred,testset[,1])
    accuracy = sum(diag(cm))/sum(cm)
    hasil[i] = accuracy
  }
  hasil
}

B = 5000

hasil = sim(B, 0.6)
hist(hasil)
summary(hasil)

shapiro.test(hasil)

sim1 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    dataset = df
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Logistic Regression to the Training set
    glm.r= glm(formula = admit ~ gre + gpa, data = trainingset, family = "binomial")
    # Predicting the Test set results
    ypred = predict(glm.r, newdata = testset)
    ypred = round(1/(1+exp(-ypred)))
    cm = table(ypred,testset[,1])
    accuracy = sum(diag(cm))/sum(cm)
    hasil[i] = accuracy
  }
  hasil
}

B = 5000
hasil1 = sim1(B, 0.6)
hist(hasil1)
summary(hasil1)

wilcox.test(hasil, hasil1, alternative = "two.sided") 

sim2 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    dataset = df
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Logistic Regression to the Training set
    glm.r= glm(formula = admit ~ gre , data = trainingset, family = "binomial")
    # Predicting the Test set results
    ypred = predict(glm.r, newdata = testset)
    ypred = round(1/(1+exp(-ypred)))
    cm = table(ypred,testset[,1])
    accuracy = sum(diag(cm))/sum(cm)
    hasil[i] = accuracy
  }
  hasil
}

B = 5000
hasil2 = sim2(B, 0.6)
hist(hasil2)
summary(hasil2)

wilcox.test(hasil2, hasil1, alternative = "two.sided")

#=========================STUDI KASUS : LOAN========================= 


data = read.table('LOAN.txt',head=TRUE)
dim(data)

has = glm(PersonalLoan ~ ., data=data, family="binomial")
summary(has)

has1 = glm(PersonalLoan ~ Income + Family + CCAvg + Education + SecuritiesAccount + CDAccount + Online + CreditCard, data=data, family="binomial")
summary(has1)

#install.packages("caTools")

par(mfrow=c(1,1))

library(caTools)

sim = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    dataset = data
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    # Fitting Logistic Regression to the Training set
    glm.r= glm(formula = PersonalLoan ~ ., data = trainingset, family="binomial")
    # Predicting the Test set results
    ypred = predict(glm.r, newdata = testset)
    ypred = round(1/(1+exp(-ypred)))
    cm = table(ypred,testset[,1])
    accuracy = sum(diag(cm))/sum(cm)
    hasil[i] = accuracy
  }
  hasil
}

B = 5000

hasil = sim(B, 0.6)
hist(hasil)
summary(hasil)
shapiro.test(hasil)

sim1 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    
    dataset = data
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Logistic Regression to the Training set
    
    glm.r= glm(formula = PersonalLoan ~ Income + Family + CCAvg + Education + SecuritiesAccount + CDAccount + Online + CreditCard, data = trainingset, family="binomial")
    
    # Predicting the Test set results
    ypred = predict(glm.r, newdata = testset)
    ypred = round(1/(1+exp(-ypred)))
    cm = table(ypred,testset[,1])
    accuracy = sum(diag(cm))/sum(cm)
    hasil[i] = accuracy
  }
  hasil
}

B = 1000

hasil1 = sim1(B, 0.6)
hist(hasil1)
summary(hasil1)

shapiro.test(hasil)
shapiro.test(hasil1)

wilcox.test(hasil2, hasil1, alternative = "two.sided")

#====================================STUDI KASUS : BOSTON HOUSING===============

#Load Library
library(e1071)
library(hydroGOF)

BH <- read.csv(file = "HousingData.csv",
               stringsAsFactors = FALSE)

dim(BH)
#[1] 506  14

u = BH[,14]
median(u)
#[1] 21.2

w = which(u>30)#30K

BH[,15] = 0
BH[w,15] = 1

BH = na.omit(BH)

dataset = BH[-14]


# Encoding the target feature as factor 
dataset$V15 = factor(dataset$V15, levels = c(0, 1)) 

# Splitting the dataset into the Training set and Test set 
#install.packages('caTools') 
library(caTools) 

set.seed(123) 
split = sample.split(dataset$V15, SplitRatio = 0.75) 

training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE) 
# Feature Scaling 
training_set[-14] = scale(training_set[-14]) 
test_set[-14] = scale(test_set[-14]) 

# Fitting SVM to the Training set 
#install.packages('e1071') 
library(e1071) 

classifier = glm(formula = V15 ~ ., data=training_set,
                 family="binomial") 

# Predicting the Test set results 
y_pred = predict(classifier, newdata = test_set[-14]) 
y_pred = round(1/(1+exp(-y_pred)))
# Making the Confusion Matrix 
cm = table(test_set[, 14], y_pred) 
cm
acc = sum(diag(cm))/sum(cm)
acc

sim2 = function(B, p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    dataset = BH[-14]
    
    # Encoding the target feature as factor 
    dataset$V15 = factor(dataset$V15, levels = c(0, 1)) 
    
    # Splitting the dataset into the Training set and Test set 
    #install.packages('caTools') 
    library(caTools) 
    
    #set.seed(123) 
    split = sample.split(dataset$V15, SplitRatio = 0.75) 
    
    training_set = subset(dataset, split == TRUE) 
    test_set = subset(dataset, split == FALSE) 
    # Feature Scaling 
    training_set[-14] = scale(training_set[-14]) 
    test_set[-14] = scale(test_set[-14]) 
    
    # Fitting SVM to the Training set 
    #install.packages('e1071') 
    library(e1071) 
    
    classifier = glm(formula = V15 ~ ., data=training_set,
                     family="binomial") 
    
    # Predicting the Test set results 
    y_pred = predict(classifier, newdata = test_set[-14]) 
    y_pred = round(1/(1+exp(-y_pred)))
    # Making the Confusion Matrix 
    cm = table(test_set[, 14], y_pred) 
    #cm
    acc = sum(diag(cm))/sum(cm)
    hasil[i] = acc
    
  }
  hasil
}

hasil2 = sim2(1000,0.8)
summary(hasil2)
hist(hasil2)
shapiro.test(hasil2)

hasil3 = sim2(1000,0.6)
summary(hasil3)
hist(hasil3)
shapiro.test(hasil3)


#====================================SVM class================================

#Load Library
library(e1071)
library(hydroGOF)

BH <- read.csv(file = "HousingData.csv",
               stringsAsFactors = FALSE)

dim(BH)
#[1] 506  14

u = BH[,14]
median(u)
#[1] 21.2

w = which(u>30)
BH[,15] = 0
BH[w,15] = 1

BH = na.omit(BH)

dataset = BH[-14]

# Encoding the target feature as factor 
dataset$V15 = factor(dataset$V15, levels = c(0, 1)) 

# Splitting the dataset into the Training set and Test set 
#install.packages('caTools') 
library(caTools) 

#set.seed(123) 
split = sample.split(dataset$V15, SplitRatio = 0.75) 

training_set = subset(dataset, split == TRUE) 
test_set = subset(dataset, split == FALSE) 
# Feature Scaling 
training_set[-14] = scale(training_set[-14]) 
test_set[-14] = scale(test_set[-14]) 

# Fitting SVM to the Training set 
#install.packages('e1071') 
library(e1071) 

classifier = svm(formula = V15 ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear') 

# Predicting the Test set results 
y_pred = predict(classifier, newdata = test_set[-14]) 

# Making the Confusion Matrix 
cm = table(test_set[, 14], y_pred) 
cm
acc = sum(diag(cm))/sum(cm)
acc

sim1 = function(B, p)
{
  BH <- read.csv(file = "HousingData.csv",
                 stringsAsFactors = FALSE)
  
  dim(BH)
  #[1] 506  15
  
  u = BH[,14]
  #median(u)
  #[1] 21.2
  
  w = which(u>median(u))
  
  BH[,15] = 0
  BH[w,15] = 1
  
  BH = na.omit(BH)
  
  dataset = BH[-14]
  
  # Encoding the target feature as factor 
  dataset$V15 = factor(dataset$V15, levels = c(0, 1)) 
  
  # Splitting the dataset into the Training set and Test set 
  #install.packages('caTools') 
  library(caTools) 
  
  hasil = numeric(B)
  for (i in 1:B)
  {
    
    #set.seed(123) 
    split = sample.split(dataset$V15, SplitRatio = p) 
    
    training_set = subset(dataset, split == TRUE) 
    test_set = subset(dataset, split == FALSE) 
    # Feature Scaling 
    training_set[-14] = scale(training_set[-14]) 
    test_set[-14] = scale(test_set[-14]) 
    
    # Fitting SVM to the Training set 
    #install.packages('e1071') 
    library(e1071) 
    
    classifier = svm(formula = V15 ~ ., 
                     data = training_set, 
                     type = 'C-classification', 
                     kernel = 'linear') 
    
    # Predicting the Test set results 
    y_pred = predict(classifier, newdata = test_set[-14]) 
    
    # Making the Confusion Matrix 
    cm = table(test_set$V15, y_pred) 
    acc = sum(diag(cm))/sum(cm)
    hasil[i] = acc
  }
  hasil
}

hasil1 = sim1(100,0.6)
summary(hasil1)
hist(hasil1)
shapiro.test(hasil1)




#=====================================SVM====================================
#Load Library
library(e1071)
library(hydroGOF)

BH <- read.csv(file = "HousingData.csv",
               stringsAsFactors = FALSE)

BH = na.omit(BH)

#Regression with SVM
modelsvm = svm(MEDV~.,BH,kernel = "linear")
#Predict using SVM regression
predsvm = predict(modelsvm, BH)

#Calculate RMSE 
RMSE2=rmse(predsvm,BH$MEDV)
RMSE2
#Calculate MAE
MAE= mean(abs(predsvm-BH$MEDV))
MAE
#Calculate MAE
MAPE= mean(abs(predsvm-BH$MEDV)/BH$MEDV)
MAPE

sim3 = function(B, p)
{
  #Load Library
  library(e1071)
  library(hydroGOF)
  
  BH <- read.csv(file = "HousingData.csv",
                 stringsAsFactors = FALSE)
  
  BH = na.omit(BH)
  dataset = BH
  
  hasil = matrix(0, B, 4)
  for (i in 1:B)
  {
    
    # Splitting the dataset into the Training set and Test set 
    #install.packages('caTools') 
    library(caTools) 
    
    #set.seed(123) 
    
    split = sample.split(dataset$MEDV, SplitRatio = p) 
    
    training_set = subset(dataset, split == TRUE) 
    test_set = subset(dataset, split == FALSE) 
    # Feature Scaling 
    training_set[-14] = scale(training_set[-14]) 
    test_set[-14] = scale(test_set[-14]) 
    
    #Regression with SVM
    modelsvm = svm(MEDV~.,training_set,kernel = "linear")
    
    #Predict using SVM regression
    predsvm = predict(modelsvm, test_set)
    
    #Calculate RMSE 
    RMSE2=rmse(predsvm,test_set$MEDV)
    #Calculate MAE
    MAE= mean(abs(predsvm-test_set$MEDV))
    #Calculate MAE
    MAPE= mean(abs(predsvm-test_set$MEDV)/test_set$MEDV)
    R2 = 1-sum((test_set$MEDV-predsvm)^2)/sum((test_set$MEDV-mean(test_set$MEDV))^2)
    hasil[i,1] = RMSE2
    hasil[i,2] = MAE
    hasil[i,3] = MAPE
    hasil[i,4] = R2
  }
  hasil
}


hasil3 = sim3(5000,0.6)
summary(hasil3)
par(mfrow=c(2,2))
hist(hasil3[,1])
hist(hasil3[,2])
hist(hasil3[,3])
hist(hasil3[,4])
shapiro.test(hasil3[,1])
shapiro.test(hasil3[,2]) 
shapiro.test(hasil3[,3])
shapiro.test(hasil3[,4])

#=============================MODEL REGRESI LINEAR GANDA===================

#Load Library
library(e1071)
library(hydroGOF)
BH <- read.csv(file = "HousingData.csv",
               stringsAsFactors = FALSE)
BH = na.omit(BH)
#Regression with RLG
model.lm = lm(MEDV~.,BH)

#Predict using RLG
pred.lm = predict(model.lm, BH)

#Calculate RMSE 
RMSE2=rmse(pred.lm,BH$MEDV)
RMSE2
#Calculate MAE
MAE= mean(abs(pred.lm-BH$MEDV))
MAE
#Calculate MAE
MAPE= mean(abs(pred.lm-BH$MEDV)/BH$MEDV)
MAPE

sim4 = function(B, p)
{
  #Load Library
  library(e1071)
  library(hydroGOF)
  
  BH <- read.csv(file = "HousingData.csv",
                 stringsAsFactors = FALSE)
  
  BH = na.omit(BH)
  dataset = BH
  
  hasil = matrix(0, B, 4)
  for (i in 1:B)
  {
    
    # Splitting the dataset into the Training set and Test set 
    #install.packages('caTools') 
    library(caTools) 
    
    #set.seed(123) 
    
    split = sample.split(dataset$MEDV, SplitRatio = p) 
    
    training_set = subset(dataset, split == TRUE) 
    test_set = subset(dataset, split == FALSE) 
    # Feature Scaling 
    training_set[-14] = scale(training_set[-14]) 
    test_set[-14] = scale(test_set[-14]) 
    
    #Regression with SVM
    model.lm = lm(MEDV~.,training_set)
    
    #Predict using SVM regression
    pred.lm = predict(model.lm, test_set)
    
    #Calculate RMSE 
    RMSE2=rmse(pred.lm,test_set$MEDV)
    #Calculate MAE
    MAE= mean(abs(pred.lm-test_set$MEDV))
    #Calculate MAE
    MAPE= mean(abs(pred.lm-test_set$MEDV)/test_set$MEDV)
    R2 = 1-sum((test_set$MEDV-pred.lm)^2)/sum((test_set$MEDV-mean(test_set$MEDV))^2)
    hasil[i,1] = RMSE2
    hasil[i,2] = MAE
    hasil[i,3] = MAPE
    hasil[i,4] = R2
  }
  hasil
}


hasil4 = sim4(5000,0.6)
summary(hasil4)
par(mfrow=c(2,2))
hist(hasil4[,1])
hist(hasil4[,2])
hist(hasil4[,3])
hist(hasil4[,4])
shapiro.test(hasil4[,1])
shapiro.test(hasil4[,2]) 
shapiro.test(hasil4[,3])
shapiro.test(hasil4[,4])