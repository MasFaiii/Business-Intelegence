#---------------
# Decision tree
#---------------
library(caret)
library(rpart)
library(rpart.plot)	
library(AUC)

data = read.table('busage.txt',header=TRUE)
head(data)

n = dim(data)[1]
p = 0.6
index = sample(1:n, round(n*p), replace=TRUE)

train = data[index,]
test = data[-index, ]

#train
model.Dtree <- rpart(DEFAULT~., data = train, method="class")
prp(model.Dtree)

#lift chart
pb <- NULL
pb <- predict(model.Dtree, test)
pb <- as.data.frame(pb)
pred.Dtree <- data.frame(test$DEFAULT, pb$Y)

#confusion matrix
pc <- NULL
pc <- ifelse(pb$N > pb$Y, "N", "Y")

xtab <- table(pc, test$DEFAULT)
acc = sum(diag(xtab))/sum(xtab)
acc 

sim = function(B, p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    data = read.table('busage.txt',header=TRUE)
    n = dim(data)[1]
    index = sample(1:n, round(n*p), replace=TRUE)
    
    train = data[index,]
    test = data[-index, ]
    
    model.Dtree <- rpart(DEFAULT~., data = train, method="class")
    pb <- NULL
    pb <- predict(model.Dtree, test)
    pb <- as.data.frame(pb)
    pred.Dtree <- data.frame(test$DEFAULT, pb$Y)
    colnames(pred.Dtree) <- c("target","score")
    pc <- NULL
    pc <- ifelse(pb$N > pb$Y, "N", "Y")
    xtab <- table(pc, test$DEFAULT)
    acc = sum(diag(xtab))/sum(xtab)
    hasil[i] = acc 
  }
  hasil
}
par(mfrow=c(1,2))

hasil = sim(10000, 0.6)
summary(hasil)
hist(hasil)


hasil1 = sim(10000, 0.7)
summary(hasil1)
hist(hasil1)

ks.test(hasil, hasil1)

#============================================================

#------------------
# Data Preparation
#------------------

#Read datasets
#Download the data from http://www.saedsayad.com/datasets/BikeRental.zip
train <- read.csv("bike_rental_train.csv")
test <- read.csv("bike_rental_test.csv")

#Rows and Cols
dim(train)
dim(test)

#Columns name
colnames(train)
colnames(test)

#Show  
head(train)
head(test)


#Scatter plot
pairs(~temp+humidity+windspeed+bike_rent_count, data=train, main="Scatterplot - train", col="darkgreen")
pairs(~temp+humidity+windspeed+bike_rent_count, data=test, main="Scatterplot - test", col="brown")

#----------------------------
# Decision Tree - Regression
#----------------------------
library(rpart)
library(rpart.plot)

#Train
model.Dtree <- rpart(bike_rent_count~., data = train, method="anova")
prp(model.Dtree)

#Residual plot
res.Dtree = resid(model.Dtree)
plot(train$temp, res.Dtree, ylab="Residuals", xlab="Temperature", main="Residual Plot") 
abline(0, 0)

#Q-Q plot
stdres.Dtree = scale(res.Dtree)
qqnorm(stdres.Dtree, ylab="Standardized Residuals", xlab="Normal Scores", main="QQ Plot") 
qqline(stdres.Dtree)

#Test
pred.Dtree <- predict(model.Dtree, newdata=test)
err.Dtree <- test$bike_rent_count - pred.Dtree
rmse.Dtree <- sqrt(mean((err.Dtree^2)))

#Errors histogram
hist(err.Dtree, main="bike_rent_count", sub="(Actual-Predicted)", xlab="Error", breaks=10, col="darkred")

###
data = rbind(train, test)

n = dim(data)[1]
p = 0.6
index = sample(1:n, round(n*p), replace=TRUE)

train = data[index,]
test = data[-index, ]

#Train
model.Dtree <- rpart(bike_rent_count~., data = train, method="anova")
prp(model.Dtree)

#Residual plot
res.Dtree = resid(model.Dtree)
plot(train$temp, res.Dtree, ylab="Residuals", xlab="Temperature", main="Residual Plot") 
abline(0, 0)

#Q-Q plot
#stdres.Dtree = scale(res.Dtree)
#qqnorm(stdres.Dtree, ylab="Standardized Residuals", xlab="Normal Scores", main="QQ Plot") 
#qqline(stdres.Dtree)

#Test
pred.Dtree <- predict(model.Dtree, newdata=test)
err.Dtree <- test$bike_rent_count - pred.Dtree
rmse.Dtree <- sqrt(mean((err.Dtree^2)))
rmse.Dtree
par(mfrow=c(1,2))
hist(err.Dtree)
hist(test$bike_rent_count)

sim1 = function(B, p)
{
  hasil = matrix(0, B,4)
  data = rbind(train, test)
  for (i in 1:B) 
  {
    n = dim(data)[1]
    index = sample(1:n, round(n*p), replace=TRUE)
    
    train = data[index,]
    test = data[-index, ]
    
    #Train
    model.Dtree <- rpart(bike_rent_count~., data = train, method="anova")
    res.Dtree = resid(model.Dtree)
    
    #Test
    pred.Dtree <- predict(model.Dtree, newdata=test)
    err.Dtree <- test$bike_rent_count - pred.Dtree
    rmse.Dtree <- sqrt(mean((err.Dtree^2)))
    hasil[i,1] = rmse.Dtree
    hasil[i,2] = median(abs(err.Dtree/test$bike_rent_count))
    hasil[i,3] = mean(abs(err.Dtree))
    hasil[i,4] = (cor(test$bike_rent_count,pred.Dtree))^2
  }
  hasil
}

hasil = sim1(1000, 0.9)
summary(hasil)
par(mfrow=c(2,2))
hist(hasil[,1],main="Histogram of RMSE", xlab="", ylab="")
hist(hasil[,2],main="Histogram of MdAPE", xlab="", ylab="")
hist(hasil[,3],main="Histogram of MAE", xlab="", ylab="")
hist(hasil[,4],main="Histogram of R2", xlab="", ylab="")

#===================RANDOM FOREST==========================


library(randomForest)
library(datasets)
library(caret)
data <- iris
str(data)
data$Species <- as.factor(data$Species)
table(data$Species)
data$Species <- as.factor(data$Species)
table(data$Species)

set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(Species~., data=train, proximity=TRUE) 
rf

p1 <- predict(rf, train)
confusionMatrix(p1, train$ Species)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ Species)



plot(rf)

t <- tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 150,
            trace = TRUE,
            improve = 0.05)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
#MeanDecreaseGini

#======================RANDOM FOREST FOR CLASSIFICATION======================

wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"), header = TRUE, sep = ";") 
# This command is used to load the dataset

head(wine) # Display the head and dimensions of wine dataset

dim(wine)

barplot(table(wine$quality)) # Barplot to see the quality of wines. The output looks like below


# Now, we have to convert the quality values into factors

wine$taste <- ifelse(wine$quality < 5, "bad", "good")

wine$taste[wine$quality == 5] <- "normal"

wine$taste[wine$quality == 6] <- "normal"

wine$taste <- as.factor(wine$taste)

str(wine$taste)

barplot(table(wine$taste)) # Barplot to view the taste of wines. The output is shown below.

table(wine$taste) 

# Next, we need to split the data into training and testing. 80% for training, 20% for testing.


set.seed(123)

samp <- sample(nrow(wine), 0.8 * nrow(wine))

train <- wine[samp, ]

test <- wine[-samp, ]

# Moving onto the Data visualization

library(ggplot2)


ggplot(wine,aes(fixed.acidity,volatile.acidity))+ geom_point(aes(color=taste))# This command is used to display a scatter plot. The output looks like below


dim(train)

dim(test)  # Checks the dimensions of training and testing dataset


#install.packages('randomForest')

library(randomForest)           # Install the random forest library


# Now that we have installed the randomforest library, let’s build the random forest model


model <- randomForest(taste ~ . - quality, data = train, ntree = 1000, mtry = 5)

model

model$confusion