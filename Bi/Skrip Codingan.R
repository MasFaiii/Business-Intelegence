#PRAKTIKUM BI - DIARY BISNIS

x = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7)
y = c(39343, 46205, 37731, 43525, 39891, 56642, 60150, 54445, 64445, 57189)
data = data.frame(y, x)

has.lm = lm(y ~ x, data=data)
summary(has.lm)

plot(x,y,xlab="Pengalaman", ylab="Salary",main="Hubungan Pengalaman & Salary")
abline(a = 28217, b=9021, col=2)

#koefisien korelasi

r = cor(x,y)

#koefisien determinasi

R2 = r^2
R2


# Simple Linear Regression
# Importing the dataset
dataset = read.table('salary.txt')

# Splitting the dataset into the
# Training set and Test set
#install.packages('caTools')
library(caTools)
split = sample.split(dataset[,2], SplitRatio = 0.5)
trainingset = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the Training set

lm.r= lm(formula = V2 ~ ., data = trainingset)
coef(lm.r)

# Predicting the Test set results
ypred = predict(lm.r, newdata = testset)

#install.packages("ggplot2")
library(ggplot2)

# Visualising the Training set results
ggplot() + geom_point(aes(x = trainingset$V1,
                          y = trainingset$V2), colour = 'red') +  
  geom_line(aes(x = trainingset$V1, y = predict(lm.r, newdata = trainingset)), colour = 'blue') +
  
  ggtitle('Salary vs Experience (Training set)')  
xlab('Years of experience')  
ylab('Salary')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = testset$V1, y = testset$V2),
             colour = 'red') +
  geom_line(aes(x = trainingset$V1,
                y = predict(lm.r, newdata = trainingset)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')


#MULTIPLE LINEAR REGRESSION

Year <- c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2016,
          2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016)

Month <- c(12, 11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1)
Interest_Rate <- c(2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,
                   2,2,2,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75)

Unemployment_Rate <- c(5.3,5.3,5.3,5.3,5.4,5.6,5.5,5.5,5.5,5.6,5.7,
                       5.9,6,5.9,5.8,6.1,6.2,6.1,6.1,6.1,5.9,6.2,6.2,6.1)

Stock_Index_Price <- c(1464,1394,1357,1293,1256,1254,1234,1195,1159,1167,
                       1130,1075,1047,965,943,958,971,949,884,866,876,822,704,719) 

plot(x=Interest_Rate, y=Stock_Index_Price) 

plot(x=Unemployment_Rate, y=Stock_Index_Price) 



model <- lm(Stock_Index_Price ~ Interest_Rate + Unemployment_Rate)
summary(model)

model1 <- lm(Stock_Index_Price ~ Interest_Rate + Unemployment_Rate - 1)
summary(model1)


model2 <- lm(Stock_Index_Price ~ Interest_Rate - 1)
summary(model2)

#Machine Learning

library(caTools)
dataset = data.frame(Stock_Index_Price, Interest_Rate, Unemployment_Rate) 

split = sample.split(dataset[,1], SplitRatio = 0.5)
trainingset = subset(dataset, split == TRUE)
testset = subset(dataset, split == FALSE)

# Fitting Simple Linear Regression to the Training set

lm.r= lm(formula = Stock_Index_Price ~ ., data = trainingset)
coef(lm.r)


# Predicting the Test set results
ypred = predict(lm.r, newdata = testset)
ypred

MSE = sum((ypred-testset[,1])^2)/length(ypred-3)
RMSE = sqrt(MSE)
RMSE

sim = function(B, p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    dataset = data.frame(Stock_Index_Price, Interest_Rate, Unemployment_Rate) 
    
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = Stock_Index_Price ~ ., data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-3)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

has = sim(1000, 0.5)
hist(has)
summary(has)

has = sim(1000, 0.6)
hist(has)
summary(has)

has = sim(1000, 0.7)
hist(has)
summary(has)

#==============================================


sim1 = function(B, p)
{
  hasil = matrix(0,B,4)
  for (i in 1:B)
  {
    dataset = data.frame(Stock_Index_Price, Interest_Rate, Unemployment_Rate) 
    
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = Stock_Index_Price ~ ., data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    #MSE = sum((ypred-testset[,1])^2)/length(ypred)
    MSE = sum((ypred-testset[,1])^2)/(length(ypred)-3)
    MAE = sum(abs(ypred-testset[,1]))/(length(ypred))
    MAPE = 100*sum(abs(ypred-testset[,1])/testset[,1])/(length(ypred))
    R2 = 1 - sum((ypred-testset[,1])^2)/sum((ypred-mean(testset[,1]))^2)
    hasil[i,1] = sqrt(MSE)
    hasil[i,2] = MAE
    hasil[i,3] = MAPE
    hasil[i,4] = R2
  }
  hasil
}

has = sim1(10000, 0.6)
summary(has)
par(mfrow=c(2,2))
hist(has[,1],main="RMSE")
hist(has[,2],main="MAE")
hist(has[,3],main="MAPE")
hist(has[,4],main="R2")

#===========================================================================



#=======================================================================

# read the data from the web
autompg = read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",quote = "\"",comment.char = "",stringsAsFactors = FALSE)
# give the dataframe headers
colnames(autompg) = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year", "origin", "name")
# remove missing data, which is stored as "?"
autompg = subset(autompg, autompg$hp != "?")
# remove the plymouth reliant, as it causes some issues
autompg = subset(autompg, autompg$name != "plymouth reliant")
# give the dataset row names, based on the engine, year and name
rownames(autompg) = paste(autompg$cyl, "cylinder", autompg$year, autompg$name)
# remove the variable for name, as well as origin
autompg = subset(autompg, select = c("mpg", "cyl", "disp", "hp", "wt", "acc", "year"))
# change horsepower from character to numeric
autompg$hp = as.numeric(autompg$hp)
# check final structure of data
str(autompg)

mpg_model = lm(mpg ~ wt + year, data = autompg)
coef(mpg_model)

n = nrow(autompg)
p = length(coef(mpg_model))
X = cbind(rep(1, n), autompg$wt, autompg$year)
y = autompg$mpg

(beta_hat = solve(t(X) %*% X) %*% t(X) %*% y)

coef(mpg_model)

summary(mpg_model)$sigma

y_hat = X %*% solve(t(X) %*% X) %*% t(X) %*% y
e     = y - y_hat
sqrt(t(e) %*% e / (n - p))

sqrt(sum((y - y_hat) ^ 2) / (n - p))

summary(mpg_model)

summary(mpg_model)$coef

confint(mpg_model, level = 0.99)

new_cars = data.frame(wt = c(3500, 5000), year = c(76, 81))

new_cars

predict(mpg_model, newdata = new_cars, interval = "confidence", level = 0.99)

new_cars$wt

range(autompg$wt)

new_cars$year

range(autompg$year)

plot(year ~ wt, data = autompg, pch = 20, col = "dodgerblue", cex = 1.5)

points(new_cars, col = "darkorange", cex = 3, pch = "X")

x0 = c(1, 3500, 76)

x0 %*% beta_hat

beta_hat


x0 = c(0, 0, 1)

x0 %*% beta_hat

new_cars

predict(mpg_model, newdata = new_cars, interval = "prediction", level = 0.99)


summary(mpg_model)$r.squared

null_mpg_model = lm(mpg ~ 1, data = autompg)

full_mpg_model = lm(mpg ~ wt + year, data = autompg)

anova(null_mpg_model, full_mpg_model)

summary(mpg_model)

# SSE
sum(resid(full_mpg_model) ^ 2)

## [1] 4556.646
# SST

sum(resid(null_mpg_model) ^ 2)
## [1] 23761.67

# Degrees of Freedom: Regression
length(coef(full_mpg_model)) - length(coef(null_mpg_model))
## [1] 2

# Degrees of Freedom: Error
length(resid(full_mpg_model)) - length(coef(full_mpg_model))
## [1] 387

# Degrees of Freedom: Total
length(resid(null_mpg_model)) - length(coef(null_mpg_model))
## [1] 389

names(autompg)
## [1] "mpg"  "cyl"  "disp" "hp"   "wt"   "acc"  "year"

null_mpg_model = lm(mpg ~ wt + year, data = autompg)
#full_mpg_model = lm(mpg ~ wt + year + cyl + disp + hp + acc, data = autompg)
full_mpg_model = lm(mpg ~ ., data = autompg)
anova(null_mpg_model, full_mpg_model)

# SSDiff
sum((fitted(full_mpg_model) - fitted(null_mpg_model)) ^ 2)
## [1] 26.17981
# SSE (For Full)
sum(resid(full_mpg_model) ^ 2)
## [1] 4530.466
# SST (For Null)
sum(resid(null_mpg_model) ^ 2)
## [1] 4556.646
# Degrees of Freedom: Diff
length(coef(full_mpg_model)) - length(coef(null_mpg_model))
## [1] 4
# Degrees of Freedom: Full
length(resid(full_mpg_model)) - length(coef(full_mpg_model))
## [1] 383
# Degrees of Freedom: Null
length(resid(null_mpg_model)) - length(coef(null_mpg_model))
## [1] 387

#===========================================================================


set.seed(1337)
n = 100 # sample size
p = 3

beta_0 = 5
beta_1 = -2
beta_2 = 6
sigma  = 4

x0 = rep(1, n)
x1 = sample(seq(1, 10, length = n))
x2 = sample(seq(1, 10, length = n))
X = cbind(x0, x1, x2)
C = solve(t(X) %*% X)

eps      = rnorm(n, mean = 0, sd = sigma)
y        = beta_0 + beta_1 * x1 + beta_2 * x2 + eps

sim_data = data.frame(x1, x2, y)

(beta_hat = C %*% t(X) %*% y)

coef(lm(y ~ x1 + x2, data = sim_data))
## (Intercept)          x1          x2 
##    7.290735   -2.282176    5.843424


c(beta_0, beta_1, beta_2)
## [1]  5 -2  6


y_hat = X %*% beta_hat
(s_e = sqrt(sum((y - y_hat) ^ 2) / (n - p)))
## [1] 4.294307

summary(lm(y ~ x1 + x2, data = sim_data))$sigma
## [1] 4.294307


C[3, 3]
## [1] 0.00145343
C[2 + 1, 2 + 1]
## [1] 0.00145343
sigma ^ 2 * C[2 + 1, 2 + 1]
## [1] 0.02325487

num_sims = 10000
beta_hat_2 = rep(0, num_sims)
for(i in 1:num_sims) {
  eps           = rnorm(n, mean = 0 , sd = sigma)
  sim_data$y    = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit           = lm(y ~ x1 + x2, data = sim_data)
  beta_hat_2[i] = coef(fit)[3]
}

C[3, 3]
## [1] 0.00145343
C[2 + 1, 2 + 1]
## [1] 0.00145343
sigma ^ 2 * C[2 + 1, 2 + 1]
## [1] 0.02325487


num_sims = 10000
beta_hat_2 = rep(0, num_sims)
for(i in 1:num_sims) {
  eps           = rnorm(n, mean = 0 , sd = sigma)
  sim_data$y    = beta_0 * x0 + beta_1 * x1 + beta_2 * x2 + eps
  fit           = lm(y ~ x1 + x2, data = sim_data)
  beta_hat_2[i] = coef(fit)[3]
}

var(beta_hat_2)
## [1] 0.02343408
sigma ^ 2 * C[2 + 1, 2 + 1]
## [1] 0.02325487
#The standard deviations found from the simulated data and the parent population are also very close.

sd(beta_hat_2)
## [1] 0.1530819
sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])
## [1] 0.1524955

hist(beta_hat_2, prob = TRUE, breaks = 20, 
     xlab = expression(hat(beta)[2]), main = "", border = "dodgerblue")
curve(dnorm(x, mean = beta_2, sd = sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])), 
      col = "darkorange", add = TRUE, lwd = 3)


sd_bh2 = sqrt(sigma ^ 2 * C[2 + 1, 2 + 1])
# We expect these to be: 0.68, 0.95, 0.997
mean(beta_2 - 1 * sd_bh2 < beta_hat_2 & beta_hat_2 < beta_2 + 1 * sd_bh2)
## [1] 0.6807
mean(beta_2 - 2 * sd_bh2 < beta_hat_2 & beta_hat_2 < beta_2 + 2 * sd_bh2)
## [1] 0.9529
mean(beta_2 - 3 * sd_bh2 < beta_hat_2 & beta_hat_2 < beta_2 + 3 * sd_bh2)
## [1] 0.9967

#================================================

dim(autompg)
head(autompg)


has.lm = lm(mpg ~ ., data = autompg)
summary(has.lm)
has.lm1 = lm(mpg ~ wt + year, data = autompg)
summary(has.lm1)

#install.packages("caTools")

par(mfrow=c(1,1))
library(caTools)

sim = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    
    dataset = autompg 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = mpg ~ ., data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-7)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

B = 1000

hasil = sim(B, 0.9)
hist(hasil)
summary(hasil)


sim1 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    
    dataset = autompg 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = mpg ~ wt + year, data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-3)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

B = 1000

hasil1 = sim1(B, 0.9)
hist(hasil1)
summary(hasil1)

t.test(hasil, hasil1)

cor(autompg)

hasil = sim(B, 0.8)
hasil1 = sim1(B, 0.8)
hist(hasil1)
summary(hasil1)

t.test(hasil, hasil1)


hasil = sim(B, 0.7)
hasil1 = sim1(B, 0.7)
hist(hasil1)
summary(hasil1)

t.test(hasil, hasil1)



sim2 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    
    dataset = autompg 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = mpg ~ wt, data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-2)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

B = 1000

hasil2 = sim2(B, 0.9)
hist(hasil2)
summary(hasil2)

t.test(hasil1, hasil2)

cor(autompg)

hasil1 = sim1(B, 0.8)
hasil2 = sim2(B, 0.8)
hist(hasil2)
summary(hasil2)

t.test(hasil1, hasil2)



sim3 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    
    dataset = autompg 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = mpg ~ year, data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-2)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

B = 1000

hasil3 = sim3(B, 0.9)
hist(hasil3)
summary(hasil3)

t.test(hasil2, hasil3)

cor(autompg)

hasil2 = sim2(B, 0.8)
hasil3 = sim3(B, 0.8)
hist(hasil3)
summary(hasil3)

t.test(hasil2, hasil3)








#============================================================================

#install.packages("tidyverse")

library(tidyverse)

#install.packages("remotes")

remotes::install_github("kassambara/datarium")

data("marketing", package = "datarium")

head(marketing, 4)

model <- lm(sales ~ ., data = marketing)

summary(model)

confint(model)

sigma(model)/mean(marketing$sales)

model1 <- update(model,  ~. -newspaper)

summary(model1)


#install.packages("caTools")

library(caTools)

sim = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    data("marketing", package = "datarium")
    dataset = marketing 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = sales ~ ., data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-7)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

B = 1000

hasil = sim(B, 0.9)
hist(hasil)
summary(hasil)


model1 <- lm(sales ~ youtube + facebook, data = marketing)

summary(model1)

sim1 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    data("marketing", package = "datarium")
    dataset = marketing 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = sales ~ youtube + facebook, data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-3)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

B = 1000
hasil1 = sim1(B, 0.9)
hist(hasil1)
summary(hasil1)

t.test(hasil, hasil1)


hasil = sim(B, 0.8)
hist(hasil)
summary(hasil)

t.test(hasil, hasil1)

hasil1 = sim1(B, 0.8)
hist(hasil1)
summary(hasil1)

t.test(hasil, hasil1)


hasil = sim(B, 0.7)
hist(hasil)
summary(hasil)

t.test(hasil, hasil1)

hasil1 = sim1(B, 0.7)
hist(hasil1)
summary(hasil1)

t.test(hasil, hasil1)


hasil = sim(B, 0.6)
hist(hasil)
summary(hasil)

t.test(hasil, hasil1)

hasil1 = sim1(B, 0.6)
hist(hasil1)
summary(hasil1)

t.test(hasil, hasil1)

par(mfrow=c(1,2))
hasil = sim(B, 0.5)
hist(hasil)
summary(hasil)

hasil1 = sim1(B, 0.5)
hist(hasil1)
summary(hasil1)

t.test(hasil, hasil1)

model2 <- lm(sales ~ youtube , data = marketing)

summary(model2)

sim2 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    data("marketing", package = "datarium")
    dataset = marketing 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = sales ~ youtube, data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-2)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

B = 1000
hasil1 = sim1(B, 0.9)
hasil2 = sim2(B, 0.9)
hist(hasil2)
summary(hasil1)
summary(hasil2)
t.test(hasil1, hasil2)


hasil1 = sim1(B, 0.8)
hasil2 = sim2(B, 0.8)
hist(hasil2)
summary(hasil1)
summary(hasil2)

t.test(hasil1, hasil2)


hasil1 = sim1(B, 0.7)
hasil2 = sim2(B, 0.7)
hist(hasil2)
summary(hasil1)
summary(hasil2)

t.test(hasil1, hasil2)


hasil1 = sim1(B, 0.6)
hasil2 = sim2(B, 0.6)
hist(hasil2)
summary(hasil1)
summary(hasil2)

t.test(hasil1, hasil2)


hasil1 = sim1(B, 0.5)
hasil2 = sim2(B, 0.5)
hist(hasil2)
summary(hasil1)
summary(hasil2)

t.test(hasil1, hasil2)


model3 <- lm(sales ~ facebook , data = marketing)

summary(model3)


sim2 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    data("marketing", package = "datarium")
    dataset = marketing 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = sales ~ youtube, data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-2)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

sim3 = function(B,p)
{
  hasil = numeric(B)
  for (i in 1:B)
  {
    data("marketing", package = "datarium")
    dataset = marketing 
    split = sample.split(dataset[,1], SplitRatio = p)
    trainingset = subset(dataset, split == TRUE)
    testset = subset(dataset, split == FALSE)
    
    # Fitting Simple Linear Regression to the Training set
    
    lm.r= lm(formula = sales ~ facebook, data = trainingset)
    
    # Predicting the Test set results
    ypred = predict(lm.r, newdata = testset)
    
    MSE = sum((ypred-testset[,1])^2)/length(ypred-2)
    hasil[i] = sqrt(MSE)
  }
  hasil
}

par(mfrow=c(1,2))
B = 1000
hasil2 = sim2(B, 0.9)
hist(hasil2)
summary(hasil2)

hasil3 = sim3(B, 0.9)
hist(hasil3)
summary(hasil3)

t.test(hasil2, hasil3)


hasil2 = sim2(B, 0.8)
hasil3 = sim3(B, 0.8)
hist(hasil2)
hist(hasil3)
summary(hasil2)
summary(hasil3)

t.test(hasil2, hasil3)


hasil2 = sim2(B, 0.7)
hasil3 = sim3(B, 0.7)
hist(hasil3)
summary(hasil2)
summary(hasil3)

t.test(hasil2, hasil3)


hasil2 = sim2(B, 0.6)
hasil3 = sim3(B, 0.6)
hist(hasil3)
summary(hasil2)
summary(hasil3)

t.test(hasil2, hasil3)


hasil2 = sim2(B, 0.5)
hasil3 = sim3(B, 0.5)
hist(hasil3)
summary(hasil2)
summary(hasil3)

t.test(hasil2, hasil3)