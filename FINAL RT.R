
library(readxl)
data <- read_excel("data.xlsx", sheet = "Sheet3") 
View(data)


#outliers removal
outliers<-boxplot(data$LP)$out
x<-data
x<- x[-which(x$LP %in% outliers),]
View(x)
#1st model for listing price
model1 <- lm(  sqrt(LP) ~ IP + IS + QIB + HNI + RII + F1 + F2 + F3 + NP + TLA + CFOA + ROCE ,data=x)
summary(model1)
plot(model1,2)

# To check multicollinearity
library(car)
vif(model1)


#To test for autocorrelation/independency among residuals.
durbinWatsonTest(model1)

# To test Normality of residuals
shapiro.test(model1$residuals)

#TO CHECK NORMALITY OF ERRORS BY Q-Q PLOT
plot(model1 , 2)
# To test for homoscedasticity using Breusch pagan test
library(lmtest)
bptest(model1)

install.packages("het.test")
library("het.test")
whites.htest(model1)


#model2 by taking sqr root 
model2 <- lm(  sqrt(LP) ~ IP + IS + QIB + HNI + RII + F1 + F2 + F3  ,data=x)
model2
summary(model2)


# To check multicollinearity
library(car)
vif(model2)


#To test for autocorrelation/independency among residuals.
durbinWatsonTest(model2)

# To test Normality of residuals
shapiro.test(model2$residuals)

#TO CHECK NORMALITY OF ERRORS BY Q-Q PLOT
plot(model2 , 2)
# To test for homoscedasticity using Breusch pagan test
library(lmtest)
bptest(model2)






##old model with box cox

library(readxl)
data1 <- read_excel("data.xlsx", sheet = "Sheet1")
View(data1)
model4 <- lm(xxCOR ~ IP+ IS  + QIB + HNI + RII + F1 + F2 + F3  ,data=data1)
summary(model4)

library(MASS)
bx<-boxCox(model4,seq(-3,3))


best=bx$x[which(bx$y==max(bx$y))]
best


model5 <- lm((((xxCOR^best)-1)/best) ~ IP+ IS + QIB + HNI + RII + F1 + F2 + F3 ,data=data1)
summary(model5)

# To check multicollinearity
library(car)
vif(model5)


#To test for autocorrelation/independency among residuals.
durbinWatsonTest(model5)


# To test Normality of residuals
shapiro.test(model5$residuals)

#TO CHECK NORMALITY OF ERRORS BY Q-Q PLOT
plot(model5 , 2)


# To test for homoscedasticity using Breusch pagan test
library(lmtest)
bptest(model5)

library(readxl)
data <- read_excel("data.xlsx", sheet = "Sheet3") 
View(data)
model1 <- lm(  sqrt(LP) ~ IP + IS + QIB + HNI + RII +FF +G+ H + NP + TLA + CFOA + ROCE ,data=data)
summary(model1)
plot(model1,2)
