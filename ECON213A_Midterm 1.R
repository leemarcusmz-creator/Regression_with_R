install.packages("haven")
library(haven)
house_data <- read_dta("Downloads/house.dta")
attach(house_data)
options(scipen = 999)
#QUESTION 1
#1b
hist(price)
summary(price)
hist(log(price))
#1c
cor.test(sqft, price)
plot(house_data$sqft, house_data$price)
garage_type <-as.factor(garage_type)
anova_test <- lm(price ~ garage_type)
anova(anova_test)
boxplot(house_data$price ~ house_data$garage_type)
#1d
hist(saledate, "days")
hist(saledate, "weeks")
hist(saledate, "months")
hist(saledate, "years")

#Question 2
reg.0 <- lm(price ~ bed + bath + garage + rooms, house_data)
summary(reg.0)
library(stargazer)
stargazer(reg.0, type="text")
reg.1 <- lm(price ~ yrbuilt + stories + sqft + bed + bath + garage, house_data)
summary(reg.1)
stargazer(reg.1, type="text")
plot(reg.1)
stargazer(reg.0, reg.1, type="html", out = "~/Desktop/table1.html") #stargazer with both reg.0 and reg.1

#2c
install.packages("lmtest")
library(lmtest)
bptest(reg.1) #run bp test to see if p-value rejects null for hetero or not
install.packages("gvlma") #check for heteroskedasticity present or not
library(gvlma)
gvlma(reg.1)

install.packages("carData")
library(carData)
install.packages("car")
library(car)
vif(reg.1)


install.packages("sandwich")
library(sandwich)
coeftest(reg.1, vcov=vcovHC(reg.1, type = "HC1"))
coeftest_reg.1 <- coeftest(reg.1, vcov=vcovHC(reg.1, type = "HC1"))[,4]
robust_reg.1 <- sqrt(diag(coeftest_reg.1))#robust tests for sensitivity to model specification 
summary(robust_reg.1)

AIC(reg.0, reg.1)

plot(reg.1)

#2e
hist(yrbuilt)
hist(stories)
hist(sqft)
hist(log(sqft))#makes it work
hist(bed)
hist(bath)
hist(garage)

#3c
reg.question3 <- lm(price ~ yrbuilt, house_data)y
summary(reg.question3)
stargazer(reg.question3, type="html", out = "~/Desktop/table2.html")
