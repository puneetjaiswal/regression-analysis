# Company - employee - revenue analysis
# Revenue (response), #Employee (predictor)

setwd("/Users/pjaiswal/Desktop/Project")

companyData <- read.table("2017_us_company_data.txt", header = TRUE, sep='\t')

par(mfrow=c(1,2))

# Revenue
boxplot(companyData$revenue_million_TTM,main="Revenue_Million_TTM")
boxplot(log(companyData$revenue_million_TTM),main="Log(Revenue_Million_TTM)")

# Num_emp
boxplot(companyData$num_employees,main="number of employees")
boxplot(log(companyData$num_employees),main="Log(number of employees)")

# Expense
boxplot(companyData$expense_million,main="Expense TTM")
boxplot(log(companyData$expense_million),main="Log (Expense TTM)")

# Enterprise Value
boxplot(companyData$enterprise_val_Million, main="Enterprise Valuation")
boxplot(log(companyData$enterprise_val_Million), main="Log (Enterprise Valuation)")

par(mfrow=c(2,3))
plot(x=companyData$expense_million, y=companyData$revenue_million_TTM)
plot(x=companyData$enterprise_val_Million, y=companyData$revenue_million_TTM)
plot(x=companyData$num_employees, y=companyData$revenue_million_TTM)

plot(x=log(companyData$expense_million), y=log(companyData$revenue_million_TTM))
plot(x=log(companyData$enterprise_val_Million), y=log(companyData$revenue_million_TTM))
plot(x=log(companyData$num_employees), y=log(companyData$revenue_million_TTM))

cor.test(x=companyData$expanse_million, y=companyData$revenue_million_TTM)
cor.test(x=companyData$enterprise_val_Million, y=companyData$revenue_million_TTM)
cor.test(x=companyData$num_employees, y=companyData$revenue_million_TTM)

par(mfrow=c(1,2))
revenue_lm <- lm(revenue_million_TTM ~ num_employees, companyData)
summary(revenue_lm)
abline(revenue_lm)


# Using log transformation to reduce the residual st. error
boxplot(log(companyData$num_employees))
boxplot(log(companyData$revenue_million_TTM))
cor.test(log(companyData$num_employees),log(companyData$revenue_million_TTM))
plot(log(companyData$num_employees),log(companyData$revenue_million_TTM))
revenue_lm2 <- lm(log(revenue_million_TTM) ~ log(num_employees), companyData)
abline(revenue_lm2)
summary(revenue_lm2)

# including expanse in model
boxplot(log(companyData$expense_million))
boxplot(companyData$expense_million)
revenue_lm3 <- lm(log(revenue_million_TTM) ~ log(num_employees)+log(expense_million), companyData)
summary(revenue_lm3)

par(mfrow=c(2,2))
plot(revenue_lm3)

# interaction term
revenue_lm4 <- lm(log(revenue_million_TTM) ~ log(num_employees)+log(expense_million) + log(num_employees) * log(expense_million), companyData)
summary(revenue_lm4)
plot(revenue_lm4)
# Interaction term has reduced the F-statistic value but no significan improvement in R-squared value

# include company valuation also in the model
boxplot(companyData$enterprise_val_Million)
boxplot(log(companyData$enterprise_val_Million))
revenue_lm5 <- lm(log(revenue_million_TTM) ~ log(num_employees)+log(expense_million)+log(enterprise_val_Million), companyData)
summary(revenue_lm5)
plot(revenue_lm5)


# more interaction terms
revenue_lm6 <- lm(log(revenue_million_TTM) ~ log(num_employees)+log(expense_million)+log(enterprise_val_Million)+ log(num_employees) * log(expense_million)+ log(num_employees) * log(enterprise_val_Million), companyData)
summary(revenue_lm6)
plot(revenue_lm6)


# No Log transform
revenue_lm7 <- lm(revenue_million_TTM ~ num_employees+expense_million+enterprise_val_Million, companyData)
summary(revenue_lm7)
plot(revenue_lm7)
hist(revenue_lm7$residuals)

# No Log transform, with interaction terms
revenue_lm8 <- lm(revenue_million_TTM ~ num_employees+expense_million+enterprise_val_Million+(num_employees*enterprise_val_Million), companyData)
summary(revenue_lm8)
plot(revenue_lm)

plot(companyData$num_employees,revenue_lm8$fitted.values)
plot(companyData$num_employees,companyData$revenue_million_TTM)




# antilog function used to reverse predicted response value 
antilog<-function(lx,base) 
{ 
  lbx<-lx/log(exp(1),base=base) 
  result<-exp(lbx) 
  result 
} 

plot(revenue_lm5$fitted.values,log(companyData$Revenue_Million_TTM))
abline(0,1)

# Now lets predict revenue for HORTONWORKS, INC. (HDP) - Nasdaq
# num_emp = 1080, enterprise_val = 435.78M, profit =112.29M, Observed revenue = 199.09M, expanse = revenue - profit = 86.8 M
newdata = list(Num_Employees=1080, expanse_million= 86.8, enterprise_val_Million=435.78)
logRevenue = predict(revenue_lm5,newdata, interval = "confidence", level = 0.95)
logRevenue
antilog(logRevenue, exp(1))

# Now lets predict revenue for Oracle Corporation (ORCL) - NYSE
# num_emp = 136000, enterprise_val = 180.43B, profit = 	29.57B, Observed revenue = 37.43B, expanse = revenue - profit = 7.86B
newdata = list(Num_Employees=136000, expanse_million= 7860, enterprise_val_Million=180430)
logRevenue = predict(revenue_lm5,newdata, interval = "confidence", level = 0.95)
logRevenue
antilog(logRevenue, exp(1))

# Now lets predict revenue for BOX INC (BOX) - NYSE
# num_emp = 1495, enterprise_val = 2.22B, profit = 	286.48M, Observed revenue = 398.61M, expanse = revenue - profit = 112.13M
newdata = list(Num_Employees=1495, expanse_million= 112.13, enterprise_val_Million=2220)
logRevenue = predict(revenue_lm5,newdata, interval = "confidence", level = 0.95)
logRevenue
antilog(logRevenue, exp(1))

# Now lets predict revenue for Splunk Inc. (SPLK) - NasdaqGS
# num_emp = 2700, enterprise_val = 8.3B, Observed revenue = 949.96M, profit = 758.9M, expanse = revenue - profit = 191.06M
newdata = list(Num_Employees=2700, expanse_million= 191.06, enterprise_val_Million=8300)
logRevenue = predict(revenue_lm5,newdata, interval = "confidence", level = 0.95)
logRevenue
antilog(logRevenue, exp(1))

# Now lets predict revenue for The Boeing Company (BA) - NYSE
# num_emp = 150500, enterprise_val = 112.62B, Observed revenue = 92.91B, profit = 13.78B, expanse = revenue - profit = 79.13B
newdata = list(Num_Employees=150500, expanse_million= 79130, enterprise_val_Million=112620)
logRevenue = predict(revenue_lm5,newdata, interval = "confidence", level = 0.95)
logRevenue
antilog(logRevenue, exp(1))

######## Running AIC to determine best model ############
nullModel <- lm(log(revenue_million_TTM) ~ 1, companyData)
summary(nullModel)

fullModel <- lm(log(revenue_million_TTM) ~ log(num_employees)+log(expense_million)+log(enterprise_val_Million)+ log(num_employees) * log(expense_million)+ log(num_employees) * log(enterprise_val_Million), companyData)
summary(fullModel)

step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="forward")

finalModel <-  lm(log(revenue_million_TTM) ~ log(expense_million) + log(enterprise_val_Million) + log(num_employees) + log(enterprise_val_Million):log(num_employees), data=companyData)
summary(finalModel)