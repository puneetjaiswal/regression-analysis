# Company - employee - revenue analysis
# Revenue (response), #Employee (predictor)

setwd("/Users/pjaiswal/Desktop/Project")

companyData <- read.table("2017_us_company_data.tsv", header = TRUE, sep='\t')

par(mfrow=c(1,2))
boxplot(companyData$Revenue_Million_TTM)
plot(companyData$Num_Employees,companyData$Revenue_Million_TTM)
cor.test(companyData$Num_Employees,companyData$Revenue_Million_TTM)
revenue_lm <- lm(Revenue_Million_TTM ~ Num_Employees, companyData)
summary(revenue_lm)
abline(revenue_lm)


# Using log transformation to reduce the residual st. error
boxplot(log(companyData$Num_Employees))
boxplot(log(companyData$Revenue_Million_TTM))
cor.test(log(companyData$Num_Employees),log(companyData$Revenue_Million_TTM))
plot(log(companyData$Num_Employees),log(companyData$Revenue_Million_TTM))
revenue_lm2 <- lm(log(Revenue_Million_TTM) ~ log(Num_Employees), companyData)
abline(revenue_lm2)
summary(revenue_lm2)

# including expanse in model
boxplot(log(companyData$expanse_million))
revenue_lm3 <- lm(log(Revenue_Million_TTM) ~ log(Num_Employees)+log(expanse_million), companyData)
summary(revenue_lm3)

par(mfrow=c(2,2))
plot(revenue_lm3)

# interaction term
revenue_lm4 <- lm(log(Revenue_Million_TTM) ~ log(Num_Employees)+log(expanse_million) + log(Num_Employees) * log(expanse_million), companyData)
summary(revenue_lm4)
plot(revenue_lm4)
# Interaction term has reduced the F-statistic value but no significan improvement in R-squared value

# include company valuation also in the model
boxplot(companyData$enterprise_val_Million)
boxplot(log(companyData$enterprise_val_Million))
revenue_lm5 <- lm(log(Revenue_Million_TTM) ~ log(Num_Employees)+log(expanse_million)+log(enterprise_val_Million), companyData)
summary(revenue_lm5)
plot(revenue_lm5)

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
# num_emp = 1470, enterprise_val = 525.89M, profit = 13.78 B, Observed revenue = 261.03M, expanse = revenue - profit = 79.13B
newdata = list(Num_Employees=150500, expanse_million= 79130, enterprise_val_Million=112620)
logRevenue = predict(revenue_lm5,newdata, interval = "confidence", level = 0.95)
antilog(logRevenue, exp(1))
log(92910)
