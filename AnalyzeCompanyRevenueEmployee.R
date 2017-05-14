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

par(mfrow=c(3,3))
plot(revenue_lm3)

# interaction term
revenue_lm4 <- lm(log(Revenue_Million_TTM) ~ log(Num_Employees)+log(expanse_million) + log(Num_Employees) * log(expanse_million), companyData)
summary(revenue_lm4)
plot(revenue_lm4)

