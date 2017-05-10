# Company - employee - revenue analysis
# Revenue (response), #Employee (predictor)
setwd("/Users/pjaiswal/Desktop/Project")

companyData <- read.table("2017_us_company_data.txt", header = TRUE, sep='\t')

boxplot(companyData$Num_Employees)
boxplot(companyData$Revenue_Million_TTM)
plot(companyData$Num_Employees,companyData$Revenue_Million_TTM)
revenue_lm <- lm(Revenue_Million_TTM ~ Num_Employees, companyData)
summary(revenue_lm)
abline(revenue_lm)

# Using log transformation
boxplot(log(companyData$Num_Employees))
boxplot(log(companyData$Revenue_Million_TTM))
plot(log(companyData$Num_Employees),log(companyData$Revenue_Million_TTM))
revenue_lm2 <- lm(log(Revenue_Million_TTM) ~ log(Num_Employees), companyData)
abline(revenue_lm2)
summary(revenue_lm2)

