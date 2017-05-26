#AnalyzeProfit

setwd("/Users/pjaiswal/Desktop/Project")
companyData <- read.table("2017_us_company_data.txt", header = TRUE, sep='\t')

par(mfrow=c(1,1))

boxplot(companyData$profit_Million_TTM)
boxplot(log(companyData$profit_Million_TTM))

boxplot(companyData$num_employees,main="number of employees")
boxplot(log(companyData$num_employees),main="Log(number of employees)")

# relation between num_employees and profit
plot(companyData$Num_Employees, companyData$profit_Million_TTM)
plot(log(companyData$Num_Employees), log(companyData$profit_Million_TTM))
cor.test(companyData$Num_Employees, companyData$profit_Million_TTM)
cor.test(log(companyData$Num_Employees), log(companyData$profit_Million_TTM))

# relation between expanse and profit
boxplot(companyData$expanse_million)
boxplot(log(companyData$expanse_million))
plot(companyData$expanse_million, companyData$profit_Million_TTM)
plot(log(companyData$expanse_million), log(companyData$profit_Million_TTM))
cor.test(companyData$expanse_million, companyData$profit_Million_TTM)
cor.test(log(companyData$expanse_million), log(companyData$profit_Million_TTM))

#relation between profit and enterprise value
boxplot(companyData$enterprise_val_Million)
boxplot(log(companyData$enterprise_val_Million))
plot(companyData$enterprise_val_Million, companyData$profit_Million_TTM)
plot(log(companyData$enterprise_val_Million), log(companyData$profit_Million_TTM))
cor.test(companyData$enterprise_val_Million, companyData$profit_Million_TTM)
cor.test(log(companyData$enterprise_val_Million), log(companyData$profit_Million_TTM))


par(mfrow=c(2,2))

profitLm <- lm(log(profit_Million_TTM)~log(Num_Employees)+log(expanse_million)+log(enterprise_val_Million), companyData)
summary(profitLm)
plot(profitLm)

# this one is a better model than the above one
profitLm1 <- lm(log(profit_Million_TTM)~log(Num_Employees)+log(enterprise_val_Million), companyData)
summary(profitLm1)
plot(profitLm1)

profitLm2 <- lm(log(profit_Million_TTM)~log(Num_Employees)+log(enterprise_val_Million)+log(Num_Employees)*log(enterprise_val_Million), companyData)
summary(profitLm2)
plot(profitLm2)


