
?plot

setwd("c:/000_Statistics MS/00_STAT 6509-Linear Regr/Data")

getwd()


install.packages("Quandl")
library(Quandl)

Quandl.api_key("ehEcxWoLx4AxHDWDFEQy")

#GDP by Year
mydata = Quandl("FRED/GDP",collapse="annual",start_date="2008-12-31", end_date="2016-12-31")


# top tech companies
#http://www.nasdaq.com/screening/companies-by-industry.aspx?industry=Technology&sortname=marketcap&sorttype=1

#GOOG, MSFT,FB ORCL, INTC, CSCO, NFLX, AAPL, QCOM, ADBE

#Fundamental Stock Data 2011-2016

all = Quandl.datatable("ZACKS/FC",per_type="A" ,qopts.columns=c("ticker","zacks_sector_code","per_fisc_year"))

all2<- subset(all,per_fisc_year==2016)

all2[order(all2$zacks_sector_code),]

all = Quandl.datatable("ZACKS/FC", ticker=c("AAPL","MSFT"),per_type="A")

all = Quandl.datatable("ZACKS/FC", ticker=c("AAPL","MSFT"),per_type="A")

all10 = Quandl.datatable("ZACKS/FC", ticker=c("AAPL","MSFT"),zacks_x_ind_code=10)

options(max.print=1000000) 

mydata = Quandl.datatable("ZACKS/FC"
  ,ticker=c(
    "KO",
    "PG",
    "DIS",
    "NKE",
    "HD",
    "MCD",
    "WMT",
    "JNJ",
    "MRK",
    "PFE",
    "UNH",
    "DD",
    "CAT",
    "GE",
    "GE",
    "MMM",
    "UTX",
    "AAPL",
    "CSCO",
    "IBM",
    "INTC",
    "MSFT",
    "VZ",
    "BA",
    "CVX",
    "XOM",
    "AXP",
    "GS",
    "JPM",
    "TRV",
    "V"
    )
  ,per_type="A"
  ,paginate="T"
  ,qopts.columns=c(
    "ticker",
    "comp_name",
    "exchange",
    "per_end_date",
    "per_fisc_year",
    "per_cal_year",
    "zacks_sector_code",
    "zacks_x_ind_code",
    "filing_date",
    "bus_city",
    "bus_state_name",
    "bus_post_code",
    "tot_revnu",
    "emp_cnt",
    "res_dev_exp",
    "tot_sell_gen_admin_exp",
    "oper_income",
    "pre_tax_income",
    "tot_oper_exp",
    "net_prop_plant_equip"
    ))

# this only does all cases (no subgroups) DONT USE
transform(mydata, new.col=c(NA,tot_revnu[-1]/tot_revnu[-nrow(mydata)]-1))

#Send to a csv file
write.csv(all,file="stockallfields.csv")

#Send to a csv file
write.csv(mydata,file="stockallzacks.csv")




require(plyr)
require(quantmod)
mydata2<- ddply(mydata,"ticker", transform,  
                DeltaCol1 = Delt(tot_revnu), 
                DeltaCol2 = Delt(emp_cnt),
                DeltaCol3 = Delt(res_dev_exp),
                DeltaCol4 = Delt(tot_sell_gen_admin_exp),
                DeltaCol5 = Delt(tot_oper_exp),
                DeltaCol6 = Delt(net_prop_plant_equip))
                

colnames(mydata2)[21] <- "tot_revnu_growth"
colnames(mydata2)[22] <- "emp_cnt_growth"
colnames(mydata2)[23] <- "res_dev_exp_growth"
colnames(mydata2)[24] <- "tot_sell_gen_admin_exp_growth"
colnames(mydata2)[25] <- "tot_oper_exp_growth"
colnames(mydata2)[26] <- "net_prop_plant_equip_growth"

# The Final Analysis File
mydata3<- na.omit(mydata2)
summary(mydata3)

mydata4<- subset(mydata3,(mydata3$tot_sell_gen_admin_exp_growth<=.50 & mydata3$net_prop_plant_equip_growth<=.50))

summary(mydata4)

summary(stockfile)

#Send to a csv file
write.csv(mydata4,file="stockfile.csv")

# Read final stockfile into R
stockfile<- read.csv("stockfile.csv",header=TRUE)

# GOTO STOCK LINEAR REGRESSION.R to run rest of Program!!!!!

fitx1<- lm(stockfile$tot_revnu_growth ~ stockfile$emp_cnt_growth)

## Calculate RMSE and other values
rmse <- round(sqrt(mean(resid(fitx1)^2)), 2)
coefs <- coef(fitx1)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],2)
r2 <- round(summary(fitx1)$r.squared, 2)

eqn1 <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                r^2 == .(r2) * "," ~~ RMSE == .(rmse))

fitx2<- lm(stockfile$tot_revnu_growth ~ stockfile$res_dev_exp_growth)

## Calculate RMSE and other values
rmse <- round(sqrt(mean(resid(fitx2)^2)), 2)
coefs <- coef(fitx2)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],2)
r2 <- round(summary(fitx2)$r.squared, 2)

eqn2 <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                 r^2 == .(r2) * "," ~~ RMSE == .(rmse))

fitx3<- lm(stockfile$tot_revnu_growth ~ stockfile$tot_sell_gen_admin_exp_growth)

## Calculate RMSE and other values
rmse <- round(sqrt(mean(resid(fitx3)^2)), 2)
coefs <- coef(fitx3)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],2)
r2 <- round(summary(fitx3)$r.squared, 2)

eqn3 <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                 r^2 == .(r2) * "," ~~ RMSE == .(rmse))

fitx4<- lm(stockfile$tot_revnu_growth ~ stockfile$tot_oper_exp_growth)

## Calculate RMSE and other values
rmse <- round(sqrt(mean(resid(fitx4)^2)), 2)
coefs <- coef(fitx4)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],2)
r2 <- round(summary(fitx4)$r.squared, 2)

eqn4 <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                 r^2 == .(r2) * "," ~~ RMSE == .(rmse))

fitx5<- lm(stockfile$tot_revnu_growth ~ stockfile$net_prop_plant_equip_growth)

## Calculate RMSE and other values
rmse <- round(sqrt(mean(resid(fitx5)^2)), 2)
coefs <- coef(fitx5)
b0 <- round(coefs[1], 2)
b1 <- round(coefs[2],2)
r2 <- round(summary(fitx5)$r.squared, 2)

eqn5 <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                 r^2 == .(r2) * "," ~~ RMSE == .(rmse))



par(mfrow=c(3,2))
plot(stockfile$tot_revnu_growth ~ stockfile$emp_cnt_growth)
abline(fitx1)
text(0, .4, eqn1)
plot(stockfile$tot_revnu_growth ~ stockfile$res_dev_exp_growth)
abline(fitx2)
text(0, .4, eqn2)
plot(stockfile$tot_revnu_growth ~ stockfile$tot_sell_gen_admin_exp_growth)
abline(fitx3)
text(0, .4, eqn3)
plot(stockfile$tot_revnu_growth ~ stockfile$tot_oper_exp_growth)
abline(fitx4)
text(0, .4, eqn4)
plot(stockfile$tot_revnu_growth ~ stockfile$net_prop_plant_equip_growth)
abline(fitx5)
text(.2, .4, eqn5)

























