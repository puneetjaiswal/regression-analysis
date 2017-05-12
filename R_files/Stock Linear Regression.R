setwd("c:/000_Statistics MS/00_STAT 6509-Linear Regr/Data")

# Read final stockfile into R
stockfile<- read.csv("stockfile.csv",header=TRUE)

summary(stockfile)

length(stockfile)

head(stockfile,n=4)

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
plot(stockfile$tot_revnu_growth ~ stockfile$emp_cnt_growth,
     xlab="Annual Employee Count Growth(%)",ylab="Annual Revenue Growth(%)",
     main="Annual Revenue Growth vs Employee Count Growth",yaxt="n",xaxt="n")
axis(2, at=pretty(stockfile$tot_revnu_growth), lab=paste0(pretty(stockfile$tot_revnu_growth)*100, "%"),las=TRUE)
axis(1, at=pretty(stockfile$emp_cnt_growth), lab=paste0(pretty(stockfile$emp_cnt_growth)*100, "%"),las=TRUE)
abline(fitx1, col=4)
text(0, .25, eqn1)
plot(stockfile$tot_revnu_growth ~ stockfile$res_dev_exp_growth,
     xlab="Annual R&D Expense Growth(%)",ylab="Annual Revenue Growth(%)",
     main="Annual Revenue Growth vs R&D Expense Growth",yaxt="n",xaxt="n")
axis(2, at=pretty(stockfile$tot_revnu_growth), lab=paste0(pretty(stockfile$tot_revnu_growth)*100, "%"),las=TRUE)
axis(1, at=pretty(stockfile$res_dev_exp_growth), lab=paste0(pretty(stockfile$res_dev_exp_growth)*100, "%"),las=TRUE)
abline(fitx2, col=4)
text(0, .25, eqn2)
plot(stockfile$tot_revnu_growth ~ stockfile$tot_sell_gen_admin_exp_growth,
     xlab="Annual SG&A Expense Growth(%)",ylab="Annual Revenue Growth(%)",
     main="Annual Revenue Growth vs SG&A Expense Growth",yaxt="n",xaxt="n")
axis(2, at=pretty(stockfile$tot_revnu_growth), lab=paste0(pretty(stockfile$tot_revnu_growth)*100, "%"),las=TRUE)
axis(1, at=pretty(stockfile$tot_sell_gen_admin_exp_growth), lab=paste0(pretty(stockfile$tot_sell_gen_admin_exp_growth)*100, "%"),las=TRUE)
abline(fitx3, col=4)
text(0, .25, eqn3)
plot(stockfile$tot_revnu_growth ~ stockfile$tot_oper_exp_growth,
     xlab="Annual Oper Expense Growth(%)",ylab="Annual Revenue Growth(%)",
     main="Annual Revenue Growth vs Oper Expense Growth",yaxt="n",xaxt="n")
axis(2, at=pretty(stockfile$tot_revnu_growth), lab=paste0(pretty(stockfile$tot_revnu_growth)*100, "%"),las=TRUE)
axis(1, at=pretty(stockfile$tot_oper_exp_growth), lab=paste0(pretty(stockfile$tot_oper_exp_growth)*100, "%"),las=TRUE)
abline(fitx4, col=4)
text(0, .25, eqn4)
plot(stockfile$tot_revnu_growth ~ stockfile$net_prop_plant_equip_growth,
     xlab="Annual PP&E Growth(%)",ylab="Annual Revenue Growth(%)",
     main="Annual Revenue Growth vs PP&E Expense Growth",yaxt="n",xaxt="n")
axis(2, at=pretty(stockfile$tot_revnu_growth), lab=paste0(pretty(stockfile$tot_revnu_growth)*100, "%"),las=TRUE)
axis(1, at=pretty(stockfile$net_prop_plant_equip_growth), lab=paste0(pretty(stockfile$net_prop_plant_equip_growth)*100, "%"),las=TRUE)
abline(fitx5, col=4)
text(.2, .25, eqn5)

#stockfile16<- subset(stockfile,stockfile$per_fisc_year==2016)
#plot(log(stockfile16$tot_revnu) ~ log(stockfile16$emp_cnt))

plot(log(stockfile16$emp_cnt),log(stockfile16$tot_revnu))
revenue_lm2 <- lm(log(tot_revnu) ~ log(emp_cnt), stockfile16)
abline(revenue_lm2)
summary(revenue_lm2)




