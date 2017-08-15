
# setwd('C:/Users/Bernardo/Desktop/Yewno_tasks/R_code');  source('Ex1.R')

#########################################################################
# Code for exercise 1, normality assumption on financial returns data
#
#########################################################################


rm(list=ls())

# Load necessary packages
source('C:/Users/Bernardo/Desktop/Yewno_tasks/R_code/load.libs.R')


# Pull data from Yahoo finance and calculate returns
getSymbols("DJIA", src='FRED')
ret = periodReturn(DJIA,period="daily",type="log")
ret.dt <- as.data.table(ret)
DJIA <- unlist(ret.dt[,2])

#plot returns
pl1 <- plot(ret, main="Dow Jones Index daily returns")

#quantile plot
qqnorm(DJIA); qqline(DJIA, col = 2)

#Formal normality tests
print(shapiro.test(DJIA))
print(lillie.test(DJIA))

