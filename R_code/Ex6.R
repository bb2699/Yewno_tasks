# setwd('C:/Users/Bernardo/Desktop/Yewno_tasks/R_code');  source('Ex6.R')

#########################################################################
# Code for exercise 6
#
#########################################################################

rm(list=ls())
options(digits=5)

# Load necessary packages
param.dir <- 'C:/Users/Bernardo/Desktop/Yewno_tasks/R_code/'
source(paste0(param.dir,'load.libs.R'))
source(paste0(param.dir,'Ex6.lib.R'))

# Read the data
data <- fread(paste0(param.dir,"dxy_full.csv"))
data <- na.omit(data)

# Calculate diffs to hopefully get stationary time series
ret.list <- list()
for (col in names(data)[!names(data)%in%c('Date')] ){
  ret.list[[col]] <- diffs(data[,col,with=F])
}
names(ret.list) <- NULL
ret.data <- do.call(cbind.data.frame,ret.list)

# Perform Augmented Dickey Fuller test to each one of the diff time series for stationarity
p.vals <- Dickey.Fuller(ret.data)

# Check for cointegration 
mod1.str <- 'dxy ~ eurusd -1'
mod1 <- lm(formula(mod1.str),data=data)
cat('\nCoefficents for model: ',mod1.str,'\n')
print(mod1$coefficients)

cat('\nStationarity tests on the residuals:\n')
print(adf.test(mod1$residuals))
print(PP.test(mod1$residuals))

# Define a spread in terms of the difference fx rate and predictors
coefs <- mod1$coefficients
spread <- data[,dxy] - coefs['eurusd']*data[,eurusd]  

mu <- mean(spread)
plot(spread,type='l')
cat('\nMean spread: ',mu)
abline(h=mu)

# Trading strategy
cat('\n...\nCheck the markets:')
getFX("EUR/USD")
dxy.q <- Quandl("FRED/DTWEXM",order="desc", type="zoo")
eurusd.dt <- as.data.table(EURUSD)
dxy.dt <- as.data.table(dxy.q)


newdxy <- as.numeric(dxy.dt[.N,])
neweurusd <- eurusd.dt[.N,EUR.USD]
newspread <- newdxy - coefs['eurusd'] * neweurusd  

delta <- 0.1
cat('\nRecent quote of DXY:',newdxy)
cat('\nRecent quote of EUR/USD:',neweurusd)

if(newspread < mu + delta ){
  cat('\nBuy the spread portfolio!')
  
}else{
  cat('Short the spread portfolio!')
}
