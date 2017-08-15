# setwd('C:/Users/Bernardo/Desktop/Yewno_tasks/R_code');  source('Ex5.R')

#########################################################################
# Code for exercise 5
#
#########################################################################
rm(list=ls())
options(digits=5)

######################### Parameters

# Select names for investment options
names <- c('latam','africa','asia','em')
# Date to divide data set into training and investing
divide.date <- as.Date('2017/01/01')

##########################

# Load necessary packages
param.dir <- 'C:/Users/Bernardo/Desktop/Yewno_tasks/R_code/'
source(paste0(param.dir,'load.libs.R'))
source(paste0(param.dir,'Ex5.lib.R'))

# Read the data
data <- fread(paste0(param.dir,"em.csv"))
data <- na.omit(data)
data[,Date:=data[ , lapply(.SD,as.Date,format='%m/%d/%Y'), .SDcols = c('Date')]]

# Divide the data set into a callibration data set and a set to invest
data.train <-  data[Date < divide.date,]
data.invest <- data[Date >= divide.date,]

# Calculate the optimal Portfolio
dt.sol <- OptimalPortfolio(names,data.train,lambda=1)

# Compare returns with S&P500 Benchmark
lis <- PortfolioPerformance(names,data.invest)

