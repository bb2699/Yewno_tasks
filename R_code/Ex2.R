
# setwd('C:/Users/Bernardo/Desktop/Yewno_tasks/R_code');  source('Ex2.R')
#########################################################################
# Code for exercise 2
#
#########################################################################

rm(list=ls())
param.dir <- 'C:/Users/Bernardo/Desktop/Yewno_tasks/R_code/'
#########################################################################

# Load necessary packages
source(paste0(param.dir,'load.libs.R'))
source(paste0(param.dir,'Ex2.lib.R'))

# Parameters
S <- 10
sigma <- 0.2
delta <- 1/5
N <- 5
K <- 10
r <- 0.1

# Construct the tree of stock prices
cat('\nCalculating tree of stock prices\n')
stock.tree <- StockTree(S=S, sigma=sigma, delta=delta, N=N)

# Construct the tree of option prices
cat('\nCalculating trees of option prices\n')
call.tree <- OptionTree(stock.tree, sigma=sigma, delta=delta, r=r, K=K, type='call')
put.tree <- OptionTree(stock.tree, sigma=sigma, delta=delta, r=r, K=K, type='put')

#Prices of puts and calls
put <- put.tree[1,1]
call <- call.tree[1,1]
cat('\nCall Price: ',call)
cat('\nPut Price: ',put)

# Check put-call parity
if (  call- (  put + S - K*exp(-r)  ) < 10^-6 ){
  cat('\nPut - Call parity verified!\n')
  cat('call-  put = S - K * exp(-r)  )\n')
}

# Generate output charts
stock <- capture.output(dotlattice(convertTree(stock.tree),labels=TRUE))
option <- capture.output(dotlattice(convertTree(call.tree),labels=TRUE))
cat(stock, file="lat.stock.dot")
cat(option, file="lat.option.dot")

