# setwd('C:/Users/Bernardo/Desktop/Yewno_tasks/R_code');  source('Ex3.R')

#########################################################################
# Code for exercise 3
#
#########################################################################

rm(list=ls())
options(digits=2)

# Load necessary packages
param.dir <- 'C:/Users/Bernardo/Desktop/Yewno_tasks/R_code/'
source(paste0(param.dir,'load.libs.R'))
#########################################################################

# Read the data
data <- fread(paste0(param.dir,"profitability.csv"))
P.names <- c('p_etr','p_gpm','p_nm','p_nsa','p_ppm','p_swcr')
C.names <- c('c_etr','c_gpm','c_nm','c_nsa','c_ppm','c_swcr')
P.data <- data[,P.names,with=F]
C.data <- data[,C.names,with=F]

# Perform PCA analysis
P.pca <- prcomp(P.data,center=T,scale=T)
C.pca <- prcomp(C.data,center=T,scale=T)

# Explained variance
print(summary(P.pca))
print(summary(C.pca))

# Get only the first principal components (profitability indices)
P.profit <- predict(P.pca)[,1]
C.profit <- predict(C.pca)[,1]

# Estimate Covariance matrix
cat('\nCovariance Matrix calculation\n')
Sigma <- matrix( c( var(P.profit),cov(P.profit,C.profit),
                   cov(P.profit,C.profit),var(C.profit)  ), nrow=2, ncol=2  )
print(Sigma)

#Variance calculation for a few weight cases
res.list <- list()
cat('\nPortfolio Risk calculation\n')
for (w.P in seq(0,1,by=0.1) ){
  w.C <- 1-w.P
  w <- c(w.P, w.C)
  risk <- t(w) %*% Sigma %*% w
  res.list[[paste0(w.P)]] <- data.table(Pfizer.weight=w.P,
                                        Cisco.weight=w.C,
                                        Risk=round(as.numeric(risk),1))
}

# Results
risk.res <- do.call(rbind.data.frame,res.list)
print(risk.res)

