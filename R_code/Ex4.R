# setwd('C:/Users/Bernardo/Desktop/Yewno_tasks/R_code');  source('Ex4.R')

#########################################################################
# Code for exercise 4
#
#########################################################################

rm(list=ls())

# Load necessary packages
param.dir <- 'C:/Users/Bernardo/Desktop/Yewno_tasks/R_code/'
source(paste0(param.dir,'load.libs.R'))
#########################################################################

# Read the data
data<- fread(paste0(param.dir,"hpi.csv"))
ind <- names(data)
ind <-ind[ind!='date']

# Candidate models
form2 <- 'hpi~hpi_lag1+google+google_lag1'
form1 <- 'hpi~hpi_lag1+google'
form3 <- 'hpi~hpi_lag1'
formulas <-c(form1,form2,form3)

res.list <- list()
# Fit the models
for (fo in formulas){
  mod <- lm(formula(fo),data=data[,ind,with=F])
  res.list[[fo]] <- data.table(Model=fo,AIC=AIC(mod),RMSE=sqrt(mean(mod$residuals^2)),
                               Adj.Rsq=summary(mod)$adj.r.squared)
  if(fo==form1){
    cat('\nShowing statistics for the best model ', fo,'\n')
    print(summary(mod))
  }

}

# Output result statistics
cat('\nSummary table of all models\n')
res <- do.call(rbind.data.frame,res.list)
print(res)

