# Functions for Exercise 5

returns <- function(column){
  vec <- column
  rets <- diff(vec)/vec[-length(vec)]
  out <- mean(rets)
  return(out)
}

OptimalPortfolio <- function(names,data.train,lambda=1,min.inv=0){
  # Reduce the data set accordingly
  data.red <- data.train[,names,with=F]
  
  # Estimate expected returns based on historicals
  mu <- data.red[ , lapply(.SD,returns), .SDcols = names]
  cat('\nExpected returns calculation (%)\n')
  print(mu*100)
  
  # Estimate Covariance matrix
  cat('\nCovariance Matrix calculation\n')
  Sigma <- cov(data.red)
  print(Sigma)
  
  #Define the constraints
  size <- length(names(data.red))
  constr <- matrix( c(rep(1,size),rep(-1,size)) ,size , 2)
  constr <- cbind2(constr,diag(size))
  
  # Find optimal portfolio allocation, solving quadratic program
  sol <- solve.QP( Dmat=2*lambda*Sigma, dvec=mu, Amat=constr, bvec=c(1,-1,rep(min.inv,size)) )
  vec.sol <- sol$solution
  dt.sol <- round(data.table(t(vec.sol)),3)
  setnames(dt.sol,names)
  cat('\nOptimal portfolio allocation is (%):\n')
  print(dt.sol*100)
  
  return(dt.sol)
}


# Invest with optimal portfolio in next data set, hold and compare with SP alternative
PortfolioPerformance<- function(names,data.invest){
  
  f.date <- min(data.invest[,Date])
  l.date <- max(data.invest[,Date])
  
  cat(paste0('\nBuying and holding portfolio in: ',as.character(f.date)))
  cat(paste0('\nCheck returns in: ',as.character(l.date)))
  cat('\n.....')
  
  data.red.inv <- data.invest[Date%in%c(f.date,l.date),names,with=F]
  p0 <- sum(dt.sol*as.matrix(data.red.inv)[1,])
  pt <- sum(dt.sol*as.matrix(data.red.inv)[2,])
  opt.ret <- (pt-p0)/p0*100
  cat('\nReturn obtained with optimal portfolio: ',opt.ret,' (%)')
  
  sp0 <- data.invest[Date%in%c(f.date,l.date),'sp',with=F][1,]
  spt <- data.invest[Date%in%c(f.date,l.date),'sp',with=F][2,]
  sp.ret <- as.numeric((spt-sp0)/sp0*100)
  cat('\nReturn investing only in S&P 500: ',sp.ret,' (%)')
  
  if(sp.ret < opt.ret){
    cat('\n\n******Outperformed!  :)')
  }else{
    cat('\n\n******Better put your money on S&P  :(')
  }
  return(list(opt.ret,sp.ret))
}
