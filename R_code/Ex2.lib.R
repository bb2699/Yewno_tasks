# Functions for Exercise 2

# function to create the stock price tree
StockTree <- function(S, sigma, delta, N) {
  tree <- matrix(0, nrow=N+1, ncol=N+1)
  
  u <- exp(sigma*sqrt(delta))
  d <- exp(-sigma*sqrt(delta))
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] <- S * u^(j-1) * d^((i-1)-(j-1))
    }
  }
  
  return(tree)
}


#risk free probability calculation
q_prob <- function(r, delta, sigma) {
  u <- exp(sigma*sqrt(delta))
  d <- exp(-sigma*sqrt(delta))
  
  return((exp(r*delta) - d)/(u-d))
}

# function for the price of plain vanilla put and call
OptionTree <- function(tree, sigma, delta, r, K, type) {
  q <- q_prob(r, delta, sigma)
  
  option_tree <- matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  if(type == 'put') {
    option_tree[nrow(option_tree),] <- pmax(K - tree[nrow(tree),], 0)
  } else {
    option_tree[nrow(option_tree),] <- pmax(tree[nrow(tree),] - K, 0)
  }
  
  for (i in (nrow(tree)-1):1) {
    for(j in 1:i) {
      option_tree[i, j] <- ((1-q)*option_tree[i+1,j] + q*option_tree[i+1,j+1])/exp(r*delta)
    }
  }
  return(option_tree)
}

