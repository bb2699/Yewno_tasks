

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

# change the format of the tree to be compatible with genlattice functions
convertTree <- function(mat){
  
  res <- c()
  for (i in 1:length(mat[,1])){
    vec <- mat[i,]
    res <- c(res,vec[vec!=0])
  }
  
  return(res)
}

# Generate a binomial lattice for charts
genlattice <- function(S=10, sigma=0.2, delta=1/2, N=5) {
  u = exp(sigma*sqrt(delta))
  d = exp(-sigma*sqrt(delta))
  X <- c()
  X[1] <- S
  count <- 2
  
  for (i in 1:N) {
    for (j in 0:i) {
      X[count] <- S * u^j * d^(i-j)
      count <- count + 1
    }
  }
  return(X)
}

# generate a graph specification that can be fed into graphviz
dotlattice <- function(S, labels=FALSE) {
  shape <- ifelse(labels == TRUE, "plaintext", "point")
  
  cat("digraph G {", "\n", sep="")
  cat("node[shape=",shape,", samehead, sametail];","\n", sep="")
  cat("rankdir=LR;","\n")
  
  cat("edge[arrowhead=none];","\n")
  
  # Create a dot node for each element in the lattice
  for (i in 1:length(S)) {
    cat("node", i, "[label=\"", S[i], "\"];", "\n", sep="")
  }
  
  # The number of levels in a binomial lattice of length N
  # is `$\frac{\sqrt{8N+1}-1}{2}$`
  L <- ((sqrt(8*length(S)+1)-1)/2 - 1)
  
  k<-1
  for (i in 1:L) {
    tabs <- rep("\t",i-1)
    j <- i
    while(j>0) {
      cat("node",k,"->","node",(k+i),";\n",sep="")
      cat("node",k,"->","node",(k+i+1),";\n",sep="")
      k <- k + 1
      j <- j - 1
    }
  }
  
  cat("}", sep="")
}

function(S, labels=FALSE) {
  shape <- ifelse(labels == TRUE, "plaintext", "point")
  
  cat("digraph G {", "\n", sep="")
  cat("node[shape=",shape,", samehead, sametail];","\n", sep="")
  cat("rankdir=LR;","\n")
  
  cat("edge[arrowhead=none];","\n")
  
  # Create a dot node for each element in the lattice
  for (i in 1:length(S)) {
    cat("node", i, "[label=\"", S[i], "\"];", "\n", sep="")
  }
  
  # The number of levels in a binomial lattice of length N
  # is `$\frac{\sqrt{8N+1}-1}{2}$`
  L <- ((sqrt(8*length(S)+1)-1)/2 - 1)
  
  k<-1
  for (i in 1:L) {
    tabs <- rep("\t",i-1)
    j <- i
    while(j>0) {
      cat("node",k,"->","node",(k+i),";\n",sep="")
      cat("node",k,"->","node",(k+i+1),";\n",sep="")
      k <- k + 1
      j <- j - 1
    }
  }
  
  cat("}", sep="")
}


