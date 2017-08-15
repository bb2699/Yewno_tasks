


diffs <- function(column){
  vec <- unlist(column)
  rets <- diff(vec)
  out <- data.table(rets)
  setnames(out,paste0(names(column),'_dif'))
  return(out)
}

Dickey.Fuller <- function(ret.data){
  cat('\nAugmented Dickey Fuller test for the diff series\n')
  p.val.vec <-c()
  for (i in 1:length(colnames(ret.data))){
    
    ad <- suppressWarnings(adf.test(unlist(ret.data[,i,with=F])))
    cat(names(ret.data[,i,with=F]),' P-value: ',ad$p.value,'\n')
    p.val.vec <- c(p.val.vec,ad$p.value)
  }
  return(p.val.vec)
}

normalize <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}