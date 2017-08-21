## remove elements of x such that x<q025-iqr*1.5 or x>q075+iqr*1.5
remove.outliers <- function(x, extract=FALSE) {
  q <- quantile(x, c(0.25,0.75))
  qmid <- (q[2]+q[1])/2
  qdif <- (q[2]-q[1])*2
  cond <- abs(x-qmid) > qdif
  return(ifelse(extract, x[cond], x[!cond]))
}
