smart_hclust <- function(data, n.clust){
  d <- dist(data)
  tree <- hclust(d)
  data$cluster <- as.factor(cutree(tree, n.clust))
  return(data)
}
