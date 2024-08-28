ari <- function (group1, group2)
{ group1 <- as.numeric(group1)
  group2 <- as.numeric(group2)
  x <- abs(sapply(group1, function(x) x - group1))
  x[x > 1] <- 1
  y <- abs(sapply(group2, function(x) x - group2))
  y[y > 1] <- 1
  sg <- sum(abs(x - y))/2
  bc <- choose(dim(x)[1], 2)
  ari <- 1 - sg/bc
  return(ari)
}
