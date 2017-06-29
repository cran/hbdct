OverallPw.bin.2arm <-
function(bug.res, g, cut){
  TF <- (bug.res[,c(-1,-2)] > cut)
  n.grt <- apply(TF, 1, sum)
  pw.g <- mean((n.grt >= g))
  res <- list(pw.g=pw.g, g=g)
  res
}

