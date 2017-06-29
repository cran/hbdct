MargnPw.bin.2arm <-
function(bug.res, cut){
  TF <- (bug.res > cut)
  pw.indv <- apply(TF, 2, mean)
  pw.indv
}

