gen.bin.2arm <-
function(p.c, n.c, p.t, n.t, del.cut){
  k <- length(n.c)
  x.c <- rbinom(k, n.c, p.c)
  x.t <- rbinom(k, n.t, p.t)
  mydata <- list(K=k, x.c=x.c, x.t=x.t, n.c=n.c, n.t=n.t, del.cut=del.cut)
  mydata
}

