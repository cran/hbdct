brugs.bin.2arm <-
function(p.c, n.c, p.t, n.t, pathcode=path.package("hbdct"), codefile="BUGS_Bin_2arm.txt", del.cut){

  gen.data <- gen.bin.2arm(p.c=p.c, p.t=p.t, n.c=n.c, n.t=n.t, del.cut=del.cut)

  k <- length(n.c)
  init1 <- list(mu.del=0, tau.del=1, del=rnorm(k,0,1), p.c=runif(k,0,1))
  init2 <- list(mu.del=1, tau.del=5, del=rnorm(k,1,1/sqrt(5)), p.c=runif(k,0,1))
  init3 <- list(mu.del=-1, tau.del=0.5, del=rnorm(k,-1,1/sqrt(0.5)), p.c=runif(k,0,1))
  myinits <- list(init1, init2, init3)

  fit <- try(BRugs::BRugsFit(modelFile=paste(pathcode, codefile, sep = "/"), data=gen.data, 
                inits=myinits, numChains = 3, DIC=FALSE,
                parametersToSave=c("del.grt", "mu.del.grt", "mu.del.grt.0"), 
                nBurnin = 7000, nIter = 15000, nThin=1, working.directory = NULL))
  
  bug.res <- as.vector(fit$Stat[,1])
  bug.res
}

