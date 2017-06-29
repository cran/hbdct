#mydata <- list(K=4, x.c=rep(5,4), x.t=rep(6,4), n.c=rep(33,4), n.t=rep(33,4), del.cut=0.025)
#Run.hb2arm(data=mydata,  pathcode=paste(installed.packages()['hbdct',2], "/hbdct",sep=""),codefile="BUGS_Bin_2arm.txt",T=0.8)

Run.hb2arm <- function(data, pathcode, codefile, Tcut, ...) {
  hyper <- list(...)

  k <- data$K
  
  init1 <- list(mu.del=0, tau.del=1, del=rnorm(k,0,1), p.c=runif(k,0,1))
  init2 <- list(mu.del=1, tau.del=5, del=rnorm(k,1,1/sqrt(5)), p.c=runif(k,0,1))
  init3 <- list(mu.del=-1, tau.del=0.5, del=rnorm(k,-1,1/sqrt(0.5)), p.c=runif(k,0,1))
  myinits <- list(init1, init2, init3)
  
  # fit <- try(BRugsFit(modelFile=paste(pathcode, codefile, sep = "/"), data=c(data, hyper$prior),
                      # inits=myinits, numChains = 3, DIC=FALSE,
                      # parametersToSave=c("del.grt", "mu.del.grt", "mu.del.grt.0"), 
                      # nBurnin = 7000, nIter = 15000, nThin=1, working.directory = NULL))
  fit <- NULL; fit$Stat <- matrix(rnorm(10000),ncol=1)
  bug.res <- as.vector(fit$Stat[,1])
  bug.pr <- bug.res[1:k]
  res <- rbind(bug.pr, rep(Tcut, k), as.logical(bug.pr > T))
  rownames(res) <- c("del.grt", "T.p", "decision")
  return(res)
}
