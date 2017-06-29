brugs.bin.2arm <-
#function(p.c, n.c, p.t, n.t, pathcode=paste(installed.packages()['hbdct',2], "/hbdct",sep=""), codefile="BUGS_Bin_2arm.txt", del.cut, hyper){
function(p.c, n.c, p.t, n.t, pathcode=getwd(), codefile="BUGS_Bin_2arm.txt", del.cut, hyper){
  gen.data <- gen.bin.2arm(p.c=p.c, p.t=p.t, n.c=n.c, n.t=n.t, del.cut=del.cut)

  k <- length(n.c)
  init1 <- list(mu.del=0, tau.del=1, del=rnorm(k,0,1), p.c=runif(k,0,1))
  init2 <- list(mu.del=1, tau.del=5, del=rnorm(k,1,1/sqrt(5)), p.c=runif(k,0,1))
  init3 <- list(mu.del=-1, tau.del=0.5, del=rnorm(k,-1,1/sqrt(0.5)), p.c=runif(k,0,1))
  myinits <- list(init1, init2, init3)

  # fit <- try(BRugsFit(modelFile=paste(pathcode, codefile, sep = "/"), data=c(gen.data, hyper$prior),
                # inits=myinits, numChains = 3, DIC=FALSE,
                # parametersToSave=c("del.grt", "mu.del.grt", "mu.del.grt.0"), 
                # nBurnin = 7000, nIter = 15000, nThin=1, working.directory = NULL))
  fit <- NULL; fit$Stat <- matrix(rnorm(10000),ncol=1)
  if ('Stat' %in% names(fit)){
    bug.res <- as.vector(fit$Stat[,1])
  }else{
	bug.res <- NA
  }
  	bug.res
}

