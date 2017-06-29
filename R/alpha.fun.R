alpha.fun <-
function(n.pergrp, del.cut, k, T, p.c, effect.size, 
               #rep=500, pathcode=paste(installed.packages()['hbdct',2], "/hbdct",sep=""), codefile="BUGS_Bin_2arm.txt", pathout=getwd(), hyper) {
			   rep=500, pathcode=getwd(), codefile="BUGS_Bin_2arm.txt", pathout=getwd(), hyper) {
  n.c <- rep(n.pergrp, k)
  n.t <- n.c

  p.t0 <- p.c
  p.t5 <- p.c + effect.size  

  colname <- c('id')
  for (i in 1:k) colname <- c(colname, paste('post.p', i, sep=''))
  colname <- c(colname, 'post.mu', 'post.mu.0') 

  filename <- paste(pathout, "/null_", del.cut, '_', n.pergrp, ".txt", sep="")
  cat('## del.cut:', del.cut, '\n', file=filename, sep=' ')
  cat('## n.pergrp:', n.pergrp, '\n', file=filename, sep=' ', append=TRUE)
  cat(colname, '\n', file=filename, sep=' ', append=TRUE)

  for (i in 1:rep) {
    bug.res <- brugs.bin.2arm(p.c=p.c, p.t=p.t0, n.c=n.c, n.t=n.t, 
                              pathcode=pathcode, codefile=codefile, del.cut=del.cut, hyper=hyper)

    if (!is.na(bug.res)){
      cat(i, bug.res, '\n', file=filename, sep=' ', append=TRUE)
    }else{
      next;
    }
  }

  post.null <- read.table(filename, header=TRUE, skip=2)
  post.null <- post.null[,-1] # first column is id
  alpha.fw <- OverallPw.bin.2arm(bug.res=post.null, g=1, cut=T)$pw.g

  alpha.fw
}

