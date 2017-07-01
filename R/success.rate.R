success.rate <-
  function(n.pergrp, del.cut, k, T, p.c, effect.size, 
           rep=500, pathcode, codefile="BUGS_Bin_2arm.txt", pathout) {
    n.c <- rep(n.pergrp, k)
    n.t <- n.c
    
    p.t0 <- p.c
    p.t5 <- p.c + effect.size  
    
    colname <- c('id')
    for (i in 1:k) colname <- c(colname, paste('post.p', i, sep=''))
    colname <- c(colname, 'post.mu', 'post.mu.0') 
    
    filename <- paste(pathout, "/post_", del.cut, '_', n.pergrp, ".txt", sep="")
    cat('## del.cut:', del.cut, '\n', file=filename, sep=' ')
    cat('## n.pergrp:', n.pergrp, '\n', file=filename, sep=' ', append=TRUE)
    cat(colname, '\n', file=filename, sep=' ', append=TRUE)
    
    for (i in 1:rep) {
      bug.res <- brugs.bin.2arm(p.c=p.c, p.t=p.t5, n.c=n.c, n.t=n.t,
                                pathcode=pathcode, codefile=codefile, del.cut=del.cut)
      cat(i, bug.res, '\n', file=filename, sep=' ', append=TRUE)
    }
    
    post <- read.table(filename, header=TRUE, skip=2)
    post <- post[,-1] # first column is id
    alpha.fw <- OverallPw.bin.2arm(bug.res=post, g=1, cut=T)$pw.g
    power.ind <- as.vector(MargnPw.bin.2arm(bug.res=post, cut=T))
    
    filename2 <- paste(pathout, "/oc_", n.pergrp, ".txt", sep="")
    cat("effect.size =", effect.size, '\n', file=filename2, sep = ' ')
    cat("prob.success.fw", "prob.success.ind", '\n', file = filename2, sep = ' ', append=TRUE)
    cat(alpha.fw, power.ind, '\n', file = filename2, sep = ' ', append=TRUE)
    # the last two columns of power.ind are post.mu and post.mu.0, should be discarded;  
    
  }