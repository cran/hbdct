find.n <-
function(del.cut, k, T, p.c, effect.size, 
                   alpha.target=0.1, power.target=0.9, 
                   tol.b1=0.02, tol.b2=0.02, tol.a=0.05, 
                   n.min=5, n.max=70, nstep=10, rep=500, 
                   pathcode=path.package("hbdct"), codefile="BUGS_Bin_2arm.txt", pathout=getwd(), 
                   method="linear") {
   

  if (n.min >= n.max)
    stop("n.min is larger or equal to n.max!")

  time.start <- Sys.time()  

  lower <- power.target - tol.b1;
  upper <- power.target + tol.b2;
  
  colname <- c('n', 'alpha.fw')
  for (i in 1:k) colname <- c(colname, paste('power.ind', i, sep=''))

  myseed <- 125634
  set.seed(myseed)

  if (method=="linear") {
    filename1 <- paste(pathout, '/history_linear.txt', sep='')
    cat('## del.cut:', del.cut, '\n', file=filename1, sep=' ')
    cat('## T:', T, '\n', file=filename1, sep=' ', append=TRUE)
    cat(colname, '\n', file=filename1, append=TRUE)

    n <- n.min
    power.ind <- power.fun(n, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
    alpha.fw <- alpha.fun(n, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
    cat(n, alpha.fw, power.ind, '\n', file=filename1, append=TRUE)

    if (min(power.ind) < lower) { 
      repeat {
        n.next <- n + nstep;
        power.ind <- power.fun(n.next, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
        alpha.fw <- alpha.fun(n.next, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
        cat(n.next, alpha.fw, power.ind, '\n', file=filename1, append=TRUE)
        
        if (n.next > n.max) {
          print.op(flag=0, n.next, del.cut, k, T, alpha.fw, power.ind)
          stop("Sample Size exceeds n.max!", call.=FALSE) 
        }

        if(min(power.ind) >= lower)
          break

        n <- n.next
      }

      if (max(power.ind) > upper | alpha.fw > alpha.target + tol.a) {
        bisect(n, n.next, filename1, del.cut, k, T, p.c, effect.size, 
               alpha.target, power.target, tol.b1, tol.b2, tol.a,
               rep, pathcode, codefile, pathout) 
      }else {
        print.op(flag=1, n.next, del.cut, k, T, alpha.fw, power.ind)
      }

    }else if (max(power.ind) > upper) {
      warning("n.min is larger than the upper bound!\n", 
              "The algorithm linearly decreases n from n.min to find the optimal sample size.", call.=FALSE)
      repeat {
        n.next <- n - nstep;
        power.ind <- power.fun(n.next, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
        alpha.fw <- alpha.fun(n.next, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
        cat(n.next, alpha.fw, power.ind, '\n', file=filename1, append=TRUE)

        if(max(power.ind) <= upper)
            break

        n <- n.next
      }   

      if (min(power.ind) < lower | alpha.fw > alpha.target + tol.a) {
        bisect(n.next, n, filename1, del.cut, k, T, p.c, effect.size, 
               alpha.target, power.target, tol.b1, tol.b2, tol.a,
               rep, pathcode, codefile, pathout) 
      }else {
        print.op(flag=1, n.next, del.cut, k, T, alpha.fw, power.ind)
      }

    }else if (alpha.fw > alpha.target + tol.a) {
      print.op(flag=0, n.min, del.cut, k, T, alpha.fw, power.ind)
      stop("tol.a too small, family-wise Type-I error is larger than (alpha.target + tol.a)!", call.=FALSE)

    }else {
      print.op(flag=1, n.min, del.cut, k, T, alpha.fw, power.ind)
      message("Sample Size = n.min!")
    }
  }

  if (method=="bisect") {
    filename2 <- paste(pathout, '/history_bisect.txt', sep='')
    cat('## del.cut:', del.cut, '\n', file=filename2, sep=' ')
    cat('## T:', T, '\n', file=filename2, sep=' ', append=TRUE)
    cat(colname, '\n', file=filename2, append=TRUE)

    na <- n.min
    nb <- n.max
    power.a <- power.fun(na, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout) 
    alpha.a <- alpha.fun(na, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
    power.b <- power.fun(nb, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
    alpha.b <- alpha.fun(nb, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
    cat(na, alpha.a, power.a, '\n', file=filename2, append=TRUE)
    cat(nb, alpha.b, power.b, '\n', file=filename2, append=TRUE)

    if (max(power.a) > upper) {
      print.op(flag=0, na, del.cut, k, T, alpha.a, power.a)
      stop("n.min is larger than the upper bound!", call.=FALSE)
    }

    if (min(power.b) < lower) {
      print.op(flag=0, nb, del.cut, k, T, alpha.b, power.b)
      stop("n.max is smaller than the lower bound!", call.=FALSE)
    }

    if ((min(power.a) < lower | alpha.a > alpha.target + tol.a) & 
        (max(power.b) > upper | alpha.b > alpha.target + tol.a)) {
      bisect(na, nb, filename2, del.cut, k, T, p.c, effect.size, 
               alpha.target, power.target, tol.b1, tol.b2, tol.a,
               rep, pathcode, codefile, pathout) 
    }else if (min(power.a) >= lower & alpha.a <= alpha.target + tol.a) {
      print.op(flag=1, na, del.cut, k, T, alpha.a, power.a)
      message("Sample Size = n.min!")
    }else if (max(power.b) <= upper & alpha.b <= alpha.target + tol.b1) {
      print.op(flag=1, nb, del.cut, k, T, alpha.b, power.b)
      message("Sample Size = n.max!")
    } 
  }

  time.end <- Sys.time()
  print(time.end - time.start);
  print(paste(method, "method was used."));
}

