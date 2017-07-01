bisect <-
function(na, nb, filename, 
                   del.cut, k, T, p.c, effect.size,
                   alpha.target=0.1, power.target=0.9, 
                   tol.b1=0.02, tol.b2=0.02, tol.a=0.05,
                   rep=500, 
                   pathcode=path.package("hbdct"), codefile="BUGS_Bin_2arm.txt", pathout=getwd()) {
 
  lower <- power.target - tol.b1;
  upper <- power.target + tol.b2;

  n.list <- c(na, nb);

  n <- round((na + nb)/2)
  power.ind <- power.fun(n, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
  alpha.fw <- alpha.fun(n, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
  cat(n, alpha.fw, power.ind, '\n', file=filename, append=TRUE)
  n.list <- c(n.list, n)
 
  while (min(power.ind) < lower | max(power.ind) > upper | alpha.fw > alpha.target + tol.a) {
    if (min(power.ind) < lower) {
      na <- n
      nb <- nb
    }else {
      nb <- n
      na <- na
    }
    
    n.new <- round((na + nb)/2)
    if (n.new == n) {
      cat(n, alpha.fw, power.ind, '\n', file=filename, append=TRUE)
      print.op(flag=0, n, del.cut, k, T, alpha.fw, power.ind)
      stop()
    }else if (sum(n.new == n.list) > 0) {
      n <- n.new
      power.ind <- power.fun(n.new, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
      alpha.fw <- alpha.fun(n.new, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
      cat(n, alpha.fw, power.ind, '\n', file=filename, append=TRUE)
      print.op(flag=0, n, del.cut, k, T, alpha.fw, power.ind)
      stop()
    }else {
      n <- n.new
      power.ind <- power.fun(n.new, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
      alpha.fw <- alpha.fun(n.new, del.cut, k, T, p.c, effect.size, rep, pathcode, codefile, pathout)
      cat(n, alpha.fw, power.ind, '\n', file=filename, append=TRUE) 
      n.list <- c(n.list, n.new)
    }
  }

  print.op(flag=1, n, del.cut, k, T, alpha.fw, power.ind)
}

