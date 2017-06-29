print.op <-
function(flag, n, del.cut, k, T, alpha.fw, power.ind) {
  colname <- c('del.cut', 'T', 'n', 'alpha.fw')
  for (i in 1:k) colname <- c(colname, paste('power.ind', i, sep=''))

  if (flag==1) {
    print("-------------------------------------");
    print("Sample Size Found!");
    print("-------------------------------------");
    print("Operating Characteristics associated with final sample size:");
    cat(colname, '\n');
    print(c(del.cut, T, n, alpha.fw, power.ind));
  }else {
    print("-------------------------------------");
    print("Sample Size NOT Found!");
    print("-------------------------------------");
    print("Operating Characteristics associated with n:");
    cat(colname, '\n');
    print(c(del.cut, T, n, alpha.fw, power.ind));
  }
}

