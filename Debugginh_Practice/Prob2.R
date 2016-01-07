require(utils)
require(microbenchmark)
#a
p = .3
phi = .5
#n = 2
sum_f <- function(p,phi,n) {
  if(n<=0) {return("n is not valid")}
  else if(n==1) {return(p^(n*phi)+(1-p)^(n*phi))}
  logf <- function(k){
  coef = lchoose(n,k)
  ret = coef+(phi-1)*(n*log(n)-k*log(k)-(n-k)*log(n-k))+
    k*phi*log(p)+(n-k)*phi*log(1-p)
  return(exp(ret))
  }
  k = seq(0,n)
  ret1 = sum((sapply(k[2:(length(k)-1)], logf)))
  ret2 = p^(n*phi)+(1-p)^(n*phi)
  return(ret1+ret2)
}
microbenchmark(sum_f(p,phi,2000))

#b
sum_f_vec <- function(p,phi,n) {
  if(n==0) {return("n is not valid")}
  else if(n==1) {return(p^(n*phi)+(1-p)^(n*phi))}
  #pass k as vector here
  logf <- function(k){
    coef = lchoose(n,k)
    ret = coef+(phi-1)*(n*log(n)-k*log(k)-(n-k)*log(n-k))+
      k*phi*log(p)+(n-k)*phi*log(1-p)
    return(exp(ret))
  }
  k = seq(0,n)
  #print(exp(logf(k[2:(length(k)-1)])))
  ret1 = sum(logf(k[2:(length(k)-1)]))
  ret2 = p^(n*phi)+(1-p)^(n*phi)
  return(ret1+ret2)
}
microbenchmark(sum_f_vec(p,phi,2000))

