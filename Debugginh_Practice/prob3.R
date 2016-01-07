load("mixedMember.Rda")
require(compiler)
require(microbenchmark)
#a
sum_a_ret = sapply(1:100000,sum_a<-function(i) sum(wgtsA[[i]]*muA[IDsA[[i]]]))
sum_b_ret = sapply(1:100000,sum_b<-function(i) sum(wgtsB[[i]]*muB[IDsB[[i]]]))
#b
transfer_matrix<-function(list){
  n=length(list)
  max_len = max(sapply(list,length))
  max_num = max(sapply(list,max))
  ##
  for (i in 1:n){
    if (length(list[[i]])<max_len)
      list[[i]]=c(list[[i]],rep(max_num+1,max_len-length(list[[i]])))
  }
  mat =array(unlist(list),c(max_len,length(list)))
  return(mat)
}

#transfer_matrix(list)
##change the data object to store IDdata and weight data
IDsA_mat = transfer_matrix(IDsA)
wgtsA_mat = transfer_matrix(wgtsA) ##observe some of the value is larger than one
##those are invalid data and we can just leave it there
muA_mat = c(muA,0)
IDsB_mat = transfer_matrix(IDsB)
wgtsB_mat = transfer_matrix(wgtsB) ##observe some of the value is larger than one
##those are invalid data and we can just leave it there
muB_mat = c(muA,0)
##test the time of them
microbenchmark(sapply(1:100000,
                      sum_a<-function(i) sum(wgtsA[[i]]*muA[IDsA[[i]]]))
               ,colSums(wgtsA_mat*muA_mat[IDsA_mat]))

#It is not enough to improve the case B, let's finish it in a more genius way.

#c
weight_convert<-matrix(0,nr=length(wgtsB),nc=length(muB))
for (i in 1:nrow(weight_convert)){
  weight_convert[i,IDsB[[i]]]<-wgtsB[[i]]
}
microbenchmark(
  ret<-as.vector(weight_convert%*%muB),
      sapply(1:100000,sum_b<-function(i) sum(wgtsB[[i]]*muB[IDsB[[i]]])))










