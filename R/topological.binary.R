topological.binary <-
function(x,B,alpha=0.05,critical.value){
# B represents the non-overlapping block length or pattern size
# alpha is the predetermined value of type-t error.  
# critical.value is the value used to decide rejection of H_0
  
  n=length(x)
  k=ceiling(n/B) # k is the number of blocks  
  # find non-overlapping blocks of lenth m
  blocks=array(NA,dim=c(k,B))
  blocks=matrix(unlist(split(x,k)),ncol=B,byrow = TRUE)  
  # determine the number of different m-bit patterns that appear across all the k blocks
  num.different=nrow(unique(blocks))
  if (num.different<1){
    sonucTBT=0 #H0 RET
  }else if (num.different>=min(k,2^B)){
    sonucTBT=1 #H0 REDDEDILEMEZ
  }else if (num.different<critical.value){
      sonucTBT=0 #H0 RET
  }else{
      sonucTBT=1 #H0 REDDEDILEMEZ
  }
  result=list(result.TBT=sonucTBT,statistic=num.different)
  return(result)  
}
