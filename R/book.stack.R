book.stack=function(A,B,k=2,alpha=0.05,bit=FALSE){
  
  if (bit==TRUE){
    x=A
	  N=length(x)	
	  say=1
	  i=0
  	biti=seq(0,N,B)
	  M=length(biti)
  	bitis=biti[2:M]
	  bsl=seq(1,N,B)
	  A=matrix(0,(M-1),1)
	  for (i in 1:(M-1)){
		  A[i]=sum(2 ^ (which(as.logical(rev(x[bsl[i]:bitis[i]]))) - 1))
      }
  }
  
	alfabe=sum(2 ^ (which(as.logical(rev(matrix(0,B,1)))) - 1)):sum(2 ^ (which(as.logical(rev(matrix(1,B,1)))) - 1)) 
	M=length(alfabe)

	AA=matrix(0,k,round(M/k))
    boluntule=split(alfabe, ceiling(seq_along(alfabe)/round(M/k)))
	for (i in 1:k){
	  AA[i,]=boluntule[[i]]
	}
  	
  nn=length(A)
  nu=array(NA,dim=c(nn,(M+1)))
  nu[1,]=0:M
  n=array(0,k)
  for (i in 2:nn){
    nu[i,1]=A[i-1]
    AB=nu[(i-1),]
    nu[i,2:(M+1)]= AB[which(AB!=A[i-1])]  
    for (j in 1:k){
      if (sum(AA[j,]==which(nu[(i-1),]==A[(i-1)]))==1){
        n[j]=n[j]+1
      }
    }
  }
  for (j in 1:k){
    if (sum(AA[j,]==which(nu[nn,]==A[nn]))==1){
      n[j]=n[j]+1
    }
  }
	
	kiKare=sum((n-(nn/k))^2/(nn/k))
	pDegeri=1-pchisq(kiKare,(k-1))
  
	if (pDegeri<alpha){
		sonucKR=0
	} else{
		sonucKR=1
	}  
  
	result=list(statistic=kiKare,p.value=pDegeri,BS.result=sonucKR)
	return(result)
}