adaptive.chi.square <-
function(A,B,S,alpha=0.05,bit=FALSE){
# A includes data
# B is the length of words (B-bit) that the chippered file will be divided
# S is the number of subsets where letters of an alphabet are combined
# alpha is the predetermined value of type-t error.
  

	
	N=length(A)
  M=B
  if (bit==TRUE){
	  say=1
	  i=0
	  biti=seq(0,N,B)
	  M=length(biti)
	  bitis=biti[2:M]
	  bsl=seq(1,N,B)
	  A=matrix(0,(M-1),1)
	  for (i in 1:(M-1)){
  		A[i]=sum(2 ^ (which(as.logical(rev(A[bsl[i]:bitis[i]]))) - 1)) #iki tabanindan on tabanina donusturuyor
	  }
  }
    
	
	egitimG=round((M-1)*0.5)
	egitimVeri=A[1:egitimG]
	testVeri=A[(egitimG+1):(M-1)] #test veri kumesinde yer alan elemanlari listeliyor

	freq=0
	freq2=0
	s=table(A)
	ss=sort(as.numeric(names(s)))
	sortedegitimVeri=sort(egitimVeri)
	sortedtestVeri=sort(testVeri)
	for (i in 1:length(ss)){
		freq[i]=sum((ss[i]==sortedegitimVeri)==TRUE) #A'daki i'inci elemanin egitim verideki sikligi	
		freq2[i]=sum((ss[i]==sortedtestVeri)==TRUE) #A'daki i'inci elemanin test verideki sikligi	
	}

	AA=0
	AA2=0
	for (i in 1:S){
		AA[i]=sum(freq==(i-1))
		AA2[i]=sum(freq2==(i-1))	
	}

	
  statistic=sum((AA2[which(AA2>0)]-AA[which(AA>0)])^2/AA[which(AA>0)])
	pDegeri=1-pchisq(statistic,(S-1)) #which islemi ile sifir siklikli gozeleri disarida birakiyorum.
	
	if (pDegeri<alpha){
		sonucUKK=0#H0 ret
	} else{
		sonucUKK=1
	}
	
	result=list(statistic=statistic,p.value=pDegeri,result.acsq=sonucUKK)
	return(result)
}
