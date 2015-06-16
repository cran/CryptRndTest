KSADdga <-
function(e,alfa,n,m,lambda,num.class=10){
# m is the number  of birthdays
# n is the length of year
# num.class is the number of groups of the frequency table over which the goodness of tests are applied. 
#  It is equal to the value of the class that will include the rest of the observations like that ">=10"
#	n=2^24
#	m=1024
	z=0
	expected=0
  p=0
  p[1:(num.class+1)]=dpois(0:num.class,lambda=lambda) # classes corresponding to 0,1...,10
  p[num.class+1]=p[num.class+1]+sum(dpois((num.class+1):1000,lambda=lambda)) #those correspoding to 10,11,...
	N=length(e)
	expected=round(p*N)
	if (sum(expected)!=N){
		expected[which.max(expected)]=expected[which.max(expected)]-(sum(expected)-N)#buyukse cikartir kucukse ekler
	}
	z=rep(0:(length(expected)-1),expected) #ki-kare disindaki testleri yapmak icin expectedin icindeki kadar sifir, bir vs yaziyor
	e=sort(e)
	test=kSamples::ad.test(e,z,method="simulated",dist=FALSE,Nsim=1000)
	ADtest=test$ad
	if (ADtest[1,4]<alfa){ #burada 4'uncu eleman simulated.p.val, 3'uncu eleman exact p.val ancak in the presence of ties exact p.val cannot be computed
		sonucAD=0 #H0 ret
	} else{ 
		sonucAD=1
	}
  
	KStest=0
	test2=ks.test(e,z)
	KStest[1]=test2$statistic
	KStest[2]=test2$p.value
	if (KStest[2]<alfa){ 
		sonucKS=0 #H0 ret
	} else{ 
		sonucKS=1
	}
  
  k=length(expected)
  observed=array(0,k)
  observed[1:length(table(e))]=table(e)
  KKtest=0 
  
  KKtest[1]=sum(((observed-expected)^2)/expected,na.rm=TRUE) #gives test statistic
  KKtest[2]=pchisq(KKtest[1],(k-1)) #give p.value of the chi-square test  
	if (KKtest[2]<alfa){ 
  		sonucKK=0 #H0 ret
	} else{ 
		  sonucKK=1
	}
 	result=list(sonucAD=sonucAD,ADtest=ADtest,sonucKS=sonucKS,KStest=KStest,sonucKK=sonucKK,KKtest=KKtest)
	return(result)

}
