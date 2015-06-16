Random.walk.D <-
function(a,B){
#a dizisi binary bir dizi
#N a dizisinin uzunlu?u
#B, alinan b?l?nd??? alt dizilerin uzunlu?u
	N=length(a)
	b=array(0,N)
	S=0
  b=1-2*a

  e=array(0,floor(N/B))  
  for (j in 1:floor(N/B)){		
		for (i in 1:B){
      k=1:i
			S[i]=sum(b[(j-1)*B+k])
		}
    e[j]=e[j]+sum(S==0)#length(which(S==0))
	}
	
	result=list(e=e)
	return(result)
}
