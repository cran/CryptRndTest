GCD.test <-
function(x,KS=TRUE,CSQ=TRUE,AD=TRUE,JB=TRUE,test.k=TRUE,test.g=TRUE,mu,sd,alpha=0.05){
  # x is an N x 2 matrix 
  # test.k if TRUE, test is conducted over the GCD values and related output is generated
  # test.g if TRUE, test is conducted over the the last divisor and related output is generated
  # All the tests are conducted over k
  # Only KS and CSQ tests are conducted over g
  # mu is the expected value of theoretical distribution (Normal distribution) in KS and Chi-Sq tests
  # sd is the standard deviation of theoretical distribution (Normal distribution) in KS and Chi-Sq tests
  
  if (sum(x==0)>0){
    stop("Input includes invalid value: 0.") 
  }  
  num=as.matrix(x)
  N=nrow(num)
  y=0
  y2=0
  sig.value.k=array(NA,dim=4)
  sig.value.g=array(NA,dim=2)  
  
  for (i in 1:N){
    oklit=GCD(num[i,1],num[i,2])    
    y[i]=oklit$k
    if (oklit$g<3) {
      y2[i]=3
    }else if (oklit$g>35){
      y2[i]=35
    }else{
      y2[i]=oklit$g
    }    
  }
      
  if (test.k==TRUE){
    teorik.Normal=round(rnorm(N,mu,sd))
    if (KS==TRUE){      
      sig.value.k[1]=ks.test(y,teorik.Normal,alternative = "two.sided")$p.value      
      if (sig.value.k[1]<alpha){
        KS.result.k=0#H0 ret
      }else {
        KS.result.k=1
      }
    }
    if (CSQ==TRUE){
      testCSQ=chisq.test(y, teorik.Normal, correct = FALSE)
      sig.value.k[2]=testCSQ$p.value
      if (sig.value.k[2]<alpha){
        CSQ.result.k=0#H0 ret
      }else {
        CSQ.result.k=1
      }
    }
    if (JB==TRUE){
      testJB=  jarque.bera.test(as.matrix((y-mean(y))/sd(y)))# jarqueberaTest((y-mean(y))/sd(y)) jarque.bera.test((y-mean(y))/sqrt(var(y)))
      sig.value.k[3]=testJB$p.value
      if (sig.value.k[3]<alpha){
        JB.result.k=0#H0 ret
      }else {
        JB.result.k=1
      }
    }
    if (AD==TRUE){
      testAD=AndersonDarling(c(y,teorik.Normal),number.cases=c(length(y),length(teorik.Normal)))
      sig.value.k[4]=testAD$pn
      if (sig.value.k[4]<alpha){
        AD.result.k=0#H0 ret
      }else {
        AD.result.k=1
      }
    }
  }
  
  if (test.g==TRUE){
    kuramsalDF=0
    teo=0
    teorik=0
    rasgele=runif(N,0,1)
    bsl=1
    i=0
    while ((sum(teo)<N)){
      i=i+1      
      kuramsalDF[i]=0
      j=1:i
      kuramsalDF[i]=sum(6/((pi*j)^2))
      if (kuramsalDF[i]==0){
        kuramsalDF[i]=1
      }
      if (i==1){
        teo[i]=sum(rasgele<=kuramsalDF[i])
        teorik[bsl:(bsl+teo[i]-1)]=3
        bsl=bsl+teo[i]
      }else{
        teo[i]=sum(rasgele<=kuramsalDF[i])-sum(teo[1:i-1])
        if (i>35){          
          teorik[bsl:(bsl+teo[i]-1)]=35
          bsl=bsl+teo[i]
        }else if(i<3){          
          teorik[bsl:(bsl+teo[i]-1)]=3   
          bsl=bsl+teo[i]
        }else{         
          teorik[bsl:(bsl+teo[i]-1)]=i
          bsl=bsl+teo[i]
        }       
      } 
    }
    
    if (KS==TRUE){      
      sig.value.g[1]=ks.test(y2,teorik,alternative = "two.sided")$p.value      
      if (sig.value.g[1]<alpha){
        KS.result.g=0#H0 ret
      }else {
        KS.result.g=1
      }
    }
    if (CSQ==TRUE){      
      test2=chisq.test(y2, teorik, correct = FALSE)
      sig.value.g[2]=test2$p.value
      if (sig.value.g[2]<alpha){
        CSQ.result.g=0#H0 ret
      }else {
        CSQ.result.g=1
      }
    }
  }
  
  if ((test.g==TRUE) & (test.k==TRUE)) {
    result=list(sig.value.k=sig.value.k,sig.value.g=sig.value.g,KS.result.k=KS.result.k,CSQ.result.k=CSQ.result.k,
                AD.result.k=AD.result.k,JB.result.k=JB.result.k,KS.result.g=KS.result.g,CSQ.result.g=CSQ.result.g)
  }else if ((test.g==TRUE) & (test.k==FALSE)) {
    result=list(sig.value.g=sig.value.g,KS.result.g=KS.result.g,CSQ.result.g=CSQ.result.g)
  }else if ((test.g==FALSE) & (test.k==TRUE)) {
    result=list(sig.value.k=sig.value.k,KS.result.k=KS.result.k,CSQ.result.k=CSQ.result.k,AD.result.k=AD.result.k,
                JB.result.k=JB.result.k)
  }
  return(result)
}
