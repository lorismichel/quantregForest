"loss" <- 
function(y,alpha,q){
  
  q<-as.matrix(q)
  loss<-matrix(nrow=length(y),ncol=length(alpha))
  
  for(i in 1:length(y)){
    for(k in 1:length(alpha)){
      if(y[i]>q[i,k]){
        loss[i,k]<-alpha[k]*abs(y[i]-q[i,k])
      }
      else{
        loss[i,k]<-(1-alpha[k])*abs(y[i]-q[i,k])
      }
    }
  }
  return(loss)
}