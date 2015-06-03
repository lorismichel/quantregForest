"myorder" <-
function(x){
  
  z<-order(x,rnorm(length(x)))
  return(z)

}
