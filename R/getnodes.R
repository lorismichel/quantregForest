"getnodes" <-
function(qrf,x){
  nodes <- matrix(nrow=nrow(x),ncol=qrf$ntree)
  class(qrf)<-"randomForest"
  nodes <- predict(qrf,newdata=x,predict.all=TRUE)$individual  
  return(nodes)
}

