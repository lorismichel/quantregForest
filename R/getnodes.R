"getnodes" <-
function(qrf,x){
  nodes <- matrix(nrow=nrow(x),ncol=qrf$ntree)
  nodes <- randomForest:::predict.randomForest(qrf,newdata=x,predict.all=TRUE)$individual  
  return(nodes)
}

