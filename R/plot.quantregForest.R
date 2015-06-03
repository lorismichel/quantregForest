"plot.quantregForest" <-
function(x,all=FALSE,obs=1,...){
  
  if(all==FALSE){#don't use all observations for prediction
    plot.fast(x,obs)
  }
  else{#use all observations for prediction
    plot.all(x)
  }
}