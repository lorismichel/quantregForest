"predict.quantregForest"<-function(object, newdata=NULL,quantiles=c(0.1,0.5,0.9),all=FALSE,obs=1,normalise=TRUE,...)
{
  #check if all is logical
  if(!is.logical(all)){
    stop("all has to be logical")
  }
  
  if(all==FALSE){#don't use all observations for prediction
    #check if obs is integer
    if(!{obs%%1==0}){
      stop("obs has to be an integer")
    }
    
    predict.fast(object,newdata,quantiles,obs)
  }
  else{#use all observations for prediction
    predict.all(object,newdata,quantiles,normalise=if(normalise) 1 else 0)
  }
}
