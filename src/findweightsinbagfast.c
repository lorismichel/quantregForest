
#include <Rmath.h>
#include <R.h>


void findweightsinbagfast(double *ONv, double *OrdNv,double *filterednodes,int *index,int *newindex, int *inbag, double *WEv, int *nobs, int *ntree, double *thres, int *l){
  
  int k,i,j;
  double absol;
  int count,countf;

  for (k=1; k<=(*ntree); ++k){
  	count=0;
	countf=0;
  	for(j=1;j<=(*nobs);++j){//set the first l values of the new node vector
		if(inbag[index[j+(k-1)*(*nobs)-1]+(k-1)*(*nobs)-1]==1){
			filterednodes[1+countf+(k-1)*(*nobs)-1]=OrdNv[j+(k-1)*(*nobs)-1];
			newindex[1+countf+(k-1)*(*nobs)-1]=index[j+(k-1)*(*nobs)-1];
			countf=countf+1;
			if(countf==(*l)){
				break;
			}
		}
	}
	for (j=(*l)+1;j<=(*nobs);++j){//create a node vector with unique elements
		if(inbag[index[j+(k-1)*(*nobs)-1]+(k-1)*(*nobs)-1]==1){
			if(OrdNv[j+(k-1)*(*nobs)-1]!=filterednodes[count+1+(k-1)*(*nobs)-1]){
				filterednodes[(*l)+1+count+(k-1)*(*nobs)-1]=OrdNv[j+(k-1)*(*nobs)-1];
				newindex[(*l)+1+count+(k-1)*(*nobs)-1]=index[j+(k-1)*(*nobs)-1];
				count=count+1;
			}
		}
	}
    for (i=1; i<=(*nobs); ++i){

      if(inbag[i+(k-1)* (*nobs)-1]==0){
      
		for (j=1; j<=(*l)+count; ++j){
	  		absol = filterednodes[j+(k-1)*(*nobs)-1]-ONv[i+(k-1)*(*nobs)-1];
	  			if( (absol <= (*thres)) && (absol>= -(*thres))  ){//calculate the weights, but stop after finding enough nodes.
	    			WEv[newindex[j+(k-1)*(*nobs)-1]+(i-1)*(*nobs)-1]=WEv[newindex[j+(k-1)*(*nobs)-1]+(i-1)*(*nobs)-1]+1;
					if(filterednodes[j+1+(k-1)*(*nobs)-1]!=filterednodes[j+(k-1)*(*nobs)-1]){
						break;	
					}
	  			}
      	}
    }
  }    
} 
}
