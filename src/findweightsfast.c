
#include <Rmath.h>
#include <R.h>


void findweightsfast(double *OrdNv, double *NNv, double *filterednodes, int *index, int * newindex, double *WEv, int *nobs, int *nnew, int *ntree, double *thres,int *l){
  
  int k,i,j;
  double absol;
  int count;

  for (k=1; k<=(*ntree); ++k){
  	count=0;
	for(j=1;j<=(*l);++j){//set the first l values of the new node vector
		filterednodes[j+(k-1)*(*nobs)-1]=OrdNv[j+(k-1)*(*nobs)-1];
		newindex[j+(k-1)*(*nobs)-1]=index[j+(k-1)*(*nobs)-1];
	}
	for (j=(*l)+1;j<=(*nobs);++j){//create a node vector with unique elements
		if(OrdNv[j+(k-1)*(*nobs)-1]!=OrdNv[j-(*l)+(k-1)*(*nobs)-1]){
			filterednodes[1+(*l)+count+(k-1)*(*nobs)-1]=OrdNv[j+(k-1)*(*nobs)-1];
			newindex[1+(*l)+count+(k-1)*(*nobs)-1]=index[j+(k-1)*(*nobs)-1];
			count=count+1;
		}
	}
    for (i=1; i<=(*nnew); ++i){

      for (j=1; j<=(*l)+count; ++j){
			absol = filterednodes[j+(k-1)* (*nobs)-1] - NNv[i+(k-1)* (*nnew)-1];
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
