#' given a  matrix of intensity counts, perform min/max norm.
#' 
#' @param data    matrix of numeric data only
#' @param percentile    numeric value 0.99 default.
#'
#' @return              normalized data, each column on [0,1] scale.
#' 
#' @export
percentilenormalize<-function(data=NULL,percentile=NULL){
   if(percentile>1){
     percentile<-percentile/100
   }
    if(is.null(percentile)==TRUE){
    minValues<-apply(data,2,min)
    maxValues<-apply(data,2,max) 
   }else{
    minValues<-apply(data,2,function(x) quantile(x,1-percentile)  )
    maxValues<-apply(data,2,function(x) quantile(x,percentile)  )
   }
    hv2<-maxValues-minValues
    dataXR<-data
    stopifnot(all( colnames(data)==names(minValues)))
    stopifnot(all(colnames(data)==names(maxValues)))
    stopifnot(all(colnames(data)==names(hv2)))
    for(j in 1:ncol(data)){
    dataXR[,j]<-(data[,j]-minValues[j])/hv2[j]
    }
    dataXR[dataXR<0]<-0
    dataXR[dataXR>1]<-1

  stopifnot(all(apply(dataXR,2,max)==1))
  stopifnot(all(apply(dataXR,2,min)==0))
   return(dataXR)
}

