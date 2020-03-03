#' Initializes a imcExperiment and performs some rudimentary checks.
#' Many of the arguments CAN be NULL; determination of which is required 
#' is done at run-time.  A imcExperiment must contain at least the 
#' expressions and spatial assays.  
#'
#' FIXME: add Harold's quasi-normalized TPM method for tx-level comparisons.
#' 
#' @param exprs_intensity            matrix of counts
#' @param spatial               coordinate data 
#' @param covariates            the column metadata (covariates) for each sample
#' @export 
imcExperiment<-function(
        spatial=matrix(1,3,3),
	cellIntensity=matrix(1,3,3),
	neighborHood=matrix(1,3,3),
	network=matrix(1,3,3),
	uniqueLabel=rep("A",3),
	...){
   se<-SummarizedExperiment(list(exprs=cellIntensity))
   .imcExperiment(se,
	spatial=spatial,
	neighborHood=neighborHood,
	cellIntensity=cellIntensity,
	network=network,
	uniqueLabel=uniqueLabel)
 

  #new("imcExperiment",
   #     SummarizedExperiment(
    #      assays=asys,
     #     colData=covariates
     #   ),
     #   spatial=spatial)

}

