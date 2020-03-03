#' a summarized experiment of IMC runs
#'
#' @slot spatial   Spatial coordinate data. x,y coordinates.
#' @name imcExperiment-class
#' @section Slots:
#' \describe{
#'   \item{\code{spatial}:}{data.frame class containing x,y coordinates}
#' }
#' @rdname imcExperiment-class
#' @export
#' @import methods
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
.imcExperiment<-setClass("imcExperiment",
	slots=representation(spatial="matrix",
				cellIntensity="matrix",
				neighborHood="matrix",
				network="matrix",
				uniqueLabel="character"),
	contains="SummarizedExperiment")


#' @export
#' @importFrom SummarizedExperiment SummarizedExperiment


.checkSpatialDimension<-function(object){
   nspatial<-nrow(object@spatial)
   nneigh<-nrow(object@neighborHood)
   nnet<-nrow(object@network)
   nlab<-length(object@uniqueLabel)
   nassay<-nrow(object)
   msg<-nassay==nspatial & nassay==nneigh & nassay==nnet & nassay==nlab
   if(msg==TRUE){
    msg<-NULL
   }else{
    msg<-"ERROR"
   }
  return(msg)
}

setValidity("imcExperiment",function(object){
    msg<-.checkSpatialDimension(object)
     if(is.null(msg)){
      TRUE 
     }else msg
 })   
