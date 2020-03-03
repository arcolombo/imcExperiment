
setGeneric("cellIntensity",
           function(object) standardGeneric("cellIntensity"))

setGeneric("cellIntensity<-",
           function(object,value) standardGeneric("cellIntensity<-"))

#' @title cell intensity per cell
#' @description returns the expression of IMC data
#'
#' @rdname imcExperiment-class
#' @export
setMethod("cellIntensity", "imcExperiment",
          function (object) return(assays(object)$exprs))

setReplaceMethod("cellIntensity", c("imcExperiment", "matrix"),
                 function (object, value) {
                   assays(object)$exprs <- value
                   return(object)
                 })
#' @title cell intensity accessor
#' @description returns the expression of IMC data
#'
#' @rdname imcExperiment-class
#' @export
setMethod("cellIntensity<-",c("imcExperiment","matrix"),
           function(object,value){
 	assays(object)$exprs<-value
	return(object)    	 
   })


#' @title Phenotypic data
#' @description returns the phenotypic data associated with a imcExperiment object
#' 
#' @rdname imcExperiment-class
#' @export
setMethod("pData", "imcExperiment",
          function (object) return(colData(object)))





setReplaceMethod("pData", c("imcExperiment", "DataFrame"),
                 function (object, value) {
                   colData(object) <- value
                   return(object)
                 })

setGeneric("pData<-",function(object,value) standardGeneric("pData<-"))
setMethod("pData<-",c("imcExperiment", "DataFrame"),
            function(object,value){
             colData(object)<-value
             return(object)
             })




#' finds the spatial coords.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("getSpatial", 
           function(object) standardGeneric("getSpatial"))
#' @rdname imcExperiment-class
#' @aliases spatial imcExperiment-method
#' @export
setMethod("getSpatial", "imcExperiment",
          function (object) return(object@spatial))



setGeneric("getSpatial<-",function(object,value) standardGeneric("getSpatial<-"))

#' @rdname imcExperiment-class
#' @aliases spatial imcExperiment-method
#' @export
setMethod("getSpatial<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@spatial<-value
             return(object)
             })




#' finds the network information.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("getNetwork", 
           function(object) standardGeneric("getNetwork"))
#' @rdname imcExperiment-class
#' @aliases spatial imcExperiment-method
#' @export
setMethod("getNetwork", "imcExperiment",
          function (object) return(object@network))



setGeneric("getNetwork<-",function(object,value) standardGeneric("getNetwork<-"))

#' @rdname imcExperiment-class
#' @aliases spatial imcExperiment-method
#' @export
setMethod("getNetwork<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@network<-value
             return(object)
             })



