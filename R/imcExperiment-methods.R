
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
#' @aliases getSpatial imcExperiment-method
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
#' @aliases getNetwork imcExperiment-method
#' @export
setMethod("getNetwork", "imcExperiment",
          function (object) return(object@network))



setGeneric("getNetwork<-",function(object,value) standardGeneric("getNetwork<-"))

#' @rdname imcExperiment-class
#' @aliases getNetwork imcExperiment-method
#' @export
setMethod("getNetwork<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@network<-value
             return(object)
             })


#' finds the label information.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("getLabel",
           function(object) standardGeneric("getLabel"))
#' @rdname imcExperiment-class
#' @aliases getLabel imcExperiment-method
#' @export
setMethod("getLabel", "imcExperiment",
          function (object) return(object@uniqueLabel))



#' subsets the imcExperiment to a case along with all slots.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("subsetCase",
           function(object,value) standardGeneric("subsetCase"))

#' method to subset the slots, requires rowData with column "ROIID"
#' @rdname imcExperiment-class
#' @export
setMethod("subsetCase", "imcExperiment",
             function(object,value){
             id<-which(rowData(object)[,"ROIID"]==value)
             roi<-object[id,]
             roi@spatial<-object@spatial[id,]
             roi@cellIntensity<-object@cellIntensity[id,]
             roi@neighborHood<-as.matrix(object@neighborHood[id,])
             roi@network<-as.matrix(object@network[id,])
             roi@uniqueLabel<-object@uniqueLabel[id]
             return(roi)
            })




