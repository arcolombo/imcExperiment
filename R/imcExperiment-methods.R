
#' finds the intensities getter.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("cellIntensity",
           function(object,...) standardGeneric("cellIntensity"))

#' @rdname imcExperiment-class
#' @aliases cellIntensity imcExperiment-method
#' @export
setMethod("cellIntensity", "imcExperiment",
          function (object) return(object@cellIntensity))

#' @export
setGeneric("cellIntensity<-",function(object,value) standardGeneric("cellIntensity<-"))

#' @rdname imcExperiment-class
#' @aliases cellIntensity imcExperiment-method
#' @export
setMethod("cellIntensity<-",c("imcExperiment", "matrix"),
          function(object,value){
            object@cellIntensity<-value
            return(object)
          })




#' finds the spatial coords, getter.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("getCoordinates",
           function(object) standardGeneric("getCoordinates"))

#' @rdname imcExperiment-class
#' @aliases getCoordinates imcExperiment-method
#' @export
setMethod("getCoordinates", "imcExperiment",
          function (object) return(object@coordinates))


#' @export
setGeneric("getCoordinates<-",function(object,value) standardGeneric("getCoordinates<-"))

#' @rdname imcExperiment-class
#' @aliases getCoordinates imcExperiment-method
#' @export
setMethod("getCoordinates<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@coordinates<-value
             return(object)
             })


#' finds the neighborhood information.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("getNeighborhood",
           function(object) standardGeneric("getNeighborhood"))
#' @rdname imcExperiment-class
#' @aliases getSpatial imcExperiment-method
#' @export
setMethod("getNeighborhood", "imcExperiment",
          function (object) return(object@neighborHood))



setGeneric("getNeighborhood<-",function(object,value) standardGeneric("getNeighborhood<-"))

#' @rdname imcExperiment-class
#' @aliases spatial imcExperiment-method
#' @export
setMethod("getNeighborhood<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@neighborHood<-value
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


#' @export
setGeneric("getNetwork<-",function(object,value) standardGeneric("getNetwork<-"))

#' @rdname imcExperiment-class
#' @aliases getNetwork imcExperiment-method
#' @export
setMethod("getNetwork<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@network<-value
             return(object)
             })




#' finds the distance information.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("getDistance",
           function(object) standardGeneric("getDistance"))
#' @rdname imcExperiment-class
#' @aliases getSpatial imcExperiment-method
#' @export
setMethod("getDistance", "imcExperiment",
          function (object) return(object@distance))


#' @export
setGeneric("getDistance<-",function(object,value) standardGeneric("getDistance<-"))

#' @rdname imcExperiment-class
#' @aliases spatial imcExperiment-method
#' @export
setMethod("getDistance<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@distance<-value
             return(object)
             })



#' finds the morphology information.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @export
setGeneric("getMorphology",
           function(object) standardGeneric("getMorphology"))
#' @rdname imcExperiment-class
#' @aliases getSpatial imcExperiment-method
#' @export
setMethod("getMorphology", "imcExperiment",
          function (object) return(object@morphology))


#' @export
setGeneric("getMorphology<-",function(object,value) standardGeneric("getMorphology<-"))

#' @rdname imcExperiment-class
#' @aliases spatial imcExperiment-method
#' @export
setMethod("getMorphology<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@morphology<-value
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

#' method to subset the slots, requires colData with column "ROIID"
#' @rdname imcExperiment-class
#' @export
setMethod("subsetCase", "imcExperiment",
             function(object,value){
             id<-which(colData(object)[,"ROIID"]==value)
             roi<-object[,id]
             roi@coordinates<-object@coordinates[id,]
             roi@cellIntensity<-object@cellIntensity[,id]
             roi@neighborHood<-as.matrix(object@neighborHood[id,])
             roi@network<-as.matrix(object@network[id,])
             roi@distance<-as.matrix(object@distance[id,])
             roi@morphology<-as.matrix(object@morphology[id,])
             roi@uniqueLabel<-object@uniqueLabel[id]
             return(roi)
            })




