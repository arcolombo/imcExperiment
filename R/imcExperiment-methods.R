
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

#' sets cell Intensity slot to a new matrix. rows protein, columns are cells.
#' @export
setGeneric("cellIntensity<-",function(object,value) standardGeneric("cellIntensity<-"))

#' @rdname imcExperiment-class
#' @param object IMC container
#' @param value matrix rows protein, columns are cells
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

#' Sets the coordinate positions of each cell (matrix), columns are X,Y positions.
#' @param object is IMC container
#' @param value matrix rows cells, columns are x,y
#' @export
setGeneric("getCoordinates<-",function(object,value) standardGeneric("getCoordinates<-"))

#' @rdname imcExperiment-class
#' @param object is IMC container
#' @param value matrix rows cells, columns are x,y
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

#' a slot for the histoCAT neighborhood data (matrix) columns are the neighbors
#' @rdname imcExperiment-class
#' @param object imcExperiment container
#' @aliases getSpatial imcExperiment-method
#' @export
setMethod("getNeighborhood", "imcExperiment",
          function (object) return(object@neighborHood))


#' slow assignment for the histoCAT neighborhood data (matrix) columns are the neighbors
#' @param object is IMC container
#' @param value matrix rows cells, columns are neighborhood histocat output
setGeneric("getNeighborhood<-",function(object,value) standardGeneric("getNeighborhood<-"))

#' @rdname imcExperiment-class
#' @param object is IMC container
#' @param value matrix rows cells, columns are neighborhood histoCAT output
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
#' assigns cell cluster assignment to the container. rows are cells and column is the cluster ID
#' @rdname imcExperiment-class
#' @param object IMC container
#' @aliases getNetwork imcExperiment-method
#' @export
setMethod("getNetwork", "imcExperiment",
          function (object) return(object@network))


#' re-assigns the network assignment (matrix)
#' @param object is IMC container
#' @param value matrix rows cells, columns are phenograph network ID
#' @export
setGeneric("getNetwork<-",function(object,value) standardGeneric("getNetwork<-"))

#' @rdname imcExperiment-class
#' @param object is IMC container
#' @param value matrix rows cells, columns are phenotype cluster ID
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
#'
#' distance matrix can be stored in the distance slot for pairwise distance
#' @rdname imcExperiment-class
#' @aliases getDistance imcExperiment-method
#' @export
setMethod("getDistance", "imcExperiment",
          function (object) return(object@distance))

#' re-assigns the distance matrix (rows are cells)
#' @param object is IMC container
#' @param value matrix rows cells, columns are distance measurements
#' @export
setGeneric("getDistance<-",function(object,value) standardGeneric("getDistance<-"))

#' @rdname imcExperiment-class
#' @aliases getDistance imcExperiment-method
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
#' morphological features can be stored (matrix) rows are cells and columns are Area, etc.
#' @rdname imcExperiment-class
#' @param object IMC container
#' @aliases getMorphology imcExperiment-method
#' @export
setMethod("getMorphology", "imcExperiment",
          function (object) return(object@morphology))

#' re-assigns morphological features can be stored (matrix) rows are cells and columns are Area, etc.
#' @param object is IMC container
#' @param value matrix rows cells, columns are Area, Eccentricity, etc.
#' @export
setGeneric("getMorphology<-",function(object,value) standardGeneric("getMorphology<-"))

#' @rdname imcExperiment-class
#' @param object is IMC container
#' @param value matrix rows cells, columns are Area, etc.
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
#' unique cell labels can be assigned (vector)
#' @rdname imcExperiment-class
#' @aliases getLabel imcExperiment-method
#' @export
setMethod("getLabel", "imcExperiment",
          function (object) return(object@uniqueLabel))





#' subsets the imcExperiment to a case along with all slots for a single ROI, using for distance analysis
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @param value this is ROIID a single character ID
#' @export
setGeneric("subsetCase",
           function(object,value) standardGeneric("subsetCase"))

#' method to subset the slots, requires colData with column "ROIID"
#' @rdname imcExperiment-class
#' @param object IMC container
#' @param value this is ROIID a single character ID
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




#' subsets the imcExperiment to a case along with all slots for a selected multiple ROIs.
#' @name imcExperiment-class
#'
#' @rdname imcExperiment-class
#' @param object imcExperiment
#' @param value vector of ROIID
#' @export
setGeneric("selectCases",
           function(object,value) standardGeneric("selectCases"))

#' method to subset the slots, requires colData with column "ROIID"
#' @rdname imcExperiment-class
#' @param object IMC container
#' @param value this is ROIID vector
#' @export
setMethod("selectCases", "imcExperiment",
          function(object,value){
            id<-which(colData(object)[,"ROIID"]%in%value)
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



