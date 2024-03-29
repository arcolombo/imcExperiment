
#' finds the intensities getter.
#' @name cellIntensity
#'
#' @rdname cellIntensity
#' @param object imcExperiment
#' @param ... additional arguments
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' dim(cellIntensity(imcdata))
setGeneric("cellIntensity",
           function(object,...) standardGeneric("cellIntensity"))

#' @rdname cellIntensity
#' @param object imcExperiment
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata);dim(cellIntensity(imcdata))
#' head(t(cellIntensity(imcdata)))
setMethod("cellIntensity", "imcExperiment",
          function (object) return(object@cellIntensity))

#' sets cell Intensity slot to a new matrix. rows protein, columns are cells.
#' @rdname cellIntensity
#' @param object imc container
#' @param value matrix rows protein, column are cells
#' @export
#' @return imcExperiment container
setGeneric("cellIntensity<-",function(object,value) standardGeneric("cellIntensity<-"))

#' @rdname cellIntensity
#' @param object IMC container
#' @param value matrix rows protein, columns are cells
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-asinh(counts(imcdata))
#' cellIntensity(imcdata)<-x
setMethod("cellIntensity<-",c("imcExperiment", "matrix"),
          function(object,value){
            object@cellIntensity<-value
            return(object)
          })




#' finds the spatial coords, getter.
#' @name getCoordinates
#'
#' @rdname getCoordinates
#' @param object imcExperiment
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getCoordinates(imcdata)
setGeneric("getCoordinates",
           function(object) standardGeneric("getCoordinates"))

#' @rdname getCoordinates
#' @param object imcExperiment
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getCoordinates(imcdata)
setMethod("getCoordinates", "imcExperiment",
          function (object) return(object@coordinates))

#' Sets the coordinate positions of each cell (matrix), columns are X,Y positions.
#' @param object is IMC container
#' @param value matrix rows cells, columns are x,y
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-getCoordinates(imcdata)
#' getCoordinates(imcdata)<-as.matrix(x)
setGeneric("getCoordinates<-",function(object,value) standardGeneric("getCoordinates<-"))

#' @rdname getCoordinates
#' @param object is IMC container
#' @param value matrix rows cells, columns are x,y
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-getCoordinates(imcdata)
#' getCoordinates(imcdata)<-as.matrix(x)
setMethod("getCoordinates<-",c("imcExperiment", "matrix"),
            function(object,value){
             object@coordinates<-value
             return(object)
             })


#' finds the neighborhood information.
#' @name getNeighborhood
#'
#' @rdname getNeighborhood
#' @param object imcExperiment
#' @param ... additional arguments
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getNeighborhood(imcdata)
setGeneric("getNeighborhood",
           function(object,...) standardGeneric("getNeighborhood"))

#' @rdname getNeighborhood
#' @param object imcExperiment container
#' @export
#' @return imcExperiment container
#' data(imcdata)
#' getNeighborhood(imcdata)
setMethod("getNeighborhood", "imcExperiment",
          function (object) return(object@neighborHood))


#' slow assignment for the histoCAT neighborhood data (matrix) columns are the neighbors
#' @rdname getNeighborhood
#' @param object is IMC container
#' @param value matrix rows cells, columns are neighborhood histocat output
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-matrix(1,nrow=ncol(imcdata),ncol=2)
#' getNeighborhood(imcdata)<-x
setGeneric("getNeighborhood<-",function(object,value) standardGeneric("getNeighborhood<-"))

#' @rdname getNeighborhood
#' @param object is IMC container
#' @param value matrix rows cells, columns are neighborhood histoCAT output
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-matrix(1,nrow=ncol(imcdata),ncol=2)
#' getNeighborhood(imcdata)<-x
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
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getNetwork(imcdata)
setGeneric("getNetwork",
           function(object) standardGeneric("getNetwork"))
#' assigns cell cluster assignment to the container. rows are cells and column is the cluster ID
#' @rdname imcExperiment-class
#' @param object IMC container
#' @aliases getNetwork imcExperiment-method
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getNetwork(imcdata)
setMethod("getNetwork", "imcExperiment",
          function (object) return(object@network))


#' re-assigns the network assignment (matrix)
#' @param object is IMC container
#' @param value data.frame rows cells, columns are phenograph network ID
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-data.frame(ID=seq_len(ncol(imcdata)))
#' getNetwork(imcdata)<-x
setGeneric("getNetwork<-",function(object,value) standardGeneric("getNetwork<-"))

#' @rdname imcExperiment-class
#' @param object is IMC container
#' @param value matrix rows cells, columns are phenotype cluster ID
#' @aliases getNetwork imcExperiment-method
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-data.frame(ID=seq_len(ncol(imcdata)))
#' getNetwork(imcdata)<-x
setMethod("getNetwork<-",c("imcExperiment", "data.frame"),
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
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getDistance(imcdata)
setGeneric("getDistance",
           function(object) standardGeneric("getDistance"))
#'
#' distance matrix can be stored in the distance slot for pairwise distance
#' @rdname imcExperiment-class
#' @aliases getDistance imcExperiment-method
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getDistance(imcdata)
setMethod("getDistance", "imcExperiment",
          function (object) return(object@distance))

#' re-assigns the distance matrix (rows are cells)
#' @param object is IMC container
#' @param value matrix rows cells, columns are distance measurements
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' newD<-matrix(1,nrow=ncol(imcdata),ncol=1)
#' getDistance(imcdata)<-newD
setGeneric("getDistance<-",function(object,value) standardGeneric("getDistance<-"))

#' @rdname imcExperiment-class
#' @aliases getDistance imcExperiment-method
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' newD<-matrix(1,nrow=ncol(imcdata),ncol=1)
#' getDistance(imcdata)<-newD
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
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getMorphology(imcdata)
setGeneric("getMorphology",
           function(object) standardGeneric("getMorphology"))
#' morphological features can be stored (matrix) rows are cells and columns are Area, etc.
#' @rdname imcExperiment-class
#' @param object IMC container
#' @aliases getMorphology imcExperiment-method
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getMorphology(imcdata)
setMethod("getMorphology", "imcExperiment",
          function (object) return(object@morphology))

#' re-assigns morphological features can be stored (matrix) rows are cells and columns are Area, etc.
#' @param object is IMC container
#' @param value matrix rows cells, columns are Area, Eccentricity, etc.
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-matrix(1,nrow=ncol(imcdata),ncol=4)
#' getMorphology(imcdata)<-x
setGeneric("getMorphology<-",function(object,value) standardGeneric("getMorphology<-"))

#' @rdname imcExperiment-class
#' @param object is IMC container
#' @param value matrix rows cells, columns are Area, etc.
#' @aliases spatial imcExperiment-method
#' @export
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' x<-matrix(1,nrow=ncol(imcdata),ncol=4)
#' getMorphology(imcdata)<-x
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
#' @return imcExperiment container
#' @examples
#' data(imcdata)
#' getLabel(imcdata)
setGeneric("getLabel",
           function(object) standardGeneric("getLabel"))
#' unique cell labels can be assigned (vector)
#' @rdname imcExperiment-class
#' @aliases getLabel imcExperiment-method
#' @export
#' @examples
#' data(imcdata)
#' getLabel(imcdata)
setMethod("getLabel", "imcExperiment",
          function (object) return(object@uniqueLabel))





#' subsets the imcExperiment to a case along with all slots for a single ROI, using for distance analysis
#' @name subsetCase
#' @rdname subsetCase
#' @param object imcExperiment
#' @param value this is ROIID a single character ID
#' @param ... additional parameters
#' @return returns IMC object of a single case
#' @export
#' @examples
#' data(imcdata)
#' myCase<-subsetCase(imcdata,"30-BM16-202_7Pre_s1_p1_r4_a4_ac")
#' myCase
setGeneric("subsetCase",
           function(object,value,...) standardGeneric("subsetCase"))

#' method to subset the slots, requires colData with column "ROIID"
#' @rdname subsetCase
#' @param object IMC container
#' @param value this is ROIID a single character ID
#' @return roi  imcExperiment
#' @export
#' @examples
#' data(imcdata)
#' myCase<-subsetCase(imcdata,"30-BM16-202_7Pre_s1_p1_r4_a4_ac")
#' myCase
setMethod("subsetCase", "imcExperiment",
             function(object,value){
             id<-which(colData(object)[,"ROIID"]==value)
             roi<-object[,id]
             roi@coordinates<-object@coordinates[id,]
             roi@cellIntensity<-object@cellIntensity[,id]
             roi@neighborHood<-as.matrix(object@neighborHood[id,])
             roi@network<-as.data.frame(object@network[id,])
             roi@distance<-as.matrix(object@distance[id,])
             roi@morphology<-as.matrix(object@morphology[id,])
             roi@uniqueLabel<-object@uniqueLabel[id]
             return(roi)
            })




#' subsets the imcExperiment to a case along with all slots for a selected multiple ROIs.
#' @name selectCases
#'
#' @rdname selectCases
#' @param object imcExperiment
#' @param value vector of ROIID
#' @param ... additional parameters
#' @export
#' @return imcExperiment container of selected cases
#' @examples
#' data(imcdata)
#' myCases<-selectCases(imcdata,c("30-BM16-202_7Pre_s1_p1_r4_a4_ac","B17_350_14post_s1_p1_r5_a5_ac"))
#' myCases
#' table(colData(myCases)$ROIID)
setGeneric("selectCases",
           function(object,value,...) standardGeneric("selectCases"))

#' method to subset the slots, requires colData with column "ROIID"
#' @rdname selectCases
#' @param object IMC container
#' @param value this is ROIID vector
#' @export
#' @return imcExperiment container of selected cases
#' @examples
#' data(imcdata)
#' myCases<-selectCases(imcdata,c("30-BM16-202_7Pre_s1_p1_r4_a4_ac","B17_350_14post_s1_p1_r5_a5_ac"))
#' myCases
#' table(colData(myCases)$ROIID)
setMethod("selectCases", "imcExperiment",
          function(object,value){
            id<-which(colData(object)[,"ROIID"]%in%value)
            roi<-object[,id]
            roi@coordinates<-object@coordinates[id,]
            roi@cellIntensity<-object@cellIntensity[,id]
            roi@neighborHood<-as.matrix(object@neighborHood[id,])
            roi@network<-as.data.frame(object@network[id,])
            roi@distance<-as.matrix(object@distance[id,])
            roi@morphology<-as.matrix(object@morphology[id,])
            roi@uniqueLabel<-object@uniqueLabel[id]
            return(roi)
          })



