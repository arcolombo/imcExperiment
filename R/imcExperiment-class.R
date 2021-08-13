#' a summarized experiment of IMC runs, dimensions of the spatial and intensity data are regulated.#'
#' @slot coordinates matrix class containing x,y coordinates
#' @slot cellIntensity matrix class containing intensity
#' @slot neighborHood matrix class containing x,y neighbor
#' @slot network data frame class containing network
#' @slot distance matrix class containing x,y distances
#' @slot morphology matrix class containing morphology
#' @slot uniqueLabel labels
#' @name imcExperiment-class
#' @rdname imcExperiment-class
#' @export
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @examples
#' x<-imcExperiment(cellIntensity=matrix(1,nrow=10,ncol=10),
#' coordinates=matrix(1,nrow=10,ncol=2),
#' neighborHood=matrix(1,nrow=10,ncol=10),
#' network=data.frame(matrix(1,nrow=10,ncol=10)),
#' distance=matrix(1,nrow=10,ncol=10),
#' morphology=matrix(1,nrow=10,ncol=10),
#' uniqueLabel=paste0("A",seq_len(10)),
#' panel=letters[1:10],
#' ROIID=data.frame(ROIID=rep("A",10)))

.imcExperiment<-setClass("imcExperiment",
	slots=representation(coordinates="matrix",
				cellIntensity="matrix",
				neighborHood="matrix",
				network="data.frame",
				distance='matrix',
				morphology='matrix',
				uniqueLabel="character"),
	contains="SingleCellExperiment")

#' the rows are the panel names, the columns are the single cells,the column are the single cells to match the SCE designs (scRNA)
#' @param object imcExperiment object, class imcExperiment container
#' @export
#' @importFrom SingleCellExperiment SingleCellExperiment
.checkSpatialDimension<-function(object){
   nspatial<-nrow(object@coordinates)
   ndistance<-nrow(object@distance)
   nneigh<-nrow(object@neighborHood)
   nnet<-nrow(object@network)
   nmorph<-nrow(object@morphology)
   nlab<-length(object@uniqueLabel)
   nassay<-ncol(object)
   msg<-nassay==nspatial & nassay==ndistance & nassay==nneigh & nassay==nnet & nassay==nmorph &nassay==nlab
   if(msg==TRUE){
    msg<-NULL
   }else{
    msg<-"ERROR: make sure the columns are the cells, and rows are protein"
   }
  return(msg)
}

setValidity("imcExperiment",function(object){
    msg<-.checkSpatialDimension(object)
     if(is.null(msg)){
      TRUE
     }else msg
 })
