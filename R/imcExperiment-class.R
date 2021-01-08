#' a summarized experiment of IMC runs, dimensions of the spatial and intensity data are regulated.
#'
#' @slot coordinates   Spatial coordinate (rows are cells and columns are x,y coordinates.
#' @slot cellIntensity a matrix of the intensity values rows are proteins and columns are cells (SCE format)
#' @slot neighborHood  a matrix (rows are cells, columns are features) of neighborhood observations from neighborhood analysis
#' @slot network      a matrix (rows are cells) and columns are for network assignment information
#' @slot distance a matrix of rows are cells and columns are for distance measurements
#' @slot uniqueLabel a character for each cell for unique labeling.
#' @name imcExperiment-class
#' @section Slots:
#' \describe{
#'   \item{\code{coordinates}:}{matrix class containing x,y coordinates}
#'   \item{\code{cellIntensity}:}{matrix class containing intensities}
#'   \item{\code{neighborHood}:}{matrix class containing neighborhood results}
#'   \item{\code{network}:}{matrix class containing cell assignment cluster}
#'   \item{\code{distance}:}{matrix class containing cell distance observations}
#'   \item{\code{uniqueLabel}:}{character class containing labels}
#' }
#' @rdname imcExperiment-class
#' @export
#' @import methods
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
.imcExperiment<-setClass("imcExperiment",
	slots=representation(coordinates="matrix",
				cellIntensity="matrix",
				neighborHood="matrix",
				network="matrix",
				distance='matrix',
				morphology='matrix',
				uniqueLabel="character"),
	contains="SingleCellExperiment")


#' @export
#' @importFrom SingleCellExperiment SingleCellExperiment

 # the rows are the panel names
 # the columns are the single cells
 # the column are the single cells to match the SCE designs (scRNA)
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
