#' Initializes a imcExperiment and performs some rudimentary checks.
#' Many of the arguments CAN be NULL; determination of which is required 
#' is done at run-time.  A imcExperiment must contain at least the 
#' expressions and spatial/coordinate assays.  
#'
#' 
#' @param coordinates            matrix of spatial coordinates (x,y)
#' @param cellIntensity            matrix of counts
#' @param neighborHood               neighborhood results
#' @param network            network assignments for each cell
#' @param distance            distances for each cell, can be square
#' @param morphology            morphology features for each cell, can be square
#' @param uniqueLabel	character class each cell is assigned a uniqueLabel
#' @param ROIID	character for ROI
#' @export 
imcExperiment<-function(
        coordinates=matrix(1,3,3),
	cellIntensity=matrix(1,3,3),
	neighborHood=matrix(1,3,3),
	network=matrix(1,3,3),
	distance=matrix(1,3,3),
	morphology=matrix(1,3,3),
	uniqueLabel=rep("A",3),
	panel=as.character(seq(1,3)),
	ROIID=data.frame(ROIID=rep("A",3)),
	...){
	stopifnot(length(panel)==nrow(cellIntensity))

	# the imcExperiment accepts matrix strictly, so we enforce the class conversions.
	if(is(cellIntensity,'matrix')==FALSE){
	cellIntensity<-as.matrix(cellIntensity)
 	}
	if(is(coordinates,'matrix')==FALSE){
	coordinates<-as.matrix(coordinates)
 	}
	if(is(neighborHood,'matrix')==FALSE){
	neighborHood<-as.matrix(neighborHood)
 	}
	if(is(network,'matrix')==FALSE){
	network<-as.matrix(network)
 	}
	if(is(distance,'matrix')==FALSE){
	distance<-as.matrix(distance)
 	}
	if(is(morphology,'matrix')==FALSE){
	morphology<-as.matrix(morphology)
 	}

   ##counts is labeled
	#note that the rows are proteins and columns should be single cells
	# counts is a default slot for SCE.
   sce<-SingleCellExperiment(list(counts=cellIntensity))
   colData(sce)<-DataFrame(ROIID)
   rownames(sce)<-panel
    colnames(sce)<-uniqueLabel
   # colLabels can be used to store
   .imcExperiment(sce,
	coordinates=coordinates,
	cellIntensity=cellIntensity,
	neighborHood=neighborHood,
	network=network,
	distance=distance,
	morphology=morphology,
	uniqueLabel=uniqueLabel)
 

  
}

