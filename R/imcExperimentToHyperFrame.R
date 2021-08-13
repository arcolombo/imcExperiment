#' map to point pattern from imcExperiment class.
#' @param imcExperiment  imcExperiment class
#' @param phenotypeToUse the network slot can often have many columns, this is the ID for the column number to use in the network slot.
#' @importFrom spatstat.geom hyperframe coords ppp unitname nncross
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @export
#' @examples
#' data(imcdata)
#' H<-imcExperimentToHyperFrame(imcExperiment=imcdata,phenotypeToUse = 1)
imcExperimentToHyperFrame<-function(imcExperiment=NULL,phenotypeToUse=1){
   #suppressPackageStartupMessages( require(spatstat))
   ##returns the PPP object, with marks as the original data frame.
  ### the imcExperiment structure forces rowData to have a column "ROIID".
  HH<-NULL
  for(i in unique(colData(imcExperiment)[,"ROIID"])){
   roi<-subsetCase(imcExperiment,i)
   pp<-.imcExperimentToPPP(caseExperiment=roi)
  stopifnot(all(all(coords(pp)==getCoordinates(roi))))
  #first<-split(pp,marks(pp))
  #pp<-first
  H<-hyperframe(point=pp,
	ROI=i)
  if(is.null(HH)!=TRUE){
  HH<-rbind(H,HH)
  }else{
 HH<-H
   }
  }
 return(HH)
}##main

#' map to point pattern from imcExperiment class.
#' @param caseExperiment the subset IMC experiment to cast into a point pattern
#' @param phenotypeToUse the cluster id to annotate the pattern
#' @importFrom spatstat.geom hyperframe coords ppp unitname unitname<-
.imcExperimentToPPP<-function(caseExperiment=NULL,phenotypeToUse=1){
 ### for an imcExperiment for 1 case, creates a point pattern.
  casePositions<-getCoordinates(caseExperiment)
  marksCase<-factor(getNetwork(caseExperiment)[,phenotypeToUse])
  mypat<-ppp(casePositions[,"X_position"],casePositions[,"Y_position"],
	c(min(casePositions[,"X_position"]),max(casePositions[,"X_position"])),
	c(min(casePositions[,"Y_position"]),max(casePositions[,"Y_position"])),
	marks=marksCase)
    unitname(mypat)<-"micrometer"

  return(mypat)
 }
