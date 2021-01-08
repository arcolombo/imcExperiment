#' map to point pattern from imcExperiment class.
#' @param imcExperiment  imcExperiment class
#' @importFrom spatstat ppp hyperframe unitname
#' @export
imcExperimentToHyperFrame<-function(imcExperiment=NULL){
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
#' @importFrom spatstat ppp hyperframe unitname
#'
.imcExperimentToPPP<-function(caseExperiment=NULL){
 ### for an imcExperiment for 1 case, creates a point pattern.
  casePositions<-getCoordinates(caseExperiment)
  marksCase<-factor(getNetwork(caseExperiment) )
  mypat<-ppp(casePositions[,"X_position"],casePositions[,"Y_position"],
	c(min(casePositions[,"X_position"]),max(casePositions[,"X_position"])),
	c(min(casePositions[,"Y_position"]),max(casePositions[,"Y_position"])),
	marks=marksCase)
    unitname(mypat)<-"micrometer"

  return(mypat)
 }
