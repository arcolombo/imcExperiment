# imcExperiment

 is(assay(imcData,'counts'),'matrix')
  rownames(imcData)
    # for plot scatter to work need to set the rowData feature in a specific way.
   channel<-sapply(strsplit(rownames(imcData),"_"),function(x) x[3])
   marker<-sapply(strsplit(rownames(imcData),"_"),function(x) x[2])
    rowData(imcData)<-DataFrame(channel_name=channel,marker_name=marker)
      rownames(imcData)<-marker
            plotScatter(imcData,rownames(imcData)[17:18],assay='counts')

  # convert to flowSet
             ## the warning has to do with duplicated Iridium channels.
   table(colData(imcData)$ROIID)
       (fsimc <- sce2fcs(imcData, split_by = "ROIID"))
    ## now we have a flowSet.
   pData(fsimc)
   fsApply(fsimc,nrow)
   dim(exprs(fsimc[[1]]))
   exprs(fsimc[[1]])[1:5,1:5]
    ## set up the metadata files.
   head(marker_info)
   
    exper_info<-data.frame(group_id=colData(imcData)$Treatment[match(pData(fsimc)$name,colData(imcData)$ROIID)],
                           patient_id=colData(imcData)$Patient.Number[match(pData(fsimc)$name,colData(imcData)$ROIID)],
                           sample_id=pData(fsimc)$name)
   
   ## create design
   design<-createDesignMatrix(
     exper_info,cols_design=c("group_id","patient_id"))
   
   ##set up contrast 
   contrast<-createContrast(c(0,1,0))
   nrow(contrast)==ncol(design)
   data.frame(parameters=colnames(design),contrast)
   
    ## flowSet to DiffCyt
    out_DA<-diffcyt(
      d_input=fsimc,
      experiment_info=exper_info,
      marker_info=marker_info,
      design=design,
      contrast=contrast,
      analysis_type = "DA",
      seed_clustering = 123
    )
   topTable(out_DA,format_vals = TRUE)
   
   out_DS<-diffcyt(
     d_input=fsimc,
     experiment_info=exper_info,
     marker_info=marker_info,
     design=design,
     contrast=contrast,
     analysis_type='DS',
     seed_clustering = 123,
     plot=FALSE)
   
      topTable(out_DS,format_vals = TRUE)
      
