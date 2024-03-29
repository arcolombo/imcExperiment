## ----fig.width=11,fig.height=11,message=FALSE---------------------------------
library(dplyr)
library(RColorBrewer)
library(pheatmap)
library(magrittr)
library(igraph)

 ##the rows of the IMC container input are the protein antibody panel, the columns are the cells. (need to transpose for matrix computatoins)
plot_clustering_heatmap_wrapper<-function(myExperiment=NULL, 
  useMedians=TRUE,
 color_clusters=NULL){
  
  expr<-(cellIntensity(myExperiment))
  cell_clustering<-(getNetwork(myExperiment))

   expr<-t(as.matrix(expr))
   
 if(is.null(color_clusters)==TRUE){
  color_clusters <- c("#DC050C", "#FB8072", "#1965B0", "#7BAFDE", "#882E72", 
  "#B17BA6", "#FF7F00", "#FDB462", "#E7298A", "#E78AC3", 
  "#33A02C", "#B2DF8A", "#55A1B1", "#8DD3C7", "#A6761D", 
  "#E6AB02", "#7570B3", "#BEAED4", "#666666", "#999999", 
  "#aa8282", "#d4b7b7", "#8600bf", "#ba5ce3", "#808000", 
  "#aeae5c", "#1e90ff", "#00bfff", "#56ff0d", "#ffff00")
 }

  # Calculate the median expression 
  expr_median <- data.frame(expr, cell_clustering = cell_clustering) %>%
    group_by(cell_clustering) %>% 
    summarize_all(funs(median))
 
  if(useMedians==TRUE){
  expr_median <- data.frame(expr, cell_clustering = cell_clustering) %>%
    group_by(cell_clustering) %>% 
    summarize_all(funs(median))
   }else{
   expr_median <- data.frame(expr, cell_clustering = cell_clustering) %>%
    group_by(cell_clustering) %>% 
    summarize_all(funs(mean))
 }
  # Calculate cluster frequencies
  clustering_table <- as.numeric(table(cell_clustering))
  
  # This clustering is based on the markers that were used for the main clustering
  d <- dist(expr_median[, colnames(expr)], method = "euclidean")
  cluster_rows <- hclust(d, method = "average")
   expr_heat <- as.matrix(expr_median[, colnames(expr)])
  rownames(expr_heat) <- expr_median$cell_clustering
 
  labels_row <- paste0(rownames(expr_heat), " (", 
    round(clustering_table / sum(clustering_table) * 100, 2), "%)")
  labels_col <- colnames(expr_heat)
  
  # Row annotation for the heatmap
  annotation_row <- data.frame(cluster = factor(expr_median$cell_clustering))
  rownames(annotation_row) <- rownames(expr_heat)
  
  color_clusters <- color_clusters[1:nlevels(annotation_row$cluster)]
  names(color_clusters) <- levels(annotation_row$cluster)
  annotation_colors <- list(cluster = color_clusters)
  annotation_legend <- FALSE
  
  # Colors for the heatmap
  #library(RColorBrewer);
  color <- colorRampPalette(rev(brewer.pal(n = 9, name = "RdYlBu")))(100)
  
  pheatmap(expr_heat, color = color, 
    cluster_cols = FALSE, cluster_rows = cluster_rows, 
    labels_col = labels_col, labels_row = labels_row, 
    display_numbers = TRUE, number_color = "black", 
    fontsize = 12, fontsize_number = 12,
    annotation_row = annotation_row, annotation_colors = annotation_colors,
    annotation_legend = annotation_legend)
}




plot_matrix_heatmap_wrapper<-function(expr=NULL, 
  cell_clustering=NULL, cluster_merging = NULL,useMedians=TRUE,useQuantiles=FALSE,
color_clusters=NULL){
  ## cluster_merging should be character merging vector of 29 or so clusters. this is input from the ANNOTATION object.  not a matched object level.
   expr<-as.matrix(expr)
  if(useQuantiles==TRUE){
  ##max min normalization
  library(matrixStats)
  rng <- colQuantiles(expr, probs = c(0.001, 0.999))
  expr01 <- t((t(expr) - rng[, 1]) / (rng[, 2] - rng[, 1]))
  expr01[expr01 < 0] <- 0
  expr01[expr01 > 1] <- 1
  }else{
   expr01<-expr 

  }

 if(is.null(color_clusters)==TRUE){
  color_clusters <- c("#DC050C", "#FB8072", "#1965B0", "#7BAFDE", "#882E72", 
  "#B17BA6", "#FF7F00", "#FDB462", "#E7298A", "#E78AC3", 
  "#33A02C", "#B2DF8A", "#55A1B1", "#8DD3C7", "#A6761D", 
  "#E6AB02", "#7570B3", "#BEAED4", "#666666", "#999999", 
  "#aa8282", "#d4b7b7", "#8600bf", "#ba5ce3", "#808000", 
  "#aeae5c", "#1e90ff", "#00bfff", "#56ff0d", "#ffff00")
 }

  # Calculate the median expression 
  library(dplyr)
  expr_median <- data.frame(expr, cell_clustering = cell_clustering) %>%
   dplyr::group_by(cell_clustering) %>% 
    dplyr::summarize_all(funs(median))
 if(useMedians==TRUE){
  expr01_median <- data.frame(expr01, cell_clustering = cell_clustering) %>%
    group_by(cell_clustering) %>% 
    dplyr::summarize_all(funs(median))
   }else{
   expr01_median <- data.frame(expr01, cell_clustering = cell_clustering) %>%
    dplyr::group_by(cell_clustering) %>% 
   dplyr::summarize_all(funs(mean))
 }
  # Calculate cluster frequencies
  clustering_table <- as.numeric(table(cell_clustering))
   print(dim(expr_median))
  # This clustering is based on the markers that were used for the main clustering
  d <- dist(expr_median[, colnames(expr)], method = "euclidean")
  cluster_rows <- hclust(d, method = "average")
   expr_heat <- as.matrix(expr01_median[, colnames(expr01)])
  rownames(expr_heat) <- expr01_median$cell_clustering
 
  labels_row <- paste0(rownames(expr_heat), " (", 
    round(clustering_table / sum(clustering_table) * 100, 2), "%)")
  labels_col <- colnames(expr_heat)
  
  # Row annotation for the heatmap
  annotation_row <- data.frame(cluster = factor(expr01_median$cell_clustering))
  rownames(annotation_row) <- rownames(expr_heat)
  
  color_clusters <- color_clusters[1:nlevels(annotation_row$cluster)]
  names(color_clusters) <- levels(annotation_row$cluster)
  annotation_colors <- list(cluster = color_clusters)
  annotation_legend <- FALSE
  
  if(!is.null(cluster_merging)){
    
    annotation_row$cluster_merging <- factor(cluster_merging)
    color_clusters <- color_clusters[1:nlevels(factor(cluster_merging))]
    names(color_clusters) <- levels(factor(cluster_merging))
    annotation_colors$cluster_merging <- color_clusters
    annotation_legend <- TRUE
  }
  
  # Colors for the heatmap
  library(RColorBrewer);library(pheatmap)
  color <- colorRampPalette(rev(brewer.pal(n = 9, name = "RdYlBu")))(100)
  
  pheatmap::pheatmap(expr_heat, color = color, 
    cluster_cols = FALSE, cluster_rows = cluster_rows, 
    labels_col = labels_col, labels_row = labels_row, 
    display_numbers = TRUE, number_color = "black", 
    fontsize = 10, fontsize_number = 7,
    annotation_row = annotation_row, annotation_colors = annotation_colors,
    annotation_legend = annotation_legend)
  
}






## ----message=FALSE------------------------------------------------------------
 library(imcExperiment)
  #10 cells with 10 proteins
  # 10 neighbors 
 # and square distance matrix
 # note that for SCE objects the columns are the cells, and rows are proteins
  x<-imcExperiment(cellIntensity=matrix(1,nrow=10,ncol=10),
	coordinates=matrix(1,nrow=10,ncol=2),
	neighborHood=matrix(1,nrow=10,ncol=10),
	network=matrix(1,nrow=10,ncol=10),
	distance=matrix(1,nrow=10,ncol=10),
	morphology=matrix(1,nrow=10,ncol=10),
	uniqueLabel=paste0("A",seq(1,10)),
	panel=letters[1:10],
	ROIID=data.frame(ROIID=rep("A",10)))
  

 #7 cells with 10 proteins
 # but the spatial information is 10 rows and this will fail to build.
 #x<-imcExperiment(cellIntensity=matrix(1,nrow=7,ncol=10),
#	coordinates=matrix(1,nrow=10,ncol=2),
#	neighborHood=matrix(1,nrow=10,ncol=20),
#	network=matrix(1,nrow=10,ncol=3),
#	distance=matrix(1,nrow=10,ncol=2),
#	uniqueLabel=rep("A",10),
#	ROIID=data.frame(ROIID=rep("A",10)))


 


## ----access,eval=TRUE,message=FALSE-------------------------------------------

  #get cellintensities
  cellIntensity(x)
  #set intensities
  newIn<-matrix(rnorm(100,2,5),nrow=10,ncol=10)
  rownames(newIn)<-rownames(x)
  colnames(newIn)<-colnames(x)
   cellIntensity(x)<-newIn
  cellIntensity(x)==newIn

 # if we want to store both the raw and normalized values in assays we can.
  assays(x,withDimnames = FALSE)$raw<-matrix(1,nrow=10,ncol=10)
  #store the normalized values.
   cellIntensity(x)<-asinh(counts(x)/0.5)
   all(cellIntensity(x)==asinh(assays(x)$counts/0.5))
  x



 ## access the coordinates
  getCoordinates(x)
  getCoordinates(x)<-matrix(rnorm(20,0,10),nrow=10,ncol=2)
head(  getCoordinates(x))
  
 ## access the neighborhood profile.  Note each row must equal the number of cells, but the columns can be extended depending on the radius of interactions.
  ## access the coordinates
  getNeighborhood(x)
  getNeighborhood(x)<-matrix(rnorm(100,1,5),nrow=10,ncol=10)
 head( getNeighborhood(x))

 ## get the distance usually a square matrix, or can be just first nearest etc. 
    getDistance(x)
  getDistance(x)<-matrix(rnorm(100,1,5),nrow=10,ncol=10)
  head(getDistance(x))

 # get morphological features
  getMorphology(x)
  getMorphology(x)<-matrix(rnorm(100,1,5),nrow=10,ncol=60)
  head(getMorphology(x))

 ## for each cell we can obtain the ROI that it belongs to
  rowData(x)
 ## if we want to add patient features to each ROI we can
  metas<-data.frame(ROIID=factor(rep("A",10)),treatment=factor(rep('none',10)))
  colData(x)<-DataFrame(metas)

 ##other slots for covariates
   metadata(x)$experiment<-'test'
   metadata(x)

## ----message=FALSE------------------------------------------------------------
  ### load the data from package.
  library(imcExperiment)
 ##load the data 1000 cells from IMC experiment.
  data(data)
  dim(data)
  ##output from histoCAT to R
  expr<-data[,3:36]
  normExp<-percentilenormalize(data=expr,percentile=0.99)
  normExp<-as.matrix(normExp)


  ##spatial component
  spatial<-(data[,c("X_position","Y_position")])
  spatial<-as.matrix(spatial)
 ##uniqueLabel
  uniqueLabel<-paste0(data[,"ImageId"],"_",data[,"CellId"])

 phenotypes<-as.matrix(data[,grepl("Phenograph",colnames(data))])
 morph<-as.matrix(data[,c("Area","Eccentricity",
	"Solidity",
	"Extent",
	"Perimeter")])

  x<-imcExperiment(cellIntensity=t(normExp),
	coordinates=spatial,
	neighborHood=as.matrix(data[,grepl("neighbour_",colnames(data))]),
	network=phenotypes,
	distance=matrix(1,nrow=nrow(data),ncol=10),
	morphology=morph,
	panel=colnames(normExp),
	uniqueLabel=paste0(data$ImageId,"_",data$CellId),
	ROIID=data.frame(ROIID=data$ImageId))

 ## explore the container.
  dim(assay(x))
   colData(x)$treatment<-DataFrame(treatment=rep('none',1000))
   head(colData(x))
 head( colnames(x))
  rownames(x)
  
  ## Intensity
  all(t(cellIntensity(x))==normExp)
  head(t(cellIntensity(x)))
  ##coordinate
  all(getCoordinates(x)==spatial)
  head(getCoordinates(x))
  #neighbor attraction data form histoCAT
 all(getNeighborhood(x)==as.matrix(data[,grepl("neighbour_",colnames(data))]))
 head(getNeighborhood((x)))
 ##phenotype cluster ID
  head(getNetwork(x))
 all(getNetwork(x)==phenotypes)
 ###distance calculations
  head(getDistance(x))
  ##morphology
   all(getMorphology(x)==morph)
  head(getMorphology(x))
  ##uniqueLabel
   head(getLabel(x))
   all(getLabel(x)==paste0(data$ImageId,"_",data$CellId) )

## -----------------------------------------------------------------------------
 

 ### inherited accessor.
  pca_data <- prcomp(t(counts(x)), rank=50)
  reducedDims(x) <- list(PCA=pca_data$x, TSNE=data.frame(tsne.x=data$tSNE4148542692_1,tsne.y=data$tSNE4148542692_2))
   x
     dim(reducedDims(x)$PCA)
     dim(reducedDims(x)$TSNE)

 imc<-x
 x<-NULL

## ----pheno,eval=TRUE,fig.width=11,fig.height=11,message=FALSE-----------------

 ### create  phenotypes via Rphenograph
  ##run phenograph
  library(Rphenograph)
  phenos<-Rphenograph(t(cellIntensity(imc)),k=35)
   pheno.labels<-as.numeric(membership(phenos[[2]]))
   getNetwork(imc)<-matrix(pheno.labels)
   head( getNetwork(imc))
  ##plot phenograph
  plot_clustering_heatmap_wrapper(myExperiment=imc)

## ----fig.width=11,fig.height=11-----------------------------------------------
 ###

### reading in a directory of histoCAT csv files.

 ##need to reconstruct the fold revised IMC data.
  ##merges a folder full of histoCAT csv files.
 # use morphology and the 1-10 neighbors.
  dataSet<-NULL
 for(i in c("case8.csv","case5.csv")){
  message(i)
    fpath <- system.file("extdata", i, package="imcExperiment")
    case1<-read.csv(fpath)
   proteins<-colnames(case1)[grepl("Cell_",colnames(case1))]
   neig<-colnames(case1)[grepl("neighbour_",colnames(case1))][1:10]
    case1<-case1[,c("ImageId","CellId","X_position","Y_position",proteins,
                    "Area",
                    "Eccentricity",
	"Solidity",
	"Extent",
	"Perimeter",
	 neig)]
    dataSet<-rbind(dataSet,case1)
  }
 
 expr<-dataSet[,proteins]
  normExp<-percentilenormalize(data=expr,percentile=0.99)
  normExp<-as.matrix(normExp)
  boxplot(normExp)

  ##spatial component
  spatial<-(dataSet[,c("X_position","Y_position")])
  spatial<-as.matrix(spatial)
 ##uniqueLabel
  uniqueLabel<-paste0(dataSet[,"ImageId"],"_",dataSet[,"CellId"])

  ##not yet assigned
  phenotypes<-matrix(0,nrow(dataSet),1)
  ROIID=data.frame(ROIID=dataSet[,"ImageId"])
  morph<-dataSet[,c("Area",
                    "Eccentricity",
	"Solidity",
	"Extent",
	"Perimeter")]
  
  x<-imcExperiment(cellIntensity=t(normExp),
	coordinates=spatial,
	neighborHood=as.matrix(dataSet[,grepl("neighbour_",colnames(dataSet))]),
	network=phenotypes,
	distance=matrix(1,nrow=nrow(dataSet),ncol=10),
	morphology=morph,
	panel=colnames(normExp),
	uniqueLabel=uniqueLabel,
	ROIID=ROIID)
   x
   
   
  ##phenotype
  ##run phenograph
  require(Rphenograph)
  phenos<-Rphenograph(t(cellIntensity(x)),k=35)
   pheno.labels<-as.numeric(membership(phenos[[2]]))
   getNetwork(x)<-matrix(pheno.labels)
    head(getNetwork(x))
  ##plot phenograph
  plot_clustering_heatmap_wrapper(myExperiment=x)

## -----------------------------------------------------------------------------

 ##store the ROIID in the metadata columns.
  ##access the unique cell labels.
  head(getLabel(x))
  
  roi<-subsetCase(x,372149 )
  roi

## -----------------------------------------------------------------------------

  
  ##you can append more clinical features to the columns of the sampleDat data.frame.
  H<-imcExperimentToHyperFrame(imcExperiment=x)
  helper<-function(pp=NULL,i=NULL,j=NULL){
  ps<-split(pp)
  nnd<-nncross(ps[[i]],ps[[j]])
  }
 ## 1-NN analysis.
  ## compare cluster 10 to cluster 9
   eachNND<-with(H,helper(pp=point,i="10",j="8"))
 ## first choose 2 clusters to compare.
  sapply(eachNND,function(x) mean(x[,"dist"]))




## ----classChange,fig.width=9,fig.height=9,message=FALSE-----------------------
  library(CATALYST)
library(cowplot)
library(flowCore)
library(ggplot2)
library(SingleCellExperiment)
library(diffcyt)

 ## from IMCexperiment to SCE
 sce<- SingleCellExperiment(x)
 class(sce)

 ## FIX ME: add the SOM algorithms for over-clustering and meta-clustering by using class inheritance. 

 #### for the cytof, the columns are sample info and the rows are cells. it uses the SummarizedExperiment format.
 
 

 ## change into a flowFrame?

 ## change into a daFrame (CATALYST)

 data("imcData")

 
 rd<-DataFrame(colData(imcData))

  marker_info<-data.frame(channel_name=sapply(strsplit(rownames(imcData),"_"),function(x) x[3]),
              marker_name=rownames(imcData),
              marker_class=c(rep("type",13),
                             rep("state",18),
                             rep("none",13)))

  ## Switching into Summarized Experiment class.
    dse<-SummarizedExperiment(assays=SimpleList(exprs=t(cellIntensity(imcData))),
                           rowData=rd,
                           colData=DataFrame(marker_info)
                           )
   stopifnot(all(colnames(dse)==marker_info$marker_name))
   dse

 # Transform data recommended
 dse <- transformData(dse)
## maybe look a the normalization.
# Generate clusters
dse <- (generateClusters(dse,meta_clustering=TRUE,meta_k=30,seed_clustering=828))

 ## examine each cluster.
cluster<-rowData(dse)
 data<-data.frame(t(logcounts(imcData)),cluster)
 plot_matrix_heatmap_wrapper(expr=data[,rownames(imcData)],
                                 cell_clustering=data$cluster_id)
 
 


## ---- message=FALSE-----------------------------------------------------------

# library(CATALYST)
#data(sample_ff)
#data(sample_key)
#head(sample_key)
 #sce <- prepData(sample_ff)
  #plotScatter(sce,rownames(sce)[1:2])
  ## the bar code ID events, 
  # the rows are the bar code channels
   #(fs <- sce2fcs(sce, split_by = "sample_id"))

 ## key notes for strucutre improvements.
   #subsetting x SCE to y results in SCE
   # y <- x[i, , drop = FALSE]
    ## using 'assay' method, returns a matrix.
#  y <- assay(y, 'counts')
   ## and should be a class matrix,assay
 #   y <- t(assay(x, 'counts'))

 ## improving the IMC class structure.
   ## the assay returns matrix class! required for CATALYST.
  is(assay(imcData,'counts'),'matrix')
  rownames(imcData)
    # for plot scatter to work need to set the rowData feature in a specific way.
   channel<-sapply(strsplit(rownames(imcData),"_"),function(x) x[3])
      channel[34:35]<-c("Ir1911","Ir1931")

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
      
       ### from flowSet to SE.
 d_se<-prepareData(fsimc,exper_info,marker_info)




## ----diffcyt,eval=TRUE--------------------------------------------------------
 library(diffcyt)
# Function to create random data (one sample)
d_random <- function(n = 20000, mean = 0, sd = 1, ncol = 20, cofactor = 5) {
  d <- sinh(matrix(rnorm(n, mean, sd), ncol = ncol)) * cofactor
  colnames(d) <- paste0("marker", sprintf("%02d", 1:ncol))
  d
}

# Create random data (without differential signal)
set.seed(123)
d_input <- list(
  sample1 = d_random(), 
  sample2 = d_random(), 
  sample3 = d_random(), 
  sample4 = d_random()
)

experiment_info <- data.frame(
  sample_id = factor(paste0("sample", 1:4)), 
  group_id = factor(c("group1", "group1", "group2", "group2")), 
  stringsAsFactors = FALSE
)

marker_info <- data.frame(
  channel_name = paste0("channel", sprintf("%03d", 1:20)), 
  marker_name = paste0("marker", sprintf("%02d", 1:20)), 
  marker_class = factor(c(rep("type", 10), rep("state", 10)), 
                        levels = c("type", "state", "none")), 
  stringsAsFactors = FALSE
)

# Prepare data
d_se <- prepareData(d_input, experiment_info, marker_info)

# Transform data
d_se <- transformData(d_se)

# Generate clusters
d_se <- generateClusters(d_se)




