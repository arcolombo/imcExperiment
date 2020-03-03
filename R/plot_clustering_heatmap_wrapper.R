#' plot the heatmap of phenotypes using imcExperiment container.
#' 
#' @param myExperiment imcExperiment

#' @return              a GRanges or a KallistoExperiment, depending on `what`
#'
#' @importFrom pheatmap pheatmap
#' @import     dplyr
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_clustering_heatmap_wrapper<-function(myExperiment=NULL, 
  useMedians=TRUE,
 color_clusters=NULL){
  
  expr<-(cellIntensity(myExperiment))
  cell_clustering<-(getNetwork(myExperiment))

   expr<-as.matrix(expr)
   
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


