#' histoCAT output saved as a data frame which can be set into S4 class.
#' Data set containing 1,000 cells and 73 features which include panel antibody, neighborhood computations, and phenograph clustering.
#' @format A data frame of 1,000 cells and histoCAT features
#' \describe{
#'   \item{Cell features}{raw signal intensity}
#'   \item{morphological features}{Area, Eccentricity, etc.}
#'   \item{Position}{x,y position}
#'   \item{neighborhood data}{the 10 neighborhors for each cell are computed, the number are the id of each neighbor}
#'   \item{clustering and network}{phenograph cell assignments, and tSNE coordinates commonly computed in histoCAT}
#'    }
#'    @export
