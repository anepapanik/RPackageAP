
#----------------------------------------------------------------------
# Distance function
# ----------------------------------------------------------------------
#' Creates a matrix with the distances between the different N points
#'
#' @param dataset dataset is a matrix of NxM points
#' @return distance_matrix_NxN distance_matrix_NxN is a NxN matrix
#' which contains the euclidean distance between all points of our dataset
#' @export
#' @examples
#' dataset <- matrix(c(1,1,2,2,3,3,4,4,5,5),5,2, byrow = TRUE)
#' dist_matrix_function(dataset)
#'
#'


dist_matrix_function <- function(dataset) {

  distance_matrix_NxN <- matrix(NA, nrow = nrow(dataset), ncol = nrow(dataset))

  for (obs in 1:nrow(dataset)) {
    point <- dataset[obs,]
    rep_matrix_point <- matrix(point, nrow = nrow(dataset), ncol = length(point), byrow = TRUE)
    print(rep_matrix_point)
    distance_matrix_NxN[obs,] <- sqrt(rowSums((dataset - rep_matrix_point)^2))
  }
  return(distance_matrix_NxN)
}


