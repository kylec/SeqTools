#' Quantile normalize function
#' 
#' Quantile normailze expression matrix
#' @param expression matrix 
#' @return quantile normalized expression data.frame
#' @keywords expression
#' @export
#' @examples 
#' quantnorm(expression_matrix)

quantnorm <- function(expression_matrix) {
  norm_matrix = normalize.quantiles(as.matrix(expression_matrix)) 
  norm_matrix = as.data.frame(norm_matrix)
  colnames(norm_matrix) = colnames(expression_matrix)
  return(norm_matrix)
}

#' Quantile normalize function using reference expression matrix
#' 
#' Quantile normailze expression matrix with reference expression matrix
#' @param expression matrix, reference expression matrix
#' @return quantile normalized expression data.frame
#' @keywords expression
#' @export
#' @examples 
#' quantnormref(expression_matrix, reference_matrix)

quantnormref <- function(expression_matrix, reference_matrix) {
  reference_dist = normalize.quantiles.determine.target(as.matrix(reference_matrix))
  norm_matrix = normalize.quantiles.use.target(as.matrix(expression_matrix), reference_dist)
  norm_matrix = as.data.frame(norm_matrix)
  colnames(norm_matrix) = colnames(exprssion_matrix)
  return(norm_matrix)
}

#' Transform expression matrix into z score
#' 
#' Transform expression matrix into z score
#' @param log2 expression matrix
#' @return log2 expression z-score
#' @keywords expression
#' @export
#' @examples 
#' calcZ(expression_matrix)
calcZ = function(expression_matrix) {
  means = rowMeans(expression_matrix)
  sd = apply(expression_matrix, 1, sd) 
  z = (expression_matrix-means)/sd
  return(z)
}

#' Transform expression matrix into z score based on reference expression
#' 
#' Transform expression matrix into z score based on reference expression
#' @param log2 expression matrix, log2 reference expression matrix
#' @return log2 expression z-score
#' @keywords expression
#' @export
#' @examples 
#' calcZref(expression_matrix, reference_matrix)
calcZref = function(expression_matrix, reference_matrix) {
  means = rowMeans(reference_matrix)
  sd = apply(reference_matrix, 1, sd) 
  z = (expression_matrix-means)/sd
  return(z)
}