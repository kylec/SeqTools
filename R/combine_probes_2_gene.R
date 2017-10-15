#' one to many operations helper for combine_probes_2_genes (local)
#'
#' @param one
#' @param many
#' @param func
#' @param ...
#'
#' @return
#'
#' @examples
oneToManyOperation <- function(one, many, func=NULL,...){
  idxs <- match(many, one)
  N <- length(unique(one))
  stopifnot(N == length(one))
  map <- vector("list", N)
  for(pos in 1:length(many)){
    if(!is.na(idxs[[pos]])) {
      map[[idxs[pos]]] <- c(map[[idxs[pos]]], pos)
    }
  }
  if(is.null(func)) { return (map)}
  else{ return (sapply(map, func, ...)) }
}

#' Combine multiple probes to gene level expression using svd
#'
#' @param expression dataframe
#' @param genes from rownames of expression data frame
#' @param method (svd)
#'
#' @return
#' @export
#'
#' @examples combine_probes_2_genes(exprs_df, genes)
combine_probes_2_gene <- function(expr, genes, method="svd"){
  # check if genes and expression are equal length
  stopifnot(length(genes) == nrow(expr))
  if(is.list(genes)) genes <- unlist(genes)

  stopifnot(dim(expr)[1] ==  length(genes))
  # get unique genes , remove null genes
  ugenes <- sort(setdiff(unique(genes),c("",NULL,NA)))

  M <- t(oneToManyOperation(ugenes, genes, function(idxs){
    sub.expr <- as.matrix(expr[idxs,])
    if(dim(sub.expr)[2] > 1){
      tmp <- svd(sub.expr - rowMeans(sub.expr))$v[,1]
      tmp.c <- mean(cor(tmp, t(sub.expr)))
      #cat(gene," ", tmp.c, "\n")
      multiplier <- ifelse(tmp.c < 0, -1, 1)
      sub.expr <- tmp * multiplier
    }
    sub.expr
  }))
  rownames(M) <- ugenes
  colnames(M) <- colnames(expr)
  M
}
