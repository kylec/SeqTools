# write.df
#' simplfy options for writing data frame
#'
#' @param data frame
#' @param output file (optional)
#'
#' @return
#' @export
#'
#' @examples write.df(dta.frame("a"=c(1,2),"b"=c(1,2)), file="test.txt)
write.df <- function(input_df, file=NULL) {
  if (is.null(file)) {
    write.table(input_df, row.names=F, sep="\t", quote=F)
  } else {
    write.table(input_df, file=file, row.names=F, sep="\t", quote=F)
  }
}
